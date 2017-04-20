package controllers

import java.util.{Timer, TimerTask}

import models._
import org.apache.lucene.analysis.standard.UAX29URLEmailAnalyzer
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.queryparser.simple.SimpleQueryParser
import org.apache.lucene.search._
import org.apache.lucene.store.RAMDirectory
import org.joda.time.DateTime

trait SearchService {
  def searchMaker(query: String, isSuperuser: Boolean, count: Int, offset: Int): List[Long]

  def searchJob(query: String, count: Int, offset: Int): List[Long]

  def searchGame(query: String, count: Int, offset: Int): List[Long]

  def browseMakers(viewerOpt: Option[GameMaker], count: Int, offset: Int): List[Long]

  def sudoBrowseMakers(count: Int, offset: Int): List[Long]
}

trait SearchModule {
  val searchService: SearchService
}

trait SearchCorpus[T] {
  val analyzer = new UAX29URLEmailAnalyzer

  private val indexWriter = {
    val directory = new RAMDirectory
    val config = new IndexWriterConfig(analyzer)
    new IndexWriter(directory, config)
  }

  private var reader = DirectoryReader.open(indexWriter)
  private var searcher = new IndexSearcher(reader)

  def search[U](block: IndexSearcher => U): U = block(searcher)

  def commit() = synchronized {
    indexWriter.commit()
    reader = DirectoryReader.openIfChanged(reader)
    searcher = new IndexSearcher(reader)
  }

  def updateDocument(id: Long, item: T): Unit = synchronized {
    indexWriter.deleteDocuments(LegacyNumericRangeQuery.newLongRange("id", id, id, true, true))

    val document = new Document
    toDocument(item, document)
    indexWriter.addDocument(document)
  }

  def init() = {
    getDocuments { item =>
      val document = new Document
      toDocument(item, document)
      indexWriter.addDocument(document)
    }
    commit()
  }

  lazy val parser = new SimpleQueryParser(analyzer, "content")

  def fieldParser(field: String) = new SimpleQueryParser(analyzer, field)

  def toDocument(item: T, document: Document): Unit

  val getDocuments: (T => Unit) => Unit
}

trait LuceneSearchModule extends SearchModule {
  self: DatabaseModule =>

  val searchService = new SearchService {
    val makerCorpus = new SearchCorpus[(GameMaker, Option[String])] {
      def toDocument(makerWithEmailStatus: (GameMaker, Option[String]), document: Document) = {
        val (maker, optEmail) = makerWithEmailStatus
        document.add(new LegacyLongField("id", maker.id, Field.Store.YES))

        val searchableContent = collection.mutable.Buffer(
          maker.firstName, maker.lastName, maker.location, maker.currGame, maker.currCompany, maker.currRole)

        maker.credits.foreach { credit =>
          searchableContent += credit.game.name
          credit.role.foreach { role =>
            searchableContent += role.name
          }
          credit.company.foreach { company =>
            searchableContent += company.name
          }
        }

        optEmail.foreach {
          email =>
            document.add(new TextField("email", email, Field.Store.NO))
        }

        document.add(new TextField("content", searchableContent.mkString(" "), Field.Store.NO))
        document.add(new TextField("claimed", maker.claimed.getOrElse(false).toString, Field.Store.NO))
        document.add(new TextField("hasEmail", optEmail.isDefined.toString, Field.Store.NO))

      }

      override val getDocuments = databaseService.readAllMakers _
    }

    val jobCorpus = new SearchCorpus[JobCard] {
      def toDocument(job: JobCard, document: Document) = {
        document.add(new LegacyLongField("id", job.id, Field.Store.YES))
//        val searchableContent = List(job.role.name, job.company.name, job.location, job.description)
        val searchableContent = List(job.role.name, job.location)
        document.add(new TextField("content", searchableContent.mkString(" "), Field.Store.NO))
      }

      override val getDocuments = databaseService.readAllJobCards _
    }

    val gameCorpus = new SearchCorpus[BrowseGameView] {
      def toDocument(game: BrowseGameView, document: Document) = {
        document.add(new LegacyLongField("id", game.id, Field.Store.YES))
        val searchableContent = List(game.name)
        document.add(new TextField("content", searchableContent.mkString(" "), Field.Store.NO))
      }

      override val getDocuments = databaseService.readGames _
    }

    makerCorpus.init()
    jobCorpus.init()
    gameCorpus.init()

    val timer = new Timer(true)

    timer.schedule(new TimerTask() {
      var prevRun = new DateTime

      override def run() = {
        val currentStart = new DateTime
        val allUpdates = databaseService.getIndexUpdates(prevRun)
        allUpdates.foreach {
          case SearchUpdateRequest(Some(id), None, None) =>
            databaseService.getGameMakerById(id, -1).foreach { maker =>
              val optEmail = databaseService.getEmail(id)
              makerCorpus.updateDocument(id, maker -> optEmail)
            }

          case SearchUpdateRequest(None, Some(id), None) =>
            databaseService.getJobCards(List(id), -1).foreach { jobCard =>
              jobCorpus.updateDocument(id, jobCard)
            }

          case SearchUpdateRequest(None, None, Some(id)) =>
            databaseService.getGames(List(id), -1).foreach { game =>
              gameCorpus.updateDocument(id, game)
            }

          case e => throw new RuntimeException(e.toString)
        }
        if (allUpdates.exists(_.makerId.isDefined))
          makerCorpus.commit()

        if (allUpdates.exists(_.jobId.isDefined))
          jobCorpus.commit()

        if (allUpdates.exists(_.gameId.isDefined))
          gameCorpus.commit()

        prevRun = currentStart
      }
    }, 10000, 10000)

    private def searchCorpusWithPosition(query: Query, corpus: SearchCorpus[_], count: Int, offset: Int) = {
      val total = offset + count
      corpus.search { searcher =>
        searcher.search(query, total).scoreDocs.drop(offset).toList.map { scoreDoc =>
          searcher.doc(scoreDoc.doc).getField("id").numericValue.longValue
        }
      }
    }

    override def searchMaker(query: String, isSuperuser: Boolean, count: Int, offset: Int) = {
      val parsedQuery = makerCorpus.parser.parse(query.replace(" ", "+") + "*")
      val finalQuery = if (isSuperuser) {
        val booleanQuery = new BooleanQuery.Builder

        val hasEmailQuery = new TermQuery(new Term("hasEmail", "true"))
        booleanQuery.add(new BoostQuery(hasEmailQuery, 2.0f), BooleanClause.Occur.SHOULD)

        val claimedQuery = new TermQuery(new Term("claimed", "false"))
        booleanQuery.add(new BoostQuery(claimedQuery, 1.5f), BooleanClause.Occur.SHOULD)

        val emailQuery = makerCorpus.fieldParser("email").parse(query)

        val emailOrContentQuery = new BooleanQuery.Builder
        emailOrContentQuery.add(emailQuery, BooleanClause.Occur.SHOULD)
        emailOrContentQuery.add(parsedQuery, BooleanClause.Occur.SHOULD)

        booleanQuery.add(emailOrContentQuery.build(), BooleanClause.Occur.MUST)

        booleanQuery.build
      } else {
        parsedQuery
      }
      searchCorpusWithPosition(finalQuery, makerCorpus, count, offset)
    }

    override def searchJob(query: String, count: Int, offset: Int) = {
      val parsedQuery = jobCorpus.parser.parse(query.replace(" ", "+") + "*")
      searchCorpusWithPosition(parsedQuery, jobCorpus, count, offset)
    }

    override def searchGame(query: String, count: Int, offset: Int) = {
      val parsedQuery = gameCorpus.parser.parse(query.replace(" ", "+") + "*")
      searchCorpusWithPosition(parsedQuery, gameCorpus, count, offset)
    }

    override def browseMakers(viewerOpt: Option[GameMaker], count: Int, offset: Int) =
      searchCorpusWithPosition(browseQuery(viewerOpt), makerCorpus, count, offset)

    override def sudoBrowseMakers(count: Int, offset: Int) = {
      val booleanQuery = new BooleanQuery.Builder

      val hasEmailQuery = new TermQuery(new Term("hasEmail", "true"))
      booleanQuery.add(new BoostQuery(hasEmailQuery, 2.0f), BooleanClause.Occur.SHOULD)

      val claimedQuery = new TermQuery(new Term("claimed", "false"))
      booleanQuery.add(new BoostQuery(claimedQuery, 1.5f), BooleanClause.Occur.SHOULD)

      val matchAll = new MatchAllDocsQuery
      booleanQuery.add(matchAll, BooleanClause.Occur.SHOULD)
      searchCorpusWithPosition(booleanQuery.build, makerCorpus, count, offset)
    }

    private def browseQuery(viewerOpt: Option[GameMaker]) = {
      val booleanQuery = new BooleanQuery.Builder

      val claimedQuery = new TermQuery(new Term("claimed", "true"))
      booleanQuery.add(new BoostQuery(claimedQuery, 1.2f), BooleanClause.Occur.SHOULD)

      viewerOpt.foreach { query =>
        val gameNames = (query.currGame :: query.credits.map(_.game.name)).distinct
        gameNames.foreach { name =>
          val rewrite = "\"" + name.replace(" ", "+") + "\""
          val query = makerCorpus.parser.parse(rewrite)
          booleanQuery.add(new BoostQuery(query, 1.5f), BooleanClause.Occur.SHOULD)
        }
      }

      val matchAll = new MatchAllDocsQuery
      booleanQuery.add(matchAll, BooleanClause.Occur.SHOULD)
      booleanQuery.build
    }
  }
}
