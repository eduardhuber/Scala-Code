package controllers

import play.api.mvc._
import org.slf4j.Logger
import play.api.libs.json._
import models._
import ModelImplicits._
import com.mohiva.play.silhouette.api.Silhouette
import silhouette.JWTEnv

trait SearchController {
    self: Controller with SearchModule with DatabaseModule =>

  def logger: Logger

  def silhouette: Silhouette[JWTEnv]
  private def autocompleteAction[T: Writes](corpus: AutoComplete[T]) = Action { request =>
    RequestArgs.queryForm.bindFromRequest()(request).value.map { query =>
      Ok(Json.toJson(corpus.query(query)))
    }.getOrElse(BadRequest)
  }

  lazy val roleCorpus = new AutoComplete[Role](databaseService.readAllRoles, _.name)
  lazy val companyCorpus = new AutoComplete[Company](databaseService.readAllCompanies, _.name)
  lazy val gameCorpus = new AutoComplete[Game](databaseService.readAllGames, _.name)

  def roleAutocomplete = autocompleteAction(roleCorpus)
  def companyAutocomplete = autocompleteAction(companyCorpus)
  def gameAutocomplete = autocompleteAction(gameCorpus)

  def searchMaker(count: Option[Int], offset: Option[Int]) = silhouette.UserAwareAction { implicit request =>
    RequestArgs.queryForm.bindFromRequest()(request).value.map { query =>
      val superUser = request.identity.exists(_.isSuperuser)
      val makersAndEmails = databaseService.getGameMakers(request.identity.map(_.id).getOrElse(-1),
          searchService.searchMaker(query.toLowerCase, superUser, count.getOrElse(20), offset.getOrElse(0)))
      if (superUser) {
        val makersWithEmails = makersAndEmails.map { case (maker, emailOpt) =>
          val makerJson = Json.toJson(maker).as[JsObject]
          emailOpt.map { email =>
            makerJson ++ Json.obj("email" -> email)
          }.getOrElse(makerJson)
        }
        Ok(Json.toJson(makersWithEmails))
      } else {
        Ok(Json.toJson(makersAndEmails.unzip._1))
      }
    }.getOrElse(BadRequest)
  }

  def searchJob(count: Option[Int], offset: Option[Int]) = silhouette.UserAwareAction { implicit request =>
    RequestArgs.queryForm.bindFromRequest()(request).value.map { query =>
      Ok(Json.toJson(databaseService.getJobCards(
          searchService.searchJob(query.toLowerCase, count.getOrElse(20), offset.getOrElse(0)), request.identity.map(_.id).getOrElse(-1))))
    }.getOrElse(BadRequest)
  }

  def searchGame(count: Option[Int], offset: Option[Int]) = silhouette.UserAwareAction { implicit request =>
    RequestArgs.queryForm.bindFromRequest()(request).value.map { query =>
      Ok(Json.toJson(databaseService.getGames(searchService.searchGame(query.toLowerCase, count.getOrElse(20), offset.getOrElse(0)), request.identity.map(_.id).getOrElse(-1))))
    }.getOrElse(BadRequest)
  }
}
