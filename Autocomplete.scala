package controllers

import collection.immutable.TreeMap

class AutoComplete[T](items: List[T], getName: T => String) {
  private val keyedItems = items.map { item =>
    getName(item).toLowerCase -> item
  }

  private var corpus = TreeMap(keyedItems: _*)

  def query(query: String): List[T] = {
    val lowerQuery = query.toLowerCase
    corpus.iteratorFrom(lowerQuery).takeWhile(_._1.startsWith(lowerQuery)).take(5).map(_._2).toList
  }

  def update(item: T) = synchronized {
    corpus = corpus + (getName(item).toLowerCase -> item)
  }
}
