package controllers

import play.api.libs.json.Json
import org.joda.time.DateTime
import play.api.mvc._
import util.Random

import models._
import ModelImplicits._

trait HomepageEventsController {
    self: Controller with DatabaseModule =>

  val fakeFirstNames = List("Aaron", "Adam", "Adeline", "Adrian", "Aiden", "Alaina", "Alex", "Alexander", "Ameila",
      "Andrew", "Angela", "Anna", "Annabelle", "Aubrey", "Audrey", "Austin", "Ava", "Bailey", "Barbara", "Benjamin",
      "Blake", "Brayden", "Caden", "Caleb", "Callie", "Camden", "Cameron", "Carol", "Carter", "Charlie", "Charlotte",
      "Chase", "Chloe", "Christopher", "Claire", "Colton", "Connor", "Cooper", "Dan", "Daniel", "David", "Debby", "Declan",
      "Dominic", "Donna", "Dylan", "Elena", "Eli", "Eliana", "Elijah", "Elizabeth", "Emily", "Emma", "Ethan", "Eva", "Gabriel",
      "Gabriella", "Grace", "Grayson", "Helen", "Henry", "Hudson", "Hunter", "Ian", "Isaac", "Isabella", "Isabelle", "Isaiah",
      "Jack", "Jackson", "Jacob", "James", "Jason", "Jayce", "Jayden", "Jen", "Jimmy", "John", "Jonathan", "Jordan", "Joseph",
      "Joshua", "Josiah", "Julian", "Kaitlyn", "Karen", "Kennedy", "Kylie", "Landon", "Leah", "Leo", "Levi", "Liam", "Lila",
      "Liliana", "Lillian", "Lily", "Lincoln", "Linda", "Lisa", "Liz", "Logan", "Lucas", "Lucy", "Luke", "Mack", "Madelyn",
      "Madison", "Maggie", "Makayla", "Maria", "Mason", "Matthew", "Maya", "Mia", "Michael", "Mila", "Miles", "Nancy", "Natalie",
      "Nathan", "Nicholas", "Noah", "Nolan", "Oliver", "Olivia", "Owen", "Parker", "Patricia", "Reagan", "Ruth", "Ryan", "Sadie",
      "Samantha", "Sandra", "Sarah", "Savannah", "Scarlett", "Sebastian", "Sharon", "Shirley", "Skyler", "Sophia", "Sophie", "Sue",
      "Susan", "Thomas", "Tristan", "Tyler", "Victoria", "Violet", "William", "Wyatt", "Xavier", "Zachary", "Zoe")

  val fakeLastNames = List("Adams", "Alexander", "Allen", "Anderson", "Armstrong", "Bailey", "Baker", "Barnes", "Bell",
      "Bennett", "Brooks", "Brown", "Bryant", "Butler", "Campbell", "Carpenter", "Carter", "Clark", "Coleman", "Collins", "Cook",
      "Cooper", "Cox", "Cruz", "Davis", "Diaz", "Edwards", "Elliott", "Evans", "Flores", "Foster", "Garcia", "Gomez", "Gonzales",
      "Gonzalez", "Gray", "Green", "Greene", "Griffin", "Hall", "Harris", "Hayes", "Henderson", "Hernandez", "Hill", "Howard",
      "Hughes", "Jackson", "James", "Jenkins", "Johnson", "Jones", "Kelly", "King", "Lawrence", "Lee", "Lewis", "Long", "Lopez",
      "Martin", "Martinez", "Miller", "Mitchell", "Moore", "Morgan", "Morris", "Murphy", "Nelson", "Nguyen", "Parker", "Patterson",
      "Perez", "Perry", "Peterson", "Phillips", "Powell", "Price", "Ramirez", "Reed", "Reyes", "Richardson", "Rivera", "Roberts",
      "Robinson", "Rodriguez", "Rogers", "Ross", "Russell", "Sanchez", "Sanders", "Scott", "Simmons", "Smith", "Stewart", "Taylor",
      "Thomas", "Thompson", "Torres", "Turner", "Walker", "Ward", "Washington", "Watson", "Weaver", "White", "Williams", "Wilson",
      "Wood", "Wright", "Young")

  val actions = List(
      "just invited",
      "just joined",
      "just verified",
      "just verified",
      "just verified",
      "just connected with",
      "just connected with",
      "just connected with")

  def randomName = {
    val firstName = fakeFirstNames(Random.nextInt(fakeFirstNames.size))
    val lastName = fakeLastNames(Random.nextInt(fakeLastNames.size))
    firstName + " " + lastName
  }

  def randomEvent = {
    val subject = randomName
    val verb = actions(Random.nextInt(actions.size))
    val obj = if (verb == "just joined")
      "Gamesmith"
    else
      randomName

    Event(0, subject, verb, obj, new DateTime())
  }

  def getRecentEvents = Action { implicit request =>
    //XXX: This causes crazy error messages on my local machine about RabbitMQ. 
    RequestArgs.eventsForm.bindFromRequest().value.map { timestamp =>
      val newEvents = databaseService.getEventsSince(new DateTime(timestamp))
      if (newEvents.nonEmpty || Random.nextDouble() < 0.7) {
        Ok(Json.toJson(newEvents))
      } else {
        Ok(Json.toJson(List(randomEvent)))
      }
    }.getOrElse(BadRequest)
  }
}
