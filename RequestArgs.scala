package controllers

import models._

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import java.util.Date

object RequestArgs {
  // TODO: require a password length
  val queryForm = Form("q" -> nonEmptyText)
  val loginForm = Form(tuple("email" -> email, "password" -> nonEmptyText(minLength = 8), "makerid" -> optional(longNumber)))
  val directSignUpForm = Form(tuple("email" -> email, "password" -> nonEmptyText(minLength = 8)))
  val signUpForm = Form(tuple("firstName" -> nonEmptyText, "lastName" -> nonEmptyText, "email" -> email,
    "password" -> optional(nonEmptyText(minLength = 8)), "currCompany" -> nonEmptyText, "currRole" -> nonEmptyText,
    "currGame" -> nonEmptyText ))
  val jobCardForm = Form(tuple("role" -> nonEmptyText, "company" -> nonEmptyText, "startDate" -> optional(date),
      "location" -> nonEmptyText, "description" -> nonEmptyText))
  val employerForm = Form(tuple("company" -> nonEmptyText, "name" -> nonEmptyText,
      "email" -> email, "phone" -> nonEmptyText))
  val updateForm = Form(tuple("oldpassword" -> nonEmptyText, "newpassword" -> optional(nonEmptyText(minLength = 8)),
      "oldemail" -> email, "newemail" -> optional(email)))
  val eventsForm = Form("since" -> longNumber)
  val inviteForm = Form(tuple("email" -> email, "makerid" -> longNumber))
  val resetPasswordForm = Form("email" -> email)

  case class SendMessageRequest(receiverId: Long, subject: String, message: String)

  object implicits {
    implicit val platformReads = Json.reads[Platform]
    implicit val gameReads = Json.reads[Game]
    implicit val roleReads = Json.reads[Role]
    implicit val companyReads = Json.reads[Company]
    implicit val roughDateReads = Json.reads[RoughDate]
    implicit val creditReads = Json.reads[GameCredit]
    implicit val addressReads = Json.reads[AdditionalInfo]
    implicit val makerReads = Json.reads[GameMaker]
    implicit val recruiterReads = Json.reads[Recruiter]
    implicit val settingsReads = Json.reads[UserSettings]
    implicit val sendMessageRequestReads = Json.reads[SendMessageRequest]
    implicit val joinRequestReads = Json.reads[JoinRequest]
  }
}

