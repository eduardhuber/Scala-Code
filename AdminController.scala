package controllers

import models._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.iteratee.Enumerator
import play.api.mvc._
import ModelImplicits._
import com.mohiva.play.silhouette.api.Silhouette
import com.mohiva.play.silhouette.api.util.PasswordHasher
import org.apache.commons.csv._
import play.api.libs.json.Json
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.libs.concurrent.Execution.Implicits._
import rabbitmq.{ConnectWithAlex, MessageQueueModule, Topic}
import silhouette.{JWTEnv, SuperUserAuthorizationCheck}
import org.joda.time.{ DateTime, LocalDate }

trait AdminController {
  self: Controller with DatabaseModule with EmailModule with MessageQueueModule =>


  def silhouette: Silhouette[JWTEnv]
  def passwordHasher: PasswordHasher

  val passwordForm = Form(tuple("id" -> longNumber, "password" -> nonEmptyText(minLength = 8)))
  val emailForm = Form(tuple("id" -> longNumber, "email" -> email))
  val inviteMakerForm = Form(tuple("email" -> email, "makerid" -> longNumber))
  val inviteRecruiterForm = Form(tuple("email" -> email, "password" -> nonEmptyText(minLength = 8), "firstname" -> nonEmptyText,
    "lastname" -> nonEmptyText, "currcompany" -> nonEmptyText, "location" -> nonEmptyText))
  val inviteAdminForm = Form(tuple("email" -> email, "firstname" -> nonEmptyText, "lastname" -> nonEmptyText, "password" -> nonEmptyText(minLength = 8)))

  def inviteMaker = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    inviteMakerForm.bindFromRequest()(request).value.map { case (email, makerId) =>
      val tempPassword = util.Random.alphanumeric.take(20).mkString("")
      val hashedPass = passwordHasher.hash(tempPassword).password
      databaseService.inviteMaker(email, hashedPass, makerId).map { maker =>
        if (emailService.send(email, "You've been invited to Gamesmith",
          EmailRenderer.renderMakerInvitation(maker, email, tempPassword))) {
          Ok(Json.toJson(maker))
        } else {
          ServiceUnavailable
        }
      }.getOrElse {
        BadRequest(Errors.emailMismatchError)
      }
    }.getOrElse(BadRequest)
  }

  def inviteRecruiter = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    inviteRecruiterForm.bindFromRequest()(request).value.map { case (email, password, firstName, lastName, currCompany, location) =>
      val hashedPass = passwordHasher.hash(password).password
      databaseService.inviteRecruiter(email, hashedPass, Recruiter(0, firstName, lastName, currCompany, None,
        Some(location), false, None, None, None, None, phoneNumberValidated = Option(false),
        status = None, availability = None), false).map { recruiter =>
        if (emailService.send(email, "You've been invited to Gamesmith",
          EmailRenderer.renderRecruiterInvitation(recruiter, email, password))) {
          Ok(Json.toJson(recruiter))
        } else {
          ServiceUnavailable
        }
      }.getOrElse {
        BadRequest(Errors.emailMismatchError)
      }
    }.getOrElse(BadRequest)
  }

  def inviteAdmin = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    inviteAdminForm.bindFromRequest()(request).value.map { case (email, firstName, lastName, password) =>
      val hashedPass = passwordHasher.hash(password).password
      databaseService.inviteRecruiter(email, hashedPass,
        Recruiter(0, firstName, lastName, "", None ,Some(""), false, None, None, None, None,
          phoneNumberValidated = Option(false), status = None, availability = None), true).map { recruiter =>
        Ok(Json.toJson(recruiter))
      }.getOrElse {
        BadRequest(Errors.emailMismatchError)
      }
    }.getOrElse(BadRequest)
  }

  def sudoUpdatePassword = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    passwordForm.bindFromRequest()(request).value.map { case (id, password) =>
      databaseService.getUser(id).map { user =>
        databaseService.updatePassword(id, passwordHasher.hash(password).password)
        user.recruiter.map { recruiter =>
          if (emailService.send(user.email, "Your password has been updated",
            EmailRenderer.renderNewPassword(password))) {
            NoContent
          } else {
            ServiceUnavailable
          }
        }.getOrElse(NoContent)
      }.getOrElse(NotFound)
    }.getOrElse(BadRequest)
  }

  def sudoUpdateEmail = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    emailForm.bindFromRequest()(request).value.map { case (id, email) =>
      if (databaseService.sudoUpdateEmail(id, email))
        NoContent
      else
        BadRequest(Errors.emailExistsError)
    }.getOrElse(BadRequest)
  }

  val csvFormat = CSVFormat.EXCEL
    .withRecordSeparator("\n")

  def downloadUserPrefs = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    val users = databaseService.readAllAccounts
    val csvHeader = Iterator.single("Name,Email,Job Title, Current Game Name,Phone Number,Availability,Company,Location,Member Connections,Member Messages,Surveys,Job Enquiries\n")
    val csvLines = users.map { case (account, settings) =>
      csvFormat.format(account.profile.fullName, account.email,account.maker.get.currRole, account.maker.get.currGame, account.profile.phoneNumber.getOrElse(""),account.profile.availability.getOrElse(""),account.maker.get.currCompany,account.profile.location.getOrElse(""), settings.memberConnections.toString,
        settings.memberMessaging.toString, settings.surveys.toString, settings.jobEnquiries.toString) + "\n"
    }
    val enumerator = Enumerator.enumerate(csvHeader ++ csvLines)
    Ok.chunked(enumerator).withHeaders(
      "Content-Disposition" -> "filename=gamesmith_users.csv",
      "Content-Type" -> "text/csv")
  }

  def connectWithAlex(id: Long) = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
      messageQueueService.publish(new ConnectWithAlex(id), Topic.CONNECT_ALEX)
      Ok("done")
  }
}
