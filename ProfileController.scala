package controllers

import org.slf4j.Logger
import play.api.mvc._
import play.api.libs.json._
import models._
import ModelImplicits._
import RequestArgs.implicits._
import com.mohiva.play.silhouette.api.Silhouette
import com.mohiva.play.silhouette.api.util.{Credentials, PasswordHasher, PasswordInfo}
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import play.api.data.Form
import play.api.data.Forms._
import rabbitmq.{ConnectWithAlex, MessageQueueModule, Topic}
import silhouette.{JWTEnv, SuperUserAuthorizationCheck}
import sms.PhoneValidationClient
import org.joda.time.{ DateTime, LocalDate}
import org.joda.time.format.DateTimeFormat


import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait ProfileController {
  self: Controller with DatabaseModule with S3StoreModule with EmailModule with MessageQueueModule =>

  def logger: Logger

  def silhouette: Silhouette[JWTEnv]

  def passwordHasher: PasswordHasher

  def credentialsProvider: CredentialsProvider

  def authInfoRepository: PasswordInfoDao

  def twilioSmsClient: PhoneValidationClient

  def createMaker = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    val newId = databaseService.createMaker()
    databaseService.inviteMaker("", "", newId)
    // Fire message to connect with Alex when a maker card is created.
    messageQueueService.publish(new ConnectWithAlex(newId), Topic.CONNECT_ALEX)
    Ok(Json.obj("id" -> newId))
  }

  def directSignUp = silhouette.UnsecuredAction { implicit request =>
    RequestArgs.directSignUpForm.bindFromRequest().value.map { case (email, password) =>
      val newId = databaseService.createMaker()
      val hashedPass = passwordHasher.hash(password).password
      val userId = databaseService.createUser(email, hashedPass, newId)
      if(userId != 0) {
        Ok(Json.obj("id" -> userId))
      }else{
        BadRequest(Errors.emailExistsError)
      }
    }.getOrElse(BadRequest)
  }

  def updatePassword = silhouette.SecuredAction.async { implicit request =>
    (for {
      (oldPassword, newPasswordOpt, email, _) <- RequestArgs.updateForm.bindFromRequest()(request).value
      newPassword <- newPasswordOpt
    } yield {
      val credentials = Credentials(request.identity.email, oldPassword)
      for {
        loginInfo <- credentialsProvider.authenticate(credentials)
        newAuthInfo: PasswordInfo = passwordHasher.hash(newPassword)
        _ <- authInfoRepository.update(loginInfo, newAuthInfo)
        authenticator <- silhouette.env.authenticatorService.create(loginInfo)
        result <- silhouette.env.authenticatorService.renew(authenticator, NoContent)
      } yield {
        request.identity.maker.foreach { maker =>
          if (!maker.claimed.getOrElse(false))
            databaseService.newEvent(maker.fullName, "just joined", "Gamesmith")
        }
        NoContent
      }
    }).getOrElse(Future.successful(BadRequest(Json.obj(
      "message" -> "oldPassword and newPassword must both be specified in form"))))
  }

  def resetPassword = silhouette.UnsecuredAction { implicit request =>
    val resultOpt = for {
      email <- RequestArgs.resetPasswordForm.bindFromRequest()(request).value
      (id, _) <- databaseService.getLoginInfo(email)
      profile <- databaseService.getProfile(id)
    } yield {
      val tempPassword = scala.util.Random.alphanumeric.take(20).mkString("")
      if (emailService.send(email, "Your Gamesmith password has been reset.",
        EmailRenderer.renderPasswordReset(profile, email, tempPassword))) {
        databaseService.updatePassword(id, passwordHasher.hash(tempPassword).password)
        NoContent
      } else {
        ServiceUnavailable
      }
    }
    resultOpt.getOrElse(BadRequest(Errors.emailNotFoundError))
  }

  def updateEmail = silhouette.SecuredAction.async { implicit request =>
    val resultOpt = for {
      (oldPassword, _, email, newEmailOpt) <- RequestArgs.updateForm.bindFromRequest()(request).value
      newEmail <- newEmailOpt
    } yield {
      val credentials = Credentials(email, oldPassword)
      for {
        loginInfo <- credentialsProvider.authenticate(credentials)
      } yield {
        if (databaseService.updateEmail(request.identity.id, newEmail))
          NoContent
        else
          BadRequest(Errors.emailExistsError)
      }

    }
    resultOpt.getOrElse(Future.successful(Forbidden))
  }

  def getSettings = silhouette.SecuredAction { implicit request =>
    Ok(Json.toJson(databaseService.getSettings(request.identity.id)))
  }

  def updateSettings = silhouette.SecuredAction { implicit request =>
    val resultOpt = for {
      json <- request.body.asJson
      settings <- json.asOpt[UserSettings]
    } yield {
      databaseService.updateSettings(request.identity.id, settings)
      NoContent
    }
    resultOpt.getOrElse(BadRequest)
  }

  def updateCredits(id: Long) = silhouette.SecuredAction { implicit request =>
    if (!(id == request.identity.id || request.identity.isSuperuser)) {
      Forbidden
    } else {
      // TODO: More verification!
      val resultOpt = for {
        json <- request.body.asJson
        credits <- json.asOpt[List[GameCredit]]
      } yield {
        val gamesFromCredits = credits.map(_.game)
        val allGames = if (gamesFromCredits.nonEmpty) {
          databaseService.upsertGames(gamesFromCredits).map { game =>
            game.name -> game
          }.toMap
        } else {
          Map.empty[String, Game]
        }

        val rawRoles = credits.flatMap(_.role.map(_.name)).distinct
        val allRoles = if (rawRoles.nonEmpty) {
          databaseService.upsertRoles(rawRoles).map { role =>
            role.name -> role
          }.toMap
        } else {
          Map.empty[String, Role]
        }

        val rawCompanies = credits.flatMap(_.company.map(_.name))
        val allCompanies = if (rawCompanies.nonEmpty) {
          databaseService.upsertCompanies(rawCompanies).map { company =>
            company.name -> company
          }.toMap
        } else {
          Map.empty[String, Company]
        }

        val resolvedCredits = credits.map { credit =>
          credit.copy(
            game = allGames(credit.game.name),
            role = credit.role.map { r => allRoles(r.name) },
            company = credit.company.map { c => allCompanies(c.name) })
        }

        databaseService.upsertCredits(id, resolvedCredits)
        NoContent
      }
      resultOpt.getOrElse(BadRequest)
    }
  }

  def updateProfile(id: Long) = silhouette.SecuredAction { implicit request =>
    if (!(id == request.identity.id || request.identity.isSuperuser)) {
      Forbidden
    } else {
      val resultOpt = for {
        json <- request.body.asJson
        profile <- json.asOpt[GameMaker].orElse(json.asOpt[Recruiter])
      } yield {
        databaseService.updateProfile(id, profile)
        NoContent
      }
      resultOpt.getOrElse(BadRequest)
    }
  }

  def deleteProfile(id: Long) = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    if (id != 1) {
      databaseService.deleteUser(id)
      NoContent
    } else {
      BadRequest
    }
  }

  def updateRecruiterLogo(id: Long) = silhouette.SecuredAction { implicit request =>
    val resultOpt = for {
      multipart <- request.body.asMultipartFormData
      filePart <- multipart.file("logo")
      if filePart.contentType.isDefined
    } yield {
      databaseService.getRecruiterById(id).map { recruiter =>
        if (id == request.identity.id || request.identity.isSuperuser) {
          s3StoreService.putPicture(S3Store.RECRUITER_LOGO_BUCKET_NAME, id, filePart)
          databaseService.setRecruiterLogo(recruiter.id)
          NoContent
        } else {
          Forbidden
        }
      }.getOrElse(NotFound)
    }

    resultOpt.getOrElse(BadRequest)
  }

  def updateProfilePicture(id: Long) = silhouette.SecuredAction {
    implicit request =>
      val resultOpt = for {
        multipart <- request.body.asMultipartFormData
        filePart <- multipart.file("picture")
        if filePart.contentType.isDefined
      } yield {
        if (id == request.identity.id || request.identity.isSuperuser) {
          logger.info("Picture data: " + filePart)
          s3StoreService.putPicture(S3Store.PROFILE_PICTURE_BUCKET_NAME, id, filePart)
          val pictureUrl = s3StoreService.getPictureUrl(S3Store.PROFILE_PICTURE_BUCKET_NAME, id)
          logger.info("Uploaded profile picture in S3 for {}, {}", request.identity.id, pictureUrl)
          logger.info("URL: " + pictureUrl)
          databaseService.updateProfileImg(id, pictureUrl)
          NoContent
        } else {
          logger.info("User not authorized {}", id)
          Forbidden
        }
      }
      resultOpt.getOrElse(BadRequest)
  }

  def sendPhoneValidationCode = silhouette.SecuredAction.async { implicit request =>
    databaseService.getUser(request.identity.id).flatMap(_.maker.flatMap(_.phoneNumber)).fold(
      Future.successful(BadRequest("Failed to find phone number for logged in user"))) { phoneNumber =>

      val smsCode = 100000 + scala.util.Random.nextInt(900000)
      databaseService.saveSmsCode(request.identity.id, smsCode)
      for {
        smsResponseEither <- twilioSmsClient.sendSms(phoneNumber, smsCode)
      } yield {
        smsResponseEither match {
          case Left(errorResponse) =>
            //TODO: Decide how detailed to get with what needs to get communicated to front-end!
            val m = s"Error triggering SMS with Twilio: ${errorResponse.toString}"
            logger.warn(m)
            BadRequest(m)

          case Right(_) => NoContent
        }
      }
    }
  }

  val smsCodeForm = Form("smsCode" -> number)
  def validatePhoneCode = silhouette.SecuredAction { implicit request =>
    smsCodeForm.bindFromRequest().value.map { smsCode =>
      databaseService.validateSmsCode(request.identity.id, smsCode) match {
        case Left(error) => BadRequest(Json.obj("error" -> error))
        case Right(Some(validationFailedResult)) => BadRequest(Json.toJson(validationFailedResult))
        case Right(None) => NoContent
      }
    }.getOrElse(BadRequest(Json.obj("error" -> "smsCode param must be set")))

  }

}

