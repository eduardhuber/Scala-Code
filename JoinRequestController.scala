package controllers

import play.api.mvc._
import play.api.libs.json._
import models._
import ModelImplicits._
import RequestArgs.implicits._
import com.mohiva.play.silhouette.api.Silhouette
import com.mohiva.play.silhouette.api.util.PasswordHasher
import rabbitmq.{ConnectWithAlex, MessageQueueModule, Topic}
import silhouette.{JWTEnv, SuperUserAuthorizationCheck}
import org.slf4j.Logger

trait JoinRequestController {
  self: Controller with DatabaseModule with EmailModule with MessageQueueModule =>

  def logger: Logger
  def silhouette: Silhouette[JWTEnv]
  def passwordHasher: PasswordHasher

  def createJoinRequest = silhouette.UnsecuredAction { implicit request =>
    val resultOpt = for {
      json <- request.body.asJson
      request <- json.asOpt[JoinRequest]
    } yield {
      databaseService.createJoinRequest(request).map { id =>
        Ok(Json.toJson(request.copy(id = id)))
      }.getOrElse(BadRequest(Errors.emailExistsError))
    }
    resultOpt.getOrElse(BadRequest)
  }

  def createManualJoinRequest = silhouette.UnsecuredAction { implicit request =>
    RequestArgs.signUpForm.bindFromRequest()(request).value.map { case (firstName, lastName, email, password, currCompany, currRole, currGame) =>
      val hashedPass = passwordHasher.hash(password.get).password
      val toInsert = JoinRequest(-1, firstName, lastName, email, Some(hashedPass), currCompany, currRole, currGame, None)
      databaseService.createJoinRequest(toInsert).map { id =>
        Ok(Json.toJson(databaseService.getJoinRequest(id)))
      }.getOrElse(BadRequest(Errors.emailExistsError))
    }.getOrElse(BadRequest)
  }


  def listJoinRequests = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    Ok(Json.toJson(databaseService.listJoinRequests()))
  }

  def approveJoinRequest(id: Long) = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    val joinRequest = databaseService.getJoinRequest(id)
    val tempPassword = scala.util.Random.alphanumeric.take(20).mkString("")
    var hashedPass = passwordHasher.hash(tempPassword).password
    if (!joinRequest.get.password.isEmpty) {
        hashedPass = joinRequest.get.password.getOrElse("")
    }
    databaseService.getJoinRequest(id).map { request =>
      databaseService.approveJoinRequest(request, hashedPass).map { maker =>
        // Send message to connect to Alex
        messageQueueService.publish(new ConnectWithAlex(maker.id), Topic.CONNECT_ALEX)
        
        val user = databaseService.getMaker(maker.id).get
          if (joinRequest.get.password.isEmpty) {
            emailService.send(user.email, "You've been invited to Gamesmith",
              EmailRenderer.renderMakerInvitation(maker, user.email, tempPassword))
          }else{
            databaseService.setRealUser(maker.id)
            emailService.send(user.email, "You've been invited to Gamesmith",
              EmailRenderer.renderMakerSignUpInvitation(maker, user.email))
          }
        NoContent
      }.getOrElse(BadRequest(Errors.emailMismatchError))
    }.getOrElse(NotFound)
  }

  def denyJoinRequest(id: Long) = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    databaseService.denyJoinRequest(id)
    NoContent
  }
}
