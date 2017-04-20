package controllers

import play.api.mvc._
import play.api.libs.json.Json
import models._
import ModelImplicits._
import com.mohiva.play.silhouette.api.Silhouette
import silhouette.JWTEnv

trait InteractionsController {
  self: Controller with DatabaseModule with EmailModule =>

  def silhouette: Silhouette[JWTEnv]
  /**
    * Require that the user has connection setting enabled to proceed further.
    */
  def requireConnectionEnabled(body: Long => Result) = silhouette.SecuredAction { implicit request =>
      val settings = databaseService.getSettings(request.identity.id)
      if (!settings.memberConnections) {
        // user has disabled member connections.
        BadRequest(Errors.disabledMemberConnectionsError)
      } else {
        body(request.identity.id)
      }
  }

  def verifyCredit(id: Long, decision: String) = silhouette.SecuredAction { implicit request =>
    databaseService.verifyCredit(request.identity.id, id, decision == "yes")
    if (decision == "yes") {
      databaseService.newEvent(
        request.identity.profile.fullName,
        "just verified",
        databaseService.getGameMakerByCreditId(id).get.fullName)
    }
    NoContent
  }

  def deleteCredit(makerId: Long, creditId: Long) = silhouette.SecuredAction { implicit request =>
    if (makerId == request.identity.id || request.identity.isSuperuser) {
      databaseService.deleteCredit(makerId, creditId)
      NoContent
    } else {
      Forbidden
    }
  }

  def listPendingConnections = silhouette.SecuredAction { implicit request =>
    Ok(Json.toJson(databaseService.getPendingConnections(request.identity.id)))
  }

  def requestConnect(id: Long) = requireConnectionEnabled {
    case userId =>
      val settings = databaseService.getSettings(id)
      if (!settings.memberConnections) {
        // If the user being asked to connect has settings disabled, throw an error.
        // In the ideal case the UI should not allow this.
        BadRequest(Errors.disabledMemberConnectionsError)
      } else {
        val resultOpt = for {
          requestor <- databaseService.getUser(userId)
          target <- databaseService.getUser(id)
        } yield {
          databaseService.requestConnect(userId, id)
          if (emailService.send(target.email, s"${requestor.profile.firstName} wants to connect with you.",
            EmailRenderer.renderConnectionRequest(requestor, target))) {
            NoContent
          } else {
            ServiceUnavailable
          }
        }
        resultOpt.getOrElse(NotFound)
      }
  }

  def approveConnect(id: Long) = requireConnectionEnabled {
    case userId =>
      val resultOpt = for {
        requestor <- databaseService.getUser(id)
        target <- databaseService.getUser(userId)
        if databaseService.hasPendingRequest(id, userId)
      } yield {
        databaseService.approveConnect(userId, id)
        if (emailService.send(requestor.email, s"${target.profile.firstName} has accepted your request.",
          EmailRenderer.renderConnectionAccept(requestor, target))) {
          databaseService.newEvent(requestor.profile.fullName, "just connected with", target.profile.fullName)
          NoContent
        } else {
          ServiceUnavailable
        }
      }
      resultOpt.getOrElse(NotFound)
  }

  def rejectConnect(id: Long) = requireConnectionEnabled {
    case userId =>
      if (databaseService.hasPendingRequest(id, userId)) {
        databaseService.rejectConnect(userId, id)
        NoContent
      } else {
        NotFound
      }
  }
}
