package controllers

import exceptions.MessagingDisabledException
import play.api.mvc._
import play.twirl.api.Txt
import models.DatabaseModule
import RequestArgs.implicits._
import com.mohiva.play.silhouette.api.Silhouette
import com.mohiva.play.silhouette.api.util.PasswordHasher
import org.slf4j.LoggerFactory
import silhouette.JWTEnv

trait MessagingController {
  self: Controller with EmailModule with DatabaseModule =>

  def silhouette: Silhouette[JWTEnv]

  def passwordHasher: PasswordHasher

  val log = LoggerFactory.getLogger(getClass)

  def sendMessage = silhouette.SecuredAction { implicit request =>

    val senderId = request.identity.id
    val settings = databaseService.getSettings(senderId)
    val sentCount = databaseService.getRecentMessageCount(senderId)
    if (sentCount >= 3) {
      log.info("Not sending message. Quota exceeded!")
      BadRequest(Errors.quotaExceededError)
    } else {
      val resultOpt = for {
        json <- request.body.asJson
        sendRequest <- json.asOpt[RequestArgs.SendMessageRequest]
        receiverEmail <- databaseService.getEmail(sendRequest.receiverId)
        senderEmail <- databaseService.getEmail(senderId)
      } yield {
        val receiverSettings = databaseService.getSettings(sendRequest.receiverId)
        // If receiver has disabled messaging preferences, lets not proceed.
        // If the sender is a recruiter and receiver has disabled job enquires, lets not proceed.
        if (!receiverSettings.memberMessaging || (request.identity.recruiter.nonEmpty && !receiverSettings.jobEnquiries)) {
          // Sending messages depend only on receiver settings.
          log.info("Not sending message. Receiver has settings disabled.")
          NoContent
        } else if (emailService.send(receiverEmail, "[Gamesmith] " + sendRequest.subject, Txt(sendRequest.message), senderEmail)) {
          log.info("Message sent!")
          databaseService.recordSentMessage(senderId, sendRequest.receiverId)
          NoContent
        } else {
          ServiceUnavailable
        }
      }
      resultOpt.getOrElse(BadRequest)
    }
  }

  def inviteColleague = silhouette.SecuredAction { implicit request =>
    RequestArgs.inviteForm.bindFromRequest()(request).value.map { case (email, makerId) =>
      val sender = request.identity.profile
      val tempPassword = util.Random.alphanumeric.take(20).mkString("")
      val hashedPass = passwordHasher.hash(tempPassword).password
      databaseService.inviteMaker(email, hashedPass, makerId).map { maker =>
        if (emailService.send(email, "You've been invited to Gamesmith",
          EmailRenderer.renderMakerInvitation(maker, email, tempPassword, Some(sender.firstName)))) {
          databaseService.newEvent(sender.fullName, "just invited", maker.fullName)
          NoContent
        } else {
          ServiceUnavailable
        }
      }.getOrElse {
        BadRequest(Errors.emailMismatchError)
      }
    }.getOrElse(BadRequest)
  }
}
