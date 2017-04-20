package controllers

import play.api.mvc._
import org.slf4j.Logger
import play.api.libs.json._
import models._
import ModelImplicits._
import com.mohiva.play.silhouette.api.Silhouette
import silhouette.JWTEnv

trait JobCardController {
    self: Controller with DatabaseModule with S3StoreModule with EmailModule =>

  def logger: Logger
  def silhouette: Silhouette[JWTEnv]

  val CANDIDATE_EMAIL = "candidate@gamesmith.com"
  val FROM_EMAIL = "invited@gamesmith.com"

  def getJobCardsForRecruiter = silhouette.SecuredAction { implicit request =>
    if (request.identity.recruiter.isDefined){
      Ok(Json.toJson(databaseService.getJobCardsForRecruiter(request.identity.id)))
    } else {
      NotFound
    }
  }

  def getJobCard(id: Long) = silhouette.UserAwareAction { implicit request =>
    databaseService.getJobCards(List(id), request.identity.map(_.id).getOrElse(-1)).headOption.map { card =>
      Ok(Json.toJson(card))
    }.getOrElse(NotFound)
  }

  def applyToJob(id: Long) = silhouette.SecuredAction { implicit request =>
    val userId = request.identity.id
    val resultOpt = for {
      jobCard <- databaseService.getJobCards(List(id), userId).headOption
      maker <- databaseService.getMaker(userId)
      recruiter <- databaseService.getRecruiter(jobCard.recruiter.id)
    } yield {
      if (emailService.send(recruiter.email, "Gamesmith has a new candidate for you",
          EmailRenderer.renderJobApply(jobCard, maker, databaseService.getSettings(userId)), FROM_EMAIL, CANDIDATE_EMAIL)) {
        databaseService.createApplication(userId, id)
        NoContent
      } else {
        ServiceUnavailable
      }
    }
    resultOpt.getOrElse(NotFound)
  }

  def createJobCard = silhouette.SecuredAction { implicit request =>
    RequestArgs.jobCardForm.bindFromRequest()(request).value.map { case (role, company, startDate, location, desc) =>
      val newRole = databaseService.upsertRoles(List(role)).head
      val newCompany = databaseService.upsertCompanies(List(company)).head
      val toInsert = JobCard(-1, newRole, newCompany,  startDate, location, desc, false, None, None, null)
      val id = databaseService.createJobCard(toInsert, request.identity.id)
      Ok(Json.toJson(databaseService.getJobCards(List(id), request.identity.id).head))
    }.getOrElse(BadRequest)
  }

  def deleteJobCard(id: Long) = silhouette.SecuredAction { implicit request =>
    databaseService.getJobCards(List(id), request.identity.id).headOption.map { card =>
      if (card.recruiter.id == request.identity.id || request.identity.isSuperuser) {
        databaseService.deleteJobCard(id)
        NoContent
      } else {
        Forbidden
      }
    }.getOrElse(NotFound)
  }

  def updateJobCard(id: Long) = silhouette.SecuredAction { implicit request =>
    RequestArgs.jobCardForm.bindFromRequest()(request).value.map { case (role, company, startDate, location, desc) =>
      databaseService.getJobCards(List(id), request.identity.id).headOption.map { card =>
        if (card.recruiter.id == request.identity.id || request.identity.isSuperuser) {
          databaseService.updateJobCard(JobCard(id,
            databaseService.upsertRoles(List(role)).head,
            databaseService.upsertCompanies(List(company)).head,
            startDate,location, desc, false, None, None, card.recruiter))
          NoContent
        } else {
          Forbidden
        }
      }.getOrElse(NotFound)
    }.getOrElse(BadRequest)
  }

  def uploadJobLogo(id: Long) = silhouette.SecuredAction {
    implicit request =>
      val resultOpt = for {
        multipart <- request.body.asMultipartFormData
        filePart <- multipart.file("jobLogo")
        if filePart.contentType.isDefined
      } yield {
        databaseService.getJobCards(List(id), request.identity.id).headOption.map { card =>
          if (card.recruiter.id == request.identity.id || request.identity.isSuperuser) {
            logger.info("Picture data: " + filePart)
            s3StoreService.putPicture(S3Store.RECRUITER_LOGO_BUCKET_NAME, id, filePart)
            val jobLogoUrl = s3StoreService.getPictureUrl(S3Store.RECRUITER_LOGO_BUCKET_NAME, id)
            logger.info("Uploaded job logo in S3 for {}, {}", request.identity.id, jobLogoUrl)
            logger.info("URL: " + jobLogoUrl)
            databaseService.updateJobLogo(id, jobLogoUrl)
            NoContent
          } else {
            logger.info("User not authorized {}", id)
            Forbidden
          }
        }.getOrElse(NotFound)
      }
      resultOpt.getOrElse(BadRequest)
  }
}
