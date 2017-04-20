package controllers

import play.api.mvc._
import play.api.libs.json._
import models._
import ModelImplicits._
import com.mohiva.play.silhouette.api.Silhouette
import silhouette.JWTEnv

trait EmployerController {
    self: Controller with DatabaseModule with EmailModule =>

  val CLIENT_EMAIL = "sales@gamesmith.com"
  def silhouette: Silhouette[JWTEnv]

  def applyAsEmployer = silhouette.UserAwareAction { implicit request =>
    RequestArgs.employerForm.bindFromRequest()(request).value.map { case (company, name, email, phone) =>
      if (emailService.send(CLIENT_EMAIL, name + " applied as employer.",
        EmailRenderer.renderEmployerApply(company, name, email, phone), email)) {
        Ok("done")
      } else {
        ServiceUnavailable
      }
    }.getOrElse(BadRequest)
  }
}
