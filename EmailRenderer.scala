package controllers

import models._
import play.twirl.api.Html
import org.jsoup.Jsoup

object EmailRenderer {
  def renderMakerInvitation(gameMaker: GameMaker, email: String, password: String, senderName: Option[String] = None) = {
    val plainHtml = views.html.invitation_email(gameMaker, email, password, senderName)
    inlineCss(plainHtml)
  }

  def renderMakerSignUpInvitation(gameMaker: GameMaker, email: String, senderName: Option[String] = None) = {
    val plainHtml = views.html.signup_invitation_email(gameMaker, email, senderName)
    inlineCss(plainHtml)
  }

  def renderRecruiterInvitation(recruiter: Recruiter, email: String, password: String) = {
    val plainHtml = views.html.recruiter_email(recruiter, email, password)
    inlineCss(plainHtml)
  }

  def renderNewPassword(newPassword: String) = {
    val plainHtml = views.html.password_change(newPassword)
    inlineCss(plainHtml)
  }

  def renderConnectionRequest(requestor: User, requestee: User) = {
    val plainHtml = views.html.connection_request(requestor, requestee)
    inlineCss(plainHtml)
  }

  def renderConnectionAccept(requestor: User, requestee: User) = {
    val plainHtml = views.html.connection_accept(requestor, requestee)
    inlineCss(plainHtml)
  }

  def renderPasswordReset(profile: Profile, email: String, password: String) = {
    val plainHtml = views.html.reset_password(profile, email, password)
    inlineCss(plainHtml)
  }

  def renderJobApply(jobCard: JobCard, maker: User, settings: UserSettings) = {
    val plainHtml = views.html.job_apply(jobCard, maker, settings)
    inlineCss(plainHtml)
  }

  def renderEmployerApply(company: String, name: String, email: String, phone: String) = {
    val plainHtml = views.html.employer_apply(company, name, email, phone)
    inlineCss(plainHtml) 
  }

  private def inlineCss(html: Html) = {
    val document = Jsoup.parse(html.body)
    emailStyles.foreach { case (selector, styles) =>
      document.select(selector).attr("style", styles.mkString(";"))
    }
    Html(document.toString)
  }

  val emailStyles = Map(
    "body" -> List(
      "margin: 0",
      "padding: 0",
      "position: relative",
      "-webkit-font-smoothing: antialiased",
      "-moz-osx-font-smoothing: grayscale"
    )
  )
}
