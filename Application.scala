package controllers

import java.util.concurrent.atomic.AtomicReference
import javax.inject.{Inject, Singleton}

import play.api._
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.api.libs.json.{JsObject, Json}
import RequestArgs.implicits._
import models._
import ModelImplicits._
import akka.actor.{ActorSystem, Props}
import com.mohiva.play.silhouette.api.Silhouette
import com.mohiva.play.silhouette.api.actions.SecuredErrorHandler
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import org.slf4j.LoggerFactory
import play.api.i18n.{I18nComponents, MessagesApi}
import rabbitmq.RabbitMQModule
import silhouette.{JWTEnv, SilhouetteEnvironmentModule, SuperUserAuthorizationCheck}
import sms.{PhoneValidationClient, TwilioPhoneValidationClient}
import sun.misc.BASE64Encoder
import sun.misc.BASE64Decoder
import play.api.libs.json._

trait ApplicationImpl {
  self: Controller with DatabaseModule with SearchModule with AuthHelper =>

  def silhouette: Silhouette[JWTEnv]

  def index = Action {
    Ok(views.html.index())
  }

  def getSelf = silhouette.UserAwareAction { implicit request =>
    val nonce: String = request.getQueryString("nonce").getOrElse("")
    request.identity.map(_.id).flatMap { userId =>
      databaseService.getUser(userId).map { user =>
        if (nonce.length > 0) {
          val externalId = user.id
          val email = user.email
          val data = s"external_id=$externalId&email=$email&nonce=$nonce"
          val base64Payload = new BASE64Encoder().encode(data.getBytes())
          val escapedPayload = base64Payload.replace("\n", "%0A")
          val newSig = sha256(base64Payload)
          val json: JsValue = Json.parse(s"""
              {
                "payload": "$escapedPayload",
                "sig": "$newSig"
              }
            """)
          // silhouette.env.authenticatorService.embed(token,
            Ok(json)
          // )
        } else {
          Ok(Json.toJson(user))
        }
      }
    }.getOrElse(Ok(Json.obj()))
  }

  def getGameMaker(id: Long) = silhouette.UserAwareAction { implicit request =>
    databaseService.getGameMakerById(id, request.identity.map(_.id).getOrElse(-1)).map { maker =>
      Ok(Json.toJson(maker))
    }.getOrElse(NotFound)
  }

  def getGameMakersForGame(id: Long, count: Option[Int], offset: Option[Int]) = silhouette.SecuredAction { implicit request =>
    databaseService.getGameMakersForGame(id, request.identity.id, count.getOrElse(20), offset.getOrElse(0)).map { gmg =>
      Ok(Json.toJson(gmg))
    }.getOrElse(NotFound)
  }

  def browseMakers(count: Option[Int], offset: Option[Int]) = silhouette.UserAwareAction { implicit request =>
    val makerIds = searchService.browseMakers(request.identity.flatMap(_.maker), count.getOrElse(20), offset.getOrElse(0))
    Ok(Json.toJson(databaseService.getGameMakers(request.identity.map(_.id).getOrElse(-1), makerIds).unzip._1))
  }

  def browseJobs(count: Option[Int], offset: Option[Int]) = silhouette.UserAwareAction { implicit request =>
    Ok(Json.toJson(databaseService.browseJobs(request.identity.map(_.id).getOrElse(-1), count.getOrElse(20), offset.getOrElse(0))))
  }

  def browseRecruiters(count: Option[Int], offset: Option[Int]) = silhouette.SecuredAction(SuperUserAuthorizationCheck) {
    implicit request =>
      Ok(Json.toJson(databaseService.browseRecruiters(count.getOrElse(20), offset.getOrElse(0), superuser = false)))
  }

  def browseAdmins(count: Option[Int], offset: Option[Int]) = silhouette.SecuredAction(SuperUserAuthorizationCheck) {
    implicit request =>
      Ok(Json.toJson(databaseService.browseRecruiters(count.getOrElse(20), offset.getOrElse(0), superuser = true)))
  }

  def browseGames = silhouette.SecuredAction { implicit request =>
    Ok(Json.toJson(databaseService.browseGames(request.identity.id)))
  }

  def sudoBrowseMakers(count: Option[Int], offset: Option[Int]) = silhouette.SecuredAction(SuperUserAuthorizationCheck) {
    implicit request =>
      val makerIds = searchService.sudoBrowseMakers(count.getOrElse(20), offset.getOrElse(0))
      val makersWithEmails = databaseService.getGameMakers(-1, makerIds).map { case (maker, emailOpt) =>
        val makerJson = Json.toJson(maker).as[JsObject]
        emailOpt.map { email =>
          makerJson ++ Json.obj("email" -> email)
        }.getOrElse(makerJson)
      }
      Ok(Json.toJson(makersWithEmails))
  }

  def dashboard = silhouette.SecuredAction(SuperUserAuthorizationCheck) { implicit request =>
    Ok(Json.toJson(databaseService.dashboard()))
  }

  def ssoDiscourse(sso: String, sig: String) = Action {
    val calculatedHash = sha256(sso)
    if (calculatedHash != sig) {
      Unauthorized(Json.obj("message" -> "Single Sign On Error: signature did not match secret-key encrypted payload."))
    } else {
      val nonce = new String(new BASE64Decoder().decodeBuffer(sso))
      Ok(views.html.discourse_login(nonce, ssoUrl))
    }
  }
}

@Singleton
class Application @Inject()(val environment: Environment,
                            val configuration: Configuration,
                            val wsClient: WSClient,
                            db: play.api.db.Database,
                            system: ActorSystem) extends Controller
  with ApplicationImpl
  with DefaultDatabaseModule
  with MailgunEmailModule
  with AdminController
  with HomepageEventsController
  with InteractionsController
  with JoinRequestController
  with JobCardController
  with MessagingController
  with ProfileController
  with SearchController
  with EmployerController
  with LuceneSearchModule
  with S3StoreModuleImpl
  with RabbitMQModule
  with SilhouetteEnvironmentModule
  with SilhouetteAuthenticationController
  with I18nComponents
  with SilhouetteAuthentication {

  val logger = LoggerFactory.getLogger(getClass)

  // Singleton, so safe to initalize ref/cache here
  private val gc = {
    val r = new AtomicReference[GamesCacheObject]
    r.set(GamesCacheObject(Nil, 0l))
    r
  }

  //TODO: Move to DI and initialize cache in application bootstrapper
  val gamesCacheUpdaterRef = system.actorOf(Props(
    new GamesSortedByMakerIdCacheUpdater(gc, db)
  ))

  lazy val gamesSortedByMakerIdCache: GamesSortedByMakerIdCache = new AtomicGamesSortedByMakerIdCache(gamesCacheUpdaterRef, gc)

  lazy val twilioSmsClient: PhoneValidationClient = TwilioPhoneValidationClient(wsClient, configuration)
}
