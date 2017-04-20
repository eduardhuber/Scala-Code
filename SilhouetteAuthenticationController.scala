package controllers

import _root_.exceptions.{UserAlreadyRegisteredException, NoLinkedInUserException, UserNotFoundException}
import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.exceptions.ProviderException
import com.mohiva.play.silhouette.api.services.IdentityService
import com.mohiva.play.silhouette.api.util.Credentials
import com.mohiva.play.silhouette.impl.exceptions.IdentityNotFoundException
import com.mohiva.play.silhouette.impl.providers.CredentialsProvider
import models.ModelImplicits._
import models._
import org.joda.time.DateTime
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc._
import silhouette.{CustomLinkedInProvider, JWTEnv, Token}
import play.api.Logger
import sun.misc.BASE64Encoder

import scala.concurrent.Future

trait SilhouetteAuthenticationController extends Controller with DatabaseModule with AuthHelper {

  implicit val credFormat = _root_.silhouette.formatters.json.CredentialFormat.restFormat
  implicit val tokenFormat = _root_.silhouette.formatters.json.TokenFormat.restFormat

  def silhouette: Silhouette[JWTEnv]

  def credentialsProvider: CredentialsProvider

  def userService: IdentityService[User]

  def linkedInProvider: CustomLinkedInProvider

  def authenticate = silhouette.UnsecuredAction.async(BodyParsers.parse.json[Credentials]) { implicit request =>
    val nonce: String = request.getQueryString("nonce").getOrElse("")
    // Note: In theory I can use a RequestExtractor to not have to pass a query parameter, and handle everything
    // within Silhouette.
    credentialsProvider.authenticate(request.body).flatMap { loginInfo =>
      userService.retrieve(loginInfo).flatMap {
        case Some(user) => silhouette.env.authenticatorService.create(loginInfo).flatMap { authenticator =>
          silhouette.env.eventBus.publish(LoginEvent(user, request))
          silhouette.env.authenticatorService.init(authenticator).flatMap { token =>
            if (nonce.length > 0) {
              val externalId = user.id
              val email = user.email
              val profile = user.profile
              val name = profile.fullName
              var data = ""
              if(profile.imgUrl.isDefined){
                val avatar_url =  profile.imgUrl
                data = s"external_id=$externalId&email=$email&nonce=$nonce&name=$name&avatar_url=$avatar_url"
              }
              else{
                data = s"external_id=$externalId&email=$email&nonce=$nonce&name=$name"
              }
              val base64Payload = new BASE64Encoder().encode(data.getBytes())
              val escapedPayload = base64Payload.replace("\n", "%0A")
              val newSig = sha256(base64Payload)
              val json: JsValue = Json.parse(s"""
                  {
                    "payload": "$escapedPayload",
                    "sig": "$newSig"
                  }
                """)
              silhouette.env.authenticatorService.embed(token,
                Ok(json)
              )
            } else {
              silhouette.env.authenticatorService.embed(token,
                Ok(Json.toJson(Token(token = token, expiresOn = authenticator.expirationDateTime)))
              )
            }

          }
        }
        case None =>
          Future.failed(new IdentityNotFoundException("Couldn't find user"))
      }
    }.recover {
      case e: ProviderException =>

        Unauthorized(Json.obj("message" -> "Invalid credentials"))
    }
  }

  def updateProfile(profileId: Long, profile: Profile, socialProfile: SocialProfile, prioritizeSocial: Boolean = false): Unit = {
    def convertEmptyToNone(o: Option[String]) = {
      o match {
        case None => None
        case Some("") => None
        case Some(x) => Some(x)
      }
    }

    def merge(s: String, o:Option[String]): String = {
      if (s.isEmpty) o.getOrElse("") else s
    }

    val updatedProfile = profile match {
      case m: GameMaker =>
        val updatedMaker = m.copy(
          firstName = if (prioritizeSocial) socialProfile.firstName.getOrElse(m.firstName) else merge(m.firstName, socialProfile.firstName),
          lastName = if (prioritizeSocial) socialProfile.lastName.getOrElse(m.lastName) else merge(m.lastName, socialProfile.lastName),
          currRole = if (prioritizeSocial) socialProfile.currentTitle.getOrElse(m.currRole) else merge(m.currRole, socialProfile.currentTitle),
          currCompany = if (prioritizeSocial) socialProfile.currentCompany.getOrElse(m.currCompany) else merge(m.currCompany, socialProfile.currentCompany),
          imgUrl = convertEmptyToNone(m.imgUrl).orElse(socialProfile.avatarUrl),
          bio = convertEmptyToNone(m.bio).orElse(socialProfile.bio),
          location = convertEmptyToNone(m.location).orElse(socialProfile.location),
          additionalInfo = m.additionalInfo)
        updatedMaker
      case r: Recruiter =>
        val updatedRecruiter =
          r.copy(
            firstName = if (prioritizeSocial) socialProfile.firstName.getOrElse(r.firstName) else merge(r.firstName, socialProfile.firstName),
            lastName = if (prioritizeSocial) socialProfile.lastName.getOrElse(r.lastName) else merge(r.lastName, socialProfile.lastName),
            currCompany = if (prioritizeSocial) socialProfile.currentCompany.getOrElse(r.currCompany) else merge(r.currCompany, socialProfile.currentCompany),
            imgUrl = convertEmptyToNone(r.imgUrl).orElse(socialProfile.avatarUrl),
            bio = convertEmptyToNone(r.bio).orElse(socialProfile.bio),
            location = convertEmptyToNone(r.location).orElse(socialProfile.location))
        updatedRecruiter

      case b: BasicProfile =>
        b.copy(firstName = if (prioritizeSocial) socialProfile.firstName.getOrElse(b.firstName) else merge(b.firstName, socialProfile.firstName),
          lastName = if (prioritizeSocial) socialProfile.lastName.getOrElse(b.lastName) else merge(b.lastName, socialProfile.lastName),
          imgUrl = convertEmptyToNone(b.imgUrl).orElse(socialProfile.avatarUrl),
          bio = convertEmptyToNone(b.bio).orElse(socialProfile.bio),
          location = convertEmptyToNone(b.location).orElse(socialProfile.location))
    }

    databaseService.updateProfile(profileId, updatedProfile)
  }


  def acceptMakerViaLinkedIn = silhouette.SecuredAction.async {
    implicit request =>
      linkedInProvider.authenticate().flatMap {
        case Left(result) => Future.successful(result)
        case Right(authInfo) => for {
          profile <- linkedInProvider.retrieveProfile(authInfo)
        } yield {
          // store profile.
//          if(profile.email.get == request.identity.email ){
            val socialProfile: SocialProfile = createSocialProfile(Some(request.identity.id), profile)
            databaseService.upsertSocialProfile(socialProfile)
            databaseService.setRealUser(request.identity.id)

            request.identity.profile match {
              case m: GameMaker =>
                updateProfile(m.id, m, socialProfile, prioritizeSocial = true)
                databaseService.getUser(request.identity.id).map { user =>
                  Ok(Json.toJson(user))
                }.getOrElse(BadRequest)
              case _ => BadRequest("not a maker.")
            }
//          } else{
//            NotFound
//          }
        }
      }
  }

  def  createSocialProfile(id: Option[Long],
                          profile: CustomLinkedInProvider#Profile): SocialProfile = {
    SocialProfile(
      profile.loginInfo.providerID,
      profile.loginInfo.providerKey,
      profile.email,
      profile.firstName,
      profile.lastName,
      profile.fullName,
      profile.avatarURL,
      DateTime.now(),
      profile.currentPosition.map(_.company.name),
      profile.currentPosition.flatMap(_.title),
      id,
      bio = profile.summary,
      location = profile.location
    )
  }

  def linkedInSignIn = silhouette.UnsecuredAction.async {
    implicit request =>
      linkedInProvider.authenticate().flatMap {
        case Left(result) =>
          Future.successful(result)
        case Right(authInfo) =>
          linkedInProvider.retrieveProfile(authInfo).flatMap {
            profile =>
              val socialProfileOpt =
                databaseService.getSocialProfile(profile.loginInfo.providerID, profile.loginInfo.providerKey)

              socialProfileOpt match {
                case Some(s) if s.accountId.nonEmpty =>
                  val sp: SocialProfile = createSocialProfile(s.accountId, profile)
//                  databaseService.upsertSocialProfile(sp)
                  databaseService.getProfile(s.accountId.get).foreach {
                    p =>
                      updateProfile(p.id, p, sp)
                  }

                  // The user is in the system.
                  databaseService.getUser(s.accountId.get).map {
                    user =>
                      val loginInfo = LoginInfo("credentials", user.email)
                      silhouette.env.authenticatorService.create(loginInfo).flatMap { authenticator =>
                        silhouette.env.eventBus.publish(LoginEvent(user, request))
                        silhouette.env.authenticatorService.init(authenticator).flatMap { token =>
                          silhouette.env.authenticatorService.embed(token,
                            Ok(Json.toJson(Token(token = token, expiresOn = authenticator.expirationDateTime)))
                          )
                        }
                      }
                  }.getOrElse(throw UserNotFoundException())
                case _ =>
                  throw NoLinkedInUserException()

              }
          }
      }
  }

  def linkedInJoinRequest = silhouette.UnsecuredAction.async {
    implicit request =>
      linkedInProvider.authenticate().flatMap {
        case Left(result) =>
          Future.successful(result)
        case Right(authInfo) => for {
          profile <- linkedInProvider.retrieveProfile(authInfo)
        } yield {
          val userWithEmail = profile.email.flatMap {
            email =>
              databaseService.getLoginInfo(email)
          }
          if (userWithEmail.nonEmpty) {
            throw UserAlreadyRegisteredException()
          }
          val joinrequest = JoinRequest(-1,
            profile.firstName.getOrElse(""),
            profile.lastName.getOrElse(""),
            profile.email.getOrElse(""),
            None,
            profile.currentPosition.map(_.company.name).getOrElse(""),
            profile.currentPosition.flatMap(_.title).getOrElse(""),
            "",
            None
          )
          databaseService.createJoinRequest(joinrequest).map {
            id =>
              Ok(Json.toJson(joinrequest.copy(id = id)))
          }.getOrElse(BadRequest(Errors.emailExistsError))
        }
      }
  }

}
