package controllers

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import play.api.Play.current
import play.twirl.api._
import play.api.libs.ws._
import play.api.http.Writeable

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.regions._
import com.amazonaws.services.simpleemail.AmazonSimpleEmailServiceClient
import com.amazonaws.services.simpleemail.model.{Content => AwsContent, _}

trait EmailService {
  def send(to: String, subject: String, content: Content, from: String = "invited@gamesmith.com", bcc: String = ""): Boolean
}

trait EmailModule {
  val emailService: EmailService
}

trait DumbEmailModule extends EmailModule {
  val emailService = new EmailService {
    override def send(to: String, subject: String, content: Content, from: String, bcc: String) = {
      println("Sending email to " + to)
      println("Sending email bcc " + bcc)
      println("From: " + from)
      println("Subject: " + subject)
      println("Content: " + content.body)
      true
    }
  }
}

trait AmazonSimpleEmailModule extends EmailModule {
  private val AWS_ACCESS_KEY = sys.env.get("AWS_ACCESS_KEY")
  private val AWS_SECRET_KEY = sys.env.get("AWS_SECRET_KEY")

  val emailService = if (AWS_ACCESS_KEY.isDefined && AWS_SECRET_KEY.isDefined) {
    new EmailService {
      val credentials = new BasicAWSCredentials(AWS_ACCESS_KEY.get, AWS_SECRET_KEY.get)
      val sesClient = new AmazonSimpleEmailServiceClient(credentials)
      sesClient.setRegion(Region.getRegion(Regions.US_WEST_2))

      override def send(to: String, subject: String, content: Content , from: String, bcc: String) = {
        val destination = new Destination()
          .withToAddresses(to)

        val body = content match {
          case html: Html =>
            new Body().withHtml(new AwsContent(content.body))
          case _ =>
            ???
        }

        val message = new Message()
          .withSubject(new AwsContent(subject))
          .withBody(body)

        val sendRequest = new SendEmailRequest()
          .withSource(from)
          .withDestination(destination)
          .withMessage(message)

        try {
          sesClient.sendEmail(sendRequest)
          true
        } catch {
          case e: Throwable =>
            e.printStackTrace()
            false
        }
      }
    }
  } else {
    new DumbEmailModule(){}.emailService
  }
}

trait MailgunEmailModule extends EmailModule {
  private val MAILGUN_API_KEY = sys.env.get("MAILGUN_API_KEY")
  private val mailgunEndpoint = "https://api.mailgun.net/v3/messages.gamesmith.com/messages"

  val emailService = MAILGUN_API_KEY.map { apiKey =>
    implicit val mapWritable: Writeable[Map[String, String]] = implicitly[Writeable[Map[String, Seq[String]]]].map { inputMap =>
      inputMap.mapValues { item => Seq(item) }
    }

    new EmailService {
      override def send(to: String, subject: String, content: Content, from: String, bcc: String) = {
        val emailBody = content match {
          case html: Html =>
            "html" -> html.body
          case text: Txt =>
            "text" -> text.body
          case _ =>
            ???
        }

        val body = if(bcc.isEmpty){
            Map("from" -> from,
            "to" -> to,
            "subject" -> subject,
            emailBody)
          } else {
            Map("from" -> from,
            "to" -> to,
            "bcc" -> bcc,
            "subject" -> subject,
            emailBody)
          }

        val request = WS.url(mailgunEndpoint)
          .withAuth("api", apiKey, WSAuthScheme.BASIC)
          .post(body)

        val result = Await.result(request, Duration(10, "second"))
        if (result.status == 200) {
          true
        } else {
          println("MAILGUN ERROR ----------------------")
          println(result.body)
          println("MAILGUN ERROR ----------------------")
          false
        }
      }
    }
  }.getOrElse {
    new DumbEmailModule(){}.emailService
  }
}
