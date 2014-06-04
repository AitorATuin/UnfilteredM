package com.logikujo.www

import org.apache.commons.mail._
import scala.util.Try
import scala.util.{Success,Failure}
import java.util.Properties
import com.typesafe.config.Config
import com.github.kxbmap.configs._
import org.apache.commons.mail._
import scalaz._
import Scalaz._

/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.mail 6/04/14 :: 18:36 :: eof
 *
 */
package object mail {
  type UnfilteredMailM = UnfilteredM[SendMail]

  //def unfilteredMailM = liftM[SendMail]_
  def unfilteredMailM = liftM[SendMail]((c: Configuration) => (new SendMail {
    val config: Configuration = c
  }).right[String])

  implicit def stringToSeq(single: String): Seq[String] = Seq(single)

  implicit def liftToOption[T](t: T): Option[T] = Some(t)

  sealed abstract class MailType

  case object Plain extends MailType

  case object Rich extends MailType

  case object MultiPart extends MailType

  sealed case class Mail(
                          from: (String, String), // (email -> name)
                          to: Seq[String],
                          cc: Seq[String] = Seq.empty,
                          bcc: Seq[String] = Seq.empty,
                          subject: String,
                          message: String,
                          richMessage: Option[String] = None,
                          attachment: Option[(java.io.File)] = None
                          )

  // Instantiate as much as possible when creating it
  sealed trait SendMail {
    val config: Configuration

    private def prepareEmail(smtp: String,
                             port: Int,
                             user: String,
                             passwd: String,
                             localhost: String,
                             mail: Mail): Email = {
      System.setProperty("com.logikujo.www.mail.smtp.localhost", localhost)
      val format =
        if (mail.attachment.isDefined) MultiPart
        else if (mail.richMessage.isDefined) Rich
        else Plain

      val commonsMail: Email = format match {
        case Plain => new SimpleEmail().setMsg(mail.message)
        case Rich => new HtmlEmail().setHtmlMsg(mail.richMessage.get).setTextMsg(mail.message)
        case MultiPart => {
          val attachment = new EmailAttachment()
          attachment.setPath(mail.attachment.get.getAbsolutePath)
          attachment.setDisposition(EmailAttachment.ATTACHMENT)
          attachment.setName(mail.attachment.get.getName)
          new MultiPartEmail().attach(attachment).setMsg(mail.message)
        }
      }

      // TODO Set authentication from your configuration, sys properties or w/e

      // Can't add these via fluent API because it produces exceptions
      mail.to foreach (commonsMail.addTo(_))
      mail.cc foreach (commonsMail.addCc(_))
      mail.bcc foreach (commonsMail.addBcc(_))

      val email = commonsMail.
        setFrom(mail.from._1, mail.from._2).
        setSubject(mail.subject)
      email.setHostName(localhost)
      email.setSmtpPort(port)
      email.setAuthenticator(new DefaultAuthenticator(user, passwd));
      email.setDebug(true)
      email.setStartTLSRequired(true)
      email.setStartTLSEnabled(true)
      email.setSocketConnectionTimeout(4000)
      email.setSocketTimeout(4000)
      email
    }

    def apply(mail: Mail): Try[String] = {
      println("------> " + config.opt[String]("mail.localhost"))
      val commonEmail = for {
        localhost <- config.opt[String]("mail.localhost")
        smtp <- config.opt[String]("mail.smtpServer")
        port <- config.opt[Int]("mail.smtpPort")
        user <- config.opt[String]("mail.smtpUser")
        passwd <- config.opt[String]("mail.smtpPasswd")
      //debugLevel <- config.opt[String]("com.logikujo.www.mail.debugLevel")
      } yield {
        val commonEmail = prepareEmail(smtp, port, user, passwd, localhost, mail)
        commonEmail.setDebug(config.opt[Boolean]("mail.debug").getOrElse(true))
        commonEmail.setStartTLSRequired(config.opt[Boolean]("mail.startTLSRequired").getOrElse(true))
        commonEmail.setStartTLSEnabled(config.opt[Boolean]("startTLSEnabled").getOrElse(true))
        commonEmail.setSocketConnectionTimeout(config.opt[Int]("mail.socketConnectionTimeout").getOrElse(4000))
        commonEmail.setSocketTimeout(config.opt[Int]("mail.socketTimeout").getOrElse(4000))
        commonEmail
      }
      commonEmail.
        fold(Try[String](throw new Exception("Email service is not configured.")))(m =>
        Try[String](m.send))
    }
  }
}
