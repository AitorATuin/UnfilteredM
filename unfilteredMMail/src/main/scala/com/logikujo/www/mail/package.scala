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
 * TODO:
 *  * Refactor this class, allow a Tagged MailSystem
 */
package object mail {
  trait MailSystem

  implicit def stringToSeq(single: String): Seq[String] = Seq(single)

  implicit def liftToOption[T](t: T): Option[T] = Some(t)

  sealed abstract class MailType {
    def apply(): Email
  }

  case object Plain extends MailType {
    def apply() = new SimpleEmail()
  }

  case object Rich extends MailType {
    def apply() = new HtmlEmail()
  }

  case object MultiPart extends MailType {
    def apply() = new MultiPartEmail()
  }

  case class Mail(
    from: Option[(String, String)] = None,
    to: Option[Seq[String]] = Seq.empty.some,
    cc: Option[Seq[String]] = Seq.empty.some,
    bcc: Option[Seq[String]] = Seq.empty.some,
    subject: Option[String] = None,
    message: Option[String] = None,
    richMessage: Option[String] = None,
    attachment: Option[(java.io.File)] = None) {
    def from(v: (String,String)) = this.copy(from=v.some)
    def to(v: Seq[String]) = this.copy(to=v.some)
    def cc(v: Seq[String]) = this.copy(cc=v.some)
    def bcc(v: Seq[String]) = this.copy(bcc=v.some)
    def subject(v: String) = this.copy(subject=v.some)
    def message(v: String) = this.copy(message=v.some)
    def richMessage(v: String) = this.copy(richMessage=v.some)
    def attachment(v:java.io.File) = this.copy(attachment=v.some)
    val format: MailType =
      if (attachment.isDefined) MultiPart
      else if (richMessage.isDefined) Rich
      else Plain
  }

  sealed trait MailSender {
    val config: Configuration
    val sendEmail = for {
      localhost <- config.∨[String]("mail.localhost")
      smtp <- config.∨[String]("mail.smtpServer")
      port <- config.∨[Int]("mail.smtpPort")
      user <- config.∨[String]("mail.smtpUser")
      passwd <- config.∨[String]("mail.smtpPasswd")
    } yield (mail: Mail) => {
        for {
          to <- mail.to
          (fromAddress, fromName) <- mail.from
          cc <- mail.cc
          bcc <- mail.bcc
          subject <- mail.subject
          message <- mail.message
          commonsMail: Email <- mail.format().some
        } yield {
          to foreach (commonsMail.addTo(_))
          cc foreach (commonsMail.addCc(_))
          bcc foreach (commonsMail.addBcc(_))
          commonsMail.setFrom(fromAddress, fromName)
          commonsMail.setSubject(subject)
          commonsMail.setDebug(config.opt[Boolean]("mail.debug").getOrElse(true))
          commonsMail.setStartTLSRequired(config.opt[Boolean]("mail.startTLSRequired").getOrElse(true))
          commonsMail.setStartTLSEnabled(config.opt[Boolean]("startTLSEnabled").getOrElse(true))
          commonsMail.setSocketConnectionTimeout(config.opt[Int]("mail.socketConnectionTimeout").getOrElse(4000))
          commonsMail.setSocketTimeout(config.opt[Int]("mail.socketTimeout").getOrElse(4000))
          commonsMail.setHostName(smtp)
          commonsMail.setSmtpPort(port)
          commonsMail.setAuthenticator(new DefaultAuthenticator(user, passwd))
          commonsMail
        }
      }

    def send(mail: Mail) = sendEmail(mail).map(m => mail.format match {
      case Plain => m.setMsg(mail.message)
      case Rich => new HtmlEmail().setHtmlMsg(mail.richMessage.get).setTextMsg(mail.message)
      case MultiPart => {
        val attachment = new EmailAttachment()
        attachment.setPath(mail.attachment.get.getAbsolutePath)
        attachment.setDisposition(EmailAttachment.ATTACHMENT)
        attachment.setName(mail.attachment.get.getName)
        new MultiPartEmail().attach(attachment).setMsg(mail.message)
      }
    })
    }

  // Instantiate as much as possible when creating it
  sealed trait SendMail {
    val config: Configuration

    private def prepareEmail(smtp: String,
                             port: Int,
                             user: String,
                             passwd: String,
                             localhost: String,
                             mail: Mail): Email = {
      //System.setProperty("com.logikujo.www.mail.smtp.localhost", localhost)
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

      commonEmail.
        fold(Try[String](throw new Exception("Email service is not configured.")))(m =>
        Try[String](m.send))
    }
  }
}
