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

  case object RichMultiPart extends MailType {
    def apply() = new HtmlEmail()
  }

  case class Mail(
    from: Option[(String, String)] = None,
    to: Option[Seq[String]] = Seq.empty.some,
    cc: Option[Seq[String]] = Seq.empty.some,
    bcc: Option[Seq[String]] = Seq.empty.some,
    subject: Option[String] = None,
    message: Option[String] = None,
    richMessage: Option[String] = None,
    attachment: Option[Seq[java.io.File]] = None) {
    def from(v: (String,String)):Mail = this.copy(from=v.some)
    def to(v: Seq[String]):Mail = this.copy(to=v.some)
    def cc(v: Seq[String]):Mail = this.copy(cc=v.some)
    def bcc(v: Seq[String]):Mail = this.copy(bcc=v.some)
    def subject(v: String):Mail = this.copy(subject=v.some)
    def message(v: String):Mail = this.copy(message=v.some)
    def richMessage(v: String):Mail = this.copy(richMessage=v.some)
    def attachment(v:Seq[java.io.File]):Mail = this.copy(attachment=v.some)
    val format: MailType = (richMessage, attachment) match {
      case (Some(_), Some(a)) if a.length > 0 => RichMultiPart
      case (Some(_), _) => Rich
      case (None, Some(a)) if a.length > 0 => MultiPart
      case _ => Plain
    }
    /*  if (attachment.isDefined && !attachment.get.isEmpty) MultiPart
      else if (richMessage.isDefined) Rich
      else Plain*/
  }

  sealed trait MailSender {
    val config: Configuration
    val sendEmail: \/[String, (Mail => Option[Email])] = for {
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
          mailType <- mail.format.some
        } yield {
          val commonsMail: Email = mailType()
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
          def file2Attachment(f: java.io.File) = {
            val attachment = new EmailAttachment()
            attachment.setPath(f.getAbsolutePath)
            attachment.setDisposition((EmailAttachment.ATTACHMENT))
            attachment.setName(f.getName)
            attachment
          }
          def attachFiles[E <: MultiPartEmail](mail: E, files: Seq[java.io.File]) =
            (mail /: files.map(file2Attachment))(_.attach(_).asInstanceOf[E])
          mailType match {
            case Plain =>
              commonsMail.
                setMsg(message)
            case Rich =>
              commonsMail.asInstanceOf[HtmlEmail].
                setHtmlMsg(mail.richMessage.get).
                setTextMsg(message)
            case MultiPart =>
              attachFiles[MultiPartEmail](commonsMail.asInstanceOf[MultiPartEmail], mail.attachment.get).
                setMsg(message)
            case RichMultiPart =>
              attachFiles[HtmlEmail](commonsMail.asInstanceOf[HtmlEmail], mail.attachment.get).
                setHtmlMsg(mail.richMessage.get).
                setTextMsg(message)
          }
        }
      }

    // TODO: Try -> \/ and flatten then
    def send(m: Mail) = sendEmail.flatMap(f => f(m).
      \/>("Unable to send Mail, bad formed")).
      fold(
        e => Try[String](throw new Exception(e)),
        m => Try[String](m.send)
      )
      //fold(Try[String](throw new Exception("Email service is not configured.")))((m:Email) => Try[String](m.send())))
  }

  object MailSender {
    def apply[T](c:Configuration) = new MailSender {
      val config = c
    }.withTag[T]
  }
}
