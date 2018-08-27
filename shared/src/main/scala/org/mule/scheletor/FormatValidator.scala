package org.mule.scheletor
import java.util.regex.{Pattern, PatternSyntaxException}

import org.mule.scheletor.ErrorType._
import org.mulesoft.common.core._
import org.mulesoft.common.net.Email.{parse => emailParse}
import org.mulesoft.common.net.InetAddress.{parseAsIPv4, parseAsIPv6}
import org.mulesoft.common.net.UriTemplate.{parse=>parseAsUriTemplate}
import org.mulesoft.common.net.{DomainValidator, UriValidator}
import org.mulesoft.common.parse.ParseError
import org.mulesoft.common.time.SimpleDateTime._

import scala.Option.empty

case class FormatValidator private (name: String, validate: String => Option[ErrorType]) {
  def apply(value: String): Option[ErrorType] = validate(value)
}

object FormatValidator {
  private final var formats: Map[String, FormatValidator] = Map.empty

  final val None         = FormatValidator("none", _ => empty)
  final val DateTime     = FormatValidator("date-time", checkParsing(parse, DateTimeError))
  final val Date         = FormatValidator("date", checkParsing(parseDate, DateError))
  final val Time         = FormatValidator("time", checkParsing(parseFullTime, TimeError))
  final val FullDate     = FormatValidator("full-date", Date.validate)
  final val FullTime     = FormatValidator("full-time", Time.validate)
  final val PartialTime  = FormatValidator("partial-time", checkParsing(parsePartialTime, TimeError))
  final val Email        = FormatValidator("email", checkParsing(emailParse, EmailError))
  final val IPv4         = FormatValidator("ipv4", checkParsing(parseAsIPv4, IPv4Error))
  final val IPv6         = FormatValidator("ipv6", checkParsing(parseAsIPv6, IPv6Error))
  final val URI          = FormatValidator("uri", uri(false))
  final val URIReference = FormatValidator("uri-reference", uri(true))
  final val URITemplate  = FormatValidator("uri-template", checkParsing(parseAsUriTemplate, UriTemplateError))
  final val Regex        = FormatValidator("regex", regex)

  final val HostName =
    FormatValidator("hostname", s => if (DomainValidator.isValid(s, allowLocal = true)) empty else Some(HostNameError(s)))

  def apply(name: String): Option[FormatValidator] = formats.get(name)

  def apply(name: String, validate: String => Option[ErrorType]): FormatValidator = {
    val r = new FormatValidator(name, validate)
    formats += name -> r
    r
  }

  private def uri(allowRelative: Boolean): String => Option[ErrorType] =
    s => if (UriValidator.isUri(s, allowRelative)) empty else Some(UriError(s))

  private def regex(pattern: String): Option[ErrorType] =
    if (pattern.isNullOrEmpty) Some(RegexError(pattern))
    else {
      try Pattern.compile(pattern)
      catch {
        case _: Exception => return Some(RegexError(pattern))
      }
      empty
    }

  private def checkParsing[T](pf: String => Either[ParseError, T],
                              errorType: String => ErrorType): String => Option[ErrorType] =
    s => if (pf(s).isRight) empty else Some(errorType(s))

}
