package org.mulesoft.scheletor

case class FormatValidator(validate: String => Option[String], name: String = "unnamed")

object FormatValidator {
  final val None                                   = FormatValidator(s => Option.empty, "None")

  def apply(name: String): Option[FormatValidator] = Option.empty
}
