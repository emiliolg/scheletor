package org.mule.scheletor

import org.mule.scheletor.ErrorType._
import org.mule.scheletor.ObjLike._

import scala.util.matching.Regex

trait StringSchema extends Schema {
  def string: Boolean
  def minLength: Option[Int]
  def maxLength: Option[Int]
  def pattern: Option[Regex]
  def formatValidator: FormatValidator

  override def typeName: String = "string"

  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = input.asString match {
    case Some(s) => validate(s, validator)
    case _       => if (string) validator += NotString
  }

  private def validate(s: String, validator: Validator): Unit = {
    val length = s.codePointCount(0, s.length)
    for {
      ml <- minLength
      if length < ml
    } validator += MinLength(ml, length)
    for {
      ml <- maxLength
      if length > ml
    } validator += MaxLength(ml, length)
    for {
      p <- pattern
      if !p.pattern.matcher(s).find()
    } validator += MatchError(s, p)
    for {
      err <- formatValidator.validate(s)
    } validator += err
  }
}
