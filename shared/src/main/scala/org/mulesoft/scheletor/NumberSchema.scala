package org.mulesoft.scheletor

import org.mulesoft.scheletor.ErrorType._
import org.mulesoft.scheletor.ObjLike._

import scala.Ordering.Implicits._

trait NumberSchema extends Schema {
  def minimum: Option[Number]
  def exclusiveMinimum: Option[Number]
  def maximum: Option[Number]
  def exclusiveMaximum: Option[Number]
  def multipleOf: Option[Number]
  def integer: Boolean
  def number: Boolean

  override def typeName: String = if (integer) "integer" else "number"

  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = {
    if (integer) input.asLong match {
      case Some(v) =>
        rangeValidation(v, _.longValue(), validator)
        for {
          d <- multipleOf
          if v % d.longValue() != 0
        } validator += NotMultipleOf(v, d)

      case _ => validator += NotInt
    }
    else
      input.asDouble match {
        case Some(v) =>
          rangeValidation(v, _.doubleValue(), validator)
          for {
            d <- multipleOf
            if BigDecimal(v) % BigDecimal(d.doubleValue()) != 0
          } validator += NotMultipleOf(v, d)

        case _ => if (number) validator += NotNumber
      }
  }
  private def rangeValidation[T: Ordering](v: T, conv: Number => T, validator: Validator): Unit = {

    def validate(o: Option[Number], test: T => Boolean, msg: String): Unit =
      for {
        n <- o
        if test(conv(n))
      } validator += OutOfRange(v, msg, n)

    validate(minimum, v < _, "less than")
    validate(maximum, v > _, "greater than")
    validate(exclusiveMinimum, v <= _, "less than or equal")
    validate(exclusiveMaximum, v >= _, "greater than or equal")
  }

}
