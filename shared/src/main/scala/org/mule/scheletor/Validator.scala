package org.mule.scheletor

import org.mule.scheletor.ErrorType.{ReadOnly, WriteOnly}
import org.mule.scheletor.Validator.LimitReachedException

import scala.collection.mutable.ListBuffer

object Validator {

  sealed trait Context {
    def validate(schema: Schema, validator: Validator): Unit = {}
  }

  object ReadContext extends Context {
    override def validate(s: Schema, validator: Validator): Unit = if (s.writeOnly) validator += WriteOnly
  }
  object WriteContext extends Context {
    override def validate(s: Schema, validator: Validator): Unit = if (s.readOnly) validator += ReadOnly
  }
  object NoContext extends Context

  class LimitReachedException extends RuntimeException
}

class Validator private[scheletor] (val errorLimit: Int,
                                    val context: Validator.Context,
                                    val location: Pointer = Pointer.empty,
                                    val builder: ListBuffer[ValidationError] = new ListBuffer) {

  def +=(error: ErrorType): Unit = {
    builder += ValidationError(location, error)
    if (builder.length >= errorLimit) throw new LimitReachedException
  }

  def /(propertyName: String) = new Validator(errorLimit, context, location / propertyName, builder)
  def /(index: Int)           = new Validator(errorLimit, context, location / index, builder)

  private[scheletor] def validateContext(schema: Schema): Unit = context.validate(schema, this)

  private[scheletor] def validate[T: ObjLike](schema: Schema, input: T) = {
    try schema.validate(input, this)
    catch {
      case _: LimitReachedException => // Ignore
    }
    builder.result()
  }

  def check[T: ObjLike](schema: Schema, input: T): Boolean =
    new Validator(1, context, location).validate(schema, input).isEmpty

}
