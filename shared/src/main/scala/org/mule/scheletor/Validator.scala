package org.mule.scheletor

import org.mule.scheletor.ErrorType.{ReadOnly, WriteOnly}
import org.mule.scheletor.Validator.LimitReachedException

import scala.collection.mutable.ListBuffer

object Validator {
  def failFast: Builder                                                     = builder.failFast
  def errorLimit(limit: Int): Builder                                       = builder.errorLimit(limit)
  def readOnly: Builder                                                     = builder.readOnly
  def writeOnly: Builder                                                    = builder.writeOnly
  def validate[T: ObjLike](schema: Schema, input: T): List[ValidationError] = builder.validate(schema, input)

  private def builder = new Builder

  class Builder {
    private var errorLimit       = Int.MaxValue
    private var context: Context = None

    def failFast: this.type               = errorLimit(1)
    def errorLimit(limit: Int): this.type = { errorLimit = limit; this }
    def readOnly: this.type               = { context = Read; this }
    def writeOnly: this.type              = { context = Write; this }

    def validate[T: ObjLike](schema: Schema, input: T): List[ValidationError] =
      new Validator(errorLimit, context, Pointer.empty).validate(schema, input)
  }

  sealed trait Context {
    def validate(schema: Schema, validator: Validator): Unit = {}
  }

  object Read extends Context {
    override def validate(s: Schema, validator: Validator): Unit = if (s.writeOnly) validator += WriteOnly
  }
  object Write extends Context {
    override def validate(s: Schema, validator: Validator): Unit = if (s.readOnly) validator += ReadOnly
  }
  object None extends Context

  class LimitReachedException extends RuntimeException
}

class Validator private (val errorLimit: Int,
                         val context: Validator.Context,
                         val location: Pointer,
                         val builder: ListBuffer[ValidationError] = new ListBuffer) {

  def +=(error: ErrorType): Unit = {
    builder += ValidationError(location, error)
    if (builder.length >= errorLimit) throw new LimitReachedException
  }

  def /(propertyName: String) = new Validator(errorLimit, context, location / propertyName, builder)
  def /(index: Int)           = new Validator(errorLimit, context, location / index, builder)

  private[scheletor] def validateContext(schema: Schema): Unit = context.validate(schema, this)

  private def validate[T: ObjLike](schema: Schema, input: T) = {
    try schema.validate(input, this)
    catch {
      case _: LimitReachedException => // Ignore
    }
    builder.result()
  }

  def check[T: ObjLike](schema: Schema, input: T): Boolean =
    new Validator(1, context, location).validate(schema, input).isEmpty

}
