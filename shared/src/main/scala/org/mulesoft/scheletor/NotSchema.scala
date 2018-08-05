package org.mulesoft.scheletor

import org.mulesoft.scheletor.ErrorType._

trait NotSchema extends Schema {
  def innerSchema: Schema

  override def typeName: String = ""
  override def toString: String = s"not(${innerSchema.toString})"

  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = {
    if (validator.check(innerSchema, input)) validator += MustNotValidate(innerSchema)
  }

}
