package org.mulesoft.scheletor

import org.mulesoft.scheletor.ErrorType._
import org.mulesoft.scheletor.ObjLike._

trait BooleanSchema extends Schema {

  override def typeName: String = "boolean"

  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = {
    if (input.asBoolean.isEmpty) validator += NotBoolean
  }
}
