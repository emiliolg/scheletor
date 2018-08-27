package org.mule.scheletor

import org.mule.scheletor.ErrorType._
import org.mule.scheletor.ObjLike._

trait BooleanSchema extends Schema {

  override def typeName: String = "boolean"

  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = {
    if (input.asBoolean.isEmpty) validator += NotBoolean
  }
}
