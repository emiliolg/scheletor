package org.mulesoft.scheletor

import org.mulesoft.scheletor.ErrorType._

trait EnumSchema[W] extends Schema with HasDeepEquals[W] {
  def possibleValues: List[W]
  override def typeName: String = "enum"

  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit =
    if (!possibleValues.exists(deepEquals(_, input))) validator += NotEnum

}
