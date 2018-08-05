package org.mulesoft.scheletor

import org.mulesoft.scheletor.ErrorType.NotNull

trait NullSchema extends Schema {

  override def typeName: String = "null"

  override val nullable                                                                  = true
  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = validator += NotNull
}
