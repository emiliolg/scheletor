package org.mule.scheletor

import org.mule.scheletor.ErrorType.NotNull

trait NullSchema extends Schema {

  override def typeName: String = "null"

  override val nullable                                                                  = true
  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = validator += NotNull
}
