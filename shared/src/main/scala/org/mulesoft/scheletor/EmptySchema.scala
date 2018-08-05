package org.mulesoft.scheletor

/** An Schema that accepts any value */
trait EmptySchema extends Schema {

  override def typeName: String = "empty"

  override private[scheletor] def validate[V: ObjLike](input: V, validator: Validator): Unit = {}
}
