package org.mule.scheletor

import org.mule.scheletor.ErrorType.False

/** An Schema that accepts any value */
trait EmptySchema extends Schema {
  override def typeName: String = "empty"

  override private[scheletor] def validate[V: ObjLike](input: V, validator: Validator): Unit = {}
}

/** A True Schema always validate */
trait TrueSchema extends EmptySchema {
  override def typeName: String = "true"
}

/** A False Schema never validates */
trait FalseSchema extends Schema {
  override def typeName: String = "false"

  override private[scheletor] def validate[V: ObjLike](input: V, validator: Validator): Unit = validator += False
}
