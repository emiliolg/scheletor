package org.mule.scheletor

trait RefSchema extends Schema {
  def refSchema: Schema

  override private[scheletor] def validate[V: ObjLike](input: V, validator: Validator): Unit =
    refSchema.validate(input, validator)
}
