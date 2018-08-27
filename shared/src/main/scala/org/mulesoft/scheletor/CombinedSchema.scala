package org.mulesoft.scheletor

import org.mulesoft.scheletor.ErrorType._

trait CombinedSchema extends Schema {
  def schemas: List[Schema]
  def apply(idx: Int): Schema   = schemas(idx)
  override def toString: String = typeName + schemas.mkString("(", ",", ")")
}

trait AnyOfSchema extends CombinedSchema {
  override def typeName: String = "anyOf"
  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = {
    if (!schemas.exists(validator.check(_, input)))
      validator += NoneMatched
  }
}

trait AllOfSchema extends CombinedSchema {
  override def typeName: String = "allOf"
  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = for (schema <- schemas) {
    schema.validate(input, validator)
  }
}

trait OneOfSchema extends CombinedSchema {
  override def typeName: String = "oneOf"
  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = {
    val n = schemas.count(validator.check(_, input))
    if (n == 0) validator += NoneMatched
    else if (n > 1) validator += ManyMatched(n)
  }
}
