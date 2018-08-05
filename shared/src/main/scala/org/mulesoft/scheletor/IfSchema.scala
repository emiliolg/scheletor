package org.mulesoft.scheletor

trait IfSchema extends Schema {
  def ifSchema: Option[Schema]
  def thenSchema: Option[Schema]
  def elseSchema: Option[Schema]

  override def typeName: String =
    if (empty) "empty"
    else "if " + ifSchema + thenSchema.map(" then " + _).getOrElse("") + elseSchema.map(" else " + _).getOrElse("")

  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = if (!empty) {
    val target = if (validator.check(ifSchema.get, input)) thenSchema else elseSchema
    for {
      schema <- target
    } schema.validate(input, validator)
  }

  private def empty = ifSchema.isEmpty || thenSchema.isEmpty && elseSchema.isEmpty
}
