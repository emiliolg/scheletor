package org.mulesoft.scheletor
import org.mulesoft.scheletor.ErrorType.ConstFailed

trait ConstSchema[W] extends Schema with HasDeepEquals[W] {

  def value: W

  override def typeName: String = "const"

  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit =
    if (!deepEquals(value, input)) validator += ConstFailed(value.toString, input.toString)

}
