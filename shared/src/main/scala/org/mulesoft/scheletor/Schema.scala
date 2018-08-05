package org.mulesoft.scheletor

import java.util.Objects

import org.mulesoft.scheletor.ErrorType.Nullable
import org.mulesoft.scheletor.ObjLike._

import scala.language.implicitConversions

trait Schema {
  def id: String
  def description: String
  def title: String
  def nullable: Boolean
  def readOnly: Boolean
  def writeOnly: Boolean
  def defaultValue: Option[Any]
  def typeName: String

  private[scheletor] def validate[V: ObjLike](input: V, validator: Validator): Unit = {
    validator.validateContext(this)
    if (input != null && !input.isNull) innerValidate(input, validator)
    else if (!nullable) validator += Nullable
  }

  protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = {}

  override def equals(that: Any): Boolean =
    if (this eq that.asInstanceOf[AnyRef]) true
    else
      that match {
        case that: Schema =>
          id == that.id && description == that.description &&
            title == that.title && nullable == that.nullable &&
            readOnly == that.readOnly && writeOnly == that.writeOnly && defaultValue == that.defaultValue
        case _ => false
      }

  override def hashCode(): Int =
    Objects.hashCode(id, title, description, nullable, readOnly, writeOnly, defaultValue)

  def canEqual(a: Any): Boolean = a.isInstanceOf[Schema]

  override def toString: String = if (id.isEmpty) typeName else id
}