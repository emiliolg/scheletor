package org.mule.scheletor

import org.mule.scheletor.ErrorType._
import org.mule.scheletor.ObjLike._

import scala.util.matching.Regex

import Primitives._

trait ObjectSchema extends Schema {
  def objectType: Boolean
  def minProperties: Option[Int]
  def maxProperties: Option[Int]
  def required: List[String]
  def propertyNames: Option[Schema]
  def properties: Map[String, Schema]
  def patternProperties: Map[Regex, Schema]
  def additionalPropertiesSchema: Option[Schema]
  def allowAdditionalProperties: Boolean
  def dependencies: Map[String, (Option[Schema], Set[String])]

  override def typeName: String = "object"

  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = input.asObject match {
    case Some(o) => new Validate(input, o, validator).validateAll()
    case _       => if (objectType) validator += NotObject
  }

  private class Validate[V: ObjLike](input: V, obj: ObjLike.Obj[V], validator: Validator) {
    private val propertyKeys = obj.properties

    def validateAll(): Unit = {
      numberOfProperties()
      validateRequired(required)
      if (propertyKeys.nonEmpty) {
        validatePropertyNames()
        additionalProperties()
        validatePatternProperties()
        validateProperties()
        validateDependencies()
      }
    }

    def numberOfProperties(): Unit = {
      val length = propertyKeys.length
      for {
        ml <- minProperties
        if length < ml
      } validator += MinProperties(ml, length)
      for {
        ml <- maxProperties
        if length > ml
      } validator += MaxProperties(ml, length)
    }

    def validatePropertyNames(): Unit =
      for {
        s   <- propertyNames
        key <- propertyKeys
      } s.validate(key, validator / key)

    def validateRequired(requiredProperties: Iterable[String]): Unit =
      for {
        p <- requiredProperties
        if obj.get(p).isEmpty
      } validator += MissingProperty(p)

    def additionalProperties(): Unit = if (!allowAdditionalProperties || additionalPropertiesSchema.isDefined) {
      for {
        key <- propertyKeys
        if !properties.contains(key) && !matchesAnyPattern(key)
      } {
        if (allowAdditionalProperties) additionalPropertiesSchema.get.validate(obj(key), validator / key)
        else validator += InvalidProperty(key)
      }

    }

    def validatePatternProperties(): Unit =
      for {
        (regex, schema) <- patternProperties
        key             <- propertyKeys filter (regex.pattern.matcher(_).find())
      } schema.validate(obj(key), validator / key)

    def validateProperties(): Unit =
      for {
        (name, schema) <- properties
      } obj.get(name) match {
        case Some(v) => schema.validate(v, validator / name)
        case _       => // Add default value ??
      }

    def validateDependencies(): Unit = {
      for {
        (prop, (schema, props)) <- dependencies
        if obj.get(prop).isDefined
      } {
        validateRequired(props)
        for (s <- schema) s.validate(input, validator)
      }
    }

    private def matchesAnyPattern(property: String) =
      patternProperties.nonEmpty && patternProperties.keys.exists(_.pattern.matcher(property).find())
  }
}
