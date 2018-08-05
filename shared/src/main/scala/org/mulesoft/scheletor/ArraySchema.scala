package org.mulesoft.scheletor

import org.mulesoft.scheletor.ErrorType._
import org.mulesoft.scheletor.ObjLike._

trait ArraySchema extends Schema {
  def array: Boolean
  def minItems: Option[Int]
  def maxItems: Option[Int]
  def uniqueItems: Boolean
  def allItems: Option[Schema]              // items when it is an Schema
  def items: List[Schema]                   // items when it is an Array
  def allowAdditionalItems: Boolean         // additionalItems when it is a boolean
  def additionalItemsSchema: Option[Schema] // additionalItems when it is an schema

  def contains: Option[Schema]

  override def typeName: String = "array"

  override protected def innerValidate[V: ObjLike](input: V, validator: Validator): Unit = input.asArray match {
    case Some(o) => new Validate(o, validator).validateAll()
    case _       => if (array) validator += NotArray
  }

  private class Validate[V: ObjLike](array: ObjLike.Array[V], validator: Validator) {
    private val length = array.length

    def validateAll(): Unit = {
      numberOfItems()
      validateContains()
      if (length > 0) {
        if (items.isEmpty) validateAllItems()
        else {
          validateItems()
          if (length > items.length) {
            if (!allowAdditionalItems) validator += MaxItems(items.length, length)
            else validatedAdditionalItems()
          }
        }
        validateUnique()
      }
    }
    def numberOfItems(): Unit = {
      for {
        ml <- minItems
        if length < ml
      } validator += MinItems(ml, length)
      for {
        ml <- maxItems
        if length > ml
      } validator += MaxItems(ml, length)
    }

    def validateItems(): Unit = {
      var idx = 0
      for (schema <- items) {
        if (idx < length) {
          schema.validate(array(idx), validator / idx)
          idx += 1
        }
      }
    }

    def validateAllItems(): Unit = {
      for {
        schema <- allItems
        idx    <- 0 until length
      } schema.validate(array(idx), validator / idx)
    }

    def validatedAdditionalItems(): Unit =
      for {
        schema <- additionalItemsSchema
        idx    <- items.length until length
      } schema.validate(array(idx), validator / idx)

    def validateUnique(): Unit = if (uniqueItems) {
      var idx        = 0
      var uniqueOnes = List.empty[V]
      while (idx < length) {
        val e = array(idx)
        if (uniqueOnes exists (deepEquals(_, e))) (validator / idx) += NotUnique
        else uniqueOnes = e :: uniqueOnes
        idx += 1
      }
    }
    def validateContains(): Unit = contains match {
      case Some(s) =>
        var idx = 0
        while (idx < length) {
          if (validator.check(s, array(idx))) return
          idx += 1
        }
        validator += DoesNotContain
      case _ =>
    }

  }
}
