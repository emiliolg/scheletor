package org.mule.scheletor.jsonschema

import org.mule.scheletor.ErrorType.InvalidRef
import org.mule.scheletor.ObjLike.Obj
import org.mule.scheletor.SchemaBuilder._
import org.mule.scheletor._

import scala.collection.mutable

class ReferenceLoader[V: ObjLike](loader: JsonSchemaLoader[V], obj: Obj[V]) {
  private val referenceSchemas = mutable.Map.empty[String, RefBuilder]

  def load(ref: String): Schema = referenceSchemas.getOrElse(ref, doLoad(ref)).build

  private def doLoad(ref: String): RefBuilder = {
    val builder: RefBuilder = reference(ref)
    referenceSchemas += ref -> builder
    val pointer = Pointer(ref)
    val s: V = loader.extract(pointer).getOrElse(loader.error(pointer, InvalidRef(ref)))
    builder.schema = Some(loader.loadSubSchema(s, pointer))
    builder.build.refSchema // Force lazy val calculation
    builder
  }
}
