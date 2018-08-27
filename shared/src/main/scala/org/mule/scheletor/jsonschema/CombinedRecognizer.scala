package org.mule.scheletor.jsonschema

import org.mule.scheletor.{CombinedSchema, ObjLike, SchemaLoader}
import org.mule.scheletor.SchemaBuilder._
import org.mule.scheletor.SchemaLoader.Builder

object CombinedRecognizer extends SchemaLoader.Recognizer {

  type CBuilder = Combined[_ <: CombinedSchema]

  private val combineBuilders: Seq[(String, () => CBuilder)] = List(
      "allOf" -> (() => allOfSchema()),
      "anyOf" -> (() => anyOfSchema()),
      "oneOf" -> (() => oneOfSchema)
  )

  override def extract[V: ObjLike](loader: SchemaLoader[V]): Seq[Builder] =
    for {
      (key, f) <- combineBuilders
      v        <- loader.get(key)
    } yield v.asSchemaSeq.foldLeft(f())(_.schema(_))

}
