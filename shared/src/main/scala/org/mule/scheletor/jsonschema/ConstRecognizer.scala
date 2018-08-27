package org.mule.scheletor.jsonschema

import org.mule.scheletor.{ObjLike, SchemaLoader}
import org.mule.scheletor.SchemaBuilder.constSchema
import org.mule.scheletor.SchemaLoader.Builder

object ConstRecognizer extends SchemaLoader.Recognizer {

  override def extract[V: ObjLike](loader: SchemaLoader[V]): Seq[Builder] =
    loader
      .get("const")
      .map(v => constSchema(v.value))
      .toSeq
}
