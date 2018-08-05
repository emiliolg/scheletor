package org.mulesoft.scheletor.jsonschema

import org.mulesoft.scheletor.{ObjLike, SchemaLoader}
import org.mulesoft.scheletor.SchemaBuilder.constSchema
import org.mulesoft.scheletor.SchemaLoader.Builder

object ConstRecognizer extends SchemaLoader.Recognizer {

  override def extract[V: ObjLike](loader: SchemaLoader[V]): Seq[Builder] =
    loader
      .get("const")
      .map(v => constSchema(v.value))
      .toSeq
}
