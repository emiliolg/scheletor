package org.mulesoft.scheletor.jsonschema

import org.mulesoft.scheletor.{ObjLike, SchemaLoader}
import org.mulesoft.scheletor.SchemaBuilder.enumSchema
import org.mulesoft.scheletor.SchemaLoader.Builder

object EnumRecognizer extends SchemaLoader.Recognizer {
  override def extract[V: ObjLike](loader: SchemaLoader[V]): Seq[Builder] =
    loader
      .get("enum")
      .map {
        _.asArray.foldLeft(enumSchema)(_.value(_))
      }
      .toSeq
}
