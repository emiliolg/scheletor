package org.mule.scheletor.jsonschema

import org.mule.scheletor.{ObjLike, SchemaLoader}
import org.mule.scheletor.SchemaBuilder.enumSchema
import org.mule.scheletor.SchemaLoader.Builder

object EnumRecognizer extends SchemaLoader.Recognizer {
  override def extract[V: ObjLike](loader: SchemaLoader[V]): Seq[Builder] =
    loader
      .get("enum")
      .map {
        _.asArray.foldLeft(enumSchema)(_.value(_))
      }
      .toSeq
}
