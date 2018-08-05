package org.mulesoft.scheletor.jsonschema

import org.mulesoft.scheletor.{ObjLike, SchemaLoader}
import org.mulesoft.scheletor.SchemaBuilder._
import org.mulesoft.scheletor.SchemaLoader.Builder

object NotRecognizer extends SchemaLoader.Recognizer {

  override def extract[V: ObjLike](loader: SchemaLoader[V]): Seq[Builder] =
    loader
      .get("not")
      .map(v => notSchema(v.loadSubSchema()))
      .toSeq
}
