package org.mule.scheletor.jsonschema

import org.mule.scheletor.{ObjLike, SchemaLoader}
import org.mule.scheletor.SchemaBuilder._
import org.mule.scheletor.SchemaLoader.Builder

object NotRecognizer extends SchemaLoader.Recognizer {

  override def extract[V: ObjLike](loader: SchemaLoader[V]): Seq[Builder] =
    loader
      .get("not")
      .map(v => notSchema(v.loadSubSchema()))
      .toSeq
}
