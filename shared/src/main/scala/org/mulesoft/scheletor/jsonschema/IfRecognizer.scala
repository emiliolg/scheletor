package org.mulesoft.scheletor.jsonschema

import org.mulesoft.scheletor.SchemaBuilder._
import org.mulesoft.scheletor.SchemaLoader.Builder
import org.mulesoft.scheletor.{ObjLike, SchemaBuilder, SchemaLoader}

object IfRecognizer extends SchemaLoader.Recognizer {

  final val keywords = Set("if", "then", "else")

  override def extract[V: ObjLike](l: SchemaLoader[V]): Seq[Builder] =
    if (!l.containsAnyOf(keywords)) Nil
    else
      Seq(
          ifSchema(
              l.schemaFor("if"),
              l.schemaFor("then"),
              l.schemaFor("else")
          ))

}
