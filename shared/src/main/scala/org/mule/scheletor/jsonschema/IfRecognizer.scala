package org.mule.scheletor.jsonschema

import org.mule.scheletor.SchemaLoader.Recognizer
import org.mule.scheletor.SchemaBuilder._
import org.mule.scheletor.SchemaLoader.Builder
import org.mule.scheletor.{ObjLike, SchemaBuilder, SchemaLoader}

object IfRecognizer extends Recognizer {

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
