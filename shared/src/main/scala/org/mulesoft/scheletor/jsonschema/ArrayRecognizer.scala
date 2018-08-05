package org.mulesoft.scheletor.jsonschema
import org.mulesoft.scheletor.ObjLike
import org.mulesoft.scheletor.ObjLike._
import org.mulesoft.scheletor.SchemaBuilder.{arraySchema, Arr}
import org.mulesoft.scheletor.SchemaLoader.Builder

object ArrayRecognizer extends TypeRecognizer.Instance("array") {

  override def propertyNames: Set[String] = Set("minItems", "maxItems", "uniqueItems", "items", "additionalItems")

  override def createBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Builder = loadProperties(loader, arraySchema)

  override def createBareBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Builder = loadProperties(loader, new Arr(false))

  private[jsonschema] def loadProperties[V: ObjLike](l: JsonSchemaLoader[V], builder: Arr): Builder = {
    l.ifPresent("minItems")(v => builder.minItems(v.asInt))
    l.ifPresent("maxItems")(v => builder.maxItems(v.asInt))
    l.ifPresent("uniqueItems")(v => builder.uniqueItems(v.asBoolean))
    l.ifPresent("items") { v =>
      v.value.asArray match {
        case Some(_)               => for (s <- v.asSchemaSeq) builder.item(s)
        case _ if v.value.isObject => builder.allItems(v.loadSubSchema())
        case _                     => v.error("Array or Object")
      }
    }
    l.ifPresent("additionalItems") { v =>
      v.value.asBoolean match {
        case Some(bool)            => builder.additionalItems(bool)
        case _ if v.value.isObject => builder.additionalItems(v.loadSubSchema())
        case _                     => v.error("Boolean or Object")
      }

    }
    if (l.version.supportArrayContains) l.ifPresent("contains")(v => builder.contains(v.loadSubSchema()))
    builder
  }
}
