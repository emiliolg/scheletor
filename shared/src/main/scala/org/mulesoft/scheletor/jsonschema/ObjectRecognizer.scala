package org.mulesoft.scheletor.jsonschema
import org.mulesoft.scheletor.ObjLike
import org.mulesoft.scheletor.ObjLike._
import org.mulesoft.scheletor.SchemaBuilder.{objectSchema, ObjBuilder}
import org.mulesoft.scheletor.SchemaLoader.Builder

object ObjectRecognizer extends TypeRecognizer.Instance("object") {
  override val propertyNames: Set[String] = Set("minProperties",
                                                "maxProperties",
                                                "properties",
                                                "patternProperties",
                                                "additionalProperties",
                                                "dependencies",
                                                "propertyNames")

  override def createBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Builder = loadProperties(loader, objectSchema)

  override def createBareBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Option[Builder] =
    Some(loadProperties(loader, new ObjBuilder(objectType = false)))

  private[jsonschema] def loadProperties[V: ObjLike](l: JsonSchemaLoader[V], builder: ObjBuilder): Builder = {
    l.ifPresent("minProperties")(v => builder.minProperties(v.asInt))
    l.ifPresent("maxProperties")(v => builder.maxProperties(v.asInt))
    l.ifPresent("required")(v => for (e <- v.asStringSeq) builder.required(e))

    l.ifPresent("properties") { v =>
      val obj = v.asObject
      for (name <- obj.properties) builder.property(name, l.loadSubSchema(obj(name), name))
    }

    l.ifPresent("patternProperties") { v =>
      val obj = v.asObject
      for (name <- obj.properties) builder.property(name.r, l.loadSubSchema(obj(name), name))
    }
    l.ifPresent("additionalProperties")(loadAdditionalProperties(l, builder))
    l.ifPresent("dependencies")(loadDependencies(l, builder))

    if (l.version.supportPropertyNames)
      l.ifPresent("propertyNames")(v => builder.propertyNames(v.loadSubSchema()))
    builder
  }

  private def loadAdditionalProperties[V: ObjLike](l: JsonSchemaLoader[V], b: ObjBuilder)(v: l.Value): Unit = {
    v.value.asBoolean match {
      case Some(bool)            => b.additionalProperties(bool)
      case _ if v.value.isObject => b.additionalProperties(v.loadSubSchema())
      case _                     => v.error("Boolean or Object")
    }
  }

  private def loadDependencies[V: ObjLike](l: JsonSchemaLoader[V], b: ObjBuilder)(v: l.Value): Unit = {
    val obj = v.asObject
    for (name <- obj.properties) {
      val d          = obj(name)
      val dependency = l.Value(d, name)

      d.asArray match {
        case Some(_)               => for (e <- dependency.asStringSeq) b.dependency(name, e)
        case _ if l.canBeSchema(d) => b.dependency(name, dependency.loadSubSchema())
        case _                     => v.error("Array or Schema")
      }
    }
  }
}
