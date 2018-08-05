package org.mulesoft.scheletor.jsonschema

import org.mulesoft.scheletor._
import org.mulesoft.scheletor.ErrorType._
import org.mulesoft.scheletor.SchemaBuilder._
import org.mulesoft.scheletor.SchemaLoader.Builder

object TypeRecognizer {

  def apply(typeRecognizerInstances: Instance*): TypeRecognizer = new TypeRecognizer(typeRecognizerInstances: _*)

  def apply(): TypeRecognizer = new TypeRecognizer(
      NumberRecognizer,
      IntegerRecognizer,
      StringRecognizer,
      BooleanRecognizer,
      NullRecognizer,
      ArrayRecognizer,
      ObjectRecognizer
  )

  abstract class Instance(val typeName: String) {
    def propertyNames: Set[String] = Set.empty
    def createBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Builder
    def createBareBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Builder = throw IllegalStateException
  }
}

class TypeRecognizer(typeRecognizerInstances: TypeRecognizer.Instance*) extends SchemaLoader.Recognizer {
  private val instances = typeRecognizerInstances.map(i => i.typeName -> i).toMap

  override def extract[V: ObjLike](l: SchemaLoader[V]): Seq[Builder] =
    l.get("type")
      .map { t =>
        val instance = instances.getOrElse(t.asString, l.error("type", UnknownType("typeName")))
        instance.createBuilder(l.asInstanceOf[JsonSchemaLoader[V]])
      }
      .toSeq

}
object TypeLessRecognizer {
  def apply(): TypeRecognizer = new TypeRecognizer(
      NumberRecognizer,
      StringRecognizer,
      ArrayRecognizer,
      ObjectRecognizer
  )
}

class TypeLessRecognizer(instances: TypeRecognizer.Instance*) extends SchemaLoader.Recognizer {
  override def extract[V: ObjLike](l: SchemaLoader[V]): Seq[Builder] =
    for {
      i <- instances
      if l.containsAnyOf(i.propertyNames)
    } yield i.createBareBuilder(l.asInstanceOf[JsonSchemaLoader[V]])
}

object IntegerRecognizer extends TypeRecognizer.Instance("integer") {
  override def createBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Builder =
    NumberRecognizer.loadProperties(loader, integerSchema)
}

object BooleanRecognizer extends TypeRecognizer.Instance("boolean") {
  override def createBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Builder = booleanSchema
}

object NullRecognizer extends TypeRecognizer.Instance("null") {
  override def createBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Builder = nullSchema
}

object StringRecognizer extends TypeRecognizer.Instance("string") {
  override val propertyNames: Set[String] = Set("minLength", "maxLength", "pattern", "format")

  override def createBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Builder = loadProperties(loader, stringSchema)

  override def createBareBuilder[V: ObjLike](l: JsonSchemaLoader[V]): Builder = loadProperties(l, new Str(false))

  private def loadProperties[V: ObjLike](l: JsonSchemaLoader[V], builder: Str) = {
    l.ifPresent("minLength")(v => builder.minLength(v.asInt))
    l.ifPresent("maxLength")(v => builder.maxLength(v.asInt))
    l.ifPresent("pattern")(v => builder.pattern(v.asString))
    l.ifPresent("format") { v =>
      val format = v.asString
      builder.formatValidator(FormatValidator(format).getOrElse(l.error("format", IllegalFormat(format))))
    }
    builder
  }
}

object NumberRecognizer extends TypeRecognizer.Instance("number") {

  override val propertyNames: Set[String] =
    Set("minimum", "maximum", "multipleOf", "exclusiveMinimum", "exclusiveMaximum")

  override def createBuilder[V: ObjLike](loader: JsonSchemaLoader[V]): Builder = loadProperties(loader, numberSchema)

  override def createBareBuilder[V: ObjLike](l: JsonSchemaLoader[V]): Builder =
    loadProperties(l, new Num(numberType = false))

  private[jsonschema] def loadProperties[V: ObjLike](l: JsonSchemaLoader[V], builder: Num) = {
    l.ifPresent("minimum")(v => builder.minimum(v.asDouble))
    l.ifPresent("maximum")(v => builder.maximum(v.asDouble))
    l.ifPresent("multipleOf")(v => builder.multipleOf(v.asDouble))
    if (l.version == Draft4) {
      l.ifPresent("exclusiveMinimum")(v => builder.exclusiveMinimum(v.asBoolean))
      l.ifPresent("exclusiveMaximum")(v => builder.exclusiveMaximum(v.asBoolean))
    }
    else {
      l.ifPresent("exclusiveMinimum")(v => builder.exclusiveMinimum(v.asDouble))
      l.ifPresent("exclusiveMaximum")(v => builder.exclusiveMaximum(v.asDouble))
    }
    builder
  }
}
