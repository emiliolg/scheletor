package org.mulesoft.scheletor.jsonschema
import org.mulesoft.scheletor.ObjLike._
import org.mulesoft.scheletor.SchemaBuilder._
import org.mulesoft.scheletor.SchemaLoader.{Builder, Recognizer}
import org.mulesoft.scheletor._

object JsonSchemaLoader {
  def load[V: ObjLike](input: V, version: SpecVersion = Draft7): Schema =
    new JsonSchemaLoader(input, version, Pointer.empty).load()

}

class JsonSchemaLoader[V: ObjLike](input: V,
                                   val version: SpecVersion,
                                   pointer: Pointer,
                                   root: Option[JsonSchemaLoader[V]] = None)
    extends SchemaLoader(input, pointer) {

  private val booleanSchema = input.asBoolean.map(boolConstSchema)

  override val rootLoader: JsonSchemaLoader[V]    = root.getOrElse(this)
  private val referenceLoader: ReferenceLoader[V] = root.map(_.referenceLoader).getOrElse(new ReferenceLoader(this, obj))

  override def load(): Schema = booleanSchema.getOrElse {
    get("$ref") match {
      case Some(v) => referenceLoader.load(v.asString)
      case _       => super.load()
    }
  }

  protected def createChildLoader(input: V, loc: Pointer) =
    new JsonSchemaLoader[V](input, version, pointer, Some(rootLoader))

  override protected def recognizers: List[Recognizer] = List(
      TypeRecognizer(),
      EnumRecognizer,
      NotRecognizer,
      ConstRecognizer,
      CombinedRecognizer,
      IfRecognizer,
      TypeLessRecognizer()
  )

  override protected def addCommonProperties(builder: Builder): Unit = {
    ifPresent(version.idKeyword)(v => builder.id(v.asString))
    ifPresent("title")(v => builder.title(v.asString))
    ifPresent("description")(v => builder.description(v.asString))

    if (version == Draft7) {
      ifPresent("readOnly")(v => builder.readOnly(v.asBoolean))
      ifPresent("writeOnly")(v => builder.writeOnly(v.asBoolean))
    }
    ifPresent("nullable")(v => builder.nullable(v.asBoolean))
    // default ??
  }

  override def canBeSchema(v: V): Boolean = v.isObject || v.asBoolean.isDefined
}
