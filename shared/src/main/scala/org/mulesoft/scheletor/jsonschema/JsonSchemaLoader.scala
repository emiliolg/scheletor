package org.mulesoft.scheletor.jsonschema
import org.mulesoft.scheletor.{ObjLike, Pointer, Schema, SchemaLoader}
import org.mulesoft.scheletor.SchemaLoader.{Builder, Recognizer}

object JsonSchemaLoader {
  def load[V: ObjLike](input: V, version: SpecVersion = Draft7): Schema =
    new JsonSchemaLoader(input, version, Pointer.empty).load()
}

class JsonSchemaLoader[V: ObjLike](input: V, val version: SpecVersion, pointer: Pointer)
    extends SchemaLoader(input, pointer) {

  protected def createSubSchema(input: V, loc: Pointer) = new JsonSchemaLoader[V](input, version, pointer)

  override protected def recognizers: List[Recognizer] = List(
      TypeRecognizer(),
      EnumRecognizer,
      ConstRecognizer,
      CombinedRecognizer,
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
}
