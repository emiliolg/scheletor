package org.mule.scheletor.jsonschema

import org.mule.scheletor.ErrorType.OutOfRange
import org.mule.scheletor.SchemaBuilder.reference
import org.mule.scheletor._
import org.mule.scheletor.syaml._
import org.yaml.model.YDocument.obj
import org.yaml.model.{YDocument, YNode}

trait RefSchemaTest extends LoaderTest {
  private def error(location: String, errorType: ErrorType) = ValidationError(Pointer(location), errorType)

  private def validate(node:YNode, schema: Schema) = YDocument(node).validate(schema)

  test("construction") {
    val r = reference("#/a/b")
    r.build shouldBe theSameInstanceAs(r.build)
  }

  test("simple references") {
    val doc = obj(
        `type` = "object",
        properties = obj(
            pointer = obj($ref = "#/definitions/a"),
            b = obj(`type` = "number")
        ),
        definitions = obj(
            a = obj(
                description = "small natural number",
                `type` = "integer",
                minimum = 1,
                maximum = 9
            )
        )
    )
    val schema = load[ObjectSchema](doc)
    val ps     = schema.properties

    val ref = ps("pointer").as[RefSchema]
    val n   = ref.refSchema.as[NumberSchema]
    n.minimum.value shouldBe 1
    n.maximum.value shouldBe 9

    validate(obj(pointer = 7, b = 100), schema) shouldBe empty
    validate(obj(pointer = 100, b = 100), schema) shouldBe List(error("/pointer", OutOfRange(100, "greater than", 9.0)))

  }

  test("recursive references") {
    val doc = obj(
        `type` = "object",
        properties = obj(
            pointer = obj($ref = "#/definitions/a"),
            b = obj(`type` = "number")
        ),
        definitions = obj(
            a = obj(
                description = "int list",
                `type` = "object",
                nullable = true,
                properties = obj(
                    value = obj(`type` = "integer"),
                    next = obj($ref = "#/definitions/a")
                )
            )
        )
    )
    val schema = load[ObjectSchema](doc)
    val ref    = schema.properties("pointer").as[RefSchema]
    val list   = ref.refSchema.as[ObjectSchema]
    list.nullable shouldBe true
    val ps = list.properties
    ps("value").as[NumberSchema].integer shouldBe true
    ps("next").as[RefSchema].refSchema.as[ObjectSchema] should be theSameInstanceAs list

    validate(obj(pointer = null, b = 100), schema) shouldBe empty
    validate(obj(pointer = obj(value = 1, next = obj(value = 2, next = null))), schema) shouldBe empty
  }

}
