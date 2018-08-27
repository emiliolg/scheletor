package org.mule.scheletor.jsonschema

import org.mule.scheletor._
import org.yaml.model.YDocument.{list, obj}

trait CombinedLoaderTest extends LoaderTest {

  test("AnyOf Loader") {
    val schema = load[AnyOfSchema](
        obj(
            $id = "any of schema",
            anyOf = simpleTypes
        )
    )
    checkSchemas(schema.schemas)
  }
  test("OneOf Loader") {
    val schema = load[OneOfSchema](
        obj(
            $id = "one of schema",
            oneOf = simpleTypes
        )
    )
    checkSchemas(schema.schemas)
  }
  test("AllOf Loader") {
    val schema = load[AllOfSchema](
        obj(
            $id = "all of schema",
            allOf = simpleTypes
        )
    )
    checkSchemas(schema.schemas)
  }
  test("All Combined Loader") {
    val schema = load[AllOfSchema](
        obj(
            $id = "combined schema",
            allOf = simpleTypes,
            oneOf = simpleTypes,
            anyOf = simpleTypes
        )
    )
    schema.schemas should have size 3
    schema(0) shouldBe a[AllOfSchema]
    schema(1) shouldBe a[AnyOfSchema]
    schema(2) shouldBe a[OneOfSchema]
    for (s <- schema.schemas)
      checkSchemas(s.asInstanceOf[CombinedSchema].schemas)
  }

  test("Combined With Base") {
    val schema = load[AllOfSchema](
        obj(
            $id = "combined with base schema",
            `type` = "string",
            maxLength = 30,
            anyOf = list(
                obj(minLength = 20),
                obj(pattern = "a.*")
            )
        )
    )
    schema.schemas should have size 2
    schema(0).as[StringSchema].maxLength.value shouldBe 30
    val anyOf = schema(1).as[AnyOfSchema]
    anyOf.schemas should have size 2
    anyOf(0).as[StringSchema].minLength.value shouldBe 20
    anyOf(1).as[StringSchema].pattern.value.toString shouldBe "a.*"
  }
  test("Combined With Multiple Base") {
    val schema = load[AllOfSchema](
        obj(
            $id = "combined with multiple base schema",
            `type` = list("string", "integer"),
            anyOf = list(
                obj(minLength = 20),
                obj(pattern = "a.*")
            )
        )
    )
    schema.schemas should have size 2

    val types = schema(0).as[AnyOfSchema]
    types.schemas should have size 2
    types(0).as[StringSchema]
    types(1).as[NumberSchema].integer shouldBe true

    val attrs = schema(1).as[AnyOfSchema]
    attrs.schemas should have size 2
    attrs(0).as[StringSchema].minLength.value shouldBe 20
    attrs(1).as[StringSchema].pattern.value.toString shouldBe "a.*"
  }

  private def checkSchemas(schemas: List[Schema]) = {
    schemas should have size 5
    schemas.head shouldBe a[NumberSchema]
    schemas(1) shouldBe a[StringSchema]
    schemas(2) shouldBe a[BooleanSchema]
    schemas(3) shouldBe a[TrueSchema]
    schemas(4) shouldBe a[FalseSchema]
  }
}
