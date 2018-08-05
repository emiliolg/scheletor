package org.mulesoft.scheletor.jsonschema

import org.mulesoft.scheletor._
import org.yaml.model.YDocument.obj

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
    schema.schemas(0) shouldBe a[AllOfSchema]
    schema.schemas(1) shouldBe a[AnyOfSchema]
    schema.schemas(2) shouldBe a[OneOfSchema]
    for (s <- schema.schemas)
      checkSchemas(s.asInstanceOf[CombinedSchema].schemas)
  }

  private def checkSchemas(schemas: List[Schema]) = {
    schemas should have size 3
    schemas.head shouldBe a[NumberSchema]
    schemas(1) shouldBe a[StringSchema]
    schemas(2) shouldBe a[BooleanSchema]
  }
}
