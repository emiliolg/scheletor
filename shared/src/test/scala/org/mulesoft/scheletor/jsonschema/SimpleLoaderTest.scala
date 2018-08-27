package org.mulesoft.scheletor.jsonschema
import org.mulesoft.scheletor._
import org.yaml.model.{YMap, YNode}
import org.yaml.model.YDocument.{list, obj}
import org.yaml.model.YNode.{fromMap, Null}

trait SimpleLoaderTest extends LoaderTest {

  test("V4 number schema") {
    val schema = load[NumberSchema](obj(
                                        id = "number",
                                        `type` = "number",
                                        minimum = 5,
                                        maximum = 10,
                                        multipleOf = 2,
                                        exclusiveMinimum = true,
                                        exclusiveMaximum = true
                                    ),
                                    Draft4)

    schema.minimum shouldBe None
    schema.maximum shouldBe None
    schema.multipleOf shouldBe Some(2)
    schema.exclusiveMinimum shouldBe Some(5)
    schema.exclusiveMaximum shouldBe Some(10)
    schema.id shouldBe "number"
  }
  test("V7 number schema") {
    val schema = load[NumberSchema](
        obj(
            $id = "number",
            `type` = "number",
            multipleOf = 2,
            exclusiveMinimum = 5,
            exclusiveMaximum = 10
        ))

    schema.minimum shouldBe None
    schema.maximum shouldBe None
    schema.multipleOf shouldBe Some(2)
    schema.exclusiveMinimum shouldBe Some(5)
    schema.exclusiveMaximum shouldBe Some(10)
    schema.id shouldBe "number"
  }
  test("V4 integer schema") {
    val schema = load[NumberSchema](obj(
                                        id = "integer",
                                        `type` = "integer",
                                        minimum = 5,
                                        maximum = 10,
                                        multipleOf = 2,
                                        exclusiveMinimum = true,
                                        exclusiveMaximum = true
                                    ),
                                    Draft4)

    schema.minimum shouldBe None
    schema.maximum shouldBe None
    schema.multipleOf shouldBe Some(2)
    schema.exclusiveMinimum shouldBe Some(5)
    schema.exclusiveMaximum shouldBe Some(10)
    schema.id shouldBe "integer"
  }
  test("V7 integer schema") {
    val schema = load[NumberSchema](
        obj(
            $id = "integer",
            `type` = "integer",
            multipleOf = 2,
            exclusiveMinimum = 5,
            exclusiveMaximum = 10
        ))

    schema.minimum shouldBe None
    schema.maximum shouldBe None
    schema.multipleOf shouldBe Some(2)
    schema.exclusiveMinimum shouldBe Some(5)
    schema.exclusiveMaximum shouldBe Some(10)
    schema.id shouldBe "integer"
  }

  test("String schema") {
    val schema = load[StringSchema](
        obj(
            $id = "string",
            `type` = "string",
            minLength = 2,
            maxLength = 5
        ))

    schema.minLength shouldBe Some(2)
    schema.maxLength shouldBe Some(5)
    schema.pattern shouldBe None
    schema.id shouldBe "string"
  }
  test("Boolean Schema") {
    load[BooleanSchema](obj($id = "boolean", `type` = "boolean"))

  }

  test("Null Schema") {
    load[NullSchema](obj($id = "null", `type` = "null"))
  }

  test("Enum Schema") {
    val schema = load[EnumSchema[YNode]](
        obj(
            $id = "enum",
            enum = list(1, 2, "a", obj(a = "a", b = "b"), Null)
        ))
    schema.id shouldBe "enum"
    schema.possibleValues should contain theSameElementsAs List[YNode](1, 2, "a", obj(a = "a", b = "b"), Null)

  }
    test("Const Schema") {
        val schema = load[ConstSchema[YNode]](
            obj(
                $id = "const",
                const = obj(a = "a", b = "b")
            ))
        schema.id shouldBe "const"
        schema.value shouldBe obj(a = "a", b = "b")

    }

  test("True, False, Empty Schema") {
    load[TrueSchema](true)
    load[FalseSchema](false)
    load[EmptySchema](YMap.empty)
  }

  test("Object Schema") {
    val schema = load[ObjectSchema](
        obj(
            $id = "simple object",
            `type` = "object",
            minProperties = 1,
            maxProperties = 10,
            properties = obj(
                id = obj(
                    `type` = "integer",
                    minimum = 1
                ),
                name = obj(
                    `type` = "string",
                    minLength = 2
                )
            ),
            required = list("id"),
            additionalProperties = obj(
                `type` = "string",
                pattern = "a*"
            ),
            dependencies = obj(
                name = list("a1", "a2")
            )
        ))

    schema.id shouldBe "simple object"
    schema.minProperties shouldBe Some(1)
    schema.maxProperties shouldBe Some(10)
    schema.required shouldBe List("id")
    schema.properties.keySet shouldBe Set("id", "name")

    schema.properties("id") match {
      case ns: NumberSchema =>
        ns.minimum shouldBe Some(1)
        ns.maximum shouldBe None
        ns.integer shouldBe true
      case _ => fail("Not a NumberSchema")
    }
    schema.properties("name") match {
      case ss: StringSchema =>
        ss.minLength shouldBe Some(2)
        ss.maxLength shouldBe None
      case _ => fail("Not an StringSchema")
    }
    schema.allowAdditionalProperties shouldBe true
    schema.additionalPropertiesSchema match {
      case Some(s: StringSchema) => s.pattern.toString shouldBe "Some(a*)"
      case _                     => fail(" Not a String Schema")
    }
    val ds = schema.dependencies("name")
    ds._1 shouldBe None
    ds._2 shouldBe Set("a1", "a2")
  }

  test("Array Schema All items") {
    def checkMin(s: Option[Schema], expectedMin: Int) = {
      s match {
        case Some(ns: NumberSchema) => ns.minimum shouldBe Some(expectedMin)
        case Some(_)                => fail("Not a NumberSchema")
        case _                      => fail("All items schema no present")
      }
    }

    val schema = load[ArraySchema](
        obj(
            $id = "simple array",
            `type` = "array",
            minItems = 1,
            maxItems = 10,
            uniqueItems = true,
            items = obj(
                `type` = "integer",
                minimum = 1
            ),
            contains = obj(
                `type` = "integer",
                minimum = 100
            )
        )
    )
    schema.minItems shouldBe Some(1)
    schema.maxItems shouldBe Some(10)
    schema.uniqueItems shouldBe true
    checkMin(schema.allItems, 1)
    checkMin(schema.contains, 100)
  }

  test("Tuple Schema") {
    val schema = load[ArraySchema](
        obj(
            $id = "tuple array",
            `type` = "array",
            items = simpleTypes,
            additionalItems = false
        )
    )
    schema.items should have size 5
    schema.allItems shouldBe None
    schema.items.head shouldBe a[NumberSchema]
    schema.items.head.asInstanceOf[NumberSchema].integer shouldBe true
    schema.items(1) shouldBe a[StringSchema]
    schema.items(2) shouldBe a[BooleanSchema]
    schema.allowAdditionalItems shouldBe false
    schema.additionalItemsSchema shouldBe None

    val schema2 = load[ArraySchema](
        obj(
            `type` = "array",
            items = simpleTypes,
            additionalItems = obj(`type` = "number")
        )
    )
    schema2.additionalItemsSchema match {
      case Some(ns: NumberSchema) => ns.integer shouldBe false
      case _                      => fail("Not a Number schema")
    }

  }

  test("Combined Loader") {
    val schema = load[AnyOfSchema](
        obj(
            $id = "any of schema",
            anyOf = simpleTypes
        )
    )
    val schemas = schema.schemas

    schemas should have size 5
    schemas.head shouldBe a[NumberSchema]
    schemas(1) shouldBe a[StringSchema]
    schemas(2) shouldBe a[BooleanSchema]
  }

}
