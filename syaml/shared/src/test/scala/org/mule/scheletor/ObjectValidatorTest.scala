package org.mule.scheletor

import org.mule.scheletor.ErrorType._
import org.mule.scheletor.SchemaBuilder._
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.{YMap, YNode}
import org.yaml.model.YDocument.obj
import org.yaml.model.YNode.Null
import org.mule.scheletor.syaml._

trait ObjectValidatorTest extends FunSuite with Matchers {
  val emptyObj: YNode = YMap.empty

  private def validate(doc: YNode)(implicit schema: Schema) = Validator.validate(schema, doc)
  private def listOf(errors: ErrorType*)                        = errors.map(ValidationError("/", _)).toList
  private def error(location: String, errorType: ErrorType)     = ValidationError(Pointer(location), errorType)

  test("Min Max Properties") {
    implicit val schema: ObjectSchema = objectSchema.minProperties(2).maxProperties(3).build

    validate(obj(a = "x", b = 10)) shouldBe empty

    validate(obj(a = "x")) shouldBe listOf(MinProperties(2, 1))

    validate(obj(a = "x", b = 10, c = true, d = "d")) shouldBe listOf(MaxProperties(3, 4))
  }

  test("Additional Properties") {
    implicit val schema: ObjectSchema = objectSchema.property("a").property("b").additionalProperties(false).build

    validate(obj(a = "x", b = "y")) shouldBe empty
    validate(obj(a = "x", c = Null, d = Null)) shouldBe listOf(InvalidProperty("c"), InvalidProperty("d"))

    val schema2 = objectSchema.additionalProperties(false).build
    validate(obj(boolProp = "invalid"))(schema2) shouldBe listOf(InvalidProperty("boolProp"))

  }

  test("Additional Properties Schema") {
    implicit val schema: ObjectSchema =
      objectSchema.property("a").property("b").additionalProperties(BooleanSchema).build

    validate(obj(a = "x", b = "y")) shouldBe empty
    validate(obj(a = "x", c = Null, d = "x")) shouldBe List(error("/c", Nullable), error("/d", NotBoolean))

  }

  test("Property Violation") {
    implicit val schema: ObjectSchema = objectSchema.property("boolProp", BooleanSchema).build
    validate(obj(boolProp = "invalid")) shouldBe List(error("/boolProp", NotBoolean))
    validate(emptyObj) shouldBe empty

  }
  test("No Properties") {
    implicit val schema: ObjectSchema = objectSchema.build
    validate(obj(hello = "world")) shouldBe empty
  }
  test("Required Properties") {
    implicit val schema: ObjectSchema =
      objectSchema.property("boolProp", BooleanSchema).property("nullProp", NullSchema).required("boolProp").build
    validate(emptyObj) shouldBe listOf(MissingProperty("boolProp"))
  }

  test("Multiple Violations") {
    implicit val schema: ObjectSchema = objectSchema
      .property("numberProp", NumberSchema)
      .property("^string.*".r, SchemaBuilder.StringSchema)
      .property("boolProp", BooleanSchema)
      .required("boolProp")
      .build

    validate(obj(numberProp = "not number", stringPatternMatch = 2)) shouldBe List(
        error("/", MissingProperty("boolProp")),
        error("/stringPatternMatch", NotString),
        error("/numberProp", NotNumber)
    )
  }

  test("Multiple Violations Nested") {
    val s = () =>
      SchemaBuilder.objectSchema
        .property("numberProp", NumberSchema)
        .property("^string.*".r, StringSchema)
        .property("boolProp", BooleanSchema)
        .required("boolProp")

    implicit val schema: ObjectSchema = s().property("nested", s().property("nested", s().build).build).build

    val doc = obj(
        numberProp = "not number",
        stringPatternMatch = 2,
        nested = obj(
            numberProp = "not number 1",
            stringPatternMatch = 11,
            nested = obj(
                numberProp = "not number 2",
                stringPatternMatch = 22
            )
        )
    )
    validate(doc) shouldBe List(
        error("/", MissingProperty("boolProp")),
        error("/stringPatternMatch", NotString),
        error("/numberProp", NotNumber),
        error("/nested", MissingProperty("boolProp")),
        error("/nested/stringPatternMatch", NotString),
        error("/nested/numberProp", NotNumber),
        error("/nested/nested", MissingProperty("boolProp")),
        error("/nested/nested/stringPatternMatch", NotString),
        error("/nested/nested/numberProp", NotNumber)
    )

    val emptySchema = SchemaBuilder.objectSchema.build
    validate(doc)(emptySchema) shouldBe empty

  }

  test("Pattern Property Violation") {
    implicit val schema: ObjectSchema =
      objectSchema.property("^b_.*".r, BooleanSchema).property("^s_.*".r, StringSchema).build

    validate(obj(b_1 = true, b_2 = false, s_1 = "xx", s_2 = "yy")) shouldBe empty
    validate(obj(b_1 = Null)) shouldBe List(error("/b_1", Nullable))

    validate(obj(b_1 = true, b_2 = "xx", s_1 = false, s_2 = "yy")) shouldBe List(
        error("/b_2", NotBoolean),
        error("/s_1", NotString)
    )
  }

  test("Override Additional Property") {
    implicit val schema: ObjectSchema =
      objectSchema.property("^v.*".r, EmptySchema).additionalProperties(false).build
    validate(obj(vfoo = "bar")) shouldBe empty
  }
  test("Override Additional Property Schema") {
    implicit val schema: ObjectSchema =
      objectSchema.property("^v.*".r, EmptySchema).additionalProperties(NumberSchema).build
    validate(obj(vfoo = "bar", nn = 10)) shouldBe empty
  }

  test("Property Names Failure") {
    implicit val schema: ObjectSchema = objectSchema
      .propertyNames(
          stringSchema.minLength(5).maxLength(7).build
      )
      .build

    validate(obj(a = Null)) shouldBe List(error("/a", MinLength(5, 1)))
  }

  test("Property Dependency Violation") {
    implicit val schema: ObjectSchema = objectSchema
      .property("ifPresent", NullSchema)
      .property("mustBePresent", BooleanSchema)
      .property("mustBePresentToo", BooleanSchema)
      .dependency("ifPresent", "mustBePresent")
      .dependency("ifPresent", "mustBePresentToo")
      .build

    validate(obj(name = "John Doe", ifPresent = Null)) shouldBe listOf(MissingProperty("mustBePresent"),
                                                                       MissingProperty("mustBePresentToo"))
  }
  test("Schema Dependency Violation") {
    implicit val schema: ObjectSchema = objectSchema
      .property("name", StringSchema)
      .property("creditCard", NumberSchema)
      .property("billingName", StringSchema)
      .dependency("creditCard", objectSchema.required("billingAddress", StringSchema).build)
      .dependency("creditCard", "billingName")
      .dependency("name", objectSchema.required("age", IntegerSchema).build)
      .build

    validate(obj(name = "John Doe", creditCard = 123, billingAddress = "Monroe 123", billingName = "J Doe", age = 10)) shouldBe empty

    validate(obj(name = "John Doe", creditCard = 123, billingAddress = "Monroe 123")) shouldBe listOf(
        MissingProperty("age"),
        MissingProperty("billingName")
    )

    validate(obj(name = "John Doe", creditCard = 123, billingAddress = 123, age = 10)) shouldBe List(
        error("/", MissingProperty("billingName")),
        error("/billingAddress", NotString)
    )
  }
  test("Pointer Bubbling Test") {
    val size                          = numberSchema.minimum(0.0).build
    val rectangle                     = objectSchema.property("a", size).property("b", size).build
    implicit val schema: ObjectSchema = objectSchema.property("rectangle", rectangle).build

    validate(obj(rectangle = obj(a = -5, b = 5))) shouldBe List(
        error("/rectangle/a", OutOfRange(-5.0, "less than", 0.0))
    )
    validate(obj(rectangle = obj(a = -5, b = "asd"))) shouldBe List(
        error("/rectangle/a", OutOfRange(-5.0, "less than", 0.0)),
        error("/rectangle/b", NotNumber)
    )
  }

}
