package org.mulesoft.scheletor

import org.mulesoft.scheletor.ErrorType._
import org.mulesoft.scheletor.SchemaBuilder._
import org.scalatest.{FunSuite, Matchers}
import org.yaml.model.{YNode, YSequence}
import org.yaml.model.YDocument.{list, obj}

trait SimpleValidatorTest extends FunSuite with Matchers {

  private def validate(node: YNode)(implicit schema: Schema) = Validator.validate(schema, node)

  private def listOf(errors: ErrorType*) = errors.map(ValidationError("/", _)).toList

  test("String Schema") {
    implicit val schema: StringSchema = stringSchema.id("Test").minLength(2).maxLength(10).pattern("^abc")()

    validate("abcd") shouldBe empty

    validate(YNode.Null) shouldBe listOf(Nullable)

    validate("a") shouldBe listOf(MinLength(2, 1), MatchError("a", "^abc".r))

    validate("abcdefghyjk") shouldBe listOf(MaxLength(10, 11))

    validate("xabcd") shouldBe listOf(MatchError("xabcd", "^abc".r))

    validate(100) shouldBe listOf(NotString)
  }
  test("String Like Schema") {
    implicit val schema: StringSchema = stringLikeSchema.id("Test").minLength(2).maxLength(10).pattern("^abc")()

    validate("abcd") shouldBe empty

    validate(YNode.Null) shouldBe listOf(Nullable)

    validate("a") shouldBe listOf(MinLength(2, 1), MatchError("a", "^abc".r))

    validate("abcdefghyjk") shouldBe listOf(MaxLength(10, 11))

    validate("xabcd") shouldBe listOf(MatchError("xabcd", "^abc".r))

    validate(100) shouldBe empty
  }

  test("Integer Schema") {
    implicit val schema: NumberSchema = integerSchema.id("Test").minimum(10L).maximum(1000L).multipleOf(5L)()

    validate(null) shouldBe listOf(Nullable)

    validate(15) shouldBe empty
    validate(10) shouldBe empty
    validate(1000) shouldBe empty

    validate(0) shouldBe listOf(OutOfRange(0, "less than", 10))
    validate(2000) shouldBe listOf(OutOfRange(2000, "greater than", 1000))
    validate(16) shouldBe listOf(NotMultipleOf(16, 5))

    validate("xxx") shouldBe listOf(NotInt)
    validate(10.2) shouldBe listOf(NotInt)

  }

  test("Integer Schema Exclusive") {
    implicit val schema2: NumberSchema =
      integerSchema.id("Test").exclusiveMinimum(10L).exclusiveMaximum(1000L)()

    validate(10) shouldBe listOf(OutOfRange(10, "less than or equal", 10))
    validate(1000) shouldBe listOf(OutOfRange(1000, "greater than or equal", 1000))

    validate(2) shouldBe listOf(OutOfRange(2, "less than or equal", 10))
    validate(2000) shouldBe listOf(OutOfRange(2000, "greater than or equal", 1000))
  }

  test("Number Schema") {
    implicit val schema: NumberSchema = numberSchema.id("Test").minimum(10.1).maximum(1000.2).multipleOf(0.1)()

    validate(15) shouldBe empty
    validate(10.1) shouldBe empty
    validate(1000.2) shouldBe empty

    validate(null) shouldBe listOf(Nullable)

    validate(2.3) shouldBe listOf(OutOfRange(2.3, "less than", 10.1))
    validate(2000) shouldBe listOf(OutOfRange(2000.0, "greater than", 1000.2))
    validate(16.03) shouldBe listOf(NotMultipleOf(16.03, 0.1))

    validate("xxx") shouldBe listOf(NotNumber)

  }

  test("Number Like Schema") {
    implicit val schema: NumberSchema =
      numberLikeSchema.id("Test").minimum(10.1).maximum(1000.2).multipleOf(0.1)()

    validate(15) shouldBe empty
    validate(10.1) shouldBe empty
    validate(1000.2) shouldBe empty

    validate(null) shouldBe listOf(Nullable)

    validate(2.3) shouldBe listOf(OutOfRange(2.3, "less than", 10.1))
    validate(2000) shouldBe listOf(OutOfRange(2000.0, "greater than", 1000.2))
    validate(16.03) shouldBe listOf(NotMultipleOf(16.03, 0.1))

    validate("xxx") shouldBe empty

  }
  test("Number Schema Exclusive") {
    implicit val schema2: NumberSchema =
      numberSchema.id("Test").exclusiveMinimum(10.1).exclusiveMaximum(1000.2)()

    validate(10.1) shouldBe listOf(OutOfRange(10.1, "less than or equal", 10.1))
    validate(1000.2) shouldBe listOf(OutOfRange(1000.2, "greater than or equal", 1000.2))

    validate(2.3) shouldBe listOf(OutOfRange(2.3, "less than or equal", 10.1))
    validate(2000) shouldBe listOf(OutOfRange(2000.0, "greater than or equal", 1000.2))
  }

  test("Boolean Schema") {
    implicit val schema: BooleanSchema = booleanSchema.id("Test").build

    validate(true) shouldBe empty
    validate(false) shouldBe empty
    validate(null) shouldBe listOf(Nullable)
    validate("xxx") shouldBe listOf(NotBoolean)
  }

  test("Enum Schema") {
    implicit val schema: EnumSchema[YNode] =
      enumSchema[YNode].id("Test").value("a").value(10).value(null).value(true).build

    validate(true) shouldBe empty
    validate(null) shouldBe empty
    validate(10) shouldBe empty
    validate("a") shouldBe empty

    validate(false) shouldBe listOf(NotEnum)
    validate("xxx") shouldBe listOf(NotEnum)
    validate(101) shouldBe listOf(NotEnum)
    validate(YSequence(10, "a")) shouldBe listOf(NotEnum)

    val schema2: EnumSchema[YNode] =
      enumSchema[YNode].id("Test2").value("a").value(10).value(true).build

    validate(true)(schema2) shouldBe empty
    validate(10)(schema2) shouldBe empty
    validate(null)(schema2) shouldBe listOf(Nullable)
  }

  test("Not Schema") {
    implicit val schema: Schema = notSchema(NumberSchema).build

    validate("x") shouldBe empty
    validate(10) shouldBe listOf(MustNotValidate(NumberSchema))
  }
  test("Const Schema") {
    implicit val schema: Schema = constSchema(YNode(10.23)).build

    validate(10.23) shouldBe empty
    validate(10) shouldBe listOf(ConstFailed("10.23", "10"))
  }
  test("Complex Const Schema") {
    val const0 = obj(a = 10, b = list(1, 2))
    val const1 = obj(b = list(1, 2), a = 10)
    val const2 = obj(a = 10, b = list(1, 3, 7))
    val const3 = obj(a = 10, b = list(1, 2), c = "x")

    implicit val schema: Schema = constSchema(const0).build

    validate(const0) shouldBe empty
    validate(const1) shouldBe empty
    validate(const2) shouldBe listOf(ConstFailed("{a: 10, b: [1, 2]}", "{a: 10, b: [1, 3, 7]}"))
    validate(const3) shouldBe listOf(ConstFailed("{a: 10, b: [1, 2]}", "{a: 10, b: [1, 2], c: x}"))
  }

}
