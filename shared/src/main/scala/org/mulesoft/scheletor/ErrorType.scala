package org.mulesoft.scheletor

import scala.util.matching.Regex

case class ErrorType private (key: String, defaultMessage: String, args: Any*) {
  override def toString: String = key + (if (args.isEmpty) "" else args.mkString("(", ",", ")"))
}

object ErrorType {
  private final val EmptyArgs = Array()
  final val ReadOnly          = ErrorType("ReadOnly", "Value is read only")
  final val WriteOnly         = ErrorType("WriteOnly", "Value is write only")
  final val Nullable          = ErrorType("Nullable", "Value cannot be null")
  final val NotBoolean        = ErrorType("NotBoolean", "Not a Boolean")
  final val NotString         = ErrorType("NotString", "Not an String")
  final val NotInt            = ErrorType("NotInt", "Not an Integer")
  final val NotNumber         = ErrorType("NotNumber", "Not a Number")
  final val NotObject         = ErrorType("NotObject", "Not an Object")
  final val NotArray          = ErrorType("NotArray", "Not an Array")
  final val NotEnum           = ErrorType("NotEnum", "Not a valid enum value")
  final val NotUnique         = ErrorType("NotUnique", "It is duplicated")
  final val NotNull           = ErrorType("NotNull", "Null expected and value found")
  final val NoneMatched       = ErrorType("NoneMatched", "None of the schemas validate the input")
  final val False             = ErrorType("False", "False Schema always fail")

  final val DoesNotContain =
    ErrorType("DoesNotContain", "Array does not contain any item that validates against the specified schema")

  def apply(key: String, defaultMessage: String): ErrorType = new ErrorType(key, defaultMessage, EmptyArgs)

  final def MinLength(expected: Int, actual: Int) =
    ErrorType("MinLength", "Expecting minLength: %d, actual: %d", expected, actual)

  final def MaxLength(expected: Int, actual: Int) =
    ErrorType("MaxLength", "Expecting maxLength: %d, actual: %d", expected, actual)

  final def MatchError(value: String, pattern: Regex) =
    ErrorType("MatchError", "Value '%s' does not match pattern: %s", value, pattern.regex)

  final def OutOfRange(value: Any, op: String, limit: Any) =
    ErrorType("OutOfRange", "Value '%s' is %s '%s'", value.toString, op, limit.toString)

  final def NotMultipleOf(value: Any, divisor: Any) =
    ErrorType("NotMultipleOf", "Value '%s' is not a multiple of '%s'", value.toString, divisor.toString)

  final def MinProperties(expected: Int, actual: Int) =
    ErrorType("MinProperties", "Expecting a minimum of: %d properties, actual: %d", expected, actual)

  final def MaxProperties(expected: Int, actual: Int) =
    ErrorType("MaxProperties", "Expecting a maximum of: %d properties, actual: %d", expected, actual)

  final def MissingProperty(name: String) =
    ErrorType("MissingProperty", "Property '%s' was required and it is not provided", name)

  final def InvalidProperty(name: String) =
    ErrorType("InvalidProperty", "Property '%s' was not allowed", name)

  final def ConstFailed(expected: String, actual: String) =
    ErrorType("ConstFailed", "Expecting: '%s', actual: '%s'", expected, actual)

  final def MinItems(expected: Int, actual: Int) =
    ErrorType("MinItems", "Expecting a minimum of: %d items, actual: %d", expected, actual)

  final def MaxItems(expected: Int, actual: Int) =
    ErrorType("MaxItems", "Expecting a maximum of: %d items, actual: %d", expected, actual)

  final def MustNotValidate(innerSchema: Schema) =
    ErrorType("MustNotValidate", "Must not validate against schema: %s", innerSchema.toString)

  final def ManyMatched(n: Int) =
    ErrorType("ManyMatched", "A total of %d schemas validate the input, instead of only one", n)

  final def BadType(referenceName: String, typeName: String) =
    ErrorType("BadType", "%s must be a %s", referenceName, typeName)

  final def UnknownType(name: String) = ErrorType("UnknownType", "Unknown Type '%s'", name)

  final def IllegalFormat(name: String) = ErrorType("IllegalFormat", "Illegal format validator '%s'", name)

  final def InvalidRef(ref: String) = ErrorType("InvalidRef", "Invalid reference '%s'", ref)
}
