package org.mule.scheletor

case class ValidationError(location: Pointer, errorType: ErrorType) {
  override def toString: String = "#" + location + ": " + errorType.defaultMessage.format(errorType.args: _*)
}
object ValidationError {
  def apply(location: String, errorType: ErrorType): ValidationError =
    new ValidationError(Pointer(location), errorType)
}
