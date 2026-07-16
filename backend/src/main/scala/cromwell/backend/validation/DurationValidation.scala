package cromwell.backend.validation

import cats.syntax.validated._
import common.validation.ErrorOr.ErrorOr
import wom.types.{WomIntegerType, WomStringType}
import wom.values.{WomInteger, WomString, WomValue}

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

/**
  * Validates a runtime attribute expressed as a duration.
  */
object DurationValidation {
  def instance(attributeName: String): RuntimeAttributesValidation[FiniteDuration] =
    new DurationValidation(attributeName)

  def optional(attributeName: String): OptionalRuntimeAttributesValidation[FiniteDuration] =
    instance(attributeName).optional

  private[validation] val wrongTypeFormat =
    "Expecting %s runtime attribute to be an Integer (seconds) or String with format '1 hour'." +
      " Exception: %s"

  private[validation] def validateString(attributeName: String, value: String): ErrorOr[FiniteDuration] = {
    Try(scala.concurrent.duration.Duration(value)) match {
      case Success(fd: FiniteDuration) => fd.validNel
      case Success(_) => s"Expecting $attributeName runtime attribute to be a finite duration".invalidNel
      case Failure(throwable) =>
        wrongTypeFormat.format(attributeName, throwable.getMessage).invalidNel
    }
  }

  private[validation] def validateInteger(attributeName: String, value: Int): ErrorOr[FiniteDuration] = {
    if (value < 0)
      s"Expecting $attributeName runtime attribute value greater than or equal to 0".invalidNel
    else {
      import scala.concurrent.duration._
      value.seconds.validNel
    }
  }
}

class DurationValidation(attributeName: String) extends RuntimeAttributesValidation[FiniteDuration] {
  import DurationValidation._

  override def key = attributeName

  override def coercion = Seq(WomIntegerType, WomStringType)

  override protected def validateValue: PartialFunction[WomValue, ErrorOr[FiniteDuration]] = {
    case WomInteger(value) => DurationValidation.validateInteger(key, value)
    case WomString(value) => DurationValidation.validateString(key, value)
  }

  override def missingValueMessage: String = wrongTypeFormat.format(key, "Not supported WDL type value")
}
