import java.net.URL
import java.time.LocalDate

import Aliases._
import cats.data.Validated
import cats.implicits._
import enumeratum.{Enum, EnumEntry}

trait FromString[A] {
  def apply(str: String): V[A]
}

object FromString {
  implicit val id: FromString[String] =
    _.validNec

  implicit val intFromString: FromString[Int] =
    s => s.toIntOption.toValidNec(s"Can't parse int from `$s`")

  implicit val longFromString: FromString[Long] =
    s => s.toLongOption.toValidNec(s"Can't parse long from `$s`")

  implicit val doubleFromString: FromString[Double] =
    s => s.toDoubleOption.toValidNec(s"Can't parse double from `$s`")

  implicit val booleanFromString: FromString[Boolean] =
    s => s.toBooleanOption.toValidNec(s"Can't parse boolean from `$s`")

  implicit val dateFromString: FromString[LocalDate] = s =>
    Validated
      .catchNonFatal(LocalDate.parse(s))
      .leftMap(err => NEC(s"Can't parse date from `$s`. Error: `${err.getMessage}`"))

  implicit val urlFromString: FromString[URL] = s =>
    Validated
      .catchNonFatal(new URL(s))
      .leftMap(err => NEC(s"Can't parse URL from `$s`. Error: `${err.getMessage}`"))

  implicit def enumFromString[E <: EnumEntry](implicit enum: Enum[E]): FromString[E] =
    s => enum.withNameOption(s).toValidNec(s"`$s` is not member of ${enum.values.mkString("(", ", ", ")")}")
}
