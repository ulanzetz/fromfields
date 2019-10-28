import java.net.URL
import java.time.LocalDate

import Aliases._
import cats.data.Validated.Invalid
import cats.implicits._
import enumeratum.{Enum, EnumEntry}
import utest._

object FromFieldsTTest extends TestSuite {
  sealed trait InfoType extends EnumEntry.Lowercase
  object InfoType extends Enum[InfoType] {
    case object Default extends InfoType
    case object Custom  extends InfoType

    val values = findValues
  }

  case class Info(
    id: Long,
    title: String,
    infoType: InfoType,
    site: URL,
    creationDate: LocalDate,
    updateDate: Option[LocalDate],
    price: Double,
    comission: Double,
    isPersonal: Boolean
  )

  val tests = Tests {
    "demo case" - {
      val rawInfo = Map(
        "id"           -> Some("120"),
        "title"        -> Some("Название"),
        "type"         -> Some("default"),
        "site"         -> Some("https://kek.com"),
        "creationDate" -> Some("2019-10-10"),
        "updateDate"   -> None,
        "currentPrice" -> None,
        "nominalPrice" -> Some("100.0"),
        "comisson"     -> None,
        "is_personal"  -> Some("true")
      )

      val info = Info(
        id = 120,
        title = "Название",
        infoType = InfoType.Default,
        site = new URL("https://kek.com"),
        creationDate = LocalDate.parse("2019-10-10"),
        updateDate = None,
        price = 100.0,
        comission = 0.0,
        isPersonal = true
      )

      FromFieldsT
        .to[Info]
        .mapping('type, 'infoType)
        .mapping('currentPrice, 'price)
        .fallback('nominalPrice, 'price)
        .constFallback('comission, 0.0)
        .mapping('is_personal, 'isPersonal)
        .constFallback('isPersonal, false)
        .from(rawInfo) ==> info.validNec

      val rawInvalidInfo = Map(
        "id"           -> Some("120"),
        "title"        -> Some("Название"),
        "type"         -> Some("undefined"),
        "site"         -> Some("aaaaa"),
        "creationDate" -> Some("onmonmon"),
        "updateDate"   -> None,
        "currentPrice" -> None,
        "nominalPrice" -> None,
        "is_personal"  -> Some("true")
      )

      FromFieldsT
        .to[Info]
        .mapping('type, 'infoType)
        .mapping('currentPrice, 'price)
        .fallback('nominalPrice, 'price)
        .constFallback('comission, 0.0)
        .mapping('is_personal, 'isPersonal)
        .constFallback('isPersonal, false)
        .from(rawInvalidInfo) ==> Invalid(
        NEC(
          "Can't find `infoType` in external",
          "`undefined` is not member of (Default, Custom) on `type -> infoType`",
          "Can't find `infoType` in fallback",
          "Can't find `infoType` in external fallback",
          "Can't find `site` in external",
          "Can't parse URL from `aaaaa`. Error: `no protocol: aaaaa` on `site`",
          "Can't find `site` in fallback",
          "Can't find `site` in external fallback",
          "Can't find `creationDate` in external",
          "Can't parse date from `onmonmon`. Error: `Text 'onmonmon' could not be parsed at index 0` on `creationDate`",
          "Can't find `creationDate` in fallback",
          "Can't find `creationDate` in external fallback",
          "Can't find `price` in external",
          "Field `currentPrice -> price` should be required",
          "Field `nominalPrice -> price` should be required on fallback[0] for `price`",
          "Can't find `price` in fallback",
          "Can't find `price` in external fallback"
        )
      )
    }

    "with const and pick none on option failing" - {
      case class Foo(a: String, b: Option[Long], c: String, d: Option[Int], e: Option[String])

      val fields = Map("c" -> "value".some, "d" -> "failed".some)

      FromFieldsT
        .to[Foo]
        .const('a, "foo")
        .const('b, 12L.some)
        .from(fields) ==> Foo("foo", 12L.some, "value", None, None).validNec
    }

    "with computed" - {
      case class Bar(a: Int, b: Long, c: Double, d: Option[String])

      val fields = Map("d" -> "value".some)

      FromFieldsT
        .to[Bar]
        .computed('a, 1.validNec)
        .computed('b, 2L.validNec)
        .computed('c, 3.0.validNec)
        .from(fields) ==> Bar(1, 2L, 3.0, "value".some).validNec

      FromFieldsT
        .to[Bar]
        .computed('a, "nooo".invalidNec)
        .computed('b, "aaaa".invalidNec)
        .computed('c, "why".invalidNec)
        .from(fields) ==> Invalid(
        NEC(
          "Error `nooo` on external for a",
          "No key `a` in fields",
          "Can't find `a` in fallback",
          "Can't find `a` in external fallback",
          "Error `aaaa` on external for b",
          "No key `b` in fields",
          "Can't find `b` in fallback",
          "Can't find `b` in external fallback",
          "Error `why` on external for c",
          "No key `c` in fields",
          "Can't find `c` in fallback",
          "Can't find `c` in external fallback"
        )
      )
    }

    "pick first not failed fallback" - {
      case class Foo(a: String, b: Int, d: Option[String])

      val fields = Map(
        "a"   -> "value".some,
        "d"   -> "d".some,
        "c"   -> None,
        "ccc" -> "notint".some,
        "bb"  -> "13".some,
        "bbb" -> "14".some
      )

      FromFieldsT
        .to[Foo]
        .fallback('c, 'b)
        .fallback('cc, 'b)
        .fallback('ccc, 'b)
        .fallback('bb, 'b)
        .fallback('bbb, 'b)
        .constFallback('b, 31)
        .from(fields) ==>
      Foo("value", 13, "d".some).validNec

      FromFieldsT
        .to[Foo]
        .fallback('c, 'b)
        .fallback('cc, 'b)
        .fallback('ccc, 'b)
        .from(fields) ==>
      Invalid(
        NEC(
          "Can't find `b` in external",
          "No key `b` in fields",
          "Field `c -> b` should be required on fallback[0] for `b`",
          "No key `cc -> b` in fields on fallback[1] for `b`",
          "Can't parse int from `notint` on `ccc -> b` on fallback[2] for `b`",
          "Can't find `b` in fallback",
          "Can't find `b` in external fallback"
        )
      )
    }

    "one more demo case" - {
      case class Foo(name: String, count: Int, description: Option[String], deleted: Int)

      val fields = Map(
        "name"            -> "scala".some,
        "firstCount"      -> None,
        "desc"            -> "meetup".some,
        "summary"         -> "i_am_python_dev".some,
        "deleted"         -> "JavaKekException: failed".some,
        "deleted_by_user" -> "null".some
      )

      FromFieldsT
        .to[Foo]
        .fallback('firstCount, 'count)
        .fallback('secondCount, 'count)
        .fallback('summary, 'count)
        .mapping('desc, 'description)
        .fallback('deleted_by_user, 'deleted)
        .constFallback('deleted, 0)
        .from(fields) ==>
      Invalid(
        NEC(
          "Can't find `count` in external",
          "No key `count` in fields",
          "Field `firstCount -> count` should be required on fallback[0] for `count`",
          "No key `secondCount -> count` in fields on fallback[1] for `count`",
          "Can't parse int from `i_am_python_dev` on `summary -> count` on fallback[2] for `count`",
          "Can't find `count` in fallback",
          "Can't find `count` in external fallback"
        )
      )
    }
  }
}
