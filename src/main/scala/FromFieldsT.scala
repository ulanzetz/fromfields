import Aliases._
import FromFieldsT._
import cats.implicits._
import shapeless.{::, HList, HNil, Lazy, LabelledGeneric => LGen, Witness => W}
import shapeless.labelled.{field, FieldType}
import shapeless.ops

import scala.collection.immutable.{:: => next}

final class FromFieldsT[A, CK <: HList] private (settings: Settings) {
  def mapping[To <: Symbol, R <: HList](
    from: Symbol,
    to: W.Lt[To]
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: ops.record.Selector[R, To]
  ): FromFieldsT[A, CK] = new FromFieldsT(settings.copy(mapping = settings.mapping + (to.value.name -> from.name)))

  def const[To <: Symbol, R <: HList, C](
    to: W.Lt[To],
    const: C
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: ops.record.Selector.Aux[R, To, C]
  ): FromFieldsT[A, To :: CK] =
    new FromFieldsT(settings.copy(external = settings.external + (to.value.name -> const.validNec)))

  def computed[To <: Symbol, R <: HList, C](
    to: W.Lt[To],
    value: V[C]
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: ops.record.Selector.Aux[R, To, C]
  ): FromFieldsT[A, To :: CK] =
    new FromFieldsT(settings.copy(external = settings.external + (to.value.name -> value)))

  def fallback[To <: Symbol, R <: HList](
    from: Symbol,
    to: W.Lt[To]
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: ops.record.Selector[R, To]
  ): FromFieldsT[A, CK] =
    new FromFieldsT(
      settings
        .copy(fallbacks = settings.fallbacks.updated(to.value.name, settings.fallbacks(to.value.name) :+ from.name))
    )

  def constFallback[To <: Symbol, R <: HList, C](
    to: W.Lt[To],
    const: C
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: ops.record.Selector.Aux[R, To, C]
  ): FromFieldsT[A, CK] =
    new FromFieldsT(
      settings.copy(
        externalFallbacks =
          settings.externalFallbacks.updated(to.value.name, settings.externalFallbacks(to.value.name) :+ const.validNec)
      )
    )

  def computedFallback[To <: Symbol, R <: HList, C](
    to: W.Lt[To],
    value: V[C]
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: ops.record.Selector.Aux[R, To, C]
  ): FromFieldsT[A, CK] =
    new FromFieldsT(
      settings.copy(
        externalFallbacks =
          settings.externalFallbacks.updated(to.value.name, settings.externalFallbacks(to.value.name) :+ value)
      )
    )

  def from[R <: HList](
    fields: Fields,
    prefix: String = ""
  )(
    implicit
    gen: LGen.Aux[A, R],
    fromFields: FromFields[R, CK]
  ): V[A] =
    fromFields(fields, settings, prefix).map(gen.from)
}

object FromFieldsT {
  type Mapping           = Map[String, String]
  type Fallbacks         = Map[String, List[String]]
  type External          = Map[String, V[Any]]
  type ExternalFallbacks = Map[String, List[V[Any]]]

  case class Settings(
    mapping: Mapping = Map.empty,
    external: External = Map.empty,
    fallbacks: Fallbacks = Map.empty.withDefaultValue(Nil),
    externalFallbacks: ExternalFallbacks = Map.empty.withDefaultValue(Nil)
  )

  def to[A]: FromFieldsT[A, HNil] = new FromFieldsT(Settings())

  trait FromFields[T <: HList, CK <: HList] {
    def apply(fields: Fields, settings: Settings, prefix: String): V[T]
  }

  object FromFields extends CommonFromFields {
    implicit def empty[CK <: HList]: FromFields[HNil, CK] = (_, _, _) => HNil.validNec

    implicit def externalHead[Key <: Symbol, Value, Tail <: HList, CK <: HList](
      implicit
      w: W.Aux[Key],
      sel: ops.hlist.Selector[CK, Key],
      tail: Lazy[FromFields[Tail, CK]]
    ): FromFields[FieldType[Key, Value] :: Tail, CK] =
      (fields, settings, prefix) => {
        val key = mkKey(prefix, w.value.name)
        (
          withPostfix(wrapError(settings.external(key).map(_.asInstanceOf[Value])), s"on external for $key"),
          tail.value(fields, settings, prefix)
        ).mapN(field[Key](_) :: _)
      }
  }

  trait CommonFromFields extends DeepFromFields {
    private def findFromFields[Value](
      fields: Fields,
      key: String,
      alias: String
    )(
      implicit
      fromString: FromString[Value]
    ): V[Value] = {
      val path =
        if (alias == key)
          alias
        else s"$alias -> $key"

      fields.get(alias) match {
        case Some(Some(value)) => withPostfix(fromString(value), s"on `$path`")
        case Some(None)        => s"Field `$path` should be required".invalidNec
        case None              => s"No key `$path` in fields".invalidNec
      }
    }

    protected def withPostfix[A](v: V[A], postfix: String): V[A] =
      v.leftMap(_.map(_ + s" $postfix"))

    protected def wrapError[A](v: V[A]): V[A] =
      v.leftMap(_.map(err => s"Error `$err`"))

    private def findFromFallback[Value: FromString](
      fields: Fields,
      key: String,
      fallback: List[String],
      idx: Int = 0
    ): V[Value] =
      fallback match {
        case alias next tail =>
          withPostfix(findFromFields(fields, key, alias), s"on fallback[$idx] for `$key`")
            .findValid(findFromFallback(fields, key, tail, idx + 1))
        case _ => s"Can't find `$key` in fallback".invalidNec
      }

    private def findFromExternalFallback[Value](key: String, fallback: List[V[Any]], idx: Int = 0): V[Value] =
      fallback match {
        case head next tail =>
          withPostfix(wrapError(head.asInstanceOf[V[Value]]), s"on external fallback[$idx] for `$key`")
            .findValid(findFromExternalFallback(key, tail, idx + 1))
        case Nil => s"Can't find `$key` in external fallback".invalidNec
      }

    private def find[Value: FromString](fields: Fields, key: String, settings: Settings): V[Value] = {
      import settings._

      findFromFields(fields, key, mapping.getOrElse(key, key))
        .findValid(findFromFallback(fields, key, fallbacks(key)))
        .findValid(findFromExternalFallback(key, externalFallbacks(key)))
    }

    private def findOpt[Value: FromString](fields: Fields, key: String, settings: Settings): Option[Value] = {
      import settings._

      findFromFields[Value](fields, key, mapping.getOrElse(key, key))
        .map(_.some)
        .findValid(findFromFallback[Value](fields, key, fallbacks(key)).map(_.some))
        .findValid(findFromExternalFallback(key, externalFallbacks(key)))
        .toOption
        .flatten
    }

    implicit def requiredHead[Key <: Symbol, Value, Tail <: HList, CK <: HList](
      implicit
      w: W.Aux[Key],
      fromString: FromString[Value],
      tail: FromFields[Tail, CK]
    ): FromFields[FieldType[Key, Value] :: Tail, CK] =
      (fields, settings, prefix) =>
        (find(fields, mkKey(prefix, w.value.name), settings), tail(fields, settings, prefix)).mapN(field[Key](_) :: _)

    implicit def optionalHead[Key <: Symbol, Value, Tail <: HList, CK <: HList](
      implicit
      w: W.Aux[Key],
      fromString: FromString[Value],
      tail: FromFields[Tail, CK]
    ): FromFields[FieldType[Key, Option[Value]] :: Tail, CK] =
      (fields, settings, prefix) =>
        (findOpt(fields, mkKey(prefix, w.value.name), settings).validNec, tail(fields, settings, prefix))
          .mapN(field[Key](_) :: _)
  }

  trait DeepFromFields {
    implicit def deepRequired[Key <: Symbol, Value, ValueRepr <: HList, Tail <: HList, CK <: HList](
      implicit
      w: W.Aux[Key],
      lgen: LGen.Aux[Value, ValueRepr],
      head: FromFields[ValueRepr, CK],
      tail: Lazy[FromFields[Tail, CK]]
    ): FromFields[FieldType[Key, Value] :: Tail, CK] =
      (fields, settings, prefix) =>
        (head(fields, settings, mkKey(prefix, w.value.name)).map(lgen.from), tail.value(fields, settings, prefix))
          .mapN(field[Key](_) :: _)

    implicit def deepOpt[Key <: Symbol, Value, ValueRepr <: HList, Tail <: HList, CK <: HList](
      implicit
      w: W.Aux[Key],
      lgen: LGen.Aux[Value, ValueRepr],
      head: FromFields[ValueRepr, CK],
      tail: Lazy[FromFields[Tail, CK]]
    ): FromFields[FieldType[Key, Option[Value]] :: Tail, CK] =
      (fields, settings, prefix) =>
        tail
          .value(fields, settings, prefix)
          .map(field[Key](head(fields, settings, mkKey(prefix, w.value.name)).map(lgen.from).toOption) :: _)

    protected def mkKey(prefix: String, postfix: String): String =
      if (prefix.isEmpty) postfix else s"$prefix.$postfix"
  }
}
