import Aliases._
import FromFieldsT._

import cats.implicits._
import shapeless.{HList, LabelledGeneric => LGen, Witness => W}
import shapeless.ops.record.Selector
import shapeless.labelled.{field, FieldType}

final class FromFieldsT[A] private (settings: Settings) {
  def mapping[To <: Symbol, R <: HList](
    from: Symbol,
    to: W.Lt[To]
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: Selector[R, To]
  ): FromFieldsT[A] = new FromFieldsT[A](settings.copy(mapping = settings.mapping + (to.value -> from)))

  def const[To <: Symbol, R <: HList, C](
    to: W.Lt[To],
    const: C
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: Selector.Aux[R, To, C]
  ): FromFieldsT[A] = new FromFieldsT[A](settings.copy(external = settings.external + (to.value -> const.validNec)))

  def computed[To <: Symbol, R <: HList, C](
    to: W.Lt[To],
    value: V[C]
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: Selector.Aux[R, To, C]
  ): FromFieldsT[A] =
    new FromFieldsT[A](settings.copy(external = settings.external + (to.value -> value)))

  def fallback[To <: Symbol, R <: HList](
    from: Symbol,
    to: W.Lt[To]
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: Selector[R, To]
  ): FromFieldsT[A] =
    new FromFieldsT[A](
      settings.copy(fallbacks = settings.fallbacks.updated(to.value, settings.fallbacks(to.value) :+ from))
    )

  def constFallback[To <: Symbol, R <: HList, C](
    to: W.Lt[To],
    const: C
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: Selector.Aux[R, To, C]
  ): FromFieldsT[A] =
    new FromFieldsT[A](
      settings.copy(
        externalFallbacks =
          settings.externalFallbacks.updated(to.value, settings.externalFallbacks(to.value) :+ const.validNec)
      )
    )

  def computedFallback[To <: Symbol, R <: HList, C](
    to: W.Lt[To],
    value: V[C]
  )(
    implicit
    gen: LGen.Aux[A, R],
    sel: Selector.Aux[R, To, C]
  ): FromFieldsT[A] =
    new FromFieldsT[A](
      settings.copy(
        externalFallbacks = settings.externalFallbacks.updated(to.value, settings.externalFallbacks(to.value) :+ value)
      )
    )

  def from[R <: HList](fields: Fields)(implicit gen: LGen.Aux[A, R], fromFields: FromFields[R]): V[A] =
    fromFields(fields, settings).map(gen.from)
}

object FromFieldsT {
  type Mapping           = Map[Symbol, Symbol]
  type Fallbacks         = Map[Symbol, List[Symbol]]
  type External          = Map[Symbol, V[Any]]
  type ExternalFallbacks = Map[Symbol, List[V[Any]]]

  case class Settings(
    mapping: Mapping = Map.empty,
    external: External = Map.empty,
    fallbacks: Fallbacks = Map.empty.withDefaultValue(Nil),
    externalFallbacks: ExternalFallbacks = Map.empty.withDefaultValue(Nil)
  )

  def to[A]: FromFieldsT[A] = new FromFieldsT[A](Settings())

  trait FromFields[T <: HList] {
    def apply(fields: Fields, settings: Settings): V[T]
  }

  object FromFields {
    private def findFromFields[Value](
      fields: Fields,
      key: Symbol,
      alias: Symbol
    )(
      implicit
      fromString: FromString[Value]
    ): V[Value] = {
      val path =
        if (alias == key)
          alias.name
        else s"${alias.name} -> ${key.name}"

      fields.get(alias.name) match {
        case Some(Some(value)) => withPostfix(fromString(value), s"on `$path`")
        case Some(None)        => s"Field `$path` should be required".invalidNec
        case None              => s"No key `$path` in fields".invalidNec
      }
    }

    private def withPostfix[A](v: V[A], postfix: String): V[A] =
      v.leftMap(_.map(_ + s" $postfix"))

    private def wrapError[A](v: V[A]): V[A] =
      v.leftMap(_.map(err => s"Error `$err`"))

    private def findFromExternal[Value](external: External, key: Symbol): V[Value] =
      external
        .get(key)
        .fold[V[Value]](s"Can't find `${key.name}` in external".invalidNec)(
          v => withPostfix(wrapError(v.asInstanceOf[V[Value]]), s"on external for ${key.name}")
        )

    private def findFromFallback[Value: FromString](
      fields: Fields,
      key: Symbol,
      fallback: List[Symbol],
      idx: Int = 0
    ): V[Value] =
      fallback match {
        case alias :: tail =>
          withPostfix(findFromFields(fields, key, alias), s"on fallback[$idx] for `${key.name}`")
            .findValid(findFromFallback(fields, key, tail, idx + 1))
        case _ => s"Can't find `${key.name}` in fallback".invalidNec
      }

    private def findFromExternalFallback[Value](key: Symbol, fallback: List[V[Any]], idx: Int = 0): V[Value] =
      fallback match {
        case head :: tail =>
          withPostfix(wrapError(head.asInstanceOf[V[Value]]), s"on external fallback[$idx] for `${key.name}`")
            .findValid(findFromExternalFallback(key, tail, idx + 1))
        case Nil => s"Can't find `${key.name}` in external fallback".invalidNec
      }

    private def find[Value: FromString](fields: Fields, key: Symbol, settings: Settings): V[Value] = {
      import settings._

      findFromExternal(external, key)
        .findValid(findFromFields(fields, key, mapping.getOrElse(key, key)))
        .findValid(findFromFallback(fields, key, fallbacks(key)))
        .findValid(findFromExternalFallback(key, externalFallbacks(key)))
    }

    private def findOpt[Value: FromString](fields: Fields, key: Symbol, settings: Settings): Option[Value] = {
      import settings._

      findFromExternal(external, key)
        .findValid(findFromFields[Value](fields, key, mapping.getOrElse(key, key)).map(_.some))
        .findValid(findFromFallback[Value](fields, key, fallbacks(key)).map(_.some))
        .findValid(findFromExternalFallback(key, externalFallbacks(key)))
        .toOption
        .flatten
    }

    import shapeless.{::, HNil}

    implicit val empty: FromFields[HNil] = (_, _) => HNil.validNec

    implicit def requiredHead[Key <: Symbol, Value, Tail <: HList](
      implicit
      w: W.Aux[Key],
      fromString: FromString[Value],
      tail: FromFields[Tail]
    ): FromFields[FieldType[Key, Value] :: Tail] =
      (fields, settings) => (find(fields, w.value, settings), tail(fields, settings)).mapN(field[Key](_) :: _)

    implicit def optionalHead[Key <: Symbol, Value, Tail <: HList](
      implicit
      w: W.Aux[Key],
      fromString: FromString[Value],
      tail: FromFields[Tail]
    ): FromFields[FieldType[Key, Option[Value]] :: Tail] =
      (fields, settings) =>
        (findOpt(fields, w.value, settings).validNec, tail(fields, settings)).mapN(field[Key](_) :: _)
  }
}
