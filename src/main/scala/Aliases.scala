object Aliases {
  type NEC[+A] = cats.data.NonEmptyChain[A]
  val NEC = cats.data.NonEmptyChain

  type V[+A] = cats.data.Validated[NEC[String], A]

  type Fields = Map[String, Option[String]]
}
