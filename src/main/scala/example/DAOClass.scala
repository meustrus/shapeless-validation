package example

import cats.syntax.validated._
import cats.data.{NonEmptyList, ValidatedNel}
import cats.sequence._
import shapeless._

case class DAOClass(a: Int, b: Int)

object DAOClass {

  def generate: ValidatedNel[String, DAOClass] = {

    val hlist: ValidatedNel[String, Int] :: ValidatedNel[String, Int] :: HNil =
      1.validNel :: 2.validNel :: HNil

    // val hlistSequence: ValidatedNel[String, Int :: Int :: HNil] = hlist.sequence
    // val hlistSequence: ValidatedNel[String, Int :: Int :: HNil] = SequenceValidatedHlist(hlist)

    // hlistSequence.map(Generic[DAOClass].from)
    Generic[DAOClass].from(1 :: 2 :: HNil).validNel
  }

}
