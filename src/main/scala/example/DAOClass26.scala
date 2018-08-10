package example

import cats.syntax.validated._
import cats.data.{NonEmptyList, ValidatedNel}
import cats.sequence._
import shapeless._

case class DAOClass26(
  a: Int,
  b: String,
  c: Int,
  d: String,
  e: Int,
  f: String,
  g: Int,
  h: String,
  i: Int,
  j: String,
  k: Int,
  l: String,
  m: Int,
  n: String,
  o: Int,
  p: String,
  q: Int,
  r: String,
  s: Int,
  t: String,
  u: Int,
  v: String,
  w: Int,
  x: String,
  y: Int,
  z: String
)

object DAOClass26 {

  def generate: ValidatedNel[String, DAOClass26] =
    exampleData.sequence.map(Generic[DAOClass26].from)

  private def exampleData =
    1.validNel ::
    "2".validNel ::
    3.validNel ::
    "4".validNel ::
    5.validNel ::
    "6".validNel ::
    7.validNel ::
    "8".validNel ::
    9.validNel ::
    "10".validNel ::
    11.validNel ::
    "12".validNel ::
    13.validNel ::
    "14".validNel ::
    15.validNel ::
    "16".validNel ::
    17.validNel ::
    "18".validNel ::
    19.validNel ::
    "20".validNel ::
    21.validNel ::
    "22".validNel ::
    23.validNel ::
    "24".validNel ::
    25.validNel ::
    "26".validNel ::
    HNil

}
