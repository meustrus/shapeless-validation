package example

import cats.syntax.validated._
import cats.data.{NonEmptyList, ValidatedNel}
import cats.sequence._
import org.scalatest.{FunSuite, Matchers}
import shapeless._

class SequenceValidatedHlistTest extends FunSuite with Matchers {

    import example.SequenceValidatedHlist._

    test("should map empty hlist") {
        example.SequenceValidatedHlist(HNil: HNil) shouldBe HNil.validNel
        // caseNil(HNil) shouldBe HNil.validNel
        // example.SequenceValidatedHlist(1.validNel :: HNil) shouldBe (1 :: HNil).validNel
        // caseNil[Nothing, Int]((1 :: HNil).validNel :: HNil) shouldBe (1 :: HNil).validNel
    }

    test("should map single valid value") {
        example.SequenceValidatedHlist((1.validNel: ValidatedFormat[Int]) :: HNil) shouldBe (1 :: HNil).validNel
        // caseMain[Int, HNil, HNil](caseNil)((1.validNel) :: HNil) shouldBe (1 :: HNil).validNel
        // caseFirst.apply(1.validNel :: HNil) shouldBe (1 :: HNil).validNel
        // caseOne[Nothing, Int, ValidatedNel[Nothing, Int :: HNil], HNil](implicitly)(1.validNel :: HNil) shouldBe (1 :: HNil).validNel
    }

    // test("should map two valid hlists") {
        // example.SequenceValidatedHlist(
        //     (1.validNel: ValidatedFormat[Int]) ::
        //     (2.validNel: ValidatedFormat[Int]) ::
        //     HNil
        // ) shouldBe (1 :: 2 :: HNil).validNel
        // caseMain[Int, Int :: HNil, ValidatedFormat[Int] :: HNil](
        //     caseMain[Int, HNil, HNil](
        //         caseNil
        //     )
        // )(
        //     (1.validNel: ValidatedFormat[Int]) ::
        //     (2.validNel: ValidatedFormat[Int]) ::
        //     HNil
        // ) shouldBe (1 :: 2 :: HNil).validNel
    // }
//     (cats.data.ValidatedNel[Nothing,Int] :: HNil) :: HNil
// cse: shapeless.poly.Case[SequenceValidatedHlist.type,shapeless.::[shapeless.::[cats.data.ValidatedNel[Nothing,Int],shapeless.HNil],shapeless.HNil]]
    test("examples") {
        example.size(23) shouldBe 1
        example.size("foo") shouldBe 3
        example.size((22, "foo")) shouldBe 4
        example.size(((23, "foo"), 13)) shouldBe 5
    }

}
