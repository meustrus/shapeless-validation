package example

import cats.syntax.validated._
import cats.data.{NonEmptyList, ValidatedNel}
import cats.sequence._
import shapeless.Poly1
import shapeless.{::, =:!=, HList, HNil}
import shapeless.ops.hlist.Prepend

object SequenceValidatedHlist extends Poly1 {

    case class ValidationError(message: String)
    type ValidatedFormat[+V] = ValidatedNel[ValidationError, V]



    // type VN[+E, +V] = ValidatedNel[E, V]



    // trait Case[-In] {
    //     type Out
    //     def apply(c: In): Out
    // }

    // def apply[In](x: T)(implicit cse: Case[In]): cse.Out = cse(x)

    // implicit def caseNil[E]: Case[HNil.type] = new Case[HNil.type] {
    //     type Out = VN[E, HNil.type]
    //     def apply(hlist: HNil.type): VN[E, HNil.type] = HNil.validNel
    // }

    // implicit def caseMain[E, V, R <: HList, T <: HList](implicit se: Case[T]): Case[VN[E, V] :: T] = new Case[VN[E, V] :: T] {
    //     type Out = VN[E, implicitly[Prepend[se.out, T]].out]
    //     def apply(hlist: VN[E, V] :: T) = 
    //         hlist.head.fold(
    //             e1 => se(hlist.tail).fold(
    //                 e2 => (e1 ::: e2).invalid,
    //                 _ => e1.invalid
    //             ),
    //             v1 => se(hlist.tail).fold(
    //                 e2 => e2.invalid,
    //                 v2 => (v1 :: v2).validNel
    //             )
    //         )
    // }



    implicit def caseNil = at[HNil](_.validNel: ValidatedFormat[HNil])

    implicit def caseMain[H, T <: HList, R <: HList](implicit se: Case.Aux[T, ValidatedFormat[R]]) = at[ValidatedFormat[H] :: T] { hlist =>
        hlist.head.ap(SequenceValidatedHlist(hlist.tail).map(tail => (head: H) => head :: tail))
        // hlist.head.fold(
        //     e1 => se(hlist.tail).fold(
        //         e2 => (e1 ::: e2).invalid,
        //         _ => e1.invalid
        //     ),
        //     v1 => se(hlist.tail).fold(
        //         e2 => e2.invalid,
        //         v2 => (v1 :: v2).validNel
        //     )
        // )
    }



    // implicit def caseEnd[E, V <: HList]: Case[VN[E, V] :: HNil] =
    //     at[VN[E, V] :: HNil](_.head)

    // implicit def caseFold[E, V1 <: HList, V2, T <: HList](implicit p: Prepend[V1, V2 :: HNil]): Case[VN[E, V1] :: VN[E, V2] :: T] = {
    //     def getImplicit()(implicit caseImpl: Case[VN[E, p.Out] :: T]) = caseImpl
    //     at[VN[E, V1] :: VN[E, V2] :: T] { hlist =>
    //         getImplicit().apply(
    //             hlist.head.fold(
    //                 (e1: NonEmptyList[E]) => {
    //                     hlist.tail.head.fold(
    //                         e2 => (e1 ::: e2).invalid,
    //                         _ => e1.invalid
    //                     )
    //                 },
    //                 (v1: V1) => {
    //                     hlist.tail.head.fold(
    //                         e2 => e2.invalid,
    //                         v2 => p(v1, v2 :: HNil).validNel
    //                     )
    //                 }
    //             ) :: hlist.tail.tail
    //         )
    //     }
    // }

    // implicit def caseStart[E, V, T <: ::[_, _]](implicit se: Case[VN[E, V :: HNil] :: T], notAnHList: V =:!= HList): Case[VN[E, V] :: T] =
    //     at[VN[E, V] :: T](hlist => hlist.head.map(_ :: HNil) :: hlist.tail)

}

// object SequenceValidatedHlist {

//     type VN[+E, +V] = ValidatedNel[E, V]

//     trait Case[-In] {
//         type Out
//         def apply(c: In): Out
//     }

//     def apply[T](x: T)(implicit cse: Case[T]): cse.Out = cse(x)

//     implicit def caseNil[E, V]: Case[VN[E, V :: HNil] :: HNil] = new Case[VN[E, V :: HNil] :: HNil] {
//         type Out = VN[E, V :: HNil]
//         def apply(hlist: VN[E, V :: HNil] :: HNil): VN[E, V :: HNil] = hlist.head
//     }

//     implicit def caseFold[E, V1 <: HList, V2, T <: HList](implicit p: Prepend[V1, V2 :: HNil], se: Case[VN[E, p.Out] :: T]): Case[VN[E, V1] :: VN[E, V2] :: T] = new Case[VN[E, V1] :: VN[E, V2] :: T] {
//         type Out = se.Out
//         def apply(hlist: VN[E, V1] :: VN[E, V2] :: T): se.Out = se(
//             hlist.head.fold(
//                 (e1: NonEmptyList[E]) => {
//                     hlist.tail.head.fold(
//                         e2 => (e1 ::: e2).invalid,
//                         _ => e1.invalid
//                     )
//                 },
//                 (v1: V1) => {
//                     hlist.tail.head.fold(
//                         e2 => e2.invalid,
//                         v2 => p(v1, v2 :: HNil).validNel
//                     )
//                 }
//             ) :: hlist.tail.tail
//         )
//     }

//     implicit def caseStart[E, V, T <: HList](implicit se: Case[VN[E, V :: HNil] :: T]): Case[VN[E, V] :: T] = new Case[VN[E, V] :: T] {
//         type Out = se.Out
//         def apply(hlist: VN[E, V] :: T): se.Out = se(hlist.head.map(_ :: HNil) :: hlist.tail)
//     }

// }

object size extends Poly1 {
  implicit def caseInt = at[Int](x => 1)
  implicit def caseString = at[String](_.length)
  implicit def caseTuple[T, U]
    (implicit st : Case.Aux[T, Int], su : Case.Aux[U, Int]) =
      at[(T, U)](t => size(t._1)+size(t._2))
}
