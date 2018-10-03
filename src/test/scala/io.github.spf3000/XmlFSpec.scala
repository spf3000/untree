import matryoshka._
import matryoshka.Corecursive._
import matryoshka.data._
import matryoshka.implicits._
import scalaz._
import Scalaz._
import XmlF._
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class XmlRecurseSpec extends Specification {

  //                  0         1         2         3         4
  //                  012345678901234567890123456789012345678901
  private val week = "<week><day>Mon</day><day>Tues</day></week>"
  private val mon  = "<day>Mon</day>"
  private val tue  = "<day>Tues</day>"

/**
  def transGana[M[_]: Monad, U, G[_]: Functor]
    (u: U)
    (k: DistributiveLaw[M, Base], f: CoalgebraicGTransform[M, U, G, Base])
    (implicit U: Recursive.Aux[U, G], BF: Functor[Base])
      : T =
    gana(u)(k, f <<< (U.project(_)))
 **/

//def someExpr[T](implicit T: Corecursive.Aux[T, Expr]): T =
//  Mul(Num[T](2).embed, Mul(Num[T](3).embed,
//      Num[T](4).embed).embed).embed
//
//import matryoshka.data.Mu

//someExpr[Mu[Expr]].cata(eval) // ⇒ 24

def someXml[T](implicit T: Corecursive.Aux[T, XmlF]): T =
   ElemF[T](week, IList(ElemF[T](mon, IList(TextF[T]("Mon").embed)).embed,
    ElemF[T](tue, IList(TextF[T]("Tues").embed)).embed)).embed

import matryoshka.data.Nu

someXml[Nu[XmlF]].transGana(transformAlg)

//    elem(tue, IList(text("Tues")))))


//      xml.transCataM(transformAlg)
//      .run(0) must beEqualTo(
//      IList(TagX("week", 6, 35, 41),
//            TagX("day", 11, 14, 19),
//            TextX("Mon", 11, 14),
//            TagX("day", 25, 29, 34),
//            TextX ("Tues", 25, 29)))
}

