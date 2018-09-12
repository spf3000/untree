import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import scalaz._
import Scalaz._

sealed trait XmlF[A]
object XmlF {
  case class ElemF[A](node: String, childs: IList[A]) extends XmlF[A]
  case class TextF[A](node: String)                  extends XmlF[A]

  implicit val traverse = new Traverse[XmlF] {
    def traverseImpl[G[_], A, B](fa: XmlF[A])(f: A => G[B])(implicit G: Applicative[G]) =
      fa match {
        case ElemF(l, a) => a.traverse(f) map (x => ElemF(l, x))
        case TextF(v)    => G.point(TextF[B](v))
      }
  }

  implicit val functor: Functor[XmlF]   = traverse
  implicit val foldable: Foldable[XmlF] = traverse

  /** Transform a structure `F` to a structure `G`, in top-down fashion,
   * accumulating effects in the monad `M`.
   * @group algebras
   */

  //  type CoalgebraicGTransform[N[_], XNode, F[_], G[_]]        = F[T] => G[N[T]]
  //top down transformation
  // XmlF[XNode] => IList[State[Int,XNode]]]
  val transformAlg: CoalgebraicGTransform[IList, XNode, XmlF, State[Int,?]] = {
    case x @ ElemF(l,c) =>
      for {
        _ <- State.modify[Int](_ + openPos(l))
        s <- State.get
      } yield (Tag(l,s) :: tagSiblings(c,s))
    case t @ TextF(l) =>
      for {
        _ <- State.modify[Int](_ + lastPos(t.node))
        s <- State.get
      } yield(IList(Text(l, s, s + lastPos(l))))
  }

type IntState[A] = State[Int,A]

def tagSibling(t: XNode): IntState[XNode] =
  for {
    _ <- State.modify[Int](_ + lastPos(t.node))
    s <- State.get
  } yield (Tag(t.node,s))

  private def tagSiblings(siblings: IList[XNode], startPos: Int): IList[XNode]  =  {
     siblings.traverse(tagSibling).run(startPos)._2
  }

  type Xml = Fix[XmlF]
  def text(v: String): Xml = Fix(TextF(v))
  def elem(v: String, l: IList[Xml]): Xml = Fix(ElemF(v,l))


  sealed abstract class XNode {val node: String; val start: Int; val last: Int}
  case class Tag(node: String, start: Int, close: Int, last: Int) extends XNode
  object Tag {
    def apply(node: String): Tag = Tag(node, startPos(node), closePos(node), lastPos(node))
    def apply(node: String, i: Int): Tag = Tag(node, startPos(node) + i, closePos(node) + i, lastPos(node) + i)
  }
  case class Text(node: String, start: Int, last: Int) extends XNode
  object Text {
    def apply(v: String): Text = Text(v, startPos(v), lastPos(v))
  }

  def startPos(s: String) = 0
  def openPos(xml: String) = xml.indexOf('>')
  def closePos(xml: String): Int = xml.indices.last - xml.reverse.indexOf('<')
  def lastPos(s: String) = s.indices.lastOption.getOrElse(0)
}

