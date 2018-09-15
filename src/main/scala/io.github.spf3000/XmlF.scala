import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import scalaz._
import Scalaz._

sealed trait XmlF[A]
object XmlF {
  case class ElemF[A](node: String, childs: IList[A]) extends XmlF[A]
  case class TextF[A](node: String)                   extends XmlF[A]

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
  //  type CoalgebraicGTransform[N[_], NodeX, F[_], G[_]]        = F[T] => G[N[T]]
  //top down transformation
  // XmlF[NodeX] => IList[State[Int,NodeX]]]
  val transformAlg: CoalgebraicGTransform[IList, NodeX, XmlF, State[Int, ?]] = {
    case x @ ElemF(node, childs) => {
      for {
        _   <- State.modify[Int](_ + openPos(node))
        pos <- State.get[Int]
        list = TagX(node, pos) :: tagSiblings(childs, pos)
        _ <- State.modify[Int](_ + lastPos(node) - closePos(node))
      } yield list

    }
    case t @ TextF(l) =>
      for {
        _ <- State.modify[Int](_ + lastPos(t.node))
        s <- State.get
      } yield (IList(TextX(l, s, s + lastPos(l))))
  }

  type IntState[A] = State[Int, A]

  def tagSibling(t: NodeX): IntState[NodeX] =
    for {
      _ <- State.modify[Int](_ + lastPos(t.node))
      s <- State.get
    } yield (TagX(t.node, s))

  private def tagSiblings(siblings: IList[NodeX], startPos: Int): IList[NodeX] = {
    siblings.traverse(tagSibling).run(startPos)._2
  }

  type Xml = Fix[XmlF]
  def text(v: String): Xml                = Fix(TextF(v))
  def elem(v: String, l: IList[Xml]): Xml = Fix(ElemF(v, l))


  sealed abstract class NodeX {
    val node: String; val start: Int; val last: Int
  }
  case class TagX(node: String, start: Int, close: Int, last: Int) extends NodeX
  object TagX {
    def apply(node: String): TagX =
      TagX(node, startPos(node), closePos(node), lastPos(node))
    def apply(node: String, i: Int): TagX =
      TagX(node, startPos(node) + i, closePos(node) + i, lastPos(node) + i)
  }

  case class TextX(node: String, start: Int, last: Int) extends NodeX
  object TextX {
    def apply(v: String): TextX = TextX(v, startPos(v), lastPos(v))
  }

private def startPos(s: String)        = 0
private def openPos(xml: String)       = xml.indexOf('>')
private def closePos(xml: String): Int = xml.indices.last - xml.reverse.indexOf('<')
private def lastPos(s: String)         = s.indices.lastOption.getOrElse(0)
}
