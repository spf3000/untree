import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import scalaz._
import Scalaz._
import XmlF._

class XmlRecurseSpec extends Specification {

  //                  0         1         2         3         4
  //                  012345678901234567890123456789012345678901
  private val week = "<week><day>Mon</day><day>Tues</day></week>"
  private val mon  = "<day>Mon</day>"
  private val tue  = "<day>Tues</day>"

  private val x =
    elem(week, IList(elem(mon, IList(text("Mon"))), elem(tue, IList(text("Tues")))))
      .transGana[IList, State[Int, ?]](transformAlg)
      .run(0) must beEqualTo(
      IList(TagX("week", 6, 35, 41),
            TagX("day", 11, 14, 19),
            TextX("Mon", 11, 14),
            TagX("day", 25, 29, 34)
              TextX ("Tues", 25, 29)))
}
