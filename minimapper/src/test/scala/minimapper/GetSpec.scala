package minimapper

import cats.data.Validated
import org.scalatest._
import scala.collection.immutable.ListMap

class GetSpec extends FreeSpec with Matchers {
  val baz = ListData(List(IntData(1), IntData(2), IntData(3)))
  val bar = IntData(123)
  val foo = ProductData(ListMap("bar" -> bar, "baz" -> baz))
  val top = ProductData(ListMap("foo" -> foo))

  "get should accept valid combinations of field names and indices" - {
    "single field"     in { top.get("foo")               should be(Validated.valid(foo)) }
    "two fields"       in { top.get("foo", "bar")        should be(Validated.valid(bar)) }
  }

  "get should handle invalid combinations of field names and indices" - {
    "bad first field"  in { top.get("bar", "foo")        should be(Validated.invalid(List("field not found: bar"))) }
    "bad second field" in { top.get("foo", "bam")        should be(Validated.invalid(List("field not found: bam"))) }
    "field for list"   in { top.get("foo", "baz", "boo") should be(Validated.invalid(List("field not found: boo"))) }
  }

  "getAs should handle valid types" - {
    "bar"   in { top.getAs[Int]("foo", "bar") should be(Validated.valid(123)) }
    "baz"   in { top.getAs[List[Int]]("foo", "baz") should be(Validated.valid(List(1, 2, 3))) }
  }

  "getAs should handle invalid types" - {
    "bar"   in { top.getAs[List[Int]]("foo", "bar")    should be(Validated.invalid(List("invalid list: IntData(123)"))) }
    "baz"   in { top.getAs[Int]("foo", "baz")          should be(Validated.invalid(List("invalid int: ListData(List(IntData(1), IntData(2), IntData(3)))"))) }
  }
}
