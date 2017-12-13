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
    "single field"     in { top.get("foo")               should be(Right(foo)) }
    "two fields"       in { top.get("foo", "bar")        should be(Right(bar)) }
    "first index"      in { top.get("foo", "baz", 0)     should be(Right(IntData(1))) }
    "second index"     in { top.get("foo", "baz", 1)     should be(Right(IntData(2))) }
  }

  "get should handle invalid combinations of field names and indices" - {
    "bad first field"  in { top.get("bar", "foo")        should be(Left("field not found: bar")) }
    "bad second field" in { top.get("foo", "bam")        should be(Left("field not found: bam")) }
    "index too high"   in { top.get("foo", "baz", 3)     should be(Left("index not found: 3")) }
    "index too low"    in { top.get("foo", "baz", -1)    should be(Left("index not found: -1")) }
    "index for object" in { top.get(0)                   should be(Left("index not found: 0")) }
    "field for list"   in { top.get("foo", "baz", "boo") should be(Left("field not found: boo")) }
  }

  "getAs should handle valid types" - {
    "bar"   in { top.getAs[Int]("foo", "bar") should be(Right(123)) }
    "baz"   in { top.getAs[List[Int]]("foo", "baz") should be(Right(List(1, 2, 3))) }
    "baz 0" in { top.getAs[Int]("foo", "baz", 0) should be(Right(1)) }
  }

  "getAs should handle invalid types" - {
    "bar"   in { top.getAs[List[Int]]("foo", "bar")    should be(Left("invalid list: IntData(123)")) }
    "baz"   in { top.getAs[Int]("foo", "baz")          should be(Left("invalid int: ListData(List(IntData(1), IntData(2), IntData(3)))")) }
    "baz 0" in { top.getAs[List[Int]]("foo", "baz", 0) should be(Left("invalid list: IntData(1)")) }
  }
}
