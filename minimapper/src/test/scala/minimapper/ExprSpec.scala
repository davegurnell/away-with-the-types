package minimapper

import org.scalatest._
import org.scalacheck._
import scala.collection.immutable.ListMap

class ExprSpec extends FreeSpec with Matchers {
  import syntax._

  val baz = ListData(List(IntData(1), IntData(2), IntData(3)))
  val bar = IntData(123)
  val foo = ProductData(ListMap("bar" -> bar, "baz" -> baz))
  val top = ProductData(ListMap("foo" -> foo))

  "eval and evalAs" - {
    "access an int" in {
      val expr     = dataAt("foo", "bar")
      val actual   = expr.evalAs[Int](top)
      val expected = Right(123)

      actual should be(expected)
    }

    "access a list" in {
      val expr     = dataAt("foo", "baz")
      val actual   = expr.evalAs[List[Int]](top)
      val expected = Right(List(1, 2, 3))

      actual should be(expected)
    }

    "simple expression" in {
      val expr     = dataAt("foo", "bar") * 2
      val actual   = expr.evalAs[Int](top)
      val expected = Right(246)

      actual should be(expected)
    }

    "aggregate expression" in {
      val expr     = dataAt("foo", "baz").combineAll
      val actual   = expr.evalAs[Int](top)
      val expected = Right(6)

      actual should be(expected)
    }

    "complex expression" in {
      val expr     = dataAt("foo", "bar") > dataAt("foo", "baz").combineAll
      val actual   = expr.evalAs[Boolean](top)
      val expected = Right(true)

      actual should be(expected)
    }

    "bad accessor" in {
      val expr     = dataAt("foo", "bam")
      val actual   = expr.evalAs[List[Int]](top)
      val expected = Left("field not found: bam")

      actual should be(expected)
    }

    "bad data type" in {
      val expr     = dataAt("foo", "bar").combineAll
      val actual   = expr.evalAs[Int](top)
      val expected = Left("invalid list: IntData(123)")

      actual should be(expected)
    }

    "bad result type" in {
      val expr     = dataAt("foo", "baz").combineAll
      val actual   = expr.evalAs[String](top)
      val expected = Left("invalid string: IntData(6)")

      actual should be(expected)
    }
  }

}
