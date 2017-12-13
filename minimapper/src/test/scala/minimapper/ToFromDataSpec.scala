package minimapper

import cats.data.Validated
import java.time.ZonedDateTime
import org.scalacheck._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ToFromDataSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  import JavaTimeGenerators._
  import ScalaFixtures._
  import SchemaFixtures._

  "Check equivalence between Scala ADTs and schema instances" - {
    "Boolean"                  in { check[Boolean](BooleanSchema) }
    "Int"                      in { check[Int](IntSchema) }
    "Double"                   in { check[Double](DoubleSchema) }
    "String"                   in { check[String](StringSchema) }
    "ZonedDateTime"            in { check[ZonedDateTime](TimestampSchema) }
    "List[Int]"                in { check[List[Int]](ListSchema(IntSchema)) }
    "Option[String]"           in { check[Option[String]](OptionSchema(StringSchema)) }
    "Location"                 in { check[Location](LocationSchema) }
    "Turbidity"                in { check[Turbidity](TurbiditySchema) }
    "WaterQuality"             in { check[WaterQuality](WaterQualitySchema) }
  }

  def check[A](schema: Schema)(implicit toData: ToData[A], fromData: FromData[A], arb: Arbitrary[A]) = {
    forAll { (value: A) =>
      val data = toData(value)
      val copy = fromData(data)
      schema.typeCheck(data) should be(Nil)
      copy should be(Validated.valid(value))
    }

    forAll(genDataFromSchema(schema)) { (data: Data) =>
      val Validated.Valid(value) = fromData(data)
      val copy = toData(value)
      schema.typeCheck(copy) should be(Nil)
    }
  }
}
