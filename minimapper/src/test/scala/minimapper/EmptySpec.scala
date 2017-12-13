package minimapper

import org.scalatest._
import scala.collection.immutable.{ListMap, ListSet}

class EmptySpec extends FreeSpec with Matchers {
  "Schema.empty should generate appropriate Data objects" - {
    "BooleanSchema" in {
      val schema = BooleanSchema
      schema.empty should be(BooleanData(false))
    }

    "IntSchema" in {
      val schema = IntSchema
      schema.empty should be(IntData(0))
    }

    "StringSchema" in {
      val schema = StringSchema
      schema.empty should be(StringData(""))
    }

    "ListSchema" in {
      val schema = ListSchema(IntSchema)
      schema.empty should be(ListData(Nil))
    }

    "OptionSchema" in {
      val schema = OptionSchema(IntSchema)
      schema.empty should be(NullData)
    }

    "DefaultSchema" in {
      val schema = DefaultSchema(IntSchema, IntData(123))
      schema.empty should be(IntData(123))
    }

    "ProductSchema" in {
      val schema = ProductSchema(ListMap(
        "foo" -> IntSchema,
        "bar" -> StringSchema,
        "baz" -> DefaultSchema(BooleanSchema, BooleanData(true))
      ))

      schema.empty should be(ProductData(ListMap(
        "foo" -> IntData(0),
        "bar" -> StringData(""),
        "baz" -> BooleanData(true)
      )))
    }

    "SumSchema" in {
      val schema = SumSchema(ListMap(
        "Foo" -> IntSchema,
        "Bar" -> StringSchema,
        "Baz" -> BooleanSchema
      ))

      schema.empty should be(SumData("Foo", IntData(0)))
    }
  }
}