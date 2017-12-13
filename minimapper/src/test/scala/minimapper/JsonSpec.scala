package minimapper

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import java.time.ZonedDateTime
import org.scalacheck._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class JsonSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  import syntax._
  import JsonCodecs._
  import Generators._

  "Simple Scala values can be serialized to JSON directly or via Data objects" - {
    def check[A](implicit arbitrary: Arbitrary[A], toData: ToData[A], encoder: Encoder[A]) =
      forAll { (value: A) =>
        val valueJson = value.asJson
        val data      = value.toData
        val dataJson  = data.asJson
        dataJson should be(valueJson)
      }

    "Boolean"       in { check[Boolean]       }
    "Int"           in { check[Int]           }
    "Double"        in { check[Double]        }
    "String"        in { check[String]        }
    "ZonedDateTime" in { check[ZonedDateTime] }
    "List[Int]"     in { check[List[Int]]     }
    "Option[Int]"   in { check[Option[Int]]   }
    "Data"          in { check[Data]          }
    "Location"      in { check[Location]      }
    "Turbidity"     in { check[Turbidity]     }
    "WaterQuality"  in { check[WaterQuality]  }
  }

  "Data specified by a Schema can be round-trip serialized/deserialized" - {
    def check[A](schema: Schema)(implicit arbitrary: Arbitrary[A], toData: ToData[A], encoder: Encoder[A]) =
      forAll(genDataFromSchema(schema)) { expected =>
        val json = expected.asJson
        val copy = json.as(dataDecoder(schema))
        copy should be(Right(expected))
      }

    "Boolean"       in { check[Boolean](BooleanSchema)                     }
    "Int"           in { check[Int](IntSchema)                             }
    "Double"        in { check[Double](DoubleSchema)                       }
    "String"        in { check[String](StringSchema)                       }
    "ZonedDateTime" in { check[ZonedDateTime](TimestampSchema)             }
    "List[Int]"     in { check[List[Int]](ListSchema(IntSchema))           }
    "Option[Int]"   in { check[Option[String]](OptionSchema(StringSchema)) }
    "Location"      in { check[Location](Location.schema)                   }
    "Turbidity"     in { check[Turbidity](Turbidity.schema)                 }
    "WaterQuality"  in { check[WaterQuality](WaterQuality.schema)           }
  }

}
