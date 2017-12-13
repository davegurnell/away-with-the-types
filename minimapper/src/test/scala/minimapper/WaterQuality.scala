package minimapper

import cats.implicits._
import java.time.ZonedDateTime
import minimapper.syntax._
import minimapper.Generators._
import org.scalacheck._
import scala.collection.immutable.ListMap

case class Location(lat: Double, lng: Double)

object Location {
  val schema = ProductSchema(ListMap(
    "lat" -> DoubleSchema,
    "lng" -> DoubleSchema
  ))

  implicit val locationToData: ToData[Location] =
    ToData.instance { loc =>
      ProductData(ListMap(
        "lat" -> loc.lat.toData,
        "lng" -> loc.lng.toData
      ))
    }

  implicit val locationFromData: FromData[Location] =
    FromData.instance { data =>
      (
        data.getAs[Double]("lat"),
        data.getAs[Double]("lng"),
      ).mapN(Location.apply)
    }

  implicit val arbitraryLocation: Arbitrary[Location] =
    Arbitrary {
      for {
        lat <- Arbitrary.arbitrary[Double]
        lng <- Arbitrary.arbitrary[Double]
      } yield Location(lat, lng)
    }
}

sealed trait Turbidity
case class QualitativeTurbidity(description: String) extends Turbidity
case class QuantitativeTurbidity(value: Int) extends Turbidity

object Turbidity {
  val schema = SumSchema(ListMap(
    "QualitativeTurbidity"  -> ProductSchema(ListMap(
      "description" -> StringSchema
    )),
    "QuantitativeTurbidity" -> ProductSchema(ListMap(
      "value" -> IntSchema
    ))
  ))

  implicit val turidityToData: ToData[Turbidity] =
    ToData.instance {
      case QualitativeTurbidity(description) =>
        SumData("QualitativeTurbidity", ProductData(ListMap(
          "description" -> description.toData
        )))
      case QuantitativeTurbidity(value) =>
        SumData("QuantitativeTurbidity", ProductData(ListMap(
          "value" -> value.toData
        )))
    }

  implicit val turbidityFromData: FromData[Turbidity] =
    FromData.instance { data =>
      data.typeName.flatMap {
        case "QualitativeTurbidity" =>
          data.getAs[String]("description").map(QualitativeTurbidity.apply)

        case "QuantitativeTurbidity" =>
          data.getAs[Int]("value").map(QuantitativeTurbidity.apply)
      }
    }

  implicit val arbitraryTurbidity: Arbitrary[Turbidity] =
    Arbitrary {
      Arbitrary.arbitrary[Boolean].flatMap {
       case true  => Gen.chooseNum(0, 240).map(QuantitativeTurbidity.apply)
       case false => Gen.alphaStr.map(QualitativeTurbidity.apply)
      }
    }
}

case class WaterQuality(
  location    : Location,
  timestamp   : ZonedDateTime,
  river       : String,
  temperature : Double,
  ph          : Option[Double],
  turbidity   : Turbidity
)

object WaterQuality {
  val schema = ProductSchema(ListMap(
    "location"    -> Location.schema,
    "timestamp"   -> TimestampSchema,
    "river"       -> StringSchema,
    "temperature" -> DoubleSchema,
    "ph"          -> OptionSchema(DoubleSchema),
    "turbidity"   -> Turbidity.schema
  ))

  implicit val waterQualityToData: ToData[WaterQuality] =
    ToData.instance { wq =>
      ProductData(ListMap(
        "location"    -> wq.location.toData,
        "timestamp"   -> wq.timestamp.toData,
        "river"       -> wq.river.toData,
        "temperature" -> wq.temperature.toData,
        "ph"          -> wq.ph.toData,
        "turbidity"   -> wq.turbidity.toData
      ))
    }

  implicit val waterQualityFromData: FromData[WaterQuality] =
    FromData.instance { data =>
      (
        data.getAs[Location]("location"),
        data.getAs[ZonedDateTime]("timestamp"),
        data.getAs[String]("river"),
        data.getAs[Double]("temperature"),
        data.getAs[Option[Double]]("ph"),
        data.getAs[Turbidity]("turbidity")
      ).mapN(WaterQuality.apply)
    }

  implicit val arbitraryWaterQuality: Arbitrary[WaterQuality] =
    Arbitrary {
      import Generators._

      for {
        location    <- Arbitrary.arbitrary[Location]
        timestamp   <- Arbitrary.arbitrary[ZonedDateTime]
        river       <- Arbitrary.arbitrary[String]
        temperature <- Arbitrary.arbitrary[Double]
        ph          <- Arbitrary.arbitrary[Option[Double]]
        turbidity   <- Arbitrary.arbitrary[Turbidity]
      } yield WaterQuality(
        location    = location,
        timestamp   = timestamp,
        river       = river,
        temperature = temperature,
        ph          = ph,
        turbidity   = turbidity
      )
    }
}
