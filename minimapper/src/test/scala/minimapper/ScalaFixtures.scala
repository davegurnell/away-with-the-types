package minimapper

import java.time.ZonedDateTime
import org.scalacheck._

case class Location(lat: Double, lng: Double)

sealed trait Turbidity
case class QualitativeTurbidity(description: String) extends Turbidity
case class QuantitativeTurbidity(value: Int) extends Turbidity

case class WaterQuality(
  location    : Location,
  timestamp   : ZonedDateTime,
  river       : Option[String],
  temperature : Option[Double],
  ph          : Option[Double],
  turbidity   : Turbidity
)

object ScalaFixtures {
  import JavaTimeGenerators._

  implicit val arbitraryLocation: Arbitrary[Location] =
    Arbitrary {
      for {
        lat <- Arbitrary.arbitrary[Double]
        lng <- Arbitrary.arbitrary[Double]
      } yield Location(lat, lng)
    }

  implicit val arbitraryTurbidity: Arbitrary[Turbidity] =
    Arbitrary {
      Arbitrary.arbitrary[Boolean].flatMap {
       case true  => Gen.chooseNum(0, 240).map(QuantitativeTurbidity.apply)
       case false => Gen.alphaStr.map(QualitativeTurbidity.apply)
      }
    }

  implicit val arbitraryWaterQuality: Arbitrary[WaterQuality] =
    Arbitrary {
      for {
        location    <- Arbitrary.arbitrary[Location]
        timestamp   <- Arbitrary.arbitrary[ZonedDateTime]
        river       <- Arbitrary.arbitrary[Option[String]]
        temperature <- Arbitrary.arbitrary[Option[Double]]
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
