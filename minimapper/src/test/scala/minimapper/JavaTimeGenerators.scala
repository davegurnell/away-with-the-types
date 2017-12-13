package minimapper

import java.time.{Month, Year, ZonedDateTime, ZoneId}
import org.scalacheck._

object JavaTimeGenerators {
  val genZonedDataTime: Gen[ZonedDateTime] = {
    import collection.JavaConverters._
    for {
      year       <- Gen.choose(1900, 2100)
      month      <- Gen.choose(1, 12)
      monthLength = Month.of(month).length(Year.of(year).isLeap)
      day        <- Gen.choose(1, monthLength)
      hour       <- Gen.choose(0, 23)
      minute     <- Gen.choose(0, 59)
      second     <- Gen.choose(0, 59)
    } yield ZonedDateTime.of(year, month, day, hour, minute, second, 0, ZoneId.of("UTC"))
  }

  implicit def arbitraryZonedDateTime: Arbitrary[ZonedDateTime] =
    Arbitrary(genZonedDataTime)
}
