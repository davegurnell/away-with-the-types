package minimapper

import java.time.{Month, Year, ZonedDateTime, ZoneId}
import org.scalacheck._
import scala.collection.immutable.ListMap

object Generators {
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

  def genRecursive[A](baseGen: => Gen[A], recursiveGen: => Gen[A]): Gen[A] =
    Gen.sized(size => if(size <= 0) baseGen else recursiveGen)

  def genSmaller[A](gen: Gen[A]): Gen[A] =
    Gen.sized(size => Gen.resize(size / 2, gen))

  def genBooleanData: Gen[Data] =
    Arbitrary.arbitrary[Boolean].map(BooleanData)

  def genIntData: Gen[Data] =
    Arbitrary.arbitrary[Int].map(IntData)

  def genDoubleData: Gen[Data] =
    Arbitrary.arbitrary[Double].map(DoubleData)

  def genStringData: Gen[Data] =
    Arbitrary.arbitrary[String].map(StringData)

  def genTimestampData: Gen[Data] =
    Arbitrary.arbitrary[ZonedDateTime].map(TimestampData)

  def genNullData: Gen[Data] =
    Gen.const(NullData)

  def genListData: Gen[Data] =
    Gen.listOf(genSmaller(genData)).map(ListData)

  def genProductData: Gen[Data] =
    Gen.listOf {
      for {
        key   <- Gen.alphaStr
        value <- genSmaller(genData)
      } yield (key, value)
    }.map(list => ProductData(ListMap(list : _*)))

  def genSumData: Gen[Data] =
    {
      for {
        key   <- Gen.alphaStr
        value <- genSmaller(genData)
      } yield (key, value)
    }.map(SumData.tupled)

  def genAtomicData: Gen[Data] =
    Gen.oneOf(
      genBooleanData,
      genIntData,
      genDoubleData,
      genStringData,
      genTimestampData,
      genNullData
    )

  def genRecursiveData: Gen[Data] =
    Gen.oneOf(
      genAtomicData,
      genListData,
      genProductData,
      genSumData
    )

  def genData: Gen[Data] =
    genRecursive(genAtomicData, genRecursiveData)

  implicit def arbitraryData: Arbitrary[Data] =
    Arbitrary(genData)

  def genDataFromSchema(schema: Schema): Gen[Data] =
    schema match {
      case BooleanSchema           => genBooleanData
      case IntSchema               => genIntData
      case DoubleSchema            => genDoubleData
      case StringSchema            => genStringData
      case TimestampSchema         => genTimestampData
      case DefaultSchema(child, _) => genDataFromSchema(child)
      case ListSchema(child)       => Gen.listOf(genDataFromSchema(child)).map(ListData)
      case OptionSchema(child)     => Gen.oneOf(genNullData, genDataFromSchema(child))
      case ProductSchema(children) => Gen.sequence[List[(String, Data)], (String, Data)] {
                                        children.toList.map {
                                          case (k, v) =>
                                            genDataFromSchema(v).map(v => (k, v))
                                        }
                                      }.map(seq => ProductData(ListMap(seq : _*)))
      case SumSchema(children)     => for {
                                        index <- Gen.choose(0, children.size - 1)
                                        value <- children.toList(index) match {
                                                   case (k, v) =>
                                                     genDataFromSchema(v).map(v => SumData(k, v))
                                                 }
                                      } yield value
    }

  // Schema generators --------------------------

  def genListSchema: Gen[Schema] =
    genSmaller(genSchema).map(ListSchema)

  def genOptionSchema: Gen[Schema] =
    genSmaller(genSchema).map(OptionSchema)

  def genProductSchema: Gen[Schema] =
    Gen.listOf {
      for {
        key   <- Gen.alphaStr
        value <- genSmaller(genSchema)
      } yield (key, value)
    }.map(list => ProductSchema(ListMap(list : _*)))

  def genSumSchema: Gen[Schema] =
    Gen.nonEmptyListOf {
      for {
        key   <- Gen.alphaStr
        value <- genSmaller(genSchema)
      } yield (key, value)
    }.map(list => SumSchema(ListMap(list : _*)))

  def genAtomicSchema: Gen[Schema] =
    Gen.oneOf(
      Gen.const(BooleanSchema   : Schema),
      Gen.const(IntSchema       : Schema),
      Gen.const(DoubleSchema    : Schema),
      Gen.const(StringSchema    : Schema),
      Gen.const(TimestampSchema : Schema)
    )

  def genRecursiveSchema: Gen[Schema] =
    Gen.oneOf(
      genAtomicSchema,
      genListSchema,
      genProductSchema,
      genSumSchema
    )

  def genSchema: Gen[Schema] =
    genRecursive(genAtomicSchema, genRecursiveSchema)

  implicit def arbitrarySchema: Arbitrary[Schema] =
    Arbitrary(genSchema)
}
