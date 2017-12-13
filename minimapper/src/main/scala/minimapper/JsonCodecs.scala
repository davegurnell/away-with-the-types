package minimapper

import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{ZonedDateTime, ZoneId}
import scala.collection.immutable.{ListMap, ListSet}

object JsonCodecs extends JsonCodecs

trait JsonCodecs extends ZonedDateTimeCodecInstances
  with JsonEncoderInstances
  with JsonDecoderInstances

trait JsonEncoderInstances {
  self: ZonedDateTimeCodecInstances =>

  implicit val dataEncoder: Encoder[Data] =
    Encoder.instance[Data] {
      case BooleanData(value)   => value.asJson
      case IntData(value)       => value.asJson
      case DoubleData(value)    => value.asJson
      case StringData(value)    => value.asJson
      case TimestampData(value) => value.asJson
      case NullData             => Json.Null
      case ListData(values)     => values.map(_.asJson).asJson
      case ProductData(values)  => values.mapValues(_.asJson).asJson
      case SumData(tpe, value)  => Json.obj(tpe -> value.asJson)
    }
}

trait JsonDecoderInstances {
  self: ZonedDateTimeCodecInstances =>

  private val booleanDataDecoder: Decoder[Data] =
    Decoder[Boolean].map(BooleanData)

  private val intDataDecoder: Decoder[Data] =
    Decoder[Int].map(IntData)

  private val doubleDataDecoder: Decoder[Data] =
    Decoder[Double].map(DoubleData)

  private val stringDataDecoder: Decoder[Data] =
    Decoder[String].map(StringData)

  private val timestampDataDecoder: Decoder[Data] =
    Decoder[ZonedDateTime].map(TimestampData)

  private def listDataDecoder(childDecoder: Decoder[Data]): Decoder[Data] =
    Decoder.decodeList(childDecoder).map(ListData)

  private val nullDataDecoder: Decoder[Data] =
    Decoder.instance { cursor =>
      cursor.focus match {
        case Some(Json.Null) => Right(NullData)
        case _               => Left(DecodingFailure("Null", cursor.history))
      }
    }

  private def productDecoder(schema: ProductSchema): Decoder[Data] =
    Decoder.instance { cursor =>
      schema.children.toList.traverse {
        case (k, schema) =>
          cursor
            .downField(k)
            .as(dataDecoder(schema))
            .map(v => (k, v))
      }.map(pairs => ProductData(ListMap(pairs : _*)))
    }

  private def sumDataDecoder(schema: SumSchema): Decoder[Data] =
    schema.children.map {
      case (k, schema) =>
        Decoder.instance { cursor =>
          cursor
            .downField(k)
            .as(dataDecoder(schema))
            .map(v => SumData(k, v) : Data)
        }
    }.reduceLeft(_ or _)

  def dataDecoder(schema: Schema): Decoder[Data] =
    schema match {
      case BooleanSchema           => booleanDataDecoder
      case IntSchema               => intDataDecoder
      case DoubleSchema            => doubleDataDecoder
      case StringSchema            => stringDataDecoder
      case TimestampSchema         => timestampDataDecoder
      case ListSchema(child)       => listDataDecoder(dataDecoder(child))
      case OptionSchema(child)     => nullDataDecoder.or(dataDecoder(child))
      case DefaultSchema(child, _) => dataDecoder(child)
      case schema: ProductSchema   => productDecoder(schema)
      case schema: SumSchema       => sumDataDecoder(schema)
    }
}

trait ZonedDateTimeCodecInstances {
  private val fmt = DateTimeFormatter.ISO_ZONED_DATE_TIME
  private val utc = ZoneId.of("UTC")

  implicit val zonedDateTimeEncoder: Encoder[ZonedDateTime] =
    Encoder.instance[ZonedDateTime](value => value.toInstant.atZone(utc).format(fmt).asJson)

  implicit val zonedDateTimeDecoder: Decoder[ZonedDateTime] =
    Decoder[String].emap { str =>
      try {
        Right(ZonedDateTime.parse(str, fmt))
      } catch {
        case exn: DateTimeParseException =>
          Left("Timestamp")
      }
    }
}