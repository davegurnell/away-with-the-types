package minimapper

import java.time.ZonedDateTime
import scala.collection.immutable.ListMap

sealed abstract class Data extends Product with Serializable {
  def get(path: String *): Either[String, Data] = {
    def loop(path: List[String], data: Either[String, Data]): Either[String, Data] =
      data.flatMap { data =>
        (path, data) match {
          case (Nil, data) =>
            Right(data)

          case (field :: rest, ProductData(values)) =>
            loop(rest, values.get(field).toRight(s"field not found: $field"))

          case (path, SumData(_, value)) =>
            loop(path, Right(value))

          case (field :: rest, _) =>
            Left(s"field not found: $field")
        }
      }

    loop(path.toList, Right(this))
  }

  def as[A](implicit fromData: FromData[A]): Either[String, A] =
    fromData(this)

  def getAs[A](path: String *)(implicit fromData: FromData[A]): Either[String, A] =
    get(path : _*).flatMap(_.as[A])

  def typeName: Either[String, String] =
    this match {
      case SumData(tpe, _) => Right(tpe)
      case _ => Left("type name not found")
    }
}

/**
 * A substitute for case classes.
 * Keys in the map are field names,
 * values are the values of those fields.
 */
case class ProductData(values: ListMap[String, Data]) extends Data

/**
 * An encoding of sum types. A substitute for sealed traits.
 * If a value can be of type A or type B,
 * we wrap it in a SumData instance
 * that maintains the type name as a String.
 */
case class SumData(tpe: String, value: Data) extends Data

case class BooleanData(value: Boolean) extends Data
case class IntData(value: Int) extends Data
case class DoubleData(value: Double) extends Data
case class StringData(value: String) extends Data
case class TimestampData(value: ZonedDateTime) extends Data

case class ListData(values: List[Data]) extends Data
case object NullData extends Data
