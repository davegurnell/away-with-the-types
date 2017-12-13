package minimapper

import cats.data.Validated
import cats.syntax.option._
import java.time.ZonedDateTime
import scala.collection.immutable.ListMap

sealed abstract class Data extends Product with Serializable with DataMethods

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



// ----------------------------------------------



// These are methods on Data objects.
// I've extracted them into a trait
// to make the type hierarchy above clearer.

trait DataMethods {
  self: Data =>

  type GetResult[A] = Either[String, A]

  def get(path: String *): GetResult[Data] = {
    def loop(path: List[String], data: GetResult[Data]): GetResult[Data] =
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

  def as[A](implicit fromData: FromData[A]): GetResult[A] =
    fromData(this).leftMap(_.head).toEither

  def getAs[A](path: String *)(implicit fromData: FromData[A]): GetResult[A] =
    get(path : _*).flatMap(_.as[A])
}
