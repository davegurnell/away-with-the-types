package minimapper

import cats.data.Validated
import cats.syntax.option._
import java.time.ZonedDateTime
import scala.collection.immutable.ListMap

sealed abstract class Data extends Product with Serializable {
  type Result[A] = Validated[List[String], A]

  def get(path: String *): Result[Data] = {
    def loop(path: List[String], data: Result[Data]): Result[Data] =
      data.andThen { data =>
        (path, data) match {
          case (Nil, data) =>
            Validated.valid(data)

          case (field :: rest, ProductData(values)) =>
            loop(rest, values.get(field).toValid(List(s"field not found: $field")))

          case (path, SumData(_, value)) =>
            loop(path, Validated.valid(value))

          case (field :: rest, _) =>
            Validated.invalid(List(s"field not found: $field"))
        }
      }

    loop(path.toList, Validated.valid(this))
  }

  def as[A](implicit fromData: FromData[A]): FromData.Result[A] =
    fromData(this)

  def getAs[A](path: String *)(implicit fromData: FromData[A]): FromData.Result[A] =
    get(path : _*).andThen(_.as[A])

  def typeName: FromData.Result[String] =
    this match {
      case SumData(tpe, _) => Validated.valid(tpe)
      case _ => Validated.invalid(List("type name not found"))
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
