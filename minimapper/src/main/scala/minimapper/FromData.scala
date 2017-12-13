package minimapper

import cats.data.Validated
import cats.implicits._
import java.time.ZonedDateTime
import scala.collection.immutable.ListMap

trait FromData[A] {
  def apply(data: Data): FromData.Result[A]

  /**
   * Version of apply() that returns an Either,
   * for compatibility with Expr.eval and Data.get
   */
  def applyAsEither(data: Data): Either[String, A] =
    apply(data).leftMap(_.mkString(", ")).toEither

  def map[B](func: A => B): FromData[B] =
    FromData.instance[B](data => this(data).map(func))
}

object FromData {
  type Result[A] = Validated[List[String], A]

  def apply[A](implicit instance: FromData[A]): FromData[A] =
    instance

  def instance[A](func: Data => Result[A]): FromData[A] =
    new FromData[A] {
      def apply(data: Data): Result[A] =
        func(data)
    }

  implicit val dataFromData: FromData[Data] =
    instance(_.valid)

  implicit val booleanFromData: FromData[Boolean] =
    instance {
      case BooleanData(bool) =>
        Validated.valid(bool)
      case value =>
        Validated.invalid(List(s"invalid boolean: $value"))
    }

  implicit val intFromData: FromData[Int] =
    instance {
      case IntData(num) => num.valid
      case value        => List(s"invalid int: $value").invalid
    }

  implicit val doubleFromData: FromData[Double] =
    instance {
      case DoubleData(num) => num.valid
      case value           => List(s"invalid double: $value").invalid
    }

  implicit val stringFromData: FromData[String] =
    instance {
      case StringData(str) => str.valid
      case value           => List(s"invalid string: $value").invalid
    }

  implicit val timestampFromData: FromData[ZonedDateTime] =
    instance {
      case TimestampData(dt) => dt.valid
      case value             => List(s"invalid timestamp: $value").invalid
    }

  implicit def listFromData[A](implicit aFromData: FromData[A]): FromData[List[A]] =
    instance {
      case ListData(data) => data.traverse[Result, A](aFromData.apply)
      case value          => List(s"invalid list: $value").invalid
    }

  implicit def optionFromData[A](implicit aFromData: FromData[A]): FromData[Option[A]] =
    instance {
      case NullData => None.valid
      case data     => aFromData(data).map(Some(_))
    }
}
