package minimapper

import cats.instances.either._ // Applicative for Either
import cats.instances.list._   // Traverse for List
import cats.syntax.either._    // leftMap for Either
import cats.syntax.traverse._  // traverse for Either
import java.time.ZonedDateTime
import scala.collection.immutable.ListMap

trait FromData[A] {
  def apply(data: Data): Either[String, A]

  /**
   * Version of apply() that returns an Either,
   * for compatibility with Expr.eval and Data.get
   */
  def applyAsEither(data: Data): Either[String, A] =
    apply(data).leftMap(_.mkString(", "))

  def map[B](func: A => B): FromData[B] =
    FromData.instance[B](data => this(data).map(func))
}

object FromData {
  def apply[A](implicit instance: FromData[A]): FromData[A] =
    instance

  def instance[A](func: Data => Either[String, A]): FromData[A] =
    new FromData[A] {
      def apply(data: Data): Either[String, A] =
        func(data)
    }

  implicit val dataFromData: FromData[Data] =
    instance(Right(_))

  implicit val booleanFromData: FromData[Boolean] =
    instance {
      case BooleanData(bool) =>
        Right(bool)
      case value =>
        Left(s"invalid boolean: $value")
    }

  implicit val intFromData: FromData[Int] =
    instance {
      case IntData(num) => Right(num)
      case value        => Left(s"invalid int: $value")
    }

  implicit val doubleFromData: FromData[Double] =
    instance {
      case DoubleData(num) => Right(num)
      case value           => Left(s"invalid double: $value")
    }

  implicit val stringFromData: FromData[String] =
    instance {
      case StringData(str) => Right(str)
      case value           => Left(s"invalid string: $value")
    }

  implicit val timestampFromData: FromData[ZonedDateTime] =
    instance {
      case TimestampData(dt) => Right(dt)
      case value             => Left(s"invalid timestamp: $value")
    }

  implicit def listFromData[A](implicit aFromData: FromData[A]): FromData[List[A]] =
    instance {
      case ListData(data) => data.traverse(aFromData.apply)
      case value          => Left(s"invalid list: $value")
    }

  implicit def optionFromData[A](implicit aFromData: FromData[A]): FromData[Option[A]] =
    instance {
      case NullData => Right(None)
      case data     => aFromData(data).map(Some(_))
    }
}
