package minimapper

import java.time.ZonedDateTime
import scala.collection.immutable.ListMap

trait ToData[A] {
  def apply(value: A): Data
}

trait ToDataSyntax {
  implicit class ToDataOps[A](value: A) {
    def toData(implicit toData: ToData[A]): Data =
      toData(value)
  }
}

object ToData {
  def apply[A](implicit instance: ToData[A]): ToData[A] =
    instance

  def instance[A](func: A => Data): ToData[A] =
    new ToData[A] {
      override def apply(value: A): Data =
        func(value)
    }

  implicit val dataToData: ToData[Data] =
    instance(identity)

  implicit val booleanToData: ToData[Boolean] =
    instance(BooleanData.apply)

  implicit val intToData: ToData[Int] =
    instance(IntData.apply)

  implicit val doubleToData: ToData[Double] =
    instance(DoubleData.apply)

  implicit val stringToData: ToData[String] =
    instance(StringData.apply)

  implicit val timestampToData: ToData[ZonedDateTime] =
    instance(TimestampData.apply)

  implicit def listToData[A](implicit aToData: ToData[A]): ToData[List[A]] =
    instance(in => ListData(in.map(aToData.apply)))

  implicit def optionToData[A](implicit aToData: ToData[A]): ToData[Option[A]] =
    instance {
      case Some(a) => aToData(a)
      case None    => NullData
    }
}
