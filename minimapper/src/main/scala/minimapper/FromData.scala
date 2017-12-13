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

  // def validate(test: A => Boolean, error: A => List[Message]): FromData[A] =
  //   FromData.instance[A](data => this(data).fold(
  //     errors => errors.invalid,
  //     value  => if(test(value)) value.valid else error(value).invalid
  //   ))
}

object FromData extends ShapelessFromDataInstances {
  implicit val dataInstance: FromData[Data] =
    instance(_.valid)

  implicit val booleanInstance: FromData[Boolean] =
    instance {
      case BooleanData(bool) =>
        Validated.valid(bool)
      case value =>
        Validated.invalid(List(s"invalid boolean: $value"))
    }

  implicit val intInstance: FromData[Int] =
    instance {
      case IntData(num) => num.valid
      case value        => List(s"invalid int: $value").invalid
    }

  implicit val doubleInstance: FromData[Double] =
    instance {
      case DoubleData(num) => num.valid
      case value           => List(s"invalid double: $value").invalid
    }

  implicit val stringInstance: FromData[String] =
    instance {
      case StringData(str) => str.valid
      case value           => List(s"invalid string: $value").invalid
    }

  implicit val timestampInstance: FromData[ZonedDateTime] =
    instance {
      case TimestampData(dt) => dt.valid
      case value             => List(s"invalid timestamp: $value").invalid
    }

  implicit def listInstance[A](implicit aInstance: FromData[A]): FromData[List[A]] =
    instance {
      case ListData(data) => data.traverse[Result, A](aInstance.apply)
      case value          => List(s"invalid list: $value").invalid
    }

  implicit def optionInstance[A](implicit aInstance: FromData[A]): FromData[Option[A]] =
    instance {
      case NullData => None.valid
      case data     => aInstance(data).map(Some(_))
    }
}

trait FromDataFunctions {
  type Result[A] = Validated[List[String], A]

  def apply[A](implicit instance: FromData[A]): FromData[A] =
    instance

  def instance[A](func: Data => Result[A]): FromData[A] =
    new FromData[A] {
      def apply(data: Data): Result[A] =
        func(data)
    }
}

trait ShapelessFromDataInstances extends FromDataFunctions {
  import shapeless.{Data => _, _}
  import shapeless.labelled.{FieldType, field}

  trait FromProductData[A] extends FromData[A] {
    override def map[B](func: A => B): FromProductData[B] =
      instanceProduct[B](data => this(data).map(func))
  }

  trait FromSumData[A] extends FromData[A] {
    override def map[B](func: A => B): FromSumData[B] =
      instanceSum[B](data => this(data).map(func))
  }

  def instanceProduct[A](func: Data => Result[A]): FromProductData[A] =
    new FromProductData[A] {
      def apply(data: Data): Result[A] =
        func(data)
    }

  def instanceSum[A](func: Data => Result[A]): FromSumData[A] =
    new FromSumData[A] {
      def apply(data: Data): Result[A] =
        func(data)
    }

  implicit val hnilInstance: FromProductData[HNil] =
    instanceProduct[HNil](_ => HNil.valid)

  implicit def hconsInstance[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    lazyFromH: Lazy[FromData[H]],
    fromT: FromProductData[T]
  ): FromProductData[FieldType[K, H] :: T] = {
    val name = witness.value.name
    val fromH = lazyFromH.value

    object FindField {
      def unapply(fields: ListMap[String, Data]): Option[Data] =
        fields.get(name)
    }

    instanceProduct[FieldType[K, H] :: T] {
      case data @ ProductData(FindField(value)) =>
        val h: Result[FieldType[K, H]] = fromH(value).map(field[K](_))
        val t: Result[T]               = fromT(data)
        (h, t).mapN(_ :: _)
      case value => List(s"missing field: $name").invalid
    }
  }

  implicit val cnilInstance: FromSumData[CNil] =
    instanceSum[CNil](_ => List(s"empty sum").invalid)

  implicit def cconsInstance[K <: Symbol, H, T <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    lazyFromH: Lazy[FromData[H]],
    fromT: FromSumData[T]
  ): FromSumData[FieldType[K, H] :+: T] = {
    val name = witness.value.name
    val fromH = lazyFromH.value

    instanceSum[FieldType[K, H] :+: T] {
      case SumData(`name`, data) => fromH(data).map(in => Inl(field[K](in)))
      case data: SumData         => fromT(data).map(in => Inr(in))
      case value                 => List(s"invalid sum: $value").invalid
    }
  }

  implicit def genericProductInstance[A, L](
    implicit
    gen: LabelledGeneric.Aux[A, L],
    from: Lazy[FromProductData[L]]
  ): FromProductData[A] =
    instanceProduct[A](in => from.value(in).map(gen.from))

  implicit def genericSumInstance[A, L](
    implicit
    gen: LabelledGeneric.Aux[A, L],
    from: Lazy[FromSumData[L]]
  ): FromSumData[A] =
    instanceSum[A](in => from.value(in).map(gen.from))
}
