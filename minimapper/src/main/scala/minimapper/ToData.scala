package minimapper

import java.time.ZonedDateTime
import scala.collection.immutable.ListMap

/** Type class for converting regular Scala values to Data instances. */
trait ToData[A] {
  def apply(value: A): Data
}

trait ToDataSyntax {
  /** Extend arbitrary data types with a toData method */
  implicit class ToDataOps[A](value: A) {
    def toData(implicit toData: ToData[A]): Data =
      toData(value)
  }
}

object ToData extends ShapelessToDataInstances {
  implicit val dataInstance: ToData[Data] =
    instance(identity)

  implicit val booleanInstance: ToData[Boolean] =
    instance(BooleanData.apply)

  implicit val intInstance: ToData[Int] =
    instance(IntData.apply)

  implicit val doubleInstance: ToData[Double] =
    instance(DoubleData.apply)

  implicit val stringInstance: ToData[String] =
    instance(StringData.apply)

  implicit val timestampInstance: ToData[ZonedDateTime] =
    instance(TimestampData.apply)

  implicit def listInstance[A](implicit aInstance: ToData[A]): ToData[List[A]] =
    instance(in => ListData(in.map(aInstance.apply)))

  implicit def optionInstance[A](implicit aInstance: ToData[A]): ToData[Option[A]] =
    instance {
      case Some(a) => aInstance(a)
      case None    => NullData
    }
}

trait ToDataFunctions {
  /**
   * Summon a ToData instance from implicit scope. For example:
   *
   * val toData = ToData[List[Int]]
   *
   * toData(List(1, 2, 3))
   * // res0: Data = ListData(List(IntData(1), IntData(2), IntData(3)))
   */
  def apply[A](implicit instance: ToData[A]): ToData[A] =
    instance

  /** Convenience constructor for ToData instances. */
  def instance[A](func: A => Data): ToData[A] =
    new ToData[A] {
      override def apply(value: A): Data =
        func(value)
    }
}

/**
 * Code to automatically derive
 * instances of ToData for arbitrary algebraic data types.
 *
 * This code is advanced for this talk,
 * so I've bundled all the moving parts in this trait
 * to keep them out of the way.
 */
trait ShapelessToDataInstances extends ToDataFunctions {
  import shapeless.{Data => _, _}
  import shapeless.labelled.FieldType

  /**
   * Specialization of ToData for product types.
   * Used to simplify the types and avoid casts below.
   */
  trait ToProductData[A] extends ToData[A] {
    def apply(value: A): ProductData
  }

  /**
   * Specialization of ToData for sum types.
   * Used to simplify the types and avoid casts below.
   */
  trait ToSumData[A] extends ToData[A] {
    def apply(value: A): SumData
  }

  /** Convenience constructor for ToProductData instances. */
  def instanceProduct[A](func: A => ProductData): ToProductData[A] =
    new ToProductData[A] {
      override def apply(value: A): ProductData =
        func(value)
    }

  /** Convenience constructor for ToSumData instances. */
  def instanceSum[A](func: A => SumData): ToSumData[A] =
    new ToSumData[A] {
      override def apply(value: A): SumData =
        func(value)
    }

  implicit val hnilInstance: ToProductData[HNil] =
    instanceProduct[HNil](_ => ProductData(ListMap()))

  implicit def hconsInstance[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    lazyToH: Lazy[ToData[H]],
    toT: ToProductData[T]
  ): ToProductData[FieldType[K, H] :: T] = {
    val name = witness.value.name
    val toH = lazyToH.value
    instanceProduct[FieldType[K, H] :: T] {
      case head :: tail =>
        ProductData(ListMap((name, toH(head))) ++ toT(tail).values)
    }
  }

  implicit val cnilInstance: ToSumData[CNil] =
    instanceSum[CNil](_ => sys.error("Attempt to decode empty coproduct"))

  implicit def cconsInstance[K <: Symbol, H, T <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    lazyToH: Lazy[ToData[H]],
    toT: ToSumData[T]
  ): ToSumData[FieldType[K, H] :+: T] = {
    val name = witness.value.name
    val hEnc = lazyToH.value
    instanceSum[FieldType[K, H] :+: T] {
      case Inl(head) => SumData(name, hEnc(head))
      case Inr(tail) => toT(tail)
    }
  }

  implicit def genericProductInstance[A, L](
    implicit
    gen: LabelledGeneric.Aux[A, L],
    to: Lazy[ToProductData[L]]
  ): ToProductData[A] =
    instanceProduct[A](in => to.value(gen.to(in)))

  implicit def genericSumDataInstance[A, L](
    implicit
    gen: LabelledGeneric.Aux[A, L],
    to: Lazy[ToSumData[L]]
  ): ToSumData[A] =
    instanceSum[A](in => to.value(gen.to(in)))
}

