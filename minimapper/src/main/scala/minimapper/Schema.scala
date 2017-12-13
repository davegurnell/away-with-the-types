package minimapper

import java.time.ZonedDateTime
import scala.collection.immutable.ListMap
import scala.collection.immutable.ListSet

sealed abstract class Schema extends SchemaMethods
  with Product
  with Serializable

final case class ProductSchema(children: ListMap[String, Schema]) extends Schema
final case class SumSchema(children: ListMap[String, Schema]) extends Schema

case object BooleanSchema extends Schema
case object IntSchema extends Schema
case object DoubleSchema extends Schema
case object StringSchema extends Schema
case object TimestampSchema extends Schema

final case class ListSchema(child: Schema) extends Schema
final case class OptionSchema(child: Schema) extends Schema
final case class DefaultSchema(child: Schema, default: Data) extends Schema

trait SchemaMethods {
  self: Schema =>

  def empty: Data = {
    import syntax._
    this match {
      case BooleanSchema           => BooleanData(false)
      case IntSchema               => IntData(0)
      case DoubleSchema            => DoubleData(0.0)
      case StringSchema            => StringData("")
      case TimestampSchema         => TimestampData(ZonedDateTime.now)
      case ListSchema(child)       => ListData(Nil)
      case OptionSchema(child)     => NullData
      case DefaultSchema(_, data)  => data
      case ProductSchema(children) => ProductData(children.map { case (k, v) => (k, v.empty) })
      case SumSchema(children)     => SumData(children.head._1, children.head._2.empty)
    }
  }

  def typeCheck(data: Data): List[String] = {
    (this, data) match {
      case (BooleanSchema            , _: BooleanData)     => Nil
      case (IntSchema                , _: IntData)         => Nil
      case (DoubleSchema             , _: DoubleData)      => Nil
      case (StringSchema             , _: StringData)      => Nil
      case (TimestampSchema          , _: TimestampData)   => Nil
      case (ListSchema(schema)       , ListData(datas))    => datas.flatMap(schema.typeCheck)
      case (OptionSchema(schema)     , NullData)           => Nil
      case (OptionSchema(schema)     , data)               => schema.typeCheck(data)
      case (DefaultSchema(schema, _) , data)               => schema.typeCheck(data)
      case (schema: ProductSchema    , data: ProductData)  => typeCheckProduct(schema, data)
      case (schema: SumSchema        , data: SumData)      => typeCheckSum(schema, data)
      case (schema                   , data)               => typeError(schema, data)
    }
  }

  // Helper for typeCheck
  private def typeCheckProduct(schema: ProductSchema, data: ProductData): List[String] =
    schema.children.toList.flatMap {
      case (field, schema) =>
        data.values.get(field) match {
          case Some(data) => schema.typeCheck(data)
          case None       => List(s"type error: missing field: $field")
        }
    }

  // Helper for typeCheck
  private def typeCheckSum(schema: SumSchema, data: SumData): List[String] =
    schema.children
      .collectFirst { case (data.tpe, schema) => schema.typeCheck(data.value) }
      .getOrElse(typeError(schema, data))

  // Helper for typeCheck
  private def typeError(schema: Schema, data: Data): List[String] =
    List(s"type error: expected $schema, found $data")
}