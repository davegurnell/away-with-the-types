package minimapper

import scala.language.implicitConversions

sealed trait PathSegment
case class Field(field: String) extends PathSegment
case class Index(index: Int) extends PathSegment

object PathSegment {
  implicit def stringToPathSegment(field: String): PathSegment =
    Field(field)

  implicit def intToPathSegment(index: Int): PathSegment =
    Index(index)
}
