package minimapper

import cats.data.Validated
import cats.implicits._

sealed abstract class Expr extends Product with Serializable
  with ExprConstructorMethods
  with ExprEvalMethods

final case class Select(path: List[PathSegment]) extends Expr
final case class Const(data: Data) extends Expr
final case class Apply(func: String, args: List[Expr]) extends Expr

trait ExprConstructorMethods {
  self: Expr =>

  def unary_- : Expr = Apply("unary_-", List(this))
  def +(that: Expr): Expr = Apply("+", List(this, that))
  def -(that: Expr): Expr = Apply("-", List(this, that))
  def *(that: Expr): Expr = Apply("*", List(this, that))
  def /(that: Expr): Expr = Apply("/", List(this, that))
  def >(that: Expr): Expr = Apply(">", List(this, that))
  def <(that: Expr): Expr = Apply("<", List(this, that))
  def >=(that: Expr): Expr = Apply(">=", List(this, that))
  def <=(that: Expr): Expr = Apply("<=", List(this, that))
  def ===(that: Expr): Expr = Apply("===", List(this, that))
  def =!=(that: Expr): Expr = Apply("=!=", List(this, that))
  def unary_! : Expr = Apply("unary_!", List(this))
  def &&(that: Expr): Expr = Apply("&&", List(this, that))
  def ||(that: Expr): Expr = Apply("||", List(this, that))
  def ++(that: Expr): Expr = Apply("++", List(this, that))
  def getOrElse(that: Expr): Expr = Apply("getOrElse", List(this, that))
  def combineAll: Expr = Apply("combineAll", List(this))
}

trait ExprEvalMethods {
  type Result[A] = Either[String, A]

  def eval(data: Data): Result[Data] =
    this match {
      case Select(path)      => data.get(path : _*)
      case Const(data)       => Right(data)
      case Apply(func, args) => evalApply(func, args, data)
    }

  def evalAs[A](data: Data)(implicit fromData: FromData[A]): Result[A] =
    eval(data).flatMap(fromData.applyAsEither)

  private def evalApply(name: String, args: List[Expr], data: Data): Result[Data] =
    name match {
      case "unary_-" =>
        arity1(args)(a => a.evalAs[Int](data).map(x => -x)) orElse
        arity1(args)(a => a.evalAs[Double](data).map(x => -x))

      case "+" =>
        arity2(args)((a, b) => (a.evalAs[Int](data),    b.evalAs[Int](data)).mapN(_ + _)) orElse
        arity2(args)((a, b) => (a.evalAs[Double](data), b.evalAs[Double](data)).mapN(_ + _)) orElse
        arity2(args)((a, b) => (a.evalAs[String](data), b.evalAs[String](data)).mapN(_ + _))

      case "-" =>
        arity2(args)((a, b) => (a.evalAs[Int](data),    b.evalAs[Int](data)).mapN(_ - _)) orElse
        arity2(args)((a, b) => (a.evalAs[Double](data), b.evalAs[Double](data)).mapN(_ - _))

      case "*" =>
        arity2(args)((a, b) => (a.evalAs[Int](data),    b.evalAs[Int](data)).mapN(_ * _)) orElse
        arity2(args)((a, b) => (a.evalAs[Double](data), b.evalAs[Double](data)).mapN(_ * _))

      case "/" =>
        arity2(args)((a, b) => (a.evalAs[Int](data),    b.evalAs[Int](data)).mapN(_ / _)) orElse
        arity2(args)((a, b) => (a.evalAs[Double](data), b.evalAs[Double](data)).mapN(_ / _))

      case ">" =>
        arity2(args)((a, b) => (a.evalAs[Int](data),    b.evalAs[Int](data)).mapN(_ > _)) orElse
        arity2(args)((a, b) => (a.evalAs[Double](data), b.evalAs[Double](data)).mapN(_ > _)) orElse
        arity2(args)((a, b) => (a.evalAs[String](data), b.evalAs[String](data)).mapN(_ > _))

      case "<" =>
        arity2(args)((a, b) => (a.evalAs[Int](data),    b.evalAs[Int](data)).mapN(_ < _)) orElse
        arity2(args)((a, b) => (a.evalAs[Double](data), b.evalAs[Double](data)).mapN(_ < _)) orElse
        arity2(args)((a, b) => (a.evalAs[String](data), b.evalAs[String](data)).mapN(_ < _))

      case ">=" =>
        arity2(args)((a, b) => (a.evalAs[Int](data),    b.evalAs[Int](data)).mapN(_ >= _)) orElse
        arity2(args)((a, b) => (a.evalAs[Double](data), b.evalAs[Double](data)).mapN(_ >= _)) orElse
        arity2(args)((a, b) => (a.evalAs[String](data), b.evalAs[String](data)).mapN(_ >= _))

      case "<=" =>
        arity2(args)((a, b) => (a.evalAs[Int](data),    b.evalAs[Int](data)).mapN(_ <= _)) orElse
        arity2(args)((a, b) => (a.evalAs[Double](data), b.evalAs[Double](data)).mapN(_ <= _)) orElse
        arity2(args)((a, b) => (a.evalAs[String](data), b.evalAs[String](data)).mapN(_ <= _))

      case "===" =>
        arity2(args)((a, b) => (a.eval(data), b.eval(data)).mapN(_ == _))

      case "=!=" =>
        arity2(args)((a, b) => (b.eval(data), b.eval(data)).mapN(_ != _))

      case "unary_!" =>
        arity1(args)(a => a.evalAs[Boolean](data).map(x => !x))

      case "&&" =>
        arity2[Boolean](args)((a, b) => (a.evalAs[Boolean](data), b.evalAs[Boolean](data)).mapN(_ && _))

      case "||" =>
        arity2[Boolean](args)((a, b) => (a.evalAs[Boolean](data), b.evalAs[Boolean](data)).mapN(_ || _))

      case "++" =>
        arity2[List[Data]](args)((a, b) => (a.evalAs[List[Data]](data), b.evalAs[List[Data]](data)).mapN(_ ++ _))

      case "getOrElse" =>
        arity2(args) { (a, b) =>
          for {
            opt <- a.evalAs[Option[Data]](data)
            res <- opt.fold(b.eval(data))(Right(_))
          } yield res
        }

      case "combineAll" =>
        arity1(args)(a => a.evalAs[List[Int]](data).map(_.sum)) orElse
        arity1(args)(a => a.evalAs[List[Double]](data).map(_.sum)) orElse
        arity1(args)(a => a.evalAs[List[String]](data).map(_.mkString))

      case other =>
        Left(s"unknown method: $other")
    }

  private def arity1[R](args: List[Expr])(func: Expr => Result[R])(implicit toData: ToData[R]): Result[Data] =
    args match {
      case a :: Nil => func(a).map(toData.apply)
      case _        => arityMismatch(1)
    }

  private def arity2[R](args: List[Expr])(func: (Expr, Expr) => Result[R])(implicit toData: ToData[R]): Result[Data] =
    args match {
      case a :: b :: Nil => func(a, b).map(toData.apply)
      case _             => arityMismatch(2)
    }

  private def arity3[R](args: List[Expr])(func: (Expr, Expr, Expr) => Result[R])(implicit toData: ToData[R]): Result[Data] =
    args match {
      case a :: b :: c :: Nil => func(a, b, c).map(toData.apply)
      case _                  => arityMismatch(3)
    }

  private def arityMismatch(arity: Int): Result[Data] =
    Left(s"arity mismatch: $arity")
}

trait ExprSyntax {
  self: ToDataSyntax =>

  import scala.language.implicitConversions

  def dataAt(path: PathSegment *): Select =
    Select(path.toList)

  implicit class ExprOps[A](value: A) {
    def toExpr(implicit toData: ToData[A]): Expr =
      Const(value.toData)
  }

  implicit def anyToExpr[A](value: A)(implicit toData: ToData[A]): Expr =
    value.toExpr
}
