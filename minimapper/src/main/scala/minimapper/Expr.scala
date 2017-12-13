package minimapper

import cats.instances.either._ // Applicative for Either
import cats.instances.list._   // Traverse for List
import cats.syntax.either._    // orElse for Either
import cats.syntax.traverse._  // traverse for Either

sealed abstract class Expr extends Product with Serializable
  with ExprConstructorMethods
  with ExprEvalMethods

final case class Select(path: List[String]) extends Expr
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
  def eval(data: Data): Either[String, Data] =
    this match {
      case Select(path)      => data.get(path : _*)
      case Const(data)       => Right(data)
      case Apply(func, args) => evalApply(func, args, data)
    }

  def evalAs[A](data: Data)(implicit fromData: FromData[A]): Either[String, A] =
    eval(data).flatMap(data => fromData(data))

  private def evalApply(name: String, args: List[Expr], data: Data): Either[String, Data] =
    name match {
      case "unary_-" =>
        arity1(args) { a =>
          for {
            x <- a.evalAs[Int](data)
          } yield -x
        } orElse
        arity1(args) { a =>
          for {
            x <- a.evalAs[Double](data)
          } yield -x
        }

      case "+" =>
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Int](data)
            y <- b.evalAs[Int](data)
          } yield x + y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Double](data)
            y <- b.evalAs[Double](data)
          } yield x + y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[String](data)
            y <- b.evalAs[String](data)
          } yield x + y
        }

      case "-" =>
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Int](data)
            y <- b.evalAs[Int](data)
          } yield x - y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Double](data)
            y <- b.evalAs[Double](data)
          } yield x - y
        }

      case "*" =>
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Int](data)
            y <- b.evalAs[Int](data)
          } yield x * y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Double](data)
            y <- b.evalAs[Double](data)
          } yield x * y
       }

      case "/" =>
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Int](data)
            y <- b.evalAs[Int](data)
          } yield x / y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Double](data)
            y <- b.evalAs[Double](data)
          } yield x / y
        }

      case ">" =>
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Int](data)
            y <- b.evalAs[Int](data)
          } yield x > y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Double](data)
            y <- b.evalAs[Double](data)
          } yield x > y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[String](data)
            y <- b.evalAs[String](data)
          } yield x > y
        }

      case "<" =>
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Int](data)
            y <- b.evalAs[Int](data)
          } yield x < y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Double](data)
            y <- b.evalAs[Double](data)
          } yield x < y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[String](data)
            y <- b.evalAs[String](data)
          } yield x < y
        }

      case ">=" =>
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Int](data)
            y <- b.evalAs[Int](data)
          } yield x >= y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Double](data)
            y <- b.evalAs[Double](data)
          } yield x >= y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[String](data)
            y <- b.evalAs[String](data)
          } yield x >= y
        }

      case "<=" =>
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Int](data)
            y <- b.evalAs[Int](data)
          } yield x <= y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[Double](data)
            y <- b.evalAs[Double](data)
          } yield x <= y
        } orElse
        arity2(args) { (a, b) =>
          for {
            x <- a.evalAs[String](data)
            y <- b.evalAs[String](data)
          } yield x <= y
        }

      case "===" =>
        arity2(args) { (a, b) =>
          for {
            x <- a.eval(data)
            y <- b.eval(data)
          } yield x == y
        }

      case "=!=" =>
        arity2(args) { (a, b) =>
          for {
            x <- b.eval(data)
            y <- b.eval(data)
          } yield x != y
        }

      case "unary_!" =>
        arity1(args) { a =>
          for {
            x <- a.evalAs[Boolean](data)
          } yield !x
        }

      case "&&" =>
        arity2[Boolean](args) { (a, b) =>
          for {
            x <- a.evalAs[Boolean](data)
            y <- b.evalAs[Boolean](data)
          } yield x && y
        }

      case "||" =>
        arity2[Boolean](args) { (a, b) =>
          for {
            x <- a.evalAs[Boolean](data)
            y <- b.evalAs[Boolean](data)
          } yield x || y
        }

      case "++" =>
        arity2[List[Data]](args) { (a, b) =>
          for {
            x <- a.evalAs[List[Data]](data)
            y <- b.evalAs[List[Data]](data)
          } yield x ++ y
        }

      case "getOrElse" =>
        arity2(args) { (a, b) =>
          for {
            opt <- a.evalAs[Option[Data]](data)
            res <- opt.fold(b.eval(data))(Right(_))
          } yield res
        }

      case "combineAll" =>
        arity1(args) { a =>
          for {
            x <- a.evalAs[List[Int]](data)
          } yield x.sum
        } orElse
        arity1(args) { a =>
          for {
            x <- a.evalAs[List[Double]](data)
          } yield x.sum
        } orElse
        arity1(args) { a =>
          for {
            x <- a.evalAs[List[String]](data)
          } yield x.mkString
        }

      case other =>
        Left(s"unknown method: $other")
    }

  private def arity1[R](args: List[Expr])(func: Expr => Either[String, R])(implicit toData: ToData[R]): Either[String, Data] =
    args match {
      case a :: Nil => func(a).map(toData.apply)
      case _        => arityMismatch(1)
    }

  private def arity2[R](args: List[Expr])(func: (Expr, Expr) => Either[String, R])(implicit toData: ToData[R]): Either[String, Data] =
    args match {
      case a :: b :: Nil => func(a, b).map(toData.apply)
      case _             => arityMismatch(2)
    }

  private def arity3[R](args: List[Expr])(func: (Expr, Expr, Expr) => Either[String, R])(implicit toData: ToData[R]): Either[String, Data] =
    args match {
      case a :: b :: c :: Nil => func(a, b, c).map(toData.apply)
      case _                  => arityMismatch(3)
    }

  private def arityMismatch(arity: Int): Either[String, Data] =
    Left(s"arity mismatch: $arity")
}

trait ExprSyntax {
  self: ToDataSyntax =>

  import scala.language.implicitConversions

  def dataAt(path: String *): Select =
    Select(path.toList)

  implicit class ExprOps[A](value: A) {
    def toExpr(implicit toData: ToData[A]): Expr =
      Const(value.toData)
  }

  implicit def anyToExpr[A](value: A)(implicit toData: ToData[A]): Expr =
    value.toExpr
}
