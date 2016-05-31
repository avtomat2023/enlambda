package enlambda.compile.unabs

import enlambda.compile._
import enlambda.compile.parse

object Path extends CompilerPath[Map[parse.Id, parse.Expr], Map[Id, Expr]] {
  import enlambda.util.Implicits._

  def expr(source: parse.Expr): Either[Set[parse.Var], Expr] = source match {
    case parse.Const(name) => Right(Const(name))
    case parse.Id(name) => Right(Id(name))
    case x: parse.Var => Left(Set(x))
    case parse.Abs(variable, func) => expr(func.eliminate(variable))
    case parse.App(func, arg) =>
      List(expr(func), expr(arg)).foldEither(
        { varss => varss.reduce(_++_) },
        { case List(func, arg) => App(func, arg) }
      )
  }

  override def apply(source: Map[parse.Id, parse.Expr]) = {
    val defs = source.map{ case (id, e) => expr(e).fold(
      l => Left((id.name, l)), r => Right((Id(id.name), r))
    )}
    defs.foldEither(identity, identity) match {
      case Right(assocs) => CompileSuccess(Map(assocs: _*))
      case Left(errors) => {
        val msgs = errors.map{ case (id, vars) =>
          val msgVars = vars.map(_.exprString).mkString(", ")
          "In definition of " + id + ": " + msgVars
        }
        CompileFailure("Error: Unbound variable\n" + msgs.mkString("\n"))
      }
    }
  }
}

sealed trait Expr
case class Const(name: String) extends Expr
case class Id(name: String) extends Expr
case class App(func: Expr, arg: Expr) extends Expr
