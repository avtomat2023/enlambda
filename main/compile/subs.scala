package enlambda.compile.subs

import scala.collection.mutable
import enlambda.compile._

object Substitutor {
  case class Failure(undefineds: Set[unabs.Id], recursives: Set[unabs.Id])
}

class Substitutor(val defs: Map[unabs.Id, unabs.Expr]) {
  import enlambda.util.Implicits._
  import Substitutor.Failure

  private val memo = mutable.Map[unabs.Id, Expr]()

  def go(expr: unabs.Expr, visited: Set[unabs.Id]):
      Either[Failure, Expr] = expr match {
    case const: unabs.Const => Right(Const(const.name))
    case id: unabs.Id => memo.get(id) match {
      case Some(expr) => Right(expr)
      case None => if (visited(id)) Left(Failure(Set(), Set(id)))
                   else defs.get(id) match {
                     case Some(expr) => {
                       val result = go(expr, visited + id)
                       result.right.foreach{ memo(id) = _ }
                       result
                     }
                     case None => Left(Failure(Set(id), Set()))
                   }
    }
    case unabs.App(func, arg) => {
      List(go(func, visited), go(arg, visited)).foldEither(
        { fs => Failure(
            fs.foldLeft(Set[unabs.Id]()){ (acc, f) => acc ++ f.undefineds },
            fs.foldLeft(Set[unabs.Id]()){ (acc, f) => acc ++ f.recursives }
        ) },
        { case List(func, arg) => App(func, arg) }
      )
    }
  }
}

object Path extends CompilerPath[Map[unabs.Id, unabs.Expr], Expr] {
  private val main = unabs.Id("<main>")

  def subsMain(mainExpr: unabs.Expr, substitutor: Substitutor)
      : CompileResult[Expr] = substitutor.go(mainExpr, Set(main)) match {
    case Right(expr) => CompileSuccess(expr)
    case Left(Substitutor.Failure(undefs, recs)) => {
      val errorUndef =
        if (undefs.isEmpty) None
        else Some("Error: Undefined identifier\n" +
                  undefs.map(_.name).mkString(", "))
      val errorRec =
        if (recs.isEmpty) None
        else Some("Error: Recursively defined identifier\n" +
                  recs.map(_.name).mkString(", "))
      val msg = List(errorUndef, errorRec).flatten.mkString("\n")
      CompileFailure(msg)
    }
  }

  override def apply(source: Map[unabs.Id, unabs.Expr]) = source.get(main) match {
    case Some(mainExpr) => subsMain(mainExpr, new Substitutor(source))
    case None => CompileFailure("Error: <main> not found.")
  }
}

sealed trait Expr
case class Const(name: String) extends Expr
case class App(func: Expr, arg: Expr) extends Expr
