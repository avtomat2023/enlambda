package enlambda.compile.parse

import scala.util.parsing.combinator.RegexParsers
import enlambda.compile._

object Path extends RegexParsers with CompilerPath[String, Ast] {
  override protected val whiteSpace = "([ \t\n\r]|#.*)*".r

  def program: Parser[Program] = rep(definition) ^^ Program
  def definition: Parser[Def] = id ~ ":=" ~ expr ^^ {
    case id ~ _ ~ expr => Def(id, expr)
  }
  def expr: Parser[Expr] = variable | id | const | abs | app | parenExpr
  def parenExpr: Parser[Expr] = "(" ~> expr <~ ")"

  def variable: Parser[Var] = "$" ~> "[a-zA-Z0-9_]".r ^^ { s => Var(s.charAt(0)) }
  def id: Parser[Id] = "<[^>#()]+>".r ^^ Id

  def const: Parser[Const] = oneCharOp | twoCharOp
  def oneCharOp: Parser[Const] = "[ksivcde@|]".r  ^^ Const
  def twoCharOp: Parser[Const] = "[.?]".r ~ (oneChar | escapeSequence) ^^ {
    case s1 ~ s2 => Const(s1 + s2)
  }
  // バックスラッシュ以外の任意のASCII文字
  def oneChar: Parser[String] = acceptIf { c =>
    c != '\\' && c < 0x80
  }(e => s"Invalid character $e for '.x' or '?x' operator") ^^ { _.toString }
  def escapeSequence: Parser[String] = """\\[\\nr]""".r ^^ { s =>
    s.charAt(1) match {
      case '\\' => "\\"
      case 'n' => "\n"
      case 'r' => "\r"
    }
  }

  def abs: Parser[Abs] = "^" ~ "[a-zA-Z0-9_]".r ~ expr ^^ {
    case _ ~ variable ~ expr => Abs(Var(variable.charAt(0)), expr)
  }
  def app: Parser[App] = "`" ~ expr ~ expr ^^ {
    case _ ~ f ~ arg => App(f, arg)
  }

  override def apply(input: String): CompileResult[Program] =
    parseAll(program, input) match {
      case Success(result, _) => CompileSuccess(result)
      case n: NoSuccess => CompileFailure(n.toString)
    }
}


sealed trait Ast
case class Program(defs: Seq[Def]) extends Ast
case class Def(lhs: Id, rhs: Expr) extends Ast

sealed trait Expr extends Ast {
  // 自分自身をEだとすると、
  // ^x.E を再帰的に Abstraction Elimination した式を返す
  def eliminate(x: Var): Expr
}

case class Const(name: String) extends Expr {
  override def eliminate(x: Var) = App(Const("k"), this)
}

case class Id(name: String) extends Expr {
  override def eliminate(x: Var) = App(Const("k"), this)
}

case class Var(name: Char) extends Expr {
  override def eliminate(x: Var) =
    if (name == x.name) Const("i")
    else App(Const("k"), this)
  def exprString = "$" + name.toString
}

case class Abs(variable: Var, func: Expr) extends Expr {
  override def eliminate(x: Var) = func.eliminate(variable).eliminate(x)
}

case class App(func: Expr, arg: Expr) extends Expr {
  override def eliminate(x: Var) =
    App(App(Const("s"), func.eliminate(x)), arg.eliminate(x))
}
