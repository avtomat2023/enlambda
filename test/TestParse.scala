import org.scalatest._
import enlambda.compile.parse._

class TestParse extends FunSuite with Matchers {
  private val c = Const("c")
  private val k = Const("k")
  private val i = Const("i")
  private val v = Const("v")
  private val d = Const("d")

  test("One character builtin operators") {
    val funcs = Seq("k", "s", "i", "v", "c", "d", "e", "@", "|")
    val invalidFuncs = Seq("a", "b", "^", "`")
    for (f <- funcs)
      Path.parseAll(Path.const, f) should matchPattern {
        case Path.Success(Const(f), _) =>
      }
    for (f <- invalidFuncs)
      Path.parseAll(Path.const, f) should matchPattern {
        case Path.Failure(_, _) =>
      }
  }

  test("Two character builtin operators") {
    val ops = Seq(".", "?")
    val chars = Seq("a", "#", "^", "`")
    val escapes = Seq(("\\n", "\n"), ("\\r", "\r"), ("\\\\", "\\"))

    for (op <- ops; char <- chars) {
      val input = op + char
      Path.parseAll(Path.const, input) should matchPattern {
        case Path.Success(Const(`input`), _) =>
      }
    }

    for (op <- ops; (inChar, resultChar) <- escapes) {
      val input = op + inChar
      val result = op + resultChar
      Path.parseAll(Path.const, input) should matchPattern {
        case Path.Success(Const(`result`), _) =>
      }
    }

    Path.parseAll(Path.const, ".あ") should matchPattern {
      case Path.Failure(_, _) =>
    }
  }

  test("Identifiers") {
    val ids = Seq("<a>", "<Loop>", "<print \"ABC\">", "<0>")
    val invalidIds = Seq("<##abort##>", "<>", "<f(0)>")
    for (id <- ids)
      Path.parseAll(Path.id, id) should matchPattern {
        case Path.Success(Id(`id`), _) =>
      }
    for (id <- invalidIds)
      Path.parseAll(Path.id, id) should matchPattern {
        case Path.Failure(_, _) =>
      }
  }

  test("Variables") {
    val varNames = Seq('x', 'k', 'X', '0', '_')
    for (c <- varNames)
      Path.parseAll(Path.variable, "$" + c.toString) should matchPattern {
        case Path.Success(Var(`c`), _) =>
      }
  }

  test("Abstractions") {
    Path.parseAll(Path.abs, "^x$x") should matchPattern {
      case Path.Success(Abs(Var('x'), Var('x')), _) =>
    }
  }

  test("Applications") {
    Path.parseAll(Path.app, "`ki") should matchPattern {
      case Path.Success(App(Const("k"), Const("i")), _) =>
    }
  }

  test("Expressions with parentheses") {
    val vb = Var('b')
    val vf = Var('f')
    val vq = Var('q')
    val fst = Id("<fst>")
    val snd = Id("<snd>")
    val loop = Id("<loop>")
    val idn = Id("<N>")

    val inputsAndExpecteds = Seq(
      ("^f``$f<loop>`<N>$f", Abs(vf, App(App(vf, loop), App(idn, vf)))),
      ("^b `c^q`(`k<snd>)(``$b$q<fst>)",
       Abs(vb, App(c, Abs(vq, App(App(k, snd), App(App(vb, vq), fst))))))
    )

    val invalidInputs = Seq(
      "^b`c^q(``k)<snd>``$b$q<fst>"
    )

    for ((input, expr) <- inputsAndExpecteds)
      Path.parseAll(Path.expr, input) should matchPattern {
        case Path.Success(`expr`, _) =>
      }

    for (input <- invalidInputs)
      Path.parseAll(Path.expr, input) should matchPattern {
        case Path.Failure(_, _) =>
      }
  }

  test("Definitions with whitespaces") {
    val py = Const(".Y")
    val pe = Const(".E")
    val ps = Const(".S")
    val vb = Var('b')
    val snd = Id("<snd>")
    val not = Id("<not>")
    val idif = Id("<if>")
    val pyes = Id("<print \"YES\">")
    val inputsAndExpecteds = Seq(
      ("<snd> := `ki", Def(snd, App(k, i))),
      ("<not> := ^b```<if>$biv",
       Def(not, Abs(vb, App(App(App(idif, vb), i), v)))),
      ("<print \"YES\">:=`d``.Y.E.S",
       Def(pyes, App(d, App(App(py, pe), ps))))
    )

    for ((input, expr) <- inputsAndExpecteds) {
      Path.parseAll(Path.definition, input) should matchPattern {
        case Path.Success(`expr`, _) =>
      }
    }
  }

  test("Programs with comments") {
    val a1 = Id("<a1>")
    val b1 = Id("<b1>")
    val a2 = Id("<a2>")
    val b2 = Id("<b2>")
    val a3 = Id("<a3>")
    val b3 = Id("<b3>")
    val a4 = Id("<a4>")
    val b4 = Id("<b4>")
    val a5 = Id("<a5>")
    val inputsAndExpecteds = Seq(
      ("<a1>:=i<b1>:=k", Program(Seq(Def(a1,i), Def(b1,k)))),
      ("<a2> := i\n<b2> := `v<a2>", Program(Seq(Def(a2,i), Def(b2,App(v,a2))))),
      ("<a3> := i# def of <a3>\n  #def of <b3>\n  <b3> := `v<a3>  ",
        Program(Seq(Def(a3,i), Def(b3,App(v,a3))))),
      ("<a4> := i\n<b4> := `v<a4>  \n# program end",
        Program(Seq(Def(a4,i), Def(b4,App(v,a4))))),
      // 構文解析の時点では弾かない
      ("<a5> := <a5>", Program(Seq(Def(a5,a5)))),
      (" \n ", Program(Seq()))
    )

    for ((input, expr) <- inputsAndExpecteds)
      Path.parseAll(Path.program, input) should matchPattern {
        case Path.Success(`expr`, _) =>
      }
  }
}
