import org.scalatest._
import enlambda.compile.{parse => p}
import enlambda.compile.unabs._

class TestUnabs extends FunSuite {
  val i = Const("i")
  val k = Const("k")
  val s = Const("s")

  val pi = p.Const("i")
  val pk = p.Const("k")
  val ps = p.Const("s")

  val x = p.Var('x')
  val y = p.Var('y')

  test("Consts") {
  }

  test("Id") {
    assert(Path.expr(p.Id("<a>")) == Right(Id("<a>")))
  }

  test("Variable") {
    assert(Path.expr(x) == Left(Set(x)))
  }

  test("Abstractions") {
    assert(Path.expr(p.Abs(x, x)) == Right(i))
    assert(Path.expr(p.Abs(x, y)) == Left(Set(y)))
  }

  test("Applications") {
    assert(Path.expr(p.App(pi, pk)) == Right(App(i, k)))
    assert(Path.expr(p.App(pi, x)) == Left(Set(x)))
    assert(Path.expr(p.App(x, y)) == Left(Set(x, y)))
  }
}
