import org.scalatest._
import enlambda.compile.parse._

class TestElimination extends FunSuite {
  val i = Const("i")
  val k = Const("k")
  val s = Const("s")

  val x = Var('x')
  val y = Var('y')

  test("Id") {
    assert(Id("<a>").eliminate(x) == App(k, Id("<a>")))
  }

  test("Consts") {
    val fs = Seq(i, k, s, Const(".x"))
    for (f <- fs)
      assert(f.eliminate(x) == App(k, f))
  }

  test("Same variable") {
    assert(x.eliminate(x) == i)
  }

  test("Different variable") {
    assert(x.eliminate(y) == App(k, x))
  }

  test("Application") {
    val expr = App(i, k)
    val expected = App(App(s, App(k,i)), App(k,k))
    assert(expr.eliminate(x) == expected)
  }

  test("Nested abstraction") {
    val expr = Abs(y, App(x, y))
    // ``s(``s`ks``s`kki)`ki
    val expected = App(App(s, App(App(s, App(k,s)), App(App(s, App(k,k)), i))),
                       App(k,i))
    assert(expr.eliminate(x) == expected)
  }
}
