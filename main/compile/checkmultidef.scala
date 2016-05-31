package enlambda.compile.checkmultidef

import enlambda.compile._
import enlambda.compile.parse.{Path => _, _}

object Path extends CompilerPath[Program, Map[Id, Expr]] {
  override def apply(source: Program) = {
    // FIXME: Definitionの出現順が失われる
    // ASTにソースコード情報を持たせる必要がありそう
    val dups = source.defs
      .groupBy(_.lhs).mapValues(_.size)
      .filter{ case (_,n) => n > 1 }

    if (dups.isEmpty)
      CompileSuccess(Map(source.defs.map{ d => (d.lhs, d.rhs) }: _*))
    else {
      val msgs = dups.map { case (lhs, n) =>
        "Identifier " + lhs.name + " is defined " + n.toString + " times."
      }
      CompileFailure("Error: Multiple definition\n" + msgs.mkString("\n"))
    }

  }
}
