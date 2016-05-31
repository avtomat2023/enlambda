package enlambda

import scala.io.Source
import enlambda.compile.{
  parse, checkmultidef, unabs, subs, gencode,
  CompileSuccess, CompileFailure
}

object Main extends App {
  parse.Path(Source.stdin.mkString)
    .right.flatMap(checkmultidef.Path(_))
    .right.flatMap(unabs.Path(_))
    .right.flatMap(subs.Path(_))
    .right.flatMap(gencode.Path(_)) match {
    case CompileSuccess(code) => System.out.write(code.toArray)
    case CompileFailure(msg) => {
      System.err.print(msg)
      System.exit(1)
    }
  }
}
