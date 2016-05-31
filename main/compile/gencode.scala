package enlambda.compile.gencode

import scala.collection.mutable.ArrayBuffer
import enlambda.compile._

object Path extends CompilerPath[subs.Expr, Seq[Byte]] {
  def write(buffer: ArrayBuffer[Byte], expr: subs.Expr): Unit = expr match {
    case subs.Const(name) => buffer ++= name.getBytes
    case subs.App(func, arg) => {
      buffer += '`'.toByte
      write(buffer, func)
      write(buffer, arg)
    }
  }

  override def apply(source: subs.Expr) = {
    val buffer = ArrayBuffer[Byte]()
    write(buffer, source)
    CompileSuccess(buffer)
  }
}
