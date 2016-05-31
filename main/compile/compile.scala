package enlambda.compile

object CompileSuccess {
  def apply[T](x: T): CompileSuccess[T] = Right[String, T](x)
  def unapply[T](self: Right[_,T]) = Some(self.b)
}

object CompileFailure {
  def apply(msg: String): CompileFailure = Left[String, Nothing](msg)
  def unapply(self: Left[String,_]) = Some(self.a)
}

trait CompilerPath[Source, Result] {
  def apply(source: Source): CompileResult[Result]
}
