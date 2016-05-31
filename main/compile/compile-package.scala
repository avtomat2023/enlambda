package enlambda

package object compile {
  type CompileResult[T] = Either[String, T]
  type CompileSuccess[T] = Right[String, T]
  type CompileFailure = Left[String, Nothing]
}
