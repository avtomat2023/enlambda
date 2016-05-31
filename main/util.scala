package enlambda.util

object Implicits {
  import scala.collection.GenTraversableOnce

  implicit class RichEithers[A, B](val self: GenTraversableOnce[Either[A, B]])
      extends AnyVal {
    // ひとつでもLeftがあればfLeftsで、
    // すべてRightならfRightsで値をまとめて、Eitherを返す
    def foldEither[C, D](fLefts: Seq[A] => C, fRights: Seq[B] => D)
        : Either[C, D] = {
      val (ls, rs) = self.foldRight((List[A](), List[B]())) { (either, acc) =>
        either.fold(l => (l::acc._1, acc._2), r => (acc._1, r::acc._2))
      }
      if (ls.isEmpty) Right(fRights(rs)) else Left(fLefts(ls))
    }
  }
}
