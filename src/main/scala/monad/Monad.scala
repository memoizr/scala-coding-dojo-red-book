package monad

trait Functor[T[_]] {
  def map[A, B](t: T[A])(transform: A => B): T[B]
}

object Functor {
  implicit def optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](t: Option[A])(transform: (A) => B): Option[B] = t match {
      case Some(a) => Some(transform(a))
      case None => None
    }
  }
}

trait Monad[F[_]] extends Functor[F] {
  def join[A](someOption: F[F[A]]): F[A]

  def unit[A](t: A): F[A]

  def flatMap[A, B](functor: F[A])(transform: A => F[B])
                   (implicit functorInstance: Functor[F]): F[B] = join(functorInstance.map(functor)(transform))
}

object Monad {
  implicit def optionIntMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: A): Option[A] = Some(a)

    override def map[A, B](t: Option[A])(transform: (A) => B): Option[B] = t match {
      case Some(a) => Some(transform(a))
      case None => None
    }

    override def join[A](someOption: Option[Option[A]]): Option[A] = someOption match {
      case Some(a) => a
      case None => None
    }
  }
}
