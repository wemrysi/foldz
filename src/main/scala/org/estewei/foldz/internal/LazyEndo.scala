package org.estewei.foldz.internal

import scalaz._

final case class LazyEndo[A](run: (=> A) => A) {
  final def apply(a: => A): A =
    run(a)

  final def compose(other: LazyEndo[A]): LazyEndo[A] =
    LazyEndo(a => run(other.run(a)))

  final def andThen(other: LazyEndo[A]): LazyEndo[A] =
    other compose this
}

object LazyEndo {
  implicit def lazyEndoMonoid[A]: Monoid[LazyEndo[A]] =
    new Monoid[LazyEndo[A]] {
      def append(l1: LazyEndo[A], l2: => LazyEndo[A]) = l1 compose l2
      def zero = LazyEndo[A](a => a)
    }
}
