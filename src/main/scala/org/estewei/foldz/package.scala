package org.estewei

import scala.AnyVal
import scalaz._
import monocle.Fold

package object foldz {
  import scalaz.syntax.std.function2._
  import foldz.internal.LazyEndo

  // TODO: Define for monocle.Fold
  implicit class FoldOps[S, A](val l: Fold[S, A]) extends AnyVal {

    def foldlOf[R](z: R)(f: (R, A) => R): S => R =
      s => Tag.unwrap(l.foldMap(s)(a => Dual(Endo.endo(f.flip.curried(a))))(Dual.dualMonoid[Endo[R]])).run(z)

    def foldrOf[R](z: R)(f: (A, => R) => R): S => R =
      s => l.foldMap(s)(a => LazyEndo(f.curried(a))).run(z)

  }

}
