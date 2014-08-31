package org.estewei

import scalaz._
import monocle.Fold

package object foldz {
  import scalaz.syntax.std.function2._

  // TODO: Define for monocle.Fold
  def foldlOf[S, A, R](l: Fold[S, A], r: R)(f: (R, A) => R): S => R =
    s => Tag.unwrap(l.foldMap(s)(a => Dual(Endo.endo(f.flip.curried(a))))(Dual.dualMonoid[Endo[R]])).run(r)

}
