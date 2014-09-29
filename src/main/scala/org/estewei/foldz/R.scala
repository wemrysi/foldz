package org.estewei.foldz

import scalaz._
import scala.{Predef => P}
import monocle._

import scalaz.std.tuple._
import scalaz.syntax.bifunctor._

sealed abstract class R[A, B] {
  type X

  val k: X => B
  val h: A => (=> X) => X
  val z: X

  def run[T[_]](ta: T[A])(implicit T: Foldable[T]): B =
    k(T.foldr(ta, z)(h))

  def runOf[S](l: Fold[S, A], s: S): B =
    k(l.foldrOf(z)((a, c) => h(a)(c))(s))

  def map[C](f: B => C): R[A, C] =
    R(z)(f compose k, h)

  def ap[C](f: R[A, B => C]): R[A, C] =
    R((f.z, z))(
      xy => f.k(xy._1)(k(xy._2)),
      a => xy => xy bimap (f.h(a)(_), h(a)(_)))

  def flatMap[C](f: B => R[A, C]): R[A, C] =
    ap(R(EphemeralStream[A])(
      xs => f andThen (_ run xs),
      a => xs => a ##:: xs))

  def extend[C](f: R[A, B] => C): R[A, C] =
    R(z)(f compose (R(_)(k, h)), h)

  def duplicate: R[A, R[A, B]] =
    extend(P.identity)

}

object R {

  def apply[A, B, C](_z: C)(_k: C => B, _h: A => (=> C) => C): R[A, B] =
    new R[A, B] { type X = C; val k = _k; val h = _h; val z = _z }

}
