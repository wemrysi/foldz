package org.estewei.foldz

import scalaz._
import scalaz.std.tuple._
import scalaz.std.function._
import scalaz.std.vector._
import scalaz.syntax.bifunctor._
import scalaz.syntax.arrow._

import internal.Tree1

sealed abstract class M1[A, B] {
  type M

  val k: M => B
  val h: A => M
  val m: M => (=> M) => M

  def run1(a: A): B =
    k(h(a))

  def map[C](f: B => C): M1[A, C] =
    M1(f compose k, h, m)

  def ap[C](f: M1[A, B => C]): M1[A, C] =
    M1[A, C, (f.M, M)](
      xy => f.k(xy._1)(k(xy._2)),
      f.h &&& h,
      xy1 => xy2 => { lazy val z = xy2; (f.m(xy1._1)(z._1), m(xy1._2)(z._2)) })

  def compose[C](g: M1[C, A]): M1[C, B] =
    M1[C, B, (M, g.M)](
      xy => k(xy._1),
      c => { val y = g.h(c); (h(g.k(y)), y) },
      xy1 => xy2 => { lazy val z = xy2; (m(xy1._1)(z._1), g.m(xy1._2)(z._2)) })

}

object M1 {

  def apply[A, B, X](_k: X => B, _h: A => X, _m: X => (=> X) => X): M1[A, B] =
    new M1[A, B] { type M = X; val k = _k; val h = _h; val m = _m }

  def fromSemigroup[A, B, C](_k: C => B, _h: A => C)(implicit C: Semigroup[C]): M1[A, B] =
    apply[A, B, C](_k, _h, x => y => C.append(x, y))

}
