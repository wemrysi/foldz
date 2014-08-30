package org.estewei.folds

import scala.Unit
import scalaz._
import scalaz.std.tuple._
import scalaz.std.function._
import scalaz.syntax.bifunctor._
import scalaz.syntax.arrow._

sealed abstract class R1[A, B] {
  type C

  val k: C => B
  val h: A => (=> C) => C
  val z: A => C

  def run1(a: A): B =
    k(z(a))

  def map[D](f: B => D): R1[A, D] =
    R1(f compose k, h, z)

  def ap[D](f: R1[A, B => D]): R1[A, D] =
    R1[A, D, (f.C, C)](
      cc => f.k(cc._1)(k(cc._2)),
      a => cc => cc bimap (f.h(a)(_), h(a)(_)),
      f.z &&& z)

  def flatMap[D](f: B => R1[A, D]): R1[A, D] =
    R1[A, D, (OneAnd[IList, A], B)](
      c => f(c._2) walk c._1,
      a => c => (OneAnd(a, c._1.head :: c._1.tail), c._2),
      a => (OneAnd(a, IList.empty), run1(a)))

  def compose[D](g: R1[D, A]): R1[D, B] =
    R1[D, B, (C, g.C)](
      cc => k(cc._1),
      d => cc => { val y = g.h(d)(cc._2); (h(g.k(y))(cc._1), y) },
      d => { val y = g.z(d); (z(g.k(y)), y) })

  private def walk(xs: OneAnd[IList, A]): B = {
    def go(xs0: OneAnd[IList, A]): C =
      xs0 match {
        case OneAnd(a, INil())         => z(a)
        case OneAnd(a0, ICons(a1, as)) => h(a0)(go(OneAnd(a1, as)))
      }

    k(go(xs))
  }

}

object R1 {

  def apply[A, B, X](_k: X => B, _h: A => (=> X) => X, _z: A => X): R1[A, B] =
    new R1[A, B] { type C = X; val k = _k; val h = _h; val z = _z }

  implicit def r1Monad[A]: Monad[({type λ[α] = R1[A, α]})#λ] with Zip[({type λ[α] = R1[A, α]})#λ] =
    new Monad[({type λ[α] = R1[A, α]})#λ] with Zip[({type λ[α] = R1[A, α]})#λ] {
      override def map[B, C](fb: R1[A, B])(f: B => C) =
        fb map f

      def point[B](b: => B): R1[A, B] =
        R1[A, B, Unit](_ => b, _ => _ => (), _ => ())
      
      override def ap[B, C](fb: => R1[A, B])(f: => R1[A, B => C]): R1[A, C] =
        fb ap f

      def bind[B, C](fb: R1[A, B])(f: B => R1[A, C]): R1[A, C] =
        fb flatMap f

      def zip[B, C](fb: => R1[A, B], fc: => R1[A, C]): R1[A, (B, C)] =
        tuple2(fb, fc)
    }

}
