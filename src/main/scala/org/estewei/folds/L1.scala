package org.estewei.folds

import scala.Unit
import scala.Predef.identity
import scalaz._

sealed abstract class L1[A, B] {
  type C
  val k: C => B
  val h: C => A => C
  val z: A => C
}

object L1 {
  import scalaz.std.function._
  import scalaz.std.tuple._
  import scalaz.syntax.bifunctor._

  def apply[A, B, _C](_k: _C => B, _h: _C => A => _C, _z: A => _C): L1[A, B] =
    new L1[A, B] { type C = _C; val k = _k; val h = _h; val z = _z }

  implicit val l1Instance: Scan[L1] with Arrow[L1] =
    new Scan[L1] with Arrow[L1] {

      def run1[A, B](a: A, p: L1[A, B]): B =
        p.k(p.z(a))

      def prefix1[A, B](a: A, p: L1[A, B]): L1[A, B] =
        L1(p.k, p.h, p.h(p.z(a)))

      def postfix1[A, B](p: L1[A, B], a: A): L1[A, B] =
        L1((c: p.C) => p.k(p.h(c)(a)), p.h, p.z)

      def interspersing[A, B](a: A, p: L1[A, B]): L1[A, B] =
        L1(p.k, (x: p.C) => b => p.h(p.h(x)(a))(b), p.z)

      def choice[A, B, C](f: => L1[A, C], g: => L1[B, C]): L1[A \/ B, C] = {
        lazy val p = f
        lazy val q = g

        L1[A \/ B, C, p.C \/ q.C](
          _.fold(a => p.k(a), b => q.k(b)),
          xy => ab => (xy, ab) match {
            case (-\/(x), -\/(a)) => -\/(p.h(x)(a))
            case (\/-(y), \/-(b)) => \/-(q.h(y)(b))
            case _                => xy
          },
          _.fold(x => -\/(p.z(x)), y => \/-(q.z(y))))
      }

      def arr[A, B](f: A => B): L1[A, B] =
        L1(f, (_: A) => a => a, identity)

      def first[A, B, C](f: L1[A, B]): L1[(A, C), (B, C)] =
        L1[(A, C), (B, C), (f.C, C)](
          _.leftMap(f.k),
          c => a => a.leftMap(f.h(c._1)),
          _.leftMap(f.z))

      def id[A]: L1[A, A] =
        arr(identity)

      def compose[A, B, C](f: L1[B, C], g: L1[A, B]): L1[A, C] =
        L1[A, C, (f.C, g.C)](
          cc => f.k(cc._1),
          cc => a => { val y = g.h(cc._2)(a); (f.h(cc._1)(g.k(y)), y) },
          a => { val y = g.z(a); (f.z(g.k(y)), y) }
        )
    }

  implicit def l1Monad[A]: Monad[({type λ[α] = L1[A, α]})#λ] =
    new Monad[({type λ[α] = L1[A, α]})#λ] {
      override def map[B, C](fb: L1[A, B])(f: B => C) =
        L1[A, C, fb.C](f compose fb.k, fb.h, fb.z)

      def point[B](b: => B): L1[A, B] =
        L1[A, B, Unit](_ => b, _ => _ => (), _ => ())

      def bind[B, C](fb: L1[A, B])(f: B => L1[A, C]): L1[A, C] =
        L1[A, C, (OneAnd[DList, A], B)](
          t => walk(t._1)(f(t._2)),
          t => a => (OneAnd(t._1.head, t._1.tail :+ a), t._2),
          a => (OneAnd(a, DList[A]()), fb.k(fb.z(a))))
    }

  ////

  private def walk[A, B](xs: OneAnd[DList, A])(l1: L1[A, B]): B =
    l1.k(xs.tail.foldr(l1.z(xs.head))((a, c) => l1.h(c)(a)))

}
