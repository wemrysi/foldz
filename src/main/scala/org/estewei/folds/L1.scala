package org.estewei.folds

import scala.Predef.identity
import scalaz._

sealed abstract class L1[A, B] {
  type C

  val k: C => B
  val h: C => A => C
  val z: A => C

  def map[D](f: B => D): L1[A, D] =
    L1[A, D, C](f compose k, h, z)

}

object L1 {
  import scalaz.std.function._
  import scalaz.std.tuple._
  import scalaz.syntax.bifunctor._

  def apply[A, B, _C](_k: _C => B, _h: _C => A => _C, _z: A => _C) =
    new L1[A, B] {
      type C = _C
      val k = _k
      val h = _h
      val z = _z
    }

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

      // TODO: This could be lazier, is there a benefit to making it so?
      def choice[A, B, C](f: => L1[A, C], g: => L1[B, C]): L1[A \/ B, C] = {
        val p = f
        val q = g

        L1[A \/ B, C, p.C \/ q.C](
          _.fold(p.k, q.k),
          c => x => (c, x) match {
            case (-\/(pC), -\/(a)) => -\/(p.h(pC)(a))
            case (\/-(qC), \/-(b)) => \/-(q.h(qC)(b))
            case _                 => c
          },
          _.fold(p.z andThen (-\/(_)), q.z andThen (\/-(_))))
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
          cc => a => { val gC = g.h(cc._2)(a); (f.h(cc._1)(g.k(gC)), gC) },
          a => { val gC = g.z(a); (f.z(g.k(gC)), gC) }
        )
    }

}
