package org.estewei.folds

import scalaz._
import scala.{Predef => P, Unit}

import scalaz.std.tuple._
import scalaz.std.function._
import scalaz.syntax.bifunctor._
import scalaz.syntax.arrow._

sealed abstract class L1[A, B] {
  type C

  val k: C => B
  val h: C => A => C
  val z: A => C

  def run1(a: A): B =
    k(z(a))

  def map[D](f: B => D): L1[A, D] =
    L1[A, D, C](f compose k, h, z)

  def ap[D](f: L1[A, B => D]): L1[A, D] =
    L1[A, D, (f.C, C)](
      cc => f.k(cc._1)(k(cc._2)),
      cc => a => cc bimap (f.h(_)(a), h(_)(a)),
      f.z &&& z)

  def flatMap[D](f: B => L1[A, D]): L1[A, D] =
    L1[A, D, (OneAnd[DList, A], B)](
      t => f(t._2) walk t._1,
      t => a => (OneAnd(t._1.head, t._1.tail :+ a), t._2),
      a => (OneAnd(a, DList[A]()), run1(a)))

  def compose[D](g: L1[D, A]): L1[D, B] =
    L1[D, B, (C, g.C)](
      cc => k(cc._1),
      cc => a => { val y = g.h(cc._2)(a); (h(cc._1)(g.k(y)), y) },
      a => { val y = g.z(a); (z(g.k(y)), y) })

  private def walk(xs: OneAnd[DList, A]): B =
    k(xs.tail.foldr(z(xs.head))((a, c) => h(c)(a)))

}

object L1 {

  def apply[A, B, X](_k: X => B, _h: X => A => X, _z: A => X): L1[A, B] =
    new L1[A, B] { type C = X; val k = _k; val h = _h; val z = _z }

  implicit val l1Instance: Scan[L1] with Arrow[L1] with Choice[L1] =
    new Scan[L1] with Arrow[L1] with Choice[L1] {

      def run1[A, B](a: A, p: L1[A, B]): B =
        p run1 a

      def prefix1[A, B](a: A, p: L1[A, B]): L1[A, B] =
        L1(p.k, p.h, p.h(p.z(a)))

      def postfix1[A, B](p: L1[A, B], a: A): L1[A, B] =
        L1((c: p.C) => p.k(p.h(c)(a)), p.h, p.z)

      def interspersing[A, B](a: A, p: L1[A, B]): L1[A, B] =
        L1(p.k, (x: p.C) => b => p.h(p.h(x)(a))(b), p.z)

      def id[A]: L1[A, A] =
        arr(P.identity)

      def compose[A, B, C](f: L1[B, C], g: L1[A, B]): L1[A, C] =
        f compose g

      def arr[A, B](f: A => B): L1[A, B] =
        L1(f, (_: A) => a => a, P.identity)

      def first[A, B, C](f: L1[A, B]): L1[(A, C), (B, C)] =
        L1[(A, C), (B, C), (f.C, C)](f.k.first, c => f.h(c._1).first, f.z.first)

      override def second[A, B, C](f: L1[A, B]): L1[(C, A), (C, B)] =
        L1[(C, A), (C, B), (C, f.C)](f.k.second, c => f.h(c._2).second, f.z.second)

      override def split[A, B, C, D](f: L1[A, B], g: L1[C, D]): L1[(A, C), (B, D)] =
        L1(f.k *** g.k,
          (xy: (f.C, g.C)) => ac => (f.h(xy._1)(ac._1), g.h(xy._2)(ac._2)),
          f.z *** g.z)

      override def combine[A, B, C](f: L1[A, B], g: L1[A, C]): L1[A, (B, C)] =
        L1(f.k *** g.k,
          (xy: (f.C, g.C)) => a => (f.h(xy._1)(a), g.h(xy._2)(a)),
          f.z &&& g.z)

      override def dimap[A, B, C, D](p: L1[A, B])(f: C => A)(g: B => D): L1[C, D] =
        L1(g compose p.k, (x: p.C) => p.h(x) compose f, p.z compose f)

      override def mapfst[A, B, C](p: L1[A, B])(f: C => A): L1[C, B] =
        L1(p.k, (x: p.C) => p.h(x) compose f, p.z compose f)

      override def mapsnd[A, B, C](p: L1[A, B])(f: B => C): L1[A, C] =
        p map f

      override def left[A, B, C](l1: L1[A, B]): L1[(A \/ C), (B \/ C)] =
        L1[A \/ C, B \/ C, l1.C \/ C](
          _ leftMap l1.k,
          cc => ac => (cc, ac) match {
            case (-\/(lc), -\/(a)) => -\/(l1.h(lc)(a))
            case (\/-(c) ,      _) => \/-(c)
            case (_      , \/-(c)) => \/-(c)
          },
          _ leftMap l1.z)

      override def right[A, B, C](l1: L1[A, B]): L1[(C \/ A), (C \/ B)] =
        L1[C \/ A, C \/ B, C \/ l1.C](
          _ rightMap l1.k,
          cc => ac => (cc, ac) match {
            case (\/-(lc), \/-(a)) => \/-(l1.h(lc)(a))
            case (-\/(c) ,      _) => -\/(c)
            case (_      , -\/(c)) => -\/(c)
          },
          _ rightMap l1.z)

      def choice[A, B, C](f: => L1[A, C], g: => L1[B, C]): L1[A \/ B, C] = {
        lazy val x = f
        lazy val y = g

        L1[A \/ B, C, x.C \/ y.C](
          _ fold (a => x.k(a), b => y.k(b)),
          cc => ab => (cc, ab) match {
            case (-\/(fc), -\/(a)) => \/.left(x.h(fc)(a))
            case (\/-(gc), \/-(b)) => \/.right(y.h(gc)(b))
            case (-\/(fc), \/-(_)) => \/.left(fc)
            case (\/-(gc), -\/(_)) => \/.right(gc)
          },
          _ bimap (a => x.z(a), b => y.z(b)))
      }
    }

  implicit def l1Monad[A]: Monad[({type λ[α] = L1[A, α]})#λ] =
    new Monad[({type λ[α] = L1[A, α]})#λ] {
      override def map[B, C](fb: L1[A, B])(f: B => C) =
        fb map f

      def point[B](b: => B): L1[A, B] =
        L1[A, B, Unit](_ => b, _ => _ => (), _ => ())

      override def ap[B, C](fb: => L1[A, B])(f: => L1[A, B => C]): L1[A, C] =
        fb ap f

      def bind[B, C](fb: L1[A, B])(f: B => L1[A, C]): L1[A, C] =
        fb flatMap f
    }

}
