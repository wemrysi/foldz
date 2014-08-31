package org.estewei.foldz

import scalaz._
import scala.{Predef => P, Unit}

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
      a => c => { val (xs, b) = c; (OneAnd(a, xs.head :: xs.tail), b) },
      a => (OneAnd(a, IList.empty), run1(a)))

  def compose[D](g: R1[D, A]): R1[D, B] =
    R1[D, B, (C, g.C)](
      cc => k(cc._1),
      d => cc => { val (c, gc) = cc; val y = g.h(d)(gc); (h(g.k(y))(c), y) },
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

  implicit val r1Instance: Scan[R1] with Arrow[R1] with Choice[R1] =
    new Scan[R1] with Arrow[R1] with Choice[R1] {
      def run1[A, B](a: A, p: R1[A, B]): B =
        p run1 a

      def prefix1[A, B](a: A, p: R1[A, B]): R1[A, B] =
        R1((c: p.C) => p.k(p.h(a)(c)), p.h, p.z)

      def postfix1[A, B](p: R1[A, B], a: A): R1[A, B] =
        R1(p.k, p.h, a0 => p.h(a0)(p.z(a)))

      def interspersing[A, B](a: A, p: R1[A, B]) = {
        val f: A => (=> p.C) => p.C = b => c => p.h(b)(p.h(a)(c))
        R1(p.k, f, p.z)
      }

      def id[A]: R1[A, A] =
        arr(P.identity)

      def compose[A, B, C](f: R1[B, C], g: R1[A, B]): R1[A, C] =
        f compose g

      def arr[A, B](f: A => B): R1[A, B] = {
        val g: A => (=> A) => A = a => _ => a
        R1(f, g, P.identity)
      }

      def first[A, B, C](f: R1[A, B]): R1[(A, C), (B, C)] = {
        val h: ((A, C)) => (=> (f.C, C)) => (f.C, C) =
          a => c => { val (x, y) = c; (f.h(a._1)(x), y) }

        R1(f.k.first[C], h, f.z.first[C])
      }

      override def second[A, B, C](f: R1[A, B]): R1[(C, A), (C, B)] = {
        val h: ((C, A)) => (=> (C, f.C)) => (C, f.C) =
          a => c => { val (x, y) = c; (x, f.h(a._2)(y)) }

        R1(f.k.second[C], h, f.z.second[C])
      }

      override def split[A, B, C, D](f: R1[A, B], g: R1[C, D]): R1[(A, C), (B, D)] = {
        val h: ((A, C)) => (=> (f.C, g.C)) => (f.C, g.C) =
          a => c => { val (x, y) = c; (f.h(a._1)(x), g.h(a._2)(y)) }

        R1(f.k *** g.k, h, f.z *** g.z)
      }

      override def combine[A, B, C](f: R1[A, B], g: R1[A, C]): R1[A, (B, C)] = {
        val h: A => (=> (f.C, g.C)) => (f.C, g.C) =
          a => c => { val (x, y) = c; (f.h(a)(x), g.h(a)(y)) }

        R1(f.k *** g.k, h, f.z &&& g.z)
      }

      override def dimap[A, B, C, D](p: R1[A, B])(f: C => A)(g: B => D): R1[C, D] =
        R1(g compose p.k, p.h compose f, p.z compose f)

      override def mapfst[A, B, C](p: R1[A, B])(f: C => A): R1[C, B] =
        R1(p.k, p.h compose f, p.z compose f)

      override def mapsnd[A, B, C](p: R1[A, B])(f: B => C): R1[A, C] =
        p map f

      override def left[A, B, C](p: R1[A, B]): R1[(A \/ C), (B \/ C)] =
        R1[A \/ C, B \/ C, p.C \/ C](
          _ leftMap p.k,
          a => cc => (a, cc) match {
            case (-\/(x), -\/(y)) => -\/(p.h(x)(y))
            case (\/-(c),      _) => \/-(c)
            case (_     , \/-(c)) => \/-(c)
          },
          _ leftMap p.z)

      override def right[A, B, C](p: R1[A, B]): R1[(C \/ A), (C \/ B)] =
        R1[C \/ A, C \/ B, C \/ p.C](
          _ rightMap p.k,
          a => cc => (a, cc) match {
            case (\/-(x), \/-(y)) => \/-(p.h(x)(y))
            case (-\/(c),      _) => -\/(c)
            case (_     , -\/(c)) => -\/(c)
          },
          _ rightMap p.z)

      def choice[A, B, C](f: => R1[A, C], g: => R1[B, C]): R1[A \/ B, C] = {
        lazy val x = f
        lazy val y = g

        R1[A \/ B, C, x.C \/ y.C](
          _ fold (a => x.k(a), b => y.k(b)),
          ab => cc => (ab, cc) match {
            case (-\/(a), -\/(fc)) => \/.left(x.h(a)(fc))
            case (\/-(b), \/-(gc)) => \/.right(y.h(b)(gc))
            case (\/-(_), -\/(fc)) => \/.left(fc)
            case (-\/(_), \/-(gc)) => \/.right(gc)
          },
          _ bimap (a => x.z(a), b => y.z(b)))
      }
    }

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
