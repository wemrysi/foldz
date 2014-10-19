package org.estewei.foldz

import scalaz._
import scala.{Predef => P, Function, Boolean, Vector}
import monocle.Fold

import scalaz.std.tuple._
import scalaz.std.vector._
import scalaz.syntax.bifunctor._

sealed abstract class L[A, B] {
  type R

  val k: R => B
  val h: R => A => R
  val z: R

  def run[T[_]](ta: T[A])(implicit T: Foldable[T]): B =
    k(T.foldl(ta, z)(h))

  def runOf[S](l: Fold[S, A], s: S): B =
    k(l.foldlOf(z)(Function.uncurried(h))(s))

  def map[C](f: B => C): L[A, C] =
    L(z)(f compose k, h)

  def ap[C](f: L[A, B => C]): L[A, C] =
    L((f.z, z))(
      xy => f.k(xy._1)(k(xy._2)),
      xy => a => xy bimap (f.h(_)(a), h(_)(a)))

  def flatMap[C](f: B => L[A, C]): L[A, C] =
    ap(L(Vector[A]())(
      xs => f andThen (_ run xs),
      xs => a => xs :+ a))

  def extend[C](f: L[A, B] => C): L[A, C] =
    L(z)(f compose (L(_)(k, h)), h)

  def duplicate: L[A, L[A, B]] =
    extend(P.identity)

}

object L {
  import scalaz.syntax.std.function2._

  def apply[A, B, X](_z: X)(_k: X => B, _h: X => A => X): L[A, B] =
    new L[A, B] { type R = X; val k = _k; val h = _h; val z = _z }

  def unfoldL[A, B, S](f: S => (B, A => S), s: S): L[A, B] =
    L(s)(f andThen (_._1), f andThen (_._2))

  implicit val lInstance: Folding[L] = new Folding[L] {
    def run1[A, B](a: A, p: L[A, B]): B =
      p.k(p.h(p.z)(a))

    def prefix1[A, B](a: A, p: L[A, B]): L[A, B] =
      run1(a, p.duplicate)

    def postfix1[A, B](p: L[A, B], a: A): L[A, B] =
      p.extend(run1(a, _))

    def interspersing[A, B](a: A, p: L[A, B]): L[A, B] =
      L(Maybe.empty[p.R])(
        _ cata (p.k, p.k(p.z)),
        mr => b => mr.cata(
          x => Maybe.just(p.h(p.h(x)(a))(b)),
               Maybe.just(p.h(p.z)(b))))

    def runOf[S, A, B](f: Fold[S, A])(s: S, p: L[A, B]): B =
      p runOf (f, s)

    override def run[T[_]: Foldable, A, B](ta: T[A], p: L[A, B]): B =
      p run ta

    def filtering[A, B](p: A => Boolean)(l: L[A, B]): L[A, B] =
      L(l.z)(l.k, r => a => if (p(a)) l.h(r)(a) else r)

    def prefixOf[S, A, B](f: Fold[S, A])(s: S, p: L[A, B]): L[A, B] =
      runOf(f)(s, p.duplicate)

    override def prefix[T[_]: Foldable, A, B](ta: T[A], p: L[A, B]): L[A, B] =
      run(ta, p.duplicate)

    def postfixOf[S, A, B](f: Fold[S, A])(p: L[A, B], s: S): L[A, B] =
      p.extend(runOf(f)(s, _))

    override def postfix[T[_]: Foldable, A, B](p: L[A, B], ta: T[A]): L[A, B] =
      p.extend(run(ta, _))

    override def dimap[A, B, C, D](l: L[A, B])(f: C => A)(g: B => D): L[C, D] =
      L(l.z)(g compose l.k, r => l.h(r) compose f)

    def mapfst[A, B, C](l: L[A, B])(f: C => A): L[C, B] =
      L(l.z)(l.k, r => l.h(r) compose f)

    def mapsnd[A, B, C](l: L[A, B])(f: B => C): L[A, C] =
      L(l.z)(f compose l.k, l.h)

    override def left[A, B, C](l: L[A, B]): L[A \/ C, B \/ C] =
      L[A \/ C, B \/ C, l.R \/ C](\/.left(l.z))(
        _ leftMap l.k,
        rc => ac => rc.fold(r => ac.fold(a => \/.left(l.h(r)(a)), \/.right), \/.right))

    override def right[A, B, C](l: L[A, B]): L[C \/ A, C \/ B] =
      L[C \/ A, C \/ B, C \/ l.R](\/.right(l.z))(
        _ rightMap l.k,
        cr => ca => cr.fold(\/.left, r => ca.fold(\/.left, a => \/.right(l.h(r)(a)))))
  }

  implicit def lMonadComonad[A]: Monad[L[A, ?]] with Comonad[L[A, ?]] with Zip[L[A, ?]] =
    new Monad[L[A, ?]] with Comonad[L[A, ?]] with Zip[L[A, ?]] {
      override def map[B, C](l: L[A, B])(f: B => C): L[A, C] =
        l map f

      def point[B](b: => B): L[A, B] =
        L(())(_ => b, _ => _ => ())

      override def ap[B, C](fa: => L[A, B])(f: => L[A, B => C]): L[A, C] =
        fa ap f

      def bind[B, C](l: L[A, B])(f: B => L[A, C]): L[A, C] =
        l flatMap f

      def copoint[B](l: L[A, B]): B =
        l.k(l.z)

      def cobind[B, C](l: L[A, B])(f: L[A, B] => C): L[A, C] =
        l extend f

      def zip[B, C](a: => L[A, B], b: => L[A, C]): L[A, (B, C)] =
        tuple2(a, b)
    }

}
