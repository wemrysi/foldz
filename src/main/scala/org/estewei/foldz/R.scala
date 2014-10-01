package org.estewei.foldz

import scalaz._
import scala.{Predef => P, Boolean}
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

  implicit val rInstance: Folding[R] = new Folding[R] {
    def run1[A, B](a: A, p: R[A, B]): B =
      p.k(p.h(a)(p.z))

    def prefix1[A, B](a: A, p: R[A, B]): R[A, B] =
      p.extend(run1(a, _))

    def postfix1[A, B](p: R[A, B], a: A): R[A, B] =
      run1(a, p.duplicate)

    def interspersing[A, B](a: A, p: R[A, B]): R[A, B] =
      R(Maybe.empty[p.X])(
        _ cata (p.k, p.k(p.z)),
        b => mr => mr.cata(
          x => Maybe.just(p.h(b)(p.h(a)(x))),
               Maybe.just(p.h(b)(p.z))))

    def runOf[S, A, B](f: Fold[S, A], s: S): R[A, B] => B =
      _ runOf (f, s)

    override def run[T[_]: Foldable, A, B](ta: T[A]): R[A, B] => B =
      _ run ta

    def filtering[A, B](p: A => Boolean): R[A, B] => R[A, B] =
      r => R(r.z)(r.k, a => x => if (p(a)) r.h(a)(x) else x)

    def prefixOf[S, A, B](f: Fold[S, A], s: S): R[A, B] => R[A, B] =
      _ extend runOf(f, s)

    override def prefix[T[_]: Foldable, A, B](ta: T[A]): R[A, B] => R[A, B] =
      _ extend run(ta)

    def postfixOf[S, A, B](f: Fold[S, A], p: R[A, B]): S => R[A, B] =
      runOf(f, _)(p.duplicate)

    override def postfix[T[_]: Foldable, A, B](p: R[A, B]): T[A] => R[A, B] =
      run(_) apply p.duplicate

    override def dimap[A, B, C, D](r: R[A, B])(f: C => A)(g: B => D): R[C, D] =
      R(r.z)(g compose r.k, r.h compose f)

    def mapfst[A, B, C](r: R[A, B])(f: C => A): R[C, B] =
      R(r.z)(r.k, r.h compose f)

    def mapsnd[A, B, C](r: R[A, B])(f: B => C): R[A, C] =
      R(r.z)(f compose r.k, r.h)

    override def left[A, B, C](r: R[A, B]): R[A \/ C, B \/ C] =
      R[A \/ C, B \/ C, r.X \/ C](\/.left(r.z))(
        _ leftMap r.k,
        ac => xc => ac.fold(a => xc.fold(x => \/.left(r.h(a)(x)), \/.right), \/.right))

    override def right[A, B, C](r: R[A, B]): R[C \/ A, C \/ B] =
      R[C \/ A, C \/ B, C \/ r.X](\/.right(r.z))(
        _ rightMap r.k,
        ca => cx => ca.fold(\/.left, a => cx.fold(\/.left, x => \/.right(r.h(a)(x)))))
  }

  implicit def rMonadComonad[A]: Monad[R[A, ?]] with Comonad[R[A, ?]] with Zip[R[A, ?]] =
    new Monad[R[A, ?]] with Comonad[R[A, ?]] with Zip[R[A, ?]] {
      override def map[B, C](r: R[A, B])(f: B => C): R[A, C] =
        r map f

      def point[B](b: => B): R[A, B] =
        R(())(_ => b, _ => _ => ())

      override def ap[B, C](fa: => R[A, B])(f: => R[A, B => C]): R[A, C] =
        fa ap f

      def bind[B, C](r: R[A, B])(f: B => R[A, C]): R[A, C] =
        r flatMap f

      def copoint[B](r: R[A, B]): B =
        r.k(r.z)

      def cobind[B, C](r: R[A, B])(f: R[A, B] => C): R[A, C] =
        r extend f

      def zip[B, C](a: => R[A, B], b: => R[A, C]): R[A, (B, C)] =
        tuple2(a, b)
    }
}
