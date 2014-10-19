package org.estewei.foldz

import scala.Boolean
import scalaz.Foldable
import monocle.Fold

trait Folding[P[_, _]] extends Scan[P] {

  def prefixOf[S, A, B](f: Fold[S, A])(s: S, p: P[A, B]): P[A, B]

  def postfixOf[S, A, B](f: Fold[S, A])(p: P[A, B], s: S): P[A, B]

  def runOf[S, A, B](f: Fold[S, A])(s: S, p: P[A, B]): B

  def filtering[A, B](pred: A => Boolean)(p: P[A, B]): P[A, B]

  def prefix[T[_]: Foldable, A, B](ta: T[A], p: P[A, B]): P[A, B] =
    prefixOf(Fold[T, A])(ta, p)

  def postfix[T[_]: Foldable, A, B](p: P[A, B], ta: T[A]): P[A, B] =
    postfixOf(Fold[T, A])(p, ta)

  def run[T[_]: Foldable, A, B](ta: T[A], p: P[A, B]): B =
    runOf(Fold[T, A])(ta, p)

}
