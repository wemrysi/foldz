package org.estewei.folds

import scala.Boolean
import scalaz.Foldable
import monocle.Fold

trait Folding[P[_, _]] extends Scan[P] {

  def prefixOf[S, A, B](f: Fold[S, A], s: S): P[A, B] => P[A, B]

  def postfixOf[S, A, B](f: Fold[S, A], p: P[A, B]): S => P[A, B]

  def runOf[S, A, B](f: Fold[S, A], s: S): P[A, B] => B

  def filtering[A, B](p: A => Boolean): P[A, B] => P[A, B]

  def prefix[T[_], A, B](ta: T[A])(implicit T: Foldable[T]): P[A, B] => P[A, B] =
    prefixOf(Fold[T, A], ta)

  def postfix[T[_], A, B](p: P[A, B])(implicit T: Foldable[T]): T[A] => P[A, B] =
    postfixOf(Fold[T, A], p)

  def run[T[_], A, B](ta: T[A])(implicit T: Foldable[T]): P[A, B] => B =
    runOf(Fold[T, A], ta)

}
