package org.estewei.folds

import scala.Boolean
import scalaz.{Profunctor, Foldable}
import monocle.{Fold, Prism}

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

  // Not sure if this is implementable as it seems like Monocle has specialized the p in Prism to (->)
//  def beneath[S, T, A, B](l: Prism[S, T, A, B])(implicit P: Profunctor[P]): P[A, B] => P[S, T]

}
