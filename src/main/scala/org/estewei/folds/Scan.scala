package org.estewei.folds

trait Scan[P[_, _]] extends Costrong[P] {

  def prefix1[A, B](a: A, p: P[A, B]): P[A, B]

  def postfix1[A, B](p: P[A, B], a: A): P[A, B]

  def run1[A, B](a: A, p: P[A, B]): B

  def interspersing[A, B](a: A, p: P[A, B]): P[A, B]

}
