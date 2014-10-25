package org.estewei.foldz
package internal

private[foldz] sealed abstract class Tree1[A] {
  /*
  def toStream: EphemeralStream[A] =
    this match {
      case Bin1(l, r) => l.toStream ++ r.toStream
      case Tip1(a)    =>
    }
  */
}

private[foldz] case class Bin1[A](l: Tree1[A], r: Tree1[A]) extends Tree1[A]
private[foldz] case class Tip1[A](a: A) extends Tree1[A]

