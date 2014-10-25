package org.estewei.foldz
package internal

import scala.annotation.tailrec
import scala.{Int, Boolean}
import scalaz._

sealed abstract class IStream[A] {
  import IStream.{cons => Cons, empty => Empty, singleton}

  private[internal] def head: A
  def tail: IStream[A]
  def isEmpty: Boolean

  def maybeHead: Maybe[A] =
    if (isEmpty) Maybe.empty[A] else Maybe.just(head)

  def foldr[B](z: => B)(f: (=> A) => (=> B) => B): B =
    if (isEmpty) z else f(head)(tail.foldr(z)(f))

  @tailrec
  def foldl[B](z: B)(f: B => (=> A) => B): B =
    if (isEmpty) z else tail.foldl(f(z)(head))(f)

  def take(n: Int): IStream[A] =
    if (isEmpty || n <= 0)
      Empty
    else
      Cons(head, tail.take(n - 1))

  @tailrec
  def drop(n: Int): IStream[A] =
    if (n <= 0) this else tail.drop(n - 1)

  def concat(other: => IStream[A]): IStream[A] =
    foldr(other)(h => t => Cons(h, t))

  def ++(other: => IStream[A]): IStream[A] =
    concat(other)

  def cons(a: => A): IStream[A] =
    Cons(a, this)

  def snoc(a: => A): IStream[A] =
    concat(singleton(a))

  def map[B](f: A => B): IStream[B] =
    foldr(Empty[B])(h => t => Cons(f(h), t))

  def flatMap[B](f: A => IStream[B]): IStream[B] =
    foldr(Empty[B])(h => t => f(h) ++ t)

}

object IStream {

  def empty[A]: IStream[A] =
    new IStream[A] {
      def head = scala.sys.error("unpossible!")
      def tail = this
      def isEmpty = true
    }

  def cons[A](h: => A, t: => IStream[A]): IStream[A] =
    new IStream[A] {
      def head = h
      def tail = t
      def isEmpty = false
    }

  def singleton[A](a: => A): IStream[A] =
    cons(a, empty)

  def unfold[A, B](z: B)(f: B => Maybe[(A, B)]): IStream[A] =
    f(z) match {
      case Maybe.Just((a, b)) => cons(a, unfold(b)(f))
      case Maybe.Empty()      => empty
    }

  def const[A](a: A): IStream[A] =
    cons(a, const(a))

  def fill[A](n: Int)(a: A): IStream[A] =
    const(a) take n

}
