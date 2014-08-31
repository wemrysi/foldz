package org.estewei.foldz

import scalaz._

// TODO: Replace from scalaz
trait ProChoice[=>:[_, _]] extends Profunctor[=>:] {

  def left[A, B, C](p: A =>: B): (A \/ C) =>: (B \/ C) =
    dimap[C \/ A, C \/ B, A \/ C, B \/ C](right(p))(_.fold(\/.right, \/.left))(_.fold(\/.right, \/.left))

  def right[A, B, C](p: A =>: B): (C \/ A) =>: (C \/ B) =
    dimap[A \/ C, B \/ C, C \/ A, C \/ B](left(p))(_.fold(\/.right, \/.left))(_.fold(\/.right, \/.left))

}
