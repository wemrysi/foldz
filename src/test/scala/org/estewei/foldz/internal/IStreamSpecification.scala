package org.estewei.foldz
package internal

import scala._
import Predef._
import scalaz._, Scalaz._
import org.scalacheck._

object IStreamSpecification extends Properties("IStream") {
  import Prop._

  def const[A, B](a: A)(b: => B): A = a

  property("unfold") = forAll(Gen.choose(0, 10000)) { n =>
    val s = IStream.unfold(n)(x => (x > 0) ? Maybe.just((x, x - 1)) | Maybe.empty)
    s.foldl(0)(x => _ => x + 1) == n
  }

}
