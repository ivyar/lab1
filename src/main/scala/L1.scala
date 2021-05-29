import CommutativeRing.CommutativeRingOps
import scala.language.implicitConversions

trait CommutativeRing[A] {
  def sum(a: A, b: A): Option[A]
  def mult(a: A, b: A): Option[A]
  def inverse(a: A): A
  def zero: A
  def one: A
}
object CommutativeRing {
  implicit val IntsFormCommutativeRing = new CommutativeRing[Int] {
    override def sum(a: Int, b: Int): Option[Int] = {
      Option(a.toLong + b.toLong)
        .map(x => x.toInt)
    }

    override def mult(a: Int, b: Int): Option[Int] = {
      Option(a.toLong * b.toLong)
        .map(x => x.toInt)
    }

    override def inverse(a: Int): Int = {
      -a
    }

    override def zero: Int = {
      0
    }

    override def one: Int = {
      1
    }
  }

  class CommutativeRingOps[A](a: A)(implicit g: CommutativeRing[A]) {
    def ++(b: A) = {
      g.sum(a, b)
    }

    def **(b: A) = {
      g.mult(a, b)
    }

    def inv = {
      g.inverse(a)
    }

    def zero = {
      g.zero
    }

    def one = {
      g.one
    }
  }
}

object CommutativeRingOps {
  implicit def commutativeRingSyntax[A](a: A)(implicit g: CommutativeRing[A]): CommutativeRingOps[A] = {
    new CommutativeRingOps[A](a)
  }
}