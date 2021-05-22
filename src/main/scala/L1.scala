import CommutativeRing.CommutativeRingOps
import org.scalacheck.{Arbitrary, Shrink}

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

object L1 {
  def main(args: Array[String]): Unit = {
    implicit def gops[A](a: A)(implicit g: CommutativeRing[A]) = {
      new CommutativeRingOps[A](a)
    }

    class CommutativeRingLaws[A: CommutativeRing: Arbitrary: Shrink](name: String) {
      def laws = {
        println(s"Checking ${name}")
        import org.scalacheck.Prop.forAll

        val sum = forAll { i: Int => i ++ i ==  Some(2 * i) }
        sum.check()

        val mult = forAll { i: Int => i ** i ==  Some(i * i) }
        mult.check()

        val invCombine = forAll { i: Int => i.inv ++ i == Some(0) }
        invCombine.check()

        val assocForSum = forAll { (a: Int, b: Int, c: Int) =>
          (a ++ b).flatMap(_ ++ c) == (b ++ c).flatMap(a ++ _)
        }
        assocForSum.check()

        val comm = forAll { (i: Int, j: Int) => i ++ j ==  Some(j + i) }
        comm.check()

        val zero = forAll { i: Int => i.zero ++ i ==  Some(i) }
        zero.check()

        val assocForMult = forAll { (a: Int, b: Int, c: Int) =>
          (a ** b).flatMap(_ ** c) == (b ** c).flatMap(a ** _)
        }
        assocForMult.check()

        val one = forAll { i: Int => i.one ** i == Some(i) }
        one.check()

        val distrib = forAll { (a: Int, b: Int, c: Int) =>
          (b ++ c).flatMap(a ** _) == ((a ** b) ++ (a**c))
        }
        assocForMult.check()

        println(s"Checking ${name} done")
      }
    }

    new CommutativeRingLaws[Int]("int").laws
  }
}