import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import CommutativeRingOps._

object CommutativeRingSpecification extends Properties("CommutativeRing") {
  property("invSum") = forAll { i: Int =>
    i.inv ++ i == Some(0)
  }

  property("assocForSum") = forAll { (a: Int, b: Int, c: Int) =>
    (a ++ b).flatMap(_ ++ c) == (b ++ c).flatMap(a ++ _)
  }

  property("commutative") = forAll { (i: Int, j: Int) =>
    i ++ j ==  Some(j + i)
  }

  property("zero") = forAll { i: Int =>
    i.zero ++ i ==  Some(i)
  }

  property("assocForMult") = forAll { (a: Int, b: Int, c: Int) =>
    (a ** b).flatMap(_ ** c) == (b ** c).flatMap(a ** _)
  }

  property("one") = forAll { i: Int =>
    i.one ** i == Some(i)
  }
}