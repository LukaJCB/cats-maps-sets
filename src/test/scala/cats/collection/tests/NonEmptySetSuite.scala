package cats.collection.tests

import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.tests.CatsSuite
import cats.collection._
import cats.kernel.laws.discipline.{BandTests, EqTests}
import cats._
import org.scalacheck.{Arbitrary, Gen}
import scala.collection.immutable.SortedSet

class NonEmptySetSuite extends CatsSuite {

  implicit def arbNonEmptySet[A: Order](implicit A: Arbitrary[A]): Arbitrary[NonEmptySet[A]] =
    Arbitrary(implicitly[Arbitrary[SortedSet[A]]].arbitrary.flatMap(fa =>
      A.arbitrary.map(a => NonEmptySet.of(a, fa.toList: _*))))


  checkAll("NonEmptySet[Int]", SemigroupKTests[NonEmptySet].semigroupK[Int])
  checkAll("NonEmptySet[Int]", ReducibleTests[NonEmptySet].reducible[Option, Int, String])
  checkAll("NonEmptySet[Int]", BandTests[NonEmptySet[Int]].band)
}
