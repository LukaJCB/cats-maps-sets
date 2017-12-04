package cats.collection.tests

import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.tests.CatsSuite
import cats.collection._
import cats.kernel.laws.discipline._
import cats._
import org.scalacheck.{Arbitrary, Gen}
import scala.collection.immutable.SortedMap

class NonEmptyMapSuite extends CatsSuite {

  implicit def arbNonEmptyMap[K: Order, A](implicit A: Arbitrary[A], K: Arbitrary[K]): Arbitrary[NonEmptyMap[K, A]] =
    Arbitrary(for {
      fa <- implicitly[Arbitrary[SortedMap[K, A]]].arbitrary
      k <- K.arbitrary
      a <- A.arbitrary
    } yield NonEmptyMap((k, a), fa))


  checkAll("NonEmptyMap[String, Int]", SemigroupKTests[NonEmptyMap[String, ?]].semigroupK[Int])
  checkAll("NonEmptyMap[String, Int]", NonEmptyTraverseTests[NonEmptyMap[String, ?]].nonEmptyTraverse[Option, Int, String, Double, Int, Option, List])
  checkAll("NonEmptyMap[String, Int]", BandTests[NonEmptyMap[String, Int]].band)
}
