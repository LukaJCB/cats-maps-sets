/*
 * Copyright (c) 2018 Luka Jacobowitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.collection.tests

import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.tests.CatsSuite
import cats.collection._
import cats.kernel.laws.discipline._
import cats._
import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Cogen, Gen}

import scala.collection.immutable.{SortedMap, SortedSet}

class NonEmptyMapSuite extends CatsSuite {

  implicit def arbNonEmptyMap[K: Order, A](implicit A: Arbitrary[A], K: Arbitrary[K]): Arbitrary[NonEmptyMap[K, A]] =
    Arbitrary(for {
      fa <- implicitly[Arbitrary[SortedMap[K, A]]].arbitrary
      k <- K.arbitrary
      a <- A.arbitrary
    } yield NonEmptyMap((k, a), fa))

  implicit def cogenNonEmptyMap[K: Order: Cogen, A: Order: Cogen]: Cogen[NonEmptyMap[K, A]] =
    Cogen[SortedMap[K, A]].contramap(_.toMap)

  implicit def isoNonEmptyMap[K: Order]: Isomorphisms[NonEmptyMap[K, ?]] = Isomorphisms.invariant[NonEmptyMap[K, ?]]

  checkAll("NonEmptyMap[String, Int]", SemigroupKTests[NonEmptyMap[String, ?]].semigroupK[Int])
  checkAll("NonEmptyMap[String, Int]", NonEmptyTraverseTests[NonEmptyMap[String, ?]].nonEmptyTraverse[Option, Int, Int, Double, Int, Option, Option])
  checkAll("NonEmptyMap[String, Int]", BandTests[NonEmptyMap[String, Int]].band)
  checkAll("NonEmptyMap[String, Int]", EqTests[NonEmptyMap[String, Int]].eqv)

  test("Show is not empty and is formatted as expected") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      nem.show.nonEmpty should === (true)
      nem.show.startsWith("NonEmptySortedMap(") should === (true)
      nem.show should === (implicitly[Show[NonEmptyMap[String, Int]]].show(nem))
      nem.show.contains(nem.head._2.show) should === (true)
    }
  }

  test("Show is formatted correctly") {
    val nonEmptyMap = NonEmptyMap("Key" -> "Test", SortedMap.empty[String, String])
    nonEmptyMap.show should === ("NonEmptySortedMap(Key -> Test)")
  }

  test("NonEmptyMap#filter is consistent with Map#filter") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toMap
      nem.filter(p) should === (map.filter(t => p(t._2)))
    }
  }

  test("NonEmptyMap#filterNot is consistent with Map#filterNot") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toMap
      nem.filterNot(p) should === (map.filterNot(t => p(t._2)))
    }
  }

  test("NonEmptyMap#find is consistent with Map#find") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toMap
      nem.find(p).map(_._2) should === (map.find(p))
    }
  }

  test("NonEmptyMap#exists is consistent with Map#exists") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toMap
      nem.exists(p) should === (map.exists(p))
    }
  }

  test("NonEmptyMap#forall is consistent with Map#forall") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => Boolean) =>
      val map = nem.toMap
      nem.forall(p) should === (map.forall(p))
    }
  }

  test("NonEmptyMap#map is consistent with Map#map") {
    forAll { (nem: NonEmptyMap[String, Int], p: Int => String) =>
      val map = nem.toMap
      nem.map(p).toMap should === (map.fmap(p))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nem: NonEmptyMap[String, Int], f: (Int, Int) => Int) =>
      nem.reduceLeft(f) should === (Foldable[SortedMap[String, ?]].foldLeft(nem.tail, nem.head._2)(f))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nem: NonEmptyMap[String, Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nem.reduceRight(f).value
      val last = nem.last
      val rev = nem - last._1
      val expected = Foldable[SortedMap[String, ?]]
        .foldRight(rev, Now(last._2))((a, b) => f(a, b))
        .value
      got should === (expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nem: NonEmptyMap[String, Int]) =>
      nem.reduce should === (nem.fold)
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nem: NonEmptyMap[String, Option[Int]]) =>
      nem.reduce(SemigroupK[Option].algebra[Int]) should === (nem.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nem.tail.foldLeft(Option(f(nem.head._2))) { (opt, i) =>
        opt.map(s => g(s, i._2))
      }
      nem.reduceLeftToOption(f)(g) should === (expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val got = nem.reduceRightToOption(f)(g).value
      val last = nem.last
      val rev = nem - last._1
      val expected = rev.foldRight(Option(f(last._2))) { (i, opt) =>
        opt.map(s => g(i._2, Now(s)).value)
      }
      got should === (expected)
    }
  }

  test("reduceLeftM consistent with foldM") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => Option[Int]) =>
      val got = nem.reduceLeftM(f)((acc, i) => f(i).map(acc + _))
      val expected = f(nem.head._2).flatMap { hd =>
        nem.tail.foldM(hd)((acc, i) => f(i).map(acc + _))
      }
      got should === (expected)
    }
  }

  test("reduceMapM consistent with foldMapM") {
    forAll { (nem: NonEmptyMap[String, Int], f: Int => Option[Int]) =>
      nem.reduceMapM(f) should === (nem.foldMapM(f))
    }
  }

  test("fromMap round trip") {
    forAll { l: SortedMap[String, Int] =>
      NonEmptyMap.fromMap(l).map(_.toMap).getOrElse(SortedMap.empty[String, Int]) should === (l)
    }

    forAll { nem: NonEmptyMap[String, Int] =>
      NonEmptyMap.fromMap(nem.toMap) should === (Some(nem))
    }
  }

  test("fromMapUnsafe/fromMap consistency") {
    forAll { nem: NonEmptyMap[String, Int] =>
      NonEmptyMap.fromMap(nem.toMap) should === (Some(NonEmptyMap.fromMapUnsafe(nem.toMap)))
    }
  }

  test("fromMapUnsafe empty map") {
    val _ = intercept[IllegalArgumentException] {
      NonEmptyMap.fromMapUnsafe(SortedMap.empty[String, Int])
    }
  }

  test("+ consistent with Map") {
    forAll { (nem: NonEmptyMap[String, Int], i: (String, Int)) =>
      (nem + i).toMap should === (nem.toMap + i)
    }
  }

  test("NonEmptyMap#size and length is consistent with Map#size") {
    forAll { nem: NonEmptyMap[String, Int] =>
      nem.size should === (nem.toMap.size)
      nem.length should === (nem.toMap.size)
    }
  }

}
