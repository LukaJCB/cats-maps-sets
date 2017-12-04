package cats.collection

import cats.data._
import cats.instances.sortedMap._
import cats.kernel._
import cats.{Always, Apply, Eval, FlatMap, Foldable, Functor, Later, Monad, NonEmptyTraverse, Now, Reducible, SemigroupK, Show}

import scala.collection.immutable._

final class NonEmptyMap[K, A] private(val m: SortedMap[K, A]) extends AnyVal {

  private implicit def ordering: Ordering[K] = m.ordering
  private implicit def order: Order[K] = Order.fromOrdering

  def ++(as: NonEmptyMap[K, A]): NonEmptyMap[K, A] = new NonEmptyMap(m ++ as.m)
  def -(key: K): SortedMap[K, A] = m - key

  def map[B](f: A ⇒ B): NonEmptyMap[K, B] =
    new NonEmptyMap(Functor[SortedMap[K, ?]].map(m)(f))

  def get(k: K): Option[A] = m.get(k)

  def toNonEmptyList: NonEmptyList[(K, A)] = NonEmptyList.fromListUnsafe(m.toList)

  def head: (K, A) = m.head
  def tail: SortedMap[K, A] = m.tail

  def apply(key: K): Option[A] = get(key)
  def contains(key: K): Boolean = m.contains(key)

  def size: Int = m.size
  def forall(p: A ⇒ Boolean): Boolean = m.forall { case (_, a) => p(a) }
  def exists(f: A ⇒ Boolean): Boolean = m.exists { case (_, a) => f(a) }
  def find(f: A ⇒ Boolean): Option[(K, A)] = m.find { case (_, a) => f(a) }
  def filter(p: A ⇒ Boolean): SortedMap[K, A] = m.filter  { case (_, a) => p(a) }
  def filterNot(p: A ⇒ Boolean): SortedMap[K, A] = filter(t => !p(t))


  /**
    * Left-associative fold using f.
    */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    m.foldLeft(b)((b, t) => f(b, t._2))

  /**
    * Right-associative fold using f.
    */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[SortedMap[K, ?]].foldRight(m, lb)(f)

  /**
    * Left-associative reduce using f.
    */
  def reduceLeft(f: (A, A) => A): A =
    reduceLeftTo(identity)(f)

  def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B =
    tail.foldLeft(f(head._2))((b, a) => g(b, a._2))

  /**
    * Right-associative reduce using f.
    */
  def reduceRight(f: (A, Eval[A]) => Eval[A]): Eval[A] =
    reduceRightTo(identity)(f)

  def reduceRightTo[B](f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
    Always((head, tail)).flatMap { case ((_, a), ga) =>
      Foldable[SortedMap[K, ?]].reduceRightToOption(ga)(f)(g).flatMap {
        case Some(b) => g(a, Now(b))
        case None => Later(f(a))
      }
    }


  /**
    * Reduce using the Semigroup of A
    */
  def reduce(implicit S: Semigroup[A]): A =
    reduceLeft(S.combine)

  def reduceRightToOptionWithKey[A, B](fa: SortedMap[K, A])(f: (K, A) => B)(g: ((K, A), Eval[B]) => Eval[B]): Eval[Option[B]] =
    Foldable.iterateRight(fa.toIterable, Now(Option.empty[B])) { (a, lb) =>
      lb.flatMap {
        case Some(b) => g(a, Now(b)).map(Some(_))
        case None => Later(Some(f.tupled(a)))
      }
    }

  def nonEmptyTraverse[G[_], B](f: A => G[B])(implicit G: Apply[G]): G[NonEmptyMap[K, B]] =
    reduceRightToOptionWithKey[A, G[SortedMap[K, B]]](tail)({ case (k, a) =>
      G.map(f(a))(b => SortedMap.empty[K, B] + ((k, b)))
    }) { (t, lglb) =>
      G.map2Eval(f(t._2), lglb)((b, bs) => bs + ((t._1, b)))
    }.map {
      case None => G.map(f(head._2))(a => NonEmptyMap.one(head._1, a))
      case Some(gtail) => G.map2(f(head._2), gtail)((a, bs) => NonEmptyMap((head._1, a), bs))
    }.value

  def toMap: SortedMap[K, A] = m

  /**
    * Typesafe stringification method.
    *
    * This method is similar to .toString except that it stringifies
    * values according to Show[_] instances, rather than using the
    * universal .toString method.
    */
  def show(implicit A: Show[A], K: Show[K]): String =
    s"NonEmpty${Show[SortedMap[K, A]].show(m)}"

  /**
    * Typesafe equality operator.
    *
    * This method is similar to == except that it only allows two
    * NonEmptySet[A] values to be compared to each other, and uses
    * equality provided by Eq[_] instances, rather than using the
    * universal equality provided by .equals.
    */
  def ===(that: NonEmptyMap[K, A])(implicit A: Eq[A]): Boolean =
    Eq[SortedMap[K, A]].eqv(m, that.toMap)

  def length: Int = size

}

private[collection] sealed abstract class NonEmptyMapInstances {
  implicit def catsDataInstancesForNonEmptyMap[K: Order]: SemigroupK[NonEmptyMap[K, ?]] with NonEmptyTraverse[NonEmptyMap[K, ?]]
      with FlatMap[NonEmptyMap[K, ?]] =
    new SemigroupK[NonEmptyMap[K, ?]] with NonEmptyTraverse[NonEmptyMap[K, ?]] with FlatMap[NonEmptyMap[K, ?]] {

      def combineK[A](a: NonEmptyMap[K, A], b: NonEmptyMap[K, A]): NonEmptyMap[K, A] =
        a ++ b

      override def size[A](fa: NonEmptyMap[K, A]): Long = fa.length.toLong

      override def reduceLeft[A](fa: NonEmptyMap[K, A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptyMap[K, A])(implicit A: Semigroup[A]): A =
        fa.reduce

      def reduceLeftTo[A, B](fa: NonEmptyMap[K, A])(f: A => B)(g: (B, A) => B): B = fa.reduceLeftTo(f)(g)

      def reduceRightTo[A, B](fa: NonEmptyMap[K, A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.reduceRightTo(f)(g)

      def flatMap[A, B](fa: NonEmptyMap[K, A])(f: A => NonEmptyMap[K, B]): NonEmptyMap[K, B] =
        NonEmptyMap.fromMapUnsafe(FlatMap[SortedMap[K, ?]].flatMap(fa.m)(f andThen(_.m)))

      def tailRecM[A, B](a: A)(f: A => NonEmptyMap[K, Either[A, B]]): NonEmptyMap[K, B] =
        NonEmptyMap.fromMapUnsafe(FlatMap[SortedMap[K, ?]].tailRecM(a)(f.andThen(_.m)))

      def nonEmptyTraverse[G[_], A, B](fa: NonEmptyMap[K, A])(f: A => G[B])(implicit G: Apply[G]) =
        fa nonEmptyTraverse f


      override def foldLeft[A, B](fa: NonEmptyMap[K, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyMap[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def foldMap[A, B](fa: NonEmptyMap[K, A])(f: A => B)(implicit B: Monoid[B]): B =
        fa.foldLeft(B.empty)((b, a) => B.combine(b, f(a)))

      override def fold[A](fa: NonEmptyMap[K, A])(implicit A: Monoid[A]): A =
        fa.reduce

      override def find[A](fa: NonEmptyMap[K, A])(f: A => Boolean): Option[A] =
        fa.find(f).map(_._2)

      override def forall[A](fa: NonEmptyMap[K, A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: NonEmptyMap[K, A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def toNonEmptyList[A](fa: NonEmptyMap[K, A]): NonEmptyList[A] =
        NonEmptyList(fa.head._2, fa.tail.toList.map(_._2))
    }

  implicit def catsDataEqForNonEmptyMap[K: Order, A: Eq]: Eq[NonEmptyMap[K, A]] =
    new Eq[NonEmptyMap[K, A]]{
      def eqv(x: NonEmptyMap[K, A], y: NonEmptyMap[K, A]): Boolean = x === y
    }

  implicit def catsDataShowForNonEmptyMap[K: Show, A: Show]: Show[NonEmptyMap[K, A]] =
    Show.show[NonEmptyMap[K, A]](_.show)

  implicit def catsDataBandForNonEmptyMap[K, A]: Band[NonEmptyMap[K, A]] = new Band[NonEmptyMap[K, A]] {
    def combine(x: NonEmptyMap[K, A], y: NonEmptyMap[K, A]): NonEmptyMap[K, A] = x ++ y
  }
}

object NonEmptyMap extends NonEmptyMapInstances {
  def fromSet[K: Order, A](as: SortedMap[K, A]): Option[NonEmptyMap[K, A]] =
    if (as.nonEmpty) Option(new NonEmptyMap(as)) else None

  def fromMapUnsafe[K: Order, A](m: SortedMap[K, A]): NonEmptyMap[K, A] =
    if (m.nonEmpty) new NonEmptyMap(m)
    else throw new IllegalArgumentException("Cannot create NonEmptyMap from empty map")

  def apply[K: Order, A](head: (K, A), tail: SortedMap[K, A]): NonEmptyMap[K, A] =
    new NonEmptyMap(SortedMap(head)(Order[K].toOrdering) ++ tail)


  def of[K: Order, A](a: (K, A), as: (K, A)*): NonEmptyMap[K, A] =
    new NonEmptyMap(SortedMap(as: _*)(Order[K].toOrdering) + a)

  def one[K: Order, A](k: K, a: A): NonEmptyMap[K, A] =
    new NonEmptyMap(SortedMap((k,a))(Order[K].toOrdering))
}
