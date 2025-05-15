package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.*

import scala.annotation.tailrec
import scala.jdk.Accumulator

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(_, t) if n > 0 => skip(t)(n - 1)
      case _ => s


    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h, t), Cons(x, y)) => Cons((h, x), zip(t, y))
      case _ => Nil()


    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
      case Cons(h, t) => Cons(h, concat(t, s2))
      case Nil() => s2

    /*
       * Reverse the sequence
       * E.g., [10, 20, 30] => [30, 20, 10]
       * E.g., [10] => [10]
       * E.g., [] => []
       */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      @annotation.tailrec
      def _reverse(actual: Sequence[A], remaining: Sequence[A]): Sequence[A] = remaining match
        case Nil() => actual
        case Cons(h, t) => _reverse(Cons(h, actual), t)

      _reverse(Nil(), s)

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()


    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] =
      @annotation.tailrec
      def _min(s: Sequence[Int], min: Optional[Int]): Optional[Int] = s match
        case Cons(h, t) => min match
          case Empty() => _min(t, Just(h))
          case Just(m) if h < m => _min(t, Just(h))
          case _ => _min(t, min)
        case Nil() => min

      _min(s, Empty())


    /*
   * Get the elements at even indices
   * E.g., [10, 20, 30] => [10, 30]
   * E.g., [10, 20, 30, 40] => [10, 30]
   */
    def evenIndices[A](s: Sequence[A]): Sequence[A] =
      def _evenIndices(s: Sequence[A], accumulator: Int): Sequence[A] = s match
        case Cons(h, t) if accumulator % 2 == 0 => Cons(h, _evenIndices(t, accumulator + 1))
        case Cons(h, t) => _evenIndices(t, accumulator + 1)
        case _ => s

      _evenIndices(s, 0)

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h, t) if h == elem => true
      case Cons(h, t) => contains(t)(elem)
      case _ => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] =
      def _distinct(s: Sequence[A], seen: Sequence[A]): Sequence[A] = s match
        case Cons(h, t) if contains(seen)(h) => _distinct(t, seen)
        case Cons(h, t) => Cons(h, _distinct(t, concat(seen, Cons(h, Nil()))))
        case _ => Nil()

      _distinct(s, Nil())


    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = s match {
      case Cons(h, t) =>
        val (grouped, rest) = groupContiguous(Cons(h, Nil()), t) //[[10],10,20,30] => [[10,10]] , [20,30]
        Cons(grouped, group(rest))
      case Nil() => Nil()
    }

    // Helper function to group contiguous elements
    def groupContiguous[A](group: Sequence[A], remaining: Sequence[A]): (Sequence[A], Sequence[A]) = remaining match
      case Cons(h, t) => group match // rest
        case Cons(h2, _) if h == h2 =>
          groupContiguous(Cons(h, group), t) // [[10,10],20,30]
        case _ =>
          (group, remaining) // [[10,10]] , [20,30]
      case _ =>
        (group, remaining)


    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = ???

    @tailrec
    def foldLeft[A](s: Sequence[A], acc: Int, f: (Int, A) => Int): Int = s match
      case Cons(h, t) => foldLeft((t), f(acc, h), f)
      case _ => acc


  end Sequence
end Sequences

@main def trySequences =
  import Sequences.*
  val sequence = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(sequence)) // 30

  import Sequence.*

  println(sum(map(filter(sequence)(_ >= 20))(_ + 1))) // 21+31 = 52
