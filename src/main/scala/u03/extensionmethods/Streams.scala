package u03.extensionmethods

object Streams:

  import Sequences.*

  enum Stream[A]:
    case Empty()
    case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    extension [A](stream: Stream[A])
      def toList: Sequence[A] = stream match
        case Cons(h, t) => Sequence.Cons(h(), t().toList)
        case _ => Sequence.Nil()

    extension [A](stream: Stream[A])
      def map[B](f: A => B): Stream[B] = stream match
        case Cons(head, tail) => cons(f(head()), tail().map(f))
        case _ => Empty()

    extension [A](stream: Stream[A])
      def filter(pred: A => Boolean): Stream[A] = stream match
        case Cons(head, tail) if pred(head()) => cons(head(), tail().filter(pred))
        case Cons(head, tail) => tail().filter(pred)
        case _ => Empty()

    extension [A](stream: Stream[A])
      def take(n: Int): Stream[A] = (stream, n) match
        case (Cons(head, tail), n) if n > 0 => cons(head(), tail().take(n - 1))
        case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def generate[A](supplier: () => A): Stream[A] =
      cons(supplier(), generate(supplier))

    extension [A](stream: Stream[A])
      def takeWhile(pred: A => Boolean): Stream[A] = stream match
        case (Cons(h, t)) if pred(h()) => cons(h(), t().takeWhile(pred))
        case _ => Empty()

    def fill[A](n: Int)(init: A): Stream[A] =
      if n > 0 then cons(init, fill(n - 1)(init)) else cons(init, Empty())


    def fibonacci: Stream[Int] =
      def fib(a: Int, b: Int): Stream[Int] =
        cons(a, fib(b, a + b))

      fib(0, 1)


  end Stream
end Streams

@main def streamExample =
  import Streams.*
  import Stream.*

  println(generate(() => "a").take(10).toList)

  println(generate(() => Math.random())
    .map(x => (x * 10).toInt)
    .filter(x => x < 5)
    .take(10)
    .toList)

  val stream = Stream.iterate(0)(_ + 1)
  println(Stream.toList(Stream.takeWhile(stream)(_ < 5)))

  println(Stream.toList(Stream.fill(3)("a")))

  val fibonacci: Stream[Int] = Stream.fibonacci
  println(Stream.toList(fibonacci.take(5))) // Cons (0 , Cons (1 , Cons (1 , Cons (2 , Cons (3 , Nil ()))))
// generate(() => scala.io.StdIn.readLine).map(s => {println(s);s}).foldleft("")(_ + _)
