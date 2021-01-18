package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(head, tail) => head() :: tail().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = (this, n) match {
    case (Empty, _) => Empty
    case (_, 0) => empty
    case (_, negative) if negative < 0 => empty
    case (Cons(head, tail), i) => cons(head(), tail().take(i - 1))
  }

  def drop(n: Int): Stream[A] = (this, n) match {
    case (Empty, _) => Empty
    case (stream, 0) => stream
    case (Cons(_, tail), i) => tail().drop(i - 1)
  }

  def takeWhileNoFold(p: A => Boolean): Stream[A] = this match {
    case Cons(h, tail) =>
      lazy val head = h()
      if (p(head)) {
        cons(head, tail().takeWhileNoFold(p))
      } else {
        empty
      }
    case Empty => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((_left, right) => {
      lazy val left = _left
      if (p(left)) {
        cons(left, right)
      } else {
        empty
      }
    })

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = ???

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((left, right) =>
      cons(f(left), right)
    )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h, t))

  def flatMap[C](f: A => Stream[C]): Stream[C] =
    foldRight(empty[C])((head, tail) => f(head) append tail)
  //    my solution: this.map(f).foldRight(empty[C])((left, right) => left.append(right))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n))

  def from_(n: Int): Stream[Int] =
    Stream.cons(n, from_(n+1))

  def fibs_helper(i: Int, j: Int): Stream[Int] =
    Stream.cons(i + j, fibs_helper(j, i + j))

  def fibs_(): Stream[Int] =
    Stream.cons(0, Stream.cons(1, fibs_helper(0, 1)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  // my solution: f(z).map(res => Stream.cons[A](res._1, unfold(res._2)(f))).getOrElse(empty[A])
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case _ => Stream.empty
    }

  def from(i: Int): Stream[Int] =
    unfold(i)(j => Some((j, j+1)))


  def fibs(): Stream[Int] =
    unfold((0, 1))(o => Some(o._2, (o._2, o._1 + o._2)))
  //unfold(Stream(0, 1))()

}