package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) =>
      if (n > 0) Cons(h, () => t().take(n - 1))
      else Empty
    case Empty => Empty
  }

  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Empty => Empty
      case Cons(h, t) => t().drop(n - 1)
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Option(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, bs) => cons(f(a), bs))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, bs) =>
      if (f(a)) cons(a, bs)
      else bs)

  def append[B>:A](as: Stream[B]): Stream[B] = foldRight(as)((a, bs) => cons(a, bs))

  def flapMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, bs) => f(a) append bs)

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this)(s => s match {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  })

  def takeViaUnfold(n: Int): Stream[A] = unfold((n, this))(s => s match {
    case (i, Cons(h, t)) if (i > 0) => Some(h(), (i - 1, t()))
    case _ => None
  })

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this)(s => s match {
    case Cons(h, t) if (p(h())) => Some(h(), t())
    case _ => None
  })

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, b))(s => s match {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
    case _ => None
  })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2))(z => {
    val (za, zb) = z
    val (a, sa) = za match {
      case Cons(h, t) => (Some(h()), t())
      case Empty => (None, empty)
    }
    val (b, sb) = zb match {
      case Cons(h, t) => (Some(h()), t())
      case Empty => (None, empty)
    }
    (a, b) match {
      case (None, None) => None
      case _ => Some(((a, b), (sa, sb)))
    }
  })

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s) forAll (_ match {
    case (Some(_), None) => true
    case (Some(za), Some(zb)) if (za == zb) => true
    case _ => false
  })

  def tails: Stream[Stream[A]] = unfold(this)(s => s match {
    case Cons(_, t) => Some(s, t())
    case _ => None
  })
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

  def constant[A](a: A): Stream[A] = Stream.cons(a, Stream.constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n + 1))

  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = Stream.cons(a, go(b, a + b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, Stream.unfold(s)(f))
    case _ => Stream.empty
  }

  def fibsViaUnfold(): Stream[Int] = Stream.unfold((0, 1))(prev => Some((prev._1, (prev._2, prev._1 + prev._2))))

  def fromViaUnfold(n: Int): Stream[Int] = Stream.unfold(n)(s => Some(s, s + 1))

  def constantViaUnfold[A](a: A): Stream[A] = Stream.unfold(a)(s => Some(s, s))

  def onesViaUnfold: Stream[Int] = Stream.constantViaUnfold(1)
}