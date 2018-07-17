package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    override def zero: A => A = x => x
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)

    override def zero: A = m.zero
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(b,a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = ???

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(x), Stub(y)) => Stub(x + y)
      case (Stub(x), Part(ly, w, ry)) => Part(x + ly, w, ry)
      case (Part(lx, w, rx), Stub(y)) => Part(lx, w, rx + y)
      case (Part(lx, wx, rx), Part(ly, wy, ry)) =>
        val newCount = wx + wy + (if (rx.nonEmpty || ly.nonEmpty) 1 else 0)
        Part(lx, newCount, ry)
    }

    override def zero: WC = Stub("")
  }



  def wordCount(s: String): Int = {
    def count(str: String): WC =
      if (str == " ") Part("", 0, "")
      else if (str.length == 1) Stub(str)
      else {
        val (l, r) = str.splitAt(str.length / 2)
        wcMonoid.op(count(l), count(r))
      }

    count(s) match {
      case _ :Stub => 1
      case Part(l, c, r) => c + (if (l.nonEmpty) 1 else 0)  + (if (r.nonEmpty) 1 else 0)
    }
  }

  def wordCountFoldMapV(s: String): Int = {
    def wc(c: Char) =
      if (c == ' ') Part("", 0, "")
      else Stub(c.toString)

    def wcStub(s: String) = if (s.nonEmpty) 1 else 0

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(st) => wcStub(st)
      case Part(l, w, r) => wcStub(l) + w + wcStub(r)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2) )

    override def zero: (A, B) = (A.zero, B.zero)
  }

  def lengthAndSum(as: IndexedSeq[Int]): (Int, Int) =
    foldMapV(as, productMonoid(intAddition, intAddition))(a => (1, a))

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(f: A => B, g: A => B): A => B =
        a => B.op(f(a), g(a))

      override def zero: A => B =
        a => B.zero
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        (a1.keySet ++ a2.keySet).foldLeft(Map.empty[K, V]) (
          (acc, k) => acc + (k -> V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
        )

      override def zero: Map[K, V] = Map.empty
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val mapMerge = mapMergeMonoid[A, Int](intAddition)
    foldMapV(as, mapMerge)(a => Map(a -> 1))
  }
}


trait Foldable[F[_]] {
  import Monoid.{dual, endoMonoid}

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B =
    foldRight(as)(m.zero)((a: A, b: B) => m.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldRight(as)(m.zero)((b, a) => m.op(b, a))

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)
}


object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(v) => f(v, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as.map(a => f(a, z)).getOrElse(z)
}

object Test extends App {
  val s = "    abc 1    a    klasdjflk jaslkf jksa foo bar    "
  println(Monoid.wordCount(s))
  println(Monoid.wordCountFoldMapV(s))
  val xs = List(1,2,3,4)
  println(Monoid.lengthAndSum(xs.toIndexedSeq))
  println(ListFoldable.foldRight(xs)(0)(_ + _))
  println(ListFoldable.foldLeft(xs)(0)(_ + _))
  println(ListFoldable.foldMap(xs)(x => x)(Monoid.intAddition))
  println(ListFoldable.concatenate(xs)(Monoid.intAddition))
  println(ListFoldable.toList(xs))
}