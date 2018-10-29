package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
  // primitives
  def unit[A](a: => A): F[A]

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  // derived combinators
  def lift[A, B](f: A => B): F[A] => F[B] =
    fa => map(fa)(f)

  def lift2[A, B, C](f: A => B => C): F[A] => F[B] => F[C] =
    fa => fb => map2(fa, fb)((a, b) => f(a)(b))

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)


  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldLeft(unit(List.empty[B]))((acc, a) => map2(f(a), acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] // = flatten(map(ma)(f))

  def flatten[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](errors: List[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

sealed trait Validate[+A]
case class ValidateSuccess[A](a: A) extends Validate[A]
case class ValidateFailure(errors: List[String]) extends Validate[Nothing]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled

    override def apply[A, B](fab: Stream[A => B])(fa: Stream[A]): Stream[B] =
      map2(fa, fab)((a, f) => f(a))
  }

  implicit def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] with Monad[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] with Monad[({type f[x] = Validation[E, x]})#f]  {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] =
        (fa, fab) match {
          case (Success(a), Success(f)) => Success(f(a))
          case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
          case (_, e@Failure(_)) => e
          case (e@Failure(_), _) => e
        }

      override def flatMap[A, B](ma: Validation[E, A])(fa: A => Validation[E, B]): Validation[E, B] = ma match {
        case Success(a) => fa(a)
        case e@Failure(_) => e
      }
    }

  implicit val validateApplicative: Applicative[Validate] with Monad[Validate] =
    new Applicative[Validate] with Monad[Validate] {
      override def unit[A](a: => A): Validate[A] = ValidateSuccess(a)

      override def apply[A, B](fab: Validate[A => B])(fa: Validate[A]): Validate[B] =
        (fa, fab) match {
          case (ValidateSuccess(a), ValidateSuccess(f)) => ValidateSuccess(f(a))
          case (ValidateFailure(e1), ValidateFailure(e2)) => ValidateFailure(e1 ++ e2)
          case (_, e@ValidateFailure(_)) => e
          case (e@ValidateFailure(_), _) => e
        }

      override def flatMap[A, B](ma: Validate[A])(f: A => Validate[B]): Validate[B] =  ma match {
        case ValidateSuccess(a) => f(a)
        case ValidateFailure(e) => ValidateFailure(e)
      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  // syntactic sugar
  implicit class ToIdOps[A](a: => A) {
    def pure[F[_]](implicit F0: Applicative[F]): F[A] = F0.unit(a)
  }
  implicit class ToFunctorOps[F[_], A, B](fa: F[A])(implicit F0: Functor[F]) {
    def map(f: A=>B): F[B] = F0.map(fa)(f)
  }

  implicit class ToApplicativeOps[F[_], A, B](f: F[A=>B])(implicit F0: Applicative[F]) {
    def <*>(fa: F[A]): F[B] = F0.apply(f)(fa)
  }

  implicit class ToMonadOps[M[_], A, B](ma: M[A])(implicit M0: Monad[M])  {
    def flatMap(f: A => M[B]): M[B] = M0.flatMap(ma)(f)
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] = ???
//    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A,B](fa: F[A])(f: A => B): F[B] = ???

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = ???
//    traverseS(fa)((a: A) => (for {
//      s1 <- get[S]
//      (b, s2) = f(a, s1)
//      _  <- set(s2)
//    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = ???

  val optionTraverse = ???

  val treeTraverse = ???
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
