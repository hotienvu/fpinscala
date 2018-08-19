package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type State[S, +A] = S => (A, S)

  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def flatMap[A,B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
    r => {
      val (a, r1) = ra(r)
      f(a)(r1)
    }


  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def _map[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def nonNegativeInt: Rand[Int] = rng => {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i+1) else i, r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)( i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })

  def double: Rand[Double] = rng => {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def _double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def _intDouble: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def _doubleInt: Rand[(Double, Int)] =
    both(double, int)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (i, r) = rng.nextInt
    (i%2==0, r)
  }

  def _boolean: Rand[Boolean] = map(int)(_ % 2 == 0)

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    r => {
      val (a, r1) = ra(r)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    r => fs.foldLeft((List.empty[A], r))({ case ((as, r1), ra) =>
      val (a, r2) = ra(r1)
      (a::as, r2)
    })

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((acc, ra) => map2(acc, ra)(_ :: _))


}


case class State[S,+A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  type Rand[A] = State[RNG, A]
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List.empty))((sa, acc) => sa.map2(acc)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case class InsertCoin(value: Int) extends Input
case object DispenseCandy extends Input

case class MachineState(locked: Boolean = true, candies: Int = 0, coins: Int = 0)

object Machine {
  import State._

  def initialState(candies: Int): MachineState = MachineState(candies = candies)

  def insert(coins: Int): State[MachineState, Unit] =
    modify(s => s.copy(coins = s.coins + coins))


  def dispense: State[MachineState, Unit] =
    modify(s => s.copy(candies = s.candies - 1, coins = s.coins - 1))

  def process(input: Input): State[MachineState, Unit] =
    input match {
      case InsertCoin(c) => insert(c)
      case DispenseCandy => dispense
    }

  def simulate(inputs: List[Input]): State[MachineState, List[MachineState]] =
    sequence(inputs.map(input => for {
      _ <- process(input)
      s <- get
    } yield s))
}

object StateMachineTest {

  import Machine._
  import State._

  def main(args: Array[String]): Unit = {
    val m: MachineState = initialState(10)

    def program1(): Unit = {
      val program: State[MachineState, List[MachineState]] = for {
        _ <- unit(m)
        s1 <- get
        _ <- insert(10)
        s2 <- get
        _ <- dispense
        s3 <- get
      } yield List(s1, s2, s3)
      val (outputs, s) = program.run(m)
      outputs.foreach(println)
      println(s)
    }

    def program2(): Unit = {
      val program: State[MachineState, List[MachineState]] = simulate(List(InsertCoin(1), InsertCoin(1), DispenseCandy, DispenseCandy))
      val (outputs, s) = program.run(m)
      outputs.foreach(println)
      println(s)
    }

    program1()
    println("=====")
    program2()
  }
}
