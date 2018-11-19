package fpinscala.iomonad

import scala.annotation.tailrec

object test {

  sealed trait IO[A] {
    self =>
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  }


  final case class Return[A](a: A) extends IO[A]

  final case class Suspend[A](resume: () => A) extends IO[A]

  final case class FlatMap[A, B](sub: IO[A], cont: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = Return(a)

    def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f

    def apply[A](a: => A): IO[A] = Suspend(() => a) // allow to use constructor such as IO { ... }

    def ref[A](a: A): IO[IORef[A]] = IO {
      new IORef(a)
    }

    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO {
        value = a; a
      }

      def get: IO[A] = IO {
        value
      }

      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    }

    @annotation.tailrec
    def run[A](io: IO[A]): A = io match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(my, g) => run(my.flatMap(y => g(y).flatMap(f)))
      }
    }
  }

  case class Player(name: String, score: Int)

  def contest(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score)
      println(s"${p1.name} is the winner!") else if (p2.score > p1.score)
      println(s"${p2.name} is the winner!")
    else
      println("It's a draw.")

  def winnerMsg(winner: Option[Player]): String =
    winner.map(w => s"${w.name} is the winner!").getOrElse("It's a draw.")

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None

  def contestIO(p1: Player, p2: Player): IO[Unit] = IO {
    println(winnerMsg(winner(p1, p2)))
  }

  val A = Player("A", 1)
  val B = Player("B", 2)
  val w = IO {
    winner(A, B)
  }

  val game = contestIO(A, B)
  val game2 = for {
    w <- IO {
      winner(A, B)
    }
    msg <- IO {
      winnerMsg(w)
    }
    _ <- IO {
      println(msg)
    }
  } yield ()


  def ReadLine: IO[String] = IO {
    readLine
  }

  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }

  val readx = for {
    _ <- PrintLine("Enter a number:")
    x <- ReadLine
    _ <- PrintLine(s"you just entered: $x")
  } yield ()

  val helpstring =
    """
      | The Amazing Factorial REPL, v2.0
      | q - quit
      | <number> - compute the factorial of the given number
      | <anything else> - bomb with horrible error
    """.trim.stripMargin

  import IO._

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM(1 to n toStream)(i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result

  val factorialREPL: IO[Unit] = sequence_(
    IO {
      println(helpstring)
    },
    doWhile {
      IO {
        readLine
      }
    } { line =>
      val ok = line != "q"
      when(ok) {
        for {
          n <- factorial(line.toInt)
          _ <- IO {
            println("factorial: " + n)
          }
        } yield ()
      }
    }
  )

  def doWhile_[A](fa: IO[A])(cond: A => IO[Boolean]): IO[Unit] = for {
    a <- fa
    ok <- cond(a)
    _ <- if (ok) doWhile_(fa)(cond) else IO {
      ()
    }
  } yield ()

  def when_(cond: Boolean)(body: => IO[Unit]): IO[Boolean] =
    if (cond) body.map(_ => true)
    else IO {
      false
    }

  val factorialREPL2: IO[Unit] = for {
    _ <- IO {
      println(helpstring)
    }
    _ <- doWhile_ {
      IO {
        readLine
      }
    } { line =>
      val ok = line != "q"
      when_(ok) {
        for {
          fact <- factorial(line.toInt)
          _ <- IO {
            println(s"factorial: $fact")
          }
        } yield ()
      }
    }
  } yield ()

  def main(args: Array[String]): Unit = {
    val f: Int => IO[Int] = (x: Int) => Return { println(x); x + 1}
//    val g: Int => IO[Int] = List.fill(4)(f).foldLeft(f)((acc, fn) => x => acc(x).flatMap(fn))
    val g: Int => IO[Int] = List.fill(4)(f).foldLeft(f)((acc, fn) => x => Suspend(() => ()).flatMap(_ => acc(x).flatMap(fn)))
    println(g)
    println(g(0))
    run(g(0))
  }
}