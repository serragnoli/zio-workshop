package net.degoes.zio

import java.io.IOException

import zio.{App, Chunk, DefaultRuntime, IO, Managed, Queue, Task, UIO, ZEnv, ZIO}

import scala.concurrent.duration.TimeUnit
import scala.util.Try


// Can't write combinators with feature
// flatMap embodies the the successful handling
// Anything that modifies memory or interacts with the outside world, use Task to make it lazy
trait Helpers {

  implicit class ZIOExtension[R, E, A](zio: ZIO[R, E, A]) {
    def exited: ZIO[R, Nothing, Int] = zio.fold(_ => 1, _ => 0)
  }

}

object App1 extends App with Helpers {
  //  val runtime = new DefaultRuntime {}
  //
  //  def main(args: Array[String]): Unit = {
  //    runtime.unsafeRun(Task(println("Hello World!!!")))
  //  }

  // effect: ZIO[R, E, A]
  //
  // R- The type of environment required by `effect`
  // E - The type with which the effect may fail
  // A - The type with which the effect may succeed
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =

  //    Task(println("Hello World.")).map(_ => 0)
  //
  //    Task(println("Hello World!!!"))
  //      .as(0) orElse // `as` is equivalent to Mapping above because the above is a common pattern
  //      Task.succeed(1) // Compile time makes sure this cannot fail. Future.success returns a Future which the compiler doesn't know it can fail
  //
  //    Task(println("Hello World!!!")).option.map {
  //      case None => 1
  //      case Some(_) => 0
  //    }
  //
  //    Task(println("Hello World!!!")).either.map {
  //      case Left(_) => 1
  //      case Right(_) => 0
  //    }
  //
  //    Task(println("Hello World!!!")).fold(_ => 1, _ => 0)
  //
  //    Task(println("Hello World!!!")).foldM(_ => Task.succeed(1), _ => Task.succeed(0)) //`foldM` convention from Haskell meaning `Monad`
  //
  //    Task(println("Hello World!!!")).ignore.as(0) // Use this if we don't care if it fails or not
  //
  //    Task {
  //      println("Hello World!!!")
  //      throw new Exception("Died")
  //    }.foldM (
  //      _ => UIO(println("Failed")).as(1),
  //      _ => UIO(println("Succeeded")).as(0)
  //    )

    Task(print("TEST")).exited
}

object App2 extends App {
  // DEF without arguments is the same as a VAL
  // Any time you call flatMap you give the success handler (mental model)
  //
  //

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    //    Task(print("What's your name?"))
    ???
  }
}

object App3 extends App with Helpers {

  import zio.console._

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    (for {
      _ <- putStrLn("What's your name?")
      name <- getStrLn
      _ <- putStrLn(s"Hello $name")
    } yield name).exited
  }
}

//object App4 extends App with Helpers {
//
//  import zio.console._
//  import zio.random._
//
//  def parseInt(s: String): Option[Int] = scala.util.Try(s.toInt).toOption
//
//  def gameLoop(rand: Int): ZIO[Console, IOException, Unit] =
//    for {
//      _ <- putStrLn("Guess a number between 0 and 10:")
//      guess <- getStrLn
//      _ <- parseInt(guess) match {
//        case None => putStrLn("You did not enter a number!").flatMap(_ => gameLoop(rand))
//        case Some(guess) if guess == rand => putStrLn("You guessed the correct number!")
//        case Some(_) => putStrLn("You did not guess it correctly").flatMap(_ => gameLoop(rand))
//      }
//    } yield ()
//
//  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
//    (for {
//      _ <- putStrLn("Welcome to Number Guessing Game!")
//      rand <- nextInt(10)
//      _ <- gameLoop(rand)
//    } yield ()).exited
//  }
//}

object App4Improved extends App with Helpers {

  import zio.console._
  import zio.random._

  def parseInt(string: String): Option[Int] =
    scala.util.Try(string.toInt).toOption

  def parseIntM(string: String): IO[Unit, Int] =
    ZIO.fromOption(parseInt(string))

  val getIntegerGuess =
    getStrLn.flatMap(parseIntM(_)).tapError(_ => putStrLn("You did not enter a number!"))

  def retry[R, E, A](n: Int, zio: ZIO[R, E, A]): ZIO[R, E, A] =
    if (n <= 1) zio else zio orElse retry(n - 1, zio)

  def gameLoop(rand: Int): ZIO[Console, IOException, Unit] =
    for {
      _ <- putStrLn("Guess a number between 0 and 10:")
      guess <- getIntegerGuess.eventually
      loop <- if (guess == rand)
        putStrLn("Congratulations, you guessed the correct number!") as false
      else
        putStrLn("Uh oh, you did not guess correctly. Keep trying!") as true
      _ <- if (loop) gameLoop(rand) else ZIO.unit
    } yield ()

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _ <- putStrLn("Welcome to Number Guessing Game!")
      rand <- nextInt(10)
      _ <- gameLoop(rand)
    } yield ()).exited
}

// cats-io, monix and zio: main effect type players

object App5 extends App with Helpers {
  import java.util.concurrent.TimeUnit
  import java.util.concurrent.ScheduledThreadPoolExecutor

  private[this] val scheduler = new ScheduledThreadPoolExecutor(1)

  def putStrLn(line: String): UIO[Unit] = ZIO.effectTotal(println(line))

  val getStrLn: Task[String] = ZIO.effect(scala.io.StdIn.readLine())

  val nextDouble: UIO[Double] = UIO(scala.util.Random.nextDouble)

  def sleep(n: Long, unit: TimeUnit): UIO[Unit] =
    UIO.effectAsync[Unit] { callback =>
      scheduler.schedule(
        new Runnable {
          def run = callback(UIO.unit)
        }, n, unit
      )
    }

  def run(args: List[String]): UIO[Int] =
    (for {
      double <- nextDouble
      millis = (double * 1000.0).toLong
      _      <- putStrLn(s"About to sleep for ${millis} milliseconds")
      _      <- sleep(millis, TimeUnit.MILLISECONDS)
      _      <- putStrLn(s"Slept for ${millis} milliseconds")
      _      <- putStrLn("How long would YOU like to sleep for, in seconds?")
      time   <- getStrLn.flatMap(string => Task(string.toDouble).tapError(_ =>
        putStrLn("Please enter a double!"))).eventually
      _      <- putStrLn(s"About to sleep for ${time} seconds")
      _      <- sleep((time * 1000.0).toLong, TimeUnit.MILLISECONDS)
      _      <- putStrLn(s"Slept for ${time} seconds")
      _      <- UIO(scheduler.shutdown())
    } yield ()).exited
}

// Intermediates not pasted

object App8 extends App with Helpers {
  import zio.console._
  import zio.blocking.{ Blocking, effectBlocking }
  import java.io._

  final case class Input private (private val is: InputStream) {
    val close: IO[IOException, Unit] =
      IO(is.close()).refineToOrDie[IOException]

    final def read(bufferSize: Int = 1024): ZIO[Blocking, IOException, Option[Chunk[Byte]]] =
      effectBlocking {
        val array = Array.ofDim[Byte](bufferSize)
        val bytesRead = is.read(array)
        if (bytesRead == -1) None
        else Some(Chunk.fromArray(array).take(bytesRead))
      }.refineToOrDie[IOException]
  }
  object Input {
    final def open(file: File): IO[IOException, Input] =
      IO(new Input(new FileInputStream(file))).refineToOrDie[IOException]
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (args match {
      case Nil => putStrLn("Usage: App8 <file>")
      case file :: _ =>
        val fileResource = Managed.make(Input.open(new File(file)))(_.close.ignore)

        fileResource.use { input =>
          (input.read().mapError(Some(_)).flatMap {
            case None => IO.fail(None)
            case Some(chunk) =>
              putStr(chunk.map(_.toChar).mkString("", "", ""))
          }).forever orElse IO.unit
        }
    }).exited
}

object App9 extends App with Helpers {
  import zio.console._

  final def fib(n: BigInt): UIO[BigInt] =
    if (n <= 1) UIO(n)
    else fib(n - 2).zipWith(fib(n - 1))(_ + _)

  val getNumber =
    getStrLn.flatMap(line => Task(line.toInt)).tapError(_ =>
      putStrLn("You did not enter a number!")
    ).eventually

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _      <- putStrLn("How many fibonacci numbers would you like to compute in parallel?")
      n      <- getNumber
      fibers <- ZIO.foreach(0 until n) { i =>
        for {
          _     <- putStrLn(s"Please enter fibonacci number ${i + 1} to compute:")
          n     <- getNumber
          fiber <- fib(n).fork
        } yield fiber
      }
      _      <- fibers.zipWithIndex.foldLeft[ZIO[Console, Nothing, Unit]](IO.unit) {
        case (acc, (fiber, index)) =>
          acc *> (for {
            value <- fiber.join
            _     <- putStrLn(s"The ${index + 1} fibonnaci result is ${value}")
          } yield ())
      }
    } yield ()).exited
}

object App10 extends App with Helpers {
  import zio.console._
  import zio.duration._

  final def fib(n: BigInt): UIO[BigInt] =
    if (n <= 1) UIO(n)
    else fib(n - 2).zipWith(fib(n - 1))(_ + _)

  val getNumber =
    getStrLn.flatMap(line => Task(line.toInt)).tapError(_ =>
      putStrLn("You did not enter a number!")
    ).eventually

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _      <- putStrLn("How many fibonacci numbers would you like to compute in parallel?")
      n      <- getNumber
      nums   <- ZIO.foreach(0 until n) { i =>
        putStrLn(s"Please enter fibonacci number ${i + 1} to compute:") *>
          getNumber
      }
      fibs   <- ZIO.foreachPar(nums.zipWithIndex) {
        case (num, index) =>
          fib(num).tap(num =>
            putStrLn(s"The ${index + 1} fibonnaci result is ${num}")
          )
      }.timeout(5.seconds)
      _      <- putStrLn(s"The fibonacci numbers in order: ${fibs}")
    } yield ()).exited
}

object App11 extends App with Helpers {
  import zio.console._

  sealed trait Command
  object Command {
    final case class ComputeFib(n: Int) extends Command
    case object Quit extends Command

    def fromString(s: String): Option[Command] = {
      def extractFib(value: String): Option[Command] =
        scala.util.Try(value.toInt).toOption.map(ComputeFib(_))

      s.trim.toLowerCase match {
        case "quit" | "exit" => Some(Quit)
        case fib if (fib.startsWith("fib ")) => extractFib(fib.drop(3).trim)
        case value => extractFib(value)
      }
    }
  }

  final def fib(n: BigInt): UIO[BigInt] =
    if (n <= 1) UIO(n)
    else fib(n - 2).zipWith(fib(n - 1))(_ + _)

  val promptCommand: ZIO[Console, Nothing, Command] =
    putStrLn("Please enter 'quit' or 'fib <n>':") *>
      getStrLn.flatMap(line => IO.fromOption(Command.fromString(line)))
        .tapError(_ => putStrLn("You did not enter either 'quit' or 'fib <n>'"))
        .eventually

  val getNumber =
    getStrLn.flatMap(line => Task(line.toInt)).tapError(_ =>
      putStrLn("You did not enter a number!")
    ).eventually


  def makeWorker(queue: Queue[Int]) =
    (for {
      n   <- queue.take
      num <- fib(n)
      _   <- putStrLn(s"Fibonacci number ${n} is ${num}")
    } yield ()).forever

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _     <- putStrLn("How many fibers would you like to use to compute fibonacci numbers?")
      n     <- getNumber
      queue <- Queue.bounded[Int](100)
      _     <- ZIO.forkAll_(List.fill(n)(makeWorker(queue)))
      _     <- promptCommand.flatMap {
        case Command.Quit          => ZIO.fail(())
        case Command.ComputeFib(n) => queue.offer(n)
      }.forever.ignore
    } yield ()).exited
}

