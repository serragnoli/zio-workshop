package net.degoes.zio

import java.io.IOException

import zio._

//object motivation {
//  def println(s: String): Unit = ??? //NO function, interacts with the outside world
//
//  case class Effect[+A](unsafeRun: () => A) { self => //Storing side-effect in `unsafeRun`. Not pure because it as side-effect
//    final def map[B](f: A => B): Effect[B] = Effect(() => f(self.unsafeRun))
//
//    final def flatMap[B](f: A => Effect[B]): Effect[B] =
//      Effect(() => f(self.unsafeRun())).unsafeRun()
//  }
//
//  def putStrLn(s: String): Effect[Unit] = Effect(() => println(s))
//
//  val a = "a"
//  putStrLn("foo") //This functions: Total, Deterministic, No side-effects
//  putStrLn("foo").unsafeRun //Perform the side-effect
//}


object HelloWorld extends App {

  import zio.console._

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    putStrLn("Hello World!").fold(_ => 1, _ => 0)
}

object PromptName extends App {

  import zio.console._

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      _ <- putStrLn("What's your name?")
      name <- getStrLn
      _ <- putStrLn(s"Hello $name")
    } yield 0) orElse ZIO.succeed(1)
}

object ZioTypes extends App {

  import zio.console._

  // ZIO[R, E, A] = The only effect type in ZIO
  // A - Success
  // E - Failure
  // R - Environment type (the type of environment required to run the effect
  // Effectful version of: R => Either[E, A]
  // ZIO(Config, Throwable, Unit)

  type Task[+A] = ZIO[Any, Throwable, A]
  type UIO[+A] = ZIO[Any, Nothing, A] // doesn't require anything, it can't fail
  type RIO[-R, +A] = ZIO[R, Throwable, A]
  type IO[+E, +A] = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      _ <- putStrLn("What's your name?")
      name <- getStrLn
      _ <- putStrLn(s"Hello $name")
    } yield 0) orElse UIO(1)
}

object NumberGuesser extends App {

  import zio.console._
  import zio.random._

  def analyseAnswer(random: Int, guess: String) = {
    if (random.toString == guess.trim)
      putStrLn("You guessed correctly")
    else putStrLn(s"You did not guess correctly. Number was $random")
  }


  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      random <- nextInt(10)
      _ <- putStrLn("Guess a number")
      guess <- getStrLn
      _ <- analyseAnswer(random, guess)
    } yield 0).fold(_ => 1, _ => 0)
}

// SERVER for REST endpoints with ZIO
// Effects: SERVER, Services, DB?

object AlarmApp extends App {

  import zio.console._
  import zio.duration._

  def parseDouble(input: String) =
    for {
     double <-  Task(input.toDouble).refineToOrDie[NumberFormatException]
    } yield (double * 1000).toInt.millis

  def fallback(input: String) =
    putStrLn(s"The input of fallback $input is not a valid double").flatMap(_ => getAlarmDuration)
    putStrLn(s"The input of fallback $input is not a valid double") zipRight (_ => getAlarmDuration)
      putStrLn(s"The input of fallback $input is not a valid double") *> getAlarmDuration

  val getAlarmDuration: ZIO[Console, IOException, Duration] = // DEF with no arguments is VAL. If it refers to itself LAZY val
    for {
      _ <- putStrLn("How many seconds to wait before sounding the alarm?")
      input <- getStrLn
      duration <-   parseDouble(input) orElse fallback(input)// == Task.effect.., == ZIO.effect(input.toDouble)
    } yield duration

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      duration <- getAlarmDuration
      _ <- ZIO.sleep(duration)
    } yield 0).foldCauseM(error  => putStrLn(s"Error: ${error.prettyPrint}").map(_ => 1), _ => ZIO.succeed(0))
}