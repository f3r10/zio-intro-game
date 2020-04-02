package net.degoes.zio

import zio._
import java.text.NumberFormat
import java.nio.charset.StandardCharsets
import net.degoes.zio.StmLunchTime.Attendee.State.Starving
import net.degoes.zio.StmLunchTime.Attendee.State.Full

object ZIOTypes {
  type ??? = Nothing

  /**
    * EXERCISE
    *
    * Provide definitions for the ZIO type aliases below.
    */
  type Task[+A] = ZIO[Any, Throwable, A]
  type UIO[+A] = ZIO[Any, Nothing, A]
  type RIO[-R, +A] = ZIO[R, Throwable, A]
  type IO[+E, +A] = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object HelloWorld extends App {
  import zio.console._

  /**
    * EXERCISE
    *
    * Implement a simple "Hello World!" program using the effect returned by `putStrLn`.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("Hello World!") as (0) // the same as .map(_ => 0)
}

object PrintSequence extends App {
  import zio.console._

  /**
    * EXERCISE
    *
    * Using `*>` (`zipRight`), compose a sequence of `putStrLn` effects to
    * produce an effect that prints three lines of text to the console.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    // it can be used zipRight or zipLeft (*> or <* are just aliases)
    // zipRight it will ignore the left part and return the right part and zipLeft the other way around
    putStrLn("line 1") *>
      putStrLn("line 2") *>
      putStrLn("line 3") *>
      ZIO.succeed(0)
}

object ErrorRecovery extends App {
  val StdInputFailed = 1

  import zio.console._

  val failed: ZIO[Console, String, Unit] =
    putStrLn("About to fail...") *>
      ZIO.fail("Uh oh!") *>
      putStrLn("This will NEVER be printed!")

  /**
    * EXERCISE
    *
    * Using `ZIO#orElse` or `ZIO#fold`, have the `run` function compose the
    * preceding `failed` effect into the effect that `run` returns.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    // the failed value has a failure but the return type of run point us that can not return any error but is waiting a
    // Int value as response. Therefore the error channel should be handle.
    // 1. fold will handle the two possibilities
    // failed.fold(_ => 0, _ => 1)
    // 2. the orElse will return a succeed(1) indicanting that it has an error
    // (failed as (0)) orElse ZIO.succeed(1)
    // 3. also the catchAllCause will get the cause of the error and we can print that error an return 1 as a failure
    // .. all succeeded cases have to return 0
    (failed as (0)).catchAllCause(cause => putStr(s"${cause.prettyPrint}") as 1)

}

object Looping extends App {
  import zio.console._

  /**
    * EXERCISE
    *
    * Implement a `repeat` combinator using `flatMap` and recursion.
    */
  def repeat[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, E, A] =
    if (n == 0) effect
    // else effect.flatMap(_ => repeat(n - 1)(effect))
    else effect *> repeat(n - 1)(effect)

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    repeat(100)(putStrLn("All work and no play makes Jack a dull boy")) as 0
}

object EffectConversion extends App {

  /**
    * EXERCISE
    *
    * Using ZIO.effect, convert the side-effecting of `println` into a pure
    * functional effect.
    */
  def myPrintLn(line: String): Task[Unit] = UIO(println(line))

  def run(args: List[String]) =
    (myPrintLn("Hello World!") as (0)) orElse ZIO.succeed(1)
}

object ErrorNarrowing extends App {
  import java.io.IOException
  import scala.io.StdIn.readLine
  implicit class Unimplemented[A](v: A) {
    def ? = ???
  }

  /**
    * EXERCISE
    *
    * Using `ZIO#refineToOrDie`, narrow the error type of the following
    * effect to IOException.
    */
  val myReadLine: IO[IOException, String] = IO.apply(readLine).refineToOrDie

  def myPrintLn(line: String): UIO[Unit] = UIO(println(line))

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _ <- myPrintLn("What is your name?")
      name <- myReadLine
      _ <- myPrintLn(s"Good to meet you, ${name}")
    } yield 0) orElse ZIO.succeed(1)
}

object PromptName extends App {
  val StdInputFailed = 1

  import zio.console._

  /**
    * EXERCISE
    *
    * Using `ZIO#flatMap`, implement a simple program that asks the user for
    * their name (using `getStrLn`), and then prints it out to the user (using `putStrLn`).
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("What is your name").flatMap { _ =>
      getStrLn.flatMap { name => putStrLn(s"Good to meet you, ${name}") as 0 }
    } orElse ZIO.succeed(1)
  // (
  //   for {
  //     _ <- putStrLn("What is your name")
  //     name <- getStrLn
  //     _ <- putStrLn(s"Good to meet you, ${name}")
  //   } yield 0
  // ) orElse ZIO.succeed(1)
}

object NumberGuesser extends App {
  import zio.console._
  import zio.random._

  def analyzeAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) putStrLn("You guessed correctly!")
    else putStrLn(s"You did not guess correctly. The answer was ${random}")

  /**
    * EXERCISE
    *
    * Choose a random number (using `nextInt`), and then ask the user to guess
    * the number, feeding their response to `analyzeAnswer`, above.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      nameToGuess <- nextInt(10)
      _ <- putStrLn("Guess a number between 0 and 10")
      guess <- getStrLn
      _ <- analyzeAnswer(nameToGuess, guess)
    } yield 0) orElse ZIO.succeed(1)
}

object AlarmApp extends App {
  import zio.console._
  import zio.duration._
  import java.io.IOException
  import java.util.concurrent.TimeUnit

  /**
    * EXERCISE
    *
    * Create an effect that will get a `Duration` from the user, by prompting
    * the user to enter a decimal number of seconds. Use `refineOrDie` to
    * narrow the error type as necessary.
    */
  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      IO.apply(durationLong(input.toLong).second).refineOrDie {
        case e: NumberFormatException => e
      }

    def fallback(input: String): ZIO[Console, IOException, Duration] =
      putStrLn(
        s"The input $input was not valid. It will be use 3 second as default"
      ) *>
        IO.apply(durationInt(3).millis).refineOrDie {
          case e: IOException => e
        }

    for {
      _ <- putStrLn("Please enter the number of seconds to sleep: ")
      input <- getStrLn
      duration <- parseDuration(input) orElse fallback(input)
    } yield duration
  }

  /**
    * EXERCISE
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds using ZIO.sleep(d), and then
    * prints out a wakeup alarm message, like "Time to wakeup!!!".
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      duration <- getAlarmDuration
      fiber <- (putStr(".") *> ZIO.sleep(1.second)).forever.fork
      _ <- ZIO.sleep(duration) *> putStrLn("Time to wakeup!!!!") *> fiber.interrupt
    } yield 0) orElse ZIO.succeed(1)
}

object Cat extends App {
  import zio.console._
  import zio.blocking._
  import java.io.IOException
  import scala.io.{Codec, Source}

  /**
    * EXERCISE
    *
    * Implement a function to read a file on the blocking thread pool, storing
    * the result into a string.
    */
  def readFile(file: String): ZIO[Blocking, IOException, String] =
    effectBlocking(Source.fromFile(file)(Codec.UTF8).mkString)
      .refineToOrDie[IOException]

  /**
    * EXERCISE
    *
    * Implement a version of the command-line utility "cat", which dumps the
    * contents of the specified file to standard output.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _ <- putStrLn("Enter the name of a file")
      file <- getStrLn
      content <- readFile(file)
      _ <- putStrLn(content)
    } yield 0) orElse ZIO.succeed(1)
}

object SourceManaged extends App {
  import zio.console._
  import zio.blocking._
  import zio.duration._
  import java.io.IOException

  import scala.io.Source

  final class ZioSource private (private val source: Source) {
    def execute[T](f: Source => T): ZIO[Blocking, IOException, T] =
      effectBlocking(f(source)).refineToOrDie[IOException]
  }
  object ZioSource {

    /**
      * EXERCISE
      *
      * Use the `ZManaged.make` constructor to make a managed data type that
      * will automatically acquire and release the resource when it is used.
      */
    def make(file: String): ZManaged[Blocking, IOException, ZioSource] = {
      // An effect that acquires the resource:
      val open = effectBlocking(new ZioSource(Source.fromFile(file)))
        .refineToOrDie[IOException]

      // A function that, when given the resource, returns an effect that
      // releases the resource:
      val close: ZioSource => ZIO[Blocking, Nothing, Unit] =
        _.execute(_.close()).orDie

      ZManaged.make(open)(close)
    }
  }

  /**
    * EXERCISE
    *
    * Implement a function to read a file on the blocking thread pool, storing
    * the result into a string.
    */
  def readFiles(
      files: List[String]
  ): ZIO[Blocking with Console, IOException, Unit] = {
    def read(file: String): ZIO[Blocking with Console, IOException, Unit] =
      // each read is evaluated to return the content of the file and them putStr. The resource is only used for the
      // read of the content.
      for {
        content <- ZioSource.make(file).use { resource =>
          resource.execute(_.mkString)
        }
        _ <- putStrLn(content)
      } yield ()
    ZIO.foreachPar_(files)(read)
  }
  //This part evalutates the result of each file and print inside the resource. The resource block is used for more that
  //one thing at the time
  // ZIO.foreachPar_(files) { f =>
  //   // make is the one that that is made use of the blocking part. Therefore is not necessary not sourrond the code
  //   // with a blocking method expresion
  //   ZioSource.make(f).use { source =>
  //     for {
  //       content <- source.execute(_.mkString)
  //       _ <- putStrLn(content)
  //     } yield 0
  //   }
  // }

  /**
    * EXERCISE
    *
    * Implement a function that prints out all files specified on the
    * command-line. Only print out contents from these files if they
    * can all be opened simultaneously. Otherwise, don't print out
    * anything except an error message.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (
      for {
        _ <- putStrLn("Enter the name of the files separed for a ;")
        files <- getStrLn.map(_.split(";").toList)
        _ <- readFiles(files).catchAllCause(e =>
          putStrLn(s"${e.prettyPrint}") as 1
        )
      } yield 0
    ) orElse ZIO.succeed(1)
}

object CatIncremental extends App {
  import zio.console._
  import zio.blocking._
  import java.io.{IOException, InputStream, FileInputStream}

  /**
    * BONUS EXERCISE
    *
    * Implement a `blockingIO` combinator to use in subsequent exercises.
    */
  def blockingIO[A](a: => A): ZIO[Blocking, IOException, A] =
    effectBlocking(a).refineToOrDie[IOException]

  /**
    * EXERCISE
    *
    * Implement all missing methods of `FileHandle`. Be sure to do all work on
    * the blocking thread pool.
    */
  final case class FileHandle private (private val is: InputStream) {
    final def close: ZIO[Blocking, IOException, Unit] = blockingIO(is.close())

    final def read: ZIO[Blocking, IOException, Option[Chunk[Byte]]] =
      blockingIO {
        val buffer = Array.ofDim[Byte](64)
        val count = is.read(buffer)
        if (count > -1) Some(Chunk.fromArray(buffer.take(count)))
        else None
      }
  }
  object FileHandle {
    final def open(file: String): ZIO[Blocking, IOException, FileHandle] =
      blockingIO(FileHandle(new FileInputStream(file)))
  }

  def cat(fh: FileHandle): ZIO[Blocking with Console, IOException, Unit] =
    for {
      _ <- fh.read.flatMap {
        case Some(chuck) => putStr(new String(chuck.toArray)) *> cat(fh)
        case None        => putStrLn("") *> ZIO.unit
      }
    } yield ()

  /**
    * EXERCISE
    *
    * Implement an incremental version of the `cat` utility, using `ZIO#bracket`
    * or `ZManaged` to ensure the file is closed in the event of error or
    * interruption.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    args match {
      case file :: Nil =>
        // (effect timeout 60.seconds) ensuring finalizer
        (FileHandle.open(file).bracket(_.close.ignore)(cat) as 0) orElse ZIO
          .succeed(1)

      case _ => putStrLn("Usage: cat <file>") as 2
    }
}

object AlarmAppImproved extends App {
  import zio.console._
  import zio.duration._
  import java.io.IOException
  import java.util.concurrent.TimeUnit

  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO
        .effect(
          Duration((input.toDouble * 1000.0).toLong, TimeUnit.MILLISECONDS)
        )
        .refineToOrDie[NumberFormatException]

    val fallback =
      putStrLn("You didn't enter the number of seconds!") *> getAlarmDuration

    for {
      _ <- putStrLn("Please enter the number of seconds to sleep: ")
      input <- getStrLn
      duration <- parseDuration(input) orElse fallback
    } yield duration
  }

  /**
    * EXERCISE
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds using ZIO.sleep(d), concurrently
    * prints a dot every second that the alarm is sleeping for, and then
    * prints out a wakeup alarm message, like "Time to wakeup!!!".
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      duration <- getAlarmDuration
      fiber1 <- (putStr(".") *> ZIO.sleep(1.second)).forever.fork
      _ <- ZIO.sleep(duration) *> putStrLn("Time to wakeup") *> fiber1.interrupt
    } yield 0) orElse ZIO.succeed(1)
}

object ComputePi extends App {
  import zio.random._
  import zio.console._
  import zio.clock._
  import zio.duration._
  import zio.stm._

  /**
    * Some state to keep track of all points inside a circle,
    * and total number of points.
    */
  final case class PiState(
      inside: Ref[Long],
      total: Ref[Long]
  )

  /**
    * A function to estimate pi.
    */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /**
    * A helper function that determines if a point lies in
    * a circle of 1 radius.
    */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /**
    * An effect that computes a random (x, y) point.
    */
  val randomPoint: ZIO[Random, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  def updateOnce(ref: PiState): ZIO[Random, Nothing, Unit] =
    for {
      tuple <- randomPoint
      (x, y) = tuple
      inside = if (insideCircle(x, y)) 1 else 0
      _ <- ref.inside.update(x => x + inside)
      _ <- ref.total.update(x => x + 1)
    } yield ()

  def printEstimate(ref: PiState): ZIO[Console, Nothing, Unit] =
    for {
      state_inside <- ref.inside.get
      state_total <- ref.total.get
      _ <- putStrLn(s"${estimatePi(state_inside, state_total)}")
    } yield ()

  /**
    * EXERCISE
    *
    * Build a multi-fiber program that estimates the value of `pi`. Print out
    * ongoing estimates continuously until the estimation is complete.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      inside_state <- Ref.make(0L)
      total_state <- Ref.make(0L)
      piState = PiState(inside_state, total_state)
      worker = updateOnce(piState).forever
      workers = List.fill(4)(worker)
      fiber1 <- ZIO.forkAll(workers)
      fiber2 <- (printEstimate(piState) *> ZIO.sleep(1.second)).forever.fork
      _ <- putStrLn("Enter any key to terminate...")
      _ <- getStrLn *> (fiber1 zip fiber2).interrupt //fiber1.interrupt *> fiber2.interrupt
    } yield 0) orElse ZIO.succeed(1)
}

object StmSwap extends App {
  import zio.console._
  import zio.stm._
  import zio.duration._

  /**
    * EXERCISE
    *
    * Demonstrate the following code does not reliably swap two values in the
    * presence of concurrency.
    */
  def exampleRef = {
    def swap[A](ref1: Ref[A], ref2: Ref[A]): UIO[Unit] =
      for {
        v1 <- ref1.get
        v2 <- ref2.get
        _ <- ref2.set(v1)
        _ <- ref1.set(v2)
      } yield ()

    for {
      ref1 <- Ref.make(100)
      ref2 <- Ref.make(0)
      fiber1 <- swap(ref1, ref2).repeat(Schedule.recurs(100)).fork
      fiber2 <- swap(ref2, ref1).repeat(Schedule.recurs(100)).fork
      _ <- (fiber1 zip fiber2).join
      value <- (ref1.get zipWith ref2.get)(_ + _)
    } yield value
  }

  /**
    * EXERCISE
    *
    * Using `STM`, implement a safe version of the swap function.
    */
  def exampleStm = {
    def swap[A](ref1: TRef[A], ref2: TRef[A]): UIO[Unit] =
      (for {
        v1 <- ref1.get
        v2 <- ref2.get
        _ <- ref2.set(v1)
        _ <- ref1.set(v2)
      } yield ()).commit

    for {
      ref1 <- TRef.make(100).commit
      ref2 <- TRef.make(0).commit
      fiber1 <- swap(ref1, ref2).repeat(Schedule.recurs(100)).fork
      fiber2 <- swap(ref2, ref1).repeat(Schedule.recurs(100)).fork
      _ <- (fiber1 zip fiber2).join
      value <- (ref1.get zipWith ref2.get)(_ + _).commit
    } yield value
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    // exampleRef.map(_.toString).flatMap(putStrLn) as 0
    for {
      fiber1 <- (exampleStm.map(_.toString).flatMap(a => putStrLn(a)) *> ZIO
        .sleep(
          1.second
        )).forever.fork
      _ <- ZIO.sleep(10.seconds) *> putStrLn("Time to wakeup") *> fiber1.interrupt
    } yield 0
}

object StmLock extends App {
  import zio.console._
  import zio.stm._
  import zio.duration._

  /**
    * EXERCISE
    *
    * Using STM, implement a simple binary lock by implementing the creation,
    * acquisition, and release methods.
    */
  class Lock private (tref: TRef[Boolean]) {
    def acquire: UIO[Unit] =
      (for {
        locked <- tref.get
        _ <- STM.check(!locked)
        _ <- tref.set(!locked)
      } yield ()).commit
    def release: UIO[Unit] =
      (for {
        locked <- tref.get
        _ <- STM.check(locked)
        _ <- tref.set(!locked)
      } yield ()).commit
  }
  object Lock {
    def make: UIO[Lock] =
      TRef.makeCommit(false).map(new Lock(_))
    // (for {
    //   tref <- TRef.make(false).commit
    // } yield new Lock(tref))
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      lock <- Lock.make
      fiber1 <- lock.acquire
        .bracket_(ZIO.sleep(1.second) *> lock.release)(
          putStrLn("Bob  : I have the lock!")
        )
        .repeat(Schedule.recurs(10))
        .fork
      fiber2 <- lock.acquire
        .bracket_(lock.release)(putStrLn("Sarah: I have the lock!"))
        .repeat(Schedule.recurs(10))
        .fork
      _ <- (fiber1 zip fiber2).join
    } yield 0) as 1
}

object StmQueue extends App {
  import zio.console._
  import zio.stm._
  import scala.collection.immutable.{Queue => ScalaQueue}

  /**
    * EXERCISE
    *
    * Using STM, implement a async queue with double back-pressuring.
    */
  class Queue[A] private (capacity: Int, queue: TRef[ScalaQueue[A]]) {
    def take: UIO[A] =
      (for {
        q <- queue.get
        // Option 1:
        // a <- q.dequeueOption match {
        //   case Some((a, as)) => queue.set(as) *> STM.succeed(a)
        //   case _             => STM.retry
        // }
        // Option 2:
        _ <- STM.check(q.nonEmpty)
        a <- queue.set(q.tail) *> STM.succeed(q.head)
      } yield a).commit
    def offer(a: A): UIO[Unit] =
      (for {
        q <- queue.get
        _ <- STM.check(q.size < capacity)
        _ <- queue.update(_ enqueue a)
      } yield ()).commit
  }
  object Queue {
    def make[A]: UIO[Queue[A]] =
      (for {
        tref <- TRef.make[ScalaQueue[A]](ScalaQueue.empty[A])
      } yield new Queue(5, tref)).commit
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    for {
      queue <- Queue.make[Int]
      // _ <- ZIO.foreach(0 to 100)(i => putStrLn(s"${i}"))
      _ <- ZIO.foreach(0 to 100)(i => queue.offer(i)).fork
      _ <- ZIO.foreach(0 to 100)(_ =>
        queue.take.flatMap(i => putStrLn(s"Got: ${i}"))
      )
    } yield 0
}

object StmLunchTime extends App {
  import zio.console._
  import zio.stm._

  /**
    * EXERCISE
    *
    * Using STM, implement the missing methods of Attendee.
    */
  final case class Attendee(state: TRef[Attendee.State]) {
    import Attendee.State._

    def isStarving: STM[Nothing, Boolean] =
      // Option 1
      for {
        s <- state.get
        r <- s match {
          case Starving =>
            STM.succeed(
              true
            ) // is the succeed necessary? could only be the boolean?
          case Full => STM.succeed(false)
        }
      } yield r

    // Option 2
    // state.get.map {
    //   case Starving => true
    //   case Full     => false
    // }

    def feed: STM[Nothing, Unit] =
      // option 1
      state.updateSome {
        case Starving => Full
      }.unit
    // unit the same as -> .as(())
    // option 2 (the simpler)
    // state.set(Full)
    // option 3
    // for {
    //   s <- state.updateSome {
    //     case Starving => Full
    //   }
    // } yield ()
  }
  object Attendee {
    sealed trait State
    object State {
      case object Starving extends State
      case object Full extends State
    }
  }

  /**
    * EXERCISE
    *
    * Using STM, implement the missing methods of Table.
    */
  final case class Table(seats: TArray[Boolean]) {
    def findEmptySeat: STM[Nothing, Option[Int]] =
      seats
        .fold[(Int, Option[Int])]((0, None)) {
          case ((index, z @ Some(_)), _) => (index + 1, z)
          case ((index, None), taken) =>
            (index + 1, if (taken) None else Some(index))
        }
        .map(_._2)

    // the attendee can only take a seat if the seat is empty. Meanwhile the attendee has to wait (STM.check)
    def takeSeat(index: Int): STM[Nothing, Unit] =
      for {
        isSeatTaken <- seats.apply(index)
        _ <- STM.check(!isSeatTaken)
        _ <- seats.update(index, _ => true)
      } yield ()

    def vacateSeat(index: Int): STM[Nothing, Unit] =
      seats.update(index, _ => false)
    // seats.update(index, a => !a).as(())
  }

  /**
    * EXERCISE
    *
    * Using STM, implement a method that feeds a single attendee.
    */
  def feedAttendee(t: Table, a: Attendee): STM[Nothing, Unit] =
    for {
      index <- t.findEmptySeat.collect { case Some(index) => index }
      _ <- t.takeSeat(index) *> a.feed *> t.vacateSeat(index)
    } yield ()

  /**
    * EXERCISE
    *
    * Using STM, implement a method that feeds only the starving attendees.
    */
  def feedStarving(table: Table, list: List[Attendee]): UIO[Unit] =
    UIO.foreachPar_(list) { attendee =>
      (for {
        isStarving <- attendee.isStarving
        _ <- if (isStarving) feedAttendee(table, attendee) else STM.succeed(())
      } yield ()).commit
    }
  // STM.collectAll(list.map(feedAttendee(table, _))).as(()).commit

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val Attendees = 100
    val TableSize = 5

    for {
      attendees <- ZIO.foreach(0 to Attendees)(i =>
        TRef
          .make[Attendee.State](Attendee.State.Starving)
          .map(Attendee(_))
          .commit
      )
      table <- TArray
        .fromIterable(List.fill(TableSize)(false))
        .map(Table(_))
        .commit
      _ <- feedStarving(table, attendees)
    } yield 0
  }
}

object StmPriorityQueue extends App {
  import zio.console._
  import zio.stm._
  import zio.duration._

  /**
    * EXERCISE
    *
    * Using STM, design a priority queue, where smaller integers are assumed
    * to have higher priority than greater integers.
    */
  // in the map data structure will contain different queque which will depend on the priority. For instance
  // 1 -> TQueue(task.....)
  // 2 -> TQueue(task.....)
  class PriorityQueue[A] private (
      minLevel: TRef[Int],
      map: TMap[Int, TQueue[A]]
  ) {
    def offer(a: A, priority: Int): STM[Nothing, Unit] =
      for {
        min <- minLevel.get
        _ <- if (priority < min)
          minLevel.set(
            priority
          ) // the minLevel has to change if a new set of values with a new priority is offer to the PriorityQueue
        else STM.unit
        otqueque <- map.get(
          priority
        ) // this will get the elements assigned to the priority key passed as argument. The get method returns an Option type because that key may not have any elements.
        tqueque <- otqueque
          .map(a => STM.succeed(a))
          .getOrElse(
            TQueue.bounded(Int.MaxValue)
          ) // the requirements do not indicate anything about a max capacity of the queue therefore is create with a max int value
        _ <- tqueque.offer(a) // this is a safe transaction operation
        _ <- map.put(priority, tqueque)
      } yield ()

    // in the take part the priority elements should be got in firt place.
    // If the tqueque gets empty the minLevel should change
    // And the key -> elements should be removed
    def take: STM[Nothing, A] =
      for {
        min <- minLevel.get
        tqueque <- map.get(min).collect { case Some(tq) => tq } // this partialfunction will only get if there is a queue assigned to the min level priority
        a <- tqueque.take
        size <- tqueque.size
        _ <- if (size == 0) setNextMinValue(min)
        else STM.unit
      } yield a

    private def setNextMinValue(min: Int): STM[Nothing, Unit] = {

      def lookUpForNextPriorityValue: STM[Nothing, Unit] =
        map.keys
          .map(_.sorted.headOption)
          .map(_.fold(Int.MaxValue)(identity))
          .flatMap(minLevel.set)

      map.delete(min) *> lookUpForNextPriorityValue
    }
  }
  object PriorityQueue {
    def make[A]: STM[Nothing, PriorityQueue[A]] = ???
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _ <- putStrLn("Enter any key to exit...")
      queue <- PriorityQueue.make[String].commit
      lowPriority = ZIO.foreach(0 to 100) { i =>
        ZIO.sleep(1.millis) *> queue
          .offer(s"Offer: ${i} with priority 3", 3)
          .commit
      }
      highPriority = ZIO.foreach(0 to 100) { i =>
        ZIO.sleep(2.millis) *> queue
          .offer(s"Offer: ${i} with priority 0", 0)
          .commit
      }
      _ <- ZIO.forkAll(List(lowPriority, highPriority)) *> queue.take.commit
        .flatMap(putStrLn(_))
        .forever
        .fork *>
        getStrLn
    } yield 0).fold(_ => 1, _ => 0)
}

object StmReentrantLock extends App {
  import zio.console._
  import zio.stm._

  private final case class WriteLock(
      writeCount: Int,
      readCount: Int,
      fiberId: Fiber.Id
  )
  private final class ReadLock private (readers: Map[Fiber.Id, Int]) {
    def total: Int = readers.values.sum

    def noOtherHolder(fiberId: Fiber.Id): Boolean =
      readers.size == 0 || (readers.size == 1 && readers.contains(fiberId))

    def readLocks(fiberId: Fiber.Id): Int =
      readers.get(fiberId).fold(0)(identity)

    def adjust(fiberId: Fiber.Id, adjust: Int): ReadLock = {
      val total = readLocks(fiberId)

      val newTotal = total + adjust

      new ReadLock(
        readers =
          if (newTotal == 0) readers - fiberId
          else readers.updated(fiberId, newTotal)
      )
    }
  }
  private object ReadLock {
    val empty: ReadLock = new ReadLock(Map())

    def apply(fiberId: Fiber.Id, count: Int): ReadLock =
      if (count <= 0) empty else new ReadLock(Map(fiberId -> count))
  }

  /**
    * EXERCISE
    *
    * Using STM, implement a reentrant read/write lock.
    */
  class ReentrantReadWriteLock(data: TRef[Either[ReadLock, WriteLock]]) {
    def writeLocks: UIO[Int] = data.get.map(_.fold(_ => 0, _.writeCount)).commit

    def writeLocked: UIO[Boolean] = writeLocks.map(_ > 0)

    def readLocks: UIO[Int] = data.get.map(_.fold(_.total, _.readCount)).commit

    def readLocked: UIO[Boolean] = readLocks.map(_ > 0)

    // val acquireRead: STM[Nothing, Int] = STM.fiberId.flatMap(fiberId => adjust)
    val read: Managed[Nothing, Int] = Managed.make {
      (STM.fiberId
        .flatMap { fiberId =>
          data.get.collect {
            case Left(readLocked) => Left(readLocked.adjust(fiberId, 1))
            case Right(wl @ WriteLock(w, r, `fiberId`)) =>
              val newTotal = r + 1
              if (newTotal < 0)
                throw new RuntimeException(
                  s"Release fiber $fiberId that has not reference"
                )
              else Right(WriteLock(w, newTotal, fiberId))
          }
        }
        .flatMap(data.set(_)) *> data.get.map(_.fold(_.total, _.readCount))).commit
    } { _ =>
      (STM.fiberId
        .flatMap { fiberId =>
          data.get.collect {
            case Left(readLocked) => Left(readLocked.adjust(fiberId, 1))
            case Right(wl @ WriteLock(w, r, `fiberId`)) =>
              val newTotal = r - 1
              if (newTotal < 0)
                throw new RuntimeException(
                  s"Release fiber $fiberId that has not reference"
                )
              else Right(WriteLock(w, newTotal, fiberId))
          }
        }
        .flatMap(data.set(_)) *> data.get.map(_.fold(_.total, _.readCount))).commit
    }

    val write: Managed[Nothing, Int] = ???
  }
  object ReentrantReadWriteLock {
    def make: UIO[ReentrantReadWriteLock] =
      TRef
        .make[Either[ReadLock, WriteLock]](Left(ReadLock.empty))
        .map(tref => new ReentrantReadWriteLock(tref))
        .commit
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = ???
}

object StmDiningPhilosophers extends App {
  import zio.console._
  import zio.stm._

  sealed trait Fork
  val Fork = new Fork {}

  final case class Placement(
      left: TRef[Option[Fork]],
      right: TRef[Option[Fork]]
  )

  final case class Roundtable(seats: Vector[Placement])

  /**
    * EXERCISE
    *
    * Using STM, implement the logic of a philosopher to take not one fork, but
    * both forks when they are both available.
    */
  def takeForks(
      left: TRef[Option[Fork]],
      right: TRef[Option[Fork]]
  ): STM[Nothing, (Fork, Fork)] =
    ???

  def putForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]])(
      tuple: (Fork, Fork)
  ) = {
    val (leftFork, rightFork) = tuple

    right.set(Some(rightFork)) *> left.set(Some(leftFork))
  }

  def setupTable(size: Int): ZIO[Any, Nothing, Roundtable] = {
    val makeFork = TRef.make[Option[Fork]](Some(Fork))

    (for {
      allForks0 <- STM.foreach(0 to size) { i => makeFork }
      allForks = allForks0 ++ List(allForks0(0))
      placements = (allForks zip allForks.drop(1)).map {
        case (l, r) => Placement(l, r)
      }
    } yield Roundtable(placements.toVector)).commit
  }

  def eat(
      philosopher: Int,
      roundtable: Roundtable
  ): ZIO[Console, Nothing, Unit] = {
    val placement = roundtable.seats(philosopher)

    val left = placement.left
    val right = placement.right

    for {
      forks <- takeForks(left, right).commit
      _ <- putStrLn(s"Philosopher ${philosopher} eating...")
      _ <- putForks(left, right)(forks).commit
      _ <- putStrLn(s"Philosopher ${philosopher} is done eating")
    } yield ()
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val count = 10

    def eaters(table: Roundtable): Iterable[ZIO[Console, Nothing, Unit]] =
      (0 to count).map { index => eat(index, table) }

    for {
      table <- setupTable(count)
      fiber <- ZIO.forkAll(eaters(table))
      _ <- fiber.join
      _ <- putStrLn("All philosophers have eaten!")
    } yield 0
  }
}

object SimpleActor extends App {
  import zio.console._
  import zio.stm._

  sealed trait Command
  case object ReadTemperature extends Command
  final case class AdjustTemperature(value: Double) extends Command

  type TemperatureActor = Command => Task[Double]

  /**
    * EXERCISE
    *
    * Using ZIO Queue and Promise, implement the logic necessary to create an
    * actor as a function from `Command` to `Task[Double]`.
    */
  def makeActor(initialTemperature: Double): UIO[TemperatureActor] = {
    type Bundle = (Command, Promise[Nothing, Double])

    ???
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    val temperatures = (0 to 100).map(_.toDouble)

    (for {
      actor <- makeActor(0)
      _ <- ZIO.foreachPar(temperatures) { temp =>
        actor(AdjustTemperature(temp))
      }
      temp <- actor(ReadTemperature)
      _ <- putStrLn(s"Final temperature is ${temp}")
    } yield 0) orElse ZIO.succeed(1)
  }
}

object Sharding extends App {

  /**
    * EXERCISE
    *
    * Create N workers reading from a Queue, if one of them fails, then wait
    * for the other ones to process their current item, but terminate all the
    * workers.
    *
    * Return the first error, or never return, if there is no error.
    */
  def shard[R, E, A](
      queue: Queue[A],
      n: Int,
      worker: A => ZIO[R, E, Unit]
  ): ZIO[R, Nothing, E] = {
    val qworker =
      ZIO.uninterruptible(ZIO.interruptible(queue.take) flatMap worker).forever
    val qworkers = List.fill(n)(qworker)

    ZIO.collectAllPar(qworkers).flatMap(_.headOption.getOrElse(ZIO.never)).flip
  }

  def run(args: List[String]) = ???
}

object CustomEnvironment extends App {
  import zio.console._
  import java.io.IOException

  type MyFx = Logging with Files

  trait Logging {
    val logging: Logging.Service
  }
  object Logging {
    trait Service {
      def log(line: String): UIO[Unit]
    }
    def log(line: String) = ZIO.accessM[Logging](_.logging.log(line))
  }
  trait Files {
    val files: Files.Service
  }
  object Files {
    trait Service {
      def read(file: String): IO[IOException, String]
    }
    def read(file: String) = ZIO.accessM[Files](_.files.read(file))
  }

  val effect =
    (for {
      file <- Files.read("build.sbt")
      _ <- Logging.log(file)
    } yield ()).provideSome[Files](env =>
      new Files with Logging {
        val logging = new Logging.Service {
          def log(line: String): UIO[Unit] = UIO(println(line))
        }
        val files = env.files
      }
    )

  def run(args: List[String]) = ???
}

object Hangman extends App {
  import Dictionary.Dictionary
  import zio.console._
  import zio.random._
  import java.io.IOException

  /**
    * EXERCISE
    *
    * Implement an effect that gets a single, lower-case character from
    * the user.
    */
  lazy val getChoice: ZIO[Console, IOException, Char] = ???

  /**
    * EXERCISE
    *
    * Implement an effect that prompts the user for their name, and
    * returns it.
    */
  lazy val getName: ZIO[Console, IOException, String] = ???

  /**
    * EXERCISE
    *
    * Implement an effect that chooses a random word from the dictionary.
    * The dictionary is `Dictionary.Dictionary`.
    */
  lazy val chooseWord: ZIO[Random, Nothing, String] = ???

  /**
    * EXERCISE
    *
    * Implement the main game loop, which gets choices from the user until
    * the game is won or lost.
    */
  def gameLoop(oldState: State): ZIO[Console, IOException, Unit] = ???

  def renderState(state: State): ZIO[Console, Nothing, Unit] = {

    /**
      *
      *  f     n  c  t  o
      *  -  -  -  -  -  -  -
      *
      *  Guesses: a, z, y, x
      *
      */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    putStrLn(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    final def failures: Int = (guesses -- word.toSet).size

    final def playerLost: Boolean = failures > 10

    final def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    final def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  sealed trait GuessResult
  object GuessResult {
    case object Won extends GuessResult
    case object Lost extends GuessResult
    case object Correct extends GuessResult
    case object Incorrect extends GuessResult
    case object Unchanged extends GuessResult
  }

  def analyzeNewInput(
      oldState: State,
      newState: State,
      char: Char
  ): GuessResult =
    if (oldState.guesses.contains(char)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(char)) GuessResult.Correct
    else GuessResult.Incorrect

  /**
    * EXERCISE
    *
    * Implement hangman using `Dictionary.Dictionary` for the words,
    * and the above helper functions.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      name <- getName
      word <- chooseWord
      state = State(name, Set(), word)
      _ <- renderState(state)
      _ <- gameLoop(state)
    } yield 0) orElse ZIO.succeed(1)
}

/**
  * GRADUATION PROJECT
  *
  * Implement a game of tic tac toe using ZIO, then develop unit tests to
  * demonstrate its correctness and testability.
  */
object TicTacToe extends App {
  import zio.console._

  sealed trait Mark {
    final def renderChar: Char = this match {
      case Mark.X => 'X'
      case Mark.O => 'O'
    }
    final def render: String = renderChar.toString
  }
  object Mark {
    case object X extends Mark
    case object O extends Mark
  }

  final case class Board private (value: Vector[Vector[Option[Mark]]]) {

    /**
      * Retrieves the mark at the specified row/col.
      */
    final def get(row: Int, col: Int): Option[Mark] =
      value.lift(row).flatMap(_.lift(col)).flatten

    /**
      * Places a mark on the board at the specified row/col.
      */
    final def place(row: Int, col: Int, mark: Mark): Option[Board] =
      if (row >= 0 && col >= 0 && row < 3 && col < 3)
        Some(
          copy(value = value.updated(row, value(row).updated(col, Some(mark))))
        )
      else None

    /**
      * Renders the board to a string.
      */
    def render: String =
      value
        .map(_.map(_.fold(" ")(_.render)).mkString(" ", " | ", " "))
        .mkString("\n---|---|---\n")

    /**
      * Returns which mark won the game, if any.
      */
    final def won: Option[Mark] =
      if (wonBy(Mark.X)) Some(Mark.X)
      else if (wonBy(Mark.O)) Some(Mark.O)
      else None

    private final def wonBy(mark: Mark): Boolean =
      wonBy(0, 0, 1, 1, mark) ||
        wonBy(0, 2, 1, -1, mark) ||
        wonBy(0, 0, 0, 1, mark) ||
        wonBy(1, 0, 0, 1, mark) ||
        wonBy(2, 0, 0, 1, mark) ||
        wonBy(0, 0, 1, 0, mark) ||
        wonBy(0, 1, 1, 0, mark) ||
        wonBy(0, 2, 1, 0, mark)

    private final def wonBy(
        row0: Int,
        col0: Int,
        rowInc: Int,
        colInc: Int,
        mark: Mark
    ): Boolean =
      extractLine(row0, col0, rowInc, colInc).collect { case Some(v) => v }.toList == List
        .fill(3)(mark)

    private final def extractLine(
        row0: Int,
        col0: Int,
        rowInc: Int,
        colInc: Int
    ): Iterable[Option[Mark]] =
      for {
        row <- (row0 to (row0 + rowInc * 2))
        col <- (col0 to (col0 + colInc * 2))
      } yield value(row)(col)
  }
  object Board {
    final val empty = new Board(Vector.fill(3)(Vector.fill(3)(None)))

    def fromChars(
        first: Iterable[Char],
        second: Iterable[Char],
        third: Iterable[Char]
    ): Option[Board] =
      if (first.size != 3 || second.size != 3 || third.size != 3) None
      else {
        def toMark(char: Char): Option[Mark] =
          if (char.toLower == 'x') Some(Mark.X)
          else if (char.toLower == 'o') Some(Mark.O)
          else None

        Some(
          new Board(
            Vector(
              first.map(toMark).toVector,
              second.map(toMark).toVector,
              third.map(toMark).toVector
            )
          )
        )
      }
  }

  val TestBoard = Board
    .fromChars(
      List(' ', 'O', 'X'),
      List('O', 'X', 'O'),
      List('X', ' ', ' ')
    )
    .get
    .render

  /**
    * The entry point to the game will be here.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn(TestBoard) as 0
}
