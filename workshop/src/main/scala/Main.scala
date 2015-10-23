package workshop

object Step1 { // Executing effects.

  def greetingsImpure: Unit =
    println("Hello lambda world!")

  ///////////////////

  case class PrintLn(s: String)

  type ConsoleProgram = PrintLn

  def greetings: ConsoleProgram =
    PrintLn("Hello lambda world!")

  object Interpreter {
    def run(inst: ConsoleProgram): Unit =
      println(inst.s)
  }

}

object Step2 { // Adding more instructions.

  def getLineImpure: String =
    readLine("Write something: ")

  ///////////////////

  sealed trait ConsoleInstruction[A]
  case class PrintLn(s: String) extends ConsoleInstruction[Unit]
  case class ReadLine(s: String) extends ConsoleInstruction[String]

  type ConsoleProgram[A] = ConsoleInstruction[A]

  def greetings: ConsoleProgram[Unit] =
    PrintLn("Hello lambda world!")

  def getLine: ConsoleProgram[String] =
    ReadLine("Write something: ")

  object Interpreter {
    def run[A](inst: ConsoleProgram[A]): A = inst match {
      case PrintLn(s) => println(s)
      case ReadLine(s) => readLine(s)
    }
  }

}

object Step3 { // Sequence programs

  def greetingsImpure: Unit =
    println("Hello lambda world!")

  def getLineImpure: String =
    readLine("Write something: ")

  def greetingsGetLineImpure: String = {
    greetingsImpure
    getLineImpure
  }

  ///////////////////

  sealed trait ConsoleInstruction[A]
  case class PrintLn(s: String) extends ConsoleInstruction[Unit]
  case class ReadLine(s: String) extends ConsoleInstruction[String]

  sealed trait ConsoleProgram[A]
  case class Instruction[A](inst: ConsoleInstruction[A]) extends ConsoleProgram[A]
  case class Sequence[A, B](p1: ConsoleProgram[A], p2: ConsoleProgram[B]) extends ConsoleProgram[B]

  def greetings: ConsoleProgram[Unit] =
    Instruction(PrintLn("Hello lambda world!"))

  def getLine: ConsoleProgram[String] =
    Instruction(ReadLine("Write something: "))

  def greetingsGetLine: ConsoleProgram[String] =
    Sequence(greetings, getLine)

  object Interpreter {
    def runInstruction[A](inst: ConsoleInstruction[A]): A = inst match {
      case PrintLn(s) => println(s)
      case ReadLine(s) => readLine(s)
    }

    def run[A](program: ConsoleProgram[A]): A = program match {
      case Instruction(inst) => runInstruction(inst)
      case Sequence(p1, p2) =>
        run(p1)
        run(p2)
    }
  }

}

object Step4 { // Context-dependent programs

  def echoImpure: Unit = {
    val s = readLine("Write something: ")
    println(s)
  }

  //////////////////

  sealed trait ConsoleInstruction[A]
  case class PrintLn(s: String) extends ConsoleInstruction[Unit]
  case class ReadLine(s: String) extends ConsoleInstruction[String]

  sealed trait ConsoleProgram[A]
  case class Instruction[A](inst: ConsoleInstruction[A]) extends ConsoleProgram[A]
  case class Sequence[A, B](p: ConsoleProgram[A], cont: A => ConsoleProgram[B]) extends ConsoleProgram[B]

  def greetings: ConsoleProgram[Unit] =
    Instruction(PrintLn("Hello lambda world!"))

  def getLine: ConsoleProgram[String] =
    Instruction(ReadLine("Write something: "))

  def greetingsGetLine: ConsoleProgram[String] =
    Sequence(greetings, (_: Unit) => getLine)

  def echo: ConsoleProgram[Unit] =
    Sequence(getLine, (s: String) => Instruction(PrintLn(s)))

  object Interpreter {
    def runInstruction[A](inst: ConsoleInstruction[A]): A = inst match {
      case PrintLn(s) => println(s)
      case ReadLine(s) => readLine(s)
    }

    def run[A](program: ConsoleProgram[A]): A = program match {
      case Instruction(inst) => runInstruction(inst)
      case Sequence(p, cont) =>
        val res = run(p)
        val p2 = cont(res)
        run(p2)
    }
  }

}

object Step5 { // Mix pure computations.

  def authorizedImpure: Boolean = {
    val user = readLine("User: ")
    val passwd = readLine("Passwd: ")
    (user == "lambda" && passwd == "world")
  }

  /////////////////////

  sealed trait ConsoleInstruction[A]
  case class PrintLn(s: String) extends ConsoleInstruction[Unit]
  case class ReadLine(s: String) extends ConsoleInstruction[String]

  sealed trait ConsoleProgram[A]
  case class Instruction[A](inst: ConsoleInstruction[A]) extends ConsoleProgram[A]
  case class Sequence[A, B](p: ConsoleProgram[A], cont: A => ConsoleProgram[B]) extends ConsoleProgram[B]
  case class Value[A](a: A) extends ConsoleProgram[A]

  def greetings: ConsoleProgram[Unit] =
    Instruction(PrintLn("Hello lambda world!"))

  def getLine: ConsoleProgram[String] =
    Instruction(ReadLine("Write something: "))

  def greetingsGetLine: ConsoleProgram[String] =
    Sequence(greetings, (_: Unit) => getLine)

  def echo: ConsoleProgram[Unit] =
    Sequence(getLine, (s: String) => Instruction(PrintLn(s)))

  def authorized: ConsoleProgram[Boolean] =
    Sequence(
      Instruction(ReadLine("User: ")), (user: String) =>
        Sequence(
          Instruction(ReadLine("Passwd: ")), (passwd: String) =>
            Value(user == "lambda" && passwd == "world")
        )
    )

  object Interpreter {
    def runInstruction[A](inst: ConsoleInstruction[A]): A = inst match {
      case PrintLn(s) => println(s)
      case ReadLine(s) => readLine(s)
    }

    def run[A](program: ConsoleProgram[A]): A = program match {
      case Instruction(inst) => runInstruction(inst)
      case Sequence(p, cont) =>
        val res = run(p)
        val p2 = cont(res)
        run(p2)
      case Value(a) => a
    }
  }

}

object Step6 { // Smart constructors

  sealed trait ConsoleInstruction[A]
  case class PrintLn(s: String) extends ConsoleInstruction[Unit]
  case class ReadLine(s: String) extends ConsoleInstruction[String]

  sealed trait ConsoleProgram[A]
  case class Instruction[A](inst: ConsoleInstruction[A]) extends ConsoleProgram[A]
  case class Sequence[A, B](p: ConsoleProgram[A], cont: A => ConsoleProgram[B]) extends ConsoleProgram[B]
  case class Value[A](a: A) extends ConsoleProgram[A]

  object ConsoleProgram {
    def printL(s: String): ConsoleProgram[Unit] = Instruction(PrintLn(s))
    def readL(s: String): ConsoleProgram[String] = Instruction(ReadLine(s))
  }

  import ConsoleProgram._

  def greetings: ConsoleProgram[Unit] =
    printL("Hello lambda world!")

  def getLine: ConsoleProgram[String] =
    readL("Write something: ")

  def greetingsGetLine: ConsoleProgram[String] =
    Sequence(greetings, (_: Unit) => getLine)

  def echo: ConsoleProgram[Unit] =
    Sequence(getLine, (s: String) => printL(s))

  def authorized: ConsoleProgram[Boolean] =
    Sequence(
      readL("User: "), (user: String) =>
        Sequence(
          readL("Passwd: "), (passwd: String) =>
            Value(user == "lambda" && passwd == "world")
        )
    )

  object Interpreter {
    def runInstruction[A](inst: ConsoleInstruction[A]): A = inst match {
      case PrintLn(s) => println(s)
      case ReadLine(s) => readLine(s)
    }

    def run[A](program: ConsoleProgram[A]): A = program match {
      case Instruction(inst) => runInstruction(inst)
      case Sequence(p, cont) =>
        val res = run(p)
        val p2 = cont(res)
        run(p2)
      case Value(a) => a
    }
  }

}

object Step7 { // Infix notation

  sealed trait ConsoleInstruction[A]
  case class PrintLn(s: String) extends ConsoleInstruction[Unit]
  case class ReadLine(s: String) extends ConsoleInstruction[String]

  sealed trait ConsoleProgram[A] {
    def sequence[B](cont: A => ConsoleProgram[B]): ConsoleProgram[B] =
      Sequence(this, cont)
  }
  case class Instruction[A](inst: ConsoleInstruction[A]) extends ConsoleProgram[A]
  case class Sequence[A, B](p: ConsoleProgram[A], cont: A => ConsoleProgram[B]) extends ConsoleProgram[B]
  case class Value[A](a: A) extends ConsoleProgram[A]

  object ConsoleProgram {
    def printL(s: String): ConsoleProgram[Unit] = Instruction(PrintLn(s))
    def readL(s: String): ConsoleProgram[String] = Instruction(ReadLine(s))
  }

  import ConsoleProgram._

  def greetings: ConsoleProgram[Unit] =
    printL("Hello lambda world!")

  def getLine: ConsoleProgram[String] =
    readL("Write something: ")

  def greetingsGetLine: ConsoleProgram[String] =
    greetings sequence { _ =>
      getLine
    }

  def echo: ConsoleProgram[Unit] =
    getLine sequence printL

  def authorized: ConsoleProgram[Boolean] =
    readL("User: ") sequence { user =>
      readL("Passwd: ") sequence { passwd =>
        Value(user == "lambda" && passwd == "world")
      }
    }

  object Interpreter {
    def runInstruction[A](inst: ConsoleInstruction[A]): A = inst match {
      case PrintLn(s) => println(s)
      case ReadLine(s) => readLine(s)
    }

    def run[A](program: ConsoleProgram[A]): A = program match {
      case Instruction(inst) => runInstruction(inst)
      case Sequence(p, cont) =>
        val res = run(p)
        val p2 = cont(res)
        run(p2)
      case Value(a) => a
    }
  }

}

object Step8 { // For-comprehension notation.

  sealed trait ConsoleInstruction[A]
  case class PrintLn(s: String) extends ConsoleInstruction[Unit]
  case class ReadLine(s: String) extends ConsoleInstruction[String]

  sealed trait ConsoleProgram[A] {
    def flatMap[B](cont: A => ConsoleProgram[B]): ConsoleProgram[B] =
      Sequence(this, cont)

    def map[B](f: A => B): ConsoleProgram[B] =
      flatMap(a => Value(f(a)))
  }
  case class Instruction[A](inst: ConsoleInstruction[A]) extends ConsoleProgram[A]
  case class Sequence[A, B](p: ConsoleProgram[A], cont: A => ConsoleProgram[B]) extends ConsoleProgram[B]
  case class Value[A](a: A) extends ConsoleProgram[A]

  object ConsoleProgram {
    def printL(s: String): ConsoleProgram[Unit] = Instruction(PrintLn(s))
    def readL(s: String): ConsoleProgram[String] = Instruction(ReadLine(s))
  }

  import ConsoleProgram._

  def authorized: ConsoleProgram[Boolean] =
    for {
      user <- readL("User: ")
      passwd <- readL("Passwd: ")
    } yield (user == "lambda" && passwd == "world")

  object Interpreter {
    def runInstruction[A](inst: ConsoleInstruction[A]): A = inst match {
      case PrintLn(s) => println(s)
      case ReadLine(s) => readLine(s)
    }

    def run[A](program: ConsoleProgram[A]): A = program match {
      case Instruction(inst) => runInstruction(inst)
      case Sequence(p, cont) =>
        val res = run(p)
        val p2 = cont(res)
        run(p2)
      case Value(a) => a
    }
  }

}

object Step9 { // Bonus track. Abstract our Program.

  trait UniversalInterpreter[F[_]] {
    def apply[A](fa: F[A]): A
  }

  object Program {
    case class Instruction[F[_], A](inst: F[A]) extends Program[F, A]
    case class Sequence[F[_], A, B](p: Program[F, A], cont: A => Program[F, B]) extends Program[F, B]
    case class Value[F[_], A](a: A) extends Program[F, A]

    def liftF[F[_], A](fa: F[A]): Program[F, A] = Instruction(fa)
  }
  sealed trait Program[F[_], A] {
    import Program._

    def flatMap[B](cont: A => Program[F, B]): Program[F, B] =
      Sequence(this, cont)

    def map[B](f: A => B): Program[F, B] =
      flatMap(a => Value(f(a)))

    def run(runInstruction: UniversalInterpreter[F]): A = this match {
      case Instruction(inst) => runInstruction(inst)
      case Sequence(p, cont) =>
        val res = p.run(runInstruction)
        val p2 = cont(res)
        p2.run(runInstruction)
      case Value(a) => a
    }
  }

  ////////////////////////////////////

  sealed trait ConsoleInstruction[A]
  case class PrintLn(s: String) extends ConsoleInstruction[Unit]
  case class ReadLine(s: String) extends ConsoleInstruction[String]

  type ConsoleProgram[A] = Program[ConsoleInstruction, A]

  object ConsoleProgram {
    def printL(s: String): ConsoleProgram[Unit] = Program.liftF(PrintLn(s))
    def readL(s: String): ConsoleProgram[String] = Program.liftF(ReadLine(s))
  }

  import ConsoleProgram._

  def authorized: ConsoleProgram[Boolean] =
    for {
      user <- readL("User: ")
      passwd <- readL("Passwd: ")
    } yield (user == "lambda" && passwd == "world")

  object Interpreter {
    val consoleInterpreter = new UniversalInterpreter[ConsoleInstruction] {
      def apply[A](inst: ConsoleInstruction[A]): A = inst match {
        case PrintLn(s) => println(s)
        case ReadLine(s) => readLine(s)
      }
    }
  }

}

object Step10 { // Using Scalaz

  import scalaz._, Scalaz._

  sealed trait ConsoleInstruction[A]
  case class PrintLn(s: String) extends ConsoleInstruction[Unit]
  case class ReadLine(s: String) extends ConsoleInstruction[String]

  type ConsoleProgram[A] = Free[ConsoleInstruction, A]

  object ConsoleProgram {
    def printL(s: String): ConsoleProgram[Unit] = Free.liftF(PrintLn(s))
    def readL(s: String): ConsoleProgram[String] = Free.liftF(ReadLine(s))
  }

  import ConsoleProgram._

  def authorized: ConsoleProgram[Boolean] =
    for {
      user <- readL("User: ")
      passwd <- readL("Passwd: ")
    } yield (user == "lambda" && passwd == "world")

  object Interpreter {
    val consoleInterpreter = new ~>[ConsoleInstruction, Id] {
      def apply[A](inst: ConsoleInstruction[A]): A = inst match {
        case PrintLn(s) => println(s)
        case ReadLine(s) => readLine(s)
      }
    }
  }

}

object Step11 { // Using Cats

  import cats._
  import cats.free.Free

  sealed trait ConsoleInstruction[A]
  case class PrintLn(s: String) extends ConsoleInstruction[Unit]
  case class ReadLine(s: String) extends ConsoleInstruction[String]

  type ConsoleProgram[A] = Free[ConsoleInstruction, A]

  object ConsoleProgram {
    def printL(s: String): ConsoleProgram[Unit] = Free.liftF(PrintLn(s))
    def readL(s: String): ConsoleProgram[String] = Free.liftF(ReadLine(s))
  }

  import ConsoleProgram._

  def authorized: ConsoleProgram[Boolean] =
    for {
      user <- readL("User: ")
      passwd <- readL("Passwd: ")
    } yield (user == "lambda" && passwd == "world")

  object Interpreter {
    val consoleInterpreter = new ~>[ConsoleInstruction, Id] {
      def apply[A](inst: ConsoleInstruction[A]): A = inst match {
        case PrintLn(s) => println(s)
        case ReadLine(s) => readLine(s)
      }
    }
  }

}
