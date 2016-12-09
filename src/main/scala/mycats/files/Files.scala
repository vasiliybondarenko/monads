package mycats.files

import cats.{Id, ~>}
import cats.free.Free
import cats.free.Free._
import mycats.calculator.Calculator.{Add, Minus, Multiply}

import scalaz.effect.IO

/**
  * Created by Bondarenko on 12/9/16.
  */
object Files extends App{

  val result = program.foldMap(FakeInterpreter).unsafePerformIO()

  println(s"RESULT: $result")

  def program: Free[FileOperation, IO[Unit]] = for {
    lines <- read("test.txt")
    found <- find(lines, "hardwired")
    io <- show(found.getOrElse(""))
  } yield io

  //our ADT
  sealed trait FileOperation[A]
  case class Read(f: String) extends FileOperation[List[String]]
  case class Find(lines: List[String], str: String) extends FileOperation[Option[String]]
  case class Show(str: String) extends FileOperation[IO[Unit]]

  type Op[A] = Free[FileOperation, A]

  def read(f: String): Op[List[String]] = liftF[FileOperation, List[String]](Read(f))
  def find(lines: List[String], str: String): Op[Option[String]] = liftF[FileOperation, Option[String]](Find(lines, str))
  def show(str: String): Op[IO[Unit]] = liftF[FileOperation, IO[Unit]](Show(str))


  object FakeInterpreter extends (FileOperation ~> Id) {
    def apply[A](i: FileOperation[A]) = i match {
      case Read(f) => List("Load", "ReLOad", "Death magnetic", "Hardwired... To selfdistruct")
      case Find(lines, str) => lines.find(_.toLowerCase.contains(str.toLowerCase))
      case Show(s) => IO( println(s) )

    }
  }


}
