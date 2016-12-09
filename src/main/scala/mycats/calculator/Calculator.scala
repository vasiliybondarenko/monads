package mycats.calculator

import cats.data.State
import cats.free.Free
import cats.free.Free._
import cats.~>
import cats.arrow.FunctionK
import cats.{Id, ~>}


/**
  * Created by Bondarenko on 12/8/16.
  */
object Calculator extends App{

  val result = program.foldMap(pureCompiler)
  println(result)

  def program: Op[Int] = for {
    x <- add(1, 2)
    y <- multiply(x, 3)
    z <- minus(y, 1)
  } yield z


  //our ADT
  sealed trait Operation[A]
  case class Add[A](a: A, b: A) extends Operation[A]
  case class Multiply[A](a: A, b: A) extends Operation[A]
  case class Minus[A](a: A, b: A) extends Operation[A]

  type Op[A] = Free[Operation, A]

  def add[A](a: A, b: A): Op[A] = liftF[Operation, A](Add(a, b))
  def multiply[A](a: A, b: A): Op[A] = liftF[Operation, A](Multiply(a, b))
  def minus[A](a: A, b: A): Op[A] = liftF[Operation, A](Minus(a, b))



  def  x(a: Int, b: Int) = a + b
  def  y(a: Int, b: Int) = a * b
  def  z(a: Int, b: Int) = a - b
  def pureCompiler: Operation ~> Id = new (Operation ~> Id) {
    def apply[A](operation: Operation[A]): Id[A] =
      operation match {
        case Add(a, b) => a
        case Multiply(a, b) => a
        case Minus(a, b) => a
      }
  }

}
