package mycats.fsm

import cats._
import cats.free.Free
import cats.free.Free._

import scalaz.effect.IO

/**
  * Created by Bondarenko on 12/12/16.
  */
object FreeParser extends App{

  println(test("123-12\n"))

  def test(s: String) = {
    s.toStream.foldLeft[NextState](InitState("i") -> true){ (state, ch) =>
      val (nextState, valid) = parser(state._1, ch).foldMap(RealInterpreter)
      p((nextState, valid && state._2, ch))
      (nextState, valid && state._2)
    }
  }

  def p[T](t: =>T): T = {
    println(t)
    t
  }

  import Helpers.CondHelper
  import Helpers.FreeHelper
  import Helpers.A
  import Helpers.B

  def parser(s: ParserState, ch: Ch): Free[Transition, NextState] = for {
    s0 <- ((InitState(B(s)) && IsDigit(ch)) ~~> DigitsState("d")).lift()
    s1 <- ((DigitsState(B(s)) && IsDigit(ch)) ~~> DigitsState("d")).lift()
    s2 <- ((DigitsState(B(s)) && IsOp(ch)) ~~> OpState("o")).lift()
    s3 <- ((DigitsState(B(s)) && IsEnd(ch)) ~~> FinalState("f")).lift()
    s4 <- ((OpState(B(s)) && IsDigit(ch)) ~~> DigitsState("d")).lift()
    s5 <- (FinalState(B(s)) ~~> FinalState("f")).lift()
    error <- (ErrorState(B(s)) ~~> ErrorState("e")).lift()
  } yield s0 | s1 | s2 | s3 | s4 | s5 | error


  def liftCondition(t: Transition[NextState]) = liftF[Transition, NextState](t)

  type Ch = Char
  type Chars = Stream[Ch]

  //ADT
  type NextState = (ParserState, Boolean)
  type CONDITION = Transition[Boolean]
  sealed trait Transition[A]
  sealed trait Condition extends Transition[Boolean]
  sealed trait ParserState extends Condition
  case class IsDigit(ch: Ch) extends Condition
  case class IsOp(ch: Ch) extends Condition
  case class IsEnd(ch: Ch) extends Condition
  case class DigitsState(s: String) extends ParserState
  case class OpState(s: String) extends ParserState
  case class InitState(s: String) extends ParserState
  case class FinalState(s: String) extends ParserState
  case class ErrorState(s: String) extends ParserState
  case class And(c1: Condition, c2: Condition) extends Condition
  case class Then(c: Condition, next: ParserState) extends Transition[NextState]

  type Cond[A] = Free[Transition, A]

  //Interpretation
  object RealInterpreter extends (Transition ~> Id) {
    def apply[A](i: Transition[A]) = i match {
      case IsDigit(ch) => isDigit(ch)
      case IsOp(ch) => isOp(ch)
      case IsEnd(ch) => isEnd(ch)
      case DigitsState(s) => s == "d"
      case OpState(s) => s == "o"
      case InitState(s) => s == "i"
      case FinalState(s) => s == "f"
      case ErrorState(s) => false
      case And(c1, c2) => apply(c1).asInstanceOf[Boolean] && apply(c2).asInstanceOf[Boolean]
      case Then(c, next) => (next, apply(c).asInstanceOf[Boolean])

    }
  }
  def isDigit(ch: Ch): Boolean = "[0-9]".r.pattern.asPredicate().test(ch.toString)
  def isOp(ch: Ch): Boolean = "[+-]|\\*".r.pattern.asPredicate().test(ch.toString)
  def isEnd(ch: Ch): Boolean = ch == '\n'
  def a = InitState -> true

  //Helpers
  object Helpers {
    implicit class CondHelper(c: Condition) {
      def &&(c2: Condition): Condition = And(c, c2)
      def ~~>(nextState: ParserState):Transition[NextState] = Then(c, nextState)
    }

    implicit class FreeHelper(t: Transition[NextState]) {
      def lift() = liftF[Transition, NextState](t)
    }

    implicit class A(s1: NextState) {
      def |(s2: NextState): NextState = if(s1._2) s1 else s2
    }

    implicit object B {
      def apply(s: String): ParserState = s match {
        case "i" => InitState("i")
        case "d" => DigitsState("d")
        case "o" => OpState("o")
        case "f" => FinalState("f")
      }

      def apply(s: ParserState) = s match {
        case DigitsState(s) => "d"
        case OpState(s) => "o"
        case InitState(s) => "i"
        case FinalState(s) => "f"
        case ErrorState(s) => "e"
      }
    }
  }

}
