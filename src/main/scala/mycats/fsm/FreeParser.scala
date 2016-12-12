package mycats.fsm

import cats._
import cats.free.Free
import cats.free.Free._

/**
  * Created by Bondarenko on 12/12/16.
  */
object FreeParser extends App with Utils{

  doWhile(_ != "exit") {
    isValid
  }

  def isValid(s: String): Boolean = test(s)._2

  def test(s: String): (ParserState, Boolean) = runWith(s)(RealInterpreter)

  def runWith(s: String)(interpreter: Interpreter) = {
    s.toStream.foldLeft[NextState](InitState() -> true){ (state, ch) =>
      val (nextState, valid) = parser(state._1, ch).foldMap(interpreter)
      (nextState, valid && state._2)
    }
  }

  import Helpers.{CondHelper, Equality, FreeHelper, Or}

  def parser(s: ParserState, ch: Ch): Free[Transition, NextState] = for {
    s0 <- (( (s is InitState()) && IsDigit(ch)) ~~> DigitsState()).lift()
    s1 <- (( (s is DigitsState()) && IsDigit(ch)) ~~> DigitsState()).lift()
    s2 <- (( (s is DigitsState()) && IsOp(ch)) ~~> OpState()).lift()
    s3 <- (( (s is DigitsState()) && IsEnd(ch)) ~~> FinalState()).lift()
    s4 <- (( (s is OpState()) && IsDigit(ch)) ~~> DigitsState()).lift()
    s5 <- (  (s is FinalState()) ~~> FinalState()).lift()
    error <- (ErrorState() ~~> ErrorState()).lift()
  } yield s0 | s1 | s2 | s3 | s4 | s5 | error


  def liftCondition(t: Transition[NextState]) = liftF[Transition, NextState](t)

  type Ch = Char
  type Chars = Stream[Ch]

  //ADT
  type Interpreter = (Transition ~> Id)
  type NextState = (ParserState, Boolean)
  type CONDITION = Transition[Boolean]
  sealed trait Transition[A]
  sealed trait Condition extends Transition[Boolean]
  sealed trait ParserState extends Condition
  case class IsDigit(ch: Ch) extends Condition
  case class IsOp(ch: Ch) extends Condition
  case class IsEnd(ch: Ch) extends Condition
  case class DigitsState() extends ParserState
  case class OpState() extends ParserState
  case class InitState() extends ParserState
  case class FinalState() extends ParserState
  case class ErrorState() extends ParserState
  case class And(c1: Condition, c2: Condition) extends Condition
  case class Is(c1: ParserState, c2: ParserState) extends Condition
  case class Then(c: Condition, next: ParserState) extends Transition[NextState]

  type Cond[A] = Free[Transition, A]

  //Interpretation
  object RealInterpreter extends (Transition ~> Id) {
    def apply[A](i: Transition[A]) = i match {
      case IsDigit(ch) => isDigit(ch)
      case IsOp(ch) => isOp(ch)
      case IsEnd(ch) => isEnd(ch)
      case DigitsState() => true
      case OpState() => true
      case InitState() => true
      case FinalState() => true
      case ErrorState() => false
      case Is(c1, c2) => c1 == c2
      case And(c1, c2) => apply(c1).asInstanceOf[Boolean] && apply(c2).asInstanceOf[Boolean]
      case Then(c, next) => (next, apply(c).asInstanceOf[Boolean])

    }
  }
  def isDigit(ch: Ch): Boolean = "[0-9]".r.pattern.asPredicate().test(ch.toString)
  def isOp(ch: Ch): Boolean = "[+-]|\\*|\\/".r.pattern.asPredicate().test(ch.toString)
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

    implicit class Or(s1: NextState) {
      def |(s2: NextState): NextState = if(s1._2) s1 else s2
    }

    implicit class Equality(state: ParserState) {

      def is(s: ParserState): Is = Is(s, state)
    }
  }

}
