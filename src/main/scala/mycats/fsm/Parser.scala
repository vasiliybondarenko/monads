package mycats.fsm

import scalaz.effect.IO
import scala.io._

/**
  * Created by Bondarenko on 12/11/16.
  */
object Parser extends App with Utils{

  doWhile(_ != "q")(ok)


  def p[T](f: =>T) = {
    println(f)
    f
  }

  type CHARS = Stream[Char]
  type CH = Char

  abstract  class ParserState()
  case class DigitsState(c: CHARS) extends ParserState
  case class OpState(c: CHARS) extends ParserState
  case class InitState(c: CHARS) extends ParserState
  case class SpacesAfterDigitState(c: CHARS) extends ParserState
  case class SpacesAfterOpState(c: CHARS) extends ParserState

  def isDigit(ch: CH): Boolean = "[0-9]".r.pattern.asPredicate().test(ch.toString)
  def isSpace(ch: CH): Boolean = "[ \t]".r.pattern.asPredicate().test(ch.toString)
  def isOp(ch: CH): Boolean = "[+-]|\\*".r.pattern.asPredicate().test(ch.toString)
  def isEnd(ch: CH): Boolean = ch == '\n'

  def ok(s: String): Boolean = accepts(InitState(s"$s\n".toStream))

  def accepts(s: ParserState): Boolean = {
    s match {
      case InitState(ch #:: rest) if isDigit(ch) => accepts(DigitsState(rest))
      case InitState(_) => false
      case DigitsState(ch #:: rest) if isDigit(ch) => accepts(DigitsState(rest))
      case DigitsState(ch #:: rest) if isSpace(ch) => accepts(SpacesAfterDigitState(rest))
      case DigitsState(ch #:: rest) if isOp(ch) => accepts(OpState(rest))
      case DigitsState(ch #:: rest) if isEnd(ch) => true
      case DigitsState(_) => false
      case OpState(ch #:: rest) if isSpace(ch) => accepts(SpacesAfterOpState(rest))
      case OpState(ch #:: rest) if isDigit(ch) => accepts(DigitsState(rest))
      case OpState(_) => false
      case SpacesAfterOpState(ch #:: rest) if isSpace(ch) => accepts(SpacesAfterOpState(rest))
      case SpacesAfterOpState(ch #:: rest) if isDigit(ch) => accepts(DigitsState(rest))
      case SpacesAfterOpState(_) => false
      case SpacesAfterDigitState(ch #:: rest) if isSpace(ch) => accepts(SpacesAfterDigitState(rest))
      case SpacesAfterDigitState(ch #:: rest) if isOp(ch) => accepts(OpState(rest))
      case SpacesAfterDigitState(_) => false
    }
  }

}


