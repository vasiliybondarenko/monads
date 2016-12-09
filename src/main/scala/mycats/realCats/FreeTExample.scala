package mycats.realCats

/**
  * Created by Bondarenko on 12/8/16.
  */
object FreeTExample extends App{
  import cats.free._
  import cats._
  import cats.data._
  import TeletypeOps._

  def program(implicit TO : TeletypeOps[TeletypeState]) : TeletypeT[TeletypeState, Unit] = {
    for {
      userSaid <- TO.readLine("what's up?!")
      _ <- TO.log(s"user said : $userSaid")
      _ <- TO.writeLine("thanks, see you soon!")
    } yield ()
  }

  val state = program.foldMap(interpreter)


  val initialState = Nil


  val (stored, _) = state.run(initialState).value


  /* A base ADT for the user interaction without state semantics */
  sealed abstract class Teletype[A] extends Product with Serializable
  // defined class Teletype

  final case class WriteLine(line : String) extends Teletype[Unit]
  // defined class WriteLine

  final case class ReadLine(prompt : String) extends Teletype[String]
  // defined class ReadLine

  type TeletypeT[M[_], A] = FreeT[Teletype, M, A]
  // defined type alias TeletypeT

  type Log = List[String]
  // defined type alias Log

  /** Smart constructors, notice we are abstracting over any MonadState instance
    *  to potentially support other types beside State
    */
  class TeletypeOps[M[_]](implicit MS : MonadState[M, Log]) {
    def writeLine(line : String) : TeletypeT[M, Unit] =
      FreeT.liftF[Teletype, M, Unit](WriteLine(line))
    def readLine(prompt : String) : TeletypeT[M, String] =
      FreeT.liftF[Teletype, M, String](ReadLine(prompt))
    def log(s : String) : TeletypeT[M, Unit] =
      FreeT.liftT[Teletype, M, Unit](MS.modify(s :: _))
  }
  // defined class TeletypeOps

  object TeletypeOps {
    implicit def teleTypeOpsInstance[M[_]](implicit MS : MonadState[M, Log]) : TeletypeOps[M] = new TeletypeOps
  }
  // defined object TeletypeOps
  // warning: previously defined class TeletypeOps is not a companion to object TeletypeOps.
  // Companions must be defined together; you may wish to use :paste mode for this.

  type TeletypeState[A] = State[List[String], A]





  def interpreter = new (Teletype ~> TeletypeState) {
    def apply[A](fa : Teletype[A]) : TeletypeState[A] = {
      fa match {
        case ReadLine(prompt) =>
          println(prompt)
          val userInput = scala.io.StdIn.readLine()
          StateT.pure[Eval, List[String], A](userInput)
        case WriteLine(line) =>
          StateT.pure[Eval, List[String], A](println(line))
      }
    }
  }
  // interpreter: cats.~>[Teletype,TeletypeState]


}
