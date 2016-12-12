package mycats.fsm

import scala.io.StdIn
import scalaz.effect.IO

/**
  * Created by Bondarenko on 12/12/16.
  */
trait Utils {

  def doWhile(cond: String=>Boolean)(f: String=>Boolean) = {
    Stream.continually(sample(cond)(s => f(s"$s\n")).unsafePerformIO())
      .takeWhile(_.isDefined)
      .foreach(x => println(x.get))
  }

  private def sample(cond: String=>Boolean)(f: String=>Boolean): IO[Option[Boolean]] = {
    IO{
      StdIn.readLine()
    }.map{ s =>
      if(cond(s)) Some(f(s))
      else None
    }
  }
}
