package mycats.realCats

import cats.free.Free
import cats.free.Free.liftF

/**
  * Created by Bondarenko on 12/7/16.
  */
object MapExamples extends App{

  //val result: Option[Int] = program1.foldMap(impureCompiler)
  //val result: Option[Int] = program2.foldMap(impureCompiler)
  val result = program1.foldMap(pureCompiler).run(Map.empty)

  println(executePure(program3))
  //println(s"RESULT: ${execute(program1)}")


  def execute[T](program: KVStore[T]): T = program.foldMap(impureCompiler)

  def executePure[T](program: KVStore[T]) = program.foldMap(pureCompiler).run(Map.empty).value

  def program1: KVStore[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", (_ + 12))
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

  def program2: KVStore[Option[Int]] =
    for {
      _ <- put("master", 1)
      _ <- update[Int]("master", (_ - 1))
      _ <- delete("master")
      n <- get[Int]("master")
    } yield n

def program3: KVStore[Unit] = for {
  c <- put("free cat", "monster")
} yield c


  sealed trait KVStoreOperation[A]
  case class Put[T](key: String, value: T) extends KVStoreOperation[Unit]
  case class Get[T](key: String) extends KVStoreOperation[Option[T]]
  case class Delete(key: String) extends KVStoreOperation[Unit]

  type KVStore[A] = Free[KVStoreOperation, A]

  // Put returns nothing (i.e. Unit).
  def put[VALUE](key: String, value: VALUE): KVStore[Unit] = liftF[KVStoreOperation, Unit](Put[VALUE](key, value))

  // Get returns a VALUE value.
  def get[VALUE](key: String): KVStore[Option[VALUE]] = liftF[KVStoreOperation, Option[VALUE]](Get[VALUE](key))

  // Delete returns nothing (i.e. Unit).
  def delete(key: String): KVStore[Unit] = liftF(Delete(key))

  // Update composes get and set, and returns nothing.
  def update[VALUE](key: String, f: VALUE => VALUE): KVStore[Unit] =
    for {
      vMaybe <- get[VALUE](key)
      _ <- vMaybe.map(v => put[VALUE](key, f(v))).getOrElse(Free.pure(()))
    } yield ()


  import cats.arrow.FunctionK
  import cats.{Id, ~>}
  import scala.collection.mutable

  // the program will crash if a key is not found,
  // or if a type is incorrectly specified.
  def impureCompiler: FunctionK[KVStoreOperation, Id]  =
    new (KVStoreOperation ~> Id) {

      // a very simple (and imprecise) key-value store
      val kvs = mutable.Map.empty[String, Any]

      def apply[A](fa: KVStoreOperation[A]): Id[A] =
        fa match {
          case Put(key, value) =>
            println(s"put($key, $value)")
            kvs(key) = value
            ()
          case Get(key) =>
            println(s"get($key)")
            kvs.get(key).map(_.asInstanceOf[A])
          case Delete(key) =>
            println(s"delete($key)")
            kvs.remove(key)
            ()
        }
    }

  import cats.data.State

  type MAP = Map[String, Any]
  type KVStoreState[A] = State[MAP, A]
  def pureCompiler: KVStoreOperation ~> KVStoreState = new (KVStoreOperation ~> KVStoreState) {
    def apply[A](operation: KVStoreOperation[A]): KVStoreState[A] =
      operation match {
        case Put(key, value) => State.modify[MAP](x => x.updated(key, value))
        case Get(key) => State.inspect(map => map.get(key).map(_.asInstanceOf[A]))
        case Delete(key) => State.modify[MAP](map => map - key)
      }
  }

}


