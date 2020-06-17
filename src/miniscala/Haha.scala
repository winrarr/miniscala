package miniscala

object Haha {

  sealed abstract class Nat
  case object Zero extends Nat
  case class Succ(n: Nat) extends Nat
}
