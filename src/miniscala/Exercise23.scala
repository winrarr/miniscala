object Exercise23 {
  sealed abstract class IntList
  case object Nil extends IntList
  case class Cons(x: Int, xs: IntList) extends IntList

  def main(args: Array[String]): Unit = {
    println(ordered(Cons(1, Cons(2, Cons(3, Cons(2, Nil))))))
  }

  def ordered(xs: IntList): Boolean = xs match {
    case Nil => true
    case Cons(n, nx) => nx match {
      case Nil => true
      case Cons(m, mx) =>
        if (n <= m) { ordered(mx) }
        false
    }
  }
}
