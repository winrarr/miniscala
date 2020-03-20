package miniscala

object Set {
  sealed abstract class List[A] {
    def foreach[U](f: A => U): Unit

    @inline final def ++ (that: List[A]): List[A] = union(this, that)
    @inline final def -- (that: List[A]): List[A] = difference(this, that)

    @inline final def ++ (that: IterableOnce[A]): List[A] = union(this, toList[A](that))
    @inline final def -- (that: IterableOnce[A]): List[A] = difference(this, toList[A](that))
  }

  case class Nil[A]() extends List[A] {
    override def foreach[U](f: A => U): Unit = { }
  }

  case class Cons[A](x: A, cs: List[A]) extends List[A] {
    override def foreach[U](f: A => U): Unit = {
      f(x)
      cs.foreach[U](f)
    }
  }

  def main(args: Array[String]): Unit = {
    print(toList(collection.immutable.List(1, 2, 4, 6, 4, 8, 9)))
  }

  def toList[A](l: IterableOnce[A]): List[A] = {
    var res: List[A] = Nil()
    l.iterator.foreach(p => res = Cons(p, res))
    res
  }

  def length[A](xs: List[A]): Int = xs match {
    case Nil() => 0
    case Cons(_, ys) => 1 + length(ys)
  }

  def append[A](xs: List[A], x: A): List[A] = xs match {
    case Nil() => Cons[A](x, Nil[A]())
    case Cons(y, ys) => Cons[A](y, append(ys, x))
  }

  type Set[A] = List[A]

  def apply[A](elems: A*): Set[A] = {
    toList(elems)
  }

  def makeEmpty[A](): Set[A] = {
    Nil()
  }

  def isEmpty[A](set: Set[A]): Boolean = set == Nil()

  def size[A](set: Set[A]): Int = length(set)

  def add[A](set: Set[A], x: A): Set[A] = append(set, x)

  @scala.annotation.tailrec
  def contains[A](set: Set[A], x: A): Boolean = set match {
    case Nil() => false
    case Cons(c, cs) => if (c == x) true else contains(cs, x)
  }

  def remove[A](set: Set[A], x: A): Set[A] = set match {
    case Nil() => Nil()
    case Cons(c, cs) => if (c == x) remove(cs, x) else Cons(c, remove(cs, x))
  }

  def union[A](set1: Set[A], set2: Set[A]): Set[A] = set1 match {
    case Nil() => set2
    case Cons(c, Nil()) => Cons(c, set2)
    case Cons(c, cs) => Cons(c, union(cs, set2))
  }

  def intersection[A](set1: Set[A], set2: Set[A]): Set[A] = {
    @scala.annotation.tailrec
    def traverse(set1: Set[A], set2: Set[A], res: Set[A]): Set[A] = set1 match {
      case Nil() => Nil()
      case Cons(c, cs) => if (contains(set2, c)) traverse(cs, set2, Cons(c, res)) else traverse(cs, set2, res)
    }
    traverse(set1, set2, Nil())
  }

  def difference[A](set1: Set[A], set2: Set[A]): Set[A] = {
    @scala.annotation.tailrec
    def traverse(set1: Set[A], set2: Set[A], res: Set[A] = Nil[A]()): Set[A] = set1 match {
      case Nil() => Nil()
      case Cons(c, cs) => if (!contains(set2, c)) traverse(cs, set2, Cons(c, res)) else traverse(cs, set2, res)
    }
    traverse(set1, set2, Nil())
  }
}