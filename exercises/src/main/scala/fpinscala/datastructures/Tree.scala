package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left,right) => Branch(map(left)(f),map(right)(f))
    }


  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left,right) => size(left) + size(right)
    }

  def minimum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(a) => a
      case Branch(left,right) => minimum(left) min minimum(right)
    }

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(a) => 0
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

  def fold[A,B](tree: Tree[A])(g: (B,B) => B)(f: Leaf[A] => B): B =
    tree match {
      case _ => throw new RuntimeException("")
      case Branch(left,right) => g(fold(left)(g)(f), fold(right)(g)(f))
    }

  def fold2[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold2(l)(f)(g), fold2(r)(f)(g))
  }

  def size2[A](tree: Tree[A]): Int =
    fold(tree)((x:Int,y:Int) => x + y)((_) => 1)

  def depth2[A](tree: Tree[A]): Int =
    fold(tree)((x: Int, y: Int) => 1 + (x max y))((_) => 0)

  def minimum2(tree: Tree[Int]): Int =
    fold(tree)((x: Int, y: Int) => x min y)(l => l.value)

  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)((x: Tree[B], y: Tree[B]) => Branch(x,y).asInstanceOf[Tree[B]])(l => Leaf(f(l.value)))
}