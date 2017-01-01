package fpinscala.datastructures


sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  //Exercise  
    
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t    
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_,t) => Cons(h,t)
  }
    
  def drop[A](l: List[A], n: Int): List[A] = {
    
    @annotation.tailrec
    def go(n: Int, index: Int, l: List[A]): List[A] = {
      if (index >= n) l
      else go(n,index+1,tail(l))
    }
    go(n,0,l)    
  }
  
  def drop1[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

    
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  

  def length[A](l: List[A]): Int = {   
    @annotation.tailrec
    def go(n: Int, l: List[A]): Int = {
      l  match {
        case Nil => sys.error("init of empty list")
        case Cons(_,Nil) => n+1
        case Cons(h,t) => go(n+1,t)
      }
    }
    go(0,l)    
  }
  
   def lenth2[A](l: List[A]) =
    foldRight(l, 0)((_,acc) => acc + 1)

  
  
  
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")
    
  

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))
  
}


object testList {
  def main(args: Array[String]): Unit = {
    println("List ")
    val l = List(5,6,7,8);
    println("List pattern: %d".format(List.x));
    println("List tail:");
    println(List.tail(l))
    println(List.setHead(l, 4))
    println(List.drop(l, 2))
    println(List.init(l))
    println(List.length(l))
    
    
    
    /*
    var a = Array(1, 2, 3,4)
    println(a.toString())
    println ("Is sorted: %b".format(isSorted(a,  (a: Int,b: Int) => a > b))) 
    var b = Array(1, 2, 3,5,4)
    println ("Is sorted: %b".format(isSorted(b,  (a: Int,b: Int) => a > b)))
    * */ 
    
  }
}
