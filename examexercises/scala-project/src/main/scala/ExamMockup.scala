// Datatypes
trait Expr
case class Number(n:Int) extends Expr
case class Sum(e1 : Expr, e2 : Expr) extends Expr
case class Variable(name: String) extends Expr
case class Product(e1 : Expr, e2 : Expr) extends Expr

// Write a func on show: Expr => String that returns the representa on of a given
// expression as a string.
def show: Expr => String =
  case Number(n) => n.toString
  case Sum(a, b) => show(a) + "+" + show(b)

// Add case classes for variables and products

// Extend the show func on to deal with the addi onal cases. Make sure you get
// operator precedence right. Use parentheses, but try to use as few parentheses as
// possible. For example, Sum(Prod(2,
// Var("x")), Var("y")) should be represented as 2 * x + y, whereas Prod(Sum(2,
// Var("x")), Var("y")) should be rendered as (2 + x) * y.

def showComplete: Expr => String =
  case Number(n) => n.toString
  case Sum(a, b) => showComplete(a) + "+" + showComplete(b)
  case Variable(name) => name
  case Product(a, b) => (a, b) match
      case (Sum(_, _), Sum(_, _)) => "(" + showComplete(a) + ")*(" + showComplete(b) + ")"
      case (Sum(_, _), _) => "(" + showComplete(a) + ")*" + showComplete(b)
      case (_, Sum(_, _)) => showComplete(a) + "*(" + showComplete(b) + ")"
      case (_, _) => showComplete(a) + "*" + showComplete(b)

// Implement the toString method for the type List so that it behaves as the method
// provided by the Scala standard library.
def toString[A](xs: List[A]): String =
  def loop(xs: List[A]): String =
    xs match
      case Nil => ""
      case x::xs => xs match
          case Nil => x.toString
          case xs => x.toString + ", " + loop(xs)
  "List(" + loop(xs) + ")"

// Recursive and High-Order Functions

// Define a func on unzip that given a list of pairs List((a1,b1), ..., (an,bn))
// returns the pair of lists List(a1,..., an) and List(b1, ..., bn).

// define unzip by direct recursion
def unzip[A, B]: List[(A, B)] => (List[A], List[B]) =
  case Nil => (List(), List())
  case (x, y)::xs => {
    val (l1, l2) = unzip(xs)
    (x +: l1, y +: l2)
  }

def unzipFoldRight[A, B](l: List[(A, B)]): (List[A], List[B]) =
  foldRight(l, (List(), List()), (x: (A, B), y: (List[A], List[B])) => {
              val (l1, l2) = y
              val (a, b) = x
              (a +: l1, b +: l2)
            })

// Define the func on to scanleft from the standard library: scanleft is similar
// to foldLeft, but instead of just reducing the list, it also computes a new list
// collec ng the intermediate result of folding:
// Try with scanLeft(List(1, 2, 3, 4), "0", (b: String, el: Int) => "(" + b + " + " + el.toString() + ")")
def scanLeft[A, B](xs: List[A], b: B, f: (B, A) => B): List[B] =
    xs match
        case Nil => List(b)
        case h::tail => b::scanLeft(tail, f(b, h), f)

// Lazy evaluation and Streams

// ALL THESE FUNCTIONS ARE IN LazyEvaluation.scala

// Use unfold to define a func on from : Int => Stream[Int] that given an integer
// n generates the stream of integers m >= n

// Use unfold to define a func on take : [A] => (Stream[A], Int)) => Stream[A]
// that given a stream s and an integer n returns the (finite) stream of the first
// n elements of s


// Subtyping, Generics, and Variance

// ALL THESE FUNCTIONS ARE IN FBoundedpolymorphism.scala

def exam(): Unit =
  // Datatypes
  println("Exam")
  println("show")
  println(show(Sum(Number(1), Number(2))))
  println(show(Sum(Number(1), Sum(Number(2), Number(3)))))
  println("showComplete")
  println(showComplete(Sum(Product(Number(2), Variable("x")), Variable("y"))))
  println(showComplete(Product(Sum(Number(2), Variable("x")), Variable("y"))))
  println(showComplete(Product(Variable("x"), Sum(Number(2), Variable("y")))))
  println("toString")
  println(toString(List(1, 2, 3)))
  println(toString(List("a", "b", "c")))
  println(toString(List()))
  // Recursive and High-Order Functions
  println("unzip")
  println(unzip(List((1, 2), (3, 4), (5, 6))))
  println(unzip(List(('a', 'b'), ('c', 'd'), ('e', 'f'))))
  println("unzipFoldRight")
  println(unzipFoldRight(List((1, 2), (3, 4), (5, 6))))
  println(unzipFoldRight(List(('a', 'b'), ('c', 'd'), ('e', 'f'))))
  println("scanLeft")
  println(scanLeft(List(1, 2, 3, 4), "0", (b: String, el: Int) => "(" + b + " + " + el.toString() + ")"))
  println(foldLeft(List(1, 2, 3, 4), "0", (b: String, el: Int) => "(" + b + " + " + el.toString() + ")"))
  println(scanLeft(List(1, 2, 3, 4), 0, (b: Int, el: Int) => b + el))
  println(foldLeft(List(1, 2, 3, 4), 0, (b: Int, el: Int) => b + el))
  // Subtyping, Generics, and Variance
