// true if and only if the input list is empty
def isEmpty[A] : List[A] => Boolean =
  case Nil => true
  case _ => false

// Return the first element of the list, if the input list is non-empty
def head[A] : List[A] => A =
  case Nil => throw new Exception("Empty list")
  case x::xs => x

// Return the rest of the list (empty, if the list is empty)
def tail[A] : List[A] => List[A] =
  case Nil => List()
  case x::xs => xs

// Return the list of all elements of the input list except the first n ones,
// or else the empty list if the input list has less than n elements. If n is
// negative, does not drop any elements.
def drop[A](n: Int) : List[A] => List[A] =
  case Nil => List()
  case x::xs => n match
      case n if n <= 0 => x::xs
      case _ => drop(n-1)(xs)

// Compute the length of a list
def length[A] : List[A] => Int =
  case Nil => 0
  case x::xs => 1 + length(xs)

// Return a new list with x at the end of list xs
def snoc[A](x:A, xs: List[A]) : List[A] =
  xs match
    case Nil => List(x)
    case _ => xs:::(List(x))

// Return a new list with ys at the end of list xs
def append[A](xs: List[A], ys: List[A]) : List[A] = xs match
  case Nil => ys
  case _ => xs:::ys

// Return the reverse of the input list
def reverse[A] : List[A] => List[A] =
  case Nil => List()
  case x::xs => reverse(xs):::List(x)

// Return the list of the elements that occur at odd positions
def odds[A](xs: List[A]) : List[A] = xs match
  case Nil => List()
  case x::xs => x::odds(drop(1)(xs))

// Return the two halves of the input list
def split[A] : List[A] => (List[A], List[A]) =
  case Nil => (List(), List())
  case xs => {
    val n = length(xs)
    def loop(c: Int, ys: List[A]): List[A] =
      ys match
        case Nil => List()
        case y::ys if c < n/2 => y::loop(c+1, ys)
        case y::ys if c >= n/2 => Nil
    val first = loop(0, xs)
    var second = loop(0, drop(n/2)(xs))
    second = if (n % 2 == 0) second else second:::drop(n-1)(xs)
    (first, second)
  }

// Return a new list by reordering the input according to the ordering lt
def msort[A](lt : (A,A) => Boolean, xs: List[A]) : List[A] =
  def merge(xs: List[A], ys: List[A]): List[A] = (xs, ys) match
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x::xs, y::ys) => lt(x, y) match
        case true => x::merge(xs, y::ys)
        case _ => y::merge(x::xs, ys)
  xs match
    case Nil => List()
    case x::Nil => List(x)
    case _ => {
      val (first, second) = split(xs)
      merge(msort(lt, first), msort(lt, second))
    }

// Given a list of chars, return the corresponding list of words,
// where a word is a list of chars other than the empty space
def words(xs: List[Char]) : List[List[Char]] = xs match
  case Nil => List()
  case xs => walk(List(), xs)

// Hint use an auxiliary function which returns the words of the list append(w,xs),
// where w the (prefix of) the word under consideration
def walk(w: List[Char], xs: List[Char]) : List[List[Char]] =
  xs match
    case Nil => List(w)
    case x::xs => x match
        case ' ' => w::walk(List(), xs)
        case _ => walk(snoc(x, w), xs)

def collections(): Unit =
  val ints = List(1, 2, 3, 4, 5, 6)
  println("isEmpty")
  println(isEmpty(ints))
  println(isEmpty(List()))
  println("head")
  println(head(ints))
  //println(head(List()))
  println("tail")
  println(tail(ints))
  println(tail(List()))
  println("drop")
  println(drop(2)(ints))
  println(drop(4)(ints))
  println(drop(-1)(ints))
  println("length")
  println(length(ints))
  println("snoc")
  println(snoc(7, ints))
  println("append")
  println(append(ints, List(7, 8, 9)))
  println("reverse")
  println(reverse(ints))
  println("odds")
  println(odds(ints))
  println("split")
  println(split(List(1)))
  println("merge sort")
  println(msort((x: Int, y: Int) => x < y, reverse(ints)))
  println("words")
  println(words("Hello World Hellooo".toList))
