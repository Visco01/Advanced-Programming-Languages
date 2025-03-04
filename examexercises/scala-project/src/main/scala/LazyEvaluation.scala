enum Stream[+A]:
  case Empty
  case Cons(hd: A, tl: () => Stream[A])

  def head: A = this match
    case Empty => throw new Exception("Empty stream")
    case Cons(hd,_) => hd

  def tail: Stream[A] = this match
    case Empty => throw new Exception("Empty stream")
    case Cons(_,tl) => tl()

enum Option[+A]:
  case None
  case Some(a:A)

import Stream.*
import Option.*

// unfold: the func on takes an ini al state and a func on f that produces both
// the next state and a value, thus building the resul ng stream. The return type
// of f is Option so that f can signal when to terminate the stream
// f(state) => new state S, new value A inside Option (can be either Some or None)
def unfold[A, S](state: S)(f: S => Option[(A, S)]): Stream[A] =
  f(state) match
    case Some((h,s)) => Cons(h, () => unfold(s)(f)) // h is head, state is the state of the next unfold.
    case None => Empty // Empty stream otherwise

// Use unfold to define a func on that given an integer n generates
// the stream of integers m >= n
def from: Int => Stream[Int] =
  (n: Int) => unfold(n)((x) => Some(x, x+1))

// Use unfold to define a func on that given a stream s and an integer n returns
// the (finite) stream of the first n elements of s

// The state is defined by the tuple (stream, n) where n is the number of elements to be taken
def take[A]: (Stream[A], Int) => Stream[A] =
  (s: Stream[A], n: Int) => unfold((s,n)) { // defining f in line
    case (Cons(hd, tl), 1) => Some(hd, (Empty, 0)) // First, the base case so that is reachable.
    case (Cons(hd, tl), n) => Some(hd, (tl(), n - 1)) // Some with the head, and new state stream tl and n - 1
    case _ => None
  }

// Write a stream dan-then-dog, where the elements of the stream alternate
// between the strings “dan.jpg” and “dog.jpg” (star ng with “dan.jpg”). More
// specifically, dan-then-dog should be a thunk that when called produces a pair
// of “dan.jpg” and a thunk that when called produces a pair of “dog.jpg” and a
// thunk that when called… etc.
def danThenDog: Stream[String] =
  unfold("dan"){
    case "dan" => Some("dan", "dog")
    case "dog" => Some("dog", "dan")
    case _ => None
  }

def danThenDog2: Stream[String] =
  unfold("dan")(x => Some(x, if x == "dan" then "dog" else "dan"))

// Write a func on cycle-list that takes a list xs and returns a stream which
// iterates the elements of xs, that is, if xs is the list 1,2,3, then
// cycle-list(xs) produces the stream 1,2,3,1,2,3,1,2,3,...
def cyclelist[A](xs: List[A]): Stream[A] =
  def loop(ys: List[A]): Stream[A] = ys match
    case Nil => loop(xs)
    case y::ys => Cons(y, () => loop(ys))
  loop(xs)


def lazyeval(): Unit =
  println("from")
  println(from(5))
  println(from(5).head)
  println(from(5).tail)
  println(from(5).tail.tail)
  println("take")
  println(take(from(5), 3))
  println(take(from(5), 3).head)
  println(take(from(5), 3).tail)
  println(take(from(5), 3).tail.tail)
  println(take(from(5), 3).tail.tail.tail)
  //println(take(from(5), 3).tail.tail.tail.tail) EMPTY STREAM
  println("danThenDog")
  println(danThenDog)
  println(danThenDog.head)
  println(danThenDog.tail)
  println(danThenDog.tail.head)
  println(danThenDog.tail.tail)
  println(danThenDog.tail.tail.head)
  println(danThenDog.tail.tail.tail)
  println(danThenDog.tail.tail.tail.head)
  println("cyclelist")
  println(cyclelist(List(1,2,3)))
  println(cyclelist(List(1,2,3)).head)
  println(cyclelist(List(1,2,3)).tail)
  println(cyclelist(List(1,2,3)).tail.tail)
  println(cyclelist(List(1,2,3)).tail.tail.tail)
  println(cyclelist(List(1,2,3)).tail.tail.tail.tail)
  println(cyclelist(List(1,2,3)).tail.tail.tail.tail.tail)
