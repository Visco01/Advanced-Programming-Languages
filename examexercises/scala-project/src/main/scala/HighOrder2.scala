// two that takes a func on f : A => A as input and
// returns another func on that applies f twice
def two[A]: (A => A) => A => A =
  (f: A => A) => (a: A) => f(f(a))

// three that behaves similarly to two but applies f three  mes
def three[A]: (A => A) => (A => A) =
  (f: A => A) => (a: A) => f(f(f(a)))

// one and zero that apply f one and zero  mes, respec vely
// (make sure you have a clear idea of
// what it means to apply a func on zero  mes).
def one[A]: (A => A) => (A => A) =
  (f: A => A) => (a: A) => f(a)

def zero[A]: (A => A) => (A => A) =
  (f: A => A) => (a: A) => a

// These are all repeaters.
// The type of a repeater can be defined as follows:
type Repeater[A] = (A=>A) => (A=>A)

// Design a func on rep2nat which consumes a Repeater as
// input and produces the number of  mes it
// repeats its argument
def rep2nat[A]: Repeater[A] => (A => A) => A => A =
  (r: Repeater[A]) => (f: A => A) => (a: A) => r(f)(a)

// Write the func on repadd1 that increments a repeater by 1.
def repadd1[A]: Repeater[A] => (A => A) => A => A =
  (r: Repeater[A]) => (f: A => A) => (a: A) => r(f)(f(a))

// Write the func on nat2rep that converts a natural number n into the repeater that repeats
// its input func on n  mes. Update your defini on of to not use zero or repadd1 to get some good
// prac ce of using just func on literals.
def nat2rep[A]: Int => Repeater[A] =
  (x: Int) => (f: A => A) => {
    if x == 0 then
      (a: A) => a
    else
      (a: A) => f(nat2rep(x-1)(f)(a))
  }

// Write the func ons rep_plus and rep_prod that take two repeaters and return the repeater
// that represents, respec vely, the sum and the product of the input repeaters. Don’t use rep2nat,
// nat2rep or any built-in arithme c.
def rep_plus[A]: Repeater[A] => Repeater[A] => Repeater[A] =
  (r1: Repeater[A]) => (r2: Repeater[A]) => (f: A => A) => (a: A) => r1(f)(r2(f)(a))

def rep_mult[A]: Repeater[A] => Repeater[A] => Repeater[A] =
  (r1: Repeater[A]) => (r2: Repeater[A]) => (f: A => A) => (a: A) => r1(r2(f))(a)

// Write a func on adder that takes two func ons Int => Double and returns a new func on
// Int => Double whose results are the sum of the results of the first two func ons
type Int2Double = (Int => Double)

def adder(f1: Int2Double)(f2: Int2Double): Int2Double =
  (x: Int) => f1(x) + f2(x)

def multiplier(f1: Int2Double)(f2: Int2Double): Int2Double =
  (x: Int) => f1(x) * f2(x)

def divider(f1: Int2Double)(f2: Int2Double): Int2Double =
  (x: Int) => f1(x) / f2(x)

// Write a func on that takes a single func on op (a binary operator such as
// +) and returns a li ed version of that opera on (like adder above).
// Defined, adder, multiplierand divider in terms of lifter.
def lifter(op: (Double, Double) => Double): Int2Double => Int2Double => Int2Double =
  (f1: Int2Double) => (f2: Int2Double) => (x: Int) => op(f1(x), f2(x))

def adder2(f1: Int2Double)(f2: Int2Double): Int2Double =
  (x: Int) => lifter((x,y) => x + y)(f1)(f2)(x)

def multiplier2(f1: Int2Double)(f2: Int2Double): Int2Double =
  (x: Int) => lifter((x,y) => x * y)(f1)(f2)(x)

def divider2(f1: Int2Double)(f2: Int2Double): Int2Double =
  (x: Int) => lifter((x,y) => x / y)(f1)(f2)(x)

// Write a li ed version of the Boolean && (and) operator.
type Int2Bool = (Int => Boolean)

def lifterBoolean(op: (Boolean, Boolean) => Boolean): Int2Bool => Int2Bool => Int2Bool =
  (f1: Int2Bool) => (f2: Int2Bool) => (x: Int) => op(f1(x), f2(x))

def meet(f: Int2Bool, g: Int2Bool): Int2Bool =
  (x: Int) => lifterBoolean((x, y) => x && y)(f)(g)(x)

// Generalize meet to accept more than one predicate. That is, write a func on
// meet which, given a list of predicates, returns a single predicate that li s &&
// across all of the predicates.
def MeetL(l: List[Int=>Boolean]): (Int => Boolean) =
  (x: Int) => foldLeft(l, true, (b: Boolean, f: Int => Boolean) => b && f(x))

def MeetR(l: List[Int=>Boolean]): (Int => Boolean) =
  (x: Int) => foldRight(l, true, (f: Int => Boolean, b: Boolean) => b && f(x))

def Meet(l: List[Int=>Boolean]): (Int => Boolean) =
  (x: Int) => l match
    case Nil => true
    case f::fs => f(x) && Meet(fs)(x)

def highorder2(): Unit =
  val f = (x: Int) => x + 1
  println("two")
  println(two[Int]((x) => x+1)(0))
  println("three")
  println(three[Int]((x) => x+1)(0))
  println("one")
  println(one[Int]((x) => x+1)(0))
  println("zero")
  println(zero[Int]((x) => x+1)(0))
  println("rep2nat")
  println(rep2nat[Int](three[Int])((x)=>x+1)(0))
  println("repadd1")
  println(repadd1[Int](three[Int])(f)(0))
  println("nat2rep")
  println(nat2rep[Int](3)(f)(0))
  println("rep_plus")
  println(rep_plus[Int](three[Int])(two[Int])(f)(0))
  println("rep_mult")
  println(rep_mult[Int](three[Int])(two[Int])(f)(0))
  println("adder")
  println(adder((x) => x.toDouble)((x) => x.toDouble)(1))
  println("multiplier")
  println(multiplier((x) => x.toDouble)((x) => x.toDouble)(2))
  println("divider")
  println(divider((x) => x.toDouble)((x) => x.toDouble)(4))
  println("lifter")
  println(lifter((x, y) => x + y)((x) => x.toDouble)((x) => x.toDouble)(1))
  println("adder2")
  println(adder2((x) => x.toDouble)((x) => x.toDouble)(1))
  println("multiplier2")
  println(multiplier2((x) => x.toDouble)((x) => x.toDouble)(2))
  println("divider2")
  println(divider2((x) => x.toDouble)((x) => x.toDouble)(4))
  println("lifterBoolean")
  println(lifterBoolean((x, y) => x && y)((x) => x > 0)((x) => x > 0)(1))
  println("meet")
  println(meet((x) => x > 0, (x) => x > 2)(1))
  println("MeetL")
  println(MeetL(List((x) => x > 0, (x) => x > -2, (x) => x % 2 == 0))(6))
  println("MeetR")
  println(MeetR(List((x) => x > 0, (x) => x > -2, (x) => x % 2 == 0))(5))
  println("Meet")
  println(Meet(List((x) => x > 0, (x) => x > -2, (x) => x % 2 == 0))(6))
