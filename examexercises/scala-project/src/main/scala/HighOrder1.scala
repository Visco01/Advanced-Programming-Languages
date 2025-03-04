def foldRight[A, B](xs: List[A], b: B, f: (A, B) => B): B =
  xs match
    case Nil => b
    case x::xs => f(x, foldRight(xs, b, f))

def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B): B =
  xs match
    case Nil => b
    case x::xs => foldLeft(xs, f(b, x), f)

// takes a
// list of func ons f1,...,fn and computes the composi on f1 o...o fn, so that
// pipe(List(f1,...,fn))(a) = f1(...fn(a)...)
// [Hint: fold the composi on func on over the input list of func ons].
def pipe[A]: List[A=>A] => A => A =
  case Nil => (a: A) => a
  case x::xs => (a: A) => pipe(xs)(x(a))

// given an element x and a list of func on
// fs returns the list obtained by applying to a each of the func ons in fs: mapfuns(a,List(f1,...,fn))
// = List(f1(a),...fn(a)). Define the func on by direct recursion first, and then using foldLeft.
def mapFuns[A](a: A, xs: List[A=>A]): List[A] =
  xs match
    case Nil => List()
    case x::xs => x(a)::mapFuns(a, xs)

def mapFunsFoldLeft[A](a: A, xs: List[A=>A]): List[A] =
  foldLeft(xs, List[A](), (b: List[A], f: A=>A) => b:::List(f(a)))

//Define the following functions by direct recursion
//checks that all the elements in the list as sa sfy the predicate p.
def forAll[A](xs: List[A], p: A => Boolean): Boolean =
  xs match
    case Nil => true
    case x::xs => if p(x) then forAll(xs, p) else false

//checks that one or more elements in the list as sa sfies the predicate p
def exists[A](xs:List[A], p:A => Boolean): Boolean =
  xs match
    case Nil => true
    case x::xs => if p(x) then true else exists(xs, p)

// checks that exactly one element in the list as sa sfies the predicate p.
def existsOne[A](xs:List[A], p:A => Boolean): Boolean =
  xs match
    case Nil => false
    case x::xs => if p(x) then if forAll(xs, (y: A) => !p(y)) then true else false else existsOne(xs, p)

// Rewrite the func ons forAll and exists using  olding
// write a version of the two func ons using foldLeft
def forAllFoldLeft[A](xs: List[A], p: A => Boolean): Boolean =
  foldLeft(xs, true, (b: Boolean, x: A) => (b && p(x)))

def existsFoldLeft[A](xs:List[A], p:A => Boolean): Boolean =
  foldLeft(xs, false, (b: Boolean, x: A) => (b || p(x)))

// write a version of the two func ons using foldRight
def forAllFoldRight[A](xs: List[A], p: A => Boolean): Boolean =
  foldRight(xs, true, (x: A, b: Boolean) => (b && p(x)))

def existsFoldRight[A](xs:List[A], p:A => Boolean): Boolean =
  foldRight(xs, false, (x: A, b: Boolean) => (b || p(x)))

def quicksort(xs: List[Int]): List[Int] =
  xs match
    case Nil => List()
    case x::xs => {
      val less: List[Int] = (x::xs).filter((a)=>(a < x))
      val equal: List[Int] = (x::xs).filter((a)=>(a == x))
      val greater: List[Int] = (x::xs).filter((a)=>(a > x))
      quicksort(less):::equal:::quicksort(greater)
    }

//Find those elements of a list of strings whose length is strictly greater than 3.
def stringGreater(xs: List[String], k: Int): List[String] =
  foldLeft(xs, List[String](), (l: List[String], b: String) => (if b.length > k then l:::List(b) else l))

// Add 1.0 to every element of a list of floats.
def add1(xs: List[Float]): List[Float] =
  foldRight(xs, List[Float](), (b: Float, l: List[Float]) => List(b + 1.0f):::l)

// Given a list of strings strs and another string sep, produce the string that contains every element
// of strs separated by sep. For example, given inputs List("hi"
// ,bye") and ";"
// , produce
// "hi;bye"
def interpolate(xs: List[String], sep: String): String =
  foldLeft(xs, "", (b: String, l: String) => (b+sep+l)).drop(sep.length)

def highorder1(): Unit =
  val ints = List(1, 2, 7, 1, 5, 6)
  println("foldRight")
  println(foldRight(ints, 0, (x: Int, y: Int) => x + y))
  println("foldLeft")
  println(foldLeft(ints, 0, (x: Int, y: Int) => x + y))
  println("pipe")
  println(pipe(List((x: Int) => x + 1, (x: Int) => x * 2, (x: Int) => x - 3))(5))
  println("mapFuns")
  println(mapFuns(5, List((x: Int) => x + 1, (x: Int) => x * 2, (x: Int) => x - 3)))
  println("mapFunsFoldLeft")
  println(mapFunsFoldLeft(5, List((x: Int) => x + 1, (x: Int) => x * 2, (x: Int) => x - 3)))
  println("forAll")
  println(forAll(ints, (x: Int) => x > 0))
  println("exists")
  println(exists(ints, (x: Int) => x > 5))
  println("existsOne")
  println(existsOne(ints, (x: Int) => x > 5))
  println(existsOne(ints, (x: Int) => x > 2))
  println("forAllFoldLeft")
  println(forAllFoldLeft(ints, (x: Int) => x > 0))
  println(forAllFoldLeft(ints, (x: Int) => x > 5))
  println("existsFoldLeft")
  println(existsFoldLeft(ints, (x: Int) => x > 5))
  println(existsFoldLeft(ints, (x: Int) => x > 20))
  println("forAllFoldRight")
  println(forAllFoldRight(ints, (x: Int) => x > 0))
  println(forAllFoldRight(ints, (x: Int) => x > 5))
  println("existsFoldRight")
  println(existsFoldRight(ints, (x: Int) => x > 5))
  println(existsFoldRight(ints, (x: Int) => x > 20))
  println("quicksort")
  println(quicksort(ints))
  val strings = List("hello", "world", "scala", "programming", "language")
  println("stringGreater")
  println(stringGreater(strings, 7))
  println("add1")
  println(add1(List(1.0f, 2.0f, 3.0f, 4.0f, 5.0f)))
  println("interpolate")
  println(interpolate(strings, ";"))
