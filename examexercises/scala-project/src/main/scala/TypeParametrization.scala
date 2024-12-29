// Covariant Stream Trait
trait ScalaStream[+A] {
  def filter(p: A => Boolean): ScalaStream[A]
  def map[B](f: A => B): ScalaStream[B]
}

// Invariant JavaStream Trait
trait JavaStream[T] {
  def filter[U >: T](p: U => Boolean): JavaStream[T]
  def map[R, U >: T, S <: R](f: U => S): JavaStream[R]
}

// Helper Classes
class Point(val x: Double, val y: Double) {
  override def toString: String = s"Point($x, $y)"
}

class ColorPoint(x: Double, y: Double, val color: String) extends Point(x, y) {
  override def toString: String = s"ColorPoint($x, $y, $color)"
}

class Line(val m: Double, val q: Double) {
  override def toString: String = s"Line(y = $m * x + $q)"
}

// Belongs Function
def belongs(l: Line)(p: Point): Boolean = p.y == l.m * p.x + l.q

// Covariant ScalaStream Example
def pointsOnLine(pts: ScalaStream[Point], l: Line): ScalaStream[Point] =
  pts.filter(belongs(l))

// Polymorphic JavaStream Example
def javaPointsOnLine[U <: Point](pts: JavaStream[U], l: Line): JavaStream[U] =
  pts.filter(belongs(l))

def typeparametrization(): Unit =
  println("Type Parametrization")
  // Covariant ScalaStream Example
  // val coloredPoints: ScalaStream[ColorPoint] = ???
  // val line = new Line(2, 0)
  // val filteredPoints = pointsOnLine(coloredPoints, line)
  // println(s"Filtered Points: $filteredPoints")

  // // Invariant JavaStream Example
  // val javaColoredPoints: JavaStream[ColorPoint] = ???
  // val javaFilteredPoints = javaPointsOnLine(javaColoredPoints, line)
  // println(s"Java Filtered Points: $javaFilteredPoints")
