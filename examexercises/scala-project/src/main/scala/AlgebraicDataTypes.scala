// One very useful idiom involving variant types is op onal values. For example,
// an element of the type

// OptionNat = ⟨NaN ∣ Some(Nat)⟩ is either the trivial NaN

// Some value or else natural number wrapped inside the constructor . Define the
// Scala implementation of the type .
sealed trait OptionNat
case object NaN extends OptionNat
case class Some(nat: Int) extends OptionNat:
    require(nat >= 0, "Value must be positive")

// conservative extension of the successor func on to the type OptionNat
def succ(nat: OptionNat): OptionNat = nat match
  case NaN => NaN
  case Some(nat) => Some(nat + 1)

// Consider the following type representing represents finite mappings from
// numbers to numbers

// FiniteMap= Nat → OptionNat

// The domain of such a mapping is the set of inputs for which the result is
// implementa on of the type FiniteMap
case class Nat(nat: Int):
    require(nat >= 0, "Value must be positive")

case class FiniteMap(private val map: Map[Nat, OptionNat] = Map()):
    def put(key: Nat, value: OptionNat): FiniteMap =
      FiniteMap(map.updated(key, value))

    def get(key: Nat): OptionNat =
      map.getOrElse(key, NaN)

// emptyMap : FiniteMap representing the map with empty domain
def emptyMap(): FiniteMap =
  FiniteMap()

// extendMap : FiniteMap → Nat → Nat → FiniteMap
def extendMap(m: FiniteMap)(key: Nat)(value: OptionNat): FiniteMap =
  m.put(key, value)

def adt(): Unit =
  val x: OptionNat = Some(4)
  val y: OptionNat = NaN
  println(x)
  println(y)
  println(succ(x))
  println(succ(y))
  var map: FiniteMap = emptyMap()
  map = extendMap(map)(Nat(3))(Some(5))
  map = extendMap(map)(Nat(2))(Some(6))
  map = extendMap(map)(Nat(1))(NaN)
  println(map)
