trait IntSet:
  def union(set: IntSet) : IntSet

case class ListIntSet0(contents : List[Int]) extends IntSet:
  // Typecheck Error!
  // def union(set:ListIntSet0) : ListIntSet0 =
  //   val elements = for (el <- set.contents if !contents.contains(el)) yield el
  //   ListIntSet0(contents:::elements)

  // workaround (still ineffective)
  def union(set:IntSet) : ListIntSet0 =
    val new_elements =
    for (el <- set.asInstanceOf[ListIntSet0].contents if !contents.contains(el)) yield el
    ListIntSet0(contents:::new_elements)

case class OtherIntSet(contents: List[Int]) extends IntSet:
  def union(set: IntSet): IntSet = this // A dummy implementation

// The problem with the IntSet and ListIntSet0 types can be fixed by resor ng to the
// technique known as F-bounded polymorphism
trait F_IntSet[Rep]:
  def union(s:Rep) : Rep

case class ListIntSet1(contents : List[Int]) extends F_IntSet[ListIntSet1]:
  def union(set: ListIntSet1): ListIntSet1 =
    val elements = for (el <- set.asInstanceOf[ListIntSet1].contents if !contents.contains(el)) yield el
    ListIntSet1(contents:::elements)

// Consider the following polymorphic func ons
def test_union0[U<:IntSet](set1 : U, set2 : U) = set1.union(set2)
def test_union1[U<:F_IntSet[U]](set1 : U, set2 : U) = set1.union(set2)
// What are the return types of the two func ons? Explain how they are derived and argue which one (if any)
// is preferable as more expressive.

// Typecheck error: explain why and how to solve it
// Fix:
// trait List[+A]:
//   def prepend[B >: A](elem: B) : List[B] = elem::this

// Error in the following code:
// trait ListL[+A]:
//   def prepend(elem: A) : ListL[A] = elem::this

def fboundedpolymorphism(): Unit =
  println("F-Bounded Polymorphism")
  val intset = ListIntSet0(List(0, 1, 2, 3))
  val intset1 = OtherIntSet(List(1, 2, 3, 4))
  // This will throw a ClassCastException
  //println(intset.union(intset1))
  val intset2 = ListIntSet1(List(0, 1, 2, 3))
  val intset3 = ListIntSet1(List(1, 2, 3, 4))
  println(intset2.union(intset3))
