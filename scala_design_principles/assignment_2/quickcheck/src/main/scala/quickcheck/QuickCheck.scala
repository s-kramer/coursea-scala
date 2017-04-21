package quickcheck

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.oneOf
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(genHeap, Gen.const(empty))
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of heap with two elements should be the min of the elements") = forAll { (x: A, y: A) =>
    findMin(insert(y, insert(x, empty))) == Math.min(x, y)
  }

  property("empty heap with added element is not empty anymore") = forAll { (x: A) =>
    isEmpty(empty) && !isEmpty(insert(x, empty))
  }
  property("non-empty heap with removed minimal is empty again") = forAll { (x: A) =>
    isEmpty(deleteMin(insert(x, empty)))
  }

  property("multielement heap with removed minimal is not empty") = forAll { (x1: A, x2: A) =>
    !isEmpty(deleteMin(insert(x2, insert(x1, empty))))
  }

  property("minimum of two melded heaps is the minimum of one or the other") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("size of two melded heaps should be at least the size of both of the heaps") = forAll { (h1: H, h2: H) =>
    val isH1Empty = isEmpty(h1)
    val isH2Empty = isEmpty(h2)

    isEmpty(meld(h1, h2)) == (isH1Empty && isH2Empty)
  }

  def deleteMinWithLast(last: A, heap: H): Boolean = {
    if (isEmpty(heap)) true
    else {
      val newLast = findMin(heap)
      if (newLast >= last) deleteMinWithLast(newLast, deleteMin(heap))
      else false
    }
  }

  property("subsequently deleted elements of the heap form a sorted sequence") = forAll { (h: H) =>
    deleteMinWithLast(findMin(h), h)
  }

  property("subsequently deleted elements of two melded heaps form a sorted sequence") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    deleteMinWithLast(findMin(melded), melded)
  }

  property("heap melded with empty is not changed") = forAll { (h: H) =>
    val melded = meld(h, empty)

    def compareMins(h1: H, h2: H): Boolean = {
      if (isEmpty(h1)) isEmpty(h2)
      else {
        if (findMin(h1) == findMin(h2)) compareMins(deleteMin(h1), deleteMin(h2))
        else false
      }
    }

    compareMins(h, melded)
  }

  property("minimmum of melded heaps should remain the same after moving one element between heaps") =
    forAll { (h1: H, h2: H) =>
      val minH1 = findMin(h1)
      val minH2 = findMin(h2)
      val min = Math.min(minH1, minH2)

      min == findMin(meld(deleteMin(h1), insert(minH1, h2)))
    }

  property("elements removed from melded heaps should be the same as elements removed from heaps with one element transferred") =
    forAll { (h1: H, h2: H) =>
      def compareHeaps(heap1: H, heap2: H): Boolean = {
        if (isEmpty(heap1)) isEmpty(heap2)
        else {
          val minHeap1 = findMin(heap1)
          val minHeap2 = findMin(heap2)
          minHeap1 == minHeap2 && compareHeaps(deleteMin(heap1), deleteMin(heap2))
        }
      }

      compareHeaps(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
    }

}
