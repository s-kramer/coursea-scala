package kmeans

import java.util.concurrent._

import scala.collection._
import org.scalatest.{FunSuite, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import scala.math._

object KM extends KMeans

import KM._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite with Matchers{

  def checkClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points, means) == expected,
      s"classify($points, $means) should equal to $expected")
  }

  test("'classify should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()
    checkClassify(points, means, expected)
  }

  test("'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val points: GenSeq[Point] = IndexedSeq()
    val mean = Point(1, 1, 1)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val p1 = Point(1, 1, 0)
    val p2 = Point(1, -1, 0)
    val p3 = Point(-1, 1, 0)
    val p4 = Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean = Point(0, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap((mean, GenSeq(p1, p2, p3, p4)))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val p1 = Point(1, 1, 0)
    val p2 = Point(1, -1, 0)
    val p3 = Point(-1, 1, 0)
    val p4 = Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean1 = Point(1, 0, 0)
    val mean2 = Point(-1, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val expected = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))
    checkClassify(points, means, expected)
  }

  def checkParClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points.par, means.par) == expected,
      s"classify($points par, $means par) should equal to $expected")
  }

  test("'classify with data parallelism should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  test("kmeans should update the means to averages of point positions") {
    val p11 = Point(-3, 6, 0)
    val p12 = Point(-1, 6, 0)
    val p13 = Point(-3, 4, 0)
    val p14 = Point(-1, 4, 0)

    val p21 = Point(2, 3, 0)
    val p22 = Point(4, 3, 0)
    val p23 = Point(2, 1, 0)
    val p24 = Point(4, 1, 0)

    val p31 = Point(-2, -1, 0)
    val p32 = Point(0, -1, 0)
    val p33 = Point(0, -3, 0)
    val p34 = Point(-2, -3, 0)
    val points: GenSeq[Point] = IndexedSeq(p11, p12, p13, p14, p21, p22, p23, p24, p31, p32, p33, p34)

    val mean1 = Point(-4, 4, 0)
    val mean2 = Point(5, 5, 0)
    val mean3 = Point(0, -4, 0)
    val means: GenSeq[Point] = GenSeq(mean1, mean2, mean3 )

    val expectedMeans = GenSeq(Point(-2, 5, 0), Point(3, 2, 0), Point(-1, -2, 0))

    val convergedMeans = kMeans(points, means, 0.1)
    convergedMeans should contain theSameElementsInOrderAs expectedMeans
  }

}

