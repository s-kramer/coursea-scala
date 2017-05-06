package barneshut

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection._
import scala.math._

@RunWith(classOf[JUnitRunner])
class BarnesHutSuite extends FunSuite {

  // test cases for quad tree

  import FloatOps._

  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  test("Fork with 4 empty quadrants") {
    val nw = Empty(17.5F, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 0.0f, s"${quad.mass} should be 0")
    assert(quad.massX ~= 0f, s"${quad.massX} should be 0f")
    assert(quad.massY ~= 0f, s"${quad.massY} should be 0f")
    assert(quad.total == 0, s"${quad.total} should be 1")
  }

  /*
  (-2.5, -2.5)      (2.5, -2.5)



  (-2.5, 2.5)       (2.5, 2.5)
   */

  test("Fork with 4 empty quadrants - confirmation test") {
    val nw = Empty(-2.5F, -2.5f, 5f)
    val ne = Empty(2.5f, -2.5f, 5f)
    val sw = Empty(-2.5f, 2.5f, 5f)
    val se = Empty(2.5f, 2.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 0f, s"${quad.centerX} should be 0f")
    assert(quad.centerY == 0f, s"${quad.centerY} should be 0f")
    assert(quad.mass ~= 0.0f, s"${quad.mass} should be 0")
    assert(quad.massX ~= 0f, s"${quad.massX} should be 0f")
    assert(quad.massY ~= 0f, s"${quad.massY} should be 0f")
    assert(quad.total == 0, s"${quad.total} should be 1")
  }

  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }
  /*
         *                  *
  (17.5, 27.5)        (22.5, 27.5)
       *                   *



  (17.5, 32.5)        (22.5, 32.5)
       *                   *
   */

  test("Fork with 4 leafs") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(123f, 23f, 26f, 0f, 0f)
    val b3 = new Body(123f, 18f, 31f, 0f, 0f)
    val b4 = new Body(123f, 23f, 31f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b1))
    val ne = Leaf(22.5f, 27.5f, 5f, Seq(b2))
    val sw = Leaf(17.5f, 32.5f, 5f, Seq(b3))
    val se = Leaf(22.5f, 32.5f, 5f, Seq(b4))
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 4 * 123f, s"${quad.mass} should be 492f")
    assert(quad.massX ~= 20.5f, s"${quad.massX} should be 20")
    assert(quad.massY ~= 28.5f, s"${quad.massY} should be 28")
    assert(quad.total == 4, s"${quad.total} should be 1")
  }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  test("Leaf.insert(b) should return a Fork when size exceeds minimal size") {
    val leaf = Leaf(20f, 30f, 10f, Seq())
    val b = new Body(150f, 25, 35, 0f, 0f)
    val updatedLeaf = leaf.insert(b)

    updatedLeaf match {
      case Fork(nw, ne, sw, se) =>
        assert(nw.total === 0)
        assert(ne.total === 0)
        assert(sw.total === 0)
        assert(se.total === 1)
      case _ =>
        fail("Leaf.insert(b) should have returned a Fork, was $inserted")
    }
  }

  test("Leaf.insert(b) should return a Leaf when size is smaller than minimal") {
    val b1 = new Body(150f, 20f + minimumSize, 30f + minimumSize, 0f, 0f)
    val b2 = new Body(150f, 20f - minimumSize, 30f - minimumSize, 0f, 0f)
    val leaf = Leaf(20f, 30f, minimumSize, Seq(b1))
    val updatedLeaf = leaf.insert(b2)

    updatedLeaf match {
      case l@Leaf(x, y, size, bodies) =>
        assert(x ~= 20f + minimumSize)
        assert(y ~= 30f + minimumSize)
        assert(size ~= minimumSize)
        assert(bodies.length == 2)
      case _ =>
        fail("Leaf.insert(b) should have returned a Leaf, was $inserted")
    }

  }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries: Boundaries = getBoundaries96

    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).exists(_ == body)
    assert(res, s"Body not found in the right sector")
  }

  test("'SectorMatrix.+=' should correctly add multiple bodies to the buckets of a sector matrix of size 96") {
    val body1 = new Body(6, 2, 2, 0.1f, 0.1f)
    val body2 = new Body(5, 15, 15, 0.1f, 0.1f)
    val body3 = new Body(5, 27, 27, 0.1f, 0.1f)
    val body4 = new Body(5, 39, 39, 0.1f, 0.1f)
    val body5 = new Body(5, 51, 51, 0.1f, 0.1f)
    val boundaries: Boundaries = getBoundaries96

    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body1
    sm += body2
    sm += body3
    sm += body4
    sm += body5

    def verifySingleBodyInMatrix(i: Int, j: Int, b: Body) = {
      val res = sm(i, j).size == 1 && sm(i, j).exists(_ == b)
      assert(res, s"Body not found in the sector[$i, $j]")
    }

    verifySingleBodyInMatrix(0, 0, body1)
    verifySingleBodyInMatrix(1, 1, body2)
    verifySingleBodyInMatrix(2, 2, body3)
    verifySingleBodyInMatrix(3, 3, body4)
    verifySingleBodyInMatrix(4, 4, body5)
  }

  test("'SectorMatrix.+=' should correctly add multiple bodies (some of which are in the same sector) to the buckets of a sector matrix of size 96") {
    val body1 = new Body(6, 2, 2, 0.1f, 0.1f)
    val body2 = new Body(5, 15, 15, 0.1f, 0.1f)
    val body3 = new Body(5, 16, 16, 0.1f, 0.1f)
    val body4 = new Body(5, 17, 17, 0.1f, 0.1f)
    val body5 = new Body(5, 51, 51, 0.1f, 0.1f)
    val boundaries: Boundaries = getBoundaries96

    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body1
    sm += body2
    sm += body3
    sm += body4
    sm += body5

    def verifyMultipleBodyInMatrix(i: Int, j: Int, bs: Seq[Body]): Unit = {
      val res = sm(i, j).size == bs.length && bs.forall(b => sm(i, j).exists(_ == b))
      assert(res, s"Bodies (${bs.length}) not found in the sector[$i, $j]")
    }

    verifyMultipleBodyInMatrix(0, 0, Seq(body1))
    verifyMultipleBodyInMatrix(1, 1, Seq(body2, body3, body4))
    verifyMultipleBodyInMatrix(4, 4, Seq(body5))
  }

  test("'SectorMatrix.combine should correctly combine two SectorMatrices") {
    val body1 = new Body(6, 2, 2, 0.1f, 0.1f)
    val body2 = new Body(5, 15, 15, 0.1f, 0.1f)
    val body3 = new Body(5, 16, 16, 0.1f, 0.1f)
    val body4 = new Body(5, 17, 17, 0.1f, 0.1f)
    val body5 = new Body(5, 51, 51, 0.1f, 0.1f)
    val boundaries: Boundaries = getBoundaries96

    val sm1 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    val sm2 = new SectorMatrix(boundaries, SECTOR_PRECISION)

    sm1 += body1
    sm1 += body2
    sm1 += body3

    sm2 += body4
    sm2 += body5

    val sm = sm1 combine sm2


    verifyMultipleBodyInMatrix(sm, 0, 0, Seq(body1))
    verifyMultipleBodyInMatrix(sm, 1, 1, Seq(body2, body3, body4))
    verifyMultipleBodyInMatrix(sm, 4, 4, Seq(body5))
  }

  def verifyMultipleBodyInMatrix(sm: SectorMatrix, i: Int, j: Int, bs: Seq[Body]): Unit = {
    val res = sm(i, j).size == bs.length && bs.forall(b => sm(i, j).exists(_ == b))
    assert(res, s"Bodies (${bs.length}) not found in the sector[$i, $j]")
  }

  test("'SectorMatrix.combine should correctly combine two SectorMatrices - confirmation test") {
    val body1 = new Body(6, 12, 34, 0.1f, 0.1f)
    val body2 = new Body(5, 23, 45, 0.1f, 0.1f)
    val body3 = new Body(5, 56, 9, 0.1f, 0.1f)
    val body4 = new Body(5, 8, 79, 0.1f, 0.1f)
    val body5 = new Body(5, 5, 99, 0.1f, 0.1f)
    val boundaries: Boundaries = getBoundaries96

    val sm1 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    val sm2 = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm2 += body1
    sm1 += body2
    sm2 += body3
    sm1 += body4
    sm2 += body5

    val sm = sm1 combine sm2

    // 1   13   25    37  49  61  73  85  97
    //   0    1     2   3   4   5   6   7
    verifyMultipleBodyInMatrix(sm, 0, 2, Seq(body1))
    verifyMultipleBodyInMatrix(sm, 1, 3, Seq(body2))
    verifyMultipleBodyInMatrix(sm, 4, 0, Seq(body3))
    verifyMultipleBodyInMatrix(sm, 0, 6, Seq(body4))
    verifyMultipleBodyInMatrix(sm, 0, 7, Seq(body5))
  }


  private def getBoundaries96 = {
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    boundaries
  }

  test("Simulator updateBoundaries body boundaries are the boundaries of single body simulation") {
    val simulator = new Simulator(null, null)
    val boundaries = new Boundaries()
    val body = new Body(mass = 0, x = 10, y = 10, xspeed = 0, yspeed = 0)
    val expectedBoundary = Boundaries(10, 10, 10, 10)
    val actualBoundaries = simulator.updateBoundaries(boundaries, body)

    assert(actualBoundaries === expectedBoundary)
  }

  test("Simulator updateBoundaries uses two points to create boundary") {
    val simulator = new Simulator(null, null)
    val boundaries = new Boundaries()
    val body1 = new Body(mass = 0, x = 10, y = 10, xspeed = 0, yspeed = 0)
    val body2 = new Body(mass = 0, x = 20, y = 20, xspeed = 0, yspeed = 0)
    val expectedBoundary = Boundaries(10, 10, 20, 20)
    val boundaryWithOneBody = simulator.updateBoundaries(boundaries, body1)
    val actualBoundaries = simulator.updateBoundaries(boundaryWithOneBody, body2)

    assert(actualBoundaries === expectedBoundary)
  }

  test("Simulator updateBoundaries only uttermost bodies constitute boundaries") {
    val simulator = new Simulator(null, null)
    val boundaries = new Boundaries()
    val body1 = new Body(mass = 0, x = 10, y = 10, xspeed = 0, yspeed = 0)
    val body2 = new Body(mass = 0, x = 20, y = 20, xspeed = 0, yspeed = 0)
    val body3 = new Body(mass = 0, x = 30, y = 30, xspeed = 0, yspeed = 0)
    val expectedBoundary = Boundaries(10, 10, 30, 30)
    val boundaryWithOneBody = simulator.updateBoundaries(boundaries, body1)
    val boundaryWithTwoBodies = simulator.updateBoundaries(boundaryWithOneBody, body2)
    val actualBoundaries = simulator.updateBoundaries(boundaryWithTwoBodies, body3)

    assert(actualBoundaries === expectedBoundary)
  }

  test("Simulator mergeBoundaries empty boundary doesn't change the effective boundaries") {
    val simulator = new Simulator(null, null)
    val emptyBounds = new Boundaries()
    val boundary = Boundaries(0, 0, 10, 10)

    val actualBoundaries = simulator.mergeBoundaries(emptyBounds, boundary)
    assert(actualBoundaries === Boundaries(0, 0, 10, 10))
  }

  test("Simulator mergeBoundaries two non-empty boundaries are summed") {
    val simulator = new Simulator(null, null)
    val boundary1 = Boundaries(0, 0, 10, 10)
    val boundary2 = Boundaries(-10, -15, 0, 0)

    val actualBoundaries = simulator.mergeBoundaries(boundary1, boundary2)
    assert(actualBoundaries === Boundaries(-10, -15, 10, 10))
  }

  test("Simulator mergeBoundaries only uttermost boundaries constitute result") {
    val simulator = new Simulator(null, null)
    val boundary1 = Boundaries(0, 0, 10, 10)
    val boundary2 = Boundaries(-10, -15, 0, 0)
    val boundary3 = Boundaries(-30, -35, -20, -20)

    val boundariesAfterFirstMerge = simulator.mergeBoundaries(boundary1, boundary2)
    val actualBoundaries = simulator.mergeBoundaries(boundariesAfterFirstMerge, boundary3)
    assert(actualBoundaries === Boundaries(-30, -35, 10, 10))
  }

}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }

}

