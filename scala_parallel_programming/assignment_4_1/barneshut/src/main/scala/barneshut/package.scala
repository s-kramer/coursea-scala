import common._
import barneshut.conctrees._

package object barneshut {

  case class Boundaries(var minX: Float, var minY: Float, var maxX: Float, var maxY: Float) {
    def this() {
      this(minX = Float.MaxValue, minY = Float.MaxValue, maxX = Float.MinValue, maxY = Float.MinValue)
    }

    def width: Float = maxX - minX

    def height: Float = maxY - minY

    def size: Float = math.max(width, height)

    def centerX: Float = minX + width / 2

    def centerY: Float = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX

    def massY: Float = centerY

    def mass: Float = 0

    def total: Int = 0

    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(
                   nw: Quad, ne: Quad, sw: Quad, se: Quad
                 ) extends Quad {
    val centerX: Float = nw.centerX + nw.size / 2
    val centerY: Float = nw.centerY + nw.size / 2
    val size: Float = 2 * nw.size
    val mass: Float = sumUp(q => q.mass)
    val massX: Float = sumUp(q => q.mass * q.massX) / orOne(mass)
    val massY: Float = sumUp(q => q.mass * q.massY) / orOne(mass)

    private def orOne(x: Float): Float = if (x == 0) 1 else x

    val total: Int = sumUp(q => q.total)

    private def sumUp[T: Numeric](f: Quad => T): T = quadStream.map(f)
                                                     .reduce { (a, b) => implicitly[Numeric[T]].plus(a, b) }

    private def quadStream: Stream[Quad] = Stream(nw, ne, sw, se)

    def insert(b: Body): Fork = {
      def isInQuad(b: Body, q: Quad): Boolean =
        b.x > (q.centerX - q.size / 2) && b.x < (q.centerX + q.size / 2) &&
          b.y > (q.centerY - q.size / 2) && b.y < (q.centerY + q.size / 2)

      if (isInQuad(b, nw)) Fork(nw.insert(b), ne, sw, se)
      else if (isInQuad(b, ne)) Fork(nw, ne.insert(b), sw, se)
      else if (isInQuad(b, sw)) Fork(nw, ne, sw.insert(b), se)
      else Fork(nw, ne, sw, se.insert(b))
    }

    private[barneshut] def insertAll(b: Seq[Body]): Quad = {
      def insertHelper(q: Quad, bodies: Seq[Body]): Quad = bodies match {
        case Nil => q
        case x :: xs => insertHelper(q.insert(x), xs)
      }

      //        if (b.isEmpty) q
      //        else insertHelper(q.insert(b.head), b.tail)

      insertHelper(this, b)
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
    extends Quad {
    private val totalMass = bodies.aggregate(0F)((acc, b1) => acc + b1.mass, _ + _)
    val mass: Float = totalMass
    val massX: Float =
      bodies.aggregate(0F)((acc, b1) => acc + b1.mass * b1.x, _ + _) / (if (bodies.isEmpty) 1 else totalMass)
    val massY: Float =
      bodies.aggregate(0F)((acc, b1) => acc + b1.mass * b1.y, _ + _) / (if (bodies.isEmpty) 1 else totalMass)
    val total: Int = bodies.length

    def insert(b: Body): Quad = {
      if (size > minimumSize) {
        val halfSize = size / 2
        val quarterSize = size / 4
        val fork = Fork(
          Empty(centerX - quarterSize, centerY - quarterSize, halfSize),
          Empty(centerX + quarterSize, centerY - quarterSize, halfSize),
          Empty(centerX - quarterSize, centerY + quarterSize, halfSize),
          Empty(centerX + quarterSize, centerY + quarterSize, halfSize))
        fork.insertAll(b +: bodies)
      } else {
        Leaf(centerX, centerY, size, b +: bodies)
      }
    }

  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
        // no force
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body,
          // or recursion is needed
          if (quad.size / distance(x, y, quad.centerX, quad.centerY) < theta)
            addForce(quad.mass, quad.massX, quad.massY)
          else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize: Float = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- matrix.indices) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      def limitedSector(value: Float, minValue: Float) = ((value - minValue) / sectorSize)
                                                         .toInt max 0 min sectorPrecision - 1

      this (limitedSector(b.x, boundaries.minX), limitedSector(b.y, boundaries.minY)) += b
      this
    }

    def apply(x: Int, y: Int): ConcBuffer[Body] = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      val result = new SectorMatrix(boundaries, sectorPrecision)

      for {i <- matrix.indices} yield {
        val combined = matrix(i) combine that.matrix(i)
        result.matrix.update(i, combined)
      }
      result
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4

      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this (x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear(): Unit = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        System.currentTimeMillis() - startTime
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: $totalTime ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString: String = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString "\n"
    }
  }

}
