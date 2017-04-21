package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {col <- from until end} yield blurColumn(src, dst, col, radius)
  }

  private def blurColumn(src: Img, dst: Img, col: RGBA, radius: RGBA) = {
    for {row <- 0 until src.height if isWithinImage(src, col, row)}
      yield dst(col, row) = boxBlurKernel(src, col, row, radius)
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val numColsPerTask = math.max(1, src.width / numTasks)
    val colsForTasks = (0 until src.width).sliding(numColsPerTask, numColsPerTask)

    val tasks = for {colsForTask <- colsForTasks}
      yield task {
        for {col <- colsForTask}
          blurColumn(src, dst, col, radius)
      }
    tasks.foreach(task => task.join())
  }

}
