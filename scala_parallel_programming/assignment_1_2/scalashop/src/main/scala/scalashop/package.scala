
import common._

import scala.collection.immutable.Stream.Empty

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Restricts the coordinates to the size of specified image. */
  def isWithinImage(img: Img, x: Int, y: Int): Boolean = {
    x == clamp(x, 0, img.width - 1) && y == clamp(y, 0, img.height - 1)
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private[scalashop] val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, col: Int, row: Int, radius: Int): RGBA = {
    val possibleComponents = for {i <- col - radius to col + radius
                                  j <- row - radius to row + radius if isWithinImage(src, i, j)}
      yield src(i, j)

    def sumRgbaComponents(xs: Seq[RGBA]): (Long, Long, Long, Long) = xs.foldLeft((0L, 0L, 0L, 0L)) {
      case ((ar, ag, ab, aa), el) => (ar + red(el), ag + green(el), ab + blue(el), aa + alpha(el))
    }

    val (r, g, b, a) = sumRgbaComponents(possibleComponents)


    val componentCount = possibleComponents.length
    rgba((r / componentCount).toInt, (g / componentCount).toInt, (b / componentCount).toInt, (a / componentCount).toInt)
  }

}
