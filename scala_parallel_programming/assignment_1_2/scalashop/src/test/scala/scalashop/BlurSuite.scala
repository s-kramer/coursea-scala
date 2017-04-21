package scalashop

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite with BeforeAndAfter {
  test("boxBlurKernel should correctly handle radius 0") {
    val src = new Img(5, 5)

    for (x <- 0 until 5; y <- 0 until 5)
      src(x, y) = rgba(x, y, x + y, math.abs(x - y))

    for (x <- 0 until 5; y <- 0 until 5)
      assert(boxBlurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
        "boxBlurKernel(_,_,0) should be identity.")
  }

  test("boxBlurKernel should return the correct value on an interior pixel " +
    "of a 3x4 image with radius 1") {
    val src = new Img(3, 4)
    // @formatter:off
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16
    // @formatter:on

    assert(boxBlurKernel(src, 1, 2, 1) === 12,
      s"(boxBlurKernel(1, 2, 1) should be 12, " +
        s"but it's ${boxBlurKernel(src, 1, 2, 1)})")
  }

  test("HorizontalBoxBlur.blur with radius 1 should correctly blur the upper part of 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    // @formatter:off
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    // @formatter:on

    HorizontalBoxBlur.blur(src, dst, 0, 2, 1)

    check(dst, 0, 0, 2)
    check(dst, 1, 0, 2)
    check(dst, 2, 0, 3)
    check(dst, 0, 1, 3)
    check(dst, 1, 1, 4)
    check(dst, 2, 1, 4)
    check(dst, 0, 2, 0)
    check(dst, 1, 2, 0)
    check(dst, 2, 2, 0)
  }

  test("VerticalBoxBlur.blur with radius 1 should correctly blur 2 columns of 3x3 image") {
    val w = 3
    val h = 3
    val src = new Img(w, h)
    val dst = new Img(w, h)
    // @formatter:off
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
    // @formatter:on

    VerticalBoxBlur.blur(src, dst, 0, 2, 1)

    check(dst, 0, 0, 2)
    check(dst, 1, 0, 2)
    check(dst, 2, 0, 0)
    check(dst, 0, 1, 3)
    check(dst, 1, 1, 4)
    check(dst, 2, 1, 0)
    check(dst, 0, 2, 5)
    check(dst, 1, 2, 5)
    check(dst, 2, 2, 0)
  }

  def check(dst: Img, x: RGBA, y: RGBA, expected: RGBA): Unit =
    assert(dst(x, y) == expected,
      s"(destination($x, $y) should be $expected)")

  test("VerticalBoxBlur.blur with radius 2 should correctly blur the entire 4x3 image") {
    val w = 4
    val h = 3
    val src = get4Over3
    val dst = new Img(w, h)

    VerticalBoxBlur.blur(src, dst, 0, 4, 2)

    check(dst, 0, 0, 4)
    check(dst, 1, 0, 5)
    check(dst, 2, 0, 5)
    check(dst, 3, 0, 6)
    check(dst, 0, 1, 4)
    check(dst, 1, 1, 5)
    check(dst, 2, 1, 5)
    check(dst, 3, 1, 6)
    check(dst, 0, 2, 4)
    check(dst, 1, 2, 5)
    check(dst, 2, 2, 5)
    check(dst, 3, 2, 6)
  }

  test("HorizontalBoxBlur.blur with radius 2 should correctly blur the entire 4x3 image") {
    val w = 4
    val h = 3
    val src = get4Over3
    val dst = new Img(w, h)

    HorizontalBoxBlur.blur(src, dst, from = 0, end = 3, radius = 2)

    check(dst, 0, 0, 4)
    check(dst, 1, 0, 5)
    check(dst, 2, 0, 5)
    check(dst, 3, 0, 6)
    check(dst, 0, 1, 4)
    check(dst, 1, 1, 5)
    check(dst, 2, 1, 5)
    check(dst, 3, 1, 6)
    check(dst, 0, 2, 4)
    check(dst, 1, 2, 5)
    check(dst, 2, 2, 5)
    check(dst, 3, 2, 6)
  }

  test("Parallel and sequential HorizontalBoxBlur.blurs should give the same results 4x3 image") {
    val w = 4
    val h = 3
    val src = get4Over3
    val dstSeq = new Img(w, h)
    val dstPar = new Img(w, h)

    HorizontalBoxBlur.blur(src, dstSeq, from = 0, end = 3, radius = 2)
    HorizontalBoxBlur.parBlur(src, dstPar, numTasks = 4, radius = 2)

    assert(dstSeq.data === dstPar.data)
  }

  test("Parallel and sequential VerticalBoxBlur.blurs should give the same results 4x3 image") {
    val w = 4
    val h = 3
    val src = get4Over3
    val dstSeq = new Img(w, h)
    val dstPar = new Img(w, h)

    VerticalBoxBlur.blur(src, dstSeq, from = 0, end = 4, radius = 2)
    VerticalBoxBlur.parBlur(src, dstPar, numTasks = 4, radius = 2)

    assert(dstSeq.data === dstPar.data)
  }

  test("isWithinImage") {
    val src: Img = get4Over3

    assert(isWithinImage(src, 0, 0) === true)
    assert(isWithinImage(src, 0, 2) === true)
    assert(isWithinImage(src, 3, 0) === true)
    assert(isWithinImage(src, 3, 2) === true)

    assert(isWithinImage(src, 1, 1) === true)
    assert(isWithinImage(src, 2, 1) === true)
    assert(isWithinImage(src, 3, 2) === true)

    assert(isWithinImage(src, -1, -1) === false)

    assert(isWithinImage(src, -1, 0) === false)
    assert(isWithinImage(src, -2, 1) === false)

    assert(isWithinImage(src, 0, 3) === false)
    assert(isWithinImage(src, 2, 4) === false)

    assert(isWithinImage(src, 3, 4) === false)

  }

  def get4Over3: Img = {
    val w = 4
    val h = 3
    val src = new Img(w, h)
    // @formatter:off
    src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
    src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
    src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11
    // @formatter:on

    src
  }

}
