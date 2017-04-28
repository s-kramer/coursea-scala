package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelParenthesesBalancing._

@RunWith(classOf[JUnitRunner])
class ParallelParenthesesBalancingSuite extends FunSuite {

  def check(input: String, expected: Boolean): Unit = {
    def balance(chars: Array[Char]) = parBalance(chars, 1)

    assert(balance(input.toArray) == expected,
      s"balance($input) should be $expected")
  }

  test("balance should work for empty string") {
    check("", true)
  }

  test("balance should work for string of length 1") {

    check("(", false)
    check(")", false)
    check(".", true)
  }

  test("balance should work for string of length 2") {

    check("()", true)
    check(")(", false)
    check("((", false)
    check("))", false)
    check(".)", false)
    check(".(", false)
    check("(.", false)
    check(").", false)
  }

  test("balance should work for string of length 3 and more") {
    check(")))(.", false)
    check("((((()))))", true)
    check("()()()()()", true)
    check("((((())))))))))", false)
  }

}