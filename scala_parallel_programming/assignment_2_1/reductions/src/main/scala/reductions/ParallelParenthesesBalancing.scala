package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    var idx = 0
    var balance = 0
    while (idx < chars.length) {
      if (balance < 0) return false
      else if (chars(idx) == '(') balance += 1
      else if (chars(idx) == ')') balance -= 1
      idx += 1
    }

    balance == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, initialUnmatchedLeft: Int, initialUnmatchedRight: Int): (Int, Int) = {

      // This version performs orders of magnitude slower than the index-based one because
      // it avoids copying array chunks (surprisingly even when using array's view) or long traversal if used with
      // List[Char] instead of the Array[Char]
      def traverseChunkSeq(chars: Seq[Char], unmatchedLeft: Int, unmatchedRight: Int): (Int, Int) =
        if (chars.isEmpty) (unmatchedLeft, unmatchedRight)
        else chars.head match {
          case '(' => traverseChunkSeq(chars.tail, unmatchedLeft + 1, unmatchedRight)
          case ')' =>
            if (unmatchedLeft > 0) traverseChunkSeq(chars.tail, unmatchedLeft - 1, unmatchedRight)
            else traverseChunkSeq(chars.tail, unmatchedLeft, unmatchedRight + 1)
          case _ => traverseChunkSeq(chars.tail, unmatchedLeft, unmatchedRight)
        }

      def traverseChunk(i: Int, unmatchedLeft: Int, unmatchedRight: Int): (Int, Int) =
        if (chars.isEmpty || i >= until) (unmatchedLeft, unmatchedRight)
        else chars(i) match {
          case '(' => traverseChunk(i + 1, unmatchedLeft + 1, unmatchedRight)
          case ')' =>
            if (unmatchedLeft > 0) traverseChunk(i + 1, unmatchedLeft - 1, unmatchedRight)
            else traverseChunk(i + 1, unmatchedLeft, unmatchedRight + 1)
          case _ => traverseChunk(i + 1, unmatchedLeft, unmatchedRight)
        }


      traverseChunk(idx, initialUnmatchedLeft, initialUnmatchedRight)
//      traverseChunkSeq(chars.view(idx, until), initialUnmatchedLeft, initialUnmatchedRight)
//      traverseChunkSeq(chars.slice(idx, until), initialUnmatchedLeft, initialUnmatchedRight)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val medium = (until + from) / 2
        val ((lo, lc), (ro, rc)) = parallel(reduce(from, medium), reduce(medium, until))
        (math.max(0, lo - rc) + ro, lc + math.max(0, rc - lo))
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
