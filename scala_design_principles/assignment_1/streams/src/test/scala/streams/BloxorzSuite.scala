package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level: String =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }


  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("new neighbors of initial position return legal neighbors and moves") {
    new Level1 {
      assert(newNeighborsOnly(neighborsWithHistory(startBlock, List[Move]()), Set[Block]()).toSet ==
        Set(
          (Block(Pos(2, 1), Pos(3, 1)), List(Down)),
          (Block(Pos(1, 2), Pos(1, 3)), List(Right))
        )
      )
    }
  }

  test("new neighbors of initial position with non-empty moves list return legal neighbors and extended moves") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet == Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ))
    }
  }

  test("from for single move") {
    new Level1 {
      private val initialStream = Set((Block(Pos(1, 1), Pos(1, 1)), List())).toStream
      private val initialExplored: Set[Block] = Set()
      private val actual = from(initialStream, initialExplored).take(2).toSet
      assert(
        actual ==
          Set((Block(Pos(2, 1), Pos(3, 1)), List(Down)), (Block(Pos(1, 2), Pos(1, 3)), List(Right)))
      )
    }
  }

  test("new neighbors after a few moves") {
    new Level1 {
      private val initialStream = Set((Block(Pos(2, 4), Pos(3, 4)), List(Right, Right, Right, Down))).toStream
      private val initialExplored: Set[Block] = Set(Block(Pos(2, 3), Pos(3, 3)))
      private val actual = from(initialStream, initialExplored).take(1).toSet
      assert(
        actual ==
          Set((Block(Pos(2, 5), Pos(3, 5)), List(Right, Right, Right, Right, Down)))
      )
    }
  }

  trait Level2 extends SolutionChecker {
    /* terrain for level 2*/

    val level: String =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |----------
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List()
  }

  test("no solution available") {
    new Level2 {
      assert(solution == List())
    }
  }
}
