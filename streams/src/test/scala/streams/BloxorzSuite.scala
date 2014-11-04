package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait LevelImpossible extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----oooo
        |--T---ooo-""".stripMargin

  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("isStanding level 1") {
    new Level1 {
      assert(Block(Pos(1,1),Pos(1,1)).isStanding)
      assert(!Block(Pos(1,1),Pos(1,2)).isStanding)
    }
  }

  test("isLegal level 1") {
    new Level1 {
      assert(Block(Pos(1,1),Pos(1,1)).isLegal)
      assert(!Block(Pos(1,5),Pos(1,6)).isLegal)
    }
  }

  test("start block") {
    new Level1 {
      assert(startBlock == Block(Pos(1,1),Pos(1,1)))
    }
  }

  test("neighbors block") {
    new Level1 {
      assert(Block(Pos(1,1),Pos(1,1)).neighbors == List(
        (Block(Pos(1,-1),Pos(1,0)),Left),
        (Block(Pos(1,2),Pos(1,3)),Right),
        (Block(Pos(-1,1),Pos(0,1)),Up),
        (Block(Pos(2,1),Pos(3,1)),Down)
      ))
    }
  }

  test("legal neighbors block") {
    new Level1 {
      assert(Block(Pos(1,1),Pos(1,1)).legalNeighbors == List(
        (Block(Pos(1,2),Pos(1,3)),Right),
        (Block(Pos(2,1),Pos(3,1)),Down)
      ))
    }
  }

  test("done?"){
    new Level1 {
      assert(done(Block(Pos(4,7),Pos(4,7))))
    }
  }

  test("neighours with history for start"){
    new Level1 {
      val nei = neighborsWithHistory(startBlock,List())
      assert(nei(0)===(Block(Pos(1,2),Pos(1,3)),List(Right)))
      assert(nei(1)===(Block(Pos(2,1),Pos(3,1)),List(Down)))
    }
  }

//  test("stream from start"){
//    new Level1 {
//      pathsFromStart.foreach(e=> println(e))
//    }
//  }


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

  test("solution length for level impossible") {
    new LevelImpossible {
      assert(solution.length == 0)
    }
  }
}
