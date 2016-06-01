package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StringParserTerrainSuite extends FunSuite {

  trait Level1 extends GameDef with StringParserTerrain{
    val level =
      """ooo-------
        |oSoooo----
        |oT-""".stripMargin

    val levelVector = Vector(Vector('o', 'o','o'), Vector('o', 'o','S', 'o'), Vector('o', 'T'))
  }

	test("terrainFunction: Pos(0,0) => true") {
    new Level1 {
      assert(terrainFunction(levelVector)(Pos(0,0)))
    }
  }

  test("terrainFunction: Pos(1,3) => true") {
    new Level1 {
      assert(terrainFunction(levelVector)(Pos(1,3)))
    }
  }

  test("terrainFunction: Pos(0,3) => false") {
    new Level1 {
      assert(!terrainFunction(levelVector)(Pos(0,3)))
    }
  }

  test("terrainFunction: Pos(4,0) => false") {
    new Level1 {
      assert(!terrainFunction(levelVector)(Pos(4,0)))
    }
  }

  test("findChar: S => Pos(1,1)") {
    new Level1 {
      assert(findChar('S',levelVector) == Pos(1,1))
    }
  }

  test("findChar: T => Pos(2,1)") {
    new Level1 {
      assert(findChar('T',levelVector) == Pos(2,1))
    }
  }
}
