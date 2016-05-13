package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def buildTriangle(): Array[Array[Int]] = {
      val triangle: Array[Array[Int]] = Array.ofDim[Int](r + 1, r + 1)

      for (i <- 1 to r) {
        val currentRow = Array.ofDim[Int](i + 1)
        currentRow(0) = 1
        currentRow(i) = 1

        for (j <- 1 until i) {
          currentRow(j) = triangle(i - 1)(j) + triangle(i - 1)(j - 1)
        }
        triangle(i) = currentRow
      }
      triangle
    }

    buildTriangle()(r)(c)
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def recursiveBalance(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') recursiveBalance(chars.tail, open + 1)
      else if (chars.head == ')') open > 0 && recursiveBalance(chars.tail, open - 1)
      else recursiveBalance(chars.tail, open)
    }
    recursiveBalance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(m: Int, c: List[Int]): Int = {
      if (m == 0) 1
      else if (m < 0) 0
      else if (c.isEmpty && m >= 1) 0
      else loop(m, c.tail) + loop(m - c.head, c)
    }

    loop(money, coins)
  }
}
