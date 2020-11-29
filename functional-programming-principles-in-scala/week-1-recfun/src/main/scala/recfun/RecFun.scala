package recfun

import scala.annotation.tailrec
import scala.collection.mutable

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalance(chars: List[Char], numOpen: Int): Boolean = {
      if (chars.isEmpty)
        numOpen == 0
      else if (chars.head == '(')
        isBalance(chars.tail, numOpen + 1)
      else if (chars.head == ')')
        numOpen > 0 && isBalance(chars.tail, numOpen - 1)
      else
        isBalance(chars.tail, numOpen)
    }

    isBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val count = mutable.ArrayDeque.fill(money + 1)(0)
    count(0) = 1

    for (c <- coins) {
      for (i <- 1.until(money + 1)) {
        if (c <= i) {
          count(i) += count(i - c)
        }
      }
    }

    count(money)
  }
}
