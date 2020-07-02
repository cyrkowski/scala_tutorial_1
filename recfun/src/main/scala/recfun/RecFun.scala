package recfun

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
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(openParenthesesNum: Int, chars: List[Char]): Boolean = {
      if (openParenthesesNum < 0 || (chars.isEmpty && openParenthesesNum != 0)) {
        false
      } else if (chars.isEmpty) {
        true
      } else if (chars.head == '(') {
        balanceIter(openParenthesesNum + 1, chars.tail)
      } else if (chars.head == ')') {
        balanceIter(openParenthesesNum - 1, chars.tail)
      } else {
        balanceIter(openParenthesesNum, chars.tail)
      }
    }

    balanceIter(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def findCombinationsCount(money: Int, coins: List[Int], coinCheckIndex: Int): Int =
      if (money == 0) 1
      else if (money < 0) 0
      else if (coins.length == coinCheckIndex) 0
      else {
        val usingFirstCoin = findCombinationsCount(money - coins.apply(coinCheckIndex), coins, coinCheckIndex)
        val usingRestCoins = findCombinationsCount(money, coins, coinCheckIndex + 1)
        usingFirstCoin + usingRestCoins
      }

    findCombinationsCount(money, coins, 0)
  }
}
