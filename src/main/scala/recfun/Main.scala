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
    * Sample 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Sample 2
    */
  def balance(chars: List[Char], buffer: Int = 0): Boolean = {
    // Checks if there is a closing parentheses, and the count is still positive
    def isBalanced(chain: List[Char], remaining_parenthesis: Int): Boolean = {
      if (chain.isEmpty)
        true
      else if (chain.head == '(') {
        val chain_tail = chain.tail
        remaining_parenthesis > 0 && chain_tail.count(_ == ')') > 0 &&
          isBalanced(chain_tail, remaining_parenthesis - 1)
      }
      else
        isBalanced(chain.tail, remaining_parenthesis)
    }

    val opening_parentheses_number = chars.count(_ == '(')
    val closing_parentheses_number = chars.count(_ == ')')

    if (opening_parentheses_number != closing_parentheses_number)
      false
    else
      isBalanced(chars, closing_parentheses_number)
  }

  /**
    * Sample 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(m: Int, c: List[Int]): Int = {
      if (c.isEmpty) 0
      else if (m - c.head == 0) 1
      else if (m - c.head < 0) 0
      else countChange(m - c.head, c) + countChange(m, c.tail)
    }

    count(money, coins.sorted)
  }
}
