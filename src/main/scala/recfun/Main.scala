package recfun
import common._

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
    val initial = List(1);
    def pascalInternal(l: List[Int], row: Int) : Int = {
      if (row == 0) return l.apply(c)
      var current = List(1);
      for ((leftVal, index) <- l.zipWithIndex)
        if (index < l.size - 1)
          current ::= (leftVal + l.apply(index + 1))
      current ::= 1
      pascalInternal(current, row - 1)
    }
    pascalInternal(List(1), r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val filtered = chars.filter(x=>x == '(' || x == ')')
      def balanced(chars: List[Char], open: Int): Boolean = {
        if (chars.isEmpty) return open == 0
        if (chars.head == '(') return balanced(chars.tail,open+1)
        if (chars.head == ')') return open>0 && balanced(chars.tail,open-1)
        return balanced(chars.tail,open)
      }
      balanced(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) {
      return 0;
    }
    var currMoney = money;
    val coin = coins.head
    var count = countChange(currMoney, coins.tail) //without current
    while (currMoney > 0) {
      currMoney -= coin
      if (currMoney == 0) {
        count += 1;
      }
      count += countChange(currMoney, coins.tail)
    }
    return count;
  }
}
