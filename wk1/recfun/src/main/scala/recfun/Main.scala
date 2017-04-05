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
      c match {
        case 0 => 1
        case x: Int if x == r => 1
        case _ => pascal(c, r - 1) + pascal(c - 1, r - 1)

      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceRec(unClosed: Int, chars: List[Char]): Int = {
        if (chars.isEmpty) {
          unClosed
        } else {
          chars.head match {
            case '(' => balanceRec(unClosed + 1, chars.tail)
            case ')' => {
              if (unClosed - 1 < 0) {
                -1
              } else {
                balanceRec(unClosed - 1, chars.tail)
              }
            }
            case _ => balanceRec(unClosed, chars.tail)
          }
        }
      }

      if (chars.isEmpty) {
        true
      } else {
        balanceRec(0, chars) == 0
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countChangeRec(money: Int, coins: List[Int]): Int = {
        money match {
          case 0 => 0
          case m: Int if m < 0 => 1
          case _ =>
            if (coins.nonEmpty) {
              val remainder = money - coins.head
              if (remainder == 0) {
                1
              } else if (remainder < 0) {
                0
              } else {
                countChangeRec(remainder, coins) + countChangeRec(money, coins.tail)
              }
            } else {
              0
            }
        }
      }
      countChangeRec(money, coins.sorted)
    }
}
