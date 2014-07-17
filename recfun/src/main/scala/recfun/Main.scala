package recfun
import common._
import scala.collection.mutable.Stack

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
    if(c==0 || c==r) 1 else pascal(c-1,r-1) + pascal(c,r-1)
    
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(stack: Stack[Char],cs: List[Char]): Boolean = {
      if(cs.isEmpty) stack.isEmpty
      else{
        val c = cs.head
        if (c=='('){
          loop(stack.push(c),cs.tail)
        }else if (c==')'){
          if(stack.isEmpty || stack.pop()!='(') false else loop(stack,cs.tail)
        }else {
          loop(stack,cs.tail)
        }
      }
    }
    loop(Stack(),chars)  
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, newCoin:Int,oldCoins: List[Int]): Int = {
      if(oldCoins.isEmpty) {
       if(money%newCoin == 0) 1 else 0
      }else if(newCoin > money){
        loop(money,oldCoins.head,oldCoins.tail)
      }else if(newCoin == money){
        1 + loop(money,oldCoins.head,oldCoins.tail)
      }
      else {
    	def sumall(sum:Int,money:Int,newCoin:Int,oldCoins:List[Int]): Int={
    	  if(money<0) sum
    	  else if(money==0){
    	    sum + 1
    	  }
    	  else{
    	    val count =  loop(money,oldCoins.head,oldCoins.tail)
    	    sumall(sum + count ,money-newCoin,newCoin,oldCoins)
    	  }
    	    
    	}
    	sumall(0,money,newCoin,oldCoins)
    	
      }
    }
    val sortedCoins = coins.sortWith(_ > _)
    loop(money,sortedCoins.head,sortedCoins.tail)
  }
}
