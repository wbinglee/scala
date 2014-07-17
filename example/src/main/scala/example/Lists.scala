package example

import common._

object Lists {
  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def sum(xs: List[Int]): Int = {
    /**
     * 
     * foreach solution
     * 
    var total = 0
    xs.foreach( total += _)
    total
    * 
    */
    
    // recursive solution
    def loop(acc: Int, list: List[Int]): Int = {
      if (list.isEmpty) acc else loop(acc + list.head ,list.tail)
    }
    loop(0,xs)
  }

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
  def max(xs: List[Int]): Int = {
    /**
     * 
     * foreach solution
    if (xs.isEmpty) throw new NoSuchElementException("No max in an empty list.")
    var m  = xs.indexOf(0)
    xs.foreach{ e => 
      if (e > m) m = e
  	}
    m
    **/
		  
    //Recursive solution
    def loop(m: Int, list: List[Int]): Int = {
      if (list.isEmpty) m 
      else 
        if (m < list.head) loop(list.head,list.tail) else loop(m, list.tail)
    }
    if (xs.isEmpty) throw new NoSuchElementException("No max in an empty list.")
    else loop(xs.indexOf(0),xs)
  }
}
