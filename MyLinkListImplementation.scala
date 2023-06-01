import scala.annotation.tailrec

object MyLinkListImplementation extends App {

  abstract class LinkedList[+A] {

    def head: A
    def tail: LinkedList[A]
    def isEmpty: Boolean
    def remove: LinkedList[A]
    def size(n:Int = 0):Int
    def add[B >: A](element: B): LinkedList[B]
    def map[B](transformer: Function[A, B]): LinkedList[B]
    def flatMap[B](transformer: Function[A, LinkedList[B]]): LinkedList[B]
    def filter(predicate: Function[A, Boolean]): LinkedList[A]
    def reverse():LinkedList[A]
    def ++[B >: A](list: LinkedList[B]): LinkedList[B]
    def foreach(function: A => Unit): Unit
    def sort(compare: (A, A) => Int): LinkedList[A]
    def zipWith[B >: A](linkedList: LinkedList[B], function:(B, B) => B): LinkedList[B]
    def fold[B](v1: B)(func: (B, A) => B): B

  }

  case class Full[+A](theHead: A, theTail: LinkedList[A]) extends LinkedList[A] {

    def isEmpty = false
    def head: A = theHead
    def remove: LinkedList[A] = tail
    def tail: LinkedList[A] = theTail
    override def toString: String = "[" + print + "]"

    def add[B >: A](element: B): LinkedList[B] = Full(element, this)
    def print: String = s"$head, $tail".replaceAll("\\[", "").replaceAll("]", "")
    def map[B](transformer: Function[A, B]): LinkedList[B] = new Full[B](transformer(head), tail.map(transformer))
    def flatMap[B](transformer: Function[A, LinkedList[B]]): LinkedList[B] = transformer(head) ++ tail.flatMap(transformer)
    def size(n: Int = 0): Int = if (tail.isEmpty) n + 1 else tail.size(n + 1)
    def ++[B >: A](list: LinkedList[B]): LinkedList[B] = new Full[B](head, tail ++ list)
    def fold[B](v1: B) (func: (B, A) => B): B = tail.fold(func(v1, head))(func)

    def filter(predicate: Function[A, Boolean]): LinkedList[A] =
      if (predicate(head)) new Full[A](head, tail.filter(predicate))
      else tail.filter(predicate)

    def foreach(function: A => Unit): Unit = {
      function (head)
      tail foreach function
    }

    def sort(compare: (A, A) => Int): LinkedList[A] = {
      @tailrec
      def sorting(list: LinkedList[A], sortedList: LinkedList[A] = Empty): LinkedList[A] = list match {
        case Empty => sortedList
        case Full(_, _) =>
          if (sortedList.isEmpty) sorting(list.remove, sortedList.add(list.head))
          else if (compare(sortedList.head, list.head) < 0) sorting(list.remove, sortedList.add(list.head))
          else sorting(list ++ Full(sortedList.head, Empty), sortedList.remove)
      }
      sorting(this)
    }

    def zipWith[B >: A](incomeList: LinkedList[B], function: (B, B) => B): LinkedList[B] = {
      @tailrec
      def zipping(n: Int, list1: LinkedList[B], list2: LinkedList[B], buffer: LinkedList[B] = Empty): LinkedList[B] = {
        if (n > 0) zipping(n - 1, list1.tail, list2.tail, buffer.add(function(list1.head, list2.head)))
        else buffer.reverse()
      }
      if (this.size() < incomeList.size()) zipping(this.size(), this, incomeList)
      else zipping(incomeList.size(), incomeList, this)
    }

    def reverse(): LinkedList[A] = {
      @tailrec
      def reversion(list: LinkedList[A], bufferList: LinkedList[A] = Empty): LinkedList[A] = {
        if (list.tail.isEmpty) bufferList.add(list.head)
        else reversion(list.tail, bufferList.add(list.head))
      }
      reversion(this)
    }
  }

  case object Empty extends LinkedList[Nothing] {

    def head = throw new NoSuchElementException()
    def tail = throw new NoSuchElementException()
    def isEmpty: Boolean = true
    override def toString = "_"
    def size(n:Int): Int = 0

    def add[B >: Nothing](element: B): LinkedList[B] = Full(element, Empty)
    def map[B](transformer: Function[Nothing, B]): LinkedList[B] = Empty
    def flatMap[B](transformer: Function[Nothing, LinkedList[B]]): LinkedList[B] = Empty
    def filter(predicate: Function[Nothing, Boolean]): LinkedList[Nothing] = Empty
    def ++[B >: Nothing](list: LinkedList[B]): LinkedList[B] = list
    def foreach(f: Nothing => Unit): Unit = ()
    def sort(sorting: (Nothing, Nothing) => Int): LinkedList[Nothing] = Empty
    def remove: LinkedList[Nothing] = Empty
    def zipWith[B >: Nothing](linkedList: LinkedList[B], function: (B, B) => B): LinkedList[B] = Empty
    def reverse(): LinkedList[Nothing] = Empty
    def fold[B](v1: B)(func: (B, Nothing) => B): B = v1
  }

  val evenPredicate: Int => Boolean = _ % 2 == 0
  val stringToIntTransformer: String => Int = _.toInt

  val myList = new Full[Int](1, new Full[Int](2, new Full[Int](3, new Full[Int](4, Empty))))
  val anotherList = new Full[Int](5, new Full[Int](6, new Full[Int](7, Empty)))

  println((myList ++ anotherList).sort((x, y) => y - x))
  println(myList) // 1, 2, 3, 4
  println(myList.map(x => x + 1)) // 2, 3, 4, 5
  println(myList ++ anotherList) // 1, 2, 3, 4, 5, 6, 7
  println((myList ++ anotherList).filter(x => x % 2 == 0)) // 2, 4, 6
  myList.foreach(println)
  println((myList ++ anotherList).sort((x, y) => x - y))
  println(myList.size())
  println((myList ++ anotherList).size())
  println(Empty.size())
  println(myList.zipWith(anotherList, (x, y) => x * y))
  println(myList.reverse())

  println(myList.fold(1) (_ + _))


}
