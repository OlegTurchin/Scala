import scala.annotation.tailrec

object MyStreamImplementation extends App {

  abstract class MyStream[+A] {

    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]
    def #::[B >: A](element: B): MyStream[B] //prepend
    def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] //concatenate
    def foreach[U](f: A => U): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]
    def take[B >: A](n: Int): MyStream[B]
    def takeAsList[B >: A](n: Int): List[B]
    @tailrec
    final def toList[B >: A](acc: List[B] = Nil): List[B] =
      if (isEmpty) acc.reverse else tail.toList(head :: acc)

  }

  class Full[A](val theHead: A, theTail: => MyStream[A]) extends MyStream[A] {

    val head: A = theHead
    lazy val tail: MyStream[A] = theTail
    def isEmpty: Boolean = false

    def #::[B >: A](element: B): MyStream[B] = new Full(element, this)
    def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Full(head, tail ++ anotherStream)

    def foreach[U](func: A => U): Unit = {
      func(head)
      tail foreach func
    }

    def map[B](func: A => B): MyStream[B] = new Full(func(head), tail map func)
    def flatMap[B](func: A => MyStream[B]): MyStream[B] = func(head) ++ tail.flatMap(func)

    def filter(predicate: A => Boolean): MyStream[A] =
      if (predicate(head)) new Full(head, tail filter predicate)
      else tail filter predicate

    def take[B >: A](n: Int): MyStream[B] =
      if (n <= 0) Empty
      else if (n == 1) new Full(head, Empty)
      else new Full(head, tail.take(n - 1))

    def takeAsList[B >: A](n: Int): List[B] = take(n).toList()
  }

  object Empty extends MyStream[Nothing]{

    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException()
    def tail: MyStream[Nothing] = throw new NoSuchElementException()
    def #::[B >: Nothing](element: B): MyStream[B] = new Full[B](element, this)
    def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream
    def foreach[U](f: Nothing => U): Unit = ()
    def map[B](f: Nothing => B): MyStream[B] = this
    def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
    def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this
    def take[B >: Nothing](n: Int): MyStream[B] = this
    def takeAsList[B >: Nothing](n: Int): List[B] = Nil
  }

  object MyStream {
    def from[A](start: A) (generator: A => A): MyStream[A] = {
      new Full(start, MyStream.from(generator(start))(generator))
    }
  }

//  val naturals = MyStream.from(1)(_ + 1)

//  naturals take 10 foreach println
//  naturals takeAsList 10 foreach println
//  0 #:: naturals takeAsList 10 foreach println

//  naturals take 63 foreach println
//  naturals take 31 foreach println
//  naturals takeAsList 1_000_000 foreach println // works like a charm
//  naturals take 1_000_000 foreach println // throw SOF at 17k cuz of foreach ????

//  val fibonacciStream = naturals map(fibFunc(_)) take 46
//  fibonacciStream foreach println
//  primeStream foreach println

//  def fibonacci(first: Int, second: Int): MyStream[Int] =


  val time = System.currentTimeMillis()
//  val naturals: LazyList[Int] = LazyList.from(1).map(_ + 1)
//  val naturals: Stream[Int] = Stream.from(1).map(_ + 1)
//  val naturals: Stream[Int] = Stream.from(1).map(_ + 1)

  val naturals: MyStream[Int] = MyStream.from(1)(_ + 1).map(_ + 1)    // is_prime solved in 760ms 0_0
//  val naturals: MyStream[Int] = MyStream.from(1)(_ + 1)               // is_prime this one in 6_000ms wtf?

  naturals filter(isPrime(_)) take 6_000 foreach println
//  naturals map(fibFunc(_)) take 10_000 foreach println
//  fibonacci(1, 1) take 10_000 foreach println
//  eratosthenes(naturals) take 5_000 foreach println       //    trow SOF at 6k

  println(System.currentTimeMillis() - time)

  def fibonacci(f:Int, s:Int): MyStream[Int] =  // good for practice (probably),
    new Full(f, fibonacci(s, f + s))            // but makes no practical sense cuz of the same performance

  def eratosthenes(num: MyStream[Int]): MyStream[Int] = {          // was interesting to think about but separate tailrec method
    if(num.isEmpty) num                                            // is still much better (no SOF at 6k and 2x performance)
    else new Full[Int](num.head, eratosthenes(num.tail filter(x => x % num.head != 0)))
  }

  @tailrec
  def fibFunc(num: Int, last: Int = 1, prev: Int = 1, ref: Int = 3): Int = {
    if (num < 3) 1
    else if (num == ref) last + prev
    else fibFunc(num, last + prev, last, ref + 1)
  }

  @tailrec
  def isPrime(num: Int, del: Int = 2): Boolean =
    if (num == del) true
    else if (num % del != 0) isPrime(num, del + 1)
    else false
}
