import scala.annotation.tailrec
// Assuming that the collatz conjecture holds for the first million integers

def collatzChainLength(num: Long): Long = {
  require(num > 0, "Cannot perform algorithm with non-positive value")
  @tailrec
  def collatzChainLengthHelper(n: Long, maxChain: Long): Long = {
    n % 2 match {
      case _ if n == 1 => maxChain
      case 0 => { collatzChainLengthHelper(n / 2, maxChain + 1) }
      case 1 => { collatzChainLengthHelper(3 * n + 1, maxChain + 1) }
    }
  }

  collatzChainLengthHelper(num, 0)
}

assert(collatzChainLength(1) == 0)
assert(collatzChainLength(2) == 1)
assert(collatzChainLength(13) == 9)

(1 to 10)
  .map(i => (i,collatzChainLength(i)))
  .foreach(println)

val answer = (1 to 1000000).map(i => (i,collatzChainLength(i))).maxBy(_._2)

println(s"Answer is ${answer._1}")
//Answer is 837799

//@tailrec
//def maxCollatzChain(num: Int): Int = {
//  def maxCollatzChainHelper(n: Int, k: Int, maxChain: Int): Int = {
//    (n-1)%3 match {
//      case 0 if (n != 0) => maxCollatzChainHelper()
//    }
//  }
//
//  return maxCollatzChainHelper(num, 1, 0);
//}
