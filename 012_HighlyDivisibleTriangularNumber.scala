import scala.math.sqrt

def numberOfDivisors(n: Int): Int = {
  val count = (1 to sqrt(n).toInt).count(i => n%i == 0)
  if (sqrt(n) == sqrt(n).toInt.toDouble) 2*count-1 else 2*count
}

assert(numberOfDivisors(1) == 1)
assert(numberOfDivisors(3) == 2)
assert(numberOfDivisors(6) == 4)
assert(numberOfDivisors(10) == 4)
assert(numberOfDivisors(15) == 4)
assert(numberOfDivisors(21) == 4)
assert(numberOfDivisors(28) == 6)
assert(numberOfDivisors(36) == 9)


val stream: Stream[Int] = {
  1 #:: 3 #:: (stream zip stream.tail).map{ t => t._2 + (t._2 - t._1 + 1) }
}

println(s"Answer is ${stream.find(numberOfDivisors(_) > 500).get}")

