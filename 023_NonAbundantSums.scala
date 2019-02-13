import scala.annotation.tailrec

val primes: Stream[Int] = 2 #:: Stream.from(3,2).filter(isPrime)
def isPrime(n: Int): Boolean = primes.takeWhile(p => p*p <= n).forall(n % _ != 0)

def primeFactorsFn(n: Int): Seq[Int] = {
  @tailrec
  def primeFactorsHelper(m: Int, factors: Seq[Int]): Seq[Int] = {
    primes.takeWhile(_ < m).find(m % _ == 0) match {
      case Some(f) => primeFactorsHelper(m / f, factors :+ f)
      case None => factors :+ m
    }
  }

  primeFactorsHelper(n, List())
}

def properDivisors(n: Int): Seq[Int] = {
  val primeFactors: Seq[Int] = primeFactorsFn(n)

  (0 to primeFactors.length - 1)
    .flatMap { e =>
      primeFactors
        .combinations(e)
        .map(_.product)
    }
}

def isAbundantNumber(n: Int): Boolean = {
  n < properDivisors(n).sum
}

val abundantNumbers = (1 to 28123).filter(isAbundantNumber).toSet

def isSumOfAbundantNumbers(n: Int): Boolean = {
  abundantNumbers.exists(k => abundantNumbers.contains(n - k))
}

val answer = (1 to 28123).filterNot(isSumOfAbundantNumbers).sum
println("Answer is " + answer)
