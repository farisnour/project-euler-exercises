import scala.annotation.tailrec

def riffleShuffle(deck: List[Int], repeat: Int = 1): List[Int] = {
  @tailrec
  def riffleShuffleHelper(list: List[Int], r: Int): List[Int] = r match {
    case 0 => list
    case _ => riffleShuffleHelper(singleRiffleShuffle(list), r-1)
  }

  def singleRiffleShuffle(deck: List[Int]): List[Int] = {
    val (upperHalf, lowerHalf) = (deck.take(deck.size / 2), deck.takeRight(deck.size / 2))
    (upperHalf zip lowerHalf).flatMap(t => List(t._1, t._2))
  }

  riffleShuffleHelper(deck, repeat)
}

def s(n: Long): Int = {
  /*@tailrec
  def sHelper(d: Seq[Int], r: Int): Int = {
    if (r >= 15) 999
    else if (d == (1 to n)) r
    else sHelper(riffleShuffle(d), r + 1)
  }

  sHelper(riffleShuffle(1 to n), 1)*/
  Stream.from(2).take(100).find(k => BigInt(2).pow(k) % (n-1) == 1).getOrElse(999)
}

val primes: Stream[Int] = 2 #:: Stream.from(3,2).filter(isPrime)
def isPrime(n: Int): Boolean = primes.takeWhile(p => p*p <= n).forall(n % _ != 0)

def primeFactorsFn(n: Long): Seq[Long] = {
  @tailrec
  def primeFactorsHelper(m: Long, factors: Seq[Long]): Seq[Long] = {
    primes.takeWhile(_ < m).find(m % _ == 0) match {
      case Some(f) => primeFactorsHelper(m / f.toLong, factors :+ f.toLong)
      case None => factors :+ m
    }
  }

  primeFactorsHelper(n, List())
}

def divisors(n: Long): Seq[Long] = {
  val primeFactors: Seq[Long] = primeFactorsFn(n)

  (0 to primeFactors.length)
    .flatMap { e =>
      primeFactors
        .combinations(e)
        .map(_.product)
    }
}

def deckSizesFn(n: Int): Seq[Long] = {
  divisors(BigInt(2).pow(n).toLong - 1)
    .filter ( k =>
      BigInt(2).pow(n) % k == 1
        && divisors(n).filter(_ != n)
        .forall(l => BigInt(2).pow(l.toInt) % k != 1)
    )
    .map(_ + 1)
}

println(s"Answer is ${deckSizesFn(60).map(l => BigInt(l)).sum}")

