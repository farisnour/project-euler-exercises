val primes: Stream[Int] = 2 #:: Stream.from(3,2).filter(isPrime)
def isPrime(n: Int): Boolean = primes.takeWhile(p => p*p <= n).forall(n % _ != 0)

val answer = primes.takeWhile(_ < 2000000).foldLeft(0L)((a,z) => a+z)

println("Question:")
println("The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.")
println("Find the sum of all the primes below two million.")

println("\nAnswer:")
println(s"Sum of all primes below two million is $answer")