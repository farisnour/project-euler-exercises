implicit class Combinations(n: Int) {
  private def fact(n: Int): BigInt = (1 to n).map(BigInt(_)).product
  def ! = fact(n) // allows 10!
  def choose(k: Int): BigInt = fact(n) / (fact(n - k) * fact(k))
}

// Answer is 137846528820
println(s"Answer is ${40 choose 20}")
