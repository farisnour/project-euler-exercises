def getDivisors(k: Int): Seq[Int] = {
  (1 until k).filter(k % _ == 0).toList
}

def d(n: Int): Int = {
  getDivisors(n).sum
}

def getAmicableNumbers(n: Int): Seq[Int] = {
  (1 until n).filter(k => d(d(k)) == k && d(k) != k).toList
}

println(s"Answer is ${getAmicableNumbers(10000).sum}")
