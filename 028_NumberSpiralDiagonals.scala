def d(n: Int): Long = {
  require(n >= 0)

  2 * (n + 1)
}

def a(n: Int): Long = {
  require(n >= 0)

  if (n == 0) {
    3
  }
  else if (n == 1) {
    3 + 10
  }
  else if (n == 2) {
    3 + (2 * 10) + (1 * 8)
  }
  else {
    3 + (n * 10) + (((n-1) * n * 8) / 2)
  }
}

def K(n: Int): Long = {
  require(n >= 0)

  (4 * a(n)) + (6 * d(n))
}

val answer = (0 to 499).map(n => K(n)).sum + 1
println("Answer is " + answer)
