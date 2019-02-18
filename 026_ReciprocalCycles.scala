import scala.annotation.tailrec

// Let s(n) be the length of the recurring cycle in the unit decimal fraction 1/n
// i.e: s(7) = 6 Because 1/7 = 0.(142857)
def s(n: Int): Int = {
  @tailrec
  def helperFn(k: Int, chain: Seq[Int]): Int = {
    val next = (k - (k / n) * n) * 10
    next match {
      case 0 => 0
      case x if chain.contains(x) => chain.length - chain.indexOf(x)
      case x => helperFn(x, chain :+ x)
    }
  }

  val ans = helperFn(10, List[Int]())
  ans
}

val answer = (1 to 1000).map(n => (n,s(n))).maxBy(_._2)._1
println(s"Answer is $answer. 1/983 contains a recurring cycle of 982")
println(s"1/983 = 0.(0010172939979...)")

