import scala.annotation.tailrec

def factorial(n: Int): Int = (1 to n).product

// Inefficient, but thats ok for this program
def remove[T](list:Seq[T], index: Int) = list.splitAt(index) match {case (l1,l2) => l1 ++ l2.drop(1)}

val input = 1000000 // Get the one millionth lexicographic permutation

def nthLexicographicPermutation(n: Int): Seq[Int] = {
//  @tailrec
  def helperFn(k: Int, remaining: Seq[Int]): Seq[Int] = {
    val len = remaining.length - 1
    val index = (k.toDouble / factorial(len)).ceil.toInt - 1
    len match {
      case 0 => remaining
      case _ => remaining(index) +: helperFn(k - index * factorial(len), remove(remaining, index))
    }
  }

  helperFn(n, (0 to 9).toList)
}

val answer = nthLexicographicPermutation(1000000)
println("Answer is " + answer)

