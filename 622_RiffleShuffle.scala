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