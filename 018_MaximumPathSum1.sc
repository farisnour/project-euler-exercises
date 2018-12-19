val input =
  """75
    |95 64
    |17 47 82
    |18 35 87 10
    |20 04 82 47 65
    |19 01 23 75 03 34
    |88 02 77 73 07 63 67
    |99 65 04 28 06 16 70 92
    |41 41 26 56 83 40 80 70 33
    |41 48 72 33 47 32 37 16 94 29
    |53 71 44 65 25 43 91 52 97 51 14
    |70 11 33 28 77 73 17 78 39 68 17 57
    |91 71 52 38 17 14 91 43 58 50 27 29 48
    |63 66 04 68 89 53 67 30 73 16 69 87 40 31
    |04 62 98 27 23 09 70 98 73 93 38 53 60 04 23""".stripMargin


val tree = input.split("\n").toList.map(_.split(" ").map(_.toInt).toList)

def maxOfTree(tree: List[List[Int]]): Int = {
  def maxOfTreeHelper(t: List[List[Int]], max: Int): Int = {
    if (t.isEmpty || t(0).isEmpty) return max
    else if (t.length == 1 && t(0).length == 1) return t(0)(0) + max
    val leftMax = maxOfTreeHelper(leftTree(t), t(0)(0) + max)
    val rightMax = maxOfTreeHelper(rightTree(t), t(0)(0) + max)
    math.max(leftMax, rightMax)
  }

  maxOfTreeHelper(tree, 0)
}

def leftTree(tree: List[List[Int]]): List[List[Int]] = {
  tree.drop(1).map(_.dropRight(1))
}

def rightTree(tree: List[List[Int]]): List[List[Int]] = {
  tree.drop(1).map(_.drop(1))
}

println(s"Answer is ${maxOfTree(tree)}")

