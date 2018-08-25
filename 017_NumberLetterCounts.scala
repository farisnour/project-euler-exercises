def intToEnglishString(i: Int): String = {
  i match {
    case -1 => "and"
    case 0 => ""
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 14 => "fourteen"
    case 15 => "fifteen"
    case 16 => "sixteen"
    case 17 => "seventeen"
    case 18 => "eighteen"
    case 19 => "nineteen"
    case 20 => "twenty"
    case 30 => "thirty"
    case 40 => "forty"
    case 50 => "fifty"
    case 60 => "sixty"
    case 70 => "seventy"
    case 80 => "eighty"
    case 90 => "ninety"
    case 100 => "one hundred"
    case 200 => "two hundred"
    case 300 => "three hundred"
    case 400 => "four hundred"
    case 500 => "five hundred"
    case 600 => "six hundred"
    case 700 => "seven hundred"
    case 800 => "eight hundred"
    case 900 => "nine hundred"
    case 1000 => "one thousand"
  }
}

def breakUpInt(i: Int): List[Int] = {
  require(i >= 1 && i <= 1000, "cannot handle integer")
  val firstDigit = i / 100
  val secondDigit = (i % 100) / 10
  val thirdDigit = i % 10
  val thousandDigit = i / 1000
  val length = i.toString.length

  if (length == 4) {
    List(1000 * thousandDigit)
  } else if (secondDigit > 1 && length == 3) {
    List(100 * firstDigit, -1, 10 * secondDigit, thirdDigit)
  } else if (secondDigit > 1) {
    List(10 * secondDigit, thirdDigit)
  } else if (List(0,1).contains(secondDigit) && length == 3 && (secondDigit, thirdDigit) == (0,0)) {
    List(100 * firstDigit)
  } else if (List(0,1).contains(secondDigit) && length == 3) {
    List(100 * firstDigit, -1, i % 100)
  } else if (List(0,1).contains(secondDigit)) {
    List(i % 100)
  } else {
    List()
  }
}

def countWords(s: String): Int = s.split(" ").map(_.length).sum

val answer = (1 to 1000).map(breakUpInt).map(_.map(intToEnglishString)).flatMap(_.map(countWords)).sum
println(s"Answer is $answer")

