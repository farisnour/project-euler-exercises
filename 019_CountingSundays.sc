val nextMonth = Map(
  "January" -> "February",
  "February" -> "March",
  "March" -> "April",
  "April" -> "May",
  "May" -> "June",
  "June" -> "July",
  "July" -> "August",
  "August" -> "September",
  "September" -> "October",
  "October" -> "November",
  "November" -> "December",
  "December" -> "January"
)

val nextDay = Map(
  "Monday" -> "Tuesday",
  "Tuesday" -> "Wednesday",
  "Wednesday" -> "Thursday",
  "Thursday" -> "Friday",
  "Friday" -> "Saturday",
  "Saturday" -> "Sunday",
  "Sunday" -> "Monday"
)

def isLeapYear(y: Int): Boolean = {
  (y % 4 == 0) && ((y % 100 != 0) || (y % 400 == 0))
}

def addOne(tupleDate: (Int, String, Int)): (Int, String, Int) = {
//  val day = tupleDate._1
//  val month = tupleDate._2
//  val year = tupleDate._3
  val monthsWith31Days = List("January", "March", "May", "July", "August", "October", "December")
  val monthsWith30Days = List("April", "June", "September", "November")
  val monthOfFebruary = List("February")
  tupleDate match {
    case (31, m, y) if (monthsWith31Days.contains(m) && m == "December") => (1, nextMonth(m), y + 1)
    case (31, m, y) if (monthsWith31Days.contains(m)) => (1, nextMonth(m), y)
    case (d, m, y) if (monthsWith31Days.contains(m)) => (d + 1, m, y)
    case (30, m, y) if (monthsWith30Days.contains(m)) => (1, nextMonth(m), y)
    case (d, m, y) if (monthsWith30Days.contains(m)) => (d + 1, m, y)
    case (28, m, y) if (monthOfFebruary.contains(m) && !isLeapYear(y)) => (1, nextMonth(m), y)
    case (29, m, y) if (monthOfFebruary.contains(m) && isLeapYear(y)) => (1, nextMonth(m), y)
    case (d, m, y) if (monthOfFebruary.contains(m)) => (d + 1, m, y)
    case (_,_,_) => throw new Exception("This date makes no sense")
  }
}


val dates = Stream.iterate((1, "January", 1900))(addOne)
val days = Stream.iterate("Monday")(nextDay)

val answer = (dates zip days)
              .drop(365).take(1461*25)
              .count(tuple => tuple._2 == "Sunday" && tuple._1._1 == 1)

println(s"Answer is ${answer}")
