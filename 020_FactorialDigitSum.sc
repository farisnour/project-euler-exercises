def factorial: Stream[BigInt] = 1 #:: factorial.zip(Stream.from(1)).map(x => x._1 * x._2)

println(s"Answer is ${factorial(100).toString.toList.map(_.asDigit).sum}")
