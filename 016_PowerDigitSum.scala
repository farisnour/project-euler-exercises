val answer = Stream.continually(BigDecimal(math.pow(2,10)).toBigInt()).take(100).product.toString.toList.map(_.asDigit).sum
println(s"Answer is $answer")

