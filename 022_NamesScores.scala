import scala.io.Source

val input = Source.fromFile("/Users/farisnour/Documents/project-euler-exercises/p022_names.txt").mkString

val sanitizedInput = input.split(',').map(_.stripPrefix("\"").stripSuffix("\""))

val sorted = (Stream.from(1) zip sanitizedInput.sorted)
  .map { case (i, name) => (i, name, getScore(name)) }

val answer = sorted.collect { case (i, name, score) => i * score}.sum

println(s"Answer is $answer")

def getScore(name: String) = {
  name.toList.foldLeft(0)((a,z) => a + Char.char2int(z) - 64)
}

