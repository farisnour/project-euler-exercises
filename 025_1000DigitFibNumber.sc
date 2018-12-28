val fibs:Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: (fibs zip fibs.tail).map{ t => t._1 + t._2 }

(Stream.from(0) zip fibs).map(t => (t._1,t._2,t._2.toString.length)).find(_._3 >= 1000).foreach(o => println(s"Answer is ${o._1}"))
