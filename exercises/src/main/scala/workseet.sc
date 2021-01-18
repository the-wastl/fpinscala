import fpinscala.laziness.Stream
import fpinscala.laziness.Stream._
import fpinscala.laziness._

var x = Stream(1,2,3,4,5,6)

x.drop(2).toList
x.drop(100).toList
x.takeWhileNoFold(_ < 4).toList
x.takeWhileNoFold((_) => false).toList

x.takeWhile(_ < 4).toList
x.takeWhile((_) => false).toList

x.map(_ + 1).toList
x.append(Stream(12)).toList

x.flatMap(i => Stream(i,i)).toList

Stream.from(10).take(10).toList

Stream.fibs().take(10).toList