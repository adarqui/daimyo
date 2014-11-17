import scalaz._
import Scalaz._

object Echo {
    def echo(args: List[String]) {
        println(intersperse(args, " ") mkString (""))
    }
    def main(args: Array[String]) {
        echo (args.toList)
    }
}
