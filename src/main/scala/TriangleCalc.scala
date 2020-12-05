import scala.annotation.tailrec
import scala.io.Source

object TriangleCalc extends App {

	println("Hello, world! " + calculate)

	def calculate: Double ={

		@tailrec
		def processCalculation(lines:List[String], total: Double, rowIndex: Int): Double ={
			def add( head: String, index:Int): Double ={
				total + head.split(" ").toList(index).toInt
			}
			lines match {
				case Nil => total
				case head :: Nil if rowIndex < 2 => add(head, 0)
				case head :: Nil => add(head, rowIndex-1)
				case head :: tail if rowIndex < 2 => processCalculation(tail, add(head, 0), rowIndex+1)
				case head :: tail => processCalculation(tail, add(head, rowIndex-1), rowIndex+1)
			}
		}
		val ipfileStream = getClass.getResourceAsStream("triangle.txt")
		val readlines = Source.fromInputStream(ipfileStream).getLines.toList

		processCalculation(readlines, 0, 0)


	}
}
