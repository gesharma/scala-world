import scala.annotation.tailrec

object SundayCalc extends App {
	import java.util.Calendar._
	val cal = getInstance

	def isFirstSunday(year: Int) = {
		(JANUARY to DECEMBER).map { month =>
			cal.set(year, month, 1)
			if(cal.get(DAY_OF_WEEK) == SUNDAY) 1 else 0
		}.sum
	}

	def betweenYears(start:Int, end:Int): Int ={
		(for(i<- start to end) yield isFirstSunday(i)).sum
	}

	val feb = List(2)
	val month30 = List(4,6,9,11)
	val month31 = List(1,3,5,7,8,10,12)

	val isLeapYear = (year: Int) => ((year % 4) == 0) && !(((year % 100) == 0) && !((year % 400) == 0))

	def getNextMonthDay(firstDay:Int, daysInMonth:Int): Int = {
		cal.set(DAY_OF_WEEK, firstDay)
		cal.add(DAY_OF_MONTH, daysInMonth)
		cal.get(DAY_OF_WEEK)
	}

	def isFirstSundayWithManualLogic(year: Int) : Int = {
		@tailrec
		def checkMonths(total: Int, firstDayOfCurrentMonth:Int, month: Int): Int ={
			if(month<13) {
				val firstDayOfNextMonth = month match {
					case _ if month30.contains(month) => getNextMonthDay(firstDayOfCurrentMonth, 30)
					case _ if month31.contains(month) => getNextMonthDay(firstDayOfCurrentMonth, 31)
					case _ if isLeapYear(year) => getNextMonthDay(firstDayOfCurrentMonth, 29)
					case _ => getNextMonthDay(firstDayOfCurrentMonth, 28)
				}
				if (firstDayOfNextMonth == 7) {
					checkMonths(total + 1, firstDayOfNextMonth, month + 1)
				} else {
					checkMonths(total, firstDayOfNextMonth, month + 1)
				}
			} else {
				total
			}
		}
		checkMonths(0, 1, 1)
	}

	def betweenYearsManual(start:Int, end:Int): Int ={
		@tailrec
		def yearLoop(totalSundays:Int, year:Int): Int ={
			if(year<=end){
				yearLoop(totalSundays + isFirstSundayWithManualLogic(year), year + 1)
			} else {
				totalSundays
			}
		}

		yearLoop(0, start)

	}

	println(betweenYearsManual(1901, 2000))

}