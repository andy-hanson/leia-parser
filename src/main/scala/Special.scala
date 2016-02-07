package leia.parse

import java.util.Calendar

/**
Things that aren't parsed as words go through the whole process as Specials.
*/
sealed trait Special
	extends SentencePart
	with Lexed
	with PreLexed
	with NounPhrase

object Time
{
	def now() =
		apply(Calendar getInstance())

	def day(month:Int, dayOfMonth:Int, dayOfWeek:Int) =
		apply(month, dayOfMonth, dayOfWeek, -1, -1, -1)

	def ofDay(hour:Int, minute:Int, amPm:Int) =
		apply(-1, -1, -1, hour, minute, amPm)

	private def todayPlus(n:Int) = {
		val calendarToday =
			Calendar getInstance()

		calendarToday add (Calendar DAY_OF_MONTH, n)

		apply(calendarToday) dayOnly
	}

	def yesterday() =
		todayPlus(-1)
	def today() =
		todayPlus(0)
	def tomorrow() =
		todayPlus(1)

	private def apply(c:Calendar): Time =
		apply(
			c get Calendar.MONTH,
			c get Calendar.DAY_OF_MONTH,
			c get Calendar.DAY_OF_WEEK,
			c get Calendar.HOUR,
			c get Calendar.MINUTE,
			c get Calendar.AM_PM
		)
}

/**
A day and time of day.
Fields with the value -1 are unknown.
For example, "today" has unknown hour, minute, and pm.
*/
case class Time(
	/** 0: Jan, 11: Dec */
	month:Int ,
	/** 1: 1st, 31: 31st */
	dayOfMonth:Int,
	/** 1: Sunday, 7:Saturday */
	dayOfWeek:Int,
	/** 1: 1(pm/am), 12: Noon/midnight */
	hour:Int,
	/** 0-59 */
	minute:Int,
	/** 0: am, 1: pm */
	pm:Int
	)
	extends Special
{
	/** Erases hour/minute/pm info. */
	def dayOnly =
		copy (
			hour = -1,
			minute = -1,
			pm = -1
		)
}

/** Unused. */
case class Dollars(amount:Double) extends Special
