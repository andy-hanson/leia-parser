package leia.parse.hours

/**
Parses business hours.
Mostly self-contained.

Throughout, hours are represented in a 48-hour scale.
So a restaurant open from 10am to 2am goes from 10 to 26.

Example:
Input:
tue-fri 2pm-2am
Output:
0, 14.0, 14.0, 14.0, 14.0, 0, 0
0, 26.0, 26.0, 26.0, 26.0, 0, 0
*/

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

/** Translates a string to business hours. */
object BusinessHours
{
	def apply(str:String) =
		if (str isEmpty)
			Hours None
		else
		{
			val assigns =
				BusinessHoursParser(str)
			val hours =
				assigns.foldLeft(Hours.None)(_ assign _)
			hours
		}
}

/**
Represents a single time assignment (eg Mon 12-8)
*/
case class Assign(day:Int, range:TimeRange)

/**
Time from one hour to the next.
*/
case class TimeRange(start:Double, end:Double)

object Hours
{
	/**
	Empty Hours range.
	*/
	val None =
		Hours(Map())
}

/**
Represents the hours a restaurant is open.
*/
case class Hours(
	/** Maps days to the hours of that day */
	dayToTimes:Map[Int, TimeRange])
{
	/**
	Adds new open hours.
	*/
	def assign(a:Assign) =
		Hours(dayToTimes updated (a.day, a.range))

	/**
	Outputs a string looking like so:
	mon-open tues-open ... sun-open
	mon-close tues-close ... sun-close
	*/
	def display =
	{
		val days =
			1 to 7
		def display(f:TimeRange => Double, wrap:Boolean = false) =
			days map { day =>
				dayToTimes get day match {
					case Some(range) =>
						var t = f(range)
						if (wrap && t < 12)
							t += 24
						t toString
					case None =>
						"0"
				}
			} mkString ", "
		val opens =
			display(_ start)
		val closes =
			display(_ end, true)

		s"$opens\n$closes"
	}
}

/**
Translates input text into many assignments.
eg: "tue-thu 2pm-2am" ->
List(
	Assign(2,TimeRange(14.0,2.0)),
	Assign(3,TimeRange(14.0,2.0)),
	Assign(4,TimeRange(14.0,2.0)))
(2am is not converted to 26 until Hours.display)
*/
object BusinessHoursParser extends RegexParsers
{
	def apply(str:String): Seq[Assign] =
	{
		val res =
			phrase(all)(new CharSequenceReader(prepare(str)))

		res getOrElse { throw new Exception(""+res) }
	}

	def prepare(str:String) =
	{
		val low =
			str toLowerCase

		low indexOf "call" match {
			case -1 =>
				low
			case i =>
				low slice (0, i)
		}
	}

	type P[ResultType] =
		Parser[ResultType]

	/** Any number of (optionally delimeted) assignments */
	lazy val all: P[Seq[Assign]] =
		((assign ~ (delim?))*) ^^ (_ flatMap { case assign ~ delim => assign })

	/** Optional stuff between assignments */
	lazy val delim =
		";" | "\n"

	/** An assignment, eg "mon-fri 9-5" */
	lazy val assign: P[Seq[Assign]] =
		days ~ skip ~ timeRange ^^ {
			case dayz ~ _ ~ rangeOp =>
				rangeOp match {
					case Some(range) =>
						dayz map { day => Assign(day, range) }
					case None =>
						Seq()
				}
		}

	/** The days of an assignment; may include multiple of simple_days */
	lazy val days: P[Seq[Int]] =
		simple_days ~ comma ~ simple_days ^^ {
			case a ~ _ ~ b =>
				a ++ b
		} |
		simple_days

	/** A single day or day range */
	lazy val simple_days: P[Seq[Int]] =
		day ~ dash ~ day ^^ {
			case start ~ _ ~ end =>
				Range(start, end + 1)
		} |
		day ^^ { d => Seq(d) }

	/** List of every day name, in order. */
	val dayNames =
		Seq(
			"monday",
			"tuesday",
			"wednesday",
			"thursday",
			"friday",
			"saturday",
			"sunday"
		)

	/** A single day. */
	lazy val day: P[Int] =
		dayNames.zipWithIndex map { case (name, idx) =>
			val short =
				name slice (0, 3)
			val opts =
				if (name == "thursday")
					name | "thurs" | short
				else
					name | short

			opts ~ ('.'?) ^^ { _ => idx + 1 }
		} reduce (_ | _)

	/** A single time range. None if closed. */
	lazy val timeRange: P[Option[TimeRange]] =
		timeOpen ~ dash ~ timeClose ^^ {
			case a ~ _ ~ b =>
				Some(TimeRange(a, b))
		} |
		"closed" ^^ { _ => None }

	/** A single time. */
	def time(defaultAmPm:String): P[Double] =
		num ~ ((':' ~ num)?) ~ skip ~ (("am"|"pm")?) ^^ {
			case hourPre ~ minOp ~ _ ~ amPm =>
				val hour =
					amPm getOrElse defaultAmPm match {
						case "am" =>
							hourPre
						case "pm" =>
							if (hourPre == 12)
								hourPre
							else
								hourPre + 12
					}
				val minute =
					minOp match {
						case Some(_ ~ min) =>
							min
						case None =>
							0
					}

				hour + (minute / 60.0)
		}

	/** Open times are assumed to be AM. */
	lazy val timeOpen =
		time("am")

	/** Close times are assumed to be PM. */
	lazy val timeClose =
		time("pm")

	/** A single number. */
	lazy val num: P[Int] =
		"[0-9]+".r ^^ (_ toInt)

	lazy val comma =
		skip ~ ',' ~ skip

	lazy val dash =
		skip ~ '-' ~ skip

	/** Skip any number (including 0) of spaces. */
	lazy val skip =
		"""\s*""".r
}



