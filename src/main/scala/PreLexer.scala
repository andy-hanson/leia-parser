package leia.parse

import scala.util.parsing.combinator.RegexParsers

/** Does basic preprocessing to a string. */
object LexerPrepare
{
	def apply(str:String) =
		str toLowerCase
}

object PreLexer extends RegexParsers
{
	/** Takes out the Words and Punctuations and Specials in a string. */
	def apply(str:String): Seq[PreLexed] =
	{
		val res =
			parseAll(all, LexerPrepare(str))

		res getOrElse { throw new Exception(res toString) }
	}

	type P[ResultType] =
		Parser[ResultType]

	/** Any number of single PreLexed. */
	lazy val all: P[Seq[PreLexed]] =
		singleThenIgnore*

	/** A single PreLexed, then (optionally) ignored chars. */
	lazy val singleThenIgnore: P[PreLexed] =
		single ~ (ignore?) ^^ { case single ~ ignore => single }

	/** A single PreLexed. */
	lazy val single: P[PreLexed] =
		time | date | dollars | word | punc

	/** A 'word'; a sequence of letters. */
	lazy val word: P[PreLexed] =
		"[a-z]+".r ^^ Word

	/** Eg: 4pm, Saturday May 31st 4pm */
	lazy val time: P[Time] =
		timeOfDay | specialTime | date

	/** Eg: 4am, 4pm, 4, 4:15 */
	lazy val timeOfDay: P[Time] =
		num ~ ((":" ~ num)?) ~ (' '?) ~ (("am" | "pm")?) ^^ {
			case hour ~ min ~ space ~ amOrPm =>
				val minute =
					min match {
						case Some(colon ~ m) => m
						case None => 0
					}
				val amPmCode =
					amOrPm match {
						case Some(x) =>
							x match {
								case "am" => 0
								case "pm" => 1
							}
						case None => -1
					}
				Time.ofDay(hour, minute, amPmCode)
		}

	/** Special times are evaluated based on the day the program is run. */
	lazy val specialTime: P[Time] =
		"today" ^^ { _ => Time today() } |
			"now" ^^ { _ => Time now() } |
			"tomorrow" ^^ { _ => Time tomorrow() } |
			"yesterday" ^^ { _ => Time yesterday() }

	/** Eg "Monday May 15th" */
	lazy val date: P[Time] =
		(day?) ~ month ~ nth ^^ {
			case d ~ month ~ nth =>
				val day =
					d getOrElse 0
				Time.day(month, nth, day)
		} |
			day ^^ { day => Time.day(0, 0, day) }

	/** Sunday - saturday. Converts to a 2-letter code. */
	lazy val day: P[Int] =
		("sunday" | "monday" | "tuesday" | "wednesday" |
			"thursday" | "friday" | "saturday") ^^ { x =>
			val codes =
				Array("Su", "Mo" ,"Tu", "We", "Th", "Fr", "Sa")
			codes indexOf (x substring (0, 2))
		}

	lazy val month: P[Int] =
		"January" ^^ { _ => 0 } |
		"February" ^^ { _ => 1 } |
		"March" ^^ { _ => 2 } |
		"April" ^^ { _ => 3 } |
		"May" ^^ { _ => 4 } |
		"June" ^^ { _ => 5 } |
		"July" ^^ { _ => 6 } |
		"August" ^^ { _ => 7 } |
		"September" ^^ { _ => 8 } |
		"October" ^^ { _ => 9 } |
		"November" ^^ { _ => 10 } |
		"December" ^^ { _ => 11 }

	/** 1st, 2nd, 3rd etc. */
	lazy val nth: P[Int] =
		num ~ (("th" | "st" | "nd")?) ^^ { case num ~ th => num }

	/** Eg "$$35" */
	lazy val dollars: P[Dollars] =
		'$' ~ num ^^ { case a ~ b => Dollars(b) }

	/** A (natural) number. */
	lazy val num: P[Int] =
		"[0-9]+".r ^^ (_ toInt)

	lazy val punc: P[PreLexed] =
		contraction | period | comma

	/** The 's in "what's good" */
	lazy val contraction: P[ContractionEnding] =
		("'s" | "'m" | "'re") ^^ ContractionEnding

	/** Any sentence-ending punctuation. */
	lazy val period =
		("." | "!" | "?") ^^ { _ => Period }

	lazy val comma =
		"," ^^ { _ => Comma }

	lazy val ignore =
		"""[\s\;]""".r
}





