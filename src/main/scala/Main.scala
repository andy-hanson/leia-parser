package leia.parse

/**
Only used by Andy.
Tests the parser software.
Runs the business hours parser on the business hours files.
*/

import scala.io.Source

import hours.BusinessHours

object Main
{
	def read(fileName:String) =
		Source fromFile fileName mkString

	def printlnln(x:Any)
	{
		println(x)
		println()
	}

	lazy val sentences =
		read("./sentences") split '\n' filterNot (_ startsWith "//")

	lazy val hourses =
		read("./business-hours-2.txt") split '|' map (_ trim)

	def testSentence(text:String)
	{
		println(All sentenceDetail text)
	}

	def testHours(str:String)
	{
		println(BusinessHours(str) display)
	}

	def parseHours()
	{
		hourses foreach { hours =>
			println(BusinessHours(hours) display)
			println('|')
		}
	}

	def main(args:Array[String])
	{
		val lexStress =
				"4:00pm 4:30 am 8 9am Sunday January 81nd Saturday" //"I want to find a nice place for dinner"

		val text =
			"I eat food today"
			//"Is Taco Bell good?"
			//"Who do I want to eat for dinner tonight?"
			//"I like Trader Ed's!"
			//"What time is Taco Bell open?"
			//"What is close?"
			//"I want to eat something nice tonight."
			//"I want to want to eat"
			//"What food should I eat?"



			//"I want some fast food. Yes! Fast food is better." //"What time does Taco Bell open?"

			//"Good afternoon, tomorrow I want to eat Mexican food. I like close places. Thank you very much, good bye."

			//"What time does Taco Bell open?"
			//"At what time does Taco Bell open? At what time is Taco Bell open?
			//"When does Taco Bell open? When is Taco Bell open?"

			//"When is Taco Bell open? When does Taco Bell open? What time does Taco Bell open? At what time does Taco Bell open?"

			//"I want to eat at a Mexican places"

			//"Good afternoon, tomorrow I want to eat Mexican food. I like close places. Thank you very much, good bye."

			//"Does Joe's Pizza accept Visa? Where is Joe's Pizza? Does Joe's Pizza have Mexican food?"
			//"Could you give me a place where I could eat sometime?"
			//"I like Mexican"
			//"When is Joe's Pizza open?"
			//"What is nearby"//"I want to find a nice restaurant for a dinner with my father tomorrow at 7 pm."
			//"When is KFC open?"//"is Subway open?"//"Subway is open"//"Is Subway open?"
			//"What can you recommend for a poor fellow who's down on his luck?"
			//"I want to find a nice restaurant for a dinner with my father tomorrow at 7 pm."

		//testHours("tue-thu 2pm-2am")
		//parseHours()

		testSentence(text)

		//println(Lexicon.lexicon("joe's pizza"))

		//testAllSentences()
	}

	def testAllSentences()
	{
		sentences foreach { sentence =>
			//testSentence(sentence)

			println(sentence)
			printlnln(All(sentence) toList)
		}

	}
}

