package leia.parse

/**
Parser main methods.
Wrapped by DependencyParse.java (in src/java).
*/

object All
{
	/**
	Parse a String to a SentencePart.
	*/
	def apply(text:String): Array[SentencePart] =
	{
		val pre =
			PreLexer(text)
		val lexed =
			Lexer(pre)
		val parsed =
			Parser(lexed)

		parsed map SentencePart toArray
	}

	/*
	def fancify(x:Any) =
	{
		val text =
			x.toString

		var s = ""

		def print(x:Any)
		{
			s += x
		}

		var indent = 0

		text foreach { ch =>
			print(ch)

			ch match {
				case '(' =>
					print(s"\n${"\t" * indent}")
					indent += 1
				case ')' =>
					indent -= 1
				case ',' =>
					print(' ')
				case _ =>
			}
		}

		s
	}
	*/

	/**
	Prints out detailed intermediate results for an input string.
	*/
	def sentenceDetail(text:String) =
	{
		var s = ""

		def print(x:Any)
		{
			s += x.toString
		}
		def println(x:Any)
		{
			print(x)
			print("\n")
		}
		def printlnln(x:Any)
		{
			print(x)
			print("\n\n")
		}

		printlnln(s"Input:\n$text")

		val pre =
			PreLexer(text)

		printlnln(s"PreLexed:\n${pre mkString "\n"}")

		val lexed =
			Lexer(pre)

		printlnln(s"Lexed:\n${lexed mkString "\n"}")

		val parsed =
			Parser(lexed)

		printlnln(s"Parsed:\n${parsed mkString "\n"}")

		val out =
			parsed map SentencePart

		printlnln("Dependency trees:")
		out foreach printlnln

		s
	}
}
