package leia.parse

import collection.mutable.ArrayBuffer

object Lexer
{
	/**
	Reads lexicon entries from a stream of Words.
	(Passes through all else unchanged.)
	*/
	def apply(pre:Seq[PreLexed]): Seq[Lexed] =
	{
		val out = new ArrayBuffer[Lexed](pre length)

		var i = 0
		while (i < pre.length)
		{
			val (newI, token) =
				get(pre, i)

			i = newI
			out += token
		}

		out toList
	}

	/**
	From a sequence of PreLexed, get a single lexeme.
	The longest word possible is preferred, eg "fast food" over "fast".
	Returns the index after the lexeme.
	*/
	def get(pre:Seq[PreLexed], i:Int): (Int, Lexed) =
	{
		pre(i) match {
			case Word(text) =>
				var k =
					pre length

				while (k > i)
				{
					for (str <- wordsToString(pre slice (i, k)))
						Lexicon get str match {
							case Some(lexed) =>
								return (k, lexed)
							case None =>
						}
					k -= 1
				}

				(i + 1, UnknownWord(text))

			case x:Lexed =>
				(i + 1, x)
		}

	}

	/**
	Gets a string of the PreLexed.
	Does more than just invert the PreLexing;
		spaces were taken out in that stage and only one will be put back.
	So "What 's   happening" will become "What's happening"
	*/
	def wordsToString(pre:Seq[PreLexed]): Option[String] =
	{
		var s = ""

		for (em <- pre)
		{
			em match {
				case Word(text) =>
					s += ' ' + text
				case ContractionEnding(ctr) =>
					s += ctr
				case _ =>
					// the sequence contains something that is definitely not part of a lexeme
					return None
			}
		}

		Some(s tail)
	}
}
