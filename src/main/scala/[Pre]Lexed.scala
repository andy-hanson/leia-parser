package leia.parse

import collection.mutable.{ HashMap, HashSet }

/**
Result of the first stage of lexing. A Word or Special.
The lexicon has not yet been consulted.
*/
trait PreLexed

/** Result of lexing. A LexedWord or Special. */
trait Lexed

/** Something surrounded by spaces that's not Special. */
case class Word(text:String) extends PreLexed

object LexedWord
{
	/** Create a LexedWord with no meanings. */
	def apply(word:String): LexedWord =
		LexedWord(word, new HashMap[String, Meaning])
}

/** A word, and all possible parts of speech it could be. */
case class LexedWord(
	word:String,
	poss:HashMap[String, Meaning])
	extends Lexed
{
	/** Whether this word means "be" */
	def isBe =
		poss get "v" match {
			case Some(m:Meaning) if m.word == "be" =>
				true
			case _ =>
				false
		}

	override def toString =
		s"'$word': $poss"
}


case class UnknownWord(word:String) extends Lexed

/**
A possessive or contracted 'be'.
Does not include "I would've" or "I can't".
*/
case class ContractionEnding(text:String) extends Lexed with PreLexed
{
	def meaning =
	{
		val x =
			text match {
				case "'s" | "'m" | "'re" =>
					"be"
			}

		Meaning(x)
	}
}

/**
A special symbol.
*/
sealed trait Punctuation extends Lexed with PreLexed
case object Period extends Punctuation
case object Comma extends Punctuation

