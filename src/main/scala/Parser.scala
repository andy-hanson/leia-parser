package leia.parse

import scala.util.parsing.combinator.PackratParsers

/**
Parses Lexeds into Phrases.
*/
object Parser extends PackratParsers
{
	def apply(lexeds:Seq[Lexed]) =
	{
		val res =
			all(SeqReader(lexeds))

		res getOrElse { throw new Exception(""+res) }
	}

	/** The input type for the parser is Lexed. */
	type Elem =
		Lexed

	type P[ResultType] =
		PackratParser[ResultType]

	/** Any number of parseable phrases. */
	lazy val all: P[Seq[Phrase]] =
		phrase(any*)

	/** A parseable phrase, then (optionally) punctuation. */
	lazy val any: P[Phrase] =
		(sentence | nounPh | prepPh) ~ (punc?) ^^ {
			case content ~ punc => content
		}

	lazy val punc =
		accept("period or comma", {
			case Period | Comma =>
				null
		})

	lazy val comma =
		accept("comma", {
			case Comma =>
				null
		})

	/**
	A 'sentence' is the ideal first parsed thing.
	They may have Periods ("!?.") between them, and may not.
	*/
	lazy val sentence: P[Sentence] =
		interjection | question | statement  | command// | conjunct

	/*
	// conjunctions are poorly tested

	lazy val conjunct: P[Conjunct] =
		coordinative_conjunct | subordinative_conjunct | correlative_conjunct

	lazy val coordinative_conjunct =
		sentence ~ coord_conj ~ sentence ^^ {
			case a ~ b ~ c => CoordinativeConjunct(a, b, c)
		}

	lazy val subordinative_conjunct =
		sub_conj ~ sentence ~ sentence ^^ {
			case a ~ b ~ c => SubordinativeConjunct(a, b, c)
		} |
		sentence ~ sub_conj ~ sentence ^^ {
			case a ~ b ~ c => SubordinativeConjunct(b, c, a)
		}

	lazy val correlative_conjunct =
		corr_conj_1 ~ sentence ~ corr_conj_2 ~ sentence ^^ {
			case a ~ b ~ c ~ d => CorrelativeConjunct(a, b, c, d)
		}
	*/

	/**
	All of these should be tagged with the QUESTION role in output.
	*/
	lazy val question =
		be_question | modal_question | choose_question | how_much_is_question

	/**
	Eg:
	Am I food?
	When am I food?
	(For whom) am I food?
	Am I yummy (to you)?
	*/
	lazy val be_question =
		(question_starter?) ~ (prepPh*) ~ be ~ nounPh ~ ((nounPh | adjPh)?) ~ (prepPh*) ^^ {
			case start ~ mods1 ~ be ~ subj ~ been ~ mods2 =>
				BeQuestion(start, be, subj, been, mods1 ++ mods2)
		}

	/**
	Eg:
	[When] can I (eat food)?
	*/
	lazy val modal_question =
		(prepPh*) ~ (question_starter?) ~ modal ~ nounPh ~ verbPh ^^ {
			case preps ~ start ~ mod ~ subj ~ pred =>
				ModalQuestion(preps, start, mod, subj, pred)
		}

	/**
	Eg:
	What food should I eat?
	[At] what (time tonight) should (my father) (eat food)?
	*/
	lazy val choose_question =
		(prep?) ~ choose ~ nounPh ~ modal ~ nounPh ~ verbPh ^^ {
			case prep ~ choose ~ obj ~ mod ~ subj ~ pred =>
				ChooseQuestion(prep, choose, obj, mod, subj, pred)
		}

	/**
	Eg:
	How hungry am I?
	*/
	lazy val how_much_is_question =
		how_much ~ adjPh ~ be ~ nounPh ^^ {
			case how ~ linked ~ be ~ subj =>
				HowMuchIsQuestion(how, linked, be, subj)
		}

	/**
	Simply wraps a verb phrase in a Command.
	This should be placed in the COMMAND role in the output.
	*/
	lazy val command: P[Command] =
		verbPh ^^ Command

	/**
	Eg:
	(For dinner) [,] I (want food).
	*/
	lazy val statement: P[Statement] =
		(verbMod*) ~ (comma?) ~ nounPh ~ verbPh ^^ {
			case mods ~ _ ~ a ~ b => Statement(a, b, mods)
		}

	/** Times, Dates, infinitives, and normal nominals (including pronouns) */
	lazy val nounPh: P[NounPhrase] =
		special | infinitive | nomPh

	/** Matches any Special. */
	lazy val special: P[NounPhrase] =
		accept("special value", {
			case s:Special => s
		})

	/** Matches any Time. */
	lazy val time =
		acceptMatch("time", {
			case t:Time => t
		})

	/**
	After a plainNomPh, it may do things or have things done to it.
	Eg:
	(good food) (to eat)
	(the food) (that I eat)
	(the food) (that eats)
	*/
	lazy val nomPh =
		plainNomPh ~ (infinitive?) ~ (done_to?) ~ (does?) ^^ {
			case plain ~ inf ~ done_to ~ does =>
				def mod(n:NomPhrase) =
					n copy (whatFor = inf, doneTo = done_to, does = does)
				plain match {
					case n:NomPhrase =>
						mod(n)
					case Possessive(haver, had) =>
						Possessive(haver, mod(had))
				}
		}

	/**
	A plainerNomPh may possess another.
	Eg:
	((my father) 's father) 's father
	*/
	lazy val plainNomPh: P[NounPhrase] =
		plainerNomPh |
		plainerNomPh ~ apostropheS ~ plainerNomPh ^^ {
			case haver ~ _ ~ had =>
				Possessive(haver, had)
		}

	lazy val plainerNomPh: P[NomPhrase] =
		nomBPh | normNomPh

	/**
	Eg:
	somewhere nice
	*/
	lazy val nomBPh: P[NomPhrase] =
		nominal_b ~ ((adjPh | afj)*) ^^ {
			case nomB ~ adjs =>
				val mods = adjs map {
					case a:AdjectivalPhrase => a
					case m:Meaning =>
						AdjectivePhrase(Seq(), m)
				}
				NomPhrase(None, mods, nomB)
		}

	/**
	Eg:
	the (very good) dinner tonight
	*/
	lazy val normNomPh: P[NomPhrase] =
		(det?) ~ (adjPh*) ~ nominal ~ (afj*) ^^ {
			case det ~ adjs ~ nom ~ afj =>
				val afjs =
					afj map (a => AdjectivePhrase(Seq(), a))
				NomPhrase(det, adjs ++ afjs, nom)
		}

	/**
	Eg:
	whom I eat
	*/
	lazy val done_to =
		done_to_relativiser ~ statement ^^ { case _ ~ a => a }

	/**
	Eg:
	who eats
	*/
	lazy val does =
		does_relativiser ~ verbPh ^^ { case _ ~ a => a }

	/** 's */
	lazy val apostropheS =
		accept("'s", { case ContractionEnding("'s") => null })

	/**
	Eg:
	to eat
	*/
	lazy val infinitive: P[Infinitive] =
		infinitive_starter ~ verbPh ^^ { case to ~ predicate => Infinitive(predicate) }

	/**
	Eg:
	can only eat (mexican food)
	gives me money (for food)
	drives me crazy
	*/
	lazy val verbPh: P[VerbPhrase] =
		(modal?) ~ (verbMod*) ~ verb ~ (nounPh?) ~ (nounPh?) ~ (adjPh?) ~ (verbMod*) ^^ {
			case modal ~ mods1 ~ verb ~ ben ~ obj ~ adjph ~ mods2 =>
				val mods =
					mods1 ++ mods2
				val (bene, dObj) = ben match {
					case bb:Some[_] =>
						val b = bb.asInstanceOf[Some[NounPhrase]]
						obj match {
							case o:Some[_] =>
								(b, o.asInstanceOf[Some[NounPhrase]])
							case None =>
								(None, b)
						}
					case None =>
						(None, obj)
				}

				VerbPhrase(modal, verb, bene, dObj, adjph, mods)
		}

	/**
	Any thing that modifies a verb phrase.
	*/
	lazy val verbMod =
		advPh | prepPh | time

	/** An adverb. */
	lazy val advPh =
		adv ^^ Adverb

	/**
	Eg:
	for dinner
	*/
	lazy val prepPh: P[PrepositionalPhrase] =
		prep ~ nounPh ^^ { case p ~ n => PrepositionalPhrase(p, n) }

	/**
	A regular adjective phrase, or something specially
	*/
	lazy val adjPh: P[AdjectivalPhrase] =
		adjecPh | specialAdj

	/**
	Certain Special things may be considered Adjectival Phrases.
	(unused)
	Eg:
	"4pm" in "a 4pm meeting"
	*/
	lazy val specialAdj: P[AdjectivalPhrase] =
		accept("adjectival phrase", {
			case a:AdjectivalPhrase =>
				a
		})

	/**
	Eg:
	very very good
	*/
	lazy val adjecPh: P[AdjectivePhrase] =
		(adv*) ~ adj ^^ {
			case advs ~ v => AdjectivePhrase(advs, v)
		}

	/**
	Eg:
	"'s" in "What's up?"
	*/
	lazy val contraction_ending =
		accept("contraction", {
			case c:ContractionEnding =>
				c meaning
		})

	/** Matches any word or contraction that means 'be' */
	lazy val be: P[Meaning] =
		apostropheS ^^ { _ => Meaning("be") } | accept("be", {
			case l:LexedWord if l isBe =>
				l poss "v"
		})




	/** Parses a word of a given type. */
	def word(pos:String): P[Meaning] =
		accept("part of speech " + pos, {
			case LexedWord(word, poss) if poss contains pos =>
				poss(pos)
			case UnknownWord(word) =>
				Meaning unknown word
		})

	/** Parsers for parts of speech are now easy. */

	lazy val adj =
		word("adj")
	lazy val afj =
		word("afj")
	lazy val adv =
		word("adv")
	lazy val choose =
		word("choose")
	lazy val det =
		word("det")
	lazy val how_much =
		word("how-much")
	lazy val interjection =
		word("int") ^^ Interjection
	lazy val infinitive_starter =
		word("is")
	lazy val modal =
		word("m")
	lazy val nominal =
		word("n") | word("proper")
	lazy val nominal_b =
		word("nb")
	lazy val verb =
		apostropheS ^^ { _ => Meaning("be") } | word("v")
	lazy val prep =
		word("prep")
	lazy val question_starter =
		word("qs")
	lazy val coord_conj =
		word("cc")
	lazy val sub_conj =
		word("sc")
	lazy val corr_conj_1 =
		word("cc1")
	lazy val corr_conj_2 =
		word("cc2")
	lazy val does_relativiser =
		word("does-relativiser")
	lazy val done_to_relativiser =
		word("done-to-relativiser")
}
