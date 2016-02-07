package leia.parse

import collection.mutable.HashMap

/**
Stores every possible lexeme, and associates LexedWords with them.
*/
object Lexicon extends
{
	/**
	Optionally returns the lexicon entry at str.
	*/
	def get(str:String): Option[LexedWord] =
		lexicon get (str toLowerCase)

	/**
	The lexicon itself is just a map.
	It is lazy, so it is only loaded once, upon its first use.
	*/
	private lazy val lexicon: collection.Map[String, LexedWord] =
		generate()

	/** Reads in the lexicon. */
	private def generate() =
	{
		val lexicon =
			new HashMap[String, LexedWord]

		def add(word:String, pos:String, meaning:Meaning)
		{
			val entry =
				lexicon getOrElseUpdate (word toLowerCase, LexedWord(word))

			entry poss(pos) = meaning
		}

		addNormal(add)
		addOnomasticon(add)

		lexicon
	}

	/**
	Adds the onomasticon to the lexicon.
	All entries are nouns.
	*/
	private def addOnomasticon(add:(String, String, Meaning) => Unit)
	{
		val names =
			IO.readJSON[Map[String, Seq[String]]]("./onomasticon.json")

		names foreach { case (name, shortNames) =>
			(name +: shortNames) foreach { x =>
				addS(x) foreach (add(_, "proper",  Meaning(name, "plural")))
				add(x, "proper", Meaning(name))
			}
		}
	}

	/** Add the regular lexicon entries. */
	private def addNormal(add:(String, String, Meaning) => Unit)
	{
		val words =
			IO.readJSON[Map[String, Map[String, Seq[String]]]]("./lexicon.json")

		words foreach { case (wordAndPOS, specialMorphs) =>
			val parts =
				wordAndPOS split '|'
			if (parts.length != 2)
				throw new Error(s"Bad lexicon entry: $wordAndPOS")

			val Array(word, pos) =
				parts

			val plainMeaning =
				Meaning(word)

			add(word, pos, plainMeaning)

			val morph =
				morphs(word, pos, specialMorphs)

			morph.foreach { case (key, values) =>
				val meaningOp =
					key match {
						case "syn" =>
							Some(plainMeaning)

						case "past" | "plural" =>
							if (key == "past")
								assert(pos == "v" || pos == "m")

							Some(Meaning(word, key))

						case "cat" | "eg" =>
							None
					}

				for (meaning <- meaningOp; value <- values)
					add(value, pos, meaning)
			}
		}
	}

	/**
	Generates the morphology of a word.
	specialMorphology is what was specially written in the lexicon (eg "past": ["drove"]).
	It overrides the default morphology.
	*/
	private def morphs(
		word:String, pos:String,
		specialMorphology:Map[String, Seq[String]])
		: Map[String, Seq[String]] =
	{
		val default: Map[String, Seq[String]] =
			pos match {
				case "v" =>
					Map("syn" -> addS(word), "past" -> addD(word))
				case "n" =>
					Map("plural" -> addS(word))
				case _ =>
					Map()
			}

		default ++ specialMorphology
	}

	private def addS(x:String): Seq[String] =
		addChar(x, 's')
	/*{
		x.last match {
			case 'y' =>
				Seq(x + "ies")
			case _ =>
				Seq(x + 's', x + "es", x + x.last + "es")
		}
	}*/


	private def addD(x:String): Seq[String] =
		addChar(x, 'd')
	/*{
		x.last match {
			case 'y' =>
				Seq(x + "ied")
			case 'a' | 'e' | 'i' | 'o' | 'u' =>
				Seq(x + 'd')
			case _ =>
				Seq(x + "ed", x + x.last + "ed")
		}
	}*/

	private def addChar(x:String, ch:Char): Seq[String] =
	{
		x.last match {
			case 'y' =>
				Seq(x + "ie" + ch)
			case _ =>
				Seq(x + ch, x + 'e' + ch, x + x.last + 'e' + ch)
		}
	}


}
