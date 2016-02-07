package leia.parse

/** A Dependency or Special. */
trait SentencePart

/**
Translates a Phrase to a SentencePart.
*/
object SentencePart extends (Phrase => SentencePart)
{
	def apply(phrase:Phrase): SentencePart =
		phrase match {
			case Statement(subject, predicate, modifiers) =>
				verbPhrase(predicate) withMods
					((modifiers map apply) :+ role("SUBJECT")(subject))

			case Command(predicate) =>
				role("COMMAND")(predicate)

			case BeQuestion(starter, be, subject, been, mods) =>
				val allMods =
					(mods map apply) ++
						Seq(role("SUBJECT")(subject)) ++
						been.map {
							case n:NounPhrase =>
								role("DIRECT OBJECT")(n)
							case a:AdjectivalPhrase =>
								role("LINKED ADJECTIVE")(a)
						}

				val content =
					word("v")(be) withMods allMods

				val qs =
					starter map word("qs")

				Dependency role ("QUESTION", content) withMods qs

			case ModalQuestion(preps, starter, modal, subject, predicate) =>
				val mods =
					(preps map apply) ++ Seq(
						role("SUBJECT")(subject),
						word("m")(modal)
					)

				val content =
					verbPhrase(predicate) withMods mods

				val start =
					starter map word("qs")

				Dependency.role("QUESTION", content, start)

			case ChooseQuestion(prep, choose, objekt, modal, subject, predicate) =>
				val chchosen =
					forceDependency(objekt) withMod word("choose")(choose)

				val chosen =
					prep match {
						case Some(p) =>
							word("prep")(p) withMod chchosen
						case None =>
							chchosen
					}

				val content =
					verbPhrase(predicate) withMods Seq(
						role("SUBJECT")(subject),
						Dependency role ("DIRECT OBJECT", chosen),
						word("m")(modal)
					)

				Dependency role("QUESTION", content)

			case HowMuchIsQuestion(how, linked, be, subject) =>
				val howMuchLinked =
					forceDependency(linked) withMod word("how-much")(how)

				val content =
					word("v")(be) withMods Seq(
						role("SUBJECT")(subject),
						Dependency role ("LINKED ADJECTIVE", howMuchLinked)
					)

				Dependency role("QUESTION", content)


			case Possessive(haver, had) =>
				nomPhrase(had) withMod role("POSSESSED BY")(haver)

			case np:NomPhrase =>
				nomPhrase(np)

			case Adverb(adv) =>
				word("adv")(adv)

			case AdjectivePhrase(advs, adj) =>
				word("adj")(adj) withMods (advs map word("adv"))

			case PrepositionalPhrase(prep, obj) =>
				word("prep")(prep) withMod apply(obj)

			case Infinitive(vp) =>
				apply(vp)

			case vp:VerbPhrase =>
				verbPhrase(vp)

			case Interjection(i) =>
				word("int")(i)

			//TODO: conjunctions

			case s:Special =>
				s
		}

	private def verbPhrase(vp:VerbPhrase) =
	{
		val directObject =
			vp.directObject map role("DIRECT OBJECT")
		val beneficiary =
			vp.beneficiary map role("BENEFICIARY")
		val modal =
			vp.modal map { m =>
				word("m")(m)
			}
		val linked =
			vp.linkedAdjective map role("LINKED ADJECTIVE")
		val otherMods =
			vp.modifiers map apply
		val mods =
			directObject ++ beneficiary ++ modal ++ linked ++ otherMods

		word("v")(vp verb) withMods mods
	}

	private def nomPhrase(np:NomPhrase) =
	{
		val adjs =
			np.adjectives map apply
		val dets =
			np.determiner map word("det")
		val whatFor =
			np.whatFor map role("WHAT FOR")
		val doneTo =
			np.doneTo map role("DONE TO")
		val does =
			np.does map role("DOES")
		val mods =
			adjs ++ dets ++ whatFor ++ doneTo ++ does

		word("n")(np.nominal) withMods mods
	}

	private def role(str:String)(x:Phrase) =
		Dependency role (str, apply(x))

	private def word(pos:String)(x:Meaning): Dependency =
	{
		println(x)

		val mods =
			x.mods map (Dependency emptyRole _.toUpperCase)

		Dependency(x word, pos, mods)
	}

	private def forceDependency(phrase:Phrase) =
		apply(phrase).asInstanceOf[Dependency]
}




