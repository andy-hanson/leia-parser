package leia.parse

object Dependency
{
	def emptyRole(role:String) =
		apply(role, "role")

	def role(role:String, value:SentencePart, mods:Iterable[SentencePart] = None) =
		apply(role, "role", Array(value) ++ mods)

	def apply(
		head:String, partOfSpeech:String,
		modifiers:Iterable[SentencePart] = None): Dependency =
		apply(head, partOfSpeech, modifiers toArray)

}

/**
The main data structure of the dependency tree.
*/
case class Dependency private(
	/** The word in question. */
	head:String,
	/** Its part of speech. */
	partOfSpeech:String,
	/** Unordered list of things that describe this. */
	modifiers:Array[SentencePart])
	extends SentencePart
{
	override def toString =
		show("")

	private def show(indent:String): String =
	{
		val open =
			s"<$head|$partOfSpeech"

		if (modifiers.isEmpty)
			s"$open>"
		else
		{
			val newIndent =
				s"$indent\t"

			val mods =
				modifiers map {
					case d:Dependency =>
						d show newIndent
					case x =>
						x.toString
				} mkString s",\n$newIndent"

			s"<$head|$partOfSpeech,\n$newIndent$mods\n$indent>"
		}
	}

	def withMod(newMod:SentencePart) =
		withMods(Seq(newMod))

	def withMods(newMods:Iterable[SentencePart]) =
		copy(modifiers = modifiers ++ newMods)
}


