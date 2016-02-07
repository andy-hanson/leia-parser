package leia.parse

object Meaning
{
	def apply(word:String): Meaning =
		apply(word, Set.empty[String])

	def apply(word:String, mod:String): Meaning =
		apply(word, Set(mod))

	def unknown(word:String) =
		apply(word, "unknown")
}

/**
Represents the meaning of a word.
Synonyms all point to the same meaning.
"ate" has the same meaning as "eat" but with the "past" mod.
"places" has the same meaning as "place" but with the "plural" mod.
Unknown words have the "unknown" mod.
*/
case class Meaning(word:String, mods:Set[String]) extends
{
	assert(mods subsetOf Set("past", "plural", "unknown"))
}
