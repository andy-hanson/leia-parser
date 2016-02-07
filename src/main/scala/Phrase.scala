/** Data structures for each grammar structure. */

package leia.parse

trait Phrase

sealed trait Sentence extends Phrase

case class Interjection(word:Meaning) extends Sentence

case class Statement(
	subject:NounPhrase,
	predicate:VerbPhrase,
	modifiers: Seq[Phrase])
	extends Sentence

case class Command(vp:VerbPhrase)
	extends Sentence

sealed trait Question extends Sentence

case class BeQuestion(
	starter:Option[Meaning],
	be:Meaning,
	subject:NounPhrase,
	been:Option[Phrase],
	modifiers:Seq[Phrase])
	extends Question

case class ModalQuestion(
	preps:Seq[Phrase],
	starter:Option[Meaning],
	modal:Meaning,
	subject:NounPhrase,
	predicate:VerbPhrase)
	extends Question

case class ChooseQuestion(
	prep:Option[Meaning],
	choose:Meaning,
	objekt:NounPhrase,
	modal:Meaning,
	subject:NounPhrase,
	predicate:VerbPhrase)
	extends Question

case class HowMuchIsQuestion(
	how:Meaning,
	linked:AdjectivalPhrase,
	be:Meaning,
	subject:NounPhrase)
	extends Question

trait NounPhrase extends Phrase

case class Possessive(
	haver:NounPhrase,
	had:NomPhrase)
	extends NounPhrase

object NomPhrase
{
	def apply(
		determiner:Option[Meaning],
		adjectives:Seq[AdjectivalPhrase],
		nominal:Meaning)
		: NomPhrase =
		NomPhrase(determiner, adjectives, nominal, None, None, None)
}

case class NomPhrase(
	determiner:Option[Meaning],
	adjectives:Seq[AdjectivalPhrase],
	nominal:Meaning,
	whatFor:Option[Infinitive],
	doneTo:Option[Statement],
	does:Option[VerbPhrase])
	extends NounPhrase

//case class Pronoun(word:Meaning) extends NounPhrase

case class Infinitive(vp:VerbPhrase) extends NounPhrase

case class VerbPhrase(
	modal:Option[Meaning],
	verb:Meaning,
	beneficiary:Option[NounPhrase],
	directObject:Option[NounPhrase],
	linkedAdjective:Option[AdjectivalPhrase],
	modifiers:Seq[Phrase])
	extends Phrase

case class Adverb(meaning:Meaning) extends Phrase

trait AdjectivalPhrase extends Phrase

case class AdjectivePhrase(
	adverbs:Seq[Meaning],
	adjective:Meaning)
	extends AdjectivalPhrase

case class PrepositionalPhrase(
	preposition:Meaning,
	objekt:NounPhrase)
	extends Phrase



object Conjunct
{
	/** Generically pattern-matches any conjunct, extracting both inner sentences. */
	def unapply(a:Any): Option[(Sentence, Sentence)] =
		a match {
			case c:Conjunct =>
				Some((c.primary, c.secondary))
			case _ =>
				None
		}
}

sealed trait Conjunct extends Sentence
{
	def primary:Sentence
	def secondary:Sentence
}

case class CoordinativeConjunct(
	primary:Sentence,
	conjunction:Meaning,
	secondary:Sentence)
	extends Conjunct

/** "A if B" and "if B A" both parse to SC(if, B, A) */
case class SubordinativeConjunct(
	conjunction:Meaning,
	secondary:Sentence,
	primary:Sentence)
	extends Conjunct

case class CorrelativeConjunct(
	conjunction1:Meaning,
	secondary:Sentence,
	conjunction2:Meaning,
	primary:Sentence)
	extends Conjunct

