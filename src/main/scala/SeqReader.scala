package leia.parse

/**
Wraps a sequence and pretends to be streaming.
Don't waste your time reading this.
*/

import scala.util.parsing.input.{ Position, Reader }

object SeqReader
{
	def apply(seq:Seq[Lexed]): SeqReader =
		SeqReader(0, seq)
}

case class SeqReader(_pos:Int, seq:Seq[Lexed]) extends Reader[Lexed]
{
	def atEnd =
		_pos >= seq.size

	def first =
		if (atEnd)
			UnknownWord("off the edge")
		else
			seq(_pos)

	def rest =
		SeqReader(_pos + 1, seq)

	def pos =
		SeqPosition(_pos, seq)
}

case class SeqPosition(pos:Int, seq:Seq[Lexed]) extends Position
{
	def column =
		pos
	def line =
		0

	def lineContents =
		if (0 <= pos && pos < seq.size)
			seq(pos).toString
		else
			s"index $pos"
}

