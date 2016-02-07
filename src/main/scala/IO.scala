package leia.parse

import
	scala.io.Source,
	scala.util.parsing.json.JSON.{ parseFull }

object IO
{
	/** Gets the text at a file name. */
	def read(fileName:String) =
		Source fromFile fileName mkString

	/**
	Gets the JSON at a file name.
	Type is the expected type of the JSON (eg: Map[String, Int])
	*/
	def readJSON[Type](fileName:String): Type =
		parseFull(read(fileName)).getOrElse {
			throw new Error(s"Bad JSON in $fileName (use test-json.js)")
		}.asInstanceOf[Type]
}
