package leia.parse;

/**
Provides a Java interface for the parser.
Methods wrap those in All.scala.
*/
public class DependencyParse
{
	public static String sentenceDetail(String text)
	{
		return All$.MODULE$.sentenceDetail(text);
	}

	public static SentencePart[] parse(String text)
	{
		return All$.MODULE$.apply(text);
	}
}
