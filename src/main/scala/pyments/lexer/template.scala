
package pyments.lexer

import java.lang.Number

import pyments._
import pyments._
import java.util.regex.Pattern
import Helpers._

/*
Coldfusion statements
*/
class ColdfusionLexer(val options: LexerOptions) extends RegexLexer {

  override val name = "cfstatement"
  override val aliases = "cfs" :: Nil
  override val fileNames = Nil
  override val mimeTypes = Nil
  override val flags = Pattern.CASE_INSENSITIVE | Pattern.MULTILINE

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""//.*""", Comment),
      ("""\+\+|--""", Operator),
      ("""[-+*/^&=!]""", Operator),
      ("""<=|>=|<|>""", Operator),
      ("""mod\b""", Operator),
      ("""(eq|lt|gt|lte|gte|not|is|and|or)\b""", Operator),
      ("""\|\||&&""", Operator),
      (""""""", Str.Double) >> "string",
      // There is a special rule for allowing html in single quoted
      // strings, evidently.
      ("'.*?'", Str.Single),
      ("""\d+""", pyments.Number),
      ("""(if|else|len|var|case|default|break|switch)\b""", Keyword),
      ("""([A-Za-z_$][A-Za-z0-9_.]*)\s*(\()""", ByGroups(Name.Function, Punctuation)),
      ("""[A-Za-z_$][A-Za-z0-9_.]*""", Name.Variable),
      ("""[()\[\]{};:,.\\]""", Punctuation),
      ("""\s+""", Text))),
    ("string", List[Definition](
      ("""""""", Str.Double),
      ("""#.+?#""", Str.Interpol),
      ("""[^"#]+""", Str.Double),
      ("""#""", Str.Double),
      (""""""", Str.Double) >> Pop)))
}

/*
Coldfusion markup only
*/
class ColdfusionMarkupLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "Coldfusion"
  override val aliases = "cf" :: Nil
  override val fileNames = Nil
  override val mimeTypes = Nil

  private val cf = new ColdfusionLexer(options)

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""[^<]+""", Other),
      Include("tags"),
      ("""<[^<>]*""", Other))),
    ("tags", List[Definition](
      ("""(?s)<!---.*?--->""", Comment.Multiline),
      ("""(?s)<!--.*?-->""", Comment),
      ("""<cfoutput.*?>""", Name.Builtin) >> "cfoutput",
      ("""(?s)(<cfscript.*?>)(.+?)(</cfscript.*?>)""",
        ByGroups(Name.Builtin, Using(cf), Name.Builtin)),
      // negative lookbehind is for strings with embedded >
      ("""(?s)(</?cf(?:component|include|if|else|elseif|loop|return|""" +
        """dbinfo|dump|abort|location|invoke|throw|file|savecontent|""" +
        """mailpart|mail|header|content|zip|image|lock|argument|try|""" +
        """catch|break|directory|http|set|function|param)\b)(.*?)((?<!\\)>)""",
        ByGroups(Name.Builtin, Using(cf), Name.Builtin)))),
    ("cfoutput", List[Definition](
      ("""[^#<]+""", Other),
      ("""(#)(.*?)(#)""", ByGroups(Punctuation, Using(cf),
        Punctuation)),
      //(r'<cfoutput.*?>""", Name.Builtin) >> Push,
      ("""</cfoutput.*?>""", Name.Builtin) >> Pop,
      Include("tags"),
      ("""(?s)<[^<>]*""", Other),
      ("""#""", Other))))
}

/*
Coldfusion markup in html
*/
class ColdfusionHtmlLexer(val options: LexerOptions) extends DelegatingLexer {
  override val name = "Coldfusion HTML"
  override val aliases = "cfm" :: Nil
  override val fileNames = "*.cfm" :: "*.cfml" :: "*.cfc" :: Nil
  override val mimeTypes = "application/x-coldfusion" :: Nil

  def _root_lexer() = new ColdfusionMarkupLexer(options)

  def _language_lexer() = new HtmlLexer(options)
}
