package pyments.lexer

import java.lang.Number

import pyments._
import pyments._
import java.util.regex.Pattern
import Helpers._

/*
For the output of 'objdump -Sr on compiled C files'
*/
class CObjdumpLexer(val options: LexerOptions) extends DelegatingLexer {
  override val name = "c-objdump"
  override val aliases = "c-objdump" :: Nil
  override val fileNames = "*.c-objdump" :: Nil
  override val mimeTypes = "text/x-c-objdump" :: Nil

  def _root_lexer() = new CLexer(options)

  def _language_lexer() = new ObjdumpLexer(options)

}

/*
For the output of 'objdump -Sr on compiled C++ files'
*/
class CppObjdumpLexer(val options: LexerOptions) extends DelegatingLexer {

  override val name = "cpp-objdump"
  override val aliases = "cpp-objdump" :: "c++-objdumb" :: "cxx-objdump" :: Nil
  override val fileNames = "*.cpp-objdump" :: "*.c++-objdump" :: "*.cxx-objdump" :: Nil
  override val mimeTypes = "text/x-cpp-objdump" :: Nil

  def _root_lexer() = new CppLexer(options)

  def _language_lexer() = new ObjdumpLexer(options)
}

/*
For the output of 'objdump -dr'
*/
class ObjdumpLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "objdump"
  override val aliases = "objdump" :: Nil
  override val fileNames = "*.objdump" :: Nil
  override val mimeTypes = "text/x-objdump" :: Nil

  val hex = "[0-9A-Za-z]"

  private val gas = new GasLexer(options)

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      // File name & format:
      ("""(.*?)(:)( +file format )(.*?)$""",
        ByGroups(Name.Label, Punctuation, Text, Str)),
      // Section header
      ("""(Disassembly of section )(.*?)(:)$""",
        ByGroups(Text, Name.Label, Punctuation)),
      // Function labels
      // (With offset)
      ("""(""" + hex + """+)( )(<)(.*?)([-+])(0[xX][A-Za-z0-9]+)(>:)$""",
        ByGroups(pyments.Number.Hex, Text, Punctuation, Name.Function,
          Punctuation, pyments.Number.Hex, Punctuation)),
      // (Without offset)
      ("""(""" + hex + """+)( )(<)(.*?)(>:)$""",
        ByGroups(pyments.Number.Hex, Text, Punctuation, Name.Function,
          Punctuation)),
      // Code line with disassembled instructions
      ("""( *)(""" + hex + """+:)(\t)((?:""" + hex + hex + """ )+)( *\t)([a-zA-Z].*?)$""",
        ByGroups(Text, Name.Label, Text, pyments.Number.Hex, Text,
          Using(gas))),
      // Code line with ascii
      ("""( *)(""" + hex + """+:)(\t)((?:""" + hex + hex + """ )+)( *)(.*?)$""",
        ByGroups(Text, Name.Label, Text, pyments.Number.Hex, Text, Str)),
      // Continued code line, only raw opcodes without disassembled
      // instruction
      ("""( *)(""" + hex + """+:)(\t)((?:""" + hex + hex + """ )+)$""",
        ByGroups(Text, Name.Label, Text, pyments.Number.Hex)),
      // Skipped a few bytes
      ("""\t\.\.\.$""", Text),
      // Relocation line
      // (With offset)
      ("""(\t\t\t)(""" + hex + """+:)( )([^\t]+)(\t)(.*?)([-+])(0x""" + hex + """+)$""",
        ByGroups(Text, Name.Label, Text, Name.Property, Text,
          Name.Constant, Punctuation, pyments.Number.Hex)),
      // (Without offset)
      ("""(\t\t\t)(""" + hex + """+:)( )([^\t]+)(\t)(.*?)$""",
        ByGroups(Text, Name.Label, Text, Name.Property, Text,
          Name.Constant)),
      ("""[^\n]+\n""", Other))))
}

/*
For Gas (AT&T) assembly code.
*/
class GasLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "GAS"
  override val aliases = "gas" :: Nil
  override val fileNames = "*.s" :: "*.S" :: Nil
  override val mimeTypes = "text/x-gas" :: Nil

  val hex = "[0-9A-Za-z]"

  //: optional Comment or Whitespace
  val string = """"(\\"|[^"])*""""
  val char = """[a-zA-Z$._0-9@]"""
  val identifier = """(?:[a-zA-Z$_]""" + char + """*|\.""" + char + """+)"""
  val number = """(?:0[xX][a-zA-Z0-9]+|\d+)"""

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      Include("whitespace"),
      (identifier + """:""", Name.Label),
      ("""\.""" + identifier, Name.Attribute) >> "directive-args",
      ("""lock|rep(n?z)?|data\d+""", Name.Attribute),
      (identifier, Name.Function) >> "instruction-args",
      ("""[\r\n]+""", Text))),
    ("directive-args", List[Definition](
      (identifier, Name.Constant),
      (string, Str),
      ("""@""" + identifier, Name.Attribute),
      (number, pyments.Number.Integer),
      ("""[\r\n]+""", Text) >> Pop,
      ("""#.*?$""", Comment) >> Pop,
      Include("punctuation"),
      Include("whitespace"))),
    ("instruction-args", List[Definition](
      // For objdump-disassembled code, shouldn't occur in
      // actual assembler input
      ("""([a-z0-9]+)( )(<)(""" + identifier + """)(>)""",
        ByGroups(pyments.Number.Hex, Text, Punctuation, Name.Constant, Punctuation)),
      ("""([a-z0-9]+)( )(<)(""" + identifier + """)([-+])(""" + number + """)(>)""",
        ByGroups(pyments.Number.Hex, Text, Punctuation, Name.Constant,
          Punctuation, pyments.Number.Integer, Punctuation)),

      // Address constants
      (identifier, Name.Constant),
      (number, pyments.Number.Integer),
      // Registers
      ("""%""" + identifier, Name.Variable),
      // Numeric constants
      ("""$""" + number, pyments.Number.Integer),
      ("""[\r\n]+""", Text) >> Pop,
      ("""#.*?$""", Comment) >> Pop,
      Include("punctuation"),
      Include("whitespace"))),
    ("whitespace", List[Definition](
      ("""\n""", Text),
      ("""\s+""", Text),
      ("""#.*?\n""", Comment))),
    ("punctuation", List[Definition](
      ("""[-*,.():]+""", Punctuation))))
}
