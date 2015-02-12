package pyments.lexer

import pyments._
import pyments._
import java.util.regex.Pattern
import Helpers._

/*
Lexer for `CMake <http://cmake.org/Wiki/CMake>`_ files.
*/
class CMakeLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "CMake"
  override val aliases = "cmake" :: Nil
  override val fileNames = "*.cmake" :: "CMakeLists.txt" :: Nil
  override val mimeTypes = "text/x-cmake" :: Nil

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""\b([A-Za-z_]+)([ \t]*)(\()""", ByGroups(Name.Builtin, Text, Punctuation)) >> "args",
      Include("keywords"),
      Include("ws"))),
    ("args", List[Definition](
      ("""\(""", Punctuation) >> Push,
      ("""\)""", Punctuation) >> Pop,
      ("""(\$\{)(.+?)(\})""", ByGroups(Operator, Name.Variable, Operator)),
      ("""(?s)".*?"""", Str.Double),
      ("""\\\S+""", Str),
      ("""[^\)$"# \t\n]+""", Str),
      ("""\n""", Text), // explicitly legal
      Include("keywords"),
      Include("ws"))),
    ("keywords", List[Definition](
      ("""\b(WIN32|UNIX|APPLE|CYGWIN|BORLAND|MINGW|MSVC|MSVC_IDE|MSVC60|MSVC70|MSVC71|MSVC80|MSVC90)\b""", Keyword))),
    ("ws", List[Definition](
      ("""[ \t]+""", Text),
      ("""#.+\n""", Comment))))
}
