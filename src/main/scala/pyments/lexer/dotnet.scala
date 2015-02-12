package pyments.lexer

import java.lang.Number

import pyments._
import pyments._
import java.util.regex.Pattern
import Helpers._

/*
	For
    `Visual Basic.NET <http://msdn2.microsoft.com/en-us/vbasic/default.aspx>`_
    source code.
*/
class VbNetLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "VB.net"

  override val aliases = "vb.net" :: "vbnet" :: Nil
  override val fileNames = "*.vb" :: "*.bas" :: Nil
  override val mimeTypes = "text/x-vbnet" :: "text/x-vba" :: Nil

  override val flags = Pattern.CASE_INSENSITIVE | Pattern.MULTILINE

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""^\s*<.*?>""", Name.Attribute),
      ("""\s+""", Text),
      ("""\n""", Text),
      ("""rem\b.*?\n""", Comment),
      ("'.*?\n", Comment),
      ("""#If\s.*?\sThen|#ElseIf\s.*?\sThen|#End\s+If|#Const|""" +
        """#ExternalSource.*?\n|#End\s+ExternalSource|""" +
        """#Region.*?\n|#End\s+Region|#ExternalChecksum""",
        Comment.Preproc),
      ("""[\(\){}!#,.:]""", Punctuation),
      ("""Option\s+(Strict|Explicit|Compare)\s+""" +
        """(On|Off|Binary|Text)""", Keyword.Declaration),
      ("""(?<!\.)(AddHandler|Alias|""" +
        """ByRef|ByVal|Call|Case|Catch|CBool|CByte|CChar|CDate|""" +
        """CDec|CDbl|CInt|CLng|CObj|Continue|CSByte|CShort|""" +
        """CSng|CStr|CType|CUInt|CULng|CUShort|Declare|""" +
        """Default|Delegate|DirectCast|Do|Each|Else|ElseIf|""" +
        """EndIf|Erase|Error|Event|Exit|False|Finally|For|""" +
        """Friend|Get|Global|GoSub|GoTo|Handles|If|""" +
        """Implements|Inherits|Interface|""" +
        """Let|Lib|Loop|Me|MustInherit|""" +
        """MustOverride|MyBase|MyClass|Narrowing|New|Next|""" +
        """Not|Nothing|NotInheritable|NotOverridable|Of|On|""" +
        """Operator|Option|Optional|Overloads|Overridable|""" +
        """Overrides|ParamArray|Partial|Private|Protected|""" +
        """Public|RaiseEvent|ReadOnly|ReDim|RemoveHandler|Resume|""" +
        """Return|Select|Set|Shadows|Shared|Single|""" +
        """Static|Step|Stop|SyncLock|Then|""" +
        """Throw|To|True|Try|TryCast|Wend|""" +
        """Using|When|While|Widening|With|WithEvents|""" +
        """WriteOnly)\b""", Keyword),
      ("""(?<!\.)End\b""", Keyword) >> "end",
      ("""(?<!\.)(Dim|Const)\b""", Keyword) >> "dim",
      ("""(?<!\.)(Function|Sub|Property)(\s+)""",
        ByGroups(Keyword, Text)) >> "funcname",
      ("""(?<!\.)(Class|Structure|Enum)(\s+)""",
        ByGroups(Keyword, Text)) >> "classname",
      ("""(?<!\.)(Module|Namespace|Imports)(\s+)""",
        ByGroups(Keyword, Text)) >> "namespace",
      ("""(?<!\.)(Boolean|Byte|Char|Date|Decimal|Double|Integer|Long|""" +
        """Object|SByte|Short|Single|String|Variant|UInteger|ULong|""" +
        """UShort)\b""", Keyword.Type),
      ("""(?<!\.)(AddressOf|And|AndAlso|As|GetType|In|Is|IsNot|Like|Mod|""" +
        """Or|OrElse|TypeOf|Xor)\b""", Operator.Word),
      ("""&=|[*]=|/=|\\=|\^=|\+=|-=|<<=|>>=|<<|>>|:=|""" +
        """<=|>=|<>|[-&*/\\^+=<>]""",
        Operator),
      ("\"", Str) >> "string",
      ("""[a-zA-Z_][a-zA-Z0-9_]*[%&@!#$]?""", Name),
      ("""#.*?#""", Literal.Date),
      ("""(\d+\.\d*|\d*\.\d+)([fF][+-]?[0-9]+)?""", pyments.Number.Float),
      ("""\d+([SILDFR]|US|UI|UL)?""", pyments.Number.Integer),
      ("""&H[0-9a-f]+([SILDFR]|US|UI|UL)?""", pyments.Number.Integer),
      ("""&O[0-7]+([SILDFR]|US|UI|UL)?""", pyments.Number.Integer),
      ("""_\n""", Text) // Line continuation
      )),
    ("string", List[Definition](
      ("\"\"", Str),
      (""""C?""", Str) >> Pop,
      ("""[^"]+""", Str))),
    ("dim", List[Definition](
      ("""[a-z_][a-z0-9_]*""", Name.Variable) >> Pop,
      ("""""", Text) >> Pop // any other syntax
      )),
    ("funcname", List[Definition](
      ("""[a-z_][a-z0-9_]*""", Name.Function) >> Pop)),
    ("classname", List[Definition](
      ("""[a-z_][a-z0-9_]*""", Name.Class) >> Pop)),
    ("namespace", List[Definition](
      ("""[a-z_][a-z0-9_.]*""", Name.Namespace) >> Pop)),
    ("end", List[Definition](
      ("""\s+""", Text),
      ("""(Function|Sub|Property|Class|Structure|Enum|Module|Namespace)\b""", Keyword) >> Pop,
      ("""""", Text) >> Pop)))
}
/*
Lexer for ASP.NET pages.
*/
class GenericAspxLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "aspx-gen"

  override val aliases = Nil
  override val fileNames = Nil
  override val mimeTypes = Nil

  override val flags = Pattern.DOTALL

  private val xml = new XmlLexer(options)

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""(<%[@=#]?)(.*?)(%>)""", ByGroups(Name.Tag, Other, Name.Tag)),
      ("""(<script.*?>)(.*?)(</script>)""", ByGroups(Using(xml), Other, Using(xml))),
      ("""(.+?)(?=<)""", Using(xml)),
      (""".+""", Using(xml)))))
}

/*
Lexer for highligting Visual Basic.net within ASP.NET pages.
*/
class VbNetAspxLexer(val options: LexerOptions) extends DelegatingLexer {
  def _root_lexer() = new VbNetLexer(options)

  def _language_lexer() = new GenericAspxLexer(options)

  override val name = "aspx-vb"

  override val aliases = "aspx-vb" :: Nil
  override val fileNames = "*.aspx" :: "*.asax" :: "*.ascx" :: "*.ashx" :: "*.asmx" :: "*.axd" :: Nil
  override val mimeTypes = Nil

}

/*
For `Boo <http://boo.codehaus.org/>`_ source code.
*/
class BooLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "Boo"

  override val aliases = "boo" :: Nil
  override val fileNames = "*.boo" :: Nil
  override val mimeTypes = "text/x-boo" :: Nil

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""\s+""", Text),
      ("""(#|//).*$""", Comment.Single),
      ("""/[*]""", Comment.Multiline) >> "comment",
      ("""[\[\]\{\}\:\(\),\.;\[\]]""", Punctuation),
      ("""\\\n""", Text),
      ("""\\""", Text),
      ("""(in|is|and|or|not)\b""", Operator.Word),
      ("""/(\\\\|\\/|[^/\s])/""", Str.Regex),
      ("""@/(\\\\|\\/|[^/])*/""", Str.Regex),
      ("""=~|!=|==|<<|>>|[-+/*%=<>&^|]""", Operator),
      ("""(as|abstract|callable|constructor|destructor|do|import|""" +
        """enum|event|final|get|interface|internal|of|override|""" +
        """partial|private|protected|public|return|set|static|""" +
        """struct|transient|virtual|yield|super|and|break|cast|""" +
        """continue|elif|else|ensure|except|for|given|goto|if|in|""" +
        """is|isa|not|or|otherwise|pass|raise|ref|try|unless|when|""" +
        """while|from|as)\b""", Keyword),
      ("""def(?=\s+\(.*?\))""", Keyword),
      ("""(def)(\s+)""", ByGroups(Keyword, Text)) >> "funcname",
      ("""(class)(\s+)""", ByGroups(Keyword, Text)) >> "classname",
      ("""(namespace)(\s+)""", ByGroups(Keyword, Text)) >> "namespace",
      ("""(?<!\.)(true|false|null|self|__eval__|__switch__|array|""" +
        """assert|checked|enumerate|filter|getter|len|lock|map|""" +
        """matrix|max|min|normalArrayIndexing|print|property|range|""" +
        """rawArrayIndexing|required|typeof|unchecked|using|""" +
        """yieldAll|zip)\b""", Name.Builtin),
      ("\"\"\"(\\\\|\\\"|.*?)\"\"\"", Str.Double),
      ("\"\"\"(\\\\|\\\"|[^\"]*?)\"\"\"", Str.Double),
      ("'(\\\\|\\'|[^']*?)'", Str.Single),
      ("""[a-zA-Z_][a-zA-Z0-9_]*""", Name),
      ("""(\d+\.\d*|\d*\.\d+)([fF][+-]?[0-9]+)?""", pyments.Number.Float),
      ("""[0-9][0-9\.]*(m|ms|d|h|s)""", pyments.Number),
      ("""0\d+""", pyments.Number.Oct),
      ("""0x[a-fA-F0-9]+""", pyments.Number.Hex),
      ("""\d+L""", pyments.Number.Integer.Long),
      ("""\d+""", pyments.Number.Integer))),
    ("comment", List[Definition](
      ("""/[*]""", Comment.Multiline) >> Push,
      ("""[*]/""", Comment.Multiline) >> Pop,
      ("""[^/*]""", Comment.Multiline),
      ("""[*/]""", Comment.Multiline))),
    ("funcname", List[Definition](
      ("""[a-zA-Z_][a-zA-Z0-9_]*""", Name.Function) >> Pop)),
    ("classname", List[Definition](
      ("""[a-zA-Z_][a-zA-Z0-9_]*""", Name.Class) >> Pop)),
    ("namespace", List[Definition](
      ("""[a-zA-Z_][a-zA-Z0-9_.]*""", Name.Namespace) >> Pop)))
}

class CSharpLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "C#"

  override val aliases = "csharp" :: "c#" :: Nil
  override val fileNames = "*.cs" :: Nil
  override val mimeTypes = "text/x-csharp" :: Nil

  override val flags = Pattern.MULTILINE | Pattern.DOTALL

  val cs_ident = """@?[_a-zA-Z][a-zA-Z0-9_]*"""

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      //method names
      ("""^([ \t]*(?:""" + cs_ident + """(?:\[\])?\s+)+?)""" + //return type
        """(""" + cs_ident + """)""" + //method name
        """(\s*)(\()""", //signature start
        ByGroups(Using(this), Name.Function, Text, Punctuation)),
      ("""^\s*\[.*?\]""", Name.Attribute),
      ("""[^\S\n]+""", Text),
      ("""\\\n""", Text), //line continuation
      ("""//.*?\n""", Comment.Single),
      ("""/[*](.|\n)*?[*]/""", Comment.Multiline),
      ("""\n""", Text),
      ("""[~!%^&*()+=|\[\]:;,.<>/?-]""", Punctuation),
      ("""[{}]""", Punctuation),
      ("""@"(\\\\|\\"|[^"])*"""", Str),
      (""""(\\\\|\\"|[^"\n])*["\n]""", Str),
      ("""'\\.'|'[^\\]'""", Str.Char),
      ("""[0-9](\.[0-9]*)?([eE][+-][0-9]+)?[flFLdD]?|0[xX][0-9a-fA-F]+[Ll]?""", pyments.Number),
      ("""#[ \t]*(if|endif|else|elif|define|undef|""" +
        """line|error|warning|region|endregion|pragma)\b.*?\n""", Comment.Preproc),
      ("""\b(extern)(\s+)(alias)\b""", ByGroups(Keyword, Text, Keyword)),
      ("""(abstract|as|base|break|case|catch|""" +
        """checked|const|continue|default|delegate|""" +
        """do|else|enum|event|explicit|extern|false|finally|""" +
        """fixed|for|foreach|goto|if|implicit|in|interface|""" +
        """internal|is|lock|new|null|operator|""" +
        """out|override|params|private|protected|public|readonly|""" +
        """ref|return|sealed|sizeof|stackalloc|static|""" +
        """switch|this|throw|true|try|typeof|""" +
        """unchecked|unsafe|virtual|void|while|""" +
        """get|set|new|partial|yield|add|remove|value)\b""", Keyword),
      ("""(global)(::)""", ByGroups(Keyword, Punctuation)),
      ("""(bool|byte|char|decimal|double|float|int|long|object|sbyte|""" +
        """short|string|uint|ulong|ushort)\b\??""", Keyword.Type),
      ("""(class|struct)(\s+)""", ByGroups(Keyword, Text)) >> "class",
      ("""(namespace|using)(\s+)""", ByGroups(Keyword, Text)) >> "namespace",
      (cs_ident, Name))),
    ("class", List[Definition](
      (cs_ident, Name.Class) >> Pop)),
    ("namespace", List[Definition](
      ("""(?=\()""", Text) >> Pop, //using (resource)
      ("""(""" + cs_ident + """|\.)+""", Name.Namespace) >> Pop)))
}
