package pyments.lexer

import java.lang.Number

import pyments._
import pyments._
import java.util.regex.Pattern
import Helpers._

/*
Generic pyments.lexer for XML (eXtensible Markup Language).
*/
class XmlLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "XML"

  override val aliases = "xml" :: Nil
  override val fileNames = "*.xml" :: "*.xsl" :: "*.rss" :: "*.xslt" :: "*.xsd" :: "*.wsdl" :: Nil
  override val mimeTypes = "text/xml" :: "application/xml" :: "image/svg+xml" :: "application/rss+xml" :: "application/atom+xml" :: Nil

  override val flags = Pattern.DOTALL | Pattern.MULTILINE

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""[^<&]+""", Text),
      ("""&\S*?;""", Name.Entity),
      ("""\<\!\[CDATA\[.*?\]\]\>""", Comment.Preproc),
      ("""<!--""", Comment) >> "comment",
      ("""<\?.*?\?>""", Comment.Preproc),
      ("""<![^>]*>""", Comment.Preproc),
      ("""<\s*[a-zA-Z0-9:._-]+""", Name.Tag) >> "tag",
      ("""<\s*/\s*[a-zA-Z0-9:._-]+\s*>""", Name.Tag))),
    ("comment", List[Definition](
      ("""[^-]+""", Comment),
      ("""-->""", Comment) >> Pop,
      ("""-""", Comment))),
    ("tag", List[Definition](
      ("""\s+""", Text),
      ("""[a-zA-Z0-9_.:-]+\s*=""", Name.Attribute) >> "attr",
      ("""/?\s*>""", Name.Tag) >> Pop)),
    ("attr", List[Definition](
      ("""\s+""", Text),
      ("\".*?\"", Str) >> Pop,
      ("""'.*?'""", Str) >> Pop,
      ("""[^\s>]+""", Str) >> Pop)))
}

/*
For ActionScript 3 source code.
*/
class ActionScript3Lexer(val options: LexerOptions) extends RegexLexer {
  override val name = "ActionScript 3"

  override val aliases = "as3" :: "actionscript3" :: Nil
  override val fileNames = "*.as" :: Nil
  override val mimeTypes = "application/x-actionscript" :: "text/x-actionscript" :: "text/actionscript" :: Nil

  override val flags = Pattern.DOTALL | Pattern.MULTILINE

  val identifier = """[$a-zA-Z_][a-zA-Z0-9_]*"""
  val typeidentifier = identifier + """(?:\.<\w+>)?"""

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""\s+""", Text),
      ("""(function\s+)(""" + identifier + """)(\s*)(\()""",
        ByGroups(Keyword.Declaration, Name.Function, Text, Operator)) >> "funcparams",
      ("""(var|const)(\s+)(""" + identifier + """)(\s*)(:)(\s*)(""" +
        typeidentifier + """)""",
        ByGroups(Keyword.Declaration, Text, Name, Text, Punctuation, Text, Keyword.Type)),
      ("""(import|package)(\s+)((?:""" + identifier + """|\.)+)(\s*)""",
        ByGroups(Keyword, Text, Name.Namespace, Text)),
      ("""(new)(\s+)(""" + typeidentifier + """)(\s*)(\()""",
        ByGroups(Keyword, Text, Keyword.Type, Text, Operator)),
      ("""//.*?\n""", Comment.Single),
      ("""/\*.*?\*/""", Comment.Multiline),
      ("""/(\\\\|\\/|[^\n])*/[gisx]*""", Str.Regex),
      ("""(\.)(""" + identifier + """)""", ByGroups(Operator, Name.Attribute)),
      ("""(case|default|for|each|in|while|do|break|return|continue|if|else|""" +
        """throw|try|catch|with|new|typeof|arguments|instanceof|this|""" +
        """switch|import|include|as|is)\b""",
        Keyword),
      ("""(class|public|final|internal|native|override|private|protected|""" +
        """static|import|extends|implements|interface|intrinsic|return|super|""" +
        """dynamic|function|const|get|namespace|package|set)\b""",
        Keyword.Declaration),
      ("""(true|false|null|NaN|Infinity|-Infinity|undefined|void)\b""",
        Keyword.Constant),
      ("""(decodeURI|decodeURIComponent|encodeURI|escape|eval|isFinite|isNaN|""" +
        """isXMLName|clearInterval|fscommand|getTimer|getURL|getVersion|""" +
        """isFinite|parseFloat|parseInt|setInterval|trace|updateAfterEvent|""" +
        """unescape)\b""", Name.Function),
      (identifier, Name),
      ("""[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?""", pyments.Number.Float),
      ("""0x[0-9a-f]+""", pyments.Number.Hex),
      ("""[0-9]+""", pyments.Number.Integer),
      (""""(\\\\|\\"|[^"])*"""", Str.Double),
      ("'(\\\\|\\'|[^'])*'", Str.Single),
      ("""[~\^\*!%&<>\|+=:;,/?\\{}\[\]();.-]+""", Operator))),
    ("funcparams", List[Definition](
      ("""\s+""", Text),
      ("""(\s*)(\.\.\.)?(""" + identifier + """)(\s*)(:)(\s*)(""" +
        typeidentifier + """|\*)(\s*)""",
        ByGroups(Text, Punctuation, Name, Text, Operator, Text,
          Keyword.Type, Text)) >> "defval",
      ("""\)""", Operator) >> "type")),
    ("type", List[Definition](
      ("""(\s*)(:)(\s*)(""" + typeidentifier + """|\*)""",
        ByGroups(Text, Operator, Text, Keyword.Type)) >> Pop(2),
      ("""\s*""", Text) >> Pop(2))),
    ("defval", List[Definition](
      ("""(=)(\s*)([^(),]+)(\s*)(,?)""",
        ByGroups(Operator, Text, Using(this), Text, Operator)) >> Pop,
      (""",?""", Operator) >> Pop)))
}

/*
For `CoffeeScript`_ source code. _CoffeeScript: http://coffeescript.org
*/
class CoffeeScriptLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "CoffeeScript"

  override val aliases = "coffee-script" :: "coffeescript" :: Nil
  override val fileNames = "*.coffee" :: Nil
  override val mimeTypes = "application/coffeescript" :: Nil

  override val flags = Pattern.DOTALL

  val tokens = Map[String, StateDef](
    ("commentsandwhitespace", List[Definition](
      ("""\s+""", Text),
      ("""###.*?###""", Comment.Multiline),
      ("""#.*?\n""", Comment.Single))),
    ("multilineregex", List[Definition](
      Include("commentsandwhitespace"),
      ("""///([gim]+\b|\B)""", Str.Regex) >> Pop,
      ("""/""", Str.Regex),
      ("""[^/#]+""", Str.Regex))),
    ("slashstartsregex", List[Definition](
      Include("commentsandwhitespace"),
      ("""///""", Str.Regex) >> (Pop, "multilineregex"),
      ("""/(\\.|[^[/\\\n]|\[(\\.|[^\]\\\n])*])+/""" +
        """([gim]+\b|\B)""", Str.Regex) >> Pop,
      ("""""", Text) >> Pop)),
    ("root", List[Definition](
      ("""^(?=\s|/|<!--)""", Text) >> "slashstartsregex",
      Include("commentsandwhitespace"),
      ("""\+\+|--|~|&&|\band\b|\bor\b|\bis\b|\bisnt\b|\bnot\b|\?|:|=|""" +
        """\|\||\\(?=\n)|(<<|>>>?|==?|!=?|[-<>+*`%&\|\^/])=?""",
        Operator) >> "slashstartsregex",
      ("""\([^()]*\)\s*->""", Name.Function),
      ("""[{(\[;,]""", Punctuation) >> "slashstartsregex",
      ("""[})\].]""", Punctuation),
      ("""(for|in|of|while|break|return|continue|switch|when|then|if|else|""" +
        """throw|try|catch|finally|new|delete|typeof|instanceof|super|""" +
        """extends|this|class|by)\b""", Keyword) >> "slashstartsregex",
      ("""(true|false|yes|no|on|off|null|NaN|Infinity|undefined)\b""",
        Keyword.Constant),
      ("""(Array|Boolean|Date|Error|Function|Math|netscape|""" +
        """Number|Object|Packages|RegExp|String|sun|decodeURI|""" +
        """decodeURIComponent|encodeURI|encodeURIComponent|""" +
        """eval|isFinite|isNaN|parseFloat|parseInt|document|window)\b""",
        Name.Builtin),
      ("""[$a-zA-Z_][a-zA-Z0-9_\.:]*\s*[:=]\s""", Name.Variable) >> "slashstartsregex",
      ("""@[$a-zA-Z_][a-zA-Z0-9_\.:]*\s*[:=]\s""", Name.Variable.Instance) >> "slashstartsregex",
      ("""@""", Name.Other) >> "slashstartsregex",
      ("""@?[$a-zA-Z_][a-zA-Z0-9_]*""", Name.Other) >> "slashstartsregex",
      ("""[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?""", pyments.Number.Float),
      ("""0x[0-9a-fA-F]+""", pyments.Number.Hex),
      ("""[0-9]+""", pyments.Number.Integer),
      (""""""""", Str) >> "tdqs",
      ("'''", Str) >> "tdqs",
      (""""""", Str) >> "dqs",
      ("'", Str) >> "sqs")),
    ("strings", List[Definition](
      ("""[^#\\\'"]+""", Str) // note that all coffee script strings are multi-line.
      // hashmarks, quotes and backslashes must be parsed one at a time
      )),
    ("interpoling_string", List[Definition](
      ("""\}""", Str.Interpol) >> Pop,
      Include("root"))),
    ("dqs", List[Definition](
      (""""""", Str) >> Pop,
      ("""\\.|\'""", Str), // double-quoted string don't need ' escapes
      ("""#\{""", Str.Interpol) >> "interpoling_string",
      Include("strings"))),
    ("sqs", List[Definition](
      ("'", Str) >> Pop,
      ("""#|\\.|"""", Str), // single quoted strings don't need " escapses
      Include("strings"))),
    ("tdqs", List[Definition](
      (""""""""", Str) >> Pop,
      ("""\\.|\'|"""", Str), // no need to escape quotes in triple-string
      ("""#\{""", Str.Interpol) >> "interpoling_string",
      Include("strings"))),
    ("tsqs", List[Definition](
      ("'''", Str) >> Pop,
      ("""#|\\.|\'|"""", Str), // no need to escape quotes in triple-strings
      Include("strings"))))
}

/*
For HTML 4 and XHTML 1 markup. Nested JavaScript and CSS is highlighted
    by the appropriate pyments.lexer.
    */

class HtmlLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "HTML"

  override val aliases = "html" :: Nil
  override val fileNames = "*.html" :: "*.htm" :: "*.xhtml" :: "*.xslt" :: Nil
  override val mimeTypes = "text/html" :: "application/xhtml+xml" :: Nil

  override val flags = Pattern.DOTALL | Pattern.CASE_INSENSITIVE

  private val js = new JavascriptLexer(options)
  private val css = new CssLexer(options)

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""[^<&]+""", Text),
      ("""&\S*?;""", Name.Entity),
      ("""\<\!\[CDATA\[.*?\]\]\>""", Comment.Preproc),
      ("""<!--""", Comment) >> "comment",
      ("""<\?.*?\?>""", Comment.Preproc),
      ("""<![^>]*>""", Comment.Preproc),
      ("""<\s*script\s*""", Name.Tag) >> ("script-content", "tag"),
      ("""<\s*style\s*""", Name.Tag) >> ("style-content", "tag"),
      ("""<\s*[a-zA-Z0-9:]+""", Name.Tag) >> "tag",
      ("""<\s*/\s*[a-zA-Z0-9:]+\s*>""", Name.Tag))),
    ("comment", List[Definition](
      ("""[^-]+""", Comment),
      ("""-->""", Comment) >> Pop,
      ("""-""", Comment))),
    ("tag", List[Definition](
      ("""\s+""", Text),
      ("""[a-zA-Z0-9_:-]+\s*=""", Name.Attribute) >> "attr",
      ("""[a-zA-Z0-9_:-]+""", Name.Attribute),
      ("""/?\s*>""", Name.Tag) >> Pop)),
    ("script-content", List[Definition](
      ("""<\s*/\s*script\s*>""", Name.Tag) >> Pop,
      (""".+?(?=<\s*/\s*script\s*>)""", Using(js)))),
    ("style-content", List[Definition](
      ("""<\s*/\s*style\s*>""", Name.Tag) >> Pop,
      (""".+?(?=<\s*/\s*style\s*>)""", Using(css)))),
    ("attr", List[Definition](
      ("""".*?"""", Str) >> Pop,
      ("'.*?'", Str) >> Pop,
      ("""[^\s>]+""", Str) >> Pop)))
}

/*
For JavaScript source code.
*/
class JavascriptLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "JavaScript"

  override val aliases = "js" :: "javascript" :: Nil
  override val fileNames = "*.js" :: "*.json" :: Nil
  override val mimeTypes = "application/javascript" :: "application/x-javascript" :: "text/x-javascript" :: "text/javascript" :: "application/json" :: Nil

  override val flags = Pattern.DOTALL

  val tokens = Map[String, StateDef](
    ("commentsandwhitespace", List[Definition](
      ("""\s+""", Text),
      ("""<!--""", Comment),
      ("""//.*?\n""", Comment.Single),
      ("""/\*.*?\*/""", Comment.Multiline))),
    ("slashstartsregex", List[Definition](
      Include("commentsandwhitespace"),
      ("""/(\\.|[^[/\\\n]|\[(\\.|[^\]\\\n])*])+/""" +
        """([gim]+\b|\B)""", Str.Regex) >> Pop,
      ("""(?=/)""", Text) >> (Pop, "badregex"),
      ("""""", Text) >> Pop)),
    ("badregex", List[Definition](
      ("\n", Text) >> Pop)),
    ("root", List[Definition](
      ("""^(?=\s|/|<!--)""", Text) >> "slashstartsregex",
      Include("commentsandwhitespace"),
      ("""\+\+|--|~|&&|\?|:|\|\||\\(?=\n)|""" +
        """(<<|>>>?|==?|!=?|[-<>+*%&\|\^/])=?""", Operator) >> "slashstartsregex",
      ("""[{(\[;,]""", Punctuation) >> "slashstartsregex",
      ("""[})\].]""", Punctuation),
      ("""(for|in|while|do|break|return|continue|switch|case|default|if|else|""" +
        """throw|try|catch|finally|new|delete|typeof|instanceof|void|""" +
        """this)\b""", Keyword) >> "slashstartsregex",
      ("""(var|let|with|function)\b""", Keyword.Declaration) >> "slashstartsregex",
      ("""(abstract|boolean|byte|char|class|const|debugger|double|enum|export|""" +
        """extends|final|float|goto|implements|import|int|interface|long|native|""" +
        """package|private|protected|public|short|static|super|synchronized|throws|""" +
        """transient|volatile)\b""", Keyword.Reserved),
      ("""(true|false|null|NaN|Infinity|undefined)\b""", Keyword.Constant),
      ("""(Array|Boolean|Date|Error|Function|Math|netscape|""" +
        """Number|Object|Packages|RegExp|String|sun|decodeURI|""" +
        """decodeURIComponent|encodeURI|encodeURIComponent|""" +
        """Error|eval|isFinite|isNaN|parseFloat|parseInt|document|this|""" +
        """window)\b""", Name.Builtin),
      ("""[$a-zA-Z_][a-zA-Z0-9_]*""", Name.Other),
      ("""[0-9][0-9]*\.[0-9]+([eE][0-9]+)?[fd]?""", pyments.Number.Float),
      ("""0x[0-9a-fA-F]+""", pyments.Number.Hex),
      ("""[0-9]+""", pyments.Number.Integer),
      (""""(\\\\|\\"|[^"])*"""", Str.Double),
      ("""'(\\\\|\\'|[^'])*'""", Str.Single))))
}

/*
For CSS (Cascading Style Sheets).
*/
class CssLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "CSS"

  override val aliases = "css" :: Nil
  override val fileNames = "*.css" :: Nil
  override val mimeTypes = "text/css" :: Nil

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      Include("basics"))),
    ("basics", List[Definition](
      ("""\s+""", Text),
      ("""/\*(?:.|\n)*?\*/""", Comment),
      ("""\{""", Punctuation) >> "content",
      ("""\:[a-zA-Z0-9_-]+""", Name.Decorator),
      ("""\.[a-zA-Z0-9_-]+""", Name.Class),
      ("""\#[a-zA-Z0-9_-]+""", Name.Function),
      ("""@[a-zA-Z0-9_-]+""", Keyword) >> "atrule",
      ("""[a-zA-Z0-9_-]+""", Name.Tag),
      ("""[~\^\*!%&\[\]\(\)<>\|+=@:;,./?-]""", Operator),
      (""""(\\\\|\\"|[^"])*"""", Str.Double),
      ("""'(\\\\|\\'|[^'])*'""", Str.Single))),
    ("atrule", List[Definition](
      ("""\{""", Punctuation) >> "atcontent",
      (""";""", Punctuation) >> Pop,
      Include("basics"))),
    ("atcontent", List[Definition](
      Include("basics"),
      ("""\}""", Punctuation) >> Pop(2))),
    ("content", List[Definition](
      ("""\s+""", Text),
      ("""\}""", Punctuation) >> Pop,
      ("""url\(.*?\)""", Str.Other),
      ("""^@.*?$""", Comment.Preproc),
      ("""(azimuth|background-attachment|background-color|""" +
        """background-image|background-position|background-repeat|""" +
        """background|border-bottom-color|border-bottom-style|""" +
        """border-bottom-width|border-left-color|border-left-style|""" +
        """border-left-width|border-right|border-right-color|""" +
        """border-right-style|border-right-width|border-top-color|""" +
        """border-top-style|border-top-width|border-bottom|""" +
        """border-collapse|border-left|border-width|border-color|""" +
        """border-spacing|border-style|border-top|border|caption-side|""" +
        """clear|clip|color|content|counter-increment|counter-reset|""" +
        """cue-after|cue-before|cue|cursor|direction|display|""" +
        """elevation|empty-cells|float|font-family|font-size|""" +
        """font-size-adjust|font-stretch|font-style|font-variant|""" +
        """font-weight|font|height|letter-spacing|line-height|""" +
        """list-style-type|list-style-image|list-style-position|""" +
        """list-style|margin-bottom|margin-left|margin-right|""" +
        """margin-top|margin|marker-offset|marks|max-height|max-width|""" +
        """min-height|min-width|opacity|orphans|outline|outline-color|""" +
        """outline-style|outline-width|overflow(?:-x|-y|)|padding-bottom|""" +
        """padding-left|padding-right|padding-top|padding|page|""" +
        """page-break-after|page-break-before|page-break-inside|""" +
        """pause-after|pause-before|pause|pitch|pitch-range|""" +
        """play-during|position|quotes|richness|right|size|""" +
        """speak-header|speak-numeral|speak-punctuation|speak|""" +
        """speech-rate|stress|table-layout|text-align|text-decoration|""" +
        """text-indent|text-shadow|text-transform|top|unicode-bidi|""" +
        """vertical-align|visibility|voice-family|volume|white-space|""" +
        """widows|width|word-spacing|z-index|bottom|left|""" +
        """above|absolute|always|armenian|aural|auto|avoid|baseline|""" +
        """behind|below|bidi-override|blink|block|bold|bolder|both|""" +
        """capitalize|center-left|center-right|center|circle|""" +
        """cjk-ideographic|close-quote|collapse|condensed|continuous|""" +
        """crop|crosshair|cross|cursive|dashed|decimal-leading-zero|""" +
        """decimal|default|digits|disc|dotted|double|e-resize|embed|""" +
        """extra-condensed|extra-expanded|expanded|fantasy|far-left|""" +
        """far-right|faster|fast|fixed|georgian|groove|hebrew|help|""" +
        """hidden|hide|higher|high|hiragana-iroha|hiragana|icon|""" +
        """inherit|inline-table|inline|inset|inside|invert|italic|""" +
        """justify|katakana-iroha|katakana|landscape|larger|large|""" +
        """left-side|leftwards|level|lighter|line-through|list-item|""" +
        """loud|lower-alpha|lower-greek|lower-roman|lowercase|ltr|""" +
        """lower|low|medium|message-box|middle|mix|monospace|""" +
        """n-resize|narrower|ne-resize|no-close-quote|no-open-quote|""" +
        """no-repeat|none|normal|nowrap|nw-resize|oblique|once|""" +
        """open-quote|outset|outside|overline|pointer|portrait|px|""" +
        """relative|repeat-x|repeat-y|repeat|rgb|ridge|right-side|""" +
        """rightwards|s-resize|sans-serif|scroll|se-resize|""" +
        """semi-condensed|semi-expanded|separate|serif|show|silent|""" +
        """slow|slower|small-caps|small-caption|smaller|soft|solid|""" +
        """spell-out|square|static|status-bar|super|sw-resize|""" +
        """table-caption|table-cell|table-column|table-column-group|""" +
        """table-footer-group|table-header-group|table-row|""" +
        """table-row-group|text|text-bottom|text-top|thick|thin|""" +
        """transparent|ultra-condensed|ultra-expanded|underline|""" +
        """upper-alpha|upper-latin|upper-roman|uppercase|url|""" +
        """visible|w-resize|wait|wider|x-fast|x-high|x-large|x-loud|""" +
        """x-low|x-small|x-soft|xx-large|xx-small|yes)\b""", Keyword),
      ("""(indigo|gold|firebrick|indianred|yellow|darkolivegreen|""" +
        """darkseagreen|mediumvioletred|mediumorchid|chartreuse|""" +
        """mediumslateblue|black|springgreen|crimson|lightsalmon|brown|""" +
        """turquoise|olivedrab|cyan|silver|skyblue|gray|darkturquoise|""" +
        """goldenrod|darkgreen|darkviolet|darkgray|lightpink|teal|""" +
        """darkmagenta|lightgoldenrodyellow|lavender|yellowgreen|thistle|""" +
        """violet|navy|orchid|blue|ghostwhite|honeydew|cornflowerblue|""" +
        """darkblue|darkkhaki|mediumpurple|cornsilk|red|bisque|slategray|""" +
        """darkcyan|khaki|wheat|deepskyblue|darkred|steelblue|aliceblue|""" +
        """gainsboro|mediumturquoise|floralwhite|coral|purple|lightgrey|""" +
        """lightcyan|darksalmon|beige|azure|lightsteelblue|oldlace|""" +
        """greenyellow|royalblue|lightseagreen|mistyrose|sienna|""" +
        """lightcoral|orangered|navajowhite|lime|palegreen|burlywood|""" +
        """seashell|mediumspringgreen|fuchsia|papayawhip|blanchedalmond|""" +
        """peru|aquamarine|white|darkslategray|ivory|dodgerblue|""" +
        """lemonchiffon|chocolate|orange|forestgreen|slateblue|olive|""" +
        """mintcream|antiquewhite|darkorange|cadetblue|moccasin|""" +
        """limegreen|saddlebrown|darkslateblue|lightskyblue|deeppink|""" +
        """plum|aqua|darkgoldenrod|maroon|sandybrown|magenta|tan|""" +
        """rosybrown|pink|lightblue|palevioletred|mediumseagreen|""" +
        """dimgray|powderblue|seagreen|snow|mediumblue|midnightblue|""" +
        """paleturquoise|palegoldenrod|whitesmoke|darkorchid|salmon|""" +
        """lightslategray|lawngreen|lightgreen|tomato|hotpink|""" +
        """lightyellow|lavenderblush|linen|mediumaquamarine|green|""" +
        """blueviolet|peachpuff)\b""", Name.Builtin),
      ("""\!important""", Comment.Preproc),
      ("""/\*(?:.|\n)*?\*/""", Comment),
      ("""\#[a-zA-Z0-9]{1,6}""", pyments.Number),
      ("""[\.-]?[0-9]*[\.]?[0-9]+(em|px|\%|pt|pc|in|mm|cm|ex|s)\b""", pyments.Number),
      ("""-?[0-9]+""", pyments.Number),
      ("""[~\^\*!%&<>\|+=@:,./?-]+""", Operator),
      ("""[\[\]();]+""", Punctuation),
      (""""(\\\\|\\"|[^"])*"""", Str.Double),
      ("""'(\\\\|\\'|[^'])*'""", Str.Single),
      ("""[a-zA-Z][a-zA-Z0-9]+""", Name))))
}
