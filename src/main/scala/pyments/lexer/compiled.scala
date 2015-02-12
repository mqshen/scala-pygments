package pyments.lexer

import java.lang.Number

import pyments._
import pyments._
import java.util.regex.Pattern
import Helpers._

/*
 For C source code with preprocessor directives.
*/
class CLexer(val options: LexerOptions, stdLibHighlighting: Boolean = true, c99highlighting: Boolean = true) extends RegexLexer {

  override val name = "C"
  override val aliases = "c" :: Nil
  override val fileNames = "*.c" :: "*.h" :: "*.idc" :: Nil
  override val mimeTypes = "text/x-chdr" :: "text/x-csrc" :: Nil

  //: optional Comment or Whitespace
  val _ws = """(?:\s|//.*?\n|/[*].*?[*]/)+"""

  val tokens = Map[String, StateDef](
    ("whitespace", List[Definition](
      // preprocessor directives: without whitespace
      ("""^#if\s+0""", Comment.Preproc) >> "if0",
      ("""^#""", Comment.Preproc) >> "macro",
      // or with whitespace
      ("""^""" + _ws + """#if\s+0""", Comment.Preproc) >> "if0",
      ("""^""" + _ws + """#""", Comment.Preproc) >> "macro",
      ("""^(\s*)([a-zA-Z_][a-zA-Z0-9_]*:(?!:))""", ByGroups(Text, Name.Label)),
      ("""\n""", Text),
      ("""\s+""", Text),
      ("""\\\n""", Text), // line continuation
      ("""//(\n|(.|\n)*?[^\\]\n)""", Comment.Single),
      ("""/(\\\n)?[*](.|\n)*?[*](\\\n)?/""", Comment.Multiline))),
    ("statements", List[Definition](
      ("""L?"""", Str) >> "string",
      ("L?'(\\.|\\[0-7]{1,3}|\\\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'", Str.Char),
      ("""(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[LlUu]*""", pyments.Number.Float),
      ("""(\d+\.\d*|\.\d+|\d+[fF])[fF]?""", pyments.Number.Float),
      ("""0x[0-9a-fA-F]+[LlUu]*""", pyments.Number.Hex),
      ("""0[0-7]+[LlUu]*""", pyments.Number.Oct),
      ("""\d+[LlUu]*""", pyments.Number.Integer),
      ("""\*/""", Error),
      ("""[~!%^&*+=|?:<>/-]""", Operator),
      ("""[()\[\],.]""", Punctuation),
      ("""\b(case)(.+?)(:)""", ByGroups(Keyword, Using(this), Text)),
      ("""(auto|break|case|const|continue|default|do|else|enum|extern|""" +
        """for|goto|if|register|restricted|return|sizeof|static|struct|""" +
        """switch|typedef|union|volatile|virtual|while)\b""", Keyword),
      ("""(int|long|float|short|double|char|unsigned|signed|void)\b""",
        Keyword.Type),
      ("""(_{0,2}inline|naked|restrict|thread|typename)\b""", Keyword.Reserved),
      ("""__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|""" +
        """declspec|finally|int64|try|leave)\b""", Keyword.Reserved),
      ("""(true|false|NULL)\b""", Name.Builtin),
      ("""[a-zA-Z_][a-zA-Z0-9_]*""", Name))),
    ("root", List[Definition](
      Include("whitespace"),
      // functions
      ("""((?:[a-zA-Z0-9_*\s])+?(?:\s|[*]))""" + // return arguments
        """([a-zA-Z_][a-zA-Z0-9_]*)""" + // method name
        """(\s*\([^;]*?\))""" + // signature
        """(""" + _ws + """)(\{)""",
        ByGroups(Using(this), Name.Function, Using(this), Using(this), Punctuation)) >>
        "function",
      // function declarations
      ("""((?:[a-zA-Z0-9_*\s])+?(?:\s|[*]))""" + // return arguments
        """([a-zA-Z_][a-zA-Z0-9_]*)""" + // method name
        """(\s*\([^;]*?\))""" + // signature
        """(""" + _ws + """)(;)""",
        ByGroups(Using(this), Name.Function, Using(this), Using(this), Punctuation)),
      ("""""", Text) >> "statement")),
    ("statement", List[Definition](
      Include("whitespace"),
      Include("statements"),
      ("""[{}]""", Punctuation),
      (""";""", Punctuation) >> Pop)),
    ("function", List[Definition](
      Include("whitespace"),
      Include("statements"),
      (";", Punctuation),
      ("\\{", Punctuation) >> Push,
      ("\\}", Punctuation) >> Pop)),
    ("string", List[Definition](
      (""""""", Str) >> Pop,
      ("""\\([\\abfnrtv"\']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})""", Str.Escape),
      ("""[^\\"\n]+""", Str), // all other characters
      ("""\\\n""", Str), // line continuation
      ("""\\""", Str) // stray backslash
      )),
    ("macro", List[Definition](
      ("""[^/\n]+""", Comment.Preproc),
      ("""/[*](.|\n)*?[*]/""", Comment.Multiline),
      ("""//.*?\n""", Comment.Single) >> Pop,
      ("""/""", Comment.Preproc),
      ("""(?<=\\)\n""", Comment.Preproc),
      ("""\n""", Comment.Preproc) >> Pop)),
    ("if0", List[Definition](
      ("""^\s*#if.*?(?<!\\)\n""", Comment.Preproc) >> Push,
      ("""^\s*#el(?:se|if).*\n""", Comment.Preproc) >> Pop,
      ("""^\s*#endif.*?(?<!\\)\n""", Comment.Preproc) >> Pop,
      (""".*?\n""", Comment))))

  val stdlibTypes = "size_t" :: "ssize_t" :: "off_t" :: "wchar_t" :: "ptrdiff_t" ::
    "sig_atomic_t" :: "fpos_t" :: "clock_t" :: "time_t" :: "va_list" ::
    "jmp_buf" :: "FILE" :: "DIR" :: "div_t" :: "ldiv_t" :: "mbstate_t" ::
    "wctrans_t" :: "wint_t" :: "wctype_t" :: Nil
  val c99Types = "_Bool" :: "_Complex" :: "int8_t" :: "int16_t" :: "int32_t" :: "int64_t" ::
    "uint8_t" :: "uint16_t" :: "uint32_t" :: "uint64_t" :: "int_least8_t" ::
    "int_least16_t" :: "int_least32_t" :: "int_least64_t" ::
    "uint_least8_t" :: "uint_least16_t" :: "uint_least32_t" ::
    "uint_least64_t" :: "int_fast8_t" :: "int_fast16_t" :: "int_fast32_t" ::
    "int_fast64_t" :: "uint_fast8_t" :: "uint_fast16_t" :: "uint_fast32_t" ::
    "uint_fast64_t" :: "intptr_t" :: "uintptr_t" :: "intmax_t" :: "uintmax_t" :: Nil

  override def getTokensUnprocessed(text: String) = {
    super.getTokensUnprocessed(text).map {
      case (i, Name, value) if (stdLibHighlighting && stdlibTypes.contains(value)) => (i, Keyword.Type, value)
      case (i, Name, value) if (c99highlighting && c99Types.contains(value)) => (i, Keyword.Type, value)
      case t => t
    }
  }
}

class JavaLexer(val options: LexerOptions) extends RegexLexer {
  override val name = "Java"
  override val aliases = "java" :: Nil
  override val fileNames = "*.java" :: Nil
  override val mimeTypes = "text/x-java" :: Nil

  override val flags = Pattern.MULTILINE | Pattern.DOTALL

  //: optional Comment or Whitespace
  val _ws = """(?:\s|//.*?\n|/[*].*?[*]/)+"""

  // don't use raw unicode strings!
  val op = """[-~\\^\\*!%&\\\\<>\\|+=:/?@\u00a6-\u00a7\u00a9\u00ac\u00ae\u00b0-\u00b1\u00b6\u00d7\u00f7\u03f6\u0482\u0606-\u0608\u060e-\u060f\u06e9\u06fd-\u06fe\u07f6\u09fa\u0b70\u0bf3-\u0bf8\u0bfa\u0c7f\u0cf1-\u0cf2\u0d79\u0f01-\u0f03\u0f13-\u0f17\u0f1a-\u0f1f\u0f34\u0f36\u0f38\u0fbe-\u0fc5\u0fc7-\u0fcf\u109e-\u109f\u1360\u1390-\u1399\u1940\u19e0-\u19ff\u1b61-\u1b6a\u1b74-\u1b7c\u2044\u2052\u207a-\u207c\u208a-\u208c\u2100-\u2101\u2103-\u2106\u2108-\u2109\u2114\u2116-\u2118\u211e-\u2123\u2125\u2127\u2129\u212e\u213a-\u213b\u2140-\u2144\u214a-\u214d\u214f\u2190-\u2328\u232b-\u244a\u249c-\u24e9\u2500-\u2767\u2794-\u27c4\u27c7-\u27e5\u27f0-\u2982\u2999-\u29d7\u29dc-\u29fb\u29fe-\u2b54\u2ce5-\u2cea\u2e80-\u2ffb\u3004\u3012-\u3013\u3020\u3036-\u3037\u303e-\u303f\u3190-\u3191\u3196-\u319f\u31c0-\u31e3\u3200-\u321e\u322a-\u3250\u3260-\u327f\u328a-\u32b0\u32c0-\u33ff\u4dc0-\u4dff\ua490-\ua4c6\ua828-\ua82b\ufb29\ufdfd\ufe62\ufe64-\ufe66\uff0b\uff1c-\uff1e\uff5c\uff5e\uffe2\uffe4\uffe8-\uffee\ufffc-\ufffd]+"""

  val letter = """[a-zA-Z\\$_\u00aa\u00b5\u00ba\u00c0-\u00d6\u00d8-\u00f6\u00f8-\u02af\u0370-\u0373\u0376-\u0377\u037b-\u037d\u0386\u0388-\u03f5\u03f7-\u0481\u048a-\u0556\u0561-\u0587\u05d0-\u05f2\u0621-\u063f\u0641-\u064a\u066e-\u066f\u0671-\u06d3\u06d5\u06ee-\u06ef\u06fa-\u06fc\u06ff\u0710\u0712-\u072f\u074d-\u07a5\u07b1\u07ca-\u07ea\u0904-\u0939\u093d\u0950\u0958-\u0961\u0972-\u097f\u0985-\u09b9\u09bd\u09ce\u09dc-\u09e1\u09f0-\u09f1\u0a05-\u0a39\u0a59-\u0a5e\u0a72-\u0a74\u0a85-\u0ab9\u0abd\u0ad0-\u0ae1\u0b05-\u0b39\u0b3d\u0b5c-\u0b61\u0b71\u0b83-\u0bb9\u0bd0\u0c05-\u0c3d\u0c58-\u0c61\u0c85-\u0cb9\u0cbd\u0cde-\u0ce1\u0d05-\u0d3d\u0d60-\u0d61\u0d7a-\u0d7f\u0d85-\u0dc6\u0e01-\u0e30\u0e32-\u0e33\u0e40-\u0e45\u0e81-\u0eb0\u0eb2-\u0eb3\u0ebd-\u0ec4\u0edc-\u0f00\u0f40-\u0f6c\u0f88-\u0f8b\u1000-\u102a\u103f\u1050-\u1055\u105a-\u105d\u1061\u1065-\u1066\u106e-\u1070\u1075-\u1081\u108e\u10a0-\u10fa\u1100-\u135a\u1380-\u138f\u13a0-\u166c\u166f-\u1676\u1681-\u169a\u16a0-\u16ea\u16ee-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u1770\u1780-\u17b3\u17dc\u1820-\u1842\u1844-\u18a8\u18aa-\u191c\u1950-\u19a9\u19c1-\u19c7\u1a00-\u1a16\u1b05-\u1b33\u1b45-\u1b4b\u1b83-\u1ba0\u1bae-\u1baf\u1c00-\u1c23\u1c4d-\u1c4f\u1c5a-\u1c77\u1d00-\u1d2b\u1d62-\u1d77\u1d79-\u1d9a\u1e00-\u1fbc\u1fbe\u1fc2-\u1fcc\u1fd0-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ffc\u2071\u207f\u2102\u2107\u210a-\u2113\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u214e\u2160-\u2188\u2c00-\u2c7c\u2c80-\u2ce4\u2d00-\u2d65\u2d80-\u2dde\u3006-\u3007\u3021-\u3029\u3038-\u303a\u303c\u3041-\u3096\u309f\u30a1-\u30fa\u30ff-\u318e\u31a0-\u31b7\u31f0-\u31ff\u3400-\u4db5\u4e00-\ua014\ua016-\ua48c\ua500-\ua60b\ua610-\ua61f\ua62a-\ua66e\ua680-\ua697\ua722-\ua76f\ua771-\ua787\ua78b-\ua801\ua803-\ua805\ua807-\ua80a\ua80c-\ua822\ua840-\ua873\ua882-\ua8b3\ua90a-\ua925\ua930-\ua946\uaa00-\uaa28\uaa40-\uaa42\uaa44-\uaa4b\uac00-\ud7a3\uf900-\ufb1d\ufb1f-\ufb28\ufb2a-\ufd3d\ufd50-\ufdfb\ufe70-\ufefc\uff21-\uff3a\uff41-\uff5a\uff66-\uff6f\uff71-\uff9d\uffa0-\uffdc]"""

  val upper = """[A-Z\\$_\u00c0-\u00d6\u00d8-\u00de\u0100\u0102\u0104\u0106\u0108\u010a\u010c\u010e\u0110\u0112\u0114\u0116\u0118\u011a\u011c\u011e\u0120\u0122\u0124\u0126\u0128\u012a\u012c\u012e\u0130\u0132\u0134\u0136\u0139\u013b\u013d\u013f\u0141\u0143\u0145\u0147\u014a\u014c\u014e\u0150\u0152\u0154\u0156\u0158\u015a\u015c\u015e\u0160\u0162\u0164\u0166\u0168\u016a\u016c\u016e\u0170\u0172\u0174\u0176\u0178-\u0179\u017b\u017d\u0181-\u0182\u0184\u0186-\u0187\u0189-\u018b\u018e-\u0191\u0193-\u0194\u0196-\u0198\u019c-\u019d\u019f-\u01a0\u01a2\u01a4\u01a6-\u01a7\u01a9\u01ac\u01ae-\u01af\u01b1-\u01b3\u01b5\u01b7-\u01b8\u01bc\u01c4\u01c7\u01ca\u01cd\u01cf\u01d1\u01d3\u01d5\u01d7\u01d9\u01db\u01de\u01e0\u01e2\u01e4\u01e6\u01e8\u01ea\u01ec\u01ee\u01f1\u01f4\u01f6-\u01f8\u01fa\u01fc\u01fe\u0200\u0202\u0204\u0206\u0208\u020a\u020c\u020e\u0210\u0212\u0214\u0216\u0218\u021a\u021c\u021e\u0220\u0222\u0224\u0226\u0228\u022a\u022c\u022e\u0230\u0232\u023a-\u023b\u023d-\u023e\u0241\u0243-\u0246\u0248\u024a\u024c\u024e\u0370\u0372\u0376\u0386\u0388-\u038f\u0391-\u03ab\u03cf\u03d2-\u03d4\u03d8\u03da\u03dc\u03de\u03e0\u03e2\u03e4\u03e6\u03e8\u03ea\u03ec\u03ee\u03f4\u03f7\u03f9-\u03fa\u03fd-\u042f\u0460\u0462\u0464\u0466\u0468\u046a\u046c\u046e\u0470\u0472\u0474\u0476\u0478\u047a\u047c\u047e\u0480\u048a\u048c\u048e\u0490\u0492\u0494\u0496\u0498\u049a\u049c\u049e\u04a0\u04a2\u04a4\u04a6\u04a8\u04aa\u04ac\u04ae\u04b0\u04b2\u04b4\u04b6\u04b8\u04ba\u04bc\u04be\u04c0-\u04c1\u04c3\u04c5\u04c7\u04c9\u04cb\u04cd\u04d0\u04d2\u04d4\u04d6\u04d8\u04da\u04dc\u04de\u04e0\u04e2\u04e4\u04e6\u04e8\u04ea\u04ec\u04ee\u04f0\u04f2\u04f4\u04f6\u04f8\u04fa\u04fc\u04fe\u0500\u0502\u0504\u0506\u0508\u050a\u050c\u050e\u0510\u0512\u0514\u0516\u0518\u051a\u051c\u051e\u0520\u0522\u0531-\u0556\u10a0-\u10c5\u1e00\u1e02\u1e04\u1e06\u1e08\u1e0a\u1e0c\u1e0e\u1e10\u1e12\u1e14\u1e16\u1e18\u1e1a\u1e1c\u1e1e\u1e20\u1e22\u1e24\u1e26\u1e28\u1e2a\u1e2c\u1e2e\u1e30\u1e32\u1e34\u1e36\u1e38\u1e3a\u1e3c\u1e3e\u1e40\u1e42\u1e44\u1e46\u1e48\u1e4a\u1e4c\u1e4e\u1e50\u1e52\u1e54\u1e56\u1e58\u1e5a\u1e5c\u1e5e\u1e60\u1e62\u1e64\u1e66\u1e68\u1e6a\u1e6c\u1e6e\u1e70\u1e72\u1e74\u1e76\u1e78\u1e7a\u1e7c\u1e7e\u1e80\u1e82\u1e84\u1e86\u1e88\u1e8a\u1e8c\u1e8e\u1e90\u1e92\u1e94\u1e9e\u1ea0\u1ea2\u1ea4\u1ea6\u1ea8\u1eaa\u1eac\u1eae\u1eb0\u1eb2\u1eb4\u1eb6\u1eb8\u1eba\u1ebc\u1ebe\u1ec0\u1ec2\u1ec4\u1ec6\u1ec8\u1eca\u1ecc\u1ece\u1ed0\u1ed2\u1ed4\u1ed6\u1ed8\u1eda\u1edc\u1ede\u1ee0\u1ee2\u1ee4\u1ee6\u1ee8\u1eea\u1eec\u1eee\u1ef0\u1ef2\u1ef4\u1ef6\u1ef8\u1efa\u1efc\u1efe\u1f08-\u1f0f\u1f18-\u1f1d\u1f28-\u1f2f\u1f38-\u1f3f\u1f48-\u1f4d\u1f59-\u1f5f\u1f68-\u1f6f\u1fb8-\u1fbb\u1fc8-\u1fcb\u1fd8-\u1fdb\u1fe8-\u1fec\u1ff8-\u1ffb\u2102\u2107\u210b-\u210d\u2110-\u2112\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u2130-\u2133\u213e-\u213f\u2145\u2183\u2c00-\u2c2e\u2c60\u2c62-\u2c64\u2c67\u2c69\u2c6b\u2c6d-\u2c6f\u2c72\u2c75\u2c80\u2c82\u2c84\u2c86\u2c88\u2c8a\u2c8c\u2c8e\u2c90\u2c92\u2c94\u2c96\u2c98\u2c9a\u2c9c\u2c9e\u2ca0\u2ca2\u2ca4\u2ca6\u2ca8\u2caa\u2cac\u2cae\u2cb0\u2cb2\u2cb4\u2cb6\u2cb8\u2cba\u2cbc\u2cbe\u2cc0\u2cc2\u2cc4\u2cc6\u2cc8\u2cca\u2ccc\u2cce\u2cd0\u2cd2\u2cd4\u2cd6\u2cd8\u2cda\u2cdc\u2cde\u2ce0\u2ce2\ua640\ua642\ua644\ua646\ua648\ua64a\ua64c\ua64e\ua650\ua652\ua654\ua656\ua658\ua65a\ua65c\ua65e\ua662\ua664\ua666\ua668\ua66a\ua66c\ua680\ua682\ua684\ua686\ua688\ua68a\ua68c\ua68e\ua690\ua692\ua694\ua696\ua722\ua724\ua726\ua728\ua72a\ua72c\ua72e\ua732\ua734\ua736\ua738\ua73a\ua73c\ua73e\ua740\ua742\ua744\ua746\ua748\ua74a\ua74c\ua74e\ua750\ua752\ua754\ua756\ua758\ua75a\ua75c\ua75e\ua760\ua762\ua764\ua766\ua768\ua76a\ua76c\ua76e\ua779\ua77b\ua77d-\ua77e\ua780\ua782\ua784\ua786\ua78b\uff21-\uff3a]"""

  val idrest = """%s(?:%s|[0-9])*(?:(?<=_)%s)?""" format (letter, letter, op)

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      // method names
      ("""(class|trait|object)(\s+)""", ByGroups(Keyword, Text)) >> "class",
      ("""'%s""" format idrest, Text.Symbol),
      ("""[^\S\n]+""", Text),
      ("""//.*?\n""", Comment.Single),
      ("""/\*""", Comment.Multiline) >> "comment",
      ("""@%s""" format idrest, Name.Decorator),
      ("""(assert|break|case|catch|continue|default|do|else|finally|for|""" +
        """if|goto|instanceof|new|return|switch|this|throw|try|while)\b""", Keyword),
      (""":(?!%s)""" format op, Keyword) >> "type",
      ("""%s%s\b""" format (upper, idrest), Name.Class),
      ("""(true|false|null)\b""", Keyword.Constant),
      ("""(import|package)(\s+)""", ByGroups(Keyword, Text)) >> "import",
      ("""(type)(\s+)""", ByGroups(Keyword, Text)) >> "type",
      ("\"\"\".*?\"\"\"", Str),
      (""""(\\\\|\\"|[^"])*"""", Str),
      ("'.'", Str.Char),
      (idrest, Name),
      ("""[^`]+`""", Name),
      ("""\[""", Operator) >> "typeparam",
      ("""[\(\)\{\};,.#]""", Operator),
      (op, Operator),
      ("""([0-9][0-9]*\.[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?[fFdD]?""", pyments.Number.Float),
      ("""0x[0-9a-f]+""", pyments.Number.Hex),
      ("""[0-9]+L?""", pyments.Number.Integer),
      ("""\n""", Text))),
    ("class", List[Definition](
      ("""(%s|%s|`[^`]+`)(\s*)(\[)""" format (idrest, op), ByGroups(Name.Class, Text, Operator)) >>
        "typeparam",
      ("""[\s\n]+""", Text),
      ("""\{""", Operator) >> Pop,
      ("""\(""", Operator) >> Pop,
      ("""%s|%s|`[^`]+`""" format (idrest, op), Name.Class) >> Pop)),
    ("type", List[Definition](
      ("""\s+""", Text),
      ("""<[%:]|>:|[#_\u21D2]|forSome|type""", Keyword),
      ("""([,\);}]|=>|=)([\s\n]*)""", ByGroups(Operator, Text)) >> Pop,
      ("""[\(\{]""", Operator) >> Push,
      ("""((?:%s|%s|`[^`]+`)(?:\.(?:%s|%s|`[^`]+`))*)(\s*)(\[)""" format
        (idrest, op, idrest, op),
        ByGroups(Keyword.Type, Text, Operator)) >> "typeparam", //(Pop, "typeparam"),
      ("""((?:%s|%s|`[^`]+`)(?:\.(?:%s|%s|`[^`]+`))*)(\s*)$""" format
        (idrest, op, idrest, op),
        ByGroups(Keyword.Type, Text)) >> Pop,
      ("""\.|%s|%s|`[^`]+`""" format (idrest, op), Keyword.Type))),
    ("typeparam", List[Definition](
      ("""[\s\n,]+""", Text),
      ("""<[%:]|=>|>:|[#_\u21D2]|forSome|type""", Keyword),
      ("""([\]\)\}])""", Operator) >> Pop,
      ("""[\(\[\{]""", Operator) >> Push,
      ("""\.|%s|%s|`[^`]+`""" format (idrest, op), Keyword.Type))),
    ("comment", List[Definition](
      ("""[^/\*]+""", Comment.Multiline),
      ("""/\*""", Comment.Multiline) >> Push,
      ("""\*/""", Comment.Multiline) >> Pop,
      ("""[\*/]""", Comment.Multiline))),
    ("import", List[Definition](
      ("""(%s|\.)+""" format idrest, Name.Namespace) >> Pop)))

}

/*
For `Scala <http://www.scala-lang.org>`_ source code.
*/
class ScalaLexer(val options: LexerOptions) extends RegexLexer {

  override val name = "Scala"
  override val aliases = "scala" :: Nil
  override val fileNames = "*.scala" :: Nil
  override val mimeTypes = "text/x-scala" :: Nil

  override val flags = Pattern.MULTILINE | Pattern.DOTALL

  //: optional Comment or Whitespace
  val _ws = """(?:\s|//.*?\n|/[*].*?[*]/)+"""

  // don't use raw unicode strings!
  val op = """[-~\\^\\*!%&\\\\<>\\|+=:/?@\u00a6-\u00a7\u00a9\u00ac\u00ae\u00b0-\u00b1\u00b6\u00d7\u00f7\u03f6\u0482\u0606-\u0608\u060e-\u060f\u06e9\u06fd-\u06fe\u07f6\u09fa\u0b70\u0bf3-\u0bf8\u0bfa\u0c7f\u0cf1-\u0cf2\u0d79\u0f01-\u0f03\u0f13-\u0f17\u0f1a-\u0f1f\u0f34\u0f36\u0f38\u0fbe-\u0fc5\u0fc7-\u0fcf\u109e-\u109f\u1360\u1390-\u1399\u1940\u19e0-\u19ff\u1b61-\u1b6a\u1b74-\u1b7c\u2044\u2052\u207a-\u207c\u208a-\u208c\u2100-\u2101\u2103-\u2106\u2108-\u2109\u2114\u2116-\u2118\u211e-\u2123\u2125\u2127\u2129\u212e\u213a-\u213b\u2140-\u2144\u214a-\u214d\u214f\u2190-\u2328\u232b-\u244a\u249c-\u24e9\u2500-\u2767\u2794-\u27c4\u27c7-\u27e5\u27f0-\u2982\u2999-\u29d7\u29dc-\u29fb\u29fe-\u2b54\u2ce5-\u2cea\u2e80-\u2ffb\u3004\u3012-\u3013\u3020\u3036-\u3037\u303e-\u303f\u3190-\u3191\u3196-\u319f\u31c0-\u31e3\u3200-\u321e\u322a-\u3250\u3260-\u327f\u328a-\u32b0\u32c0-\u33ff\u4dc0-\u4dff\ua490-\ua4c6\ua828-\ua82b\ufb29\ufdfd\ufe62\ufe64-\ufe66\uff0b\uff1c-\uff1e\uff5c\uff5e\uffe2\uffe4\uffe8-\uffee\ufffc-\ufffd]+"""

  val letter = """[a-zA-Z\\$_\u00aa\u00b5\u00ba\u00c0-\u00d6\u00d8-\u00f6\u00f8-\u02af\u0370-\u0373\u0376-\u0377\u037b-\u037d\u0386\u0388-\u03f5\u03f7-\u0481\u048a-\u0556\u0561-\u0587\u05d0-\u05f2\u0621-\u063f\u0641-\u064a\u066e-\u066f\u0671-\u06d3\u06d5\u06ee-\u06ef\u06fa-\u06fc\u06ff\u0710\u0712-\u072f\u074d-\u07a5\u07b1\u07ca-\u07ea\u0904-\u0939\u093d\u0950\u0958-\u0961\u0972-\u097f\u0985-\u09b9\u09bd\u09ce\u09dc-\u09e1\u09f0-\u09f1\u0a05-\u0a39\u0a59-\u0a5e\u0a72-\u0a74\u0a85-\u0ab9\u0abd\u0ad0-\u0ae1\u0b05-\u0b39\u0b3d\u0b5c-\u0b61\u0b71\u0b83-\u0bb9\u0bd0\u0c05-\u0c3d\u0c58-\u0c61\u0c85-\u0cb9\u0cbd\u0cde-\u0ce1\u0d05-\u0d3d\u0d60-\u0d61\u0d7a-\u0d7f\u0d85-\u0dc6\u0e01-\u0e30\u0e32-\u0e33\u0e40-\u0e45\u0e81-\u0eb0\u0eb2-\u0eb3\u0ebd-\u0ec4\u0edc-\u0f00\u0f40-\u0f6c\u0f88-\u0f8b\u1000-\u102a\u103f\u1050-\u1055\u105a-\u105d\u1061\u1065-\u1066\u106e-\u1070\u1075-\u1081\u108e\u10a0-\u10fa\u1100-\u135a\u1380-\u138f\u13a0-\u166c\u166f-\u1676\u1681-\u169a\u16a0-\u16ea\u16ee-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u1770\u1780-\u17b3\u17dc\u1820-\u1842\u1844-\u18a8\u18aa-\u191c\u1950-\u19a9\u19c1-\u19c7\u1a00-\u1a16\u1b05-\u1b33\u1b45-\u1b4b\u1b83-\u1ba0\u1bae-\u1baf\u1c00-\u1c23\u1c4d-\u1c4f\u1c5a-\u1c77\u1d00-\u1d2b\u1d62-\u1d77\u1d79-\u1d9a\u1e00-\u1fbc\u1fbe\u1fc2-\u1fcc\u1fd0-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ffc\u2071\u207f\u2102\u2107\u210a-\u2113\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u214e\u2160-\u2188\u2c00-\u2c7c\u2c80-\u2ce4\u2d00-\u2d65\u2d80-\u2dde\u3006-\u3007\u3021-\u3029\u3038-\u303a\u303c\u3041-\u3096\u309f\u30a1-\u30fa\u30ff-\u318e\u31a0-\u31b7\u31f0-\u31ff\u3400-\u4db5\u4e00-\ua014\ua016-\ua48c\ua500-\ua60b\ua610-\ua61f\ua62a-\ua66e\ua680-\ua697\ua722-\ua76f\ua771-\ua787\ua78b-\ua801\ua803-\ua805\ua807-\ua80a\ua80c-\ua822\ua840-\ua873\ua882-\ua8b3\ua90a-\ua925\ua930-\ua946\uaa00-\uaa28\uaa40-\uaa42\uaa44-\uaa4b\uac00-\ud7a3\uf900-\ufb1d\ufb1f-\ufb28\ufb2a-\ufd3d\ufd50-\ufdfb\ufe70-\ufefc\uff21-\uff3a\uff41-\uff5a\uff66-\uff6f\uff71-\uff9d\uffa0-\uffdc]"""

  val upper = """[A-Z\\$_\u00c0-\u00d6\u00d8-\u00de\u0100\u0102\u0104\u0106\u0108\u010a\u010c\u010e\u0110\u0112\u0114\u0116\u0118\u011a\u011c\u011e\u0120\u0122\u0124\u0126\u0128\u012a\u012c\u012e\u0130\u0132\u0134\u0136\u0139\u013b\u013d\u013f\u0141\u0143\u0145\u0147\u014a\u014c\u014e\u0150\u0152\u0154\u0156\u0158\u015a\u015c\u015e\u0160\u0162\u0164\u0166\u0168\u016a\u016c\u016e\u0170\u0172\u0174\u0176\u0178-\u0179\u017b\u017d\u0181-\u0182\u0184\u0186-\u0187\u0189-\u018b\u018e-\u0191\u0193-\u0194\u0196-\u0198\u019c-\u019d\u019f-\u01a0\u01a2\u01a4\u01a6-\u01a7\u01a9\u01ac\u01ae-\u01af\u01b1-\u01b3\u01b5\u01b7-\u01b8\u01bc\u01c4\u01c7\u01ca\u01cd\u01cf\u01d1\u01d3\u01d5\u01d7\u01d9\u01db\u01de\u01e0\u01e2\u01e4\u01e6\u01e8\u01ea\u01ec\u01ee\u01f1\u01f4\u01f6-\u01f8\u01fa\u01fc\u01fe\u0200\u0202\u0204\u0206\u0208\u020a\u020c\u020e\u0210\u0212\u0214\u0216\u0218\u021a\u021c\u021e\u0220\u0222\u0224\u0226\u0228\u022a\u022c\u022e\u0230\u0232\u023a-\u023b\u023d-\u023e\u0241\u0243-\u0246\u0248\u024a\u024c\u024e\u0370\u0372\u0376\u0386\u0388-\u038f\u0391-\u03ab\u03cf\u03d2-\u03d4\u03d8\u03da\u03dc\u03de\u03e0\u03e2\u03e4\u03e6\u03e8\u03ea\u03ec\u03ee\u03f4\u03f7\u03f9-\u03fa\u03fd-\u042f\u0460\u0462\u0464\u0466\u0468\u046a\u046c\u046e\u0470\u0472\u0474\u0476\u0478\u047a\u047c\u047e\u0480\u048a\u048c\u048e\u0490\u0492\u0494\u0496\u0498\u049a\u049c\u049e\u04a0\u04a2\u04a4\u04a6\u04a8\u04aa\u04ac\u04ae\u04b0\u04b2\u04b4\u04b6\u04b8\u04ba\u04bc\u04be\u04c0-\u04c1\u04c3\u04c5\u04c7\u04c9\u04cb\u04cd\u04d0\u04d2\u04d4\u04d6\u04d8\u04da\u04dc\u04de\u04e0\u04e2\u04e4\u04e6\u04e8\u04ea\u04ec\u04ee\u04f0\u04f2\u04f4\u04f6\u04f8\u04fa\u04fc\u04fe\u0500\u0502\u0504\u0506\u0508\u050a\u050c\u050e\u0510\u0512\u0514\u0516\u0518\u051a\u051c\u051e\u0520\u0522\u0531-\u0556\u10a0-\u10c5\u1e00\u1e02\u1e04\u1e06\u1e08\u1e0a\u1e0c\u1e0e\u1e10\u1e12\u1e14\u1e16\u1e18\u1e1a\u1e1c\u1e1e\u1e20\u1e22\u1e24\u1e26\u1e28\u1e2a\u1e2c\u1e2e\u1e30\u1e32\u1e34\u1e36\u1e38\u1e3a\u1e3c\u1e3e\u1e40\u1e42\u1e44\u1e46\u1e48\u1e4a\u1e4c\u1e4e\u1e50\u1e52\u1e54\u1e56\u1e58\u1e5a\u1e5c\u1e5e\u1e60\u1e62\u1e64\u1e66\u1e68\u1e6a\u1e6c\u1e6e\u1e70\u1e72\u1e74\u1e76\u1e78\u1e7a\u1e7c\u1e7e\u1e80\u1e82\u1e84\u1e86\u1e88\u1e8a\u1e8c\u1e8e\u1e90\u1e92\u1e94\u1e9e\u1ea0\u1ea2\u1ea4\u1ea6\u1ea8\u1eaa\u1eac\u1eae\u1eb0\u1eb2\u1eb4\u1eb6\u1eb8\u1eba\u1ebc\u1ebe\u1ec0\u1ec2\u1ec4\u1ec6\u1ec8\u1eca\u1ecc\u1ece\u1ed0\u1ed2\u1ed4\u1ed6\u1ed8\u1eda\u1edc\u1ede\u1ee0\u1ee2\u1ee4\u1ee6\u1ee8\u1eea\u1eec\u1eee\u1ef0\u1ef2\u1ef4\u1ef6\u1ef8\u1efa\u1efc\u1efe\u1f08-\u1f0f\u1f18-\u1f1d\u1f28-\u1f2f\u1f38-\u1f3f\u1f48-\u1f4d\u1f59-\u1f5f\u1f68-\u1f6f\u1fb8-\u1fbb\u1fc8-\u1fcb\u1fd8-\u1fdb\u1fe8-\u1fec\u1ff8-\u1ffb\u2102\u2107\u210b-\u210d\u2110-\u2112\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u2130-\u2133\u213e-\u213f\u2145\u2183\u2c00-\u2c2e\u2c60\u2c62-\u2c64\u2c67\u2c69\u2c6b\u2c6d-\u2c6f\u2c72\u2c75\u2c80\u2c82\u2c84\u2c86\u2c88\u2c8a\u2c8c\u2c8e\u2c90\u2c92\u2c94\u2c96\u2c98\u2c9a\u2c9c\u2c9e\u2ca0\u2ca2\u2ca4\u2ca6\u2ca8\u2caa\u2cac\u2cae\u2cb0\u2cb2\u2cb4\u2cb6\u2cb8\u2cba\u2cbc\u2cbe\u2cc0\u2cc2\u2cc4\u2cc6\u2cc8\u2cca\u2ccc\u2cce\u2cd0\u2cd2\u2cd4\u2cd6\u2cd8\u2cda\u2cdc\u2cde\u2ce0\u2ce2\ua640\ua642\ua644\ua646\ua648\ua64a\ua64c\ua64e\ua650\ua652\ua654\ua656\ua658\ua65a\ua65c\ua65e\ua662\ua664\ua666\ua668\ua66a\ua66c\ua680\ua682\ua684\ua686\ua688\ua68a\ua68c\ua68e\ua690\ua692\ua694\ua696\ua722\ua724\ua726\ua728\ua72a\ua72c\ua72e\ua732\ua734\ua736\ua738\ua73a\ua73c\ua73e\ua740\ua742\ua744\ua746\ua748\ua74a\ua74c\ua74e\ua750\ua752\ua754\ua756\ua758\ua75a\ua75c\ua75e\ua760\ua762\ua764\ua766\ua768\ua76a\ua76c\ua76e\ua779\ua77b\ua77d-\ua77e\ua780\ua782\ua784\ua786\ua78b\uff21-\uff3a]"""

  val idrest = """%s(?:%s|[0-9])*(?:(?<=_)%s)?""" format (letter, letter, op)

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      // method names
      ("""(class|trait|object)(\s+)""", ByGroups(Keyword, Text)) >> "class",
      ("""'%s""" format idrest, Text.Symbol),
      ("""[^\S\n]+""", Text),
      ("""//.*?\n""", Comment.Single),
      ("""/\*""", Comment.Multiline) >> "comment",
      ("""@%s""" format idrest, Name.Decorator),
      ("""(abstract|ca(?:se|tch)|d(?:ef|o)|e(?:lse|xtends)|""" +
        """f(?:inal(?:ly)?|or(?:Some)?)|i(?:f|mplicit)|""" +
        """lazy|match|new|override|pr(?:ivate|otected)""" +
        """|re(?:quires|turn)|s(?:ealed|uper)|""" +
        """t(?:h(?:is|row)|ry)|va[lr]|w(?:hile|ith)|yield)\b|""" +
        """(<[%:-]|=>|>:|[#=@_\u21D2\u2190])(\b|(?=\\s)|$)""", Keyword),
      (""":(?!%s)""" format op, Keyword) >> "type",
      ("""%s%s\b""" format (upper, idrest), Name.Class),
      ("""(true|false|null)\b""", Keyword.Constant),
      ("""(import|package)(\s+)""", ByGroups(Keyword, Text)) >> "import",
      ("""(type)(\s+)""", ByGroups(Keyword, Text)) >> "type",
      ("\"\"\".*?\"\"\"", Str),
      (""""(\\\\|\\"|[^"])*"""", Str),
      ("'.'", Str.Char),
      (idrest, Name),
      ("""[^`]+`""", Name),
      ("""\[""", Operator) >> "typeparam",
      ("""[\(\)\{\};,.#]""", Operator),
      (op, Operator),
      ("""([0-9][0-9]*\.[0-9]*|\.[0-9]+)([eE][+-]?[0-9]+)?[fFdD]?""", pyments.Number.Float),
      ("""0x[0-9a-f]+""", pyments.Number.Hex),
      ("""[0-9]+L?""", pyments.Number.Integer),
      ("""\n""", Text))),
    ("class", List[Definition](
      ("""(%s|%s|`[^`]+`)(\s*)(\[)""" format (idrest, op), ByGroups(Name.Class, Text, Operator)) >>
        "typeparam",
      ("""[\s\n]+""", Text),
      ("""\{""", Operator) >> Pop,
      ("""\(""", Operator) >> Pop,
      ("""%s|%s|`[^`]+`""" format (idrest, op), Name.Class) >> Pop)),
    ("type", List[Definition](
      ("""\s+""", Text),
      ("""<[%:]|>:|[#_\u21D2]|forSome|type""", Keyword),
      ("""([,\);}]|=>|=)([\s\n]*)""", ByGroups(Operator, Text)) >> Pop,
      ("""[\(\{]""", Operator) >> Push,
      ("""((?:%s|%s|`[^`]+`)(?:\.(?:%s|%s|`[^`]+`))*)(\s*)(\[)""" format
        (idrest, op, idrest, op),
        ByGroups(Keyword.Type, Text, Operator)) >> "typeparam", //(Pop, "typeparam"),
      ("""((?:%s|%s|`[^`]+`)(?:\.(?:%s|%s|`[^`]+`))*)(\s*)$""" format
        (idrest, op, idrest, op),
        ByGroups(Keyword.Type, Text)) >> Pop,
      ("""\.|%s|%s|`[^`]+`""" format (idrest, op), Keyword.Type))),
    ("typeparam", List[Definition](
      ("""[\s\n,]+""", Text),
      ("""<[%:]|=>|>:|[#_\u21D2]|forSome|type""", Keyword),
      ("""([\]\)\}])""", Operator) >> Pop,
      ("""[\(\[\{]""", Operator) >> Push,
      ("""\.|%s|%s|`[^`]+`""" format (idrest, op), Keyword.Type))),
    ("comment", List[Definition](
      ("""[^/\*]+""", Comment.Multiline),
      ("""/\*""", Comment.Multiline) >> Push,
      ("""\*/""", Comment.Multiline) >> Pop,
      ("""[\*/]""", Comment.Multiline))),
    ("import", List[Definition](
      ("""(%s|\.)+""" format idrest, Name.Namespace) >> Pop)))
}

/*
    For Ada source code.

    *New in Pygments 1.3.*
*/
class AdaLexer(val options: LexerOptions) extends RegexLexer {

  override val name = "Ada"
  override val aliases = "ada" :: "ada95" :: "ada2005" :: Nil
  override val fileNames = "*.adb" :: "*.ads" :: "*.ada" :: Nil
  override val mimeTypes = "text/x-ada" :: Nil

  override val flags = Pattern.MULTILINE | Pattern.CASE_INSENSITIVE

  val _ws = """(?:\s|//.*?\n|/[*].*?[*]/)+"""

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""[^\S\n]+""", Text),
      ("""--.*?\n""", Comment.Single),
      ("""[^\S\n]+""", Text),
      ("""function|procedure|entry""", Keyword.Declaration) >> "subprogram",
      ("""(subtype|type)(\s+)([a-z0-9_]+)""",
        ByGroups(Keyword.Declaration, Text, Keyword.Type)) >> "type_def",
      ("""task|protected""", Keyword.Declaration),
      ("""(subtype)(\s+)""", ByGroups(Keyword.Declaration, Text)),
      ("""(end)(\s+)""", ByGroups(Keyword.Reserved, Text)) >> "end",
      ("""(pragma)(\s+)([a-zA-Z0-9_]+)""", ByGroups(Keyword.Reserved, Text,
        Comment.Preproc)),
      ("""(true|false|null)\b""", Keyword.Constant),
      ("""(Byte|Character|Float|Integer|Long_Float|Long_Integer|""" +
        """Long_Long_Float|Long_Long_Integer|Natural|Positive|Short_Float|""" +
        """Short_Integer|Short_Short_Float|Short_Short_Integer|String|""" +
        """Wide_String|Duration)\b""", Keyword.Type),
      ("""(and(\s+then)?|in|mod|not|or(\s+else)|rem)\b""", Operator.Word),
      ("""generic|private""", Keyword.Declaration),
      ("""package""", Keyword.Declaration) >> "package",
      ("""array\b""", Keyword.Reserved) >> "array_def",
      ("""(with|use)(\s+)""", ByGroups(Keyword.Namespace, Text)) >> "import",
      ("""([a-z0-9_]+)(\s*)(:)(\s*)(constant)""",
        ByGroups(Name.Constant, Text, Punctuation, Text, Keyword.Reserved)),
      ("""<<[a-z0-9_]+>>""", Name.Label),
      ("""([a-z0-9_]+)(\s*)(:)(\s*)(declare|begin|loop|for|while)""",
        ByGroups(Name.Label, Text, Punctuation, Text, Keyword.Reserved)),
      ("""\b(abort|abs|abstract|accept|access|aliased|all|array|at|begin|""" +
        """body|case|constant|declare|delay|delta|digits|do|else|elsif|end|""" +
        """entry|exception|exit|interface|for|goto|if|is|limited|loop|new|""" +
        """null|of|or|others|out|overriding|pragma|protected|raise|range|""" +
        """record|renames|requeue|return|reverse|select|separate|subtype|""" +
        """synchronized|task|tagged|terminate|then|type|until|when|while|""" +
        """xor)\b""",
        Keyword.Reserved),
      (""""[^"]*"""", Str),
      Include("attribute"),
      Include("numbers"),
      ("""'[^']'""", Str.Char),
      ("""([a-z0-9_]+)(\s*|[(,])""", ByGroups(Name, Using(this))),
      ("""(<>|=>|:=|[\(\)\|:;,.'])""", Punctuation),
      ("""[*<>+=/&-]""", Operator),
      ("""\n+""", Text))),
    ("numbers", List[Definition](
      ("""[0-9_]+#[0-9a-f]+#""", pyments.Number.Hex),
      ("""[0-9_]+\.[0-9_]*""", pyments.Number.Float),
      ("""[0-9_]+""", pyments.Number.Integer))),
    ("attribute", List[Definition](
      ("""(')([a-zA-Z0-9_]+)""", ByGroups(Punctuation, Name.Attribute)))),
    ("subprogram", List[Definition](
      ("""\(""", Punctuation) >> (Pop, "formal_part"),
      (""";""", Punctuation) >> Pop,
      ("""is\b""", Keyword.Reserved) >> Pop,
      (""""[^"]+"|[a-z0-9_]+""", Name.Function),
      Include("root"))),
    ("end", List[Definition](
      ("""(if|case|record|loop|select)""", Keyword.Reserved),
      (""""[^"]+"|[a-zA-Z0-9_]+""", Name.Function),
      ("""[\n\s]+""", Text),
      (""";""", Punctuation) >> Pop)),
    ("type_def", List[Definition](
      (""";""", Punctuation) >> Pop,
      ("""\(""", Punctuation) >> "formal_part",
      ("""with|and|use""", Keyword.Reserved),
      ("""array\b""", Keyword.Reserved) >> (Pop, "array_def"),
      ("""record\b""", Keyword.Reserved) >> "formal_part",
      Include("root"))),
    ("array_def", List[Definition](
      (""";""", Punctuation) >> Pop,
      ("""([a-z0-9_]+)(\s+)(range)""", ByGroups(Keyword.Type, Text, Keyword.Reserved)),
      Include("root"))),
    ("import", List[Definition](
      ("""[a-z0-9_.]+""", Name.Namespace) >> Pop)),
    ("formal_part", List[Definition](
      ("""\)""", Punctuation) >> Pop,
      ("""([a-z0-9_]+)(\s*)(,|:[^=])""", ByGroups(Name.Variable, Text, Punctuation)),
      ("""(in|not|null|out|access)\b""", Keyword.Reserved),
      Include("root"))),
    ("package", List[Definition](
      ("""body""", Keyword.Declaration),
      ("""is\s+new|renames""", Keyword.Reserved),
      ("""is""", Keyword.Reserved) >> Pop,
      (""";""", Punctuation) >> Pop,
      ("""\(""", Punctuation) >> "package_instantiation",
      ("""([a-zA-Z0-9_.]+)""", Name.Class),
      Include("root"))),
    ("package_instantiation", List[Definition](
      ("""("[^"]+"|[a-z0-9_]+)(\s+)(=>)""", ByGroups(Name.Variable, Text, Punctuation)),
      ("""[a-z0-9._\'"]""", Text),
      ("""\)""", Punctuation) >> Pop,
      Include("root"))))
}

/*
    For `BlitzMax <http://blitzbasic.com>`_ source code.
*/
class BlitzMaxLexer(val options: LexerOptions) extends RegexLexer {

  override val name = "BlitzMax"
  override val aliases = "blitzmax" :: "bmax" :: Nil
  override val fileNames = "*.bmx" :: Nil
  override val mimeTypes = "text/x-bmx" :: Nil

  override val flags = Pattern.MULTILINE | Pattern.CASE_INSENSITIVE

  val bmax_vopwords = """\b(Shl|Shr|Sar|Mod)\b"""
  val bmax_sktypes = """@{1,2}|[!#$%]"""
  val bmax_lktypes = """\b(Int|Byte|Short|Float|Double|Long)\b"""
  val bmax_name = """[a-z_][a-z0-9_]*"""
  val bmax_var = """(%s)(?:(?:([ \t]*)(%s)|([ \t]*:[ \t]*\b(?:Shl|Shr|Sar|Mod)\b)|([ \t]*)([:])([ \t]*)(?:%s|(%s)))(?:([ \t]*)(Ptr))?)""" format (bmax_name, bmax_sktypes, bmax_lktypes, bmax_name)
  val bmax_func = bmax_var + """?((?:[ \t]|\.\.\n)*)([(])"""

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      // Text
      ("""[ \t]+""", Text),
      ("""\.\.\n""", Text), // Line continuation
      // Comments
      ("'.*?\n", Comment.Single),
      ("""([ \t]*)\bRem\n(\n|.)*?\s*\bEnd([ \t]*)Rem""", Comment.Multiline),
      // Data types
      (""""""", Str.Double) >> "string",
      // Numbers
      ("""[0-9]+\.[0-9]*(?!\.)""", pyments.Number.Float),
      ("""\.[0-9]*(?!\.)""", pyments.Number.Float),
      ("""[0-9]+""", pyments.Number.Integer),
      ("""\$[0-9a-f]+""", pyments.Number.Hex),
      ("""\%[10]+""", pyments.Number), // Binary
      // Other
      ("""(?:(?:(:)?([ \t]*)(:?%s|([+\-*/&|~]))|Or|And|Not|[=<>^]))""" format (bmax_vopwords), Operator),
      ("""[(),.:\[\]]""", Punctuation),
      ("""(?:#[\w \t]*)""", Name.Label),
      ("""(?:\?[\w \t]*)""", Comment.Preproc),
      // Identifiers
      ("""\b(New)\b([ \t]?)([(]?)(%s)""" format (bmax_name),
        ByGroups(Keyword.Reserved, Text, Punctuation, Name.Class)),
      ("""\b(Import|Framework|Module)([ \t]+)(%s\.%s)""" format
        (bmax_name, bmax_name),
        ByGroups(Keyword.Reserved, Text, Keyword.Namespace)),
      (bmax_func, ByGroups(Name.Function, Text, Keyword.Type,
        Operator, Text, Punctuation, Text,
        Keyword.Type, Name.Class, Text,
        Keyword.Type, Text, Punctuation)),
      (bmax_var, ByGroups(Name.Variable, Text, Keyword.Type, Operator,
        Text, Punctuation, Text, Keyword.Type,
        Name.Class, Text, Keyword.Type)),
      ("""\b(Type|Extends)([ \t]+)(%s)""" format (bmax_name),
        ByGroups(Keyword.Reserved, Text, Name.Class)),
      // Keywords
      ("""\b(Ptr)\b""", Keyword.Type),
      ("""\b(Pi|True|False|Null|Self|Super)\b""", Keyword.Constant),
      ("""\b(Local|Global|Const|Field)\b""", Keyword.Declaration),
      ("""\b(TNullMethodException|TNullFunctionException|""" +
        """TNullObjectException|TArrayBoundsException|""" +
        """TRuntimeException)\b""", Name.Exception),
      ("""\b(Strict|SuperStrict|Module|ModuleInfo|""" +
        """End|Return|Continue|Exit|Public|Private|""" +
        """Var|VarPtr|Chr|Len|Asc|SizeOf|Sgn|Abs|Min|Max|""" +
        """New|Release|Delete|""" +
        """Incbin|IncbinPtr|IncbinLen|""" +
        """Framework|Include|Import|Extern|EndExtern|""" +
        """Function|EndFunction|""" +
        """Type|EndType|Extends|""" +
        """Method|EndMethod|""" +
        """Abstract|Final|""" +
        """If|Then|Else|ElseIf|EndIf|""" +
        """For|To|Next|Step|EachIn|""" +
        """While|Wend|EndWhile|""" +
        """Repeat|Until|Forever|""" +
        """Select|Case|Default|EndSelect|""" +
        """Try|Catch|EndTry|Throw|Assert|""" +
        """Goto|DefData|ReadData|RestoreData)\b""", Keyword.Reserved),
      // Final resolve (for variable names and such)
      ("""(%s)""" format (bmax_name), Name.Variable))),
    ("string", List[Definition](
      ("""""""", Str.Double),
      (""""C?""", Str.Double) >> Pop,
      ("""[^"]+""", Str.Double))))
}

/*
For C++ source code with preprocessor directives.
*/
class CppLexer(val options: LexerOptions) extends RegexLexer {

  override val name = "C++"
  override val aliases = "cpp" :: "c++" :: Nil
  override val fileNames = "*.cpp" :: "*.hpp" :: "*.c++" :: "*.h++" :: "*.cc" :: "*.hh" :: "*.cxx" :: "*.hxx" :: Nil
  override val mimeTypes = "text/x-c++hdr" :: "text/x-c++src" :: Nil

  //: optional Comment or Whitespace
  private val _ws = """(?:\s|//.*?\n|/[*].*?[*]/)+"""

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      // preprocessor directives: without whitespace
      ("""^#if\s+0""", Comment.Preproc) >> "if0",
      ("""^#""", Comment.Preproc) >> "macro",
      // or with whitespace
      ("""^""" + _ws + """#if\s+0""", Comment.Preproc) >> "if0",
      ("""^""" + _ws + """#""", Comment.Preproc) >> "macro",
      ("""\n""", Text),
      ("""\s+""", Text),
      ("""\\\n""", Text), // line continuation
      ("""/(\\\n)?/(\n|(.|\n)*?[^\\]\n)""", Comment.Single),
      ("""/(\\\n)?[*](.|\n)*?[*](\\\n)?/""", Comment.Multiline),
      ("""[{}]""", Punctuation),
      ("""L?"""", Str) >> "string",
      ("""L?'(\\.|\\[0-7]{1,3}|\\x[a-fA-F0-9]{1,2}|[^\\\'\n])'""", Str.Char),
      ("""(\d+\.\d*|\.\d+|\d+)[eE][+-]?\d+[LlUu]*""", pyments.Number.Float),
      ("""(\d+\.\d*|\.\d+|\d+[fF])[fF]?""", pyments.Number.Float),
      ("""0x[0-9a-fA-F]+[LlUu]*""", pyments.Number.Hex),
      ("""0[0-7]+[LlUu]*""", pyments.Number.Oct),
      ("""\d+[LlUu]*""", pyments.Number.Integer),
      ("""\*/""", Error),
      ("""[~!%^&*+=|?:<>/-]""", Operator),
      ("""[()\[\],.;]""", Punctuation),
      ("""(asm|auto|break|case|catch|const|const_cast|continue|""" +
        """default|delete|do|dynamic_cast|else|enum|explicit|export|""" +
        """extern|for|friend|goto|if|mutable|namespace|new|operator|""" +
        """private|protected|public|register|reinterpret_cast|return|""" +
        """restrict|sizeof|static|static_cast|struct|switch|template|""" +
        """this|throw|throws|try|typedef|typeid|typename|union|using|""" +
        """volatile|virtual|while)\b""", Keyword),
      ("""(class)(\s+)""", ByGroups(Keyword, Text)) >> "classname",
      ("""(bool|int|long|float|short|double|char|unsigned|signed|""" +
        """void|wchar_t)\b""", Keyword.Type),
      ("""(_{0,2}inline|naked|thread)\b""", Keyword.Reserved),
      ("""__(asm|int8|based|except|int16|stdcall|cdecl|fastcall|int32|""" +
        """declspec|finally|int64|try|leave|wchar_t|w64|virtual_inheritance|""" +
        """uuidof|unaligned|super|single_inheritance|raise|noop|""" +
        """multiple_inheritance|m128i|m128d|m128|m64|interface|""" +
        """identifier|forceinline|event|assume)\b""", Keyword.Reserved),
      // Offload C++ extensions, http://offload.codeplay.com/
      ("""(__offload|__blockingoffload|__outer)\b""", Keyword.Pseudo),
      ("""(true|false)\b""", Keyword.Constant),
      ("""NULL\b""", Name.Builtin),
      ("""[a-zA-Z_][a-zA-Z0-9_]*:(?!:)""", Name.Label),
      ("""[a-zA-Z_][a-zA-Z0-9_]*""", Name))),
    ("classname", List[Definition](
      ("""[a-zA-Z_][a-zA-Z0-9_]*""", Name.Class) >> Pop,
      // template specification
      ("""\s*(?=>)""", Text) >> Pop)),
    ("string", List[Definition](
      (""""""", Str) >> Pop,
      ("""\\([\\abfnrtv"']|x[a-fA-F0-9]{2,4}|[0-7]{1,3})""", Str.Escape),
      ("""[^\\"\n]+""", Str), // all other characters
      ("""\\\n""", Str), // line continuation
      ("""\\""", Str) // stray backslash
      )),
    ("macro", List[Definition](
      ("""[^/\n]+""", Comment.Preproc),
      ("""/[*](.|\n)*?[*]/""", Comment.Multiline),
      ("""//.*?\n""", Comment.Single) >> Pop,
      ("""/""", Comment.Preproc),
      ("""(?<=\\)\n""", Comment.Preproc),
      ("""\n""", Comment.Preproc) >> Pop)),
    ("if0", List[Definition](
      ("""^\s*#if.*?(?<!\\)\n""", Comment.Preproc) >> Push,
      ("""^\s*#endif.*?(?<!\\)\n""", Comment.Preproc) >> Pop,
      (""".*?\n""", Comment))))
}

class CythonLexer(val options: LexerOptions) extends RegexLexer {

  override val name = "Cython"
  override val aliases = "cython" :: "pyx" :: Nil
  override val fileNames = "*.pyx" :: "*.pxd" :: "*.pxi" :: Nil
  override val mimeTypes = "text/x-cython" :: "application/x-cython" :: Nil

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("""\n""", Text),
      ("^(\\s*)(\"\"\"(?:.|\n)*?\"\"\")", ByGroups(Text, Str.Doc)),
      ("""^(\s*)('''(?:.|\n)*?''')""", ByGroups(Text, Str.Doc)),
      ("""[^\S\n]+""", Text),
      ("""#.*$""", Comment),
      ("""[\]\{\}:\(\),;\[]""", Punctuation),
      ("""\\\n""", Text),
      ("""\\""", Text),
      ("""(in|is|and|or|not)\b""", Operator.Word),
      ("""(<)([a-zA-Z0-9.?]+)(>)""",
        ByGroups(Punctuation, Keyword.Type, Punctuation)),
      ("""!=|==|<<|>>|[-~+/*%=<>&^|.?]""", Operator),
      ("""(from)(\d+)(<=)(\s+)(<)(\d+)(:)""",
        ByGroups(Keyword, pyments.Number.Integer, Operator, Name, Operator,
          Name, Punctuation)),
      Include("keywords"),
      ("""(def|property)(\s+)""", ByGroups(Keyword, Text)) >> "funcname",
      ("""(cp?def)(\s+)""", ByGroups(Keyword, Text)) >> "cdef",
      ("""(class|struct)(\s+)""", ByGroups(Keyword, Text)) >> "classname",
      ("""(from)(\s+)""", ByGroups(Keyword, Text)) >> "fromimport",
      ("""(c?import)(\s+)""", ByGroups(Keyword, Text)) >> "import",
      Include("builtins"),
      Include("backtick"),
      ("""(?:[rR]|[uU][rR]|[rR][uU])"""""", Str) >> "tdqs",
      ("""(?:[rR]|[uU][rR]|[rR][uU])'''""", Str) >> "tsqs",
      ("""(?:[rR]|[uU][rR]|[rR][uU])"""", Str) >> "dqs",
      ("""(?:[rR]|[uU][rR]|[rR][uU])'""", Str) >> "sqs",
      ("""[uU]?"""""", Str) >> Combined("stringescape", "tdqs"),
      ("""[uU]?'''""", Str) >> Combined("stringescape", "tsqs"),
      ("""[uU]?"""", Str) >> Combined("stringescape", "dqs"),
      ("""[uU]?'""", Str) >> Combined("stringescape", "sqs"),
      Include("name"),
      Include("numbers"))),
    ("keywords", List[Definition](
      ("""(assert|break|by|continue|ctypedef|del|elif|else|except\??|exec|""" +
        """finally|for|gil|global|if|include|lambda|nogil|pass|print|raise|""" +
        """return|try|while|yield|as|with)\b""", Keyword),
      ("""(DEF|IF|ELIF|ELSE)\b""", Comment.Preproc))),
    ("builtins", List[Definition](
      ("""(?<!\.)(__import__|abs|all|any|apply|basestring|bin|bool|buffer|""" +
        """bytearray|bytes|callable|chr|classmethod|cmp|coerce|compile|""" +
        """complex|delattr|dict|dir|divmod|enumerate|eval|execfile|exit|""" +
        """file|filter|float|frozenset|getattr|globals|hasattr|hash|hex|id|""" +
        """input|int|intern|isinstance|issubclass|iter|len|list|locals|""" +
        """long|map|max|min|next|object|oct|open|ord|pow|property|range|""" +
        """raw_input|reduce|reload|repr|reversed|round|set|setattr|slice|""" +
        """sorted|staticmethod|str|sum|super|tuple|type|unichr|unicode|""" +
        """vars|xrange|zip)\b""", Name.Builtin),
      ("""(?<!\.)(self|None|Ellipsis|NotImplemented|False|True|NULL""" +
        """)\b""", Name.Builtin.Pseudo),
      ("""(?<!\.)(ArithmeticError|AssertionError|AttributeError|""" +
        """BaseException|DeprecationWarning|EOFError|EnvironmentError|""" +
        """Exception|FloatingPointError|FutureWarning|GeneratorExit|IOError|""" +
        """ImportError|ImportWarning|IndentationError|IndexError|KeyError|""" +
        """KeyboardInterrupt|LookupError|MemoryError|NameError|""" +
        """NotImplemented|NotImplementedError|OSError|OverflowError|""" +
        """OverflowWarning|PendingDeprecationWarning|ReferenceError|""" +
        """RuntimeError|RuntimeWarning|StandardError|StopIteration|""" +
        """SyntaxError|SyntaxWarning|SystemError|SystemExit|TabError|""" +
        """TypeError|UnboundLocalError|UnicodeDecodeError|""" +
        """UnicodeEncodeError|UnicodeError|UnicodeTranslateError|""" +
        """UnicodeWarning|UserWarning|ValueError|Warning|ZeroDivisionErro""" +
        """)\b""", Name.Exception))),
    ("numbers", List[Definition](
      ("""(\d+\.?\d*|\d*\.\d+)([eE][+-]?[0-9]+)?""", pyments.Number.Float),
      ("""0\d+""", pyments.Number.Oct),
      ("""0[xX][a-fA-F0-9]+""", pyments.Number.Hex),
      ("""\d+L""", pyments.Number.Integer.Long),
      ("""\d+""", pyments.Number.Integer))),
    ("backtick", List[Definition](
      (""" +`.*?`""", Str.Backtick))),
    ("name", List[Definition](
      ("""@[a-zA-Z0-9_]+""", Name.Decorator),
      ("""[a-zA-Z_][a-zA-Z0-9_]*""", Name))),
    ("funcname", List[Definition](
      ("""[a-zA-Z_][a-zA-Z0-9_]*""", Name.Function) >> Pop)),
    ("cdef", List[Definition](
      ("""(public|readonly|extern|api|inline)\b""", Keyword.Reserved),
      ("""(struct|enum|union|class)\b""", Keyword),
      ("""([a-zA-Z_][a-zA-Z0-9_]*)(\s*)(?=[(:#=]|$)""",
        ByGroups(Name.Function, Text)) >> Pop,
      ("""([a-zA-Z_][a-zA-Z0-9_]*)(\s*)(,)""",
        ByGroups(Name.Function, Text, Punctuation)),
      ("""from\b""", Keyword) >> Pop,
      ("""as\b""", Keyword),
      (""":""", Punctuation) >> Pop,
      ("""(?=["\'])""", Text) >> Pop,
      ("""[a-zA-Z_][a-zA-Z0-9_]*""", Keyword.Type),
      (""".""", Text))),
    ("classname", List[Definition](
      ("""[a-zA-Z_][a-zA-Z0-9_]*""", Name.Class) >> Pop)),
    ("import", List[Definition](
      ("""(\s+)(as)(\s+)""", ByGroups(Text, Keyword, Text)),
      ("""[a-zA-Z_][a-zA-Z0-9_.]*""", Name.Namespace),
      ("""(\s*)(,)(\s*)""", ByGroups(Text, Operator, Text)),
      ("""""", Text) >> Pop // all else: go back
      )),
    ("fromimport", List[Definition](
      ("""(\s+)(c?import)\b""", ByGroups(Text, Keyword)) >> Pop,
      ("""[a-zA-Z_.][a-zA-Z0-9_.]*""", Name.Namespace),
      //``cdef foo from "heade"""``, or ``for foo from 0 < i < 10``
      ("""""", Text) >> Pop)),
    ("stringescape", List[Definition](
      ("""\\([\\abfnrtv"\']|\n|N{.*?}|u[a-fA-F0-9]{4}|""" +
        """U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})""", Str.Escape))),
    ("strings", List[Definition](
      ("""%(\([a-zA-Z0-9]+\))?[-#0 +]*([0-9]+|[*])?(\.([0-9]+|[*]))?""" +
        """[hlL]?[diouxXeEfFgGcrs%]""", Str.Interpol),
      ("[^\\\'\"%\n]+", Str),
      //quotes, percents and backslashes must be parsed one at a time
      ("""[\'"\\]""", Str),
      // unhandled string formatting sign
      ("""%""", Str) // newlines are an error (use "nl" state)
      )),
    ("nl", List[Definition](
      ("""\n""", Str))),
    ("dqs", List[Definition](
      (""""""", Str) >> Pop,
      ("""\\\\|\\"|\\\n""", Str.Escape), // included here again for raw strings
      Include("strings"))),
    ("sqs", List[Definition](
      ("""'""", Str) >> Pop,
      ("""\\\\|\\'|\\\n""", Str.Escape), // included here again for raw strings
      Include("strings"))),
    ("tdqs", List[Definition](
      (""""""""", Str) >> Pop,
      Include("strings"),
      Include("nl"))),
    ("tsqs", List[Definition](
      ("""'''""", Str) >> Pop,
      Include("strings"),
      Include("nl"))))
}
