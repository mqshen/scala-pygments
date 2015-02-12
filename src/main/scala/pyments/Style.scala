package pyments

class TokenStyle(var color: String = "",
                 var bold: Boolean = false,
                 var italic: Boolean = false,
                 var underline: Boolean = false,
                 var backgroundColor: String = "",
                 var borderColor: String = "",
                 var roman: Boolean = false,
                 var sans: Boolean = false,
                 var mono: Boolean = false)

trait Style {
  //: overall background color (``None`` means transparent)
  val backgroundColor = "#ffffff"

  //: highlight background color
  val highlightColor = "#ffffcc"

  //: Style definitions for individual token types.
  var styles: collection.mutable.Map[Token, String]

  private val ColorFormat1 = """#([0-9a-f]{6})""".r
  private val ColorFormat2 = """#([0-9a-f]{3})""".r

  private def colorformat(text: String): String = {
    text.trim match {
      case ColorFormat1(color) => color
      case ColorFormat2(color) => "%c0%c0%c0" format (color(0), color(1), color(2))
      case _                   => ""
    }
  }

  lazy val _styles = getStyles

  def getStyles = {
    val s = collection.mutable.Map[Token, TokenStyle]()

    for { (token, css) <- Tokens.STANDARD_TYPES; if (!styles.contains(token)) }
      styles(token) = ""

    for { (ttype, str) <- styles } {
      var ndef = s.getOrElse(ttype.parent, new TokenStyle)
      val styledefs = styles.getOrElse(ttype, "").split(" ").toList

      if (styledefs.contains("noinherit") && ttype != Token) {
        ndef = s(Token)
      }
      s(ttype) = ndef

      for { styledef <- styledefs; if (styledef != "noinherit") } {
        if (styledef == "bold") ndef.bold = true
        else if (styledef == "nobold") ndef.bold = false
        else if (styledef == "italic") ndef.italic = true
        else if (styledef == "noitalic") ndef.italic = false
        else if (styledef == "underline") ndef.underline = true
        else if (styledef == "nounderline") ndef.underline = false
        else if (styledef.startsWith("bg:")) ndef.backgroundColor = colorformat(styledef.substring(3))
        else if (styledef.startsWith("border:")) ndef.borderColor = colorformat(styledef.substring(7))
        else if (styledef == "roman") ndef.roman = true
        else if (styledef == "sans") ndef.sans = true
        else if (styledef == "mono") ndef.mono = true
        else ndef.color = colorformat(styledef)
      }

    }
    s
  }

  def styleForToken(token: Token) = _styles(token)

}

