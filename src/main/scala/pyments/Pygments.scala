package pyments

object Pygments {

  def lex(code: String, lexer: Lexer) = lexer.getTokens(code)

  def format(tokens: List[(Token, String)], formatter: Formatter): List[String] = {
    try {
      formatter.format(tokens)
    } catch {
      case _ => Nil
    }
  }

  def highlight(code: String,
                lexer: Lexer,
                formatter: Formatter): List[String] = format(lex(code, lexer), formatter)
}