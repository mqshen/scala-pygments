package pyments

trait Filter {
  def filter(lexer: Option[Lexer], stream: List[(Token, java.lang.String)]): List[(Token, java.lang.String)]
}

object Filter {
  /**
   * Use this method to apply an iterable of filters to
   * a stream. If pyments.lexer is given it's forwarded to the
   * filter, otherwise the filter receives 'None'.
   */
  def applyFilters(stream: List[(Token, java.lang.String)],
                   filters: List[Filter],
                   lexer: Option[Lexer] = None) = {
    var s = stream
    for { filter_ <- filters }
      s = for (token <- filter_.filter(lexer, s)) yield token

    s
  }
}