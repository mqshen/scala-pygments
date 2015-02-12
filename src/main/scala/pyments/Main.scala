package pyments

import pyments.lexer.CythonLexer

object Main extends App {
  val code = """
def primes(int kmax):
    cdef int n, k, i
    cdef int p[1000]
    result = []
    if kmax > 1000:
        kmax = 1000
    k = 0
    n = 2
    while k < kmax:
        i = 0
        while i < k and n % p[i] != 0:
            i = i + 1
        if i == k:
            p[k] = n
            k = k + 1
            result.append(n)
        n = n + 1
    return result
"""
  var out_file = new java.io.FileOutputStream("test.html")
  var out_stream = new java.io.PrintStream(out_file)

  //Pygments.lex(code, new pyments.lexer.ScalaLexer(new LexerOptions ) )
  out_stream.println("""<link type="text/css" rel="stylesheet" href="test.css">""")
  Pygments.highlight(code, new CythonLexer(new LexerOptions), new pyments.formatters.HtmlFormatter(needWrap = false, lineSeparator = "", lineAnchors = "", lineOS = true))
    .foreach(t => out_stream.println(t))

  out_stream.close

  /*var out_file1 = new java.io.FileOutputStream("test.css")

	var out_stream1 = new java.io.PrintStream(out_file1)

	out_stream1.print((new pyments.formatters.HtmlFormatter(new FormatterOptions(new pyments.styles.MurphyStyle )) ).styleDefs)

	out_stream1.close*/
}