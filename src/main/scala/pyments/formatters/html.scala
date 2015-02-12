package pyments.formatters

import pyments._
import pyments._
import xml.Utility._

class CleverString(str: String) {
  def and(otherStr: String) = if (str.isEmpty) "" else otherStr
}

object CleverStringHelper {
  implicit def StringToClever(str: String): CleverString = new CleverString(str)

  implicit def StringToBoolean(str: String): Boolean = !str.isEmpty
  implicit def StringBuilderToBoolean(str: collection.mutable.StringBuilder): Boolean = !str.isEmpty
}

/*
    Format tokens as HTML 4 ``<span>`` tags within a ``<pre>`` tag, wrapped
    in a ``<div>`` tag. The ``<div>``'s CSS class can be set by the `cssclass`
    option.

    If the `linenos` option is set to ``"table"``, the ``<pre>`` is
    additionally wrapped inside a ``<table>`` which has one row and two
    cells: one containing the line numbers and one containing the code.
    Example:

    .. sourcecode:: html

        <div class="highlight" >
        <table><tr>
          <td class="linenos" title="click to toggle"
            onclick="with (this.firstChild.style)
                     { display = (display == '') ? 'none' : '' }">
            <pre>1
            2</pre>
          </td>
          <td class="code">
            <pre><span class="Ke">def </span><span class="NaFu">foo</span>(bar):
              <span class="Ke">pass</span>
            </pre>
          </td>
        </tr></table></div>

    (whitespace added to improve clarity).

    Wrapping can be disabled using the `nowrap` option.

    A list of lines can be specified using the `hl_lines` option to make these
    lines highlighted (as of Pygments 0.11).

    With the `full` option, a complete HTML 4 document is output, including
    the style definitions inside a ``<style>`` tag, or in a separate file if
    the `cssfile` option is given.

    The `get_style_defs(arg='')` method of a `HtmlFormatter` returns a string
    containing CSS rules for the CSS classes used by the formatter. The
    argument `arg` can be used to specify additional CSS selectors that
    are prepended to the classes. A call `fmter.get_style_defs('td .code')`
    would result in the following CSS classes:

    .. sourcecode:: css

        td .code .kw { font-weight: bold; color: #00FF00 }
        td .code .cm { color: #999999 }
        ...

    If you have Pygments 0.6 or higher, you can also pass a list or tuple to the
    `get_style_defs()` method to request multiple prefixes for the tokens:

    .. sourcecode:: python

        formatter.get_style_defs(['div.syntax pre', 'pre.syntax'])

    The output would then look like this:

    .. sourcecode:: css

        div.syntax pre .kw,
        pre.syntax .kw { font-weight: bold; color: #00FF00 }
        div.syntax pre .cm,
        pre.syntax .cm { color: #999999 }
        ...

    Additional options accepted:

    `nowrap`
        If set to ``True``, don't wrap the tokens at all, not even inside a ``<pre>``
        tag. This disables most other options (default: ``False``).

    `full`
        Tells the formatter to output a "full" document, i.e. a complete
        self-contained document (default: ``False``).

    `title`
        If `full` is true, the title that should be used to caption the
        document (default: ``''``).

    `style`
        The style to use, can be a string or a Style subclass (default:
        ``'default'``). This option has no effect if the `cssfile`
        and `noclobber_cssfile` option are given and the file specified in
        `cssfile` exists.

    `noclasses`
        If set to true, token ``<span>`` tags will not use CSS classes, but
        inline pyments.styles. This is not recommended for larger pieces of code since
        it increases output size by quite a bit (default: ``False``).

    `classprefix`
        Since the token types use relatively short class names, they may clash
        with some of your own class names. In this case you can use the
        `classprefix` option to give a string to prepend to all Pygments-generated
        CSS class names for token types.
        Note that this option also affects the output of `get_style_defs()`.

    `cssclass`
        CSS class for the wrapping ``<div>`` tag (default: ``'highlight'``).
        If you set this option, the default selector for `get_style_defs()`
        will be this class.

        *New in Pygments 0.9:* If you select the ``'table'`` line numbers, the
        wrapping table will have a CSS class of this string plus ``'table'``,
        the default is accordingly ``'highlighttable'``.

    `cssstyles`
        Inline CSS pyments.styles for the wrapping ``<div>`` tag (default: ``''``).

    `prestyles`
        Inline CSS pyments.styles for the ``<pre>`` tag (default: ``''``).  *New in
        Pygments 0.11.*

    `cssfile`
        If the `full` option is true and this option is given, it must be the
        name of an external file. If the filename does not include an absolute
        path, the file's path will be assumed to be relative to the pyments.main output
        file's path, if the latter can be found. The stylesheet is then written
        to this file instead of the HTML file. *New in Pygments 0.6.*

    `noclobber_cssfile`
        If `cssfile` is given and the specified file exists, the css file will
        not be overwritten. This allows the use of the `full` option in
        combination with a user specified css file. Default is ``False``.
        *New in Pygments 1.1.*

    `linenos`
        If set to ``'table'``, output line numbers as a table with two cells,
        one containing the line numbers, the other the whole code.  This is
        copy-and-paste-friendly, but may cause alignment problems with some
        browsers or fonts.  If set to ``'inline'``, the line numbers will be
        integrated in the ``<pre>`` tag that contains the code (that setting
        is *new in Pygments 0.8*).

        For compatibility with Pygments 0.7 and earlier, every true value
        except ``'inline'`` means the same as ``'table'`` (in particular, that
        means also ``True``).

        The default value is ``False``, which means no line numbers at all.

        **Note:** with the default ("table") line number mechanism, the line
        numbers and code can have different line heights in Internet Explorer
        unless you give the enclosing ``<pre>`` tags an explicit ``line-height``
        CSS property (you get the default line spacing with ``line-height:
        125%``).

    `hl_lines`
        Specify a list of lines to be highlighted.  *New in Pygments 0.11.*

    `linenostart`
        The line number for the first line (default: ``1``).

    `linenostep`
        If set to a number n > 1, only every nth line number is printed.

    `linenospecial`
        If set to a number n > 0, every nth line number is given the CSS
        class ``"special"`` (default: ``0``).

    `nobackground`
        If set to ``True``, the formatter won't output the background color
        for the wrapping element (this automatically defaults to ``False``
        when there is no wrapping element [eg: no argument for the
        `get_syntax_defs` method given]) (default: ``False``). *New in
        Pygments 0.6.*

    `lineseparator`
        This string is output between lines of code. It defaults to ``"\n"``,
        which is enough to break a line inside ``<pre>`` tags, but you can
        e.g. set it to ``"<br>"`` to get HTML line breaks. *New in Pygments
        0.7.*

    `lineanchors`
        If set to a nonempty string, e.g. ``foo``, the formatter will wrap each
        output line in an anchor tag with a ``name`` of ``foo-linenumber``.
        This allows easy linking to certain lines. *New in Pygments 0.9.*

    `anchorlinenos`
        If set to `True`, will wrap line numbers in <a> tags. Used in
        combination with `linenos` and `lineanchors`.


    **Subclassing the HTML formatter**

    *New in Pygments 0.7.*

    The HTML formatter is now built in a way that allows easy subclassing, thus
    customizing the output HTML code. The `format()` method calls
    `self._format_lines()` which returns a generator that yields tuples of ``(1,
    line)``, where the ``1`` indicates that the ``line`` is a line of the
    formatted source code.

    If the `nowrap` option is set, the generator is the iterated over and the
    resulting HTML is output.

    Otherwise, `format()` calls `self.wrap()`, which wraps the generator with
    other generators. These may add some HTML code to the one generated by
    `_format_lines()`, either by modifying the lines generated by the latter,
    then yielding them again with ``(1, line)``, and/or by yielding other HTML
    code before or after the lines, with ``(0, html)``. The distinction between
    source lines and other code makes it possible to wrap the generator multiple
    times.

    The default `wrap()` implementation adds a ``<div>`` and a ``<pre>`` tag.

    A custom `HtmlFormatter` subclass could look like this:

    .. sourcecode:: python

        class CodeHtmlFormatter(HtmlFormatter):

            def wrap(self, source, outfile):
                return self._wrap_code(source)

            def _wrap_code(self, source):
                yield 0, '<code>'
                for i, t in source:
                    if i == 1:
                        # it's a line of formatted code
                        t += '<br>'
                    yield i, t
                yield 0, '</code>'

    This results in wrapping the formatted lines with a ``<code>`` tag, where the
    source lines are broken using ``<br>`` tags.

    After calling `wrap()`, the `format()` method also adds the "line numbers"
    and/or "full document" wrappers if the respective options are set. Then, all
    HTML yielded by the wrapped generator is output.
    */
class HtmlFormatter(
    val options: FormatterOptions = new FormatterOptions,
    val needWrap: Boolean = true,
    val lineSeparator: String = "\n",
    val nowrap: Boolean = false,
    val lineOS: Boolean = false, //false = table
    val lineAnchors: String = "L",
    val preStyles: String = "",
    val noBackground: Boolean = false,
    val classPrefix: String = "",
    val cssStyles: String = "",
    val cssClass: String = "highlight",
    val anchorLinenOS: Boolean = true) extends Formatter {
  import CleverStringHelper._

  val name = "HTML"

  val aliases = "html" :: Nil

  val fileNames = ".html" :: "*.htm" :: Nil

  val (ttype2class, class2style) = createStylesheet

  private def createStylesheet = {
    var style = new StringBuilder

    val t2c = collection.mutable.Map[Token, String]()
    val c2s = collection.mutable.Map[String, (String, Token, Int)]()

    for { (token, stl) <- options.style._styles } {
      val css = cssClass(token)

      if (stl.color != "") style ++= ("color: #%s; ".format(stl.color))
      if (stl.bold) style ++= "font-weight: bold; "
      if (stl.italic) style ++= "font-style: italic; "
      if (stl.underline) style ++= "text-decoration: underline; "
      if (stl.backgroundColor != "") style ++= "background-color: #%s; ".format(stl.backgroundColor)
      if (stl.borderColor != "") style ++= "border: 1px solid #%s; ".format(stl.borderColor)

      if (!style.isEmpty) {
        t2c(token) = css
        c2s(css) = (style.substring(0, style.length - 2), token, 0)
      }
    }
    (t2c.toMap, c2s.toMap)
  }

  private def ttypeClass(token: Token) = Tokens.STANDARD_TYPES(token)

  /*Return the css class of this token type prefixed with the classprefix option.*/
  private def cssClass(token: Token) = {
    try {
      classPrefix + ttypeClass(token)
    } catch {
      case _ => ""
    }
  }

  def format(tokenSource: List[(Token, String)]) = {
    /*
    The formatting process uses several nested generators; which of
    them are used is determined by the user's options.

    Each generator should take at least one argument, ``inner``,
    and wrap the pieces of text generated by this.

    Always yield 2-tuples: (code, text). If "code" is 1, the text
    is part of the original tokensource being highlighted, if it's
    0, the text is some piece of wrapping. This makes it possible to
    use several different wrappers that process the original source
    linewise, e.g. line number generators.
    */
    var source = formatLines(tokenSource)
    //println(source.size)
    if (!nowrap) {
      if (lineAnchors)
        source = wrapLineanchors(source)
      //println(source.size)
      if (needWrap)
        source = wrap(source)
      //println(source.size)
      if (!lineOS)
        source = wrapTablelinenos(source)
      //println(source.size)
    }
    for { (t, piece) <- source } yield piece
  }

  /*
      Just format the tokens, without any wrapping tags.
      Yield individual lines.
  */
  private def formatLines(tokensource: List[(Token, String)]) = {
    val lsep = lineSeparator

    val c2s = class2style

    var lspan = ""
    var line = new StringBuilder

    var result = new collection.mutable.ListBuffer[(Int, String)]()

    var lastPart = ""

    for { (tType, value) <- tokensource } {
      //println("Begin process " + ttype + " " + value)
      val cSpan = "<span class='" + cssClass(tType) + "'>"
      //println(cspan)
      val parts = escape(value).split("\n", -1).toList
      //println("This token is splited on " + parts.size + " parts" )
      val reversed = parts.reverse
      // for all but the last line
      for { part <- reversed.tail.reverse } {
        if (!line.isEmpty) {
          //println("Line is empty")
          line ++= (if (lspan != cSpan)
            (lspan and "</span>") + cSpan + part + (cSpan and "</span>") + lsep
          else // both are the same
            part + (lspan and "</span>") + lsep)
          result.append((1, line))
          line.clear
        } else if (!part.isEmpty) {
          result.append((1, cSpan + part + (cSpan and "</span>") + lsep))
        } else
          result.append((1, lsep))
      }
      // for the last line
      if (line && reversed.head) {
        if (lspan != cSpan) {
          line ++= (lspan and "</span>") + cSpan + reversed.head
          lspan = cSpan
        } else line ++= reversed.head
      } else if (reversed.head) {
        line = new StringBuilder(cSpan + reversed.head)
        lspan = cSpan
      }
      lastPart = reversed.head
    }
    //println("size is " + result.size)

    if (line)
      result.append((1, line + (lspan and "</span>") + lsep))

    if (!lastPart) result.append((1, lsep))

    result.toList
  }

  private def wrapLineanchors(inner: List[(Int, String)]) = {
    var i = 0
    var result = new collection.mutable.ListBuffer[(Int, String)]
    for { (t, line) <- inner } {
      if (t == 1) {
        i += 1
        result += ((1, "<span class='line'>%s</span>".format(line)))
      } else result += ((0, line))
    }
    result.toList
  }

  private def wrapDiv(inner: List[(Int, String)]) = {
    var style = new collection.mutable.ListBuffer[String]

    if (cssStyles) style += cssStyles

    val styleStr = style.mkString("; ")

    ((0, ("<div " + (cssClass and (" class='%s'" format cssClass))
      + (styleStr and (" style='%s'" format style)) + ">"))) +: inner :+
      ((0, "</div>\n"))
  }

  private def wrapPre(inner: List[(Int, String)]) = {
    var style = new collection.mutable.ListBuffer[String]
    if (preStyles) style += preStyles

    val styleStr = style.mkString("; ")

    ((0, ("<pre " + (styleStr and (" style='%s'" format styleStr)) + ">"))) +: inner :+
      ((0, "</pre>"))
  }

  def wrap(source: List[(Int, String)]) = wrapDiv(wrapPre(source))

  private def wrapTablelinenos(inner: List[(Int, String)]) = {
    var lncount = 0
    var value = new collection.mutable.StringBuilder
    for { (l, v) <- inner } {
      lncount += l
      value ++= v
    }
    //println(inner)

    val fl = 0
    val mw = (lncount + fl - 1).toString.length
    val la = lineAnchors
    val aln = anchorLinenOS

    val lines = new collection.mutable.ListBuffer[String]

    val ls =
      (fl to (fl + lncount - 1)).map { i =>
        if (aln) ("<a href='#%s%d'>%" + mw + "d</a>") format (la, i, i)
        else ("%" + mw + "d") format (i)
      }.mkString("\n")

    var result = new collection.mutable.ListBuffer[(Int, String)]
    result += ((0, ("<table class='%stable'>".format(cssClass) +
      "<tr><td class='linenos'><div class='linenodiv'><pre>" +
      ls + "</pre></div></td><td class='code'>")))
    result += ((0, value))
    result += ((0, "</td></tr></table>"))

    result.toList
  }

  /*
      Return CSS style definitions for the classes produced by the current
      highlighting style. ``arg`` can be a string or list of selectors to
      insert before the token type classes.
  */
  def styleDefs: String = {
    val arg = if (cssClass) ("." + cssClass) else ""

    def prefix(cls: String) = {
      arg + " " + (if (cls) ("." + cls) else "")
    }

    val styles = for { (cls, params) <- class2style } yield ((params._3, params._2, cls, params._1))
    var lines = (for { (level, ttype, cls, style) <- styles }
      yield ("%s { %s } /* %s */".format(prefix(cls), style, ttype))).toList

    if (arg && !noBackground && options.style.backgroundColor) {
      val text_style = if (ttype2class.contains(Text)) (" " + class2style(ttype2class(Text))) else ""

      lines = (("%s { background: %s;%s }".format(prefix(""), options.style.backgroundColor, text_style))) :: lines
    }
    if (options.style.highlightColor) {
      lines = (("%s.hll { background-color: %s }".format(prefix(""), options.style.highlightColor))) :: lines
    }

    lines.mkString("\n")
  }

}

