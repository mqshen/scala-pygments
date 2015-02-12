package pyments

import java.util.regex.{ MatchResult, Matcher, Pattern }

class LexerOptions(val stripNewLine: Boolean = true,
                   val stripAll: Boolean = false,
                   val ensureNewLine: Boolean = true,
                   val tabSize: Int = 0,
                   val encoding: String = "UTF-8",
                   val filters: List[Filter] = Nil)

trait Lexer {
  val options: LexerOptions

  var filters = options.filters

  val name: String

  val aliases: List[String] = Nil

  val fileNames: List[String] = Nil

  val aliasFileNames: List[String] = Nil

  val mimeTypes: List[String] = Nil

  def addFilter(filter: Filter) = {
    filters = filter :: filters
  }

  /**
   * Return an iterable of (tokentype, value) pairs generated from
   * 'text'. If 'unfiltered' is set to 'True', the filtering mechanism
   * is bypassed even if filters are defined.
   *
   * Also preprocess the text, i.e. expand tabs and strip it if
   * wanted and applies registered filters.
   */
  def getTokens(t: String, unfiltered: Boolean = false) = {
    import pyments.Filter._

    var text = t.replaceAll("\r\n", "\n").replaceAll("\r", "\n")

    if (options.stripAll) text = text.trim
    else if (options.stripNewLine) text = text.stripLineEnd.reverse.stripLineEnd.reverse // =^_^=
    if (options.tabSize > 0) text = text.replaceAll("\t", " " * options.tabSize)
    if (options.ensureNewLine && !text.endsWith("\n")) text = text + "\n"

    var stream = getTokensUnprocessed(text).map(t => (t._2, t._3))
    if (!unfiltered) stream = applyFilters(stream, filters, Some(this))

    stream
  }

  /**
   * Return an iterable of (tokentype, value) pairs.
   * In subclasses, implement this method as a generator to
   * maximize effectiveness.
   */
  def getTokensUnprocessed(text: String): List[(Int, Token, String)]

}

trait TextAnalyser {

  def analyse_text(text: String): Float
}

abstract sealed class StateDef

case class TokenDefs(list: List[Definition]) extends StateDef

trait StateDefHelpers {
  implicit def ListToTokenDefs(list: List[Definition]): StateDef = TokenDefs(list)
}

abstract sealed class Desc

trait SimpleTerm {}

case class IsToken(t: Token) extends Desc with SimpleTerm {}
case class Using(lexer: Lexer) extends Desc with SimpleTerm {}
case class ByGroups(tt: SimpleTerm*) extends Desc

trait DescHelpers {
  implicit def TokenToIsToken(t: Token): Desc = IsToken(t)
  implicit def TokenToSimpleTerm(t: Token): SimpleTerm = IsToken(t)
}

abstract sealed class Definition

case class Include(state: String) extends Definition
case class WithAction(state: (String, Desc, Action)) extends Definition
case class WithoutAction(state: (String, Desc)) extends Definition

trait DefinitionHelpers {
  implicit def TupleToWithAction(state: (String, Desc, Action)): Definition = WithAction(state)
  implicit def TupleToWithoutAction(state: (String, Desc)): Definition = WithoutAction(state)
  implicit def TupleToWithoutAction2(state: (String, Token)): Definition = WithoutAction((state._1, IsToken(state._2)))
}

abstract sealed class Action

trait SimpleAction

case class GoTo(str: String) extends Action with SimpleAction

case class Pop(steps: Int) extends Action with Popper with SimpleAction

case class T(t: SimpleAction*) extends Action

case class Combined(t: String*) extends Action

trait ActionHelpers {
  implicit def StringToAction(state: String): Action = GoTo(state)
}

trait Popper {
  def steps: Int
}

object Pop extends Action with Popper with SimpleAction {
  val steps = 1
}

object Push extends Action with SimpleAction {

}

object DoNothing extends Action

/*
A pseudo match object constructed from a string.
*/
case class _PseudoMatch(val _start: Int, val _text: String) extends MatchResult {
  def end(): Int = _start + _text.length

  def end(group: Int): Int = _start + _text.length

  def group(): String = _text

  def group(group: Int): String = _text

  def groupCount(): Int = 1

  def start(): Int = _start

  def start(group: Int): Int = _start
}

/*
    Base for simple stateful regular expression-based lexers.
    Simplifies the lexing process so that you need only
    provide a list of states and regular expressions.
*/
trait RegexLexer extends Lexer {
  val tokens: Map[String, StateDef]

  val flags = Pattern.MULTILINE

  private var _tmpname = 0

  lazy val _tokens = processTokendef(tokens)

  private def processTokendef(tokendefs: Map[String, StateDef]) = {

    val processed = scala.collection.mutable.Map[String, List[((String) => Matcher, Desc, Action)]]()
    for (state <- tokendefs.keys) {
      //println("begin process %s" format state)
      processed(state) = processState(tokendefs, processed, state)
    }

    processed
  }

  private def processState(unprocessed: Map[String, StateDef],
                           processed: scala.collection.mutable.Map[String, List[((String) => Matcher, Desc, Action)]],
                           state: String): List[((String) => Matcher, Desc, Action)] = {

    if (processed.contains(state)) processed(state)
    else {

      unprocessed(state) match {

        case TokenDefs(list) => {
          //println("TokenDefs " + list)
          list.flatMap {
            case Include(otherState) => {
              //println("Include " + otherState)
              processState(unprocessed, processed, otherState)
            }
            case WithAction(tdef) =>
              (processRegex(tdef._1, flags), tdef._2, processNewState(tdef._3, unprocessed, processed)) :: Nil
            case WithoutAction(tdef) =>
              (processRegex(tdef._1, flags), tdef._2, DoNothing) :: Nil
          }
        }
      }
    }
  }

  private def processRegex(rex: String, flags: Int) = {
    (s: String) =>
      {
        Pattern.compile(rex, flags).matcher(s)
      }
  }

  private def processNewState(newState: Action,
                              unprocessed: Map[String, StateDef],
                              processed: scala.collection.mutable.Map[String, List[((String) => Matcher, Desc, Action)]]) = {

    newState match {
      case Combined(states @ _*) => {
        val tmp_state = "_tmp_%d" format _tmpname
        _tmpname += 1

        processed(tmp_state) = states.flatMap(s => processState(unprocessed, processed, s)).toList
        GoTo(tmp_state)
      }
      case s => s
    }
  }

  override def getTokensUnprocessed(text: String) = getTokensUnprocessed(text, List("root"))

  /*
      Split ``text`` into (tokentype, text) pairs.

      ``stack`` is the inital stack (default: "['root']"
  */
  def getTokensUnprocessed(text: String, stack: List[String]) = {
    var pos = 0
    val tokendefs = _tokens
    var statestack = stack
    var statetokens = tokendefs(statestack.head)
    var result = new scala.collection.mutable.ListBuffer[(Int, Token, String)]()
    var shouldStop = false

    def usingProcessing(lx: Lexer, m: MatchResult) = {
      val s = m.start
      lx.getTokensUnprocessed(m.group).map(t => (t._1 + s, t._2, t._3))
    }

    val len = text.length
    while (!shouldStop) {
      var added = false
      statetokens.map(t => (t._1(text), t._2, t._3))
        .filter(_._1.region(pos, len).lookingAt)
        .headOption
        .foreach(t => {
          val (m, token, action) = t
          token match {
            case IsToken(tt) => result += ((pos, tt, m.group()))
            case ByGroups(tl @ _*) => {
              for { (t, i) <- tl.zipWithIndex } {
                t match {
                  case IsToken(token) => {
                    val data = m.group(i + 1)
                    if (data != null && !data.isEmpty) result += ((m.start(i + 1), token, data))
                  }
                  case Using(lexer) => {
                    result ++= usingProcessing(lexer, _PseudoMatch(m.start(i + 1), m.group(i + 1)))
                  }
                }
              }
            }
            case Using(lexer) => {
              result ++= usingProcessing(lexer, m)
            }
          }

          pos = m.end()
          statestack = action match {
            case GoTo(state) => state :: statestack
            case Pop(steps)  => statestack.drop(steps)
            case Pop         => statestack.tail
            case Push        => statestack.head :: statestack
            case DoNothing   => statestack
            case T(ll @ _*) => {
              var s = statestack
              for (sa <- ll) {
                s = (sa match {
                  case GoTo(state) => state :: s
                  case Pop(steps)  => s.drop(steps)
                  case Pop         => s.tail
                  case Push        => s.head :: s
                })
              }
              s
            }
            case Combined(_) => Nil
          }
          //println("Stack " + statestack)
          statetokens = tokendefs(statestack.head)
          added = true
        })
      //println("Pos " + pos + " " + text(pos))
      if (!added) {
        try {
          if (text(pos) == '\n') {
            // at EOL, reset state to "root"
            pos += 1
            statestack = List("root")
            statetokens = tokendefs("root")
            result += ((pos, Text, "\n"))
          } else if (statestack.isEmpty || text(pos) != '\n') {
            result += ((pos, Error, text(pos).toString))
            pos += 1
          }
        } catch {
          case e: IndexOutOfBoundsException => shouldStop = true
        }
      }

    }
    result.toList
  }
}

/*
    This pyments.lexer takes two pyments.lexer as arguments. A root pyments.lexer and
    a language pyments.lexer. First everything is scanned using the language
    pyments.lexer, afterwards all ``Other`` tokens are lexed using the root
    pyments.lexer.

    The lexers from the ``template`` pyments.lexer package use this base pyments.lexer.
*/
trait DelegatingLexer extends Lexer {

  def _root_lexer(): Lexer

  def _language_lexer(): Lexer

  lazy val rootLexer = _root_lexer()
  lazy val languageLexer = _language_lexer()

  override def getTokensUnprocessed(text: String) = {

    languageLexer.getTokensUnprocessed(text).flatMap {
      case (i, Other, v) => rootLexer.getTokensUnprocessed(v)
      case t             => t :: Nil
    }
  }
}

class RichTuple1(t: (String, Token)) {
  def >>(str: String) = WithAction((t._1, IsToken(t._2), GoTo(str)))
  def >>(a: Action) = WithAction((t._1, IsToken(t._2), a))
  def >>(a1: SimpleAction, a2: SimpleAction) = WithAction((t._1, IsToken(t._2), T(a1, a2)))
}

class RichTuple2(t: (String, ByGroups)) {
  def >>(str: String) = WithAction((t._1, t._2, GoTo(str)))
  def >>(a: Action) = WithAction((t._1, t._2, a))
  def >>(a1: SimpleAction, a2: SimpleAction) = WithAction((t._1, t._2, T(a1, a2)))
}

object Helpers extends StateDefHelpers with DefinitionHelpers with ActionHelpers with DescHelpers {

  implicit def Tuple2RichTuple(t: (String, Token)) = new RichTuple1(t)
  implicit def Tuple2RichTuple(t: (String, ByGroups)) = new RichTuple2(t)

  implicit def StringToSimpleAction(s: String): SimpleAction = GoTo(s)
  val _default_analyse = (x: String) => 0.0
}