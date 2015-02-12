package pyments.styles

import java.lang.Number

import pyments._
import pyments._

class SolarizedDarkStyle extends Style {

  val BASE03 = "#002B36"
  val BASE02 = "#073642"
  val BASE01 = "#586E75"
  val BASE00 = "#657B83"
  val BASE0 = "#839496"
  val BASE1 = "#93A1A1"
  val BASE2 = "#EEE8D5"
  val BASE3 = "#FDF6E3"
  val YELLOW = "#B58900"
  val ORANGE = "#CB4B16"
  val RED = "#DC322F"
  val MAGENTA = "#D33682"
  val VIOLET = "#6C71C4"
  val BLUE = "#268BD2"
  val CYAN = "#2AA198"
  val GREEN = "#859900"

  override val backgroundColor = BASE03

  var styles = collection.mutable.Map[Token, String](
    Keyword -> GREEN,
    Keyword.Constant -> ORANGE,
    Keyword.Declaration -> BLUE,
    //Keyword.Namespace
    //Keyword.Pseudo
    Keyword.Reserved -> BLUE,
    Keyword.Type -> RED,

    //Name
    Name.Attribute -> BASE1,
    Name.Builtin -> YELLOW,
    Name.Builtin.Pseudo -> BLUE,
    Name.Class -> BLUE,
    Name.Constant -> ORANGE,
    Name.Decorator -> BLUE,
    Name.Entity -> ORANGE,
    Name.Exception -> ORANGE,
    Name.Function -> BLUE,
    //Name.Label
    //Name.Namespace
    //Name.Other
    Name.Tag -> BLUE,
    Name.Variable -> BLUE,
    //Name.Variable.Class
    //Name.Variable.Global
    //Name.Variable.Instance

    //Literal
    //Literal.Date
    Str -> CYAN,
    Str.Backtick -> BASE01,
    Str.Char -> CYAN,
    Str.Doc -> BASE1,
    //Str.Double
    Str.Escape -> ORANGE,
    Str.Heredoc -> BASE1,
    //Str.Interpol
    //Str.Other
    Str.Regex -> RED,
    //Str.Single
    //Str.Symbol
    pyments.Number -> CYAN,
    //Number.Float
    //Number.Hex
    //Number.Integer
    //Number.Integer.Long
    //Number.Oct

    Operator -> GREEN,
    //Operator.Word

    //Punctuation -> ORANGE,

    Comment -> BASE01,
    //Comment.Multiline
    Comment.Preproc -> GREEN,
    //Comment.Single
    Comment.Special -> GREEN,

    //Generic
    Generic.Deleted -> CYAN,
    Generic.Emph -> "italic",
    Generic.Error -> RED,
    Generic.Heading -> ORANGE,
    Generic.Inserted -> GREEN,
    //Generic.Output
    //Generic.Prompt
    Generic.Strong -> "bold",
    Generic.Subheading -> ORANGE,
    //Generic.Traceback

    Token -> BASE1,
    Other -> ORANGE)
}
