package pyments.styles

import java.lang.Number

import pyments._
import pyments._

class DefaultStyle extends Style {
  /*
  The default style (inspired by Emacs 22).
  */

  override val backgroundColor = "#f8f8f8"
  val defaultStyle = ""

  var styles = collection.mutable.Map[Token, String](
    Whitespace -> "#bbbbbb",
    Comment -> "italic #408080",
    Comment.Preproc -> "noitalic #BC7A00",

    //Keyword ->                   "bold #AA22FF",
    Keyword -> "bold #008000",
    Keyword.Pseudo -> "nobold",
    Keyword.Type -> "nobold #B00040",

    Operator -> "#666666",
    Operator.Word -> "bold #AA22FF",

    Name.Builtin -> "#008000",
    Name.Function -> "#0000FF",
    Name.Class -> "bold #0000FF",
    Name.Namespace -> "bold #0000FF",
    Name.Exception -> "bold #D2413A",
    Name.Variable -> "#19177C",
    Name.Constant -> "#880000",
    Name.Label -> "#A0A000",
    Name.Entity -> "bold #999999",
    Name.Attribute -> "#7D9029",
    Name.Tag -> "bold #008000",
    Name.Decorator -> "#AA22FF",

    Str -> "#BA2121",
    Str.Doc -> "italic",
    Str.Interpol -> "bold #BB6688",
    Str.Escape -> "bold #BB6622",
    Str.Regex -> "#BB6688",
    //Str.Symbol ->              "#B8860B",
    Str.Symbol -> "#19177C",
    Str.Other -> "#008000",
    pyments.Number -> "#666666",

    Generic.Heading -> "bold #000080",
    Generic.Subheading -> "bold #800080",
    Generic.Deleted -> "#A00000",
    Generic.Inserted -> "#00A000",
    Generic.Error -> "#FF0000",
    Generic.Emph -> "italic",
    Generic.Strong -> "bold",
    Generic.Prompt -> "bold #000080",
    Generic.Output -> "#888",
    Generic.Traceback -> "#04D",

    Error -> "border:#FF0000")

}