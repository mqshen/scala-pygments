package pyments.styles

import java.lang.Number

import pyments._
import pyments._

class SolarizedStyle extends Style {

  val BASE0 = "#839496"
  val BASE1 = "#93a1a1"
  val BASE2 = "#eee8d5"
  val BASE3 = "#fdf6e3"
  val BASE00 = "#657b83"
  val BASE01 = "#586e75"
  val BASE02 = "#073642"
  val BASE03 = "#002b36"
  val YELLOW = "#b58900"
  val ORANGE = "#cb4b16"
  val RED = "#dc322f"
  val MAGENTA = "#d33682"
  val VIOLET = "#6c71c4"
  val BLUE = "#268bd2"
  val CYAN = "#2aa198"
  val GREEN = "#859900"

  override val backgroundColor = BASE3

  var styles = collection.mutable.Map[Token, String](
    Text -> ("bg: %s %s" format (BASE2, BASE01)),
    Keyword -> GREEN,
    Keyword.Constant -> "bold",
    //Keyword.Declaration     ->
    Keyword.Namespace -> (RED + " bold"),
    //Keyword.Pseudo          ->
    //Keyword.Reserved        ->
    Keyword.Type -> "bold",
    Name -> BLUE,
    //Name.Attribute          ->
    Name.Builtin -> ORANGE,
    //Name.Builtin.Pseudo     ->
    Name.Class -> ORANGE,
    Name.Tag -> "bold",
    Literal -> CYAN,
    //String                  ->
    pyments.Number -> "bold",
    //Operator                ->
    Operator.Word -> GREEN,
    Comment -> (BASE1 + " italic"),
    Generic -> MAGENTA)
}
