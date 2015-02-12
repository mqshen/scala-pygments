package pyments.lexer

import java.lang.Number

import pyments._
import pyments._
import Helpers._
import xml.Utility._

class ClojureLexer(val options: LexerOptions) extends RegexLexer {

  override val name = "Clojure"
  override val aliases = "clojure" :: "clj" :: Nil
  override val fileNames = "*.clj" :: Nil
  override val mimeTypes = "text/x-clojure" :: "application/x-clojure" :: Nil

  val keywords = "fn" :: "def" :: "defn" :: "defmacro" :: "defmethod" :: "defmulti" :: "defn-" ::
    "defstruct" :: "if" :: "cond" :: "let" :: "for" :: Nil

  val builtins = "." :: ".." ::
    "\\*" :: "\\+" :: "-" :: "->" :: "/" :: "<" :: "<=" :: "=" :: "==" :: ">" :: ">=" ::
    "accessor" :: "agent" :: "agent-errors" :: "aget" :: "alength" :: "all-ns" ::
    "alter" :: "and" :: "append-child" :: "apply" :: "array-map" :: "aset" ::
    "aset-boolean" :: "aset-byte" :: "aset-char" :: "aset-double" :: "aset-float" ::
    "aset-int" :: "aset-long" :: "aset-short" :: "assert" :: "assoc" :: "await" ::
    "await-for" :: "bean" :: "binding" :: "bit-and" :: "bit-not" :: "bit-or" ::
    "bit-shift-left" :: "bit-shift-right" :: "bit-xor" :: "boolean" :: "branch?" ::
    "butlast" :: "byte" :: "cast" :: "char" :: "children" :: "class" ::
    "clear-agent-errors" :: "comment" :: "commute" :: "comp" :: "comparator" ::
    "complement" :: "concat" :: "conj" :: "cons" :: "constantly" ::
    "construct-proxy" :: "contains?" :: "count" :: "create-ns" :: "create-struct" ::
    "cycle" :: "dec" :: "deref" :: "difference" :: "disj" :: "dissoc" :: "distinct" ::
    "doall" :: "doc" :: "dorun" :: "doseq" :: "dosync" :: "dotimes" :: "doto" ::
    "double" :: "down" :: "drop" :: "drop-while" :: "edit" :: "end?" :: "ensure" ::
    "eval" :: "every?" :: "false?" :: "ffirst" :: "file-seq" :: "filter" :: "find" ::
    "find-doc" :: "find-ns" :: "find-var" :: "first" :: "float" :: "flush" ::
    "fnseq" :: "frest" :: "gensym" :: "get-proxy-class" :: "get" ::
    "hash-map" :: "hash-set" :: "identical?" :: "identity" :: "if-let" :: "import" ::
    "in-ns" :: "inc" :: "index" :: "insert-child" :: "insert-left" :: "insert-right" ::
    "inspect-table" :: "inspect-tree" :: "instance?" :: "int" :: "interleave" ::
    "intersection" :: "into" :: "into-array" :: "iterate" :: "join" :: "key" :: "keys" ::
    "keyword" :: "keyword?" :: "last" :: "lazy-cat" :: "lazy-cons" :: "left" ::
    "lefts" :: "line-seq" :: "list*" :: "list" :: "load" :: "load-file" ::
    "locking" :: "long" :: "loop" :: "macroexpand" :: "macroexpand-1" ::
    "make-array" :: "make-node" :: "map" :: "map-invert" :: "map?" :: "mapcat" ::
    "max" :: "max-key" :: "memfn" :: "merge" :: "merge-with" :: "meta" :: "min" ::
    "min-key" :: "name" :: "namespace" :: "neg?" :: "new" :: "newline" :: "next" ::
    "nil?" :: "node" :: "not" :: "not-any?" :: "not-every?" :: "not=" :: "ns-imports" ::
    "ns-interns" :: "ns-map" :: "ns-name" :: "ns-publics" :: "ns-refers" ::
    "ns-resolve" :: "ns-unmap" :: "nth" :: "nthrest" :: "or" :: "parse" :: "partial" ::
    "path" :: "peek" :: "pop" :: "pos?" :: "pr" :: "pr-str" :: "print" :: "print-str" ::
    "println" :: "println-str" :: "prn" :: "prn-str" :: "project" :: "proxy" ::
    "proxy-mappings" :: "quot" :: "rand" :: "rand-int" :: "range" :: "re-find" ::
    "re-groups" :: "re-matcher" :: "re-matches" :: "re-pattern" :: "re-seq" ::
    "read" :: "read-line" :: "reduce" :: "ref" :: "ref-set" :: "refer" :: "rem" ::
    "remove" :: "remove-method" :: "remove-ns" :: "rename" :: "rename-keys" ::
    "repeat" :: "replace" :: "replicate" :: "resolve" :: "rest" :: "resultset-seq" ::
    "reverse" :: "rfirst" :: "right" :: "rights" :: "root" :: "rrest" :: "rseq" ::
    "second" :: "select" :: "select-keys" :: "send" :: "send-off" :: "seq" ::
    "seq-zip" :: "seq?" :: "set" :: "short" :: "slurp" :: "some" :: "sort" ::
    "sort-by" :: "sorted-map" :: "sorted-map-by" :: "sorted-set" ::
    "special-symbol?" :: "split-at" :: "split-with" :: "str" :: "string?" ::
    "struct" :: "struct-map" :: "subs" :: "subvec" :: "symbol" :: "symbol?" ::
    "sync" :: "take" :: "take-nth" :: "take-while" :: "test" :: "time" :: "to-array" ::
    "to-array-2d" :: "tree-seq" :: "true?" :: "union" :: "up" :: "update-proxy" ::
    "val" :: "vals" :: "var-get" :: "var-set" :: "var?" :: "vector" :: "vector-zip" ::
    "vector?" :: "when" :: "when-first" :: "when-let" :: "when-not" ::
    "with-local-vars" :: "with-meta" :: "with-open" :: "with-out-str" ::
    "xml-seq" :: "xml-zip" :: "zero?" :: "zipmap" :: "zipper" :: Nil

  // valid names for identifiers
  // well, names can only not consist fully of numbers
  // but this should be good enough for now

  // TODO / should divide keywords/symbols into namespace/rest
  // but that's hard, so just pretend / is part of the name
  val valid_name = """[\w!$%*+,<=>?/.-]+"""

  private def multiEscape(entries: List[String]) = "(?:" + entries.map(escape).mkString("|") + ")?![\\w!$%*+,<=>?/.-]"

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      // the comments - always starting with semicolon
      // and going to the end of the line
      (""";.*$""", Comment.Single),

      // whitespaces - usually not relevant
      ("""[,\s]+""", Whitespace),

      // numbers
      ("""-?\d+\.\d+""", pyments.Number.Float),
      ("""-?\d+""", pyments.Number.Integer),
      ("""0x-?[abcdef\d]+""", pyments.Number.Hex),

      // strings, symbols and characters
      (""""(\\\\|\\"|[^"])*"""", Str),
      ("'" + valid_name, Str.Symbol),
      ("""\\(.|[a-z]+)""", Str.Char),

      // keywords
      (""":""" + valid_name, Name.Constant),

      // special operators
      ("""~@|[`\'#^~&]""", Operator),

      // highlight the keywords
      (multiEscape(keywords), Keyword),

      // highlight the builtins
      (multiEscape(builtins), Name.Builtin),

      // the remaining functions
      ("""(?<=\()""" + valid_name, Name.Function),
      // find the remaining variables
      (valid_name, Name.Variable),

      // Clojure accepts vector notation
      ("""(\[|\])""", Punctuation),

      // Clojure accepts map notation
      ("""(\{|\})""", Punctuation),

      // the famous parentheses!
      ("""(\(|\))""", Punctuation))))
}