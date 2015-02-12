
package pyments.lexer

import java.lang.Number

import pyments._
import pyments._
import java.util.regex.Pattern
import Helpers._

/*
A Common Lisp pyments.lexer.
*/
class CommonLispLexer(val options: LexerOptions) extends RegexLexer {

  val BUILTIN_FUNCTIONS = List[String](
    "<", "<=", "=", ">", ">=", "-", "/", "/=", "*", "+", "1-", "1+",
    "abort", "abs", "acons", "acos", "acosh", "add-method", "adjoin",
    "adjustable-array-p", "adjust-array", "allocate-instance",
    "alpha-char-p", "alphanumericp", "append", "apply", "apropos",
    "apropos-list", "aref", "arithmetic-error-operands",
    "arithmetic-error-operation", "array-dimension", "array-dimensions",
    "array-displacement", "array-element-type", "array-has-fill-pointer-p",
    "array-in-bounds-p", "arrayp", "array-rank", "array-row-major-index",
    "array-total-size", "ash", "asin", "asinh", "assoc", "assoc-if",
    "assoc-if-not", "atan", "atanh", "atom", "bit", "bit-and", "bit-andc1",
    "bit-andc2", "bit-eqv", "bit-ior", "bit-nand", "bit-nor", "bit-not",
    "bit-orc1", "bit-orc2", "bit-vector-p", "bit-xor", "boole",
    "both-case-p", "boundp", "break", "broadcast-stream-streams",
    "butlast", "byte", "byte-position", "byte-size", "caaaar", "caaadr",
    "caaar", "caadar", "caaddr", "caadr", "caar", "cadaar", "cadadr",
    "cadar", "caddar", "cadddr", "caddr", "cadr", "call-next-method", "car",
    "cdaaar", "cdaadr", "cdaar", "cdadar", "cdaddr", "cdadr", "cdar",
    "cddaar", "cddadr", "cddar", "cdddar", "cddddr", "cdddr", "cddr", "cdr",
    "ceiling", "cell-error-name", "cerror", "change-class", "char", "char<",
    "char<=", "char=", "char>", "char>=", "char/=", "character",
    "characterp", "char-code", "char-downcase", "char-equal",
    "char-greaterp", "char-int", "char-lessp", "char-name",
    "char-not-equal", "char-not-greaterp", "char-not-lessp", "char-upcase",
    "cis", "class-name", "class-of", "clear-input", "clear-output",
    "close", "clrhash", "code-char", "coerce", "compile",
    "compiled-function-p", "compile-file", "compile-file-pathname",
    "compiler-macro-function", "complement", "complex", "complexp",
    "compute-applicable-methods", "compute-restarts", "concatenate",
    "concatenated-stream-streams", "conjugate", "cons", "consp",
    "constantly", "constantp", "continue", "copy-alist", "copy-list",
    "copy-pprint-dispatch", "copy-readtable", "copy-seq", "copy-structure",
    "copy-symbol", "copy-tree", "cos", "cosh", "count", "count-if",
    "count-if-not", "decode-float", "decode-universal-time", "delete",
    "delete-duplicates", "delete-file", "delete-if", "delete-if-not",
    "delete-package", "denominator", "deposit-field", "describe",
    "describe-object", "digit-char", "digit-char-p", "directory",
    "directory-namestring", "disassemble", "documentation", "dpb",
    "dribble", "echo-stream-input-stream", "echo-stream-output-stream",
    "ed", "eighth", "elt", "encode-universal-time", "endp",
    "enough-namestring", "ensure-directories-exist",
    "ensure-generic-function", "eq", "eql", "equal", "equalp", "error",
    "eval", "evenp", "every", "exp", "export", "expt", "fboundp",
    "fceiling", "fdefinition", "ffloor", "fifth", "file-author",
    "file-error-pathname", "file-length", "file-namestring",
    "file-position", "file-string-length", "file-write-date",
    "fill", "fill-pointer", "find", "find-all-symbols", "find-class",
    "find-if", "find-if-not", "find-method", "find-package", "find-restart",
    "find-symbol", "finish-output", "first", "float", "float-digits",
    "floatp", "float-precision", "float-radix", "float-sign", "floor",
    "fmakunbound", "force-output", "format", "fourth", "fresh-line",
    "fround", "ftruncate", "funcall", "function-keywords",
    "function-lambda-expression", "functionp", "gcd", "gensym", "gentemp",
    "get", "get-decoded-time", "get-dispatch-macro-character", "getf",
    "gethash", "get-internal-real-time", "get-internal-run-time",
    "get-macro-character", "get-output-stream-string", "get-properties",
    "get-setf-expansion", "get-universal-time", "graphic-char-p",
    "hash-table-count", "hash-table-p", "hash-table-rehash-size",
    "hash-table-rehash-threshold", "hash-table-size", "hash-table-test",
    "host-namestring", "identity", "imagpart", "import",
    "initialize-instance", "input-stream-p", "inspect",
    "integer-decode-float", "integer-length", "integerp",
    "interactive-stream-p", "intern", "intersection",
    "invalid-method-error", "invoke-debugger", "invoke-restart",
    "invoke-restart-interactively", "isqrt", "keywordp", "last", "lcm",
    "ldb", "ldb-test", "ldiff", "length", "lisp-implementation-type",
    "lisp-implementation-version", "list", "list*", "list-all-packages",
    "listen", "list-length", "listp", "load",
    "load-logical-pathname-translations", "log", "logand", "logandc1",
    "logandc2", "logbitp", "logcount", "logeqv", "logical-pathname",
    "logical-pathname-translations", "logior", "lognand", "lognor",
    "lognot", "logorc1", "logorc2", "logtest", "logxor", "long-site-name",
    "lower-case-p", "machine-instance", "machine-type", "machine-version",
    "macroexpand", "macroexpand-1", "macro-function", "make-array",
    "make-broadcast-stream", "make-concatenated-stream", "make-condition",
    "make-dispatch-macro-character", "make-echo-stream", "make-hash-table",
    "make-instance", "make-instances-obsolete", "make-list",
    "make-load-form", "make-load-form-saving-slots", "make-package",
    "make-pathname", "make-random-state", "make-sequence", "make-string",
    "make-string-input-stream", "make-string-output-stream", "make-symbol",
    "make-synonym-stream", "make-two-way-stream", "makunbound", "map",
    "mapc", "mapcan", "mapcar", "mapcon", "maphash", "map-into", "mapl",
    "maplist", "mask-field", "max", "member", "member-if", "member-if-not",
    "merge", "merge-pathnames", "method-combination-error",
    "method-qualifiers", "min", "minusp", "mismatch", "mod",
    "muffle-warning", "name-char", "namestring", "nbutlast", "nconc",
    "next-method-p", "nintersection", "ninth", "no-applicable-method",
    "no-next-method", "not", "notany", "notevery", "nreconc", "nreverse",
    "nset-difference", "nset-exclusive-or", "nstring-capitalize",
    "nstring-downcase", "nstring-upcase", "nsublis", "nsubst", "nsubst-if",
    "nsubst-if-not", "nsubstitute", "nsubstitute-if", "nsubstitute-if-not",
    "nth", "nthcdr", "null", "numberp", "numerator", "nunion", "oddp",
    "open", "open-stream-p", "output-stream-p", "package-error-package",
    "package-name", "package-nicknames", "packagep",
    "package-shadowing-symbols", "package-used-by-list", "package-use-list",
    "pairlis", "parse-integer", "parse-namestring", "pathname",
    "pathname-device", "pathname-directory", "pathname-host",
    "pathname-match-p", "pathname-name", "pathnamep", "pathname-type",
    "pathname-version", "peek-char", "phase", "plusp", "position",
    "position-if", "position-if-not", "pprint", "pprint-dispatch",
    "pprint-fill", "pprint-indent", "pprint-linear", "pprint-newline",
    "pprint-tab", "pprint-tabular", "prin1", "prin1-to-string", "princ",
    "princ-to-string", "print", "print-object", "probe-file", "proclaim",
    "provide", "random", "random-state-p", "rassoc", "rassoc-if",
    "rassoc-if-not", "rational", "rationalize", "rationalp", "read",
    "read-byte", "read-char", "read-char-no-hang", "read-delimited-list",
    "read-from-string", "read-line", "read-preserving-whitespace",
    "read-sequence", "readtable-case", "readtablep", "realp", "realpart",
    "reduce", "reinitialize-instance", "rem", "remhash", "remove",
    "remove-duplicates", "remove-if", "remove-if-not", "remove-method",
    "remprop", "rename-file", "rename-package", "replace", "require",
    "rest", "restart-name", "revappend", "reverse", "room", "round",
    "row-major-aref", "rplaca", "rplacd", "sbit", "scale-float", "schar",
    "search", "second", "set", "set-difference",
    "set-dispatch-macro-character", "set-exclusive-or",
    "set-macro-character", "set-pprint-dispatch", "set-syntax-from-char",
    "seventh", "shadow", "shadowing-import", "shared-initialize",
    "short-site-name", "signal", "signum", "simple-bit-vector-p",
    "simple-condition-format-arguments", "simple-condition-format-control",
    "simple-string-p", "simple-vector-p", "sin", "sinh", "sixth", "sleep",
    "slot-boundp", "slot-exists-p", "slot-makunbound", "slot-missing",
    "slot-unbound", "slot-value", "software-type", "software-version",
    "some", "sort", "special-operator-p", "sqrt", "stable-sort",
    "standard-char-p", "store-value", "stream-element-type",
    "stream-error-stream", "stream-external-format", "streamp", "string",
    "string<", "string<=", "string=", "string>", "string>=", "string/=",
    "string-capitalize", "string-downcase", "string-equal",
    "string-greaterp", "string-left-trim", "string-lessp",
    "string-not-equal", "string-not-greaterp", "string-not-lessp",
    "stringp", "string-right-trim", "string-trim", "string-upcase",
    "sublis", "subseq", "subsetp", "subst", "subst-if", "subst-if-not",
    "substitute", "substitute-if", "substitute-if-not", "subtypep", "svref",
    "sxhash", "symbol-function", "symbol-name", "symbolp", "symbol-package",
    "symbol-plist", "symbol-value", "synonym-stream-symbol", "syntax:",
    "tailp", "tan", "tanh", "tenth", "terpri", "third",
    "translate-logical-pathname", "translate-pathname", "tree-equal",
    "truename", "truncate", "two-way-stream-input-stream",
    "two-way-stream-output-stream", "type-error-datum",
    "type-error-expected-type", "type-of", "typep", "unbound-slot-instance",
    "unexport", "unintern", "union", "unread-char", "unuse-package",
    "update-instance-for-different-class",
    "update-instance-for-redefined-class", "upgraded-array-element-type",
    "upgraded-complex-part-type", "upper-case-p", "use-package",
    "user-homedir-pathname", "use-value", "values", "values-list", "vector",
    "vectorp", "vector-pop", "vector-push", "vector-push-extend", "warn",
    "wild-pathname-p", "write", "write-byte", "write-char", "write-line",
    "write-sequence", "write-string", "write-to-string", "yes-or-no-p",
    "y-or-n-p", "zerop")

  val SPECIAL_FORMS = List[String](
    "block", "catch", "declare", "eval-when", "flet", "function", "go", "if",
    "labels", "lambda", "let", "let*", "load-time-value", "locally", "macrolet",
    "multiple-value-call", "multiple-value-prog1", "progn", "progv", "quote",
    "return-from", "setq", "symbol-macrolet", "tagbody", "the", "throw",
    "unwind-protect")

  val MACROS = List[String](
    "and", "assert", "call-method", "case", "ccase", "check-type", "cond",
    "ctypecase", "decf", "declaim", "defclass", "defconstant", "defgeneric",
    "define-compiler-macro", "define-condition", "define-method-combination",
    "define-modify-macro", "define-setf-expander", "define-symbol-macro",
    "defmacro", "defmethod", "defpackage", "defparameter", "defsetf",
    "defstruct", "deftype", "defun", "defvar", "destructuring-bind", "do",
    "do*", "do-all-symbols", "do-external-symbols", "dolist", "do-symbols",
    "dotimes", "ecase", "etypecase", "formatter", "handler-bind",
    "handler-case", "ignore-errors", "incf", "in-package", "lambda", "loop",
    "loop-finish", "make-method", "multiple-value-bind", "multiple-value-list",
    "multiple-value-setq", "nth-value", "or", "pop",
    "pprint-exit-if-list-exhausted", "pprint-logical-block", "pprint-pop",
    "print-unreadable-object", "prog", "prog*", "prog1", "prog2", "psetf",
    "psetq", "push", "pushnew", "remf", "restart-bind", "restart-case",
    "return", "rotatef", "setf", "shiftf", "step", "time", "trace", "typecase",
    "unless", "untrace", "when", "with-accessors", "with-compilation-unit",
    "with-condition-restarts", "with-hash-table-iterator",
    "with-input-from-string", "with-open-file", "with-open-stream",
    "with-output-to-string", "with-package-iterator", "with-simple-restart",
    "with-slots", "with-standard-io-syntax")

  val LAMBDA_LIST_KEYWORDS = List[String](
    "&allow-other-keys", "&aux", "&body", "&environment", "&key", "&optional",
    "&rest", "&whole")

  val DECLARATIONS = List[String](
    "dynamic-extent", "ignore", "optimize", "ftype", "inline", "special",
    "ignorable", "notinline", "type")

  val BUILTIN_TYPES = List[String](
    "atom", "boolean", "base-char", "base-string", "bignum", "bit",
    "compiled-function", "extended-char", "fixnum", "keyword", "nil",
    "signed-byte", "short-float", "single-float", "double-float", "long-float",
    "simple-array", "simple-base-string", "simple-bit-vector", "simple-string",
    "simple-vector", "standard-char", "unsigned-byte",

    // Condition Types
    "arithmetic-error", "cell-error", "condition", "control-error",
    "division-by-zero", "end-of-file", "error", "file-error",
    "floating-point-inexact", "floating-point-overflow",
    "floating-point-underflow", "floating-point-invalid-operation",
    "parse-error", "package-error", "print-not-readable", "program-error",
    "reader-error", "serious-condition", "simple-condition", "simple-error",
    "simple-type-error", "simple-warning", "stream-error", "storage-condition",
    "style-warning", "type-error", "unbound-variable", "unbound-slot",
    "undefined-function", "warning")

  val BUILTIN_CLASSES = List[String](
    "array", "broadcast-stream", "bit-vector", "built-in-class", "character",
    "class", "complex", "concatenated-stream", "cons", "echo-stream",
    "file-stream", "float", "function", "generic-function", "hash-table",
    "integer", "list", "logical-pathname", "method-combination", "method",
    "null", "number", "package", "pathname", "ratio", "rational", "readtable",
    "real", "random-state", "restart", "sequence", "standard-class",
    "standard-generic-function", "standard-method", "standard-object",
    "string-stream", "stream", "string", "structure-class", "structure-object",
    "symbol", "synonym-stream", "t", "two-way-stream", "vector")

  override val name = "Common Lisp"
  override val aliases = "common-lisp" :: "cl" :: Nil
  override val fileNames = "*.cl" :: "*.lisp" :: "*.el" :: Nil
  override val mimeTypes = "text/x-common-lisp" :: Nil
  override val flags = Pattern.CASE_INSENSITIVE | Pattern.MULTILINE

  // couple of useful regexes
  // characters that are not macro-characters and can be used to begin a symbol
  val nonmacro = """\\.|[a-zA-Z0-9!$%&*+-/<=>?@\[\]^_{}~]"""
  val constituent = nonmacro + """|[#.:]"""
  val terminated = """(?=[ "()\'\n,;`])""" // whitespace or terminating macro characters

  // symbol token, reverse-engineered from hyperspec
  // Take a deep breath...
  val symbol = """(\|[^|]+\||(?:%s)(?:%s)*)""" format (nonmacro, constituent)

  override def getTokensUnprocessed(text: String) = {
    super.getTokensUnprocessed(text, "root" :: Nil).map { t =>
      {
        val (index, token, value) = t
        if (token == Name.Variable)
          if (BUILTIN_FUNCTIONS.contains(value))
            (index, Name.Builtin, value)

          else if (SPECIAL_FORMS.contains(value))
            (index, Keyword, value)

          else if (MACROS.contains(value))
            (index, Name.Builtin, value)

          else if (LAMBDA_LIST_KEYWORDS.contains(value))
            (index, Keyword, value)

          else if (DECLARATIONS.contains(value))
            (index, Keyword, value)

          else if (BUILTIN_TYPES.contains(value))
            (index, Keyword.Type, value)

          else if (BUILTIN_CLASSES.contains(value))
            (index, Name.Class, value)
          else t

        else t
      }
    }

  }

  val tokens = Map[String, StateDef](
    ("root", List[Definition](
      ("", Text) >> "body")),
    ("multiline-comment", List[Definition](
      ("""#\|""", Comment.Multiline) >> Push,
      ("""\|#""", Comment.Multiline) >> Pop,
      ("""[^|#]+""", Comment.Multiline),
      ("""[|#]""", Comment.Multiline))),
    ("commented-form", List[Definition](
      ("""\(""", Comment.Preproc) >> Push,
      ("""\)""", Comment.Preproc) >> Pop,
      ("""[^()]+""", Comment.Preproc))),
    ("body", List[Definition](
      // whitespace
      ("""\s+""", Text),

      // single-line comment
      (""";.*$""", Comment.Single),

      // multi-line comment
      ("""#\|""", Comment.Multiline) >> "multiline-comment",

      // encoding comment (?)
      ("""#\d*Y.*$""", Comment.Special),

      // strings and characters
      (""""(\\.|[^"\\])*"""", Str),
      // quoting
      (":" + symbol, Str.Symbol),
      ("'" + symbol, Str.Symbol),
      ("'", Operator),
      ("`", Operator),

      // decimal numbers
      ("""[-+]?\d+\.?""" + terminated, pyments.Number.Integer),
      ("""[-+]?\d+/\d+""" + terminated, pyments.Number),
      ("""[-+]?(\d*\.\d+([defls][-+]?\d+)?|\d+(\.\d*)?[defls][-+]?\d+)""" + terminated, pyments.Number.Float),

      // sharpsign strings and characters
      ("""#\\.""" + terminated, Str.Char),
      ("""#\\""" + symbol, Str.Char),

      // vector
      ("""#\(""", Operator) >> "body",

      // bitstring
      ("""#\d*\*[01]*""", Literal),

      // uninterned symbol
      ("""#:""" + symbol, Str.Symbol),

      // read-time and load-time evaluation
      ("""#[.,]""", Operator),

      // function shorthand
      ("""#\'""", Name.Function),

      // binary rational
      ("""#[bB][+-]?[01]+(/[01]+)?""", pyments.Number),

      // octal rational
      ("""#[oO][+-]?[0-7]+(/[0-7]+)?""", pyments.Number.Oct),

      // hex rational
      ("""#[xX][+-]?[0-9a-fA-F]+(/[0-9a-fA-F]+)?""", pyments.Number.Hex),

      // radix rational
      ("""#\d+[rR][+-]?[0-9a-zA-Z]+(/[0-9a-zA-Z]+)?""", pyments.Number),

      // complex
      ("""(#[cC])(\()""", ByGroups(pyments.Number, Punctuation)) >> "body",

      //array
      ("""(#\d+[aA])(\()""", ByGroups(Literal, Punctuation)) >> "body",

      // structure
      ("""(#[sS])(\()""", ByGroups(Literal, Punctuation)) >> "body",

      //path
      ("""#[pP]?"(\\.|[^"])*"""", Literal),

      // reference
      ("""#\d+=""", Operator),
      ("""#\d+#""", Operator),

      // read-time comment
      ("""#+nil""" + terminated + """\s*\(""", Comment.Preproc) >> "commented-form",

      // read-time conditional
      ("""#[+-]""", Operator),

      // special operators that should have been parsed already
      ("""(,@|,|\.)""", Operator),

      // special constants
      ("""(t|nil)""" + terminated, Name.Constant),

      // functions and variables
      ("""\*""" + symbol + """\*""", Name.Variable.Global),
      (symbol, Name.Variable),

      // parentheses
      ("""\(""", Punctuation) >> "body",
      ("""\)""", Punctuation) >> Pop)))
}