package pyments.lexer

import pyments.{ Text, LexerOptions, Lexer }
import pyments._

/**
 * "Null" pyments.lexer, doesn't highlight anything.
 */
class TextLexer(val options: LexerOptions) extends Lexer {
  override val name = "Text only"

  override val aliases = List("text")

  override val fileNames = List("*.txt")

  override val mimeTypes = List("text/plain")

  override def getTokensUnprocessed(text: String) = List((0, Text, text))
}
/*
import org.apache.commons.compress.compressors._
abstract class CompressionAlgorithm(val name: String)

object BZIP2 extends CompressionAlgorithm(CompressorStreamFactory.BZIP2)

object GZIP extends CompressionAlgorithm(CompressorStreamFactory.GZIP)

object NONE extends CompressionAlgorithm("")

*/
/**
 * Recreate a token stream formatted with the 'RawTokenFormatter'.  This
 * pyments.lexer raises exceptions during parsing if the token stream in the
 * file is malformed.
 *
 * Additional options accepted:
 *
 * 'compress'
 * If set to ''"gz"'' or ''"bz2"'', decompress the token stream with
 * the given compression algorithm before lexing (default: ''""'').
 */
/*
class RawTokenLexer(options: LexerOptions, compress: CompressionAlgorithm = NONE) extends Lexer(options) {

    override val name = "Raw token data"
    override val aliases = "raw" :: Nil
    override val mimetypes = "application/x-pygments-tokens" :: Nil

    def getTokens(is: InputStream) = {
 		val in = (new CompressorStreamFactory).createCompressorInputStream(compress, is)
 		val output = new ByteArrayOutputStream
 		IOUtils.copy(in, new BufferedOutputStream(output))
 		in.close

 		getTokens(output.toString(options.encoding))
    }

    override private def getTokens(t: String) = {
        // do not call Lexer.getTokens() because we do not want Unicode
        // decoding to occur, and stripping is not optional.
        var text = t.stripLineEnd.reverse.stripLineEnd.reverse + "\n"
        getTokensUnprocessed(text).map(t => (t._2, t._3))
    }

    override private def getTokensUnprocessed(text: java.lang.String) = {
        val length = 0
        for{line <- text linesWithSeparators
        	 splitted = line.split("\t").toList
        	 ttypestr = splitted.head
        	 value = splitted.tail.mkString("\t")
	        } {



            var ttype = _ttype_cache.getOrElse(ttypestr, {
            	ttype = Token
                val ttypes = ttypestr.split("\\.").toList.tail.foreach{ttype_ =>
                	if not ttype_ or not ttype_[0].isupper():
                            raise ValueError('malformed token name')
                        ttype = getattr(ttype, ttype_)
                }
                _ttype_cache(ttypestr) = ttype


            })
            //val = val[2:-2].decode('unicode-escape') WHAT IS IT???


            yield (length, ttype, value)
            length += value.length
        }

    }
}

private object _ttype_cache extends HashMap[String, Token]
    */ 