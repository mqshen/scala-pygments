import org.scalatest._
import pyments.LexerOptions

import pyments._
import pyments.lexer._

class LexerSpec extends FlatSpec with Matchers{

var sql = """SELECT * FROM TABLE WHERE a < 10 GROUP BY b HAVING b < 5 ORDER BY 1 desc"""

    /*"Sql Lexer" should {
      "parse sql" in {
        (new pyments.lexer.SqlLexer(new LexerOptions)).getTokens(sql) must contain((Keyword,"SELECT"), (Text," "), (Operator,"*"), (Text," "), (Keyword,"FROM"), (Text," "), (Keyword,"TABLE"), (Text," "), (Keyword,"WHERE"), (Text," "), (Name,"a"), (Text," "), (Operator,"<"), (Text," "), (Number.Integer,"10"), (Text," "), (Keyword,"GROUP"), (Text," "), (Keyword,"BY"), (Text," "), (Name,"b"), (Text," "), (Keyword,"HAVING"), (Text," "), (Name,"b"), (Text," "), (Operator,"<"), (Text," "), (Number.Integer,"5"), (Text," "), (Keyword,"ORDER"), (Text," "), (Keyword,"BY"), (Text," "), (Number.Integer,"1"), (Text," "), (Keyword,"desc"), (Text,"")).only.inOrder
      }
    }*/

    var scala = """
    package filter

import pyments._


/*
"Null" pyments.lexer, doesn't highlight anything.
*/
class TextLexer(val options: LexerOptions) extends Lexer {
  override val name = "Text only"

  override val aliases = List("text")

  override val filenames = List("*.txt")

  override val mimetypes = List("text/plain")

    override def getTokensUnprocessed(text: String) = List((0, Text, text))
}
    """

   /* "ScalaLexer" should {
      "parse scala code" in {
        val l = new pyments.lexer.ScalaLexer(new LexerOptions)
       l.getTokens(scala) === Nil
      }
    }
*/
    val c = """
    #ifdef WITH_THREAD

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#include "pythread.h"

static PyThread_type_lock interpreter_lock = 0; /* This is the GIL */
static long main_thread = 0;

int
PyEval_ThreadsInitialized(void)
{
  return interpreter_lock != 0;
}

void
PyEval_InitThreads(void)
{
  if (interpreter_lock)
    return;
  interpreter_lock = PyThread_allocate_lock();
  PyThread_acquire_lock(interpreter_lock, 1);
  main_thread = PyThread_get_thread_ident();
}

void
PyEval_AcquireLock(void)
{
  PyThread_acquire_lock(interpreter_lock, 1);
}

void
PyEval_ReleaseLock(void)
{
  PyThread_release_lock(interpreter_lock);
}

void
PyEval_AcquireThread(PyThreadState *tstate)
{
  if (tstate == NULL)
    Py_FatalError("PyEval_AcquireThread: NULL new thread state");
  /* Check someone has called PyEval_InitThreads() to create the lock */
  assert(interpreter_lock);
  PyThread_acquire_lock(interpreter_lock, 1);
  if (PyThreadState_Swap(tstate) != NULL)
    Py_FatalError(
      "PyEval_AcquireThread: non-NULL old thread state");
}

void
PyEval_ReleaseThread(PyThreadState *tstate)
{
  if (tstate == NULL)
    Py_FatalError("PyEval_ReleaseThread: NULL thread state");
  if (PyThreadState_Swap(NULL) != tstate)
    Py_FatalError("PyEval_ReleaseThread: wrong thread state");
  PyThread_release_lock(interpreter_lock);
}

/* This function is called from PyOS_AfterFork to ensure that newly
   created child processes don't hold locks referring to threads which
   are not running in the child process.  (This could also be done using
   pthread_atfork mechanism, at least for the pthreads implementation.) */

    """

    "CLexer" should "parse c code" in {
      val l = new pyments.lexer.CLexer(new LexerOptions)
      val test = l.getTokens(c)
      println(test)
      test.map { case (token, string) =>
        println(token)
        println(string)
      }
      l.getTokens(c) === Nil
    }

}