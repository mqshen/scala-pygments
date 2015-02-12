package pyments

trait Util {
  val split_path_re = """[/\\ ]""".r

  val doctype_lookup_re = """(?smx)
    (<\?.*?\?>)?\s*
    <!DOCTYPE\s+(
     [a-zA-Z_][a-zA-Z0-9]*\s+
     [a-zA-Z_][a-zA-Z0-9]*\s+
     "[^"]*")
     [^>]*>
""".r

  val tag_re = """<(.+?)(\s.*?)?>.*?</.+?>(?uism)""".r

  /**
   * If one of the get_*_by_* functions didn't find a matching class.
   */
  /*class ClassNotFound extends Exception
	class OptionError extends Exception

	def get_choice_opt(options: Map[String, Any], optname: String, allowed: List[String], default: Any = null, normcase:Boolean = False) = {
		var string = options.getOrElse(optname, default)
	    if(normcase) string = string.toLowerCase
	    if(!allowed.contains(string)) throw new OptionError("Value for option %s must be one of %s".format(optname, allowed.mkString(", ")))
	    string
	}

	def get_bool_opt(options: Map[String, Any], optname: String, default: Any = null) = {
		var string = options.getOrElse(optname, default)
		if(string.isInstanceOf[Boolean]) string.asInstanceOf[Boolean]
		else if (string.isInstanceOf[Int]) string.asInstanceOf[Int] == 0
    	else if (!string.isInstanceOf[String]) throw new OptionError("Invalid type %r for option %s; use 1/0, yes/no, true/false, on/off".format(string, optname))
    	else if (List("1", "yes", "true", "on").contains(string.toLowerCase)) true
    	else if (List("0", "no", "false", "off").contains(string.toLowerCase)) false
    	else throw new OptionError("Invalid type %r for option %s; use 1/0, yes/no, true/false, on/off".format(string, optname))
	}
    

    */

}