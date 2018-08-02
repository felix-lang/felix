Program Environment
===================

Environment Variables
---------------------

.. code-block:: felix

  class Env {
    fun pathsep: 1 -> string

    //$ Get the value of a given variable.
    //$ Returns empty string if the variable doesn't exist.
    fun getenv:string -> string

    fun issetenv(s:string)=> getenv s != "";

    //$ Get the value of a given variable.
    //$ Returns specified default if the variable doesn't exist.
    fun getenv(name:string,dflt:string):string

    fun getenvlist (name:string) : list[string]
  }

Command Line Arguments
----------------------

.. code-block:: felix

  class System {
    const argc:int
    const _argv:&&char

    fun argv:int -> string
    fun argv_dflt (x:int) (y:string) 

    fun args () => List::map (argv) (List::range argc);

    proc setargs : + (+char) * size 
    proc setargs[N] (a:string^N) 

    gen system (cmd:string) : int => Shell::system(cmd);

    gen exit: int -> any = '::std::exit($1)'

    gen abort: 1 -> any = 
      '(fprintf(stderr,"Felix code calling abort\\n"),::std::abort())' 

    _gc_pointer type ptf_t = "thread_frame_t*";

    const ptf:ptf_t = "ptf" 

    proc pexit: int
  }

Shell
=====

.. code-block:: felix

  class Shell {

    // quote a single argument
    fun quote_arg(s:string):string

    // quote a list of arguments
    fun quote_args (s:list[string]) : string 
    
    // quote a line for a system call
    fun quote_line_for_system(s:string)

    // quote a line for a popen call
    fun quote_line_for_popen(s:string)

    // raw system call
    gen raw_system: string -> int = "::std::system($1.c_str())"


    // parse shell string into arguments
    fun parse (s:string) : list[string]

    // basic command with line quoting.
    gen basic_system (cmd: string) :int 

    // string argument, traced
    gen system (cmd:string) = {
      if Env::getenv "FLX_SHELL_ECHO" != "" do
        eprintln$ "[system] " + cmd;
      done
      return basic_system cmd;
    }

    // system call with list of string arguments
    gen system (args:list[string]) : int 

    // system call with iteratable data type for arguments
    gen system[T with Iterable[T,string]] (args:T) : int =

    // raw popen call
    gen raw_get_stdout(x:string)

    // popen, capturing standard output
    gen basic_get_stdout (cmd: string) : int * string =>

    // popen with string, traced
    gen get_stdout (cmd:string) : int * string = {
      if Env::getenv "FLX_SHELL_ECHO" != "" do
        eprintln$ "[get_stdout] " + cmd;
      done
      return basic_get_stdout cmd;
    }

    // popen with argument list
    gen get_stdout (args:list[string]) : int * string  

    // popen with iterable 
    gen get_stdout[T with Iterable[T,string]] (args:T) : int * string
