Compilation Model
=================

The Felix translator takes several sets of files and translates them
into a set of C++ files, compiles them, and links the results
into a library.

Source File kinds
+++++++++++++++++

Grammar Files
-------------

The Felix grammar is defined in user space in the library.
Certain files, with extension ".fsyn", typically found in the
grammar subdirectory of share/lib subdirectory, are processed
primarily for grammar definitions: any generated code in these
files is simply throw out.

The files used as the base grammar are determined by scanning,
environment variables and switches, which can be used to control
the syntax of the base system.

Grammar specifications are first class parts of the language,
any user file may contain them, however they cannot be exported
in a modular fashion at this time so the compilation control
machinery is required to share the user grammar, and such
sharing is global.

Macro Files
-----------

A small number of files, usually a single file, is used to
define macros which are prefixed to every ordinary file
Felix processes. These macros are typically used to specify
the host operating systems so as to allow platform dependent
conditional compilation.

Library Files
-------------

These are files provided by the standard Felix install which
contain a mix of several kinds of entity. First and foremost,
they contain commonly used data types and functions operating
on them, such as lists, arrays, strings, regular expressions, etc.

Secondly, the Felix run time library (RTL) is written in C++ and 
the standard library contains bindings to this library.
For example the Felix garbage collector is a C++ class type
which is implemented in the RTL and there is a Felix binding
to it in the standard library which allows low level access to
some of its facilities.

Thirdly, some special operations are encoded with special techniques
in the library. For example, the Felix compiler generates, for most
heap allocated types, a special Run Time Type Information (RTTI) record.
The RTTI includes information locating where in the type pointers
are located so that the garbage collector can trace them.
The standard library provides the user access to these records
to allow some degree of introspection. In particular the RTTI provides
encoders and decoders for serialisable types which allows the 
generic serialisation functions defined in the RTL to operate.


User Files
----------

User code files are written by the programmer and typically consist
of either library definitions or program code.


Inclusion model
+++++++++++++++

Unlike C or C++, Felix does not provide a way to include text
files in others. Text file inclusion is restricted to macro
files which are specified on the command line.

Instead, Felix is designed so each user and library file
can be separately parsed, independently of any other such
file, and it can therefore cache the results of a parse.
The compiler uses dependency checking to decide if a the cache
needs to be update when the file is changed. In particular,
user and library files depend only on macro files and the
base grammar files. Since these rarely change, in general
the standard library only needs to be parsed once, and 
user program files only need to be reparsed if they're modified.

This provide significant performance advantage share by almost
all modern languages but lacking in C and C++ where parsing 
files and all their dependent include files repeatedly
for every translation unit is a serious problem in large scale
software developments.

Include Directive
-----------------

In Felix and include directive may be given by specifying
a Felix file name:

.. code-block:: felix

  include "std/datatype/list";

The file name excludes the extension of the file, and must use
Unix slash separator, even on Windows. The effect of this directive
is as follows: Felix first parses the current file, generating an abstract
syntax tree (AST), and gathers the names of the included files at
the same time.

Then, it examines the cache to see if it can find the generated
AST for the include files. If not, or, if  the cached AST is out
of data, it parses the included file recursively, updating
the cache.

The resulting AST for each include file is then prefixed to
the generated AST for the including file. An AST is not included
more than once.

Therefore, when compiling a program file the included files
end up at the top of the program file. This requires  that
any executable code in any included file must be able to
operate correctly independenly of any other included file.

Order of Initialisation
-----------------------

The only ordering guarantee offered is that the main program 
file will be executed after any variable initialisations in
included files. For example, Felix provides functions to create
an alarm clock, and it provides a default alarm clock.
The alarm clock will be initialised (if used) before the main
program starts executing.

Elision of Unused Variables
---------------------------

Felix guarrantees to elide unused variables other than
unused functional parameters. The utility of this assurance
can be seen in the following example:

Felix provides a global variable containing a thread pool 
object. Since this object is in the standard library and
will be included directly or indirectly by the mainline
program, the thread pool is automatically available if it is used.
However construction of thread pools is expensive so, if the 
global thread pool variable is not used, it is elided. 
In particular, its initialiser is elided too, and so if the
pool is not used, no threads are constructed.

The rule applies to all variables and may sometime lead
to surprises. In particular a common mistake is to write
a generator with a side-effect and store the result in a
variable, assuming the side-effect will occur. If the variable
is not used, however, it will be elided and so too the generator
application, so the side effect will be lost.

Insertion Model
+++++++++++++++

Felix uses floating insertions to include C++ source
dependencies into generated C++ code. There are two kinds
of insertion, header insertions which go near the top of
the generated header (hpp) file, and body insertions which
go near the top of the generated body (cpp) file.

Typically header insertions define type and function
interfaces whilst body file provide function definitions.

Literal insertion phrases are illustrated;

.. code-block:: felix

  header '#include "myfile.hpp"'
  body 'void f() { cout << "hello"; }'

Insertions can be tagged:

.. code-block:: felix

  header cstring_h = "#include <cstring>";

Insertions can be used as dependencies of primitive
bindings: type bindings, function and procedure bindings,
and tagged insertions, expressed by requires clauses:

.. code-block:: felix

   type string = "::std::basic_string<char"
     requires header "#include <string>"
   ;

A floating insertion tag definition may use the same
tag as another, in this case all the insertion texts
will emitted if the tag is required.

Insertion dependencies can be recursive:

.. code-block:: felix

   header one = "void f()"
     requires two
   ;
   header two = "void g()"
     requires one
   ;
   type X = "X" requires one;

The code generator finds all type and
function bindings used in the final generated code
and then finds the transitive closure of the set
of required floating insertions. Then it emits
the floating insertions in an order compatible 
with the order of writing. Duplicates are elided based
on the actual text of the insertions.

Polymorphic insertions
----------------------

Tagged floating insertions can be polymorphic.
In this case the requirement must suffix the tag
name with type arguments:

.. code-block:: felix

  proc rev[T,PLT=&list[T]] : &list[T] = "_rev($1,(?1*)0);" requires _iprev_[T,PLT];

  body _iprev_[T,PLT]=
    """
    static void _rev(?2 plt, ?1*) // second arg is a dummy
    { // in place reversal
      //struct node_t { ?1 elt; void *tail; };
      struct node_t { void *tail; ?1 elt; };
      void *nutail = 0; 
      void *cur = *plt;
      while(cur)
      {
        void *oldtail = ((node_t*)FLX_VNP(cur))->tail;   // save old tail in temp
        ((node_t*)FLX_VNP(cur))->tail = nutail;          // overwrite current node tail
        nutail = cur;                                   // set new tail to current
        cur = oldtail;                                  // set current to saved old tail
      }
      *plt = nutail;                                    // overwrite 
    }
    """
  ;






 

