Parsing
=======

Overview
--------

Unlike most programming languages, Felix has tiny bootstrap grammar hard coded
into the compiler. That grammar does not specify the commonly used part of
the language, instead, it specifies an how to parse an EBNF like syntax which
specifies the actual grammar.

The parser starts by reading these EBNF specifications which are part of
the standard library in files ending in the extension `.fsyn`. It translates
these specifications into an internal data structure. The specifications
look like this:

.. code-block:: felix

  syntax blocks
  {
    stmt = block;
    block := "do" stmt* "done" =># '`(ast_seq ,_sr ,_2)';
    block := "begin" stmt* "end" =># '(block _2)';
    block := "perform" stmt =># '_2';
  }

The basic form of a grammar rule consists of:

* a nonterminal on the left
*  an `:=` symbol,
* a production, which is a regular expression like specification involving 
** quoted strings, which are treated as terminals, 
** identifiers, which are nonterminals,
** and the symbols `*` for the Kleene closure of 0 or more repetitions,
** `+` for 1 or more repetitions, 
** `?` for 0 or 1 repetition, 
** and parenthesis `(` and `)` for grouping. 
* followed by a `=>#` symbol, then
* a string in R5RS Scheme call the action
* and finally a terminating semicolon `;`.

The `syntax` group is called a DSSL or domain specific sublanguage.

Then the parser hits a statement like this:

.. code-block:: felix

  open syntax blocks;

and this is where the fun starts. The parser takes the DSSL specified
and *augments* the current parser with the specified syntax.
After all the extensions so specified a special command
causes the parser to save the extended automaton to disk as a cache:
dependency checking allows us to skip the above process if the grammar
is unmodified and load the automaton immediately.

The parser is now ready to process actual Felix files.
When it parses a string according to a grammar rule it
sets the variables `_1`, `_2`, `_3` etc to the abstracted
syntax trees previously generated and then executes the Scheme
in the action code. This results in a Scheme S-expression being
returned. Note that the Scheme code is compiled and the compiled
code saved, so that the code is not interpreted each time.

After the whole file is parsed, the resuling Scheme S-expression
is translated to a simpler kind of S-expression which is easier
to pattern match against in Ocaml, and then that S-expression
is translated into the first stage syntax tree using Ocaml
variants which are required for the compiler. Subsequent
processing stages macro process and desugar the syntax tree to another form.

The result of parsing each file is saved in the cache so if the file
and the grammar is not changed, the AST can be loaded from the cache.
Therefore Felix usually only parse the files you're actively working on.
Felix is specifically designed so that files can be parsed independently
of eaach other to minnimise parsing time.

The parser used is Dypgen, and the Scheme used is OCS Scheme.
Dypgen is a GLR+ parser. The automaton has an LALR1 kernel
and it resolves shift/reduce and reduce/reduce conflicts by
following both alternatives simultaneously; each path is
called a parser thread. Later, a parser thread may fail
reducing the number of threads being executed, and the
parser can also merge threads if they produced the same
result a different way. The parser also has a number of
other features including the ability of the action code
to kill a thread by throwing an exception.

Syntax specifications are part of the Felix language.
They can be used anywhere in Felix files, and they even
respect scopes. However *only* files processed in the initial 
grammar processing phase can be shared at this time.

