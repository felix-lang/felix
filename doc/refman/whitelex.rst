Overall Lexical Structure
=========================

Felix is a free form language meaning end of lines are generally
not significant. White space can be freely inserted between significant
lexemes, and sometimes it is required to separate them correctly.

There are two forms of lexical comments. Felix allows the usual
C++ style `// to eol` comments and C style `/* to */` comments,
except that C style comments can be nested.

Continuation lines are not supported.

Felix does not have any keywords. Instead in certain contexts,
certain identifiers are treated as keywords. The same words
are ordinary identifiers in other contexts.

The Felix grammar is specified in the library, so it can
be easily extended by the programmer. When adding new grammar,
avoid misusing symbols such as `;` or `,`, as this may either
fail or make the code unreadable.

Identifiers, integer literals, floating literals, and string
literals, are also defined in the library in the grammar.

