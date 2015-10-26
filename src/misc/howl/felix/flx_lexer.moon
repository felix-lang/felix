howl.aux.lpeg_lexer ->
  c = capture
  ident = (alpha + '_')^1 * (alpha + digit + S"_-'")^0
  ws = c 'whitespace', blank^0

  identifer = c 'identifer', ident

  keyword = c 'keyword', word {
    'assert', 'axiom', 'break', 'body', 'call', 'case', 'proj', 'caseno', 'cfun',
    'class', 'comment', 'continue', 'const', 'cproc', 'cstruct', 'ctor',
    'ctypes', 'def', 'done', 'do', 'begin', 'elif', 'else', 'otherwise',
    'endcase', 'endif', 'endtry', 'endmatch', 'end', 'enum', 'cenum', 'cflags',
    'expect', 'extern', 'finaliser', 'for', 'forget', 'fork', 'functor', 'fun',
    'gen', 'goto', 'halt', 'header', 'ident', 'include', 'incomplete', 'inf',
    'instance', 'is', 'inherit', 'inline', 'in', 'jump', 'lemma', 'let', 'loop',
    'lval', 'macro', 'method', 'module', 'namespace', 'NaN', 'new', 'noinline',
    'nonterm', 'noreturn', 'not', 'object', 'package', 'perform', 'pod',
    'private', 'proc', 'property', 'reduce', 'ref', 'rename', 'requires',
    'return', 'from', 'SCHEME', 'syntax', 'literal', 'static', 'struct', 'then',
    'todo', 'to', 'try', 'typedef', 'type', 'union', 'upto', 'use', 'val', 'var',
    'virtual', 'where', 'when', 'while', 'with', 'yield', '_gc_pointer',
    '_gc_type', '_svc', '_deref', 'and', 'as', 'callback', 'code', 'if', 'isin',
    'match', 'noexpand', 'of', 'or', 'the', 'typematch', 'typecase', 'unless',
    'open'
  }

  types = c 'type', word {
    'unit', 'void', 'bool', 'any', 'byte', 'address', 'offset', 'tiny', 'short',
    'int', 'long', 'vlong', 'utiny', 'ushort', 'uint', 'ulong', 'uvlong',
    'int8', 'int16', 'int32', 'int64', 'uint8', 'uint16', 'uint32', 'uint64',
    'size', 'ssize', 'intptr', 'uintptr', 'intmax ', 'uintmax ', 'ptrdiff',
    'offset', 'float', 'double', 'ldouble', 'char', 'wchar', 'uchar', 'string',
    'ustring', 'cont', 'array', 'varray', 'darray', 'sarray', 'bsarray',
    'carray', 'list'
  }

  typedef = sequence {
    c 'keyword', word {
      'class', 'object', 'union', 'typedef', 'interface', 'struct', 'type'
    }
    ws
    c 'type_def', ident
  }

  bool = c 'special', word { 'true', 'false' }

  fdecl = sequence {
    c 'keyword', word { 'fun', 'proc', 'gen', 'cfun', 'cproc', 'ctor' }
    ws
    c 'fdecl', ident
  }

  comment = c 'comment', any {
    P'//' * scan_until eol,
    span '/*', '*/'
  }

  operator = c 'operator', S'#+-*/%^&|!:=<>()[]{},;$'

  sq_string = span "'", "'", '\\'
  dq_string = span '"', '"', '\\'
  lsq_string = span "'''", "'''", '\\'
  ldq_string = span '"""', '"""', '\\'
  string = c 'string', any { lsq_string, ldq_string, sq_string, dq_string }

  number = c 'number', any {
    float
    hexadecimal_float
    hexadecimal
    octal
    R'19' * digit^0 * any{P"ul", S"ful"}
    R'19' * digit^0
  }

  any {
    comment
    string
    fdecl
    typedef
    bool
    keyword
    types
    identifer
    operator
    number
    identifer
  }
