howl.aux.lpeg_lexer ->
  c = capture
  ident = (alpha + '_')^1 * (alpha + digit + "_'-")^0

  identifer = c 'identifer', ident

  keyword = c 'keyword', word {
    'endtry', 'end', 'try', 'assert', 'axiom', 'body', 'call', 'case', 'proj',
    'caseno', 'cfun', 'class', 'comment', 'continue', 'const', 'cproc',
    'cstruct', 'ctor', 'ctypes', 'def', 'done', 'do', 'begin', 'elif', 'else',
    'otherwise', 'endcase', 'endif', 'endmatch', 'enum', 'cenum', 'cflags',
    'expect', 'extern', 'for', 'forget', 'fork', 'functor', 'fun', 'gen',
    'goto', 'halt', 'header', 'ident', 'include', 'incomplete', 'inf', 'in',
    'instance', 'is', 'inherit', 'inline', 'jump', 'lemma', 'let', 'loop',
    'lval', 'macro', 'module', 'namespace', 'NaN', 'new', 'noinline', 'nonterm',
    'noreturn', 'not', 'package', 'pod', 'private', 'proc', 'property',
    'reduce', 'ref', 'rename', 'requires', 'return', 'from', 'SCHEME', 'syntax',
    'literal', 'static', 'struct', 'then', 'todo', 'to', 'typedef', 'type',
    'union', 'use', 'val', 'var', 'virtual', 'where', 'when', 'with', 'yield',
    '_gc_pointer', '_gc_type', '_svc', '_deref', 'and', 'as', 'callback',
    'code', 'false', 'if', 'isin', 'match', 'noexpand', 'of', 'or', 'the',
    'true', 'typematch', 'typecase', 'unless'
  }

  types = c 'function', word {
    'unit', 'void', 'bool', 'any', 'byte', 'address', 'offset', 'tiny', 'short',
    'int', 'long', 'vlong', 'utiny', 'ushort', 'uint', 'ulong', 'uvlong',
    'int8', 'int16', 'int32', 'int64', 'uint8', 'uint16', 'uint32', 'uint64',
    'size', 'ssize', 'intptr', 'uintptr', 'intmax ', 'uintmax ', 'ptrdiff',
    'offset', 'float', 'double', 'ldouble', 'char', 'wchar', 'uchar', 'string',
    'ustring', 'cont', 'array', 'varray', 'darray', 'sarray', 'bsarray',
    'carray', 'list'
  }

  comment = c 'comment', any {
    P'//' * scan_until eol,
    span '/*', '*/'
  }

  operator = c 'operator', S'#+-*/%^&|!:=<>()[]{},;$'

  sq_string = span("'", "'", '\\')
  dq_string = span('"', '"', '\\')
  lsq_string = span("'''", "'''", '\\')
  ldq_string = span('"""', '"""', '\\')
  string = c 'string', any { lsq_string, ldq_string, sq_string, dq_string }

  number = c 'number', any {
    float,
    hexadecimal_float,
    hexadecimal,
    octal,
    R'19' * digit^0 * any{P"ul", S"ful"},
    R'19' * digit^0
  }

  any {
    comment,
    string,
    keyword,
    types,
    identifer,
    operator,
    number,
    identifer
  }
