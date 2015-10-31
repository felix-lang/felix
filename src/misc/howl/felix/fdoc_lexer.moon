howl.aux.lpeg_lexer ->
  c = capture
  at = c 'operator', '@'
  kw = (t) -> c 'keyword', t

  tangled = sequence {
    line_start
    (at * kw'felix') + (at * kw'tangle') * ' ' * scan_until eol
    sub_lex 'felix', line_start * '@'
  }

  keyword = at * kw word { 'h1', 'h2', 'h3', 'expect', 'pre', 'tangler', 'title' }

  operator = c 'operator', S'@<>'

  any {
    tangled
    keyword
    operator
  }
