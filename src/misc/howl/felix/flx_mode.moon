{
  lexer: bundle_load 'flx_lexer'

  comment_syntax: { '/*', '*/' }

  auto_pairs:
    '(': ')'
    '[': ']'
    '{': '}'
    '"': '"'
    "'": "'"

  indentation:
    more_after: {r'(^\\s*|\\s+)({|do|begin)\\s*$'}
    less_for: {r'^\\s+(}|done|end)\\s*$'}

  code_blocks:
    multiline: {
      {r'(^\\s*|\\s+)do\\s*$', '^%s*done', 'done'}
      {r'(^\\s*|\\s+)begin\\s*$', '^%s*end', 'end'}
      {r'(^\\s*|\\s+){\\s*$', '^%s*}', '}'}
    }
}
