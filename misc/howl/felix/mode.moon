{
  lexer: bundle_load 'lexer'

  comment_syntax: { '/*', '*/' }

  auto_pairs:
    '(': ')'
    '[': ']'
    '{': '}'
    '"': '"'
    "'": "'"

  indentation:
    more_after: {r'(^\\s*|\\s+)({|do)\\s*$'}
    less_for: {r'^\\s+(}|done)\\s*$'}

  code_blocks:
    multiline: {
      {r'(^\\s*|\\s+)do\\s*$', '^%s*done', 'done'}
      {r'(^\\s*|\\s+){\\s*$', '^%s*}', '}'}
    }
}
