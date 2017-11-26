# -*- coding: utf-8 -*-
"""
    pygments.lexers.felix
    ~~~~~~~~~~~~~~~~~~~~~

    Lexer for the Felix language.

    :copyright: Copyright 2006-2014 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from pygments.lexer import RegexLexer, include, bygroups, default, words, \
    combined
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Generic, Token

__all__ = ['XFelixLexer','FelixLexer']

XMath = Token.XMath

class FelixBase(RegexLexer):

    keywords = (
        '_', '_deref', 'all', 'as','begin','end','while','connector','pin',
        'circuit','endciruit','connect','wire',
        'assert', 'attempt', 'call', 'callback', 'case', 'caseno', 'cclass',
        'code', 'compound', 'ctypes', 'do', 'done', 'downto', 'elif', 'else',
        'endattempt', 'endcase', 'endif', 'endmatch', 'enum', 'except',
        'exceptions', 'expect', 'finally', 'for', 'forall', 'forget', 'fork',
        'functor', 'goto', 'ident', 'if', 'incomplete', 'inherit', 'instance',
        'interface', 'jump', 'lambda', 'loop', 'match', 'module', 'namespace',
        'new', 'noexpand', 'nonterm', 'obj', 'of', 'open', 'parse', 'raise',
        'regexp', 'reglex', 'regmatch', 'rename', 'return', 'the', 'then',
        'to', 'type', 'typecase', 'typedef', 'typematch', 'typeof', 'upto',
        'when', 'whilst', 'with', 'yield',
        'circuit','endcircuit','connect','wire','connector','pin'
    )

    keyword_directives = (
        '_gc_pointer', '_gc_type', 'body', 'comment', 'const', 'export',
        'header', 'inline', 'lval', 'macro', 'noinline', 'noreturn',
        'package', 'private', 'pod', 'property', 'public', 'publish',
        'requires', 'todo', 'virtual', 'use',
    )

    keyword_declarations = (
        'def', 'let', 'ref', 
    )

    keyword_types = (
        'unit', 'void', 'any', 'bool',
        'byte',  'offset',
        'address', 'caddress', 'cvaddress', 'vaddress',
        'tiny', 'short', 'int', 'long', 'vlong',
        'utiny', 'ushort', 'vshort', 'uint', 'ulong', 'uvlong',
        'int8', 'int16', 'int32', 'int64',
        'uint8', 'uint16', 'uint32', 'uint64',
        'float', 'double', 'ldouble',
        'complex', 'dcomplex', 'lcomplex',
        'imaginary', 'dimaginary', 'limaginary',
        'char', 'wchar', 'uchar',
        'charp', 'charcp', 'ucharp', 'ucharcp',
        'string', 'wstring', 'ustring',
        'cont',
        'array', 'varray', 'list',
        'lvalue', 'opt', 'slice',
    )

    keyword_constants = (
        'false', 'true',
    )

    name_builtins = (
    )

    name_pseudo = (
        'root', 'self', 'this',
    )

    decimal_suffixes = '((u?([tTsSiIlLvVzpj]|ll|LL))|([iIuU])(8|16|32|64))?'

    tokens = {
        'root': [
            include('whitespace'),

            # Keywords
            (words(('axiom', 'ctor', 'chip', 'fun', 'gen', 'proc', 'reduce','regdef',
              'var','val','typedef','device',
              'union'), suffix=r'\b'),
             Keyword, 'funcname'),
            (words(('class', 'cclass', 'cstruct', 'obj', 'struct', 'object'), suffix=r'\b'),
             Keyword, 'funcname'),
            (r'(module|typeclass|interface)\b', Keyword, 'funcname'),

            (words(keywords, suffix=r'\b'), Keyword),
            (words(keyword_directives, suffix=r'\b'), Name.Decorator),
            (words(keyword_declarations, suffix=r'\b'), Keyword.Declaration),
            (words(keyword_types, suffix=r'\b'), Keyword.Type),
            (words(keyword_constants, suffix=r'\b'), Keyword.Constant),

            # Operators
            include('operators'),

            # Float Literal
            # -- Hex Float
            (r'0[xX]([0-9a-fA-F_]*\.[0-9a-fA-F_]+|[0-9a-fA-F_]+)'
             r'[pP][+\-]?[0-9_]+[lLfFdD]?', Number.Float),
            # -- DecimalFloat
            (r'[0-9_]+(\.[0-9_]+[eE][+\-]?[0-9_]+|'
             r'\.[0-9_]*|[eE][+\-]?[0-9_]+)[lLfFdD]?', Number.Float),
            (r'\.(0|[1-9][0-9_]*)([eE][+\-]?[0-9_]+)?[lLfFdD]?',
             Number.Float),

            # IntegerLiteral
            # -- Binary
            (r'0[Bb][01_]+%s' % decimal_suffixes, Number.Bin),
            # -- Octal
            (r'0[oO][0-7_]+%s' % decimal_suffixes, Number.Oct),
            # -- Hexadecimal
            (r'0[xX][0-9a-fA-F_]+%s' % decimal_suffixes, Number.Hex),
            # -- Decimal
            (r'0[dD][0-9_]+%s' % decimal_suffixes, Number.Integer),
            (r'[0-9][0-9_]*%s' % decimal_suffixes, Number.Integer),

            # Strings
            ('([rR][cC]?|[cC][rR])"""', String, 'tdqs'),
            ("([rR][cC]?|[cC][rR])'''", String, 'tsqs'),
            ('([rR][cC]?|[cC][rR])"', String, 'dqs'),
            ("([rR][cC]?|[cC][rR])'", String, 'sqs'),
            ('[cCfFqQwWuU]?"""', String, combined('stringescape', 'tdqs')),
            ("[cCfFqQwWuU]?'''", String, combined('stringescape', 'tsqs')),
            ('[cCfFqQwWuU]?"', String, combined('stringescape', 'dqs')),
            ("[cCfFqQwWuU]?'", String, combined('stringescape', 'sqs')),

            # Punctuation
            (r'[\[\]{}:(),;?]', Punctuation),

            # Labels
            (r'[a-zA-Z_]\w*:>', Name.Label),

            # Identifiers
            (r"[a-zA-Z_][a-zA-Z0-9_'-]*", Name),

            # TeX Identifiers
            (r"\\[A-Za-z]+", Generic),

            (r'\\\(.*\\\)',XMath),

        ],
        'whitespace': [
            (r'\n', Text),
            (r'\s+', Text),

            include('comment'),
        ],
        'operators': [
            (r'!=|==|<<|>>|\|\||&&|\|->|[#!~+/*%=<>&^|.$-]', Operator),
        ],
        'comment': [
            (r'//(.*?)\n', Comment.Single),
            (r'/[*]', Comment.Multiline, 'comment2'),
        ],
        'comment2': [
            (r'[^/*]', Comment.Multiline),
            (r'/[*]', Comment.Multiline, '#push'),
            (r'[*]/', Comment.Multiline, '#pop'),
            (r'[/*]', Comment.Multiline),
        ],
        'funcname': [
            include('whitespace'),
            (r'[^[( ]+', Name.Function, '#pop'),
            # anonymous functions
            (r'(?=\()', Text, '#pop'),
        ],
        'stringescape': [
            (r'\\([\\abfnrtv"\']|\n|N\{.*?\}|u[a-fA-F0-9]{4}|'
             r'U[a-fA-F0-9]{8}|x[a-fA-F0-9]{2}|[0-7]{1,3})', String.Escape)
        ],
        'strings': [
            (r'%(\([a-zA-Z0-9]+\))?[-#0 +]*([0-9]+|[*])?(\.([0-9]+|[*]))?'
             '[hlL]?[diouxXeEfFgGcrs%]', String.Interpol),
            (r'[^\\\'"%\n]+', String),
            # quotes, percents and backslashes must be parsed one at a time
            (r'[\'"\\]', String),
            # unhandled string formatting sign
            (r'%', String)
            # newlines are an error (use "nl" state)
        ],
        'nl': [
            (r'\n', String)
        ],
        'dqs': [
            (r'"', String, '#pop'),
            # included here again for raw strings
            (r'\\\\|\\"|\\\n', String.Escape),
            include('strings')
        ],
        'sqs': [
            (r"'", String, '#pop'),
            # included here again for raw strings
            (r"\\\\|\\'|\\\n", String.Escape),
            include('strings')
        ],
        'tdqs': [
            (r'"""', String, '#pop'),
            include('strings'),
            include('nl')
        ],
        'tsqs': [
            (r"'''", String, '#pop'),
            include('strings'),
            include('nl')
        ],
    }

class XFelixLexer(FelixBase):
    def get_tokens_unprocessed(self,text):
      for index,token,value in RegexLexer.get_tokens_unprocessed(self,text):
        if token == Generic:
          value = "\\(" + value + "\\)"
          yield (index,token,value)
        elif token == XMath:
          yield (index,Generic,value)
        else:
          yield (index,token,value)
      

    name = 'XFelix'
    aliases = ['xfelix', 'xflx']
    filenames = ['*.flx', '*.flxh']
    mimetypes = ['text/x-felix']

class FelixLexer(FelixBase):
    def get_tokens_unprocessed(self,text):
      for index,token,value in RegexLexer.get_tokens_unprocessed(self,text):
        if token == Generic:
          yield (index,token,value)
        elif token == XMath:
           yield (index,Punctuation,u"\\")
           yield (index,Punctuation,u"(")
           yield (index,Generic,value[2:-2])
           yield (index,Punctuation,u"\\")
           yield (index,Punctuation,u")")
        else:
          yield (index,token,value)

    name = 'Felix'
    aliases = ['felix', 'flx']
    filenames = ['*.flx', '*.flxh']
    mimetypes = ['text/x-felix']

 
