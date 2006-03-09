#line 57 "interscript/src/python_tokeniser.ipk"
__version__ = "Ka-Ping Yee 1997/10/26; GvR 1998/3/20, Skaller 1998/11/21"

import string, re
from token import *

COMMENT = N_TOKENS
tok_name[COMMENT] = 'COMMENT'

NL = N_TOKENS + 1
tok_name[NL] = 'NL'

WHITESPACE = N_TOKENS+2
tok_name[WHITESPACE] = 'WHITESPACE'

MULTILINE_STRING_FIRST = N_TOKENS+3
tok_name[MULTILINE_STRING_FIRST]= 'MULTILINE_STRING_FIRST'

MULTILINE_STRING_MIDDLE = N_TOKENS+4
tok_name[MULTILINE_STRING_MIDDLE]= 'MULTILINE_STRING_MIDDLE'

MULTILINE_STRING_LAST = N_TOKENS+5
tok_name[MULTILINE_STRING_LAST]= 'MULTILINE_STRING_LAST'

# Changes from 1.3:
#     Ignore now accepts \f as whitespace.  Operator now includes '**'.
#     Ignore and Special now accept \n or \r\n at the end of a line.
#     Imagnumber is new.  Expfloat is corrected to reject '0e4'.
# Note: to quote a backslash in a regex, it must be doubled in a r'aw' string.

def group(*choices): return '(' + string.join(choices, '|') + ')'
def any(*choices): return apply(group, choices) + '*'
def maybe(*choices): return apply(group, choices) + '?'

Whitespace = r'[ \f\t]*'
Comment = r'#[^\r\n]*'
Ignore = Whitespace + any(r'\\\r?\n' + Whitespace) + maybe(Comment)
Name = r'[a-zA-Z_]\w*'

Hexnumber = r'0[xX][\da-fA-F]*[lL]?'
Octnumber = r'0[0-7]*[lL]?'
Decnumber = r'[1-9]\d*[lL]?'
Intnumber = group(Hexnumber, Octnumber, Decnumber)
Exponent = r'[eE][-+]?\d+'
Pointfloat = group(r'\d+\.\d*', r'\.\d+') + maybe(Exponent)
Expfloat = r'[1-9]\d*' + Exponent
Floatnumber = group(Pointfloat, Expfloat)
Imagnumber = group(r'0[jJ]', r'[1-9]\d*[jJ]', Floatnumber + r'[jJ]')
Number = group(Imagnumber, Floatnumber, Intnumber)

Single = any(r"[^'\\]", r'\\.') + "'"
Double = any(r'[^"\\]', r'\\.') + '"'
Single3 = any(r"[^'\\]",r'\\.',r"'[^'\\]",r"'\\.",r"''[^'\\]",r"''\\.") + "'''"
Double3 = any(r'[^"\\]',r'\\.',r'"[^"\\]',r'"\\.',r'""[^"\\]',r'""\\.') + '"""'
Triple = group("[rR]?'''", '[rR]?"""')
String = group("[rR]?'" + any(r"[^\n'\\]", r'\\.') + "'",
               '[rR]?"' + any(r'[^\n"\\]', r'\\.') + '"')

Operator = group('\+', '\-', '\*\*', '\*', '\^', '~', '/', '%', '&', '\|',
                 '<<', '>>', '==', '<=', '<>', '!=', '>=', '=', '<', '>')
Bracket = '[][(){}]'
Special = group(r'\r?\n', r'[:;.,`]')
Funny = group(Operator, Bracket, Special)

PlainToken = group(Number, Funny, String, Name)
Token = Ignore + PlainToken

ContStr = group("[rR]?'" + any(r'\\.', r"[^\n'\\]") + group("'", r'\\\r?\n'),
                '[rR]?"' + any(r'\\.', r'[^\n"\\]') + group('"', r'\\\r?\n'))
PseudoExtras = group(r'\\\r?\n', Comment, Triple)
PseudoToken = Whitespace + group(PseudoExtras, Number, Funny, ContStr, Name)

tokenprog, pseudoprog, single3prog, double3prog = map(
    re.compile, (Token, PseudoToken, Single3, Double3))
endprogs = {"'": re.compile(Single), '"': re.compile(Double),
            "'''": single3prog, '"""': double3prog,
            "r'''": single3prog, 'r"""': double3prog,
            "R'''": single3prog, 'R"""': double3prog, 'r': None, 'R': None}

opdict = {
  '(':LPAR,
  ')':RPAR,
  '[':LSQB,
  ']':RSQB,
  ':':COLON,
  ',':COMMA,
  ';':SEMI,
  '+':PLUS,
  '-':MINUS,
  '*':STAR,
  '/':SLASH,
  '|':VBAR,
  '&':AMPER,
  '<':LESS,
  '>':GREATER,
  '=':EQUAL,
  '.':DOT,
  '%':PERCENT,
  '`':BACKQUOTE,
  '{':LBRACE,
  '}':RBRACE,
  '==':EQEQUAL,
  '!=':NOTEQUAL,
  '<>':NOTEQUAL,
  '<=':LESSEQUAL,
  '>=':GREATEREQUAL,
  '~':TILDE,
  '^':CIRCUMFLEX,
  '<<':LEFTSHIFT,
  '>>':RIGHTSHIFT,
  '**':DOUBLESTAR
  }

tabsize = 8
TokenError = 'TokenError'
def printtoken(type, token, (srow, scol), (erow, ecol), line): # for testing
    print "%d,%d-%d,%d:\t%s\t%s" % \
        (srow, scol, erow, ecol, tok_name[type], repr(token))

#line 180 "interscript/src/python_tokeniser.ipk"
def tokenise(readline,
  tokeneater=printtoken,
  squashop=1, report_comments=1, split_multiline_strings=0):
  t = python_tokeniser(squashop, report_comments, split_multiline_strings)
  line = readline()
  while line:
    t.writeline(line)
    for token in t.tokens:
      apply(tokeneater,token)
    t.tokens = []
    line = readline()
  t.writeline('')
  for token in t.tokens:
    apply(tokeneater,token)
  t.tokens = []

#line 200 "interscript/src/python_tokeniser.ipk"
namechars, numchars = string.letters + '_', string.digits

class python_tokeniser:
  def __init__(self, squashop=0, report_comments=0, split_multiline_strings=0):
    self.squashop = squashop
    self.report_comments = report_comments
    self.split_multiline_strings = split_multiline_strings
    self.reset()

  def reset(self):
    self.lnum = self.parenlev = self.continued = 0
    self.contstr, self.needcont = '', 0
    self.contline = None
    self.indents = [0]
    self.tokens = []
    self.buffer = ''

  def get_tokens(self):
    tmp = self.tokens
    self.tokens = []
    return tmp

  def tokenize(self,data):
    self.write(data)
    return self.get_tokens()

  def tokeneater(self,*args):
    self.tokens.append(args)

  def close(self):
    if self.buffer:
      self.writeline(self.buffer)
      self.buffer = ''
    self.writeline('')
    return self.get_tokens()

  def write(self,data):
    lines = string.split(data,'\n')
    if lines:
      lines[0]=lines[0]+self.buffer
      self.buffer = ''
    for line in lines[:-1]:
      self.writeline(line+'\n')
    self.buffer = lines[-1]

#line 251 "interscript/src/python_tokeniser.ipk"
  def writeline(self,line):
    lnum = self.lnum = self.lnum + 1
    pos, max = 0, len(line)
    tokeneater = self.tokeneater

    if self.contstr:                                   # continued string
        if not line:
            raise TokenError, ("EOF in multi-line string", self.strstart)
        endmatch = self.endprog.match(line)
        if endmatch:
            pos = end = endmatch.end(0)
            if self.split_multiline_strings:
              tokeneater(MULTILINE_STRING_LAST,
                line[:end], (lnum,0),(lnum,end), line)
            else:
              tokeneater(STRING, self.contstr + line[:end],
                self.strstart, (lnum, end), self.contline + line)
            self.contstr, self.needcont = '', 0
            self.contline = None
        elif self.needcont and line[-2:] != '\\\n' and line[-3:] != '\\\r\n':
            tokeneater(ERRORTOKEN, self.contstr + line,
                       self.strstart, (lnum, len(line)), self.contline)
            self.contstr = ''
            self.contline = None
            return
        else:
            self.contstr = self.contstr + line
            self.contline = self.contline + line
            if self.split_multiline_strings:
              tokeneater(MULTILINE_STRING_MIDDLE,
                line, (lnum, 0), (lnum, len(line)), line)
            return

    elif self.parenlev == 0 and not self.continued:    # new statement
        if not line: self._close(); return

        column = 0
        while pos < max:                               # measure leading whitespace
            if line[pos] == ' ': column = column + 1
            elif line[pos] == '\t': column = (column/tabsize + 1)*tabsize
            elif line[pos] == '\f': column = 0
            else: break
            pos = pos + 1
        if pos == max: self._close(); return           # omitted newline

        if line[pos] in '#\r\n':                       # skip comments or blank lines
            if self.report_comments:
              tokeneater((NL, COMMENT)[line[pos] == '#'], line[pos:],
                       (lnum, pos), (lnum, len(line)), line)
            return

        if column > self.indents[-1]:                  # count indents or dedents
            self.indents.append(column)
            tokeneater(INDENT, line[:pos], (lnum, 0), (lnum, pos), line)
        while column < self.indents[-1]:
            self.indents = self.indents[:-1]
            tokeneater(DEDENT, '', (lnum, pos), (lnum, pos), line)

    else:                                              # continued statement
        if not line:
            raise TokenError, ("EOF in multi-line statement", (lnum, 0))
        self.continued = 0

    while pos < max:
        pseudomatch = pseudoprog.match(line, pos)
        if pseudomatch:                                # scan for tokens
            start, end = pseudomatch.span(1)
            spos, epos, pos = (lnum, start), (lnum, end), end
            token, initial = line[start:end], line[start]

            if initial in numchars \
                or (initial == '.' and token != '.'):  # ordinary number
                tokeneater(NUMBER, token, spos, epos, line)
            elif initial in '\r\n':
                if self.parenlev == 0:
                  tokeneater(NEWLINE, token, spos, epos, line)
                elif self.report_comments:
                  tokeneater(NL, token, spos, epos, line)

            elif initial == '#':
                if self.report_comments:
                  tokeneater(COMMENT, token, spos, epos, line)
            elif token in ("'''", '"""',               # triple-quoted
                           "r'''", 'r"""', "R'''", 'R"""'):
                self.endprog = endprogs[token]
                endmatch = self.endprog.match(line, pos)
                if endmatch:                           # all on one line
                    pos = endmatch.end(0)
                    token = line[start:pos]
                    tokeneater(STRING, token, spos, (lnum, pos), line)
                else:
                    if self.split_multiline_strings:
                      token = line[start:]
                      tokeneater(MULTILINE_STRING_FIRST,
                        token, spos, (lnum, len(line)), line)
                    self.strstart = (lnum, start)    # multiple lines
                    self.contstr = line[start:]
                    self.contline = line
                    break
            elif initial in ("'", '"') or \
                token[:2] in ("r'", 'r"', "R'", 'R"'):
                if token[-1] == '\n':                  # continued string
                    self.strstart = (lnum, start)
                    self.endprog = endprogs[initial] or endprogs[token[1]]
                    self.contstr, self.needcont = line[start:], 1
                    self.contline = line
                    if self.split_multiline_strings:
                      tokeneater(MULTILINE_STRING_FIRST,
                        line[start:], (lnum, start), (lnum, len(line)), line)
                    break
                else:                                  # ordinary string
                    tokeneater(STRING, token, spos, epos, line)
            elif initial in namechars:                 # ordinary name
                tokeneater(NAME, token, spos, epos, line)
            elif initial == '\\':                      # continued stmt
                self.continued = 1
            else:
                if initial in '([{': self.parenlev = self.parenlev + 1
                elif initial in ')]}': self.parenlev = self.parenlev - 1
                if self.squashop:
                  tokeneater(OP, token, spos, epos, line)
                else:
                  op = opdict[token]
                  tokeneater(op, token, spos, epos, line)
        else:
            tokeneater(ERRORTOKEN, line[pos],
                       (lnum, pos), (lnum, pos+1), line)
            pos = pos + 1


  def _close(self):
      for indent in self.indents[1:]:          # pop remaining indent levels
          self.tokeneater(DEDENT, '', (self.lnum, 0), (self.lnum, 0), '')
      self.tokeneater(ENDMARKER, '', (self.lnum, 0), (self.lnum, 0), '')

if __name__ == '__main__':                     # testing
    import sys
    if len(sys.argv) > 1: tokenise(open(sys.argv[1]).readline)
    else: tokenise(sys.stdin.readline)

