#line 343 "../lpsrc/flx_felix_tangler.pak"
#---------------------------------------------------------
# felix tangler: write to a file, insert source line numbers
# using '#line ' comments
# works for Felix
programming_language="felix"
from interscript.tanglers.base import tangler_base
import re
import string
from interscript.tokenisers.felix import felix_tokeniser
from interscript.tokenisers.felix import COMMENT, \
   MULTILINE_STRING_FIRST, \
   MULTILINE_STRING_MIDDLE, \
   MULTILINE_STRING_LAST
from interscript.tokenisers import felix_keyword
from interscript.tokenisers import felix_token

py_bracket_tokens = [
  felix_token.LPAR, felix_token.RPAR,
  felix_token.LSQB, felix_token.RSQB,
  felix_token.LBRACE, felix_token.RBRACE]

py_punct_tokens = [
  felix_token.COLON, felix_token.COMMA, felix_token.SEMI]

py_op_tokens = [
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.DOLLAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.QUEST,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.EXCLAMATION,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LPAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.RPAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LSQB,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.RSQB,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LBRACE,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.RBRACE,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.COLON,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.COMMA,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.SEMI,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.PLUS,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.MINUS,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.STAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.SLASH,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.VBAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.AMPER,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LESS,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.GREATER,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.EQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.DOT,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.PERCENT,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.BACKQUOTE,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.TILDE,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.CIRCUMFLEX,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.HASH,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.DOLLARDOLLAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.ANDLESS,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.ANDGREATER,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.EQEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.NOTEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LESSEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.GREATEREQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LEFTSHIFT,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.RIGHTSHIFT,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.STARSTAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LESSCOLON,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.COLONGREATER,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.DOTDOT,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.COLONCOLON,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.PLUSPLUS,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.MINUSMINUS,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.PLUSEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.MINUSEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.STAREQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.SLASHEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.PERCENTEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.CARETEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.VBAREQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.AMPEREQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.TILDEEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.COLONEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.RIGHTARROW,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.EQRIGHTARROW,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LEFTARROW,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LSQBAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.RSQBAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.AMPERAMPER,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.VBARVBAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.SLOSHAMPER,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.SLOSHVBAR,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.SLOSHCIRCUMFLEX,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.HASHBANG,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LEFTSHIFTEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.RIGHTSHIFTEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LEFTRIGHTARROW,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.ANDEQEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.ANDNOTEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.ANDLESSEQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.ANDGREATEREQUAL,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.DOTDOTDOT,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.LONGRIGHTARROW,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.PARSE_ACTION,
#line 370 "../lpsrc/flx_felix_tangler.pak"
  felix_token.HASHBANGSLASH,
  ]

class flx_tangler(tangler_base):
  def __init__(self,sink,weaver,nosref=0):
    tangler_base.__init__(self,sink,weaver,nosref)
    self.matchPOD = re.compile('^ *//@(.*)$')
    self.matchcomment = re.compile('^([^/]*)//.*$')
    self.excludeid = []
    self.userdict = {}
    self.tokeniser = felix_tokeniser(report_comments = 1, split_multiline_strings=1)
    self.language = 'felix'

#  def __del__(self):
#    try:
#      tokens = self.tokeniser.close()
#    except:
#        print 'Tokeniser error'
#        try:
#          print 'closing tokeniser for',self.sink.name
#        except:
#          print 'tangler sink missing in __del__ method'
#    tangler_base.__del__(self)

  def writeline(self,data,file,count,inhibit_sref=0):
    match = self.matchPOD.match(data)
    if match:
      command = match.group(1)
      py_exec(command,file,count,globals(),self.userdict)
    else:
      self.weaver.set_fc_anchor(file,count)
      # special hack to preserve leading #! line
      if self.sink.lines_written == 0 and len(data)>2:
        inhibit_sref = data[:2]=='#!'
      self._handle_sref(file,count, inhibit_sref)
      self._writeline(data)

      tokens = self.tokeniser.tokenize(data+'\n')

      # pretty printing
      chars_written = 0
      self.weaver.start_code_line(self.sink.lines_written)
      if tokens:
        for kind,id,lstart,lend,dummy in tokens:
          first = lstart[1]
          last = lend[1]
          self.weaver.write_code_fragment(data[chars_written:first])
          markup = None
          if kind == felix_token.NAME:
            if felix_keyword.iskeyword(id): markup = 'KEYWORD'
          elif kind == COMMENT: markup = 'COMMENT'
          elif kind in [felix_token.STRING,
            MULTILINE_STRING_FIRST,
            MULTILINE_STRING_MIDDLE,
            MULTILINE_STRING_LAST]: markup = 'STRING'
          elif kind == felix_token.NUMBER: markup = 'NUMBER'
          elif kind in py_bracket_tokens : markup = 'BRACKET'
          elif kind in py_punct_tokens : markup = 'PUNCT'
          elif kind in py_op_tokens: markup = 'OP'
          self.weaver.write_code_fragment(data[first:last], markup)
          chars_written = last
        self.weaver.write_code_fragment(data[chars_written:])
      self.weaver.end_code_line()

      dst_count = self.sink.lines_written
      dst_file = self.sink.name
      class_name = 0
      function_name = 0
      level = 0
      for kind,id,lstart,lend,dummy in tokens:
        if kind is felix_token.NAME:
          if not (felix_keyword.iskeyword(id) or id in self.excludeid):
            if not self.pass_frame.ids.has_key(id): self.pass_frame.ids[id]=[]
            self.pass_frame.ids[id].append((file,count,dst_file,dst_count))
            if class_name:
              #print 'class',id
              if not self.pass_frame.classes.has_key(id): self.pass_frame.classes[id]=[]
              self.pass_frame.classes[id].append((file,count,dst_file,dst_count))
              class_name = 0
            elif function_name:
              if not self.pass_frame.functions.has_key(id): self.pass_frame.functions[id]=[]
              self.pass_frame.functions[id].append((file,count,dst_file,dst_count))
              function_name = 0
          elif id == 'class':
            class_name = 1
          elif id in ['fun','proc']:
            function_name = 1

  def write_comment(self,line,file,count):
    self.writeline('# '+line,file,count)

  def start_section(self, file, count):
    pass
    #data = '#line '+str(count)+' '+'"'+file+'"'
    #self._writeline(data)
    #if self.weaver:
    #  self.weaver.echotangle(self.sink.lines_written,data)

  def get_comment_tangler(self):
    return script_comment_tangler(self.sink)

  def get_string_tangler(self,eol,width):
    return c_string_tangler(self.sink,self.get_weaver(),eol,width)

class script_comment_tangler(tangler_base):
  def writeline(self,data,file,count,inhibit_sref=0):
    if self.weaver:
      self.weaver.writeline(data)
    self._writeline('# '+line)

