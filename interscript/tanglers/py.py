#line 19 "interscript/src/python_tangler.ipk"
#---------------------------------------------------------
# python tangler: write to a file, insert source line numbers
# using '#line ' comments
# works for Python
programming_language="python"
from interscript.tanglers.base import tangler_base
import re
import string
from interscript.tokenisers.python import python_tokeniser
from interscript.tokenisers.python import COMMENT, \
   MULTILINE_STRING_FIRST, \
   MULTILINE_STRING_MIDDLE, \
   MULTILINE_STRING_LAST
import keyword
import token

py_bracket_tokens = [
  token.LPAR, token.RPAR,
  token.LSQB, token.RSQB,
  token.LBRACE, token.RBRACE]

py_punct_tokens = [
  token.COLON, token.COMMA, token.SEMI]

py_op_tokens = [
  token.OP,
  token.PLUS, token.MINUS, token.STAR, token.SLASH,
  token.VBAR, token.AMPER,
  token.LESS, token.GREATER, token.EQUAL,
  token.DOT, token.PERCENT,
  token.BACKQUOTE, token.EQEQUAL,
  token.NOTEQUAL, token.LESSEQUAL, token.GREATEREQUAL,
  token.TILDE, token.CIRCUMFLEX,
  token.LEFTSHIFT,  token.RIGHTSHIFT, token.DOUBLESTAR]

#line 56 "interscript/src/python_tangler.ipk"
class argument:
  def __init__(self,
    name,
    protocol=None,
    description=None,
    default=None):
    self.name = name
    self.protocol = protocol
    self.description = description
    self.default = default

#line 69 "interscript/src/python_tangler.ipk"
def tangle_precondition(indent, precondition):
  return ' ' * indent + 'assert ' + precondition + '\n'

def tangle_postcondition(indent, postcondition):
  return ' ' * indent + 'assert ' + postcondition + '\n'

def tangle_argument_check(indent, argument):
  code = ''
  if argument.protocol:
    code = code + ' '* indent + 'assert has_protocol('+\
      argument.name + ', '+ argument.protocol + ')\n'
  return code

def tangle_argument_checks(indent, arguments):
  code = ''
  for argument in arguments:
    code = code + tangle_argument_check(indent, argument)
  return code

def tangle_argument(argument):
  code = argument.name
  if argument.default: code = code + '='+argument.default
  return code

def tangle_arguments(indent, arguments):
  code = ''
  for argument in arguments[:-1]:
    code = code + ' '*indent + tangle_argument(argument)
    code = code + ',\n'
  code = code + ' '*indent + tangle_argument(arguments[-1])
  return code

def tangle_result(indent, results):
  code = ''
  for result in results:
    if result.protocol:
      code = code + ' '*indent + 'assert has_protocol(' +\
        result.name+', '+ result.protocol+')\n'
  code = code + ' '*indent + 'return '
  for result in results[:-1]:
    code = code + result.name + ', '
  code = code + results[-1].name+'\n'
  return code

def tangle_function(
  sink,
  source_file,
  source_line,
  indent,
  name,
  description=None,
  arguments=None,
  precondition=None,
  result=None,
  postcondition=None,
  initial=None,
  final=None,
  body=None):

  # argument list
  code = ' '* indent + 'def '+name
  if arguments:
    code = code + '(\n'
    code = code + tangle_arguments(indent+2, arguments)
    code = code + '):\n'
  else: code = code + '():\n'

  # argument checks
  if arguments:
    code = code + ' ' * (indent + 2) + '#check arguments\n'
    code = code + tangle_argument_checks(indent+2, arguments)

  # precondition
  if precondition:
    code = code + ' ' * (indent + 2) + '#precondition\n'
    code = code + tangle_precondition(indent+2, precondition)

  # begin try/finally block
  code = code + ' '* (indent+2) + 'try:\n'

  # initial
  if initial:
    code = code + ' ' * (indent + 4) + '#initially\n'
    for line in initial:
      code = code + ' ' * (indent+4) + line + '\n'

  # begin try/except block
  code = code + ' '* (indent+4) + 'try:\n'

  # body
  if body:
    code = code + ' ' * (indent + 6) + '#body\n'
    for line in body:
      code = code + ' ' * (indent+6) + line + '\n'


  # exception
  code = code + ' ' * (indent + 4) + '#transmit user exceptions\n'
  code = code + ' ' * (indent +4) + 'except: raise\n'
  code = code + ' ' * (indent +4) + 'else:\n'


  # postcondition
  if postcondition:
    code = code + ' ' * (indent + 6) + '#postcondition\n'
    code = code + tangle_postcondition(indent + 6, postcondition)


  # result
  if result:
    code = code + ' ' * (indent + 6) + '#return result\n'
    code = code + tangle_result(indent + 6, result)
  else: code = code + ' ' * (indent+6) + 'pass\n'

  # finally
  code = code + ' ' * (indent + 2) + '#cleanup\n'
  code = code + ' '* (indent+2) + 'finally:\n'
  if final:
    for line in final:
      code = code + ' ' * (indent+4) + line + '\n'
  else:
    code = code + ' ' * (indent+4) + 'pass\n'

  for line in string.split(code,'\n')[:-1]:
    sink.writeline(line)
  return code

#line 198 "interscript/src/python_tangler.ipk"
#-------------------------------------------------
def weave_argument(weaver, indent, argument):
  weaver.write_code_fragment(' '* indent)
  weaver.write_code_fragment(argument.name, 'NAME')
  if argument.protocol:
    weaver.write_code_fragment(':', 'PUNCT')
    weaver.write_code_fragment(' ')
    weaver.write_code_fragment(argument.protocol, 'NAME')
  if argument.default:
    weaver.write_code_fragment('=', 'PUNCT')
    weaver.write_code_fragment(argument.default)
  if argument.description:
    weaver.write_code_fragment(' ')
    weaver.write_code_fragment(repr(argument.description), 'COMMENT')

def weave_arguments(weaver, indent, arguments):
  for argument in arguments[:-1]:
    weaver.start_code_line()
    weave_argument(weaver, indent, argument)
    weaver.write_code_fragment(',','PUNCT')
    weaver.end_code_line()
  weaver.start_code_line()
  weave_argument(weaver, indent, arguments[-1])

def weave_function(
  weaver,
  indent,
  name,
  description=None,
  arguments=None,
  precondition=None,
  result=None,
  postcondition=None,
  initial=None,
  final=None,
  body=None):

  weaver.start_code_line()
  weaver.write_code_fragment(' '*indent)
  weaver.write_code_fragment('function','KEYWORD')
  weaver.write_code_fragment(' ')
  weaver.write_code_fragment(name,'NAME')
  weaver.write_code_fragment(':','PUNCT')
  weaver.write_code_fragment(' ')
  weaver.write_code_fragment('# '+description,'COMMENT')
  weaver.end_code_line()

  if arguments:
    weaver.start_code_line()
    weaver.write_code_fragment(' '*(indent+2))
    weaver.write_code_fragment('accepts','KEYWORD')
    weaver.write_code_fragment(':','PUNCT')
    weaver.end_code_line()
    weave_arguments(weaver,indent+4,arguments)
    weaver.end_code_line()

  if precondition:
    weaver.start_code_line()
    weaver.write_code_fragment(' '*(indent+2),)
    weaver.write_code_fragment('precondition','KEYWORD')
    weaver.write_code_fragment(':','PUNCT')
    weaver.write_code_fragment(' ')
    weaver.write_code_fragment(precondition)
    weaver.end_code_line()

  if result:
    weaver.start_code_line()
    weaver.write_code_fragment(' '*(indent+2))
    weaver.write_code_fragment('returns','KEYWORD')
    weaver.end_code_line()
    weave_arguments(weaver,indent+4,result)
    weaver.end_code_line()

  if postcondition:
    weaver.start_code_line()
    weaver.write_code_fragment(' '*(indent+2))
    weaver.write_code_fragment('postcondition','KEYWORD')
    weaver.write_code_fragment(':','PUNCT')
    weaver.write_code_fragment(' ')
    weaver.write_code_fragment(postcondition)
    weaver.end_code_line()

  if body:
    for line in body:
      weaver.start_code_line()
      weaver.write_code_fragment(' '*(indent+2))
      weaver.write_code_fragment(line)
      weaver.end_code_line()

#line 289 "interscript/src/python_tangler.ipk"
#-------------------------------------------------
class py_tangler(tangler_base):
  def __init__(self,sink,weaver,nosref=0):
    tangler_base.__init__(self,sink,weaver,nosref)
    self.matchPOD = re.compile('^ *#@(.*)$')
    self.matchcomment = re.compile('^([^#]*)#.*$')
    self.excludeid = []
    self.userdict = {}
    self.tokeniser = python_tokeniser(report_comments = 1, split_multiline_strings=1)
    self.language = 'python'

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

      try:
        tokens = self.tokeniser.tokenize(data+'\n')
      except TokenError, e:
        print 'Tokeniser error',e
        print 'in file',file,'line',line
        print 'data['+data+']'


      # pretty printing
      chars_written = 0
      self.weaver.start_code_line(self.sink.lines_written)
      if tokens:
        for kind,id,lstart,lend,dummy in tokens:
          first = lstart[1]
          last = lend[1]
          self.weaver.write_code_fragment(data[chars_written:first])
          markup = None
          if kind == token.NAME:
            if keyword.iskeyword(id): markup = 'KEYWORD'
          elif kind == COMMENT: markup = 'COMMENT'
          elif kind in [token.STRING,
            MULTILINE_STRING_FIRST,
            MULTILINE_STRING_MIDDLE,
            MULTILINE_STRING_LAST]: markup = 'STRING'
          elif kind == token.NUMBER: markup = 'NUMBER'
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
        if kind == token.INDENT:
          level = level + 1
        elif kind == token.DEDENT:
          level = level - 1
        if kind is token.NAME:
          if not (keyword.iskeyword(id) or id in self.excludeid):
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
          elif id == 'def':
            function_name = 1

  def write_comment(self,line,file,count):
    self.writeline('# '+line,file,count)

  def start_section(self, file, count):
    data = '#line '+str(count)+' '+'"'+file+'"'
    self._writeline(data)
    if self.weaver:
      self.weaver.echotangle(self.sink.lines_written,data)

  def get_comment_tangler(self):
    return script_comment_tangler(self.sink)

  def get_string_tangler(self,eol,width):
    return c_string_tangler(self.sink,self.get_weaver(),eol,width)

  def function(self,
    name,
    indent,
    source_file,
    source_line,
    description=None,
    arguments=None,
    precondition=None,
    result=None,
    postcondition=None,
    initial=None,
    final=None,
    body=None):

    tangle_function(
      self.sink,
      source_file,
      source_line,
      indent,
      name,
      description=description,
      arguments=arguments,
      precondition=precondition,
      result=result,
      postcondition=postcondition,
      initial=initial,
      final=final,
      body=body)

    weave_function(
      self.weaver,
      indent,
      name,
      description=description,
      arguments=arguments,
      precondition=precondition,
      result=result,
      postcondition=postcondition,
      initial=initial,
      final=final,
      body=body)

#line 444 "interscript/src/python_tangler.ipk"
class script_comment_tangler(tangler_base):
  def writeline(self,data,file,count,inhibit_sref=0):
    if self.weaver:
      self.weaver.writeline(data)
    self._writeline('# '+line)

