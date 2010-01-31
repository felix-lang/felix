#line 34 "interscript/src/input_frame.ipk"
import string
import re
import traceback
import sys
import os
import tempfile
import types
from interscript.frames.processf import process_fault

# these imports _should_ come from the global frame!
from interscript.drivers.sources.base import eof, eoi
from interscript.drivers.sources.disk import named_file_source
from interscript.drivers.sources.disk import parse_source_filename
from interscript.drivers.sources.disk import loadfile
from interscript.drivers.sources.cache import cache_source
from interscript.drivers.sinks.bufdisk import named_file_sink
from interscript.drivers.sinks.disk import simple_named_file_sink
from interscript.drivers.sinks.cache import cache_sink
from interscript.drivers.storage.memory import memory
from interscript.languages.interscript_languages import add_translation
from interscript.tanglers.data import data_tangler

from interscript.core.protocols import has_protocol

from interscript.parsers.html import sgml_wrapper, html_filter
try:
  import interscript.utilities.diff
except:
  try: raise Exception("dummy") # correct stupid python bug
  except: pass

def compile_parse_tab(res):
  return [[re.compile(x[0]), x[1]] for x in res]

class deduce: pass

#line 78 "interscript/src/input_frame.ipk"
class input_frame:

  def __init__(self, pass_frame, src, reg_list, weaver, userdict, depth):
    # the display
    self.inhibit_sref = pass_frame.inhibit_sref
    self.pass_frame = pass_frame
    self.master = pass_frame.master
    self.process = self.master.process
    self.global_frame = self.process.global_frame

    self.current_weaver = weaver
    self.current_weaver_stack = []

    self.depth = depth
    self.source = src
    self.userdict = userdict
    self.reg_list = reg_list
    self.read_buffer = []

    self.current_tangler_stack = []
    self.current_tangler = None
    self.line_offset = 0
    self.original_filename = src.get_source_name()
    self.original_count = self.line_offset
    self.current_weaver.set_original_filename(self.original_filename)
    self.head_offset = 0
    self.tabwidth = self.master.tabwidth

    self.cont_re = re.compile('^$|^ (.*)$')
    self.any_line_re = re.compile('^(.*)$')

    if 'frames' in self.process.trace:
      self.process.acquire_object(self, 'INPUT FRAME['+str(depth)+']='+src.get_source_name())
    self.post_methods()

  def __del__(self):
    if 'frames' in self.process.trace:
      self.process.release_object(self)

  def get_input_frame(self): return self

#line 124 "interscript/src/input_frame.ipk"
  def post_methods(self):
    # input frame methods
    self.userdict.update(self.process.global_frame.__dict__)
    method_names = list(self.__class__.__dict__.keys())
    is_begin_or_end_method = lambda x: x[:3]=='end' or x[:5]=='begin'
    method_names = list(filter(is_begin_or_end_method, method_names))
    method_names = method_names + [
      'head','heading','push_head','pop_head','set_head',
      'line_break','page_break',
      'set_warning_character',
      'doc','p','eop','cite_url',
      'data_output','c_output','cpp_output',
      'python_output','perl_output',
      'interscript_output',
      'tangler',
      'push','pop','select','comment','resume_code',
      'tangler_push','tangler_pop','tangler_set', # temporarily!
      'get_tangler',
      'untangle',
      'weave','weave_line','tangle','sink_line',
      'print_identifier_cross_reference',
      'print_contents',
      'print_file_list',
      'print_source_list',
      'print_include_list',
      'print_class_reference',
      'print_file_status',
      'get_weaver',
      'raw_if','enable','enable_if','disable',
      'get_input_frame',
      'table_row', 'table_rule',
      'begin_table_row','end_table_row',
      'begin_table_cell','end_table_cell',
      'item',
      'include_file','include_source',
      'include_code','insert_code','display_code',
      'sink_verbatim','expand','define',
      'include_html','html',
      'capture_output','print_output',
      'capture_python_output','print_python_output',
      'print_python_test_output',
      'set_weaver','get_weaver', 'push_weaver','pop_weaver',
      'get_attribute',
      'interscript_from_options',
      'test_interscript',
      'python','test_python',
      'register_test','set_test_result',
      'set_anchor','ref_anchor',
      'post_notice',
      'skip_upto','skip_upto_if',
      'set_encoding','get_encoding',
      'help', 'weave_help'
      ]
    for m in method_names:
      self.userdict[m]=eval('self.'+m)

    # pass frame methods
    pass_frame_method_names = [
      'get_pass_frame'
    ]
    for m in pass_frame_method_names:
      self.userdict[m]=eval('self.pass_frame.'+m)

    # processs frame methods
    process_frame_method_names = ['get_process_frame']
    for m in process_frame_method_names:
      self.userdict[m]=eval('self.process.'+m)

    #master frame methods
    mastrer_frame_method_names = [
      'get_master_frame',
      'set_title', 'get_title',
      'add_author',
      'set_native_language','get_native_language',
      'set_document_data', 'get_document_data']
    for m in mastrer_frame_method_names:
      self.userdict[m]=eval('self.master.'+m)

    #functions (non methods)
    # NOTE: most functions are made available thru the global frame!
    function_names = []
    for f in function_names:
      self.userdict[f]=eval(f)

#line 210 "interscript/src/input_frame.ipk"
  def help(self):
    "Command help"
    print("Command Help")
    d = self.userdict
    keys = list(d.keys())
    keys.sort()
    for key in keys:
      routine = d[key]
      typ = type(routine)
      doc = getattr(routine,'__doc__','')
      if typ.__name__ in ['module']:
        print(typ.__name__,key)
      else:
        print(typ.__name__, key + "->",doc)

  def weave_help(self, level):
    "Weave help"
    self.head(level,"Command Help")
    d = self.userdict
    keys = list(d.keys())
    keys.sort()
    for key in keys:
      routine = d[key]
      typ = type(routine)
      doc = getattr(routine,'__doc__','')
      if doc is None: doc = "No documentation"
      self.head(level+1, typ.__name__+ " "+ key)
      if typ is types.MethodType:
        self.weave_line("Method of class " + routine.__self__.__class__.__name__+".")
      self.begin_displayed_code()
      self.weave(doc)
      self.end_displayed_code()

#line 246 "interscript/src/input_frame.ipk"
  def close(self):
    if 'frames' in self.process.trace:
      print('closing frame',self.source.name)
    self.userdict.clear()
    del self.userdict
    del self.current_tangler
    del self.current_weaver
    del self.reg_list
    while self.current_tangler_stack: del self.current_tangler_stack[-1]
    while self.current_weaver_stack: del self.current_weaver_stack[-1]

#line 259 "interscript/src/input_frame.ipk"
  def file_pass(self):
    while 1:
      try:
        file,count,line = self.readline()

        self.echo = 'input' in self.process.trace
        if self.echo:
          print('%s %6s: %s' % (file,count,line))
        for r in self.reg_list:
          match = r[0].match(line)
          if match:
            r[1](match,file,count,self.userdict)
            break
      except eoi:
        if 'frames' in self.process.trace:
          print('EOI detected')
        if self.current_tangler:
          self.select(None)
        self.close()
        return
      except KeyboardInterrupt:
        print('!!!!!!!!! KEYBOARD INTERRUPT !!!!!!!!!')
        self.process.update_files = 0
        self.close()
        raise KeyboardInterrupt
      except process_fault as value:
        print('!!!!!!!!! PROCESS FAULT ',value,' !!!!!!!!!')
        self.process.update_files = 0
        self.close()
        raise
      except SystemExit as value:
        print('!!!!!!!!! SYSTEM EXIT !!!!!!!!!')
        self.process.update_files = 0
        self.close()
        raise SystemExit(value)
      except:
        print('!!!!!!!!! PROGRAM ERROR !!!!!!!!!')
        traceback.print_exc()
        self.process.update_files = 0
        self.close()
        sys.exit(1)

#line 308 "interscript/src/input_frame.ipk"
  def interscript_from_options(self,*args):
    "Run interscript from with the given command line options"
    from interscript  import run_from_options
    svdin  = sys.stdin
    try:
      try:
        run_from_options(args)
      except KeyboardInterrupt: raise
      except SystemExit: raise
      except:
        print('Error running embedded interscript from options')
        print('options were', args)
        traceback.print_exc()
    finally:
      sys.stdin  = svdin

  # Note: this routine is ONLY useful for testing interscript itself,
  # because it always puts the test code into the interscript directory!
  # This needs to be fixed!!! It is ALSO platform dependent!

  def test_interscript(self, description, source_terminator, *args, **kwds):
    "Run interscript on the following embedded test script"
    testno = self.register_test(description, 'interscript')
    testlabel = 'test_'+str(testno)
    self.set_anchor(testlabel)
    self.current_weaver.writeline(
      'On-the-fly interscript for test '+str(testno)+' follows.')
    source_origin_line = self.original_count
    source_origin_file = self.original_filename
    test_code = self.collect_lines_upto(source_terminator)
    self.current_weaver.script_head('interscript',source_origin_file)
    for i in range(len(test_code)):
      self.current_weaver.echotangle(source_origin_line+i+1,test_code[i])
    self.current_weaver.script_foot('interscript',source_origin_file)

    try:
      os.mkdir('interscript/tests')
    except:
      pass
    our_source_filename = 'interscript/tests/test_'+str(testno)+'.tpk'
    f = open(our_source_filename,'w')
    f.write('\n'.join(test_code)+'\n')
    f.close()

    logfile='interscript/tests/output/test_'+str(testno)+'.log'

    kargs =  []
    for key in list(kwds.keys()):
      if key not in ['description','source_terminator']:
        kargs.append('--' + key + '=' + repr(kwds[key]))

    newargs = args + tuple(kargs) + (
      '--weaver=html',
      '--weaver-prefix=interscript/tests/output/',
      '--new-logfile='+logfile,
      '--title=Test '+str(testno)+': '+description,
      our_source_filename)
    self.interscript_from_options(*newargs)
    self.set_test_result(testno,'inspect')
    self.current_weaver.doc()
    self.current_weaver.writeline('Test output at')
    self.current_weaver.cite_url('../tests/output/test_'+str(testno)+'.html')
    self.current_weaver.writeline('. Logfile at')
    self.current_weaver.cite_url('../tests/output/test_'+str(testno)+'.log')
    self.current_weaver.writeline('.')
    self.current_weaver.par()

#line 384 "interscript/src/input_frame.ipk"
  def get_attribute(self,name,default=None):
    "Get a variable, default None"
    if name in self.userdict:
      return self.userdict[name]
    else:
      return default

#line 403 "interscript/src/input_frame.ipk"
  def begin(self):
    "Begin a block"
    ho = self.head_offset
    self.select(None)
    inpt = input_frame(
      self.pass_frame,
      self.source,
      [],
      self.current_weaver,
      self.userdict.copy(),
      self.depth)
    inpt.head_offset = ho
    inpt.set_warning_character(python=self.python_warn)
    inpt.file_pass()

  def end(self):
    "end a block"
    self.select(None)
    raise eoi

#line 477 "interscript/src/input_frame.ipk"
  def include_file(self,name,encoding=None):
    "Include an interscruot file"
    if 'input' in self.process.trace:
      print('input from',name)
    file_signature = (self.depth+1,'interscript',name)
    if file_signature in self.pass_frame.skiplist:
      print('SKIPPING INCLUDE FILE',file_signature)
      i = 0
      t = self.master.src_tree
      n = len(t)
      while i<n:
        if file_signature == tuple(t[i][0:3]): break
        i = i + 1
      if i == n:
        print('COULD NOT FIND SKIP FILE',file_signature,'in',t)
      else:
        self.pass_frame.include_files.append(file_signature)
        i = i + 1
        lev = file_signature[0]
        while i<n:
          if t[i][0] >= lev: break
          print('INSERTING',t[i][2],'into include file list (cheating)')
          self.pass_frame.include_files.append(tuple(t[i][0:3]))
          i = i + 1
    else:
      self.pass_frame.include_files.append(file_signature)
      if encoding is None:
        encoding = self.source.encoding_name
      self.include_source(named_file_source(
        self.pass_frame,name, self.source.directory, encoding=encoding))

  def include_source(self,source):
    "Include an interscript source"
    self.select(None)
    ho = self.head_offset
    inpt = input_frame(
      self.pass_frame,
      source,
      [],
      self.current_weaver,
      self.userdict.copy(),
      self.depth+1)
    inpt.head_offset = ho
    inpt.set_warning_character(python='@')
    inpt.file_pass()
    self.current_weaver.set_original_filename (self.original_filename)

#line 527 "interscript/src/input_frame.ipk"
  def set_encoding(self, encoding):
    "Set the current encoding"
    self.source.set_encoding(encoding)
  def get_encoding(self):
    "Get the current encoding"
    return self.source.get_encoding()

#line 558 "interscript/src/input_frame.ipk"
  def insert_code(self,name):
    "Insert code in an external file into the tangler stream"
    ifdata = (self.depth+1,'code: '+self.current_tangler.language,name)
    self.pass_frame.include_files.append(ifdata)
    r = []
    source = named_file_source(self.pass_frame,name, self.source.directory)
    inpt = input_frame(
      self.pass_frame,
      source,
      r,
      self.current_weaver,
      self.userdict.copy(),
      self.depth+1)
    r.append([inpt.any_line_re, inpt.do_web])
    inpt.select(self.current_tangler)
    inpt.file_pass()

  def include_code(self,name,current_tangler):
    "Insert code in an external file into a nominated tangler stream"
    ifdata = (self.depth+1,'code: '+current_tangler.language,name)
    self.pass_frame.include_files.append(ifdata)
    r = []
    source = named_file_source(
      self.pass_frame,
      name,
      self.source.directory)
    inpt = input_frame(
      self.pass_frame,
      source,
      r,
      self.current_weaver,
      self.userdict.copy(),
      self.depth+1)
    r.append([inpt.any_line_re, inpt.do_web])
    inpt.select(current_tangler)
    inpt.file_pass()

  def sink_verbatim(self,filename):
    "Write code in an external file directly to the tanglers driver"
    self.current_weaver.label_chunk(filename)
    source = named_file_source(
        self.pass_frame,
        filename,
        self.source.directory)
    data = source.readlines()
    self.current_tangler.sink.writelines(data)

  def define(self, macroname, language='data'):
    "Name a chunk of code"
    self.select(self.tangler(cache_sink(macroname, self.master), language))

  def expand(self,macroname):
    "Write named chunk directly to tanglers driver"
    self.current_weaver.label_chunk(macroname)
    source = cache_source(macroname, self.master)
    data = source.readlines()
    self.current_tangler.sink.writelines(data)

#line 637 "interscript/src/input_frame.ipk"

#line 654 "interscript/src/input_frame.ipk"
  def include_html(source):
    "Include a HTML file as input. Translate to interscript."
    self.select(None)
    r = []
    self.pass_frame.include_files.append((self.depth+1,'html: '+self.current_tangler.language,name))
    inpt = input_frame(
      self.pass_frame,
      source,
      r,
      self.current_weaver,
      self.userdict.copy(),
      self.depth+1)
    inpt.html_parser = sgml_wrapper(html_filter(inpt))
    r.append((inpt.any_line_re,inpt.do_html))
    inpt.file_pass()

  def html(self):
    "Begin processesing embedded HTML. Translate to interscript."
    self.select(None)
    r = []
    inpt = input_frame(
      self.pass_frame,
      self.source,
      r,
      self.current_weaver,
      self.userdict.copy(),
      self.depth)
    inpt.html_parser = sgml_wrapper(html_filter(inpt))
    r.append((inpt.any_line_re,inpt.do_html))
    inpt.file_pass()



#line 689 "interscript/src/input_frame.ipk"
  def get_weaver(self):
    "Get the current weaver"
    return self.current_weaver

  def set_weaver(self,w):
    "set the current weaver"
    tmp = self.current_weaver
    self.current_weaver = w
    return tmp

  def push_weaver(self,w):
    "Push the current weaver onto the weaver stack, and set new current weaver"
    self.current_weaver_stack.append(self.current_weaver)
    self.current_weaver = w

  def pop_weaver(self):
    "Set the current weaver to the weaver on the weaver stack and pop it."
    self.current_weaver = self.current_weaver_stack[-1]
    del self.current_weaver_stack[-1]

  def raw_if(self,tag):
    "Set the weaver in raw mode if it has the given tag"
    self.current_weaver.raw_if(tag)

  def enable_if(self,tag):
    "Enable the current weaver if it has the given tag"
    self.current_weaver.enable_if(tag)

  def enable(self):
    "Enable the current weaver"
    self.current_weaver.enable()

  def disable(self):
    "Disable the current weaver"
    self.current_weaver.disable()

#line 727 "interscript/src/input_frame.ipk"
  def set_anchor(self,label):
    "Set the argument label as a document anchor"
    if self.current_tangler:
      self.current_tangler.weaver.set_anchor(label)
    else:
      self.current_weaver.set_anchor(label)

  def ref_anchor(self,label):
    "Generate a reference to the given anchor label"
    self.current_weaver.ref_anchor(label)

#line 740 "interscript/src/input_frame.ipk"
  def set_warning_character(self,python=None):
    "Set the interscript warning character (usually @)"
    res = self.make_parse_tab(pywarn=python)
    res = compile_parse_tab(res)
    self.reg_list = res
    self.python_warn = python

  def normal_line(self,data,file,count):
    weaver = self.get_weaver()
    if self.current_tangler:
      self.current_tangler.writeline(data,file,count)
    else:
      weaver.writeline(data)

#line 758 "interscript/src/input_frame.ipk"
  def enqueue_input(self,file, count, line):
    """Enqueue a line with cross reference information
    into the input stream."""
    self.read_buffer.append((file,count,line))

  def dequeue_input(self):
    "Read a line out of the input stream"
    data = self.read_buffer[0]
    del self.read_buffer[0]
    return data

  # This is the interscript version of a #line directive
  def line(self, number, filename):
    "Reset interscript's source reference data"
    self.inpt.original_file = filename
    self.inpt.line_offset = number - inpt.src.get_lines_read()

  def readline(self):
    while 1:
      if self.read_buffer:
        return self.dequeue_input()
      try:
        line = self.source.readline()
        self.real_filename = self.source.get_source_name()
        self.real_count = self.source.get_lines_read()
        self.original_count = self.real_count + self.line_offset
        line = line.rstrip()
        self.line = line.expandtabs(self.tabwidth)
        return (self.original_filename,self.original_count,self.line)
      except KeyboardInterrupt:
        # should inhibit output for process, not globally
        self.process.update_files = 0
        raise KeyboardInterrupt
      except eof:
        if 'input' in self.process.trace:
          print('readline: EOF')
        self.line = None
        raise eoi
      else:
        print('program error in readline:',sys.exc_info())
        self.process.update_files = 0

#line 820 "interscript/src/input_frame.ipk"
  def untangle(self,name):
    """Wrap an external file up as an interscript package, the wrapped
    file is written to the current tangler."""
    if not self.current_tangler:
      raise Exception('untangle without active tangler')
    f = open(name)
    data = f.readlines()
    f.close()
    self.current_tangler.sink.writeline('@select(output("'+name+'"))')
    for line in data:
      l = line.rstrip()
      if len(l):
        if l[0]=='@': l = '@'+l
      self.inpt.tangler.sink.writeline(l)
    self.current_tangler.sink.writeline('@select(None)')
    self.current_tangler.weaver.begin_small()
    self.current_tangler.weaver.writeline('Included '+name+', '+str(len(data))+' lines.')
    self.current_tangler.weaver.end_small()
    self.current_tangler.weaver.line_break()

#line 867 "interscript/src/input_frame.ipk"
# regexp's for the main functions

  def make_parse_tab(self, pywarn = None):
    res = []
    if pywarn:
      res = res + [
        ['^'+pywarn+'('+pywarn+')(.*)$',self.do_quote_at],
        ['^'+pywarn+'(.*[-+*/%:,\\\\([{]) *(#.*)?$', self.do_exec_suite],
        ['^'+pywarn+'(.*)$',self.do_exec_line]
        ]


    res = res + [
      ['^(.*)$',self.do_web]
      ]
    return res

#line 894 "interscript/src/input_frame.ipk"
  def collect_stuff(self,prefix, cont_re, echo):
    saved = prefix
    try:
      file2,count2,line = self.readline()
      match = cont_re.match(line)
      while match:
        if echo:
          print('%s %6s: %s' % (file2,count2,line))
        body = match.group(1)
        if not body: body = ''
        saved = saved+'\n'+body
        file2,count2,line = self.readline()
        match = cont_re.match(line)
      self.enqueue_input(file2,count2,line)
    except eoi:
      pass
    saved = saved + '\n'
    return saved

  def collect_lines_upto(self,terminal, keep=0):
    "Collect lines up to marker line"
    term_re = re.compile('^'+terminal+'$')
    saved = []
    file,count,line = self.readline()
    match = term_re.match(line)
    while not match:
      saved.append(line)
      file,count,line = self.readline()
      match = term_re.match(line)
    return saved

  def skip_upto(self,terminal):
    "Skip up to marker line"
    term_re = re.compile('^'+terminal+'$')
    file,count,line = self.readline()
    match = term_re.match(line)
    while not match:
      file,count,line = self.readline()
      match = term_re.match(line)

  def skip_upto_if(self,terminal,condition):
    "if condition is true, skip up to marker line"
    if condition: self.skip_upto(terminal)

  def collect_upto(self,terminal, keep=0):
    "Collect text up to marker line"
    return '\n'.join(self.collect_lines_upto(terminal,keep))+'\n'

#line 944 "interscript/src/input_frame.ipk"
  def python(self, terminal, keep=0):
    "Execute embedded python script"
    file = self.original_filename
    count = self.original_count
    data = self.collect_upto(terminal)
    self.process.py_exec(data,file,count,self.userdict)

#line 963 "interscript/src/input_frame.ipk"
  def print_diff_table(self, comparison,
    actual_heading='Actual', expected_heading='Expected',
    ok_message='Data compared equal.',
    diff_message='Differential follows.'):

    equal = len(comparison) == 0
    our_weaver = self.get_weaver()
    if not equal:
      if diff_message:
        our_weaver.writeline(diff_message)
      our_weaver.begin_table('Actual','Expected', CLASS='DIFF')
      for section in comparison:
        left = section[0][1:]
        right = section[1][1:]
        left = '\n'.join(left)
        right = '\n'.join(right)
        our_weaver.table_row([left, right])
      our_weaver.end_table()
    else:
      if ok_message:
        our_weaver.writeline(ok_message)

#line 987 "interscript/src/input_frame.ipk"
  def register_test(self, description, kind):
    "Allocate a sequence number for a test case as described and return it."
    testno = self.pass_frame.get_new_test_number()
    testlabel = 'test_'+str(testno)
    self.pass_frame.tests[testno]=\
      [description,testlabel,kind,'Aborted']
    return testno

  def set_test_result(self, testno, result):
    "Set the result of the given test number"
    self.pass_frame.tests[testno][3]=result

#line 1001 "interscript/src/input_frame.ipk"
  def test_python(self,
    hlevel=None,
    descr=None,
    source_filename=None,
    source_terminator=None,
    expect_filename=None,
    expect_terminator=None,
    diff_context=0):
    """Test a python script. The script may be an external file, or
    embedded, expected output may be an external file,
    embedded, or omitted, if provided a context diff will be generated
    with the nominated number of lines of context.
    """

    testno = self.pass_frame.get_new_test_number()
    testlabel = 'test_'+str(testno)
    test_record = self.pass_frame.tests[testno]=\
      [descr,testlabel,'python','Aborted']
    expect = expect_filename or expect_terminator

    # print heading
    if hlevel: our_hlevel = hlevel
    else: our_hlevel = self.last_head+1
    if descr == None: descr = 'Test'
    self.head(our_hlevel,'Test '+str(testno)+': '+descr)
    self.set_anchor(testlabel)

    our_weaver = self.get_weaver()

    if source_terminator:
      our_weaver.writeline('On-the-fly python test script follows.')
      source_origin_line = self.original_count
      source_origin_file = self.original_filename
      test_code = self.collect_lines_upto(source_terminator)
      our_weaver.script_head('python',source_origin_file)
      for i in range(len(test_code)):
        our_weaver.echotangle(source_origin_line+i+1,test_code[i])
      our_weaver.script_foot('python',source_origin_file)
    elif source_filename:
      our_weaver.writeline('Python test script from file '+source_filename+'.')

    if expect_terminator:
      expected_origin_line = self.original_count
      expected_origin_file = self.original_filename
      expected_output = self.collect_lines_upto(expect_terminator)
    elif expect_filename:
      # FIX to make document relative
      our_weaver.writeline('Expected output from file '+expected_filename+'.')
      expected_lines = loadfile(expect_filename)

    # execute the test code

    if source_filename:
      our_source_filename = source_filename
      description = None
    else:
      our_source_filebase = tempfile.mktemp()
      our_source_filename = our_source_filebase + '_test.py'
      f = open(our_source_filename,'w')
      f.write('\n'.join(test_code)+'\n')
      f.close()
      description = 'python <<temporary>>'
    our_weaver.writeline('Actual output follows.')

    status, actual_output = self.print_python_output(
      our_source_filename,
      description)
    cmd_ok = status == 0

    # delete the file if it was created anonymously
    if not source_filename:
      os.remove(our_source_filename)

    if expect:
      try:
        diff_lines = interscript.utilities.diff.diff_lines
        comparison = diff_lines(actual_output, expected_output, context=diff_context)
        equal = len(comparison)==0
        self.pass_frame.tests[testno][2]= 'diff'
        self.pass_frame.tests[testno][3]= ('Fail','Ok')[equal]
        if not equal:
          our_weaver.writeline('On-the-fly expected output follows.')
          our_weaver.expected_head(expected_origin_file)
          for i in range(len(expected_output)):
            our_weaver.echotangle(expected_origin_line+i+1,expected_output[i])
          our_weaver.expected_foot(expected_origin_file)
          self.print_diff_table(comparison)
      except ImportError:
        our_weaver.writeline('Unable to import diff to perform comparison.')
      except KeyboardInterrupt: raise
      except SystemExit: raise
      except:
        traceback.print_exc()
    else:
      self.pass_frame.tests[testno][3]='Inspect'

#line 1116 "interscript/src/input_frame.ipk"
  def do_exec_line(self,match, file,count,dict):
    self.process.py_exec(match.group(1),file,count,dict)

#line 1121 "interscript/src/input_frame.ipk"
  def do_exec_suite(self,match,file,count,dict):
    saved = self.collect_stuff(match.group(1), self.cont_re, self.echo)
    self.process.py_exec(saved,file,count,dict)

#line 1127 "interscript/src/input_frame.ipk"
  def do_web(self,match,file,count,dict):
    self.normal_line(match.group(1),file,count)

#line 1132 "interscript/src/input_frame.ipk"
  def do_quote_at(self,match,file,count,dict):
    self.normal_line(match.group(1)+match.group(2),file,count)

#line 1137 "interscript/src/input_frame.ipk"
  def do_html(self,match,file,count,dict):
    self.html_parser.writeline(match.group(1),file,count)

#line 1150 "interscript/src/input_frame.ipk"
  def tangler_push(self,f):
    "Push the current tangler onto the tangler stack then set it to the given tangler"
    self.current_tangler_stack.append(self.current_tangler)
    self.current_tangler = f

  def tangler_pop(self):
    "Set the current tangler to the top of the tangler stack and pop it."
    self.current_tangler = self.current_tangler_stack[-1]
    del self.current_tangler_stack[-1]

  def tangler_set(self,f):
    "Set the current tangler"
    self.current_tangler = f

  def get_tangler(self):
    "Get the current tangler (may be None)"
    return self.current_tangler

#line 1174 "interscript/src/input_frame.ipk"
  def data_output(self,f): return self.tangler(f,'data')
  def c_output(self,f): return self.tangler(f,'c')
  def cpp_output(self,f): return self.tangler(f,'cpp')
  def python_output(self,f): return self.tangler(f,'python')
  def perl_output(self,f): return self.tangler(f,'perl')

  # temporarily, we'll use a data tangler
  def interscript_output(self,f):
    filename = self.master.tangler_directory+f
    sink = named_file_sink(self.pass_frame,filename,self.master.tangler_prefix)
    return self.tangler(sink,'data')

  def tangler(self,device, language=deduce, *args, **kwds):
    "Create a tangle object from a dvice specification (either a filename or sink object"
    if has_protocol(device, 'filename'):
      filename = self.master.tangler_directory+str(device)
      sink = named_file_sink(
        self.pass_frame,
        filename,
        self.master.tangler_prefix)
    elif has_protocol(device, 'sink'):
      sink = device
    else: raise TypeError('tangler device argument must be string or sink')

    if language is None: language = 'data'
    if language is deduce:
      try:
        splitup = sink.name.split('.')
        if len(splitup)>1:
          extension = splitup[-1]
          #language = extension_table [extension]
          language = extension
        else: language = 'data'
      except KeyError: language = 'data'
      except IndexError: language = 'data'
    language = language.lower()
    language = language.replace('++','pp') # C++ hack
    language = language.replace('-','_') # obj-C etc

    try:
      import imp
      import interscript.tanglers
      try:
        file,pathname,description = imp.find_module(language,interscript.tanglers.__path__)
      except:
        #print "imp.find_module failed for '"+language+'"'
        raise exceptions.UserWarning("Can't load tangler")
      try:
        tmod = imp.load_module(language,file,pathname,description)
      except:
        print("imp.load_module failed for '"+pathname+"'")
        raise exceptions.UserWarning("Can't load tangler")
      try:
        class_name = language + "_tangler"
        tglr = getattr(tmod,class_name)
      except:
        print("tangler class '"+class_name+"' not found in module ")
        raise exceptions.UserWarning("Can't load tangler")
      t = tglr(sink,self.current_weaver,self.inhibit_sref)
    except:
      print('Unable to load',language,'tangler for "'+device+'": using data')
      t=data_tangler(sink,self.current_weaver)
    return t

#line 1289 "interscript/src/input_frame.ipk"
  def push(self,f):
    "Push tangler onto tangler stack"
    if self.current_tangler: self.code_foot()
    self.tangler_push(f)
    if self.current_tangler: self.code_head()

  def pop(self):
    "Pop tangler from tangler stack"
    if self.current_tangler: self.code_foot()
    self.tangler_pop()
    if self.current_tangler: self.code_head()

#line 1302 "interscript/src/input_frame.ipk"
  def select(self, *args, **kwds):
    "Select the nominated object as the current tangler or weaver"
    for arg in args:
      self.select1(arg)
    if 'tangler' in kwds:
      self.select_tangler(kwds['tangler'])
    if 'weaver' in kwds:
      self.set_weaver(kwds['weaver'])

  def select1(self, arg):
    if has_protocol(arg,'tangler'):
      self.select_tangler(arg)
    elif has_protocol(arg, 'weaver'):
      self.set_weaver(arg)
    elif arg is None:
      self.select_tangler(None)
    else:
      pass #permissive

  def select_tangler(self,f):
    if self.current_tangler: self.code_foot()
    self.tangler_set(f)
    if self.current_tangler: self.code_head()

  def code_head(self):
    dst_filename = self.current_tangler.sink.name
    dst_lineno = self.current_tangler.sink.lines_written
    src_filename = self.original_filename
    src_lineno = self.original_count

    index = self.pass_frame.section_index
    list = index.get(dst_filename, [])
    list.append((dst_lineno, src_filename, src_lineno))
    index[dst_filename]=list
    secno = len(list)
    self.current_weaver.code_head(self.current_tangler, secno)

  def code_foot(self):
    dst_filename = self.current_tangler.sink.name
    index = self.pass_frame.section_index
    list = index.get(dst_filename, [])
    secno = len(list)
    self.current_weaver.code_foot(self.current_tangler, secno)

  def begin_comments(self):
    "Begin tangling lines as comments"
    if self.current_tangler:
      self.current_tangler_push(self.current_tangler.get_comment_tangler())
    else:
      self.current_tangler_push(None)

  def end_comments(self):
    "End comment tangler"
    self.current_tangler_pop()

  def resume_code(self):
    "Pop the current tangler, use after starting string or comment tangler"
    self.current_tangler_pop()

  def comment(self,v):
    "Begin tangling an embedded comment."
    self.get_weaver().write_comment(v)

  def begin_string(self,eol = ' ', width = 0):
    "Begin tangling an embedded string."
    if self.current_tangler:
      self.current_tangler_push(self.current_tangler.get_string_tangler(eol,width))
    else:
      self.current_tangler_push(None)

  def end_string(self):
    "Terminate a string tangler"
    tangler_pop()

  def weave(self,s):
    "Weave a string of text"
    weaver = self.get_weaver()
    weaver.write(s)

  def weave_line(self,s):
    "Weave a line of text"
    weaver = self.get_weaver()
    weaver.writeline(s)

  def _tangle_line(self,s, inhibit_sref=None):
    "Tangle one line of code"
    if inhibit_sref == None: inhibit_sref = self.inhibit_sref
    if self.current_tangler:
      line = self.original_count
      file = self.original_filename
      self.current_tangler.writeline(s,file,line,inhibit_sref)
    else:
      print("tangle: No tangler for",s)

  def tangle(self,s, inhibit_sref=None):
    "Tangle lines of code"
    if inhibit_sref == None: inhibit_sref = self.inhibit_sref
    lines = s.split('\n')
    for line in lines:
      self._tangle_line(line,inhibit_sref)

  def sink_line(self,s):
    if self.current_tangler:
      snk = self.current_tangler.sink
      snk.writeline(s)
    else:
      print("tangle: No tangler for",s)

#line 1413 "interscript/src/input_frame.ipk"
  def print_contents(self,*args, **kwds):
    "Print table of contents"
    self.select(None)
    weaver = self.get_weaver()
    weaver.print_contents(*args, **kwds)

  def print_file_list(self,*args,**kwds):
    "Print complete list of all files"
    self.select(None)
    weaver = self.get_weaver()
    weaver.print_file_list(*args, **kwds)

  def print_file_status(self,*args,**kwds):
    """Weave status information. Warning: this information
    changes from pass to pass, and will prevent your document
    converging to a fixpoint if the output is woven to a
    named file sink. Make sure the weaver driver is a
    simple file sink, which is included in convergence checks."""
    self.select(None)
    weaver = self.get_weaver()
    weaver.print_file_status(*args, **kwds)

  def print_source_list(self, *args, **kwds):
    "Weave the interscript source tree"
    self.select(None)
    weaver = self.get_weaver()
    weaver.print_source_list(*args, **kwds)

  def print_include_list(self, *args, **kwds):
    "Weave the include file list"
    self.select(None)
    weaver = self.get_weaver()
    weaver.print_include_list(*args, **kwds)

  def macro(self,name):
    self.select(None)
    weaver = self.get_weaver()
    return data_tangler(memory(name),weaver)

  def print_identifier_cross_reference(self, *args, **kwds):
    "Weave the identifier cross reference table"
    self.select(None)
    weaver = self.get_weaver()
    weaver.identifier_reference(*args, **kwds)

  def print_class_reference(self, *args, **kwds):
    "Weave the class cross reference table"
    self.select(None)
    weaver = self.get_weaver()
    weaver.class_reference(*args, **kwds)

#line 1467 "interscript/src/input_frame.ipk"
  def capture_output(self,command):
    "Capture the output from executing the shell command"
    commands = self.global_frame.commands
    status, output = commands.getstatusoutput(command)
    data = output.split('\n')
    return (status,data)

  def print_output(self,command,description=None):
    "Weave output from executing the script, register it as a test case."
    status, data = self.capture_output(command)
    weaver = self.get_weaver()
    if description: cmd = description
    else: cmd = command
    weaver.test_output_head(cmd, status)
    for i in range(len(data)):
      line = data[i]
      l = line.rstrip()
      weaver.echotangle(i+1,l)
    weaver.test_output_foot(cmd, status)
    return (status, data)

  def capture_python_output(self,script):
    "Capture the output from executing the python script externally"
    return self.capture_output('"'+sys.executable+'" '+script)

  def print_python_output(self,script, description=None):
    "Weave output from executing the script externally"
    return self.print_output(
      '"'+sys.executable+'" '+script,
      description)

  def print_python_test_output(self,script, descr):
    "Weave output from executing the python script, register it as a test case."
    testno = self.pass_frame.get_new_test_number()
    testlabel = 'test_'+str(testno)
    self.pass_frame.tests[testno]=[descr,testlabel,'python','Unknown']
    self.set_anchor(testlabel)
    return self.print_python_output(script,descr)

#line 1514 "interscript/src/input_frame.ipk"
  def head(self, level, text, **kwds):
    level = int(level)
    level = level + self.head_offset
    self.last_head = level
    if 'headings' in self.process.trace:
      print(('  '*(level-1))+'"'+text+'"')
    self.pass_frame.toc.append((level,text, kwds))
    if self.current_tangler: self.code_foot()
    self.tangler_set(None)
    add_translation(*(text,), **kwds.get('translations',{}))
    self.current_weaver.head(*(level,text), **kwds)

  # like heading, but to be used in code as well:
  # doesn't switch to document mode, doesn't do
  # code headings and footings.
  # deprecated form client interface, but required for perl tangler

  def heading(self, level, text, **kwds):
    "Weave a heading"
    level = int(level)
    level = level + self.head_offset
    self.last_head = level
    if 'headings' in self.process.trace:
      print(('  '*(level-1))+'"'+text+'"')
    self.pass_frame.toc.append((level,text, kwds))
    self.current_weaver.head(*(level,text), **kwds)

  def push_head(self, amt=1):
    "Push the heading level onto the heading level stack"
    self.head_offset = self.head_offset + amt

  def pop_head(self, amt=1):
    "Pop the heading level from the heading level stack"
    self.push_head(-amt)

  def set_head(self, amt=None):
    "Set the heading level"
    if amt != None:
      self.head_offset = amt - 1
    else:
      self.head_offset = self.last_head - 1

#line 1558 "interscript/src/input_frame.ipk"
  def doc(self):
    "Begin documentation mode"
    if self.current_tangler: self.code_foot()
    self.tangler_set(None)

  def p(self): # end a paragraph and start a new one
    "Paragraph serapator"
    self.current_weaver.par()

  def eop(self): # end a paragraph without starting a new one
    "Paragraph terminator"
    self.current_weaver.eop()

  def line_break(self):
    "Break a line"
    self.current_weaver.line_break()
  def page_break(self):
    "Break a page"
    self.current_weaver.page_break()

#line 1580 "interscript/src/input_frame.ipk"
  def cite_url(self, url):
    self.current_weaver.cite_url(url)

#line 1585 "interscript/src/input_frame.ipk"
  def begin_table(self, *headings, **kwds):
    "Begin a table"
    self.get_weaver().begin_table(*headings, **kwds)

  def table_row(self, *data):
    "Weave a table row"
    self.current_weaver.table_row(data)

  def end_table(self):
    "End the current table"
    self.current_weaver.end_table()

  def table_rule(self):
    "Draw a horizontal rule across a table"
    self.current_weaver.table_rule()

  def begin_table_row(self):
    "Begin a table row"
    self.current_weaver.begin_table_row()

  def end_table_row(self):
    "End a table row"
    self.current_weaver.end_table_row()

  def begin_table_cell(self):
    "Begin a table cell"
    self.current_weaver.begin_table_cell()

  def end_table_cell(self):
    "End a table cell"
    self.current_weaver.end_table_cell()

#line 1619 "interscript/src/input_frame.ipk"
  def begin_list(self, style):
    "Begin a list of the nominated style"
    self.current_weaver.begin_list(style)

  def end_list(self):
    "End a list"
    self.current_weaver.end_list()

  def item(self,*args, **kwds):
    "Start a new list item"
    self.current_weaver.item(*args, **kwds)

  def begin_numbered_list(self, start=1):
    "Start a new numbered list item"
    self.current_weaver.begin_numbered_list(start)

  def end_numbered_list(self):
    self.current_weaver.end_numbered_list()

  def begin_numbered_list_item(self):
    self.current_weaver.begin_numbered_list_item()

  def end_numbered_list_item(self):
    self.current_weaver.end_numbered_list_item()

  def begin_bullet_list(self):
    self.current_weaver.begin_bullet_list()

  def end_bullet_list(self):
    self.current_weaver.end_bullet_list()

  def begin_bullet_list_item(self):
    self.current_weaver.begin_bullet_list_item()

  def end_bullet_list_item(self):
    self.current_weaver.end_bullet_list_item()

  def begin_keyed_list(self):
    self.current_weaver.begin_keyed_list()

  def end_keyed_list(self):
    self.current_weaver.end_keyed_list()

  def begin_keyed_list_item(self, key):
    self.current_weaver.begin_keyed_list_item(key)

  def end_keyed_list_item(self):
    self.current_weaver.end_keyed_list_item()

#line 1670 "interscript/src/input_frame.ipk"
  def begin_emphasize(self):
    "Begin setting text in emphasis font"
    self.current_weaver.begin_emphasize()

  def end_emphasize(self):
    "End setting text in emphasis font"
    self.current_weaver.end_emphasize()

  def begin_strong(self):
    "Begin setting text in strong font"
    self.current_weaver.begin_strong()

  def end_strong(self):
    "End setting text in strong font"
    self.current_weaver.end_strong()

  def begin_italic(self):
    "Begin setting text in italic font"
    self.current_weaver.begin_italic()

  def end_italic(self):
    "End setting text in italic font"
    self.current_weaver.end_italic()

  def begin_bold(self):
    "Begin setting text in bold font"
    self.current_weaver.begin_bold()

  def end_bold(self):
    "End setting text in bold font"
    self.current_weaver.end_bold()

  def begin_big(self):
    "End setting text in big font"
    self.current_weaver.begin_big()

  def end_big(self):
    "End setting text in big font"
    self.current_weaver.end_big()

  def begin_small(self):
    "Begin setting text in small font"
    self.current_weaver.begin_small()

  def end_small(self):
    self.current_weaver.end_small()

  def begin_code(self):
    "Begin setting text in monospaced font"
    self.current_weaver.begin_code()

  def end_code(self):
    "Begin setting text in monospaced font"
    self.current_weaver.end_code()

#line 1727 "interscript/src/input_frame.ipk"
  def begin_displayed_text(self):
    "Begin text display"
    self.current_weaver.begin_displayed_text()

  def end_displayed_text(self):
    self.current_weaver.end_displayed_text()


  def begin_displayed_code(self):
    "Begin display of verbatim code"
    self.current_weaver.begin_displayed_code()

  def end_displayed_code(self):
    self.current_weaver.end_displayed_code()

  # this command is used to print out a code file 'verbatim'
  # without line numbers!
  def display_code(self,name,kind='code'):
    "Display the external file as code"
    self.pass_frame.include_files.append((self.depth+1,kind,name))
    self.begin_displayed_code()
    filename = parse_source_filename(name, self.source.directory)[3]
    f = open(filename)
    data = f.readlines()
    f.close()
    weaver = self.get_weaver()
    for line in data:
      l = line.rstrip()
      weaver.writeline(l)
    self.end_displayed_code()

#line 1761 "interscript/src/input_frame.ipk"
  def post_notice(self, key, value):
    "Post a copyright, licence, or credit notice"
    self.master.noticedict[key]=value
#line 1765 "interscript/src/input_frame.ipk"

