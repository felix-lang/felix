
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

try:
  import interscript.utilities.diff
except:
  try: raise Exception("dummy") # correct stupid python bug
  except: pass

def compile_parse_tab(res):
  return [[re.compile(x[0]), x[1]] for x in res]

class deduce: pass


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


  def post_methods(self):
    # input frame methods
    self.userdict.update(self.process.global_frame.__dict__)
    method_names = list(self.__class__.__dict__.keys())
    is_begin_or_end_method = lambda x: x[:3]=='end' or x[:5]=='begin'
    method_names = list(filter(is_begin_or_end_method, method_names))
    method_names = method_names + [
      'set_warning_character',
      'doc',
      'data_output',
      'tangler',
      'push','pop','select','resume_code',
      'tangler_push','tangler_pop','tangler_set', # temporarily!
      'get_tangler',
      'untangle',
      'tangle','sink_line',
      'raw_if','enable','enable_if','disable',
      'get_input_frame',
      'include_file','include_source',
      'include_code','insert_code',
      'sink_verbatim','expand','define',
      'capture_output',
      'capture_python_output','print_python_output',
      'print_python_test_output',
      'get_attribute',
      'interscript_from_options',
      'python',
      'post_notice',
      'skip_upto','skip_upto_if',
      'set_encoding','get_encoding',
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
      'set_native_language','get_native_language',
      'set_document_data', 'get_document_data']
    for m in mastrer_frame_method_names:
      self.userdict[m]=eval('self.master.'+m)

    #functions (non methods)
    # NOTE: most functions are made available thru the global frame!
    function_names = []
    for f in function_names:
      self.userdict[f]=eval(f)


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

  def get_attribute(self,name,default=None):
    "Get a variable, default None"
    if name in self.userdict:
      return self.userdict[name]
    else:
      return default


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


  def set_encoding(self, encoding):
    "Set the current encoding"
    self.source.set_encoding(encoding)
  def get_encoding(self):
    "Get the current encoding"
    return self.source.get_encoding()


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


  def set_warning_character(self,python=None):
    "Set the interscript warning character (usually @)"
    res = self.make_parse_tab(pywarn=python)
    res = compile_parse_tab(res)
    self.reg_list = res
    self.python_warn = python

  def normal_line(self,data,file,count):
    if self.current_tangler:
      self.current_tangler.writeline(data,file,count)
    else:
      pass


  def enqueue_input(self,file, count, line):
    """Enqueue a line with cross reference information
    into the input stream."""
    self.read_buffer.append((file,count,line))

  def dequeue_input(self):
    "Read a line out of the input stream"
    data = self.read_buffer[0]
    del self.read_buffer[0]
    return data

  # This is the interscript version of a 
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


  def python(self, terminal, keep=0):
    "Execute embedded python script"
    file = self.original_filename
    count = self.original_count
    data = self.collect_upto(terminal)
    self.process.py_exec(data,file,count,self.userdict)


  def do_exec_line(self,match, file,count,dict):
    self.process.py_exec(match.group(1),file,count,dict)


  def do_exec_suite(self,match,file,count,dict):
    saved = self.collect_stuff(match.group(1), self.cont_re, self.echo)
    self.process.py_exec(saved,file,count,dict)


  def do_web(self,match,file,count,dict):
    self.normal_line(match.group(1),file,count)


  def do_quote_at(self,match,file,count,dict):
    self.normal_line(match.group(1)+match.group(2),file,count)


  def do_html(self,match,file,count,dict):
    self.html_parser.writeline(match.group(1),file,count)


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


  def push(self,f):
    "Push tangler onto tangler stack"
    self.tangler_push(f)

  def pop(self):
    "Pop tangler from tangler stack"
    self.tangler_pop()


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
    self.tangler_set(f)

  def resume_code(self):
    "Pop the current tangler, use after starting string or comment tangler"
    self.current_tangler_pop()

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

  def capture_output(self,command):
    "Capture the output from executing the shell command"
    commands = self.global_frame.commands
    status, output = commands.getstatusoutput(command)
    data = output.split('\n')
    return (status,data)

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

  def doc(self):
    "Begin documentation mode"
    self.tangler_set(None)


  def post_notice(self, key, value):
    "Post a copyright, licence, or credit notice"
    self.master.noticedict[key]=value


