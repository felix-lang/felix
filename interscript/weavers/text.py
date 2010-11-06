from interscript.weavers.base import weaver_base
import string

from interscript.drivers.sinks.bufdisk import named_file_sink
def mk_text(pass_frame,basename, directory, prefix, eol, title, language):
  if language: filename = directory + language + '_' + basename + '.txt'
  else: filename = directory + basename + '.txt'
  w = named_file_sink(pass_frame,filename, prefix)
  if not title: title = basename
  return plain_text_weaver(pass_frame,w, title=title,language=language)

class table_rule_object: pass

class plain_text_weaver(weaver_base):
  def __init__(self, pass_frame,writer,language='',**kwds):
    weaver_base.__init__(self, pass_frame, language)
    if 'weavers' in self.process.trace:
      print('initialising plain text weaver, writer',writer.get_sink_name())
    self.protocol = ('text/plain',1)
    self.persistent_frame['protocol']=self.protocol
    self.width = 55
    self.c = 0
    self.buffer = ''
    self.strong = 0
    self.code = 0
    self.sink = writer
    self.name = 'plain text weaver v1 for '+self.sink.name
    self.persistent_frame['name']=self.name
    self.tags = ['text']
    self.margin = 0
    self.numbered_list_stack = []
    self.sop = 1
    self.hcount = []
    self.toc = []

  def _write(self,line):
    if self.enabled:
      self.sink.write(line)
      self.c = self.c + len(line)
    if line:
      self.sop = 0

  def _writeline(self,line=''):
    if self.enabled:
      self._write(line+'\n')
      self.c = 0

  def _goto(self,column):
    if self.enabled:
      if column < 0: column = self.width + column
      if column < self.c: self._writeline()
      if column > self.c: self._write(' '*(column-self.c))

#line 67 "interscript/src/text_weaver.ipk"
  def set_original_filename(self, filename):
    self.original_filename = filename

  def set_anchor(self, label):
    if 'anchors' not in self.persistent_frame:
      self.persistent_frame['anchors']  = {}
    self.persistent_frame['anchors'][label]=self.sink.lines_written+1

  def get_anchor(self, label):
    href = None
    if 'anchors' in self.persistent_frame:
      if label in self.persistent_frame['anchors']:
        href =self.persistent_frame['anchors'][label]
    return href

  def ref_anchor(self, label):
    href = self.get_anchor(label)
    if href:
      self._write('line '+str(href))
    else:
      self._write('Unknown Label:'+label)

#line 91 "interscript/src/text_weaver.ipk"
  def print_contents(self, hlevel=2, maxlev=3, *args, **kwds):
    if hlevel>0:
      self.head(hlevel,'Contents')
    toc = self.persistent_frame.get('contents',[])
    for level, line, lineno in toc:
      if level <=maxlev:
        prefix = ' '*(level*3)+line+' '
        suffix = ' '+str(lineno+1)
        mid = '.' * (self.width - len(prefix) - len(suffix))
        self._writeline(prefix + mid + suffix)
    self.par()

#line 105 "interscript/src/text_weaver.ipk"
  def print_file_list(self,hlevel=2, *args, **kwds):
    if hlevel>0:
      self.head(hlevel,'File List')
    if self.master.flist:
      for line in self.master.flist:
        self._writeline(line)
    else:
      self._writeline('No data available in pass '+str(self.pass_frame.passno)+'.')
    self.par()

#line 117 "interscript/src/text_weaver.ipk"
  def print_file_status(self,hlevel=2, *args, **kwds):
    passno = self.pass_frame.passno
    h = 'File Status for pass '+str(passno-1)
    if hlevel>0:
      self.head(hlevel,h)
    if self.master.fdict:
      skeys = list(self.master.fdict.keys())
      skeys.sort()

      h = 'Unchanged Files'
      if hlevel>0:
        self.head(hlevel+1,h)
      else:
        self._writeline(h)
      for key in skeys:
        status,change_passno = self.master.fdict[key]
        if status == 'unchanged' and change_passno==0:
          self._writeline(key)

      h = 'Changed Files'
      if hlevel>0:
        self.head(hlevel+1,h)
      else:
        self._writeline(h)
      for key in skeys:
        status,change_passno = self.master.fdict[key]
        if status == 'unchanged' and change_passno == 1:
          self._writeline(key)

      h = 'Files which required 2 or more passes to converge'
      if hlevel>0:
        self.head(hlevel+1,h)
      else:
        self._writeline(h)
      for key in skeys:
        status,change_passno = self.master.fdict[key]
        if status == 'unchanged' and change_passno > 1:
          self._writeline(key+' (converged in '+str(change_passno)+' passes)')

      h = 'Unstable Files'
      if hlevel>0:
        self.head(hlevel+1,h)
      else:
        self._writeline(h)
      for key in skeys:
        status,change_passno = self.master.fdict[key]
        if status == 'changed':
          self._writeline(key)

    else:
      self._writeline('No data available in pass '+str(passno)+'.')
    self.par()

#line 172 "interscript/src/text_weaver.ipk"
  def print_source_list(self, hlevel=2, *args, **kwds):
    if hlevel>0:
      self.head(hlevel,'Source List')
    if self.master.iflist:
      for line,loc in self.master.iflist:
        self._writeline(line)
    else:
      self._writeline('No data available in pass '+str(self.pass_frame.passno)+'.')
    self.par()

#line 184 "interscript/src/text_weaver.ipk"
  def print_include_list(self, hlevel=2, *args, **kwds):
    if hlevel>0:
      self.head(hlevel,'Include List')
    if self.master.include_files:
      for level, type, name in self.master.include_files:
        self._writeline(' '*(level*3)+' '+type+': '+name)
    else:
      self._writeline('No data available in pass '+str(self.pass_frame.passno)+'.')
    self.par()

#line 196 "interscript/src/text_weaver.ipk"
  def __htabrule(self,colw):
    self._write('+')
    for w in colw:
      self._write('-'*(w+2)+'+')
    self._writeline()

  def __tabrow(self,colw,data):
    self._write('|')
    for i in range(len(colw)):
      w = colw[i]
      entry = ' '* w
      if i<len(data):
        entry = (data[i]+entry)[:w]
      self._write(' '+entry+' |')
    self._writeline()

  def begin_table(self,*headings, **kwds):
    self.table_headings = headings
    self.table_data = []

  def table_row(self,data):
    self.table_data.append(data)

  def table_rule(self):
    self.table_data.append(table_rule_object)

  def end_table(self):
    width = len(self.table_headings)
    for row in self.table_data:
      if row is not table_rule_object:
        w = len(row)
        if w>width : width = w
    colw = [0] * width
    for i in range(len(self.table_headings)):
      w = len(self.table_headings[i])
      if colw[i]<w: colw[i]=w
    for row in self.table_data:
      if row is not table_rule_object:
        for i in range(len(row)):
          w = len(row[i])
          if colw[i]<w: colw[i]=w
    self._flush()
    self._writeline()
    self.__htabrule(colw)
    self.__tabrow(colw,self.table_headings)
    self.__htabrule(colw)
    for data in self.table_data:
      if data is table_rule_object:
        self.__htabrule(colw)
      else:
        self.__tabrow(colw,data)
    self.__htabrule(colw)
    del self.table_headings
    del self.table_data

#line 253 "interscript/src/text_weaver.ipk"
  def writecode(self,line):
    self._flush()
    self._writeline(line)

  def echotangle(self,count,data):
    self.writecode("%6d: %s" % (count,data))

  def _write_word(self,word):
    if self.c == 0:
      self._write((' '*self.margin)+word)
    elif self.c + len(word) < self.width:
      self._write(' '+word)
    else:
      self._writeline()
      self._write((' '*self.margin)+word)

  def _flush(self):
    words = self.buffer.split()
    for w in words:
      self._write_word(w)
    self.buffer = ''

  def write(self,line):
    if self.translating:
      if self.strong: line = line.upper()
      self.buffer = self.buffer + line
    else:
      self._write(line)

  def writeline(self,line = ''):
    if self.translating:
      self.write(line)
      if self.code:
        self._writeline(self.buffer)
        self.buffer = ''
      else:
        self._flush()
    else:
      self._writeline(line)

  def par(self):
    self.line_break()
    if not self.sop:
      self._writeline()
      self.sop = 1

  def line_break(self):
    self._flush()
    if self.c != 0: self._writeline()

  def page_break(self):
    self.par()
    self._writeline()
    self._writeline('-' * self.width)
    self._writeline()
    self.sop = 1

  def begin_emphasize(self):
    self.write('_')

  def end_emphasize(self):
    self.write('_')

  def begin_strong(self):
    self.strong = 1

  def end_strong(self):
    self.strong = 0

  def begin_displayed_code(self):
    self.par()
    self.code = 1

  def end_displayed_code(self):
    self.par()
    self.code = 0

  def begin_displayed_text(self):
    self.par()
    self.margin = self.margin + 4

  def end_displayed_text(self):
    self.par()
    self.margin = self.margin - 4

  def new_heading(self,level):
    while level>len(self.hcount): self.hcount.append(0)
    while level<len(self.hcount): del self.hcount[-1]
    counter = self.hcount[level-1]+1
    self.hcount[level-1] = counter
    return counter

  def get_formatted_heading_number(self, sep):
    hnumber = ''
    for i in range(0,len(self.hcount)-1):
      hnumber = hnumber + str(self.hcount[i])+sep
    hnumber = hnumber + str(self.hcount[-1])
    return hnumber

  def head(self,level, text, **kwds):
    atext=kwds.get('short_text')
    anchor=kwds.get('key','')
    self.par()
    self.strong = 0
    self.new_heading(level)
    h = self.get_formatted_heading_number('.')+'. '+text
    if anchor: h=h + '['+anchor+']'
    self.toc.append((level,text,self.sink.lines_written))
    self._writeline(h)
    self._writeline('-'*len(h))
    self._writeline()

  def code_head(self,tangler, secno):
    if tangler:
      self.par()
      language = tangler.get_language()
      filename  =tangler.sink.get_sink_name()
      self._writeline( 'Start '+language+' section to '+\
        filename+'['+str(secno)+']')
      self._writeline()

  def code_foot(self,tangler,secno):
    if tangler:
      self.par()
      language = tangler.get_language()
      filename  =tangler.sink.get_sink_name()
      self._writeline()
      self._writeline( 'End '+language+' section to '+filename+\
        '['+str(secno)+']')
      self.par()

  def test_output_head(self,command, status):
    self.par()
    self._writeline( 'Start output section of '+command)
    if status:
      self._writeline( 'Command returned '+str(status))
    self.par()

  def test_output_foot(self,command,status):
    self.par()
    self._writeline('End output section of '+command)
    self.par()

  def expected_head(self,command):
    self.par()
    self._writeline( 'Start expected section of '+command)
    self.par()

  def expected_foot(self,command):
    self.par()
    self._writeline('End expected section of '+command)
    self.par()

  def diff_head(self,command):
    self.par()
    self._writeline( 'Start diff section of '+command)
    self.par()

  def diff_foot(self,command):
    self.par()
    self._writeline('End diff section of '+command)
    self.par()

  def __del__(self):
    self._flush()
    if self.c != 0:
      self._writeline()
    self.persistent_frame['contents']=self.toc

#line 424 "interscript/src/text_weaver.ipk"
  def begin_keyed_list(self):
    self.margin = self.margin + 4

  def begin_numbered_list(self,start=1):
    self.margin = self.margin + 4
    self.numbered_list_stack.append(start)

  def begin_bullet_list(self):
    self.margin = self.margin + 4

  def end_keyed_list(self):
    self.par()
    self.margin = self.margin - 4

  def end_numbered_list(self):
    self.par()
    self.margin = self.margin - 4
    del self.numbered_list_stack[-1]

  def end_bullet_list(self):
    self.par()
    self.margin = self.margin - 4

  def begin_keyed_list_item(self,key):
    self.par()
    self._goto(self.margin-4)
    self._write(key+' ')
    self._goto(self.margin)

  def begin_numbered_list_item(self):
    self.par()
    key = "%2d. " % self.numbered_list_stack[-1]
    self.numbered_list_stack[-1] = self.numbered_list_stack[-1] + 1
    self._goto(self.margin-4)
    self._write(key)

  def begin_bullet_list_item(self):
    self.par()
    key = '*   '
    self._goto(0)
    self._goto(self.margin-4)
    self._write(key)

#line 469 "interscript/src/text_weaver.ipk"
  def cite_url(self,url):
    self.write(url)


