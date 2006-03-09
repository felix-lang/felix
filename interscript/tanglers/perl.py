#line 22 "interscript/src/perl_tangler.ipk"

from interscript.tanglers.base import tangler_base
from interscript.tanglers.c import c_string_tangler
import re
import string

class perl_tangler(tangler_base):
  def __init__(self,sink,weaver, heading_level_offset = 2):
    tangler_base.__init__(self,sink,weaver)
    self.language = 'perl'

    self.mode = 'code'
    self.list_type = []
    self.pod_re = re.compile('^=([A-Za-z][A-Za-z0-9_]*) *(.*)$')
    self.heading_level_offset = heading_level_offset
    self.esc_re = re.compile('^(.*?)(>|[IBSCLFXZE]<)(.*)$')
    self.digits_re = re.compile('^([0-9]+)>(.*)$')
    self.entity_re = re.compile('^([A-Za-z]+)>(.*)$')
    # this is not a full list, we should in fact call a weaver routine.
    self.html_entity = {
      'amp':'&',
      'lt':'<',
      'gt':'>',
      'quot':'"',
      'copy':'C',
      'trade':'T',
      'reg':'R'}

  def __del__(self):
    self.flow_escape()
    self.end_lists()

  def flow_escape(self):
    line = self.flow_text
    if not line: return
    self.flow_text = ''
    # process balanced text,
    # if there is an unbalanced >, the text after it is returned
    # write a >, and then try again.
    tail = self.flow_parse(line)
    while tail:
      if 'tanglers' in self.process.trace:
        print 'Unbalanced > in perl POD text'
      self.weaver.write('>')
      tail = self.flow_parse(tail)

  # recursive descent parser
  def flow_parse(self,tail):
    if not tail: return ''
    weaver = self.weaver

    match = self.esc_re.match(tail)
    while match:
      pre, cmd, tail = match.group(1,2,3)
      if pre: weaver.write(pre)
      if cmd=='>': return tail

      assert len(cmd)==2 and cmd[1]=='<'
      cmd = cmd[0]
      if cmd == 'I':
        weaver.begin_italic()
        tail = self.flow_parse(tail)
        weaver.end_italic()
      elif cmd == 'B':
        weaver.begin_bold()
        tail = self.flow_parse(tail)
        weaver.end_bold()
      elif cmd == 'S':
        # should be non-breaking spaces, but interscript
        # doesn't implement that
        tail = self.flow_parse(tail)
      elif cmd == 'C':
        weaver.begin_code()
        tail = self.flow_parse(tail)
        weaver.end_code()
      elif cmd == 'L':
        # a link: we just hack it for now
        weaver.write('[')
        tail = self.flow_parse(tail)
        weaver.write(']')
      elif cmd == 'F':
        # filename
        weaver.begin_code()
        tail = self.flow_parse(tail)
        weaver.end_code()
      elif cmd == 'X':
        # index entry??  (Does this mean print it, or index it?)
        # I'll just print it as code :-)
        weaver.begin_code()
        tail = self.flow_parse(tail)
        weaver.end_code()
      elif cmd == 'Z':
        # zero width character? What's that mean?
        tail = self.flow_parse(tail)
      elif cmd == 'E':
        match = self.digits_re.match(tail)
        if match:
          digits, tail = match.group(1,2)
          n = chr(int(digits))
          weaver.write(n)
        else:
          match = self.entity_re.match(tail)
          if match:
            entity, tail = match.group(1,2)
            data = self.html_entity.get(entity,'E<'+entity+'>')
            weaver.write(data)
          else:
            # nothing we recognize, print literally
            weaver.write('E<')
            tail = self.flow_parse(tail)
            weaver.write('>')

      match = self.esc_re.match(tail)

    # no (more) matches, so just weave the tail
    self.weaver.writeline(tail)
    return ''


  def end_list_item(self):
    kind = self.list_type[-1]
    weaver = self.weaver
    if kind == 'keyed': weaver.end_keyed_list_item()
    elif kind == 'bullet': weaver.end_bullet_list_item()
    elif kind == 'numbered': weaver.end_numbered_list_item()

  def end_list(self):
    kind = self.list_type[-1]
    weaver = self.weaver
    if kind == 'keyed': weaver.end_keyed_list()
    elif kind == 'bullet': weaver.end_bullet_list()
    elif kind == 'numbered': weaver.end_numbered_list()
    del self.list_type[-1]

  def end_lists(self):
    while self.list_type: self.end_list()

  def begin_list(self,kind):
    # print '** list type:',kind
    self.list_type.append(kind)
    weaver = self.weaver
    if kind == 'keyed': weaver.begin_keyed_list()
    elif kind == 'bullet': weaver.begin_bullet_list()
    elif kind == 'numbered': weaver.begin_numbered_list()

  def begin_list_item(self,key=None):
    kind = self.list_type[-1]
    weaver = self.weaver
    if kind == 'keyed': weaver.begin_keyed_list_item(key)
    elif kind == 'bullet': weaver.begin_bullet_list_item()
    elif kind == 'numbered': weaver.begin_numbered_list_item()

  def writeline(self,data,file,count,inhibit_sref=0):
    if not inhibit_sref and not self.inhibit_sref:
      if (file != self.sink.last_source_file or
        count != self.sink.last_source_count+1):
        self.start_section(file,count)
    self.sink.last_source_file = file
    self.sink.last_source_count = count
    tangler_base._writeline(self,data)

    # try to find a pod command
    pod = self.pod_re.match(data)

    # if we're in code mode, and we didn't
    # get a pod command, just echotangle as code
    # otherwise, switch to pod mode

    if self.mode == 'code':
      if pod: self.mode = 'pod'
      else:
        self.weaver.echotangle(self.sink.lines_written,data)
        return

    # now we're in pod mode, if we didn't get a pod command,
    # strip the line to see if it's blank.
    # if not, weave it and switching pod end of para detection on
    # otherwise, emit an end of paragraph if detection is on
    # unless we're in litpar mode, in which case we have to
    # emulate an 'end' cmd
    # pod_par means: 0 - begin of para, 1 - flowing text, 2 - literal text
    assert self.mode == 'pod'
    if not pod:
      line = string.rstrip(data)
      if line:
        if not self.pod_par:
          self.pod_par = (line[0] in ' \t')+1
          if self.pod_par == 1: self.flow_text = ''
        if self.pod_par-1:
          self.weaver.writecode(line)
        else:
          # we have to search for escapes here!
          self.flow_text = self.flow_text + line + ' '
      elif self.pod_par:
        self.flow_escape()
        self.weaver.par()
        self.pod_par = 0 # beginning of paragraph
      return

    # we've got a pod command, so turn para detection off
    assert pod
    self.pod_par = 0
    cmd = pod.group(1)

    # if we're cuttiung back to code, terminate lists and list
    # items correctly if nececcary and switch back to code mode

    if cmd == 'cut':
      self.end_lists()
      if hasattr(self,'pod_mode'):
        if self.pod_mode in ['lit','litpar']:
          self.weaver.enable() # disable rawmode
          self.weaver.translate() # disable rawmode
        del self.pod_mode
      self.mode = 'code'
      return

    # Otherwise, just process the command

    if cmd == 'head1':
      self.end_lists()
      self.weaver.head(1+self.heading_level_offset, pod.group(2))

    elif cmd == 'head2':
      self.end_lists()
      self.weaver.head(2+self.heading_level_offset, pod.group(2))

    elif cmd == 'over':
      # list of unknown type pending, wait for =item
      self.pod_mode = 'list'

    elif cmd == 'back':
      self.end_list_item()
      self.end_list()

    elif cmd == 'item':
      if not hasattr(self,'pod_mode'):
        if 'tanglers' in self.process.trace:
          print 'POD: item before over'
        self.pod_mode = 'list'
      key = pod.group(2)
      key = string.strip(key)
      if self.pod_mode == 'item':
        self.end_list_item()
      else:
        self.pod_mode = 'item'
        list_type = 'keyed'
        if len(key)==1:
          if key in '*+.-':
            list_type = 'bullet'
        self.begin_list(list_type)
      if self.list_type[-1] == 'keyed':
        # interscript doesn't support formatting of any kind
        # in keyed list keys (because LaTeX doesn't)
        # we need another kind of list (LaTeX can be given one)
        # For now, we remove any X<...> stuff
        stripkey = ''
        tail = key
        match = self.esc_re.match(tail)
        while match:
          pre, cmd, tail = match.group(1,2,3)
          stripkey = stripkey + pre
          match = self.esc_re.match(tail)
        if tail: stripkey = stripkey + tail
        key = stripkey

      self.begin_list_item(key)

    elif cmd == 'for':
      self.weaver.raw_if(pod.group(2))
      self.pod_mode = 'litpar'
    elif cmd == 'begin':
      self.weaver.raw_if(pod.group(2))
      self.pod_mode = 'lit'
    elif cmd == 'end':
      self.weaver.enable()
      self.weaver.translate()
      self.weaver.pod_mode = ''

  def write_comment(self,line):
    self._writeline('# '+line)

  def start_section(self, file, count):
    data = '#line '+str(count)+' '+'"'+file+'"'
    self._writeline(data)
    self.weaver.echotangle(self.sink.lines_written,data)

  def get_comment_tangler(self):
    return hash_comment_tangler(self.sink,weaver, '# ')

  def get_string_tangler(self,eol,width):
    # This is _wrong_ and needs to be fixed!
    return c_string_tangler(self.sink,self.get_weaver(),eol,width)

