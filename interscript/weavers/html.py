#line 21 "interscript/src/html_weaver.ipk"
from interscript.weavers.base import weaver_base
import string
def cvt_code(line):
  l = ''
  for ch in line:
    if ch in '<>':
      l = l + {'<':'&lt;','>':'&gt;','&':'&amp;'}[ch]
    else:
      l = l + ch
  return l

def cvt_text(line):
  l = ''
  for ch in line:
    if ch in '<>&':
      l = l + {'<':'&lt;','>':'&gt;','&':'&amp;'}[ch]
    else:
      l = l + ch
  return l

from interscript.drivers.sinks.bufdisk import named_file_sink
def mk_html(pass_frame,basename, directory, prefix, eol, title, language):
  if language: filename = directory + language + '_' + basename + '.html'
  else: filename = directory + basename + '.html'
  w = named_file_sink(pass_frame,filename, prefix, eol=eol)
  if not title: title = basename
  return html_weaver(pass_frame,w,title=title,language=language)

class html_weaver(weaver_base):

#line 53 "interscript/src/html_weaver.ipk"
  def __init__(self, pass_frame,writer,language='',**kwds):
    weaver_base.__init__(self,pass_frame,language)
    if 'weavers' in self.process.trace:
      print 'html weaver, writer',writer.get_sink_name()
    self.protocol = 'text/html'
    self.persistent_frame['protocol']=self.protocol
    self.sink = writer
    self.acount = 1
    self.hcount = [0]
    self.mode = None
    self.comments = 0
    self.list = []
    self.name = 'html weaver v1 for '+self.sink.name
    self.persistent_frame['name']=self.name
    self.heading_level_offset = 0
    self.keywords = kwds
    self.tags.append('html') # this 'tags' has nothing to do with html tags!
    self.toc = []
    if kwds.has_key('title'):
      self.title=kwds['title']
    else:
      title = self.sink.name

    self.prolog()

#line 80 "interscript/src/html_weaver.ipk"
  def __del__(self):
    self.epilog()
    self.persistent_frame['contents']=self.toc
    if 'weavers' in self.process.trace:
      self.process.release_object(self)

#line 88 "interscript/src/html_weaver.ipk"
  def _setmode(self,mode):
    self._write('\n<'+mode+'>')
    self.mode = mode

  def _endmode(self):
    if self.mode:
      self._write('</'+self.mode+'>\n')
      self.mode = None

  def _startmode(self,mode):
    self._endmode()
    self._setmode(mode)

  def _ensuremode(self,mode):
    if self.mode != mode : self._startmode(mode)

  def _writeline(self,line=''):
    if self.enabled: self.sink.writeline(line)

  def _write(self,line):
    if self.enabled: self.sink.write(line)

  def writeline(self,line=''):
    self.write(line + '\n')

  def write(self,line):
    #hack to correct bug in popular broswers
    #if not self.mode: self._setmode('P')
    if self.translating:
      self._write(cvt_text(line))
    else:
      self._write(line)

  def writecode(self,line):
    self._ensuremode('PRE')
    self._writeline(cvt_code(line))

  def begin_displayed_text(self):
    self._ensuremode('P')
    # note this is HTML 2, HTML 3 uses BQ instead
    self._write('<BLOCKQUOTE>')

  def end_displayed_text(self):
    self._write('</BLOCKQUOTE>')

  def begin_displayed_code(self,DIV="CODE"):
    self._write('<DIV CLASS="'+DIV+'"><PRE>\n')

  def end_displayed_code(self):
    self._write('</PRE></DIV>')

  def line_break(self):
    self._writeline('<BR>')

  def page_break(self):
    self._writeline('<BR><HR>')

  def write_tagged(self,tag, data):
    self._write('<'+tag+'>')
    self._writeline(data)
    self._write('</'+tag+'>')

  def label_chunk(self, filename):
    self._ensuremode('PRE')
    self._write('<I>include</I> <STRONG>')
    self._writeline(cvt_code(filename)+'</STRONG>')

  def _write_section_ref(self, filename, index):
    name = filename + '['+str(index+1)+']'
    anchor = '<A HREF="'+self.get_anchor(name)+'">'+str(index+1)+'</A>'
    self._writeline (anchor+' ')

  def code_head(self,tangler, secno):
    if tangler:
      self._endmode()
      filename =tangler.sink.get_sink_name()
      language = tangler.get_language()
      w = self._writeline
      w ( '<DIV CLASS="CODE_SECTION_HEAD"><SMALL>Start <EM>'+\
        language+'</EM> section to <STRONG>'+\
        filename+'['+str(secno)+']</STRONG></SMALL>')
      dict = self.master.section_index
      if dict.has_key(filename):
        nsections = len(dict[filename])
        for i in range(nsections):
          self._write_section_ref(filename, i)
      w ('</DIV>')
      w ( '<DIV CLASS="CODE">')


  def code_foot(self,tangler, secno):
    if tangler:
      self._endmode()
      filename =tangler.sink.get_sink_name()
      language = tangler.get_language()
      self._write( '</DIV><DIV CLASS="CODE_SECTION_FOOT"><SMALL>End <EM>'+\
        language+'</EM> section to <STRONG>'+\
        filename+'['+str(secno)+']</STRONG></SMALL></DIV>')

  def script_head(self,language,filename):
      self._endmode()
      self._writeline( '<DIV CLASS="CODE_SECTION_HEAD"><SMALL>Start <EM>'+\
        language+'</EM> section from <STRONG>'+\
        filename+'</STRONG></SMALL></DIV>')
      self._writeline( '<DIV CLASS="CODE">')

  def script_foot(self,language,filename):
      self._endmode()
      self._write( '</DIV><DIV CLASS="CODE_SECTION_FOOT"><SMALL>End <EM>'+\
        language+'</EM> section from <STRONG>'+\
        filename+'</STRONG></SMALL></DIV>')

  def test_output_head(self,command, status):
    self._endmode()
    self._writeline( '<DIV CLASS="TEST_OUTPUT_SECTION_HEAD"><SMALL>Start <EM>'+\
      'output</EM> section of <STRONG>'+\
      cvt_code(command)+'</STRONG></SMALL></DIV>')
    if status:
      self._writeline( '<DIV CLASS="TEST_OUTPUT_RESULT">'+\
        '<BIG>Command returned <STRONG>'+\
        str(status)+'</STRONG></BIG></DIV>')
    if status: div_class = 'BAD_TEST_OUTPUT'
    else: div_class = 'TEST_OUTPUT'
    self._writeline( '<DIV CLASS="'+div_class+'">')

  def test_output_foot(self,command,status):
    self._endmode()
    self._writeline( '</DIV><DIV CLASS="TEST_OUTPUT_SECTION_FOOT">')
    self._writeline('<SMALL>End <EM>output</EM> section to <STRONG>'+\
      cvt_code(command)+'</STRONG></SMALL></DIV>')

  def expected_head(self,command):
    self._endmode()
    self._writeline( '<DIV CLASS="EXPECTED_OUTPUT_SECTION_HEAD">'+\
      '<SMALL>Start <EM>expected</EM> section of <STRONG>'+\
      cvt_code(command)+'</STRONG></SMALL></DIV>')
    div_class = 'EXPECTED_OUTPUT'
    self._writeline( '<DIV CLASS="'+div_class+'">')

  def expected_foot(self,command):
    self._endmode()
    self._writeline( '</DIV><DIV CLASS="EXPECTED_OUTPUT_SECTION_FOOT">')
    self._writeline('<SMALL>End <EM>expected</EM> section to <STRONG>'+\
      cvt_code(command)+'</STRONG></SMALL></DIV>')

  def diff_head(self,command):
    self._endmode()
    self._writeline( '<DIV CLASS="DIFF_SECTION_HEAD"><SMALL>Start <EM>diff</EM> section of <STRONG>'+\
      cvt_code(command)+'</STRONG></SMALL></DIV>')
    div_class = 'DIFF'
    self._writeline( '<DIV CLASS="'+div_class+'">')

  def diff_foot(self,command):
    self._endmode()
    self._writeline( '</DIV><DIV CLASS="DIFF_SECTION_FOOT">')
    self._writeline('<SMALL>End <EM>diff</EM> section to <STRONG>'+\
      cvt_code(command)+'</STRONG></SMALL></DIV>')

#line 248 "interscript/src/html_weaver.ipk"
  def set_anchor(self, label):
    self._write('<A NAME="'+label+'"></A>')

  def get_anchor(self, label):
    return '#'+label

  def ref_anchor(self, label):
    href = self.get_anchor(label)
    if href:
      self._write('<A HREF="'+href+'">'+label+'</A>')
    else:
      self._write('<EM>Unknown Label:'+label+'</EM>')

#line 263 "interscript/src/html_weaver.ipk"
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
    atext=kwds.get('short_text',text)
    anchor=kwds.get('key','')
    self._endmode()
    myanchor = anchor
    if myanchor == '': myanchor = atext
    if myanchor == '':
      myanchor = 'h'+str(self.acount)
      self.acount = self.acount + 1

    self.new_heading(level)
    hnumber = self.get_formatted_heading_number('.')+'. '
    hprefix = ''
    if self.keywords.has_key('heading_prefix'):
      hprefix = self.keywords['heading_prefix']
    ahref = '<A HREF="#'+anchor+'">'+hprefix+hnumber+str(text)+'</A>'

    html_level = level + self.heading_level_offset
    if html_level > 6: html_level = 6
    if anchor: atag = ' ID="'+anchor+'" TITLE="'+anchor+'"'
    else: atag = ' ID="'+myanchor+'"'
    self._writeline( '<H'+str(html_level)+atag+'>'+
      hprefix+hnumber+str(text)+\
      '</H'+str(html_level)+'>')

    counter = self.hcount[level-1]
    self.toc.append((level,ahref))

#line 325 "interscript/src/html_weaver.ipk"
  def begin_numbered_list(self,start=1, type='1'):
    self._endmode()
    self._write('<OL SEQNUM="'+str(start)+'" TYPE="'+type+'">')

  def end_numbered_list(self):
    self._write('</OL>')

  def begin_numbered_list_item(self):
    self._write('<LI>')

  def end_numbered_list_item(self):
    self._write('</LI>')

#line 340 "interscript/src/html_weaver.ipk"
  def begin_bullet_list(self):
    self._endmode()
    self._write('<UL>')

  def end_bullet_list(self):
    self._write('</UL>')

  def begin_bullet_list_item(self):
    self._write('<LI>')

  def end_bullet_list_item(self):
    self._endmode()
    self._write('</LI>')

#line 356 "interscript/src/html_weaver.ipk"
  def begin_keyed_list(self):
    self._endmode()
    self._write('<DL>')

  def end_keyed_list(self):
    self._write('</DL>')

  def begin_keyed_list_item(self,key):
    self._write('<DT><B>'+key+'</B></DT><DD>')

  def end_keyed_list_item(self):
    self._write('</DD>')

#line 371 "interscript/src/html_weaver.ipk"
  # default code line formatting
  def echotangle(self,count,data):
    if self.comments:
      self._writeline(data)
    else:
      self.start_code_line(count)
      self._writeline(cvt_code(data))

  def start_code_line(self, count=None):
    self._ensuremode('PRE')
    if count:
      self._write('<SPAN CLASS="LINENO">%6d: </SPAN>' % count)
    else:
      self._write('<SPAN CLASS="LINENO">      + </SPAN>')

  def end_code_line(self): self._writeline()

  def write_code_fragment(self,fragment, kind=None):
    if kind:
      self._write('<SPAN CLASS="'+kind+'">')
    self._write(cvt_code(fragment))
    if kind:
      self._write('</SPAN>')

#line 397 "interscript/src/html_weaver.ipk"
  def cite_url(self,url):
    self._write('<A HREF="'+url+'">'+url+'</A>')

#line 402 "interscript/src/html_weaver.ipk"
  def prose(self): # start of paragraph
    self._ensuremode('P')

  def par(self): # paragraph separator
    self._endmode()
    self._ensuremode('P')

  def eop(self): # end of paragraph
    self._endmode()

  def write_comment(self,v):
    saved_mode = self.mode
    self.write_tagged('SMALL',v)
    self._ensuremode(saved_mode)

#line 419 "interscript/src/html_weaver.ipk"
  def begin_code(self):
    self._write('<CODE>')

  def end_code(self):
    self._write('</CODE>')

  def begin_emphasize(self):
    self._write('<EM>')

  def end_emphasize(self):
    self._write('</EM>')

  def begin_strong(self):
    self._write('<STRONG>')

  def end_strong(self):
    self._write('</STRONG>')

  def begin_italic(self):
    self._write('<I>')

  def end_italic(self):
    self._write('</I>')

  def begin_bold(self):
    self._write('<B>')

  def end_bold(self):
    self._write('</B>')

  def begin_big(self):
    self._write('<BIG>')

  def end_big(self):
    self._write('</BIG>')

  def begin_small(self):
    self._write('<SMALL>')

  def end_small(self):
    self._write('</SMALL>')

#line 463 "interscript/src/html_weaver.ipk"
  def identifier_reference(self, hlevel=2, *args, **kwds):
    ids = self.master.ids
    if not ids:
      ids = self.pass_frame.ids
    keys = ids.keys()
    keys.sort()
    if hlevel>0:
      self.head(hlevel,'Index of Identifiers')
    self._writeline('<TABLE COLS="2" BORDER="1" CELLPADDING="2">')
    for k in keys:
      refs = ids[k]
      self._write('<TR><TD VALIGN="Top"><CODE> '+k+' </CODE></TD><TD> ')
      old_df = ''
      for sf,sc,df,dc in refs:
        if old_df != '': self._write(', ')
        if old_df != df:
          self._write(df+': <A HREF=#'+sf+':'+str(sc)+'>'+str(dc)+'</A>')
          old_df = df
        else:
          self._write('<A HREF=#'+sf+':'+str(sc)+'>'+str(dc)+'</A>')
      self._write('</TD></TR>')
    self._writeline('</TABLE>')

#line 488 "interscript/src/html_weaver.ipk"
  def class_reference(self, hlevel=2, *args, **kwds):
    ids = self.master.classes
    if not ids:
      ids = self.pass_frame.classes
    keys = ids.keys()
    keys.sort()
    if hlevel>0:
      self.head(hlevel,'Index of Classes')
    self._writeline('<TABLE COLS="2" BORDER="1" CELLPADDING="2">')
    for k in keys:
      refs = ids[k]
      self._write('<TR><TD VALIGN="Top"><CODE> '+k+' </CODE></TD><TD> ')
      old_df = ''
      for sf,sc,df,dc in refs:
        if old_df != '': self._write(', ')
        if old_df != df:
          self._write(df+': <A HREF=#'+sf+':'+str(sc)+'>'+str(dc)+'</A>')
          old_df = df
        else:
          self._write('<A HREF=#'+sf+':'+str(sc)+'>'+str(dc)+'</A>')
      self._write('</TD></TR>')
    self._writeline('</TABLE>')

  # create an HTML anchor
  def set_fc_anchor(self,file,count):
    self._write('<A NAME="'+file+':'+str(count)+'"></A>')

#line 517 "interscript/src/html_weaver.ipk"
  def print_contents(self, hlevel=2, maxlev=3, *args, **kwds):
    if hlevel>0:
      self.head(hlevel,'Contents')
    toc = self.persistent_frame.get('contents',[])
    self._write('<PRE>')
    for level, line in toc:
      if level <=maxlev:
        self._writeline(' '*(level*3)+line)
    self._writeline('</PRE>')

#line 529 "interscript/src/html_weaver.ipk"
  def print_file_list(self,hlevel=2, *args, **kwds):
    if hlevel>0:
      self.head(hlevel,'File List')
    if self.master.flist:
      for line in self.master.flist:
        self._writeline(line + '<BR>')
    else:
      self._writeline('<P>No data available in pass '+str(self.pass_frame.passno)+'.</P>')

#line 540 "interscript/src/html_weaver.ipk"
  def print_file_status(self,hlevel=2, *args, **kwds):
    passno = self.pass_frame.passno
    h = 'File Status for pass '+str(passno-1)
    if hlevel>0:
      self.head(hlevel,h)
    if self.master.fdict:
      skeys = self.master.fdict.keys()
      skeys.sort()

      h = 'Unchanged Files'
      if hlevel>0:
        self.head(hlevel+1,h)
      else:
        self._writeline('<STRONG>'+h+'<STRONG><BR>')
      for key in skeys:
        status,change_passno = self.master.fdict[key]
        if status == 'unchanged' and change_passno==0:
          self._writeline(key+'<BR>')

      h = 'Changed Files'
      if hlevel>0:
        self.head(hlevel+1,h)
      else:
        self._writeline('<STRONG>'+h+'<STRONG><BR>')
      for key in skeys:
        status,change_passno = self.master.fdict[key]
        if status == 'unchanged' and change_passno == 1:
          self._writeline(key+'<BR>')

      h = 'Files which required 2 or more passes to converge'
      if hlevel>0:
        self.head(hlevel+1,h)
      else:
        self._writeline('<STRONG>'+h+'<STRONG><BR>')
      for key in skeys:
        status,change_passno = self.master.fdict[key]
        if status == 'unchanged' and change_passno > 1:
          self._writeline(key+' (converged in '+str(change_passno)+' passes)<BR>')

      h = 'Unstable Files'
      if hlevel>0:
        self.head(hlevel+1,h)
      else:
        self._writeline('<STRONG>'+h+'<STRONG><BR>')
      for key in skeys:
        status,change_passno = self.master.fdict[key]
        if status == 'changed':
          self._writeline(key+'<BR>')

    else:
      self._writeline('<P>No data available in pass '+str(passno)+'.</P>')

#line 594 "interscript/src/html_weaver.ipk"
  def print_source_list(self, hlevel=2, *args, **kwds):
    if hlevel>0:
      self.head(hlevel,'Source List')
    if self.master.iflist:
      for line, count in self.master.iflist:
        self._writeline(line + ': '+str(count)+'<BR>')
    else:
      self._writeline('<P>No data available in pass '+str(self.pass_frame.passno)+'.</P>')

#line 605 "interscript/src/html_weaver.ipk"
  def print_include_list(self, hlevel=2, *args, **kwds):
    if hlevel>0:
      self.head(hlevel,'Include List')
    if self.master.include_files:
      for level, type, name in self.master.include_files:
        self._writeline('&nbsp;'*(level*3)+' '+type+': '+name+ '<BR>')
    else:
      self._writeline('<P>No data available in pass '+str(self.pass_frame.passno)+'.</P>')

#line 616 "interscript/src/html_weaver.ipk"
  def begin_table(self, *headings, **kwds):
    border=kwds.get('border',2)
    tbclass = kwds.get('CLASS','DEFAULT_TABLE_CLASS')
    self._writeline('<TABLE CLASS="'+tbclass+'" COLS="'+str(len(headings))+'" BORDER="'+str(border)+'"><TR>')
    for h in headings:
      self._write('<TH>')
      self.write(h)
      self._write('</TH>')
    self._writeline('</TR>')

  def table_row(self,data):
    self._write('<TR>')
    for d in data:
      self._write('<TD VALIGN="TOP">')
      if d:
        lines = string.split(d,'\n')
        for line in lines[:-1]:
          self.write(line)
          self._write('<BR>')
        if len(lines): self.write(lines[-1])
      self._write('</TD>')
    self._writeline('</TR>')

  def end_table(self):
    self._writeline('</TABLE>')

  def begin_table_row(self):
    self._write('<TR>')

  def end_table_row(self):
    self._write('</TR>')

  def begin_table_cell(self):
    self._write('<TD>')

  def end_table_cell(self):
    self._write('</TD>')

#line 656 "interscript/src/html_weaver.ipk"
  def prolog(self):
    kwds = self.keywords
    w = self._writeline
    w('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">')
    w( '<HTML>')
    w( '<HEAD>')

    self.write_tagged('TITLE', self.title)

    w( '<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">')
    if kwds.has_key('author'):
      author =kwds['author']
      w( '<META NAME="Author" CONTENT="'+author+'">')
    w( '<META NAME="Generator" CONTENT="Interscript">')
    extra = kwds.get('head_extra','');
    w('<LINK REL=STYLESHEET TYPE="text/css" HREF="interscript.css" TITLE="Interscript Standard">')
    w(extra);
    w('<LINK REL=STYLESHEET TYPE="text/css" HREF="user.css" TITLE="User Overrride">')
    w( '</HEAD>')
    w( '<BODY LANG="'+self.language+'">')
    if kwds.has_key('pagehead'):
      self._write(kwds['pagehead'])
    if kwds.has_key('title'):
      if kwds.has_key('anchor') and kwds['anchor']!='':
        atag = ' ID="'+kwds['anchor']+'" TITLE="'+kwds['anchor']+'" '
      else: atag= ''
      self._writeline('<H1 '+atag+'ALIGN="CENTER">'+self.title+'</H1>')
    if kwds.has_key('heading_level_offset'):
      self.heading_level_offset = kwds['heading_level_offset']

  def epilog(self):
    kwds = self.keywords
    self._endmode()
    if kwds.has_key('pagefoot'):
      self._write(kwds['pagefoot'])
    self._writeline('</BODY>')
    self._writeline('</HTML>')


