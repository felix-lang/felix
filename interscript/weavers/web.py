#line 87 "interscript/src/web_weaver.ipk"
from interscript.weavers.multiplexor import multiplexor
from interscript.weavers.html import html_weaver
from interscript.weavers.html import cvt_code, cvt_text
from interscript.drivers.sinks.bufdisk import named_file_sink
from interscript.drivers.sinks.disk import simple_named_file_sink
from interscript.core.sets import set
from interscript.languages.interscript_languages import tr_phrase
from interscript.core.stacks import stack

import string
import traceback

def munge2filename(x):
  s = ''
  ids = string.letters + string.digits + '_'
  for ch in x:
    if ch in ids: s = s + ch
    elif ch == ' ': s = s + '_'
    else: s = s + hex(ord(ch))
  return s

def mk_web(pass_frame,basename, directory, prefix, eol, title, language):
  if language: filename = directory + language + '_' + basename + '.html'
  else: filename = directory + basename + '.html'
  w = named_file_sink(pass_frame,filename, prefix, eol=eol)
  if not title: title = basename
  w = html_weaver(pass_frame,w,title=title,language=language)
  if language: pattern = language+'_'+basename+'_%s.html'
  else: pattern = basename+'_%s.html'
  return stacking_weaver(w,pattern,(1,2,3,4,5,6,7,8,9,10),language=language)


class stacking_weaver(multiplexor):
  def __init__(self,parent_weaver, pattern='', break_list=None, language='', **kwds):
    self.base = [parent_weaver]
    self.pass_frame = parent_weaver.pass_frame
    self.master = self.pass_frame.master
    self.process = self.pass_frame.process
    self.language = language
    self.sequence = self.pass_frame.get_new_sequence_number()
    self.persistent_frame = self.master.get_persistent_frame(self.sequence)
    self.persistent_frame['language']=language
    if 'weavers' in self.process.trace:
      self.process.acquire_object(self,'WEB WEAVER')
      print('language=',language)
    self.tags = ['web','html']
    if language: self.tags.append(language)
    self.tr_phrase = lambda x,y=language: tr_phrase(x,y)
    self.debug_missing_methods = 0
    self.keywords = kwds

    self.original_filename = 'Unknown'

    self.toc_depth = self.keywords.get('toc_depth',99)
    parent_sink = parent_weaver.sink.name
    self.basedir = '/'.join(parent_sink.split('/')[:-1])+'/'
    if self.basedir == '/': self.basedir = ''
    #print 'Base directory for stacking weaver is',self.basedir
    self.home_file = parent_sink.split('/')[-1]
    if 'title' in kwds:
      self.title = kwds['title']
    elif hasattr(parent_weaver,'title'):
      self.title = parent_weaver.title
    else:
      self.title = self.home_file
    self.pattern = pattern
    if string.find(pattern,'%') == -1:
      self.pattern = self.pattern+'_%s.html'
    # break list
    self.break_list = [0]
    if break_list:
      for b in break_list: self.break_list.append(b)
    else:
      for i in range(1,40): self.break_list.append(i)

    self.stack = [parent_weaver]
    self.childcount = 0
    self.debug = 0
    self.protocol = 'web'
    self.persistent_frame['protocol']=self.protocol
    self.hcount = [1,0]
    self.acount = 0
    self.anchor_file = {}
    self.name = 'web weaver for '+self.pattern
    self.persistent_frame['name']=self.name
    self.eol = parent_weaver.sink.eol
    self.subdoc_stack = stack((0,None))
    # table of contents
    self.toc = []

    self.home_anchor = '<A HREF="'+self.home_file+'">'+self.tr_phrase('Home')+'</A>'
    self.home_nav = '<DIV CLASS="NAVIGATION">'+self.home_anchor+'<BR><HR></DIV>'

    #self.mk_frames(self.home_file)

    # cheat here, guess next exists :-)
    next = self.pattern%('%04d'%(self.childcount+1))
    next = '<A HREF="'+next+'">'+self.tr_phrase('Next')+'</A>'

    nav = ''
    if next : nav = nav + next+' '

    hnav = '<HR><DIV CLASS="NAVIGATION">'+nav+'<BR><HR></DIV>'
    parent_weaver._writeline(hnav)

  def mk_head(self, sink):
    w = sink.writeline
    w('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">')
    w( '<HTML>')
    w( '<HEAD>')

    w('<TITLE>'+self.title+'</TITLE>')

    w( '<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">')
    if 'author' in self.keywords:
      author =self.keywords['author']
      self.frames_sink.writeline( '<META NAME="Author" CONTENT="'+author+'">')
    version = self.process.global_frame.version

    w( '<META NAME="Generator" CONTENT="Interscript '+version+'">')
    w( '</HEAD>')

  def mk_frame(self,name, orientation, subframes):
    filename = self.pattern % name
    sink = named_file_sink(
      self.pass_frame,
      self.basedir+filename,
      self.master.weaver_prefix,
      eol=self.eol)
    self.mk_head(sink)
    pc = ''
    for frame_name,frame_percent,frame_src in subframes:
      pc = pc + str(frame_percent)+'%,'
    pc = pc[:-1]
    w = sink.writeline
    w( '<FRAMESET '+orientation+'="'+pc+'" scrolling="yes">')
    for frame_name,frame_percent,frame_src in subframes:
      w( '<FRAME NAME="'+frame_name+'" SRC="'+frame_src+'">')
    w( '</FRAMESET>')
    w( '</HTML>')

  def mk_frames(self, doc,tables):
    top = self.pattern % 'top' # top level frame
    indexview = self.pattern % 'indexview'
    indexnav = self.pattern % 'indexnav'

    contents = self.pattern % 'contents'
    #classref = self.pattern % 'classref'
    #funcref = self.pattern % 'funcref'
    #identref = self.pattern % 'identref'
    #testref = self.pattern % 'testref'
    #filestatus = self.pattern % 'filestatus'
    #sectionref = self.pattern % 'sectionref'
    #sourceref = self.pattern % 'sourceref'
    #metricsref = self.pattern % 'metricsref'
    #noticesref = self.pattern % 'noticesref'

    self.mk_frame('indexview','ROWS', (
      ('indexnav',20,indexnav),
      ('indexdat',80,contents)))
    self.mk_frame('top','COLS',(
      ('indexview',30,indexview),
      ('docview',70,doc)))

    nav = '';
    for name, descr, target, count in tables:
      if count:
        filename = self.pattern % name
        nav = nav + '<A HREF="'+filename+'" TARGET="indexdat">'+self.tr_phrase(descr)+'</A> '
    ha = '<A HREF="'+self.home_file+'" TARGET="docview">'+self.tr_phrase('Home')+'</A>'
    nav = '<DIV CLASS="NAVIGATION">'+nav+' '+ha+'</DIV>'

    filename = self.pattern % 'indexnav'
    sink = named_file_sink(
      self.pass_frame,
      self.basedir+filename,
      self.master.weaver_prefix,
      eol=self.eol)
    self.mk_head(sink)
    w = sink.writeline
    w ('<BODY LANG="'+self.language+'">')
    w (nav)
    w ('</BODY>')
    w ('</HTML>')


  def print_table(self,dict,sink):
    keys = list(dict.keys())
    keys.sort()
    w = sink.writeline
    w('<TABLE COLS="2" BORDER="1" CELLPADDING="2">')
    for k in keys:
      refs = dict[k]
      w('<TR><TD VALIGN="Top"><CODE> '+k+' </CODE></TD><TD> ')
      old_df = ''
      for sf,sc,df,dc in refs:
        key = (sf, sc)
        if key in self.anchor_file:
          child = self.anchor_file[key]
        else:
          child = ''
        anchor = '<A HREF='+child+'#'+sf+':'+str(sc)+' TARGET="docview">'+str(dc)+'</A>'

        if old_df != '': w(', ')
        if old_df != df:
          w(df+': '+ anchor )
          old_df = df
        else:
          w(anchor)
      w('</TD></TR>')
    w('</TABLE>')

#line 301 "interscript/src/web_weaver.ipk"
  def set_original_filename(self, filename):
    self.original_filename = filename

  def set_anchor(self, label):
    # the rubbish below yields the current filename plus #label
    # the filename should be calculated exactly once on instantiation:
    # this mechanism will fail if the formula for finding the filename
    # changes or a special filename is used for some reason
    href = self.pattern%('%04d'%(self.childcount))+'#'+label
    self._write('<A NAME="'+label+'"></A>')
    self.register_anchor(label, href)

  def register_anchor(self, label, anchor):
    if 'anchors' not in self.persistent_frame:
      self.persistent_frame['anchors']  = {}
    self.persistent_frame['anchors'][label]=anchor

  def get_anchor(self, label):
    href = None
    if 'anchors' in self.persistent_frame:
      if label in self.persistent_frame['anchors']:
        href =self.persistent_frame['anchors'][label]
    return href

  def ref_anchor(self, label):
    href = self.get_anchor(label)
    if href:
      self._write('<A HREF="'+href+'">'+label+'</A>')
    else:
      self._write('<EM>Unknown Label:'+label+'</EM>')

#line 335 "interscript/src/web_weaver.ipk"
  def _write_section_ref(self, filename, i, text=None):
    if not text: text = str(i)
    name = filename + '['+str(i)+']'
    anchor = self.get_anchor(name)
    if anchor:
      anchor = '<A HREF="'+anchor+'">'+text+'</A>'
      self._writeline (anchor+' ')

  def code_head(self,tangler, secno):
    dst_filename = tangler.sink.name
    self.set_anchor(dst_filename+'['+str(secno)+']')
    self._endmode()
    filename =tangler.sink.get_sink_name()
    language = tangler.get_language()
    dict = self.master.section_index
    if filename in dict:
      nsections = len(dict[filename])
    else: nsections = 0
    w = self._write
    wl = self._writeline
    w ( '<DIV CLASS="CODE_SECTION_HEAD"><SMALL>'+self.tr_phrase('Start')+' <EM>'+\
      language+'</EM> section to <STRONG>'+\
      filename+'['+str(secno))
    if nsections: w('/'+str(nsections))
    w(']</STRONG></SMALL>')

    if nsections:
      if secno != nsections:
        self._write_section_ref(filename, secno+1, self.tr_phrase('Next'))
      if secno!=1:
        self._write_section_ref(filename, secno-1, self.tr_phrase('Prev'))
      if secno !=1:
        self._write_section_ref(filename, 1, self.tr_phrase('First'))
      if secno != nsections:
        self._write_section_ref(filename, nsections, self.tr_phrase('Last'))
    wl ('</DIV>')
    wl ( '<DIV CLASS="CODE">')

#line 384 "interscript/src/web_weaver.ipk"
  def _current_level(self): return len(self.hcount)-1
  def _pop_level(self): del self.hcount[-1]
  def _push_level(self): self.hcount.append(0)
  def _next_hnum(self): self.hcount[-1] = self.hcount[-1] + 1

  def head(self,level, text, **kwds):
    oldsubdoc = self.subdoc_stack.top
    while level <= oldsubdoc[0]:
      self.pattern = oldsubdoc[1]
      self.childcount = oldsubdoc[2]
      #print 'Resetting pattern to',self.pattern,'count to',self.childcount
      self.subdoc_stack.pop()
      oldsubdoc = self.subdoc_stack.top

    atext=kwds.get('short_text','')
    anchor=kwds.get('key','')
    while level > self._current_level():
      self._head(text, atext,None,None)  # synthesised dummy heading
      self._push_level()
    while level < self._current_level():
      self._pop_level()
      self._foot()
    assert level == self._current_level()
    self._next_hnum()
    if anchor:
      self.subdoc_stack.push((level,self.pattern, self.childcount))
      self.childcount = 0
      self.pattern = munge2filename(anchor) +'_%s.html'
      if self.language: self.pattern = self.language+'_'+self.pattern
      #print 'Setting pattern to',self.pattern,'count to',self.childcount
    nospawn = kwds.get('nospawn',None)
    self._head(text, atext, anchor, nospawn)
    self._push_level()
    assert self._current_level() == level + 1

#line 423 "interscript/src/web_weaver.ipk"
  # if the current page has a break level equal to the current level pop it
  # (we'd print a footer first if we used them)
  def _foot(self):
    if self._trig(0) == self._current_level():
      self._popw()

  def _head(self,text, atext, anchor,nospawn):
    level = self._current_level()
    if level == self._trig(1) and not nospawn:
      self._pushw(level,text,atext,anchor)
    else:
     self._ins_head(level,text,atext,anchor)

  def __del__(self):
    if 'weavers' in self.process.trace:
      print("WEB WEAVER DESTRUCTOR COMMENCES")
    del self.base
    while self.stack: del self.stack[-1]
    oldsubdoc = self.subdoc_stack.top
    while 0 < oldsubdoc[0]:
      self.pattern = oldsubdoc[1]
      self.childcount = oldsubdoc[2]
      print('Resetting pattern to',self.pattern,'count to',self.childcount)
      self.subdoc_stack.pop()
      oldsubdoc = self.subdoc_stack.top

    tables = [
      ['contents','Contents','docview',0],
      ['funcref','Functions','docview',0],
      ['classref','Classes','docview',0],
      ['identref','Identifiers','docview',0],
      ['testref','Tests','docview',0],
      ['sectionref','Sections','docview',0],
      ['sourceref','Source','docview',0],
      ['metricsref','Metrics','docview',0],
      ['noticesref','Notices','docview',0],
      ['filestatus','Status','docview',0]
      ]
    for i in range(len(tables)):
      name, descr, target, count = tables[i]
      s = 'tables['+str(i)+'][3]=self.mk_'+name+'(self.pattern % '+repr(name)+','+repr(target)+')'
      try:
        exec(s)
      except:
        print('Error generating table',name,descr)
        traceback.print_exc()
    self.persistent_frame['contents'] = self.toc
    self.mk_frames(self.home_file, tables)
    if 'weavers' in self.process.trace:
      print('Web Weaver finished')
    if 'weavers' in self.process.trace:
      self.process.release_object(self)

  def format_heading_number(self, hlist, sep):
    hnumber = ''
    for i in range(1,len(hlist)-1):
      hnumber = hnumber + str(hlist[i])+'.'
    hnumber = hnumber + str(hlist[-1])
    return hnumber

  def get_formatted_heading_number(self, sep):
    return self.format_heading_number(self.hcount,sep)


  def anchor_text(self,anchor,label,tooltip):
    return '<A HREF="'+anchor+'" TITLE="'+tooltip+'">'+\
      self.tr_phrase(label)+'</A>'

  def anchor_of(self, toc, ix, label):
     if ix == None: return None
     if 0<=ix<len(toc):
       level, hnum, anchor, text,hcount = toc[ix]
       return self.anchor_text(anchor, label, text)
     else: return None

  def cal_nav(self,level):
    toc = self.persistent_frame.get('contents',None)
    first, last, next, prev, along, back, up, this = (None,)*8
    n = toc and len(toc)
    if n:
      first = 0
      last = n-1

      this = 0
      for entry in toc:
        if entry[4] == self.hcount: break
        this = this + 1

      if this+1 < n: next = this + 1
      if this>0: prev = this - 1

      along = this + 1
      while along < n:
        nlev = toc[along][0]
        if nlev < level: along = None
        if nlev <= level: break
        along = along + 1

      back = this - 1
      while back >= 0:
        nlev = toc[back][0]
        if nlev < level: back = None
        if nlev <= level: break
        back = back - 1

      up = this -1
      while up >= 0:
        nlev = toc[up][0]
        if nlev < level: break
        up = up - 1

    return toc, this, first, last, next, prev, along, back, up

  def mk_nav(self, lasttoc, this, first, last, next, prev, along, back, up):
    home = self.home_anchor
    up = self.anchor_of(lasttoc,up,'Up')
    prev = self.anchor_of(lasttoc,prev,'Prev')
    next = self.anchor_of(lasttoc,next,'Next')
    along = self.anchor_of(lasttoc,along,'Along')
    back = self.anchor_of(lasttoc,back,'Back')
    this = self.anchor_of(lasttoc,this,'This')

    nav = ''
    if next : nav = nav + next+' '
    if prev : nav = nav + prev+' '
    if along : nav = nav + along+' '
    if back : nav = nav + back+' '
    if up : nav = nav + up+' '
    if home: nav = nav + home+' '
    if this:  nav = nav + this+' '
    nav = nav + '<SMALL>['+self.original_filename+']</SMALL>'

    hnav = '<DIV CLASS="NAVIGATION">'+nav+'<BR><HR></DIV>'
    fnav = '<DIV CLASS="NAVIGATION"><BR><HR>'+nav+'</DIV>'
    return hnav, fnav

  def _popw(self):
    if 'weavers' in self.process.trace:
      print('Terminating weaver',self.stack[-1].name)
    del self.stack[-1]
    self.base = [self.stack[-1]]

  def _new_child(self):
    self.childcount = self.childcount + 1
    return self.pattern % ('%04d' % self.childcount)

  def _pushw(self,level,text,atext,label):
    filename = self._new_child()
    if label:
      anchor = self.pattern%('%04d'%(self.childcount))
      self.register_anchor(label, anchor)

    #up = self.base[0].sink.name.split('/')[-1]
    #up_anchor = '<A HREF="'+up+'">'+self.tr_phrase('Up')+'</A>'
    #this = self.pattern%('%04d'%(self.childcount))
    #this = '<A HREF="'+this+'">'+self.tr_phrase('This')+'</A>'
    #print 'Spawning Weaver',filename,'for',text

    self.base = [self.stack[-1]]
    hn = self.get_formatted_heading_number('.')+'.'
    hnum = self.get_formatted_heading_number('_')
    h = hn + ' '+text
    self.toc.append([level,hnum,filename,h,self.hcount[:]])
    self._ensuremode('P')
    self._writeline( '<BR><A HREF="'+filename+'">'+h+'</A>')

    lasttoc, this, first, last, next, prev, along, back, up = self.cal_nav(level)
    hnav, fnav = self.mk_nav(lasttoc, this, first, last, next, prev, along, back, up)

    extra = ''
    #extra = '<LINK type="text/html" rel="Contents" href="'+self.pattern % 'contents' +'">\r\n'
    #extra = '<LINK type="text/html" rel="Identifiers" href="'+self.pattern % 'identref'+'">\r\n'
    #extra = '<LINK type="text/html" rel="Classes" href="'+self.pattern % 'classref'+'">\r\n'
    #extra = '<LINK type="text/html" rel="Functions" href="'+self.pattern % 'funcref'+'">\r\n'
    #extra = extra + '<LINK type="text/html" rel="Start" href="'+self.home_file+'">\r\n'
    #if next: extra = extra + '<LINK type="text/html" rel="Next" href="'+next+'">\r\n'
    #if prev: extra = extra + '<LINK type="text/html" rel="Prev" href="'+prev+'">\r\n'
    #if up: extra = extra + '<LINK type="text/html" rel="Section" href="'+up+'">\r\n'

    sink = named_file_sink(self.pass_frame, self.basedir + filename, self.master.weaver_prefix, self.eol)
    child = html_weaver(
      self.pass_frame, sink,
      title = h,
      pagehead = hnav,
      pagefoot=fnav,
      heading_prefix=hn,
      head_extra=extra,
      anchor=label,
      language=self.language)
    self.base = [child]
    self.stack.append(child)

  def _trig(self,offset=0):
    doc_level = len(self.stack)-1 # 0 origin
    return self.break_list[doc_level+offset]

  def _ins_head(self,level,text,atext,anchor):
    adjusted_level = level - self._trig()
    if anchor == '':
      anchor = 'a'+str(self.acount)
      self.acount = self.acount + 1
    hn = self.get_formatted_heading_number('.')+'.'
    hnum = self.get_formatted_heading_number('_')
    h = hn + ' '+text
    filename = self.pattern%('%04d'%self.childcount)
    self.register_anchor(anchor,filename+'#'+anchor)
    self.toc.append([level,hnum,filename+'#'+anchor,h,self.hcount[:]])
    if atext:
      for weaver in self.base:
        weaver.head(adjusted_level,text,short_text=atext,key=anchor)
    else:
      for weaver in self.base:
        weaver.head(adjusted_level,text,key=anchor)


  treehandler = """
<SCRIPT type="text/javascript">
<!--
function toggle(button,id)
{
  var n = document.getElementById(id).style;
  if (n.display == "none")
  {
    button.src = "minus.gif";
    button.alt = "-";
    n.display = "block";
  }
  else
  {
    button.src = "plus.gif";
    button.alt = "+";
    n.display = "none";
  }
}

// -->
</SCRIPT>
"""

#line 687 "interscript/src/web_weaver.ipk"
  def mk_contents(self,toc_filename,target):
    if 'weavers' in self.process.trace:
      print('Generating Table of Contents')
    self.toc_sink = named_file_sink(
      self.pass_frame,
      self.basedir+toc_filename,
      self.master.weaver_prefix,
      eol=self.eol)
    if 'weavers' in self.process.trace:
      print('File name',self.toc_sink.name)
    self.mk_head(self.toc_sink)
    w = self.toc_sink.writeline
    w( '<BODY lang="'+self.language+'">')
    if not target:
      nav = '<DIV CLASS="NAVIGATION">'+self.home_anchor+' '+self.frames_anchor+'<BR><HR></DIV>'
      self.toc_sink.writeline(nav)
    w('<H1>'+self.tr_phrase('Table of Contents')+'</H1>')
    w(stacking_weaver.treehandler)
    last_level = -1
    if self.toc:
      i = 0
      level, hnum, href, text = self.toc[i][:4]
      next_level = 0
      if len(self.toc) > i+1: next_level = self.toc[i+1][0]
      self.emit_contents_line(level, hnum, href, text, level<next_level, target)
      divid = 'h'+hnum+'d'
      # note the additional DIV element hack to force a linebreak
      w('<DIV></DIV><DIV ID='+divid+' style="display:none">')
      last_level = level
    for i in range(1, len(self.toc)):
      level, hnum, href, text = self.toc[i][:4]
      next_level = 0
      if len(self.toc) > i+1: next_level = self.toc[i+1][0]
      while level <= last_level:
        self.toc_sink.writeline('</DIV>')
        last_level = last_level - 1
      self.emit_contents_line(level, hnum, href, text, level<next_level, target)
      divid = 'h'+hnum+'d'
      w('<DIV></DIV><DIV ID='+divid+' style="display:none">')
      last_level = level
    while 1 <= last_level:
      w('</DIV>')
      last_level = last_level - 1
    w( '</BODY>')
    w( '</HTML>')
    del self.toc_sink
    return 1

  def emit_contents_line(self, level, hnum, href, text, enabled, target):
    self.toc_sink.writeline('&nbsp;'*(3*level))
    headid ='h'+hnum
    if enabled:
      self.toc_sink.writeline('<IMG SRC="minus.gif" ID='+headid+' ONCLICK="toggle(this,'+"'"+headid+'d'+"'"+')" ALT="-">')
    else:
      self.toc_sink.writeline('<IMG SRC="dot.gif" ID='+headid+' ALT=".">')

    if target:
      self.toc_sink.writeline('<A HREF="'+href+'" TARGET="'+target+'">'+text+'</A>')
    else:
      self.toc_sink.writeline('<A HREF="'+href+'">'+text+'</A>')

#line 794 "interscript/src/web_weaver.ipk"
  def mk_identref(self,filename,target):
    ids = self.master.ids
    if len(ids) == 0:
      ids = self.pass_frame.ids
    if len(ids) == 0: return 0
    sink = named_file_sink(
      self.pass_frame,
      self.basedir+filename,
      self.master.weaver_prefix,
      eol=self.eol)
    self.mk_head(sink)
    w = sink.writeline
    w ('<BODY LANG="'+self.language+'">')

    w('<H1>Index of Identifiers</H1>')
    self.print_table(ids,sink)

    w('</BODY>')
    w('</HTML>')
    return 1

  def mk_sectionref(self,filename,target):
    dict = self.pass_frame.section_index
    if len(dict) == 0: return 0
    sink = named_file_sink(
      self.pass_frame,
      self.basedir+filename,
      self.master.weaver_prefix,
      eol=self.eol)
    self.mk_head(sink)
    w = sink.writeline
    w ('<BODY LANG="'+self.language+'">')

    w('<H1>Index of Sections</H1>')
    keys = list(dict.keys())
    keys.sort()
    w = sink.writeline
    w('<TABLE COLS="1" BORDER="1" CELLPADDING="2">')
    for k in keys:
      w('<TR><TD VALIGN="Top"><CODE> '+k+' </CODE>: ')
      nsections = len(dict[k])
      for i in range(nsections):
        name = k + '['+str(i+1)+']'
        anchor = '<A HREF="'+self.get_anchor(name)+\
          '" TARGET="'+target+'">'+str(i+1)+'</A>'
        w(anchor+' ')
      w('</TD></TR>')
    w('</TABLE>')
    w('</BODY>')
    w('</HTML>')
    return 1

  def mk_classref(self,filename,target):
    ids = self.master.classes
    if len(ids) == 0:
      ids = self.pass_frame.classes
    if len(ids)==0: return 0
    sink = named_file_sink(
      self.pass_frame,
      self.basedir+filename,
      self.master.weaver_prefix,
      eol=self.eol)
    w = sink.writeline
    self.mk_head(sink)
    w ('<BODY LANG="'+self.language+'">')

    w('<H1>Index of Classes</H1>')
    self.print_table(ids,sink)
    w('</BODY>')
    w('</HTML>')
    return 1

  def mk_funcref(self,filename,target):
    ids = self.master.functions
    if len(ids) == 0:
      ids = self.pass_frame.functions
    if len(ids) ==0: return 0
    sink = named_file_sink(
      self.pass_frame,
      self.basedir+filename,
      self.master.weaver_prefix,
      eol=self.eol)
    w = sink.writeline
    self.mk_head(sink)
    w ('<BODY LANG="'+self.language+'">')

    w('<H1>Index of Functions</H1>')
    self.print_table(ids,sink)

    w('</BODY>')
    w('</HTML>')
    return 1

  def mk_sourceref(self,filename,target):
    data = self.master.include_files
    if not data:
      data = self.pass_frame.include_files
    if len(data) == 0: return 0
    sink = named_file_sink(
      self.pass_frame,
      self.basedir+filename,
      self.master.weaver_prefix,
      eol=self.eol)
    w = sink.writeline
    self.mk_head(sink)
    w ('<BODY LANG="'+self.language+'">')

    w('<H1>Source tree</H1>')

    for level, type, name in data:
      w('&nbsp;'*(level*3)+' '+type+': '+name+ '<BR>')

    w('<H1>External Sources</H1>')
    w('<H2>FTP Sources</H2>')
    data = self.master.ftp_list
    if not data:
      data = self.pass_frame.ftp_list

    w('<TABLE COLS="4">')
    w('<TR><TH>host</TH><TH>directory</TH><TH>filename</TH><TH>local</TH></TR>')
    for remote_host,remote_directory,remote_filename, local_filename in data:
      w('<TR><TD>'+remote_host+\
        '</TD><TD>'+remote_directory+\
        '</TD><TD>'+remote_filename+\
        '</TD><TD>'+local_filename+'</TD></TR>')
    w('</TABLE>')
    w('</BODY>')
    w('</HTML>')
    return 1

  def mk_metricsref(self,filename,target):
    data = self.master.iflist
    if not data:
      data = self.pass_frame.iflist
    if len(data) ==0: return 0
    sink = named_file_sink(
      self.pass_frame,
      self.basedir+filename,
      self.master.weaver_prefix,
      eol=self.eol)
    w = sink.writeline
    self.mk_head(sink)
    w ('<BODY LANG="'+self.language+'">')

    w('<H1>Software Metrics</H1>')

    for name, count in data:
      w(name+ ': '+str(count)+' LOC<BR>')

    w('</BODY>')
    w('</HTML>')
    return 1

  def mk_noticesref(self,filename,target):
    data = self.master.noticedict
    if len(data)==0: return 0
    sink = named_file_sink(
      self.pass_frame,
      self.basedir+filename,
      self.master.weaver_prefix,
      eol=self.eol)
    w = sink.writeline
    self.mk_head(sink)
    w ('<BODY LANG="'+self.language+'">')

    w('<H1>Notices</H1>')
    keys = list(data.keys())
    keys.sort()
    for key in keys:
      value = data[key]
      w('<A HREF="'+key+'.html" TARGET="notices">'+key+'</A>')
      sink2 = named_file_sink(
        self.pass_frame,
        self.basedir+key+'.html',
        self.master.weaver_prefix,
        eol=self.eol)
      w2 = sink2.writeline
      self.mk_head(sink)
      w2( '<BODY>')
      w2('<PRE>')
      sink2.write(cvt_text(value))
      w2('</PRE>')
      w2('</BODY>')
      w2('</HTML>')
      del w2
      del sink2
    w('</BODY>')
    w('</HTML>')
    return 1

  def mk_testref(self,filename,target):
    ids = self.master.tests
    if len(ids) == 0:
      ids = self.pass_frame.tests
    if len(ids)==0: return 0
    sink = named_file_sink(
      self.pass_frame,
      self.basedir+filename,
      self.master.weaver_prefix,
      eol=self.eol)
    w = sink.writeline
    self.mk_head(sink)
    w ('<BODY LANG="'+self.language+'">')

    w('<H1>Index of Tests</H1>')
    w('<TABLE CLASS="TEST_SUMMARY_TABLE" COLS="4" BORDER="1">')
    w('<TR><TH>No</TH><TH>Description</TH><TH>Kind</TH><TH>Result</TH><TR>')
    keys = list(ids.keys())
    keys.sort()
    for key in keys:
      descr, label, kind, result = ids[key]
      href = self.get_anchor(label)
      w('<TR><TD>'+str(key)+'</TD><TD><A TARGET="'+target+'" HREF="'+href+'">'+descr+'</A></TD><TD>'+kind+'</TD><TD>'+result+'</TD></TR>')
    w('</TABLE>')
    w('</BODY>')
    w('</HTML>')
    return 1

  def mk_filestatus(self,filename,target):
    if 'weavers' in self.process.trace:
      print('Creating file status file:',filename)
    filestatus_output = simple_named_file_sink(
      self.pass_frame,self.basedir+filename, self.master.weaver_prefix,eol='\r\n')
    filestatus_weaver = html_weaver(
      self.pass_frame,
      filestatus_output,title='File Status', language=self.language)
    filestatus_weaver.print_file_status(hlevel=1)
    return 1

  def set_fc_anchor(self,file,count):
    filename = self.base[0].sink.basename
    self.anchor_file[(file,count)]=filename
    for weaver in self.base:
      weaver.set_fc_anchor(file,count)

  def heading_reference(self, *args, **kwds): pass # always generated
  def identifier_reference(self, *args, **kwds): pass # always generated
  def class_reference(self, *args, **kwds): pass # always generated
  def function_reference(self, *args, **kwds): pass # always generated
  def test_reference(self, *args, **kwds): pass # always generated


