#line 39 "interscript/src/html_parser.ipk"
from interscript.drivers.sources.base import eoi
import string
import traceback
class sgml_wrapper:
  def __init__(self, sgml):
    self.sgml = sgml

  def writeline(self,data,file,count):
    self.sgml.feed(data)

  def close():
    self.sgml.close(self)

  def reset(self):
    self.sgml.reset()

# this is a hack: sgmllib needs to be imported here
# so the class SGMLParser defined in it can be used as a base
import sgmllib

class html_filter(sgmllib.SGMLParser):
  def __init__(self, input_frame):
    sgmllib.SGMLParser.__init__(self)
    self.save_data = 0
    self.script_language = ''
    self.input_frame = input_frame
    self.weaver = input_frame.get_weaver()
    self.process = input_frame.process
    # feeding <HTML> in here is a hack to get around a bug in sgmllib,
    # which fails to process unbalanced end tags correctly
    self.feed('<HTML>')

  def _save(self):
    self.save_data = 1
    self.saved_data = ''
  def _saved(self):
    self.save_data = 0
    return self.saved_data

  def handle_data(self,data):
    new_data = ''
    for ch in data:
      if ch == '\n': ch = ' \n'
      new_data = new_data + ch
    if self.save_data:
      self.saved_data = self.saved_data + new_data
    else:
      self.weaver.write(new_data)

  def handle_comment(self,data):
    if 'parsers' in self.process.trace:
      print 'SGML comment',data
    if self.script_language != '':
      self.saved_comments = self.saved_comments + data

  def start_html(self, attributes): pass
  def start_head(self, attributes): pass
  def end_head(self): pass
  def start_body(self, attributes): pass
  def end_body(self): pass
  def end_html(self):
    del self.input_frame
    del self.weaver
    raise eoi

# fonts
  def start_b(self,attributes): self.weaver.begin_bold()
  def end_b(self): self.weaver.end_bold()

  def start_i(self,attributes): self.weaver.begin_italic()
  def end_i(self): self.weaver.end_italic()

  def start_em(self,attributes): self.weaver.begin_emphasize()
  def end_em(self): self.weaver.end_emphasize()

  def start_strong(self,attributes): self.weaver.begin_strong()
  def end_strong(self): self.weaver.end_strong()

  def start_small(self,attributes): self.weaver.begin_small()
  def end_small(self): self.weaver.end_small()

  def start_big(self,attributes): self.weaver.begin_big()
  def end_big(self): self.weaver.end_big()

  def start_code(self,attributes): self.weaver.begin_code()
  def end_code(self): self.weaver.end_code()

# paragraphs
  def start_p(self,attributes): self.weaver.prose()
  def end_p(self): self.weaver.eop()

# displays
  def start_pre(self,attributes): self.weaver.begin_displayed_code()
  def end_pre(self): self.weaver.end_displayed_code()

#lists
  def start_ol(self,attributes):
    self.weaver.begin_numbered_list()
    self.list_kind = 'ol'
  def end_ol(self):
    self.weaver.end_numbered_list()

  def start_dl(self,attributes):
    self.weaver.begin_keyed_list()
    self.list_kind = 'dl'
  def end_dl(self):
    self.weaver.end_keyed_list()

  def start_ul(self,attributes):
    self.weaver.begin_bullet_list()
    self.list_kind = 'ul'
  def end_ul(self):
    self.weaver.end_bullet_list()

#list items
  def start_li(self,attributes):
    if self.list_kind == 'ol':
      self.weaver.begin_numbered_list_item()
    else:
      self.weaver.begin_bullet_list_item()

  def end_li(self):
    if self.list_kind == 'ol':
      self.weaver.end_numbered_list_item()
    else:
      self.weaver.end_bullet_list_item()

  def start_dt(self,attributes): self._save()
  def end_dt(self):
    self.weaver.begin_keyed_list_item(self._saved())

  def start_dd(self,attributes): pass
  def end_dd(self): self.weaver.end_keyed_list_item()

#headings
  def start_h1(self,attributes): self._save()
  def end_h1(self): self.weaver.head(1,self._saved())

  def start_h2(self,attributes): self._save()
  def end_h2(self): self.weaver.head(2,self._saved())

  def start_h3(self,attributes): self._save()
  def end_h3(self): self.weaver.head(3,self._saved())

  def start_h4(self,attributes): self._save()
  def end_h4(self): self.weaver.head(4,self._saved())

  def start_h5(self,attributes): self._save()
  def end_h5(self): self.weaver.head(5,self._saved())

  def start_h6(self,attributes): self._save()
  def end_h6(self): self.weaver.head(6,self._saved())

  def unknown_starttag(self,tag,attributes):
    print 'UNKNOWN START TAG',tag,attributes

  def unknown_endtag(self,tag):
    print 'UNKNOWN END TAG',tag

  def unknown_charref(self,ref):
    print 'BAD CHAR REF',ref

  def unknown_entityref(self,ref):
    print 'UNKNOWN ENTITY REF',ref

  # due to a bug in sgmllib, this routine will
  # never be called
  def report_unbalanced(self,tag):
    print 'LONELY ENDTAG',tag

  def start_script(self,attributes):
    if 'parsers' in self.process.trace:
      print 'start of script'
    for param, value in attributes:
      if string.lower(param) == 'language':
        self.script_language = string.lower(value)
        self.saved_comments = ''

  def end_script(self):
    if 'parsers' in self.process.trace:
      print 'end of script'
    if self.script_language == 'python':
      try:
        exec self.saved_comments in globals(),self.input_frame.userdict
      except:
        print "Error executing python <SCRIPT>"
        traceback.print_exc()
    else:
      print 'Sorry',self.script_language,'not available'
