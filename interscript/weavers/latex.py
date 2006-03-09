#line 13 "interscript/src/latex_weaver.ipk"
from interscript.weavers.base import weaver_base
import string

from interscript.drivers.sinks.bufdisk import named_file_sink
def mk_latex(pass_frame,basename, directory, prefix, eol, title, language):
  if language: filename = directory + language + '_' + basename + '.tex'
  else: filename = directory + basename + '.tex'
  w = named_file_sink(pass_frame,filename, prefix)
  if not title: title = basename
  return latex_weaver(pass_frame,w, title=title,language=language)

def cvt_code(line):
  v = '\\verb+'
  for ch in line:
    if ch != '+': v = v + ch
    else: v = v + '+\\verb-+-\\verb+'
  v = v + '+'
  return v

def cvt_text(line):
  l = ''
  for ch in line:
    if ch in '$&%#_^{}\\': l = l + '\\'
    l = l + ch
  return l

class latex_weaver(weaver_base):
#line 42 "interscript/src/latex_weaver.ipk"
  def __init__(self, pass_frame, writer, language='', **kwds):
    weaver_base.__init__(self, pass_frame, language)
    self.sink = writer
    if 'weavers' in self.process.trace:
      print 'initialising latex weaver, writer',writer.get_sink_name()
    self.protocol = 'LaTeX2e'
    self.persistent_frame['protocol']=self.protocol
    self.acount = 1
    self.tag_stack = []
    self.comments = 0
    self.master = pass_frame.master
    self.list = []
    self.heading_level_offset = 0
    self.name = 'latex2e weaver v1 for '+self.sink.name
    self.persistent_frame['name']=self.name
    self.verbatim = 0
    self.start_of_line = 1
    self.prolog(kwds)

#line 63 "interscript/src/latex_weaver.ipk"
  def __del__(self):
    self.epilog()

#line 68 "interscript/src/latex_weaver.ipk"
  def identifier_reference(self, hlevel=2, *args, **kwds):
    ids = self.master.ids
    keys = ids.keys()
    keys.sort()
    if hlevel >0:
      self.head(hlevel,'Index of Identifiers')
    self._writeline('\\begin{tabular}{ll}')
    for k in keys:
      refs = ids[k]
      self._write(cvt_code(k)+'&')
      old_df = ''
      for sf,sc,df,dc in refs:
        if old_df != '': self._write(', ')
        if old_df != df:
          self._writeline(cvt_code(df)+': '+str(dc)+'\\ref{'+sf+':'+str(sc)+'}')
          old_df = df
        else:
          self._write(': '+str(dc)+'\\ref{'+sf+':'+str(sc)+'}')
      self._write('\\\\\n')
    self._writeline('\\end{tabular}\n')

#line 91 "interscript/src/latex_weaver.ipk"
  def class_reference(self, hlevel=2, *args, **kwds):
    ids = self.master.classes
    keys = ids.keys()
    keys.sort()
    if hlevel >0:
      self.head(hlevel,'Index of Classes')
    self._writeline('\\begin{tabular}{ll}')
    for k in keys:
      refs = ids[k]
      self._write('\\verb+'+k+'+&')
      old_df = ''
      for sf,sc,df,dc in refs:
        if old_df != '': self._write(', ')
        if old_df != df:
          self._writeline(cvt_code(df)+': '+str(dc)+'\\ref{'+sf+':'+str(sc)+'}')
          old_df = df
        else:
          self._writeline(': '+str(dc)+'\\ref{'+sf+':'+str(sc)+'}')
      self._write('\\\\\n')
    self._writeline('\\end{tabular}\n')

  # create a Latex anchor
  def set_fc_anchor(self,file,count):
    self._write('\\label{'+file+':'+str(count)+'}')

#line 118 "interscript/src/latex_weaver.ipk"
  def print_contents(self,*args,**kwds):
    self._writeline('\\tableofcontents')

#line 123 "interscript/src/latex_weaver.ipk"
  def print_file_list(self, hlevel=2, *args, **kwds):
    self.head(hlevel,'File List')
    for line in self.master.flist:
      self._writeline(cvt_code(line))

#line 130 "interscript/src/latex_weaver.ipk"
  def print_source_list(self, hlevel=2, *args, **kwds):
    self.head(hlevel,'Source List')
    for line,count in self.master.iflist:
      self._writeline(cvt_code(line))

#line 145 "interscript/src/latex_weaver.ipk"
  def prolog(self,kwds):
    # this bit handles lambda translation process
    # to convert interscript generated UTF-8 to unicode
    if kwds.has_key('llambda'):
      self._writeline('\\ocp\\inutf=inutf8')
      self._writeline('\\inputTranslation\\inutf8')
      self.omega=1
    else:
      self.omega=0

    # see Kopka pp25-27
    # the default document class is for a book
    # other standard classes include:
    #   article report letter

    documentclass = 'book'
    if kwds.has_key('documentclass'):
      documentclass=kwds['documentclass']

    # the options are a python list of words
    # for the standard book class they're from the set:
    #   10pt 11pt 12pt
    #   letterpaper legalpaper executivepaper
    #   a4paper a5paper b5paper
    #   landscape
    #   onecolumn twocolumn
    #   oneside twoside
    #   openright openany
    #   notitlepage titlepage

    # note: the default paper size Latex uses is
    # american letterpaper. Don't count on this,
    # I intend to make the ISO Standard A4 that everyone
    # else uses the default!

    docopts = []
    if kwds.has_key('documentclass_options'):
      docopts =kwds['documentclass']
    docoptstr=''
    if docopts: docoptstr = docopts[0]
    for opt in range(1,len(docopts)):
     docoptstr = dosoptstr + ', ' + opt
    self._writeline('\\documentclass['+docoptstr+']{'+documentclass+'}')

    if kwds.has_key('heading_level_offset'):
      self.heading_level_offset = kwds['heading_level_offset']

    # page heading control
    pagestyle = 'headings'
    if kwds.has_key('pagestyle'):
      pagestyle=kwds['pagestyle']
    self._writeline('\\pagestyle{'+pagestyle+'}')

    pagenumbering= 'arabic'
    if kwds.has_key('pagenumbering'):
      pagenumbering=kwds['pagenumbering']
    self._writeline('\\pagenumbering{'+pagenumbering+'}')

    # page layout
    page_format_params = [
      'topmargin','headheight','headsep','topskip','textheight','footskip',
      'oddsidemargin','evensidemargin',
      'textwidth']
    for p in page_format_params:
      if kwds.has_key(p):
        param=kwds[p]
        self._writeline('\\setlength{\\'+p+'}{'+param+'}')

    # lines and paragraphs

    # Note: we do _not_ permit indented paragraphs AT ALL.
    # Don't even try it. FAR FAR too many things are broken
    # by indentation.

    baselinestretch= 1
    if kwds.has_key('baselinestretch'):
      baselinestretch=kwds['baselinestretch']
    self._writeline('\\renewcommand{\\baselinestretch}{'+str(baselinestretch)+'}')

    self._writeline('\\setlength{\\parskip 2mm plus 0.5mm minus 1mm}')
    self._writeline('\\setlength{\\parindent 0mm}')

    self._writeline( '\\begin{document}')
    if kwds.has_key('title'):
      title=kwds['title']
    else:
      title = self.sink.pass_frame.master.filename
    self._writeline('\\title{'+cvt_text(title)+'}')
    if kwds.has_key('author'):
      author =kwds['author']
      self._writeline('\\author{'+cvt_text(author)+'}')

    self._writeline( '\\maketitle')

  def epilog(self):
    self._writeline('\\end{document}')

#line 244 "interscript/src/latex_weaver.ipk"
  def _writeline(self,line=''):
    if self.enabled:
      self.sink.writeline(line)
      self.start_of_line = 1

  def _write(self,line):
    if self.enabled:
      self.sink.write(line)
      self.start_of_line = 0

  def write(self,line):
    if self.translating and not self.verbatim:
      self._write(cvt_text(line))
    else:
      self._write(line)

  def writeline(self,line=''):
    line = string.rstrip(line)
    self.write(line);
    if not self.start_of_line: self._writeline()

  def writecode(self,line):
    self._writeline('\\hbox to 0pt{'+cvt_code(line)+'\\hss}\\\\')

  def begin_displayed_text(self):
    self._write('\\begin{quote}\n')

  def end_displayed_text(self):
    self._write('\\end{quote}\n')


  def begin_displayed_code(self):
    self._writeline('\\begin{verbatim}')
    self.verbatim = 1

  def end_displayed_code(self):
    self._writeline('\\end{verbatim}')
    self.verbatim = 0

  def line_break(self):
    self._writeline('\\newline')

  def page_break(self):
    self._writeline('\\newpage')

  def write_tagged(self,tag, data):
    self._write('{\\'+tag)
    self._write(data)
    self._write('}')


  def code_head(self,tangler, secno):
    if tangler:
      self._write( '{\\par\\noindent\\small Start section to '+\
        cvt_code(tangler.sink.get_sink_name())+\
        '['+str(secno)+']}\\\\')

  def code_foot(self,tangler, secno):
    if tangler:
      self._write( '{\\small End section to '+\
        cvt_code(tangler.sink.get_sink_name())+\
        '['+str(secno)+']}')

#line 309 "interscript/src/latex_weaver.ipk"
  def head(self,level, text, **kwds):
    atext=kwds.get('short_text','')
    anchor=kwds.get('key','')
    cmds = {
      1:'\\part',
      2:'\\chapter',
      3:'\\section',
      4:'\\subsection',
      5:'\\subsubsection',
      6:'\\paragraph',
      7:'\\subparagraph'}
    lev = level+self.heading_level_offset
    if lev>7:lev=7
    cmd = cmds[lev]
    if anchor == '': anchor = atext
    if anchor == '':
      anchor = 'h'+str(self.acount)
      self.acount = self.acount + 1

    if atext:
      self._writeline(cmd+'['+atext+']{'+text+'}\\label{'+anchor+'}')
    else:
      self._writeline(cmd+'{'+text+'}\\label{'+anchor+'}')

#line 335 "interscript/src/latex_weaver.ipk"
  def begin_table(self, *headings, **kwds):
    self._writeline('\\begin{table}[h]\\begin{tabular}{|'+'l|'*len(headings)+'}\hline')
    self.write(headings[0])
    for h in headings[1:]:
      self._write('&')
      self.write(h)
    self._writeline(r'\\\hline')

  def table_row(self,data):
    self.write(data[0])
    for d in data[1:]:
      self._write('&')
      self.write(d)
    self._writeline(r'\\')

  def table_rule(self):
    self._writeline(r'\hline')

  def end_table(self):
    self._writeline('\\hline\\end{tabular}\\end{table}')

#line 359 "interscript/src/latex_weaver.ipk"
  def begin_numbered_list(self,start=1, type='1'):
    self._writeline('\\begin{enumerate}')

  def end_numbered_list(self):
    self._writeline('\\end{enumerate}')

  def begin_numbered_list_item(self):
    self._writeline('\\item ')

  def end_numbered_list_item(self):
    pass

#line 373 "interscript/src/latex_weaver.ipk"
  def begin_bullet_list(self):
    self._writeline('\\begin{itemize}')

  def end_bullet_list(self):
    self._writeline('\\end{itemize}')

  def begin_bullet_list_item(self):
    self._write('\\item ')

  def end_bullet_list_item(self):
    pass

#line 387 "interscript/src/latex_weaver.ipk"
  def begin_keyed_list(self):
    self._writeline('\\begin{description}')

  def end_keyed_list(self):
    self._writeline('\\end{description}')

  def begin_keyed_list_item(self,key):
    self._write('\\item[')
    self.write(key)
    self._write(']')

  def end_keyed_list_item(self):
    pass

#line 403 "interscript/src/latex_weaver.ipk"
  # default code line formatting
  def echotangle(self,count,data):
    if self.comments:
      self._writeline(data)
    else:
      self.writecode("%6d: %s" % (count,data))

#line 412 "interscript/src/latex_weaver.ipk"
  def prose(self): # start of paragraph
    self._write('\\noindent ')

  def par(self): # paragraph separator
    self._write('\\par\n\\noindent ')

  def eop(self): # end of paragraph
    self._write('\\par\n')

  def write_comment(self,v):
    self.write_tagged('small',v)

#line 426 "interscript/src/latex_weaver.ipk"
  def begin_code(self):
    self._write('{\\tt ')

  def end_code(self):
    self._write('}')

  def begin_emphasize(self):
    self._write('{\\em ')

  def end_emphasize(self):
    self._write('}')

  def begin_strong(self):
    self._write('{\\bfseries ')

  def end_strong(self):
    self._write('}')

  def begin_italic(self):
    self._write('{\\itshape ')

  def end_italic(self):
    self._write('}')

  def begin_bold(self):
    self._write('{\\bfseries ')

  def end_bold(self):
    self._write('}')

  def begin_big(self):
    self._write('{\\large ')

  def end_big(self):
    self._write('}')

  def begin_small(self):
    self._write('{\\small ')

  def end_small(self):
    self._write('}')

#line 470 "interscript/src/latex_weaver.ipk"
  def cite_url(self,url):
    self._write('{\\bfseries ')
    self.write(url)
    self._write('}')

