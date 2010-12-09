#line 487 "interscript/src/latex_weaver.ipk"
from interscript.weavers.latex import latex_weaver
from interscript.drivers.sinks.bufdisk import named_file_sink

def mk_llambda(pass_frame,basename, directory, prefix, eol, title, language):
  if language: filename = directory + language + '_' + basename + '.tex'
  else: filename = directory + basename + '.tex'
  w = named_file_sink(pass_frame,filename, prefix)
  if not title: title = basename
  return llambda_weaver(pass_frame,w, title=title,language=language)

class llambda_weaver(latex_weaver):
  def __init__(self, pass_frame, writer, language='', **kwds):
    kwds2=kwds.copy()
    kwds2['llambda']=None
    latex_weaver.__init__(*(pass_frame, writer, language), **kwds2)

