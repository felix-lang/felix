#line 216 "interscript/src/weavers.ipk"
from interscript.weavers.multiplexor import multiplexor
from interscript.drivers.sinks.bufdisk import named_file_sink
import traceback

def auto_weaver(pass_frame,basename,autoweave,title=None):
  if not autoweave:
    if 'weavers' in pass_frame.master.process.trace:
      print('No weavers specified')
    return multiplexor(pass_frame, [])

  prefix = pass_frame.master.weaver_prefix
  eol = pass_frame.master.html_eol
  directory = pass_frame.master.weaver_directory
  languages = pass_frame.master.languages
  if not title:
    title = pass_frame.master.get_title()
  if not title:
    title = basename
  weavers = []
  if not languages: language = [pass_frame.master.get_native_language()]
  print('Weaving for languages',languages)
  for language in languages:
    for w in autoweave:
      try:
        modname = 'interscript.weavers.'+w
        ctorname = modname + '.mk_' +w
        try: exec('import '+modname)
        except ImportError:
          print('Cannot import module '+modname)
          raise
        try:
          mk = eval(ctorname)
        except AttributeError:
          print('Cannot find weaver constructor '+ctorname)
          raise
        try:
          w =  mk(pass_frame, basename, directory, prefix, eol, title, language)
        except:
          print('Cannot invoke weaver constructor '+ctorname)
          traceback.print_exc()
          raise
        weavers.append(w)
      except KeyboardInterrupt: raise
      except:
        print('Cannot create weaver for',w)
  return multiplexor(pass_frame, weavers)

