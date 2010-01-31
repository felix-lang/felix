#line 175 "interscript/src/sink_drivers.ipk"
import string
from interscript.drivers.sinks.base import sink_base
from interscript.drivers.sinks import sink_open_error

class simple_named_file_sink(sink_base):
  def __init__(self,pass_frame,input_filename, prefix='', eol='\n'):
    self.eol = eol

    # compute absolute pathname, and create directories if necessary
    # we don't use posixpath because we're enforcing an _interscript_
    # pathname convention here
    pathlist = input_filename.split('/')
    self.basename = pathlist[-1]
    platform = pass_frame.process.site.platform
    pathname = platform.mk_dir(prefix, pathlist)
    try:
      file = platform.open(pathname,'w')
    except:
      raise sink_open_error(pathname)
    sink_base.__init__(self, name = input_filename, file = file)
    if pass_frame: pass_frame.flist.append(pathname)

  def __del__(self):
    self.file.close()

  def raw_write(self,line): self.file.write(line)
  def raw_eol(self): self.raw_write(self.eol)

