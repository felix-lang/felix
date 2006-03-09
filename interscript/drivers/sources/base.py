#line 80 "interscript/src/source_drivers.ipk"
class eof(Exception): pass
class eoi(Exception): pass

#line 88 "interscript/src/source_drivers.ipk"
#---------------------------------------------------------
# source base
import string
class source:
  __class_protocols__ = ['file','source']
  def __init__(self, encoding, **kwds):
    self.lines_read = 0
    self.mode = 'r'
    for k in kwds.keys():
      self.__dict__[k]=kwds[k]
    self.closed = 1
    self.set_encoding(encoding)

  def set_encoding(self, encoding):
    self.encoding_name = encoding
    self.encoding_routine = None
    if encoding != 'utf8':
      encoding_module = 'interscript.encoding.'+encoding
      conversion_routine = encoding + '_to_utf8'
      try:
        exec 'import '+encoding_module
        exec 'self.encoding_routine = '+encoding_module+'.'+conversion_routine
      except:
        print 'UNABLE TO LOAD ENCODING "'+encoding+'", reading as UTF8'

  def get_encoding(self): return self.encoding_name

  def get_source_name(self):
    return self.name

  def get_lines_read(self):
    return self.lines_read

  def raw_readlines(self):
    if self.closed: raise eof
    lines = []
    try:
      while 1: lines.append(self.raw_readline())
    except eof:
      pass
    return lines

  def readline(self, sizehint=None):
    return self.raw_readline()+'\n'

  def read(self, sizehint=None):
    return string.join(self.raw_readlines())+'\n'

  def readlines(self, sizehint=None):
    x = []
    for line in self.raw_readlines():
      x.append(line+'\n')
    return x

  def isatty(self):
    return 0

  def close(self):
    if not self.closed:
      self._close()
      self.closed = 1

  def _close(self): pass
  def flush(self): pass
  def __del__(self):
    self.close()

  def raw_readline(self):
    data =  self._raw_readline()
    if self.encoding_routine:
      data = self.encoding_routine(data)
    return data

class file_source(source):
  def __init__(self, encoding, *args, **kwds):
    apply(source.__init__, (self,encoding)+args, kwds)

  def _close(self):
    self.file.close()

  def _raw_readline(self):
    if self.closed: raise eof
    line = self.file.readline()
    if len(line)==0: raise eof
    self.lines_read = self.lines_read + 1
    return string.rstrip(line)

  def get_filename(self):
    return self.name

