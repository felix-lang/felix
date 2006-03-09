#line 103 "interscript/src/sink_drivers.ipk"
import string
class sink_base:
  __class_protocols__ = ['sink','file']
  def __init__(self, **kwds):
    self.lines_written = 0
    self.last_source_file = ''
    self.last_source_line = -1
    self.last_inhibit_sref = 1
    self.closed = 0
    for k in kwds.keys():
      self.__dict__[k]=kwds[k]

  def raw_close(self): pass
  def raw_flush(self): pass
  def raw_eol(self): self.raw_write('\n')
  def isatty(self): return 0

  def raw_writeline(self,line):
    self.raw_write(line)
    self.raw_eol()
    self.lines_written = self.lines_written  + 1

  def writeline(self,line):
    self.write(line + '\n')

  def raw_writelines(self,lines):
    for line in lines: self.raw_writeline(line)

  def writelines(self,lines):
    self.write(string.join(lines,''))

  def get_sink_name(self):
    return self.name

  def write(self,text):
    lines = string.split(text,'\n')
    for line in lines[:-1]:
      self.raw_writeline(line)
    self.raw_write(lines[-1])

  def close(self):
    self.closed = 1
    self.raw_close()

  def flush(self):
    self.raw_flush

