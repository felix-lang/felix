#line 333 "interscript/src/sink_drivers.ipk"
import sys
from interscript.drivers.sinks.base import sink_base
class cache_sink(sink_base):
  def __init__(self, name, master_frame):
    sink_base.__init__(self,name=name)
    self.saved = ''
    self.list = []
    self.closed = 0
    self.master_frame = master_frame

  def raw_eol(self):
    self.list.append(self.saved)
    self.saved = ''

  def raw_write(self,data):
    self.saved = self.saved + data

  def __del__(self):
    macros = self.master_frame.get_persistent_frame('macros')
    macros[self.name]=self.list
