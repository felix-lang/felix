#line 153 "interscript/src/sink_drivers.ipk"
from interscript.drivers.sinks.base import sink_base
class null_sink(sink_base):
  def __init__(self):
    sink_base.__init__(self,name='null sink')

  def raw_write(self,line):
    pass

