#line 319 "interscript/src/sink_drivers.ipk"
import sys
from interscript.drivers.sinks.base import sink_base
class stdout_sink(sink_base):
  def __init__(self):
    sink_base.__init__(self,name='standard output')

  def raw_write(self,line):
    sys.stdout.write(line)

