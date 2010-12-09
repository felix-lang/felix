#line 164 "interscript/src/sink_drivers.ipk"
from interscript.drivers.sinks.base import sink_base
class tee_sink(sink_base):
  def __init__(self, *args, **kwds):
    sink_base.__init__(self)

  def raw_write(self,line):
    pass

