#line 65 "interscript/src/weavers.ipk"
from interscript.weavers.base import weaver_base
class raw_weaver(weaver_base):
  def __init__(self, pass_frame, writer ,**kwds):
    weaver_base.__init__(self, pass_frame)
    if 'weavers' in self.process.trace:
      print 'initialising raw weaver, writer',writer.get_sink_name()
    self.protocol = 'raw'
    self.persistent_frame['protocol']=self.raw
    self.tags = ['raw']
    self.sink = writer
    self.name = 'raw weaver v1 for '+self.sink.name
    self.persistent_frame['name']=self.name

