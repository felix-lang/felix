#line 89 "interscript/src/tanglers.ipk"
#---------------------------------------------------------
# tangler base
#---------------------------------------------------------
class tangler_base:
  __class_protocols__ = ['tangler']
  def __init__(self,sink,weaver,nosref=0):
    self.nosref=nosref
    self.sink = sink
    self.weaver = weaver
    self.inhibit_sref = 0
    if weaver:
      self.pass_frame = weaver.pass_frame
      self.master = self.pass_frame.master
      self.process = self.master.process
      if 'tanglers' in self.process.trace:
        self.process.acquire_object(self, 'TANGLER')
    else:
      self.pass_frame = None
      self.master = None
      self.process = None
    self.language = 'data'

  def get_language(self): return self.language

  def write_comment(self,line):
    pass

  def _writeline(self,data):
    self.sink.writeline(data)

  def _write_and_echo(self,data):
    self._writeline(data)
    self.weaver.echotangle(self.sink.lines_written,data)

  def _handle_sref(self, file, count, inhibit_sref):
    if not self.nosref and not inhibit_sref and not self.inhibit_sref:
      if (file != self.sink.last_source_file or
        count != self.sink.last_source_count+1 or
        self.sink.last_inhibit_sref):
        self.start_section(file,count)
      self.sink.last_inhibit_sref = 0
    else:
      self.sink.last_inhibit_sref = 1
    self.sink.last_source_file = file
    self.sink.last_source_count = count

  def writeline(self,data,file,count, inhibit_sref=0):
    self._handle_sref(file,count,inhibit_sref)
    self._write_and_echo(data)

  def start_section(self,file,count): pass

  def __del__(self):
    if self.process and 'tanglers' in self.process.trace:
      self.process.release_object(self)

#---------------------------------------------------------
# builtin tanglers: null, data, c, script
#---------------------------------------------------------

