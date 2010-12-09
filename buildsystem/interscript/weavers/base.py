#line 8 "interscript/src/weavers.ipk"
import time

class weaver_base:
  __class_protocols = ['weaver']
  def __init__(self, pass_frame,language=''):
    self.enabled = 1
    self.translating = 1
    self.tags = []
    if language: self.tags = [language]
    self.pass_frame = pass_frame
    self.master = pass_frame.master
    self.process = self.master.process
    if 'weavers' in self.process.trace:
      self.process.acquire_object(self,'WEAVER')
    self.language = language
    self.sequence = self.pass_frame.get_new_sequence_number()
    self.persistent_frame = self.master.get_persistent_frame(self.sequence)

  def __del__(self):
    self.persistent_frame['last write time']=time.time()
    if 'weavers' in self.process.trace:
      self.process.release_object(self)

  def enable(self): self.enabled = 1
  def disable(self): self.enabled = 0
  def translate(self): self.translating = 1
  def raw(self): self.translating = 0
  def add_tag(self,tag): self.tags.append(tag)
  def enable_if(self,tag):
    if 'weavers' in self.process.trace:
      print('Checking for tag',tag,'in',self.tags)
    if tag in self.tags: self.enable()
    else: self.disable()
  def raw_if(self,tag):
    if tag in self.tags: self.raw()
    else: self.disable()

  def writeline(self,line=''):
    self.write(line + '\n')

  def write(self,line):
    self.sink.write(line)

