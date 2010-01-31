#line 17 "interscript/src/weaver_filters.ipk"
from interscript.weavers.multiplexor import multiplexor
import re
class markup_filter(multiplexor):
  def __init__(self,pass_frame,regexp,startmethod,endmethod,base=[]):
    multiplexor.__init__(self,pass_frame,base)
    self.regexp = re.compile(regexp)
    self.startmethod = startmethod
    self.endmethod = endmethod
    self.protocol = ('regexp filter',regexp)
    self.name = 'markup filter v1'
    self.translating = 1

  def translate(self):
    self.translating = 1
    for weaver in self.base: weaver.translate()

  def raw(self):
    self.translating = 0
    for weaver in self.base: weaver.raw()

  def raw_if(self, tag):
    self.translating = 0
    for weaver in base: weaver.raw_if(tag)

  def write(self,data):
     if not self.enabled: return
     if not self.translating:
       for weaver in self.base:
         weaver.write(data)
     else:
       match = self.regexp.search(data)
       startpos = 0
       while match:
         midpos = match.start(0)
         endpos = match.end(0)
         if midpos == endpos:
           raise 'nullable regexp '+self.regexp.pattern
         if startpos != midpos:
           for weaver in self.base:
             weaver.write(data[startpos:midpos])
         for weaver in self.base:
           exec('weaver.'+self.startmethod+'()')
           weaver.write(match.group(1))
           exec('weaver.'+self.endmethod+'()')
         startpos = endpos
         match = self.regexp.search(data,startpos)
       if startpos != len(data):
         for weaver in self.base:
           weaver.write(data[startpos:])

  def writeline(self,data=''):
    if not self.enabled: return
    if data != '': self.write(data)
    for weaver in self.base:
      weaver.writeline()

  def begin_displayed_code(self):
    self.translating = 0
    for weaver in self.base:
      try: weaver.begin_displayed_code()
      except KeyboardInterrupt: raise
      except: pass

  def end_displayed_code(self):
    self.translating = 0
    for weaver in self.base: weaver.end_displayed_code()

#line 99 "interscript/src/weaver_filters.ipk"
def chain_filters(filters):
  for i in range(0,len(filters)-1):
    filters[i].base = [filters[i+1]]

def create_filters(triples):
  filters = []
  for regexp,startmethod,endmethod in triples:
    filters.append(markup_filter(regexp,startmethod,endmethod))
  return filters

def create_filter_chain(triples):
  return chain_filters(create_filters(triples))

