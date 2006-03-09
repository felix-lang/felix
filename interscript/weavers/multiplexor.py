#line 95 "interscript/src/weavers.ipk"
import traceback
class multiplexor:
  __class_protocols__ = ['weaver']
  def __init__(self,pass_frame,base):
    self.pass_frame = pass_frame
    self.master = pass_frame.master
    self.process= self.master.process
    if 'weavers' in self.master.process.trace:
      self.process.acquire_object(self,'MULTIPLEX WEAVER')
    self.base=base
    self.name = 'multiplexor v1'
    # self.persistent_frame['name']=self.name
    self.debug_missing_methods = 0
    self.list_stack = []

  def __del__(self):
    if 'weavers' in self.process.trace:
      self.process.release_object(self)
  def __nonzero__(self):
    return 1

  def callit(self,*args, **kwds):
    self._callit(self.temp,args,kwds)

  def _callit(self,at,args, kwds):
    for b in self.base:
      if hasattr(b,at):
        try:
          apply(getattr(b,at),args,kwds)
        except KeyboardInterrupt:
          raise KeyboardInterrupt
        except:
          protocol = 'No protocol attribute'
          name = 'No name attribute'
          if hasattr(b,'protocol'): protocol = b.protocol
          if hasattr(b,'name'): name = b.name
          print 'Error in call!'
          print '  Method',at
          print '  Args  ',args
          print '  Kwds  ',kwds
          print '  of    ',b.protocol,'weaver',b.name,'ignored'
          traceback.print_exc()
      elif self.debug_missing_methods:
        protocol = 'No protocol attribute'
        sinkname = 'No sink attribute'
        if hasattr(b,'protocol'): protocol = b.protocol
        if hasattr(b,'sink'):
          sinkname = 'no sink name'
          sink = b.sink
          if hasattr(sink,'name'):
            sinkname = sink.name
        print 'Warning: missing method',at,'of weaver type',protocol,'for',sinkname,'ignored'

  def __getattr__(self,x):
    self.temp = x
    return self.callit

  def begin_list(self,style='bullet'):
     if not style in ('bullet','numbered','keyed'):
       style = 'bullet'
     self.list_stack.append([style,0])
     self._callit('begin_'+style+'_list',(),{})

  def end_list(self):
    style, open_item = self.list_stack[-1]
    del self.list_stack[-1]
    if open_item:
      self._callit('end_'+style+'_list_item',(),{})
    self._callit('end_'+style+'_list',(),{})

  def item(self, *args, **kwds):
    style, open_item = self.list_stack[-1]
    if open_item:
      self._callit('end_'+style+'_list_item',(),{})
    else:
      self.list_stack[-1][1]=1
    self._callit('begin_'+style+'_list_item',args,kwds)

