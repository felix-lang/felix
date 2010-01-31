#line 5 "interscript/src/stacks.ipk"
class stack:
  __class_protocols__ = ['sequence','mutable']
  def __init__(self, *args): self.s = list(args)
  def __del__(self):
    # unbind in FILO order!
    while len(self.s): del self.s[-1]

  def pop(self):
    tmp = self.s[-1]
    del self.s[-1]
    return tmp
  def push(self,x): self.s.append(x)
  def append(self,x): self.s.append(x)
  def __len__(self): return len(self.s)
  def __bool__(self): return len(self.s)!=0
  def __getitem__(self,index): return self.s[index]
  def __setitem__(self,index,value): self.s[index]=value
  def __delitem__(self,index): del self.s[index]
  def __getslice__(self,i,j): return self.s[i:j]
  def __setslice__(self,i,j,seq): self.s[i:j]=seq
  def __delslice__(self,i,j): del self.s[i:j]
  def __mul__(self,i): return stack(*tuple(self.s * i))
  def __rmul__(self,i): return stack(*tuple(self.s * i))
  def __add__(self,s): return stack(*tuple(self.s + s.s))
  def __cmp__(self, other): return cmp(self.s,other.s)
  def __repr__(self):
    s = 'stack('
    if self.s: s = s + repr(self.s[0])
    for i in self.s[1:]: s = s + ', '+repr(i)
    return s+')'

  def __setattr__(self,attr,value):
    if attr == 'top':
      self.s[-1]=value
    else:
      self.__dict__[attr]=value

  def __getattr__(self,attr):
    if attr=='top':
      return self.s[-1]
    else:
      raise AttributeError(attr)

  def __delattr__(self,attr):
    if attr=='top':
      del self.s[-1]
    else:
      raise AttributeError(attr)

  def copy(self): return stack(*tuple(self.s))
  def count(self, item): return self.s.count(item)
  def index(self, item): return self.s.index(item)
  def sort(self, order=None):
    if order == None:
      self.s.sort()
    else:
      self.s.sort(order)
  def insert(self,index,item): self.s.insert(index,item)
  def remove(self,item): self.s.remove(item)
  def reverse(self): self.s.reverse()

