#line 6 "interscript/src/sets.ipk"
class set:
  __class_protocols__= ['sequence','mutable','set']
  def __init__(self,*args):
    self.s = {}
    for e in args: self.s[e]=None

  # set contains element
  def contains(self,e):
    return self.s.has_key(e)

  # we're an improper subset of the rhs
  def le(self,rhs):
    for e in self.s.keys():
      if e not in rhs: return 0
    return 1

  # the rhs is an improper subset of us
  def ge(self,rhs):
    for e in rhs:
      if not self.s.has_key(e): return 0
    return 1

  def eq(self,rhs):
    return self.le(rhs) and self.ge(rhs)

  def ne(self,rhs):
    for e in rhs:
      if not self.s.has_key(e): return 1
    for e in self.s.keys():
      if not e in rhs: return 1
    return 0

  def gt(self,rhs):
    return self.ne(rhs) and self.ge(rhs)

  def lt(self,rhs):
    return self.ne(rhs) and self.le(rhs)

  def min(self):
    return min(self.s.keys())

  def max(self):
    return max(self.s.keys())

  def index(self,e):
    return self.s.keys().index(e)

  def count(self,e):
    return self.s.has_key(e)

  # ensure set contains element; no error if already in set
  def insert(self,e):
    self.s[e]=None

  # remove element, must be in set or error
  def remove(self,e):
    del self.s[e]

  # remove element if in set
  def excise(self,e):
    if self.s.has_key(e): del self.s[e]

  # append all the elements in the sequence
  def append_sequence(self,seq):
    for e in seq: self.s[e]=None

  # get list of elements
  def list(self):
    return self.s.keys()

  # get tuple of elements
  def tuple(self):
    return tuple(self.s.keys())

  # get dictionary of elements
  def dict(self):
    return self.s.copy()

  # return a copy of this set
  def copy(self):
    s = set()
    s.s = self.s.copy()
    return s

  # repr is set(e1, e2, e3) etc
  def __repr__(self):
    keys = self.s.keys()
    p = 'set('
    if keys: p = p + repr(keys[0])
    for key in keys[1:]: p = p + ', ' + repr(key)
    p = p + ')'
    return p

  # 0 if empty, 1 otherwise
  def __nonzero__(self):
    return len(s)!=0

  # lexicographical comparison!
  # a < b does NOT mean a is a subset of b!!!

  def __cmp__(self,other):
    right = set()
    for e in other: right.insert(e)
    k1 = self.s.keys()
    k1.sort()
    k2 = right.s.keys()
    k2.sort()
    return cmp(k1,k2)

  def __len__(self):
    return len(self.s)

  def __getitem__(self,index):
    return self.s.keys()[index]

  def __delitem__(self,index):
    k = self.s.keys()[index]
    del self.s[k]

  def __getslice__(self,i,j):
    return apply(set,tuple(self.s.keys()[i:j]))

  def __and__(self,right):
    s = set()
    for e in self.s.keys():
      if e in right: s.insert(e)
    return s

  def __or__(self,right):
    s = set()
    s.s = self.s.copy()
    for e in right: s.s[e]=None
    return s

  def __xor__(self,right):
    s = set()
    for e in right: s.insert(e)
    for e in self.s.keys():
      if s.s.has_key(e): del s.s[e]
      else: s.s[e]=None
    return s

  def __add__(self,right):
    return self.__or__(right)

  def __sub__(self,right):
    s = set()
    for e in self.s.keys():
      if e not in right: s.insert(e)
    return s

