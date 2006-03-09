#line 38 "interscript/src/protocols.ipk"
#-------------- protocol.py ---------------------------
import types

type_protocols = {
  types.NoneType : [],
  types.TypeType : ['type','immutable'],
  types.IntType : ['integer','number','immutable'],
  types.LongType : ['integer','number','immutable'],
  types.FloatType : ['number','immutable'],
  types.StringType : ['string','immutable','filename','url'],
  types.TupleType : ['sequence','immutable'],
  types.ListType : ['sequence','mutable'],
  types.DictType : ['map','mutable'],
  types.FunctionType : ['function'],
  types.LambdaType : ['function'],
  types.CodeType : ['code'],
  types.ClassType : ['class'],
  types.InstanceType : ['instance'],
  types.MethodType : ['function'],
  types.BuiltinFunctionType: ['function'],
  types.ModuleType: ['module'],
  types.FileType: ['file'],
  types.XRangeType: ['range'],
  types.TracebackType: ['traceback'],
  types.FrameType: ['frame'],
  types.SliceType: ['slice'],
  types.EllipsisType: ['ellipsis']
}
try:
  type_protocols[types.ComplexType]='number'
except NameError:
  pass

class provides_attr:
  def __init__(self,name):
    self.name = name

def isclass(obj):
  return type(obj) is types.ClassType

def isinstancetype(obj):
  return type(obj) is types.InstanceType

def classof(obj):
  if isinstancetype(obj):
    return obj.__class__
  else:
    return None

def add_obj_proto(object,protocol):
  if hasattr(object,'__protocols__'):
    getattr(object,'__protocols__').append(protocol)
  else:
    setattr(object,'__protocols__',[protocol])

def add_obj_protos(object,protocols):
  for p in protocols: add_obj_protos(object,p)

def add_class_proto(cls,protocol):
  if hasattr(cls,'__class_protocols__'):
    getattr(cls,'__class_protocols__').append(protocol)
  else:
    setattr(cls,'__class_protocols__',[protocol])

def add_class_protos(object,protocols):
  for p in protocols: add_class_protos(object,p)

def add_type_protos(object,protocols):
  for p in protocols: add_type_protos(object,p)

def add_type_proto(typ, protocol):
  if type_protocols.has_key(typ):
    type_protocols[typ].append(protocol)
  else:
    type_protocols[typ] = [protocol]

def has_class_proto(cls,protocol):
  if cls is protocol: return 1
  if hasattr(cls,'__class_protocols__'):
    if protocol in getattr(cls,'__class_protocols__'): return 1
  return 0

def has_type_proto(object,protocol):
  typ = type(object)
  if typ is protocol: return 1
  if type_protocols.has_key(typ):
    if protocol in type_protocols[typ]: return 1
  return 0

def has_protocol(object,protocol):
  if hasattr(object,'__protocols__'):
    if protocol in getattr(object,'__protocols__'): return 1

  cls = classof(object)
  if cls:
    v = has_class_proto(cls, protocol)
    if v: return 1
    for base in cls.__bases__:
      if has_class_proto(base,protocol): return 1
  if has_type_proto(object,protocol): return 1
  if type(protocol) is types.InstanceType:
    if protocol.__class__ is provides_attr:
      if hasattr(object,protocol.name): return 1
  return 0

def has_protocols(object,protocols):
  for p in protocols:
    if not has_protocol(object,p): return 0
  return 1

