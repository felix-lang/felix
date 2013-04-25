import os
import sys
import buildsystem
from fbuild.path import Path
import types

# ------------------------------------------------------------------------------

def dodict(f,nm,d,n):
  sp = ' ' * (2 * n)
  f.write(sp + "module "+ nm)
  f.write(sp + "{"+"\n")
  k = list(d.keys())
  k.sort()
  for i in k:
    if i[0:2]!="__":
      v = d[i]
      t = v.__class__.__name__
      strv = str(v)
      if t == "bool":
        if strv=="True": strv="true"
        if strv=="False": strv="false"
      if t=="str":
        strv=repr(v)
        t = "string"
      if t=="type":
        dodict(f,v.__name__, v.__dict__,n+1)
      elif t == "dict":
        f.write(sp + "  // dictionary " + str(i)+"\n")
      elif t == "NoneType":
        strv = "false"
        t = "bool"
      else:
        f.write( sp+"  var "+str(i) + " : " + t+ " = "+ strv+";"+"\n")
  f.write(sp + "}"+"\n")

def build(ctx):
    sys.path = sys.path + [ctx.buildroot] # hack ..
    import pyconfig
    sys.path = sys.path[:-1] # chop off the hack
    try: 
      os.mkdir("host/lib/plat")
    except:
      pass
    dst = ctx.buildroot / "host/lib/plat/build_config.flx"
    f = open(dst,"w")
    dodict(f,"build_config",pyconfig.__dict__,0)
    f.close()


