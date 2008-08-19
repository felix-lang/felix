import os

from fbuild.flxbuild.flxutil import xqt, ExecutionError

def pr(f,x):
  print x
  print >> f, x

def pa(f,this,s):
  try:
    x = s + "=" + repr(this[s])
  except (KeyError, EnvironmentError):
    print 'UNABLE TO FIND VALUE FOR ATTRIBUTE:', repr(s)
    print >> f, s, '= None # EDIT ME!!'
  else:
    pr(f,x)

def pne(f,s):
  try:
    x = s + "=" + repr(this[s])
  except EnvironmentError:
    print "UNABLE TO FIND VALUE FOR ATTRIBUTE '"+s+"'"
    print >> f, s, '= None # EDIT ME!!'
  else:
    print >> f, x


def cwrite(c):
  f = "config"+os.sep+""+c+"_config.py"
  print "--------------------------"
  print "Creating "+f
  f= open(f,"w")
  return f

# needs to be conditionalised on Unix
def locate_file(fname):
  print "Try to find",fname
  n = len(fname)
  cmd = "locate " + fname
  try:
    lines = xqt(cmd)
  except ExecutionError:
    print "Cannot execute Unix locate command to find file",fname
    return None

  candidates = []
  for line in lines:
    candidate = line.strip()
    if candidate[-n:] == fname:
      candidates.append(candidate[0:-n])

  if len(candidates) == 0:
    print "Cannot find directory containing file",fname
    return None

  if len(candidates) == 1:
    print "Found unique directory ",candidates[0],"containing file",fname
    return candidates[0]
  else:
    print "Found multiple directories containing file",fname
    s = candidates[0]
    for k in candidates:
      print "Dir: ",k
      if len(k) < len(s): s = k
    print "Using shortest one:",s
    return s
