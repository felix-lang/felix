# build system utility module
import os
import sys
import glob
import time
import StringIO
import shutil

def filetime(f):
  try:
    return os.path.getmtime(f)
  except EnvironmentError:
    return 0

# returns the time of the newest file of a set
# if a file is missing, the time is in the future
# (since we have no record of when it was deleted,
# we assume it was vey recently :)

def newest_filetime(fs):
  m = 0
  for f in fs:
    x = filetime(f)
    if x == 0: return time.time()+1000.0
    m = max(m,x)
  return m

# returns the time of the oldest file of a set
# if a file is missing, the time is before the
# birth of the universe .. well PC's anyhow :)

def oldest_filetime(fs):
  m = 0
  for f in fs:
    x = filetime(f)
    if x == 0: raise MakeError # missing files not allowed
    m = max(m,x)
  return m

def fmtime(t):
    s = "%04d %02d %02d %02d %02d %02d" % (time.localtime(t)[:6])
    return s

def append_unique(s,x):
  if x not in s: return s+[x]
  else: return s

def closure1(d,i,o):
  if i not in o:
    o = o + [i]
    e = d.get(i,[])
    for k in e:
      if k not in o:
        o = closure1(d,k,o)
  return o

# d is a dictionary T -> T list
# s is a list
# closure (d,s) returns the closure of s wrt d
#
# normally d is a dependency map for packages
# and s is set of root packages to be rebuilt
# result is the all the packages that need rebuild

def closure(d,s):
  o = []
  for i in s:
    o = closure1(d,i,o)
  return o

# given a map T -> T list
# return the inverse map

def invert(d):
  m = {}
  for k in d.keys():
    for v in d[k]:
      m[v] = append_unique(m.get(v,[]),k)
  return m

def erasefile(f):
  try: os.unlink(f)
  except EnvironmentError: pass

def unix2native(f):
  if os.path.splitdrive(f)[0] or f.startswith(os.sep):
    return f

  return os.path.join(*f.split('/'))

def deletefile(f):
  erasefile(unix2native(f))

def mkdirs(x):
  if x and not os.path.exists(x):
    os.makedirs(x)

def erasedir(d):
  fs = glob.glob(d+os.sep+"*")
  for f in fs: erasefile(f)
  try: os.rmdir(d)
  except EnvironmentError: pass

def execute(cmd, verbose=False, quiet=False, invert_result=False, log=None):
  if log is None: log = sys.stdout

  cmd = ' '.join(cmd)

  if verbose and not quiet: print >> log, '>', cmd
  log.flush()

  fout = os.popen(cmd, 'r')
  stdout = []
  try:
    for line in fout:
      stdout.append(line)

      if verbose and not quiet:
        log.write(line)
  finally:
    result = fout.close()

  log.flush()

  if invert_result:
    if result:
      result = 0
    else:
      result = 1

  if quiet < 2:
    if result and not verbose:
      print >> log, cmd, 'failed!'

    if result and not invert_result:
      print >> log, '  .. ERROR CODE', hex(result), ':', cmd

  log.flush()

  if result:
    raise ExecutionError(cmd, result)

  return stdout

def xqt(*commands, **kwds):
  return execute(commands, **kwds)

def file_exists(f):
  try:
    os.stat(f)
    return 1
  except EnvironmentError: return 0

class Tee(object):
  def __init__(self, stdout=sys.stdout):
    self.stdout = stdout
    self.file = StringIO.StringIO()

  def write(self, s, quiet=0):
    if not quiet:
      self.stdout.write(s)
    self.file.write(s)

  def flush(self):
    self.stdout.flush()
    self.file.flush()

  def getvalue(self):
    return self.file.getvalue()

def tee_cmd(cmd, stdout, bufsize=1024):
  # we try to use subprocess because we can get the exit code on windows
  try:
    import subprocess
  except ImportError:
    p = os.popen(' '.join(cmd), 'r', bufsize)
    while 1:
      buf = os.read(p.fileno(), bufsize)
      if buf:
        stdout.write(buf)
      else:
        break
    return p.close()
  else:
    p = subprocess.Popen(cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        bufsize=bufsize)
    while True:
      buf = os.read(p.stdout.fileno(), bufsize)
      if buf:
        stdout.write(buf)
      else:
        break

    return p.wait()


class MakeError(EnvironmentError):
  def __init__(self, command=None, stdout=[], stderr=[]):
    self.command = command
    self.stdout = ''.join(stdout)
    self.stderr = ''.join(stderr)

  def __str__(self):
    s = []
    if self.command is not None:
      s.append('COMMAND: ' + self.command)

    if self.stdout:
      s.append('STDOUT:\n' + self.stdout)

    if self.stderr:
      s.append('STDERR:\n' + self.stderr)

    return '\n'.join(s)

class ExecutionError(Exception):
  def __init__(self, command, returncode=None):
    self.command = command
    self.returncode = returncode

  def __str__(self):
    if self.returncode is None:
      return 'Command failed: %s' % self.command
    else:
      return 'Command failed [%s]: %s' % (self.returncode, self.command)

class MissingFile(Exception):
    def __init__(self, filename):
        self.filename = filename

    def __str__(self):
        return 'File not found: ' + self.filename
