#line 21 "interscript/src/platform_frame.ipk"
import sys
import os
import string
import errno
import tempfile

class platform_frame:
  def __init__(self):
    self.python_plat = sys.platform
    self.os = os
    self.tempfile = tempfile
    self.errno = errno
    self.open = open

    self.uname = ['unknown','unknown','unknown']
    try:
      # operating system name
      f = self.os.popen('uname -s 2>tmp.tmp','r')
      self.uname[0] = f.read()
      f.close()
      os.remove('tmp.tmp')
      del f

      # operating system version
      f = self.os.popen('uname -v 2>tmp.tmp','r')
      self.uname[1] = f.read()
      f.close()
      os.remove('tmp.tmp')
      del f

      # operating system release
      f = self.os.popen('uname -r 2>tmp.tmp','r')
      self.uname[2] = f.read()
      f.close()
      os.remove('tmp.tmp')
      del f
    except:
      pass# OS dependent routines.

  # Note: we use some posixpath functions, but don't trust them

  # make the given directory, no error if it exists
  # this is posix specific, and should be moved to a platform
  # dependent place in the code

  def create_directory(self,dir):
    if not self.os.path.isdir(dir):
      try: self.os.mkdir(dir)
      except os.error, data:
        if data[0]!=self.errno.EEXIST: # File Exists is OK, everything else is fatal
          raise os.error, (data[0],data+': directory "'+dir+'"')
    if not self.os.path.isdir(dir):
      raise self.os.error, (self.errno.ENOENT, 'Created a directory '+dir+', but it is not there!')

  # given an os specific prefix and a list of component names,
  # make the directory structure in which the last component is contained
  # and synthesise and return its full os specific pathname

  def mk_dir(self,prefix, pathlist):
    if len(pathlist)>1:
      # there's more than one component in the list
      # so create directories for all but the last component

      pathname = prefix+pathlist[0]
      self.create_directory(pathname)
      for component in pathlist[1:-1]:
        pathname = pathname + self.os.sep + component
        self.create_directory(pathname)
      pathname = pathname + self.os.sep + pathlist[-1]

    else:
      # only one component on the list
      pathname = prefix+pathlist[0]

    if pathname[0]!=self.os.sep:
      # the pathname isn't absolute, so make it so
      # get current directory
      curdir = self.os.getcwd()

      # strip trailing separator
      # note this should fix cases like '/' (unix) or 'd:\' (nt)
      # as well as cope with systems that return a trailing separator
      if curdir[-1] == self.os.sep: curdir = curdir[:-1]

      # append a separator and the pathname: there will only be one
      # separator at the join point unless the current directory
      # ends with two separators (like the URL: http://)
      pathname = curdir + self.os.sep + pathname
    return pathname

  # this routine checks is a file exists and is readable
  # (in the sense that it can be opened for read)
  # it returns 1 if the file exist and can be read, 0 if the file
  # doesn't exist, and throws an exception if anything else goes wrong

  def file_exists(self,pathname):
    try:
      # note this leaks a file if it is opened but not closed :-))
      open(pathname,'r').close()
      return 1
    except IOError, data:
      if data[0] == self.errno.ENOENT:
        return 0
      raise IOError, data

  # Note: the intention is to apply mk_dir to ensure the file has a
  # parent directory, creating it if necessary, then test if the file
  # already exists or has to be created. Even if it exists, it may have
  # to be replaced.

  def map_filename(self, prefix, path):
    return prefix+string.join(string.split(path,'/'),self.os.sep)

  def get_working_directory(self): # native filename
    return os.getcwd()

  def getmtime(self, filename):
    if self.file_exists(filename):
      return self.os.path.getmtime(filename)
    else:
      return 0
