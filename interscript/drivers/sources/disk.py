#line 184 "interscript/src/source_drivers.ipk"
#---------------------------------------------------------
# gets input from a named file
from interscript.drivers.sources import source_open_error
from interscript.drivers.sources.base import file_source
from interscript.drivers.sources.base import eof
import string
import os

# this routine is a utility which loads data directly
# from an operating system level file

def loadfile(filename):
   "return a list of lines, trailing whitespace removed"
   try:
     f = open(filename)
   except:
     raise source_open_error(filename)
   data = f.readlines()
   f.close()
   for i in range(len(data)):
     data[i]=data[i].rstrip()
   return data

def parse_source_filename(filename, prefix):
  pathlist = filename.split('/')
  if prefix == '':
    prefix = os.getcwd()
    if prefix[-1] != os.sep:
      prefix = prefix + os.sep
  directory = prefix + os.sep.join(pathlist[:-1])
  if directory[-1] != os.sep:
    directory = directory + os.sep
  basename = pathlist[-1]
  full_filename = directory + basename
  return pathlist, basename, directory, full_filename

class named_file_source(file_source):
  def __init__(self,pass_frame,filename, prefix='', encoding='utf8', **kwds):
    file_source.__init__(*(self,encoding), **kwds)
    self.iflist_index = len(pass_frame.iflist)
    pass_frame.iflist.append([filename,0])
    self.name = filename
    self.pass_frame = pass_frame
    self.process = pass_frame.process
    if 'sources' in self.process.trace:
      self.process.acquire_object(self, 'NAMED FILE SOURCE '+self.name+' ['+encoding+']')

    pathlist, self.basename, self.directory, self.filename =\
      parse_source_filename(filename, prefix)

    try:
      self.file = open(self.filename,'r')
      self.closed = 0
    except:
      raise source_open_error(filename)

  # override this, to get more filename for #line directives
  def get_source_name(self):
    return self.filename

  def __del__(self):
    file_source.__del__(self)
    self.pass_frame.iflist[self.iflist_index][1]=self.lines_read
    if 'sources' in self.process.trace:
      self.process.release_object(self)

