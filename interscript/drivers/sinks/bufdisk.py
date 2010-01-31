#line 233 "interscript/src/sink_drivers.ipk"
import string
from interscript.drivers.sinks.base import sink_base
from interscript.drivers.sinks import sink_open_error

class named_file_sink(sink_base):
  def __init__(self,pass_frame,input_filename, prefix='', eol='\n'):
    self.pass_frame = pass_frame
    self.process = pass_frame.process
    self.site = self.process.site
    self.platform = self.site.platform
    if 'sinks' in self.process.trace:
      self.process.acquire_object(self,'NAMED FILE SINK '+input_filename)
    self.eol = eol

    # compute absolute pathname, and create directories if necessary
    # we don't use posixpath because we're enforcing an _interscript_
    # pathname convention here
    pathlist = input_filename.split('/')
    self.basename = pathlist[-1]
    pathname = self.platform.mk_dir(prefix, pathlist)

    if self.platform.file_exists(pathname):
      self.tmp_filename = self.platform.tempfile.mktemp()
      if 'sinks' in self.process.trace:
        print('Generating temporary',self.tmp_filename,'for',input_filename)
      try:
        file =self.platform.open(self.tmp_filename,'w')
        self.pass_frame.fdict[input_filename]='temporary'
      except:
        raise sink_open_error(self.tmp_filename)
      sink_base.__init__(self, filename = pathname, name = input_filename, file = file )
      if pass_frame: self.pass_frame.flist.append(input_filename)
    else:
      if 'sinks' in self.process.trace:
        print('Generating original',input_filename)
      try:
        file = self.platform.open(pathname,'w')
        if pass_frame: self.pass_frame.fdict[input_filename]='original'
      except:
        raise sink_open_error(pathname)
      sink_base.__init__(self, filename = pathname, name = input_filename, file = file)
      if pass_frame: self.pass_frame.flist.append(input_filename)

  def __del__(self):
    pass_frame = self.__dict__.get('pass_frame',None)
    if 'sinks' in self.process.trace:
      print('closing', self.name)
    self.file.close()
    if hasattr(self,'tmp_filename'):
      if self.process.update_files:
        original_file = self.platform.open(self.filename,'r')
        original_lines = original_file.readlines()
        original_file.close()

        new_file = self.platform.open(self.tmp_filename,'r')
        new_lines = new_file.readlines()
        new_file.close()

        if not original_lines == new_lines:
          if 'changes' in self.process.trace:
            print('File',self.filename,'is CHANGED')
          if pass_frame:
            self.pass_frame.fdict[self.name]='changed'
          file = self.platform.open(self.filename,'w')
          file.writelines(new_lines)
          file.close()
        else:
          if 'changes' in self.process.trace:
            print('File',self.filename,'is unchanged')
          if pass_frame: self.pass_frame.fdict[self.name]='unchanged'
      else:
        print('*** System error inhibiting file update for',self.filename,'***')
        if pass_frame: self.pass_frame.fdict[self.name]='cancelled'
      self.platform.os.remove(self.tmp_filename)
    else:
      if 'changes' in self.process.trace:
        print('File',self.filename,'is NEW')
    if hasattr(self,'process') and 'sinks' in self.process.trace:
      self.process.release_object(self)

  def raw_write(self,line): self.file.write(line)
  def raw_eol(self): self.raw_write(self.eol)

