#line 385 "interscript/src/source_drivers.ipk"
#---------------------------------------------------------
# gets input by FTP
import ftplib
import time
import os
import stat
from interscript.drivers.sources.base import file_source
from interscript.drivers.sources.base import eof
import string
import tempfile
import traceback
from interscript.drivers.sinks.disk import simple_named_file_sink

class dummy: pass

class ftp_file_source(file_source):
  def __init__(self, pass_frame,
    host, remote_filename,
    encoding='utf8', **kwds):
    self.pass_frame = pass_frame
    self.master = pass_frame.master
    self.process = pass_frame.process
    self.site = self.process.site
    self.platform = self.process.site.platform
    self.global_frame = pass_frame.master.process.global_frame
    if 'sources' in self.process.trace:
      self.process.acquire_object(self, 'FTP SOURCE '+remote_filename)

    apply(file_source.__init__, (self,encoding), kwds)

    self.name = remote_filename
    self.remote_filename = remote_filename
    self.host = host

    for k in kwds.keys():
      self.__dict__[k]=kwds[k]
    if not hasattr(self,'local_filename'):
      self.local_filename = self.remote_filename
    if not hasattr(self,'local_prefix'):
      self.local_prefix = ''
    if not hasattr(self,'local_directory'):
      self.local_directory = ''

    if not hasattr(self,'port'):
      self.port = ''
    if not hasattr(self,'remote_directory'):
      self.remote_directory = ''

    self.iflist_index = len(pass_frame.iflist)
    pass_frame.iflist.append([self.local_filename,0])
    pass_frame.ftp_list.append([self.host, self.remote_directory, self.remote_filename, self.local_filename])


    self.os = os
    pathname = self.fetch()
    self.file = open(pathname,'r')
    self.closed = 0

  def transfer(self,data):
    self.file.write(data+'\n')

  def fetch(self):
    if not hasattr(self,'refresh_interval'):
      self.refresh_interval = 28
    if self.refresh_interval < 0: self.refresh_interval = 100000
    pathname = self.platform.mk_dir(self.local_prefix,
      string.split(self.local_directory+self.local_filename, '/'))
    self.local_file_exists = self.platform.file_exists(pathname)

    if self.local_file_exists:
      #print 'Local file exists'
      self.local_file_modify_time = os.stat(pathname)[stat.ST_MTIME]
      now = time.time()
      age = (now - self.local_file_modify_time)/ (24 * 60 * 60)
      download = age > self.refresh_interval
    else:
      print 'Local file does not exist'
      download = 1

    if hasattr(self.global_frame,'download'):
      if self.global_frame.download == 'always': download = 1
      if self.global_frame.download == 'never': download = 0

    if download:
      try:
        if 'sources' in self.process.trace:
          print 'downloading',self.remote_filename
        # create FTP object
        ftp = ftplib.FTP()

        # connect to server
        if self.port:
          ftp.connect(self.host,self.port)
        else:
          ftp.connect(self.host)
        print 'connected to',self.host

        # login to server
        if hasattr(self,'user'):
          if hasattr(self,'password'):
            if hasattr(self,'account'):
              ftp.login(self.user,self.password,self.account)
            else: ftp.login(self.user,self.password)
          else: ftp.login(self.user)
        else: ftp.login()
        if 'sources' in self.process.trace:
          print 'logged in'

        # set remote directory
        if self.remote_directory:
          ftp.cwd(self.remote_directory)
          print 'changed to remote directory',self.remote_directory

        # get file to a temporary
        try:
          tmp_filename = tempfile.mktemp()
          self.file= open(tmp_filename,'w')
          print 'opened',tmp_filename,'for download'
          ftp.retrlines('RETR '+self.remote_filename, self.transfer)
          self.file.close()
          ftp.quit()
          if 'sources' in self.process.trace:
            print 'download complete'

          file = open(tmp_filename,'r')
          newlines = file.readlines()
          file.close()

          if self.local_file_exists:
            file = open(pathname,'r')
            oldlines = file.readlines()
            file.close()

            if newlines != oldlines:
              if 'sources' in self.process.trace:
                print 'Local file',self.local_filename,'UPDATED from',self.remote_filename
            else:
              if 'sources' in self.process.trace:
                print 'Local file',self.local_filename,'unchanged'
          else:
            if 'sources' in self.process.trace:
              print 'Writing new local file',self.local_filename

          # note that the local file is written even if it isn't changed
          # to update the time stamp
          try:
            file = simple_named_file_sink(self.pass_frame,
              self.local_directory+self.local_filename, prefix=self.local_prefix)
            file.writelines(newlines)
            del file
          except:
            print 'Error writing file', self.local_directory+self.local_filename,'in',self.local_prefix
            traceback.print_exc()
            raise

        except:
          print 'ftp error: Cannot download',self.remote_filename,
          if hasattr(self,'remote_directory'):
            print 'from directory',self.remote_directory,
          print 'of',self.host
          file.close()
          self.os.remove(tmp_filename)
          ftp.quit()
          traceback.print_exc()
          raise

      except:
        pass # ignore errors from ftp attempt
    else:
      if 'sources' in self.process.trace:
        print 'Skipping ftp download ftp://'+self.host+'/'+self.remote_directory+'/'+self.remote_filename
    return pathname
  def __del__(self):
    if 'sources' in self.process.trace:
      self.process.release_object(self)

