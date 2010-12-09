#line 588 "interscript/src/source_drivers.ipk"
#---------------------------------------------------------
# gets input by HTTP
from interscript.drivers.sources.base import file_source
from interscript.drivers.sources.base import eof
import os
import time
import http.client
import string

class http_file_source(file_source):
  def __init__(self,host,remote_filename,encoding='utf8',**kwds):
    appy(file_source.__init__, (self,encoding), kwds)
    self.name = remote_filename
    self.remote_filename = remote_filename
    self.host = host
    self.g = g
    for k in list(kwds.keys()):
      self.__dict__[k]=kwds[k]
    if not hasattr(self,'local_filename'):
      self.local_filename = self.remote_filename
    self.os = os
    self.fetch()
    self.file = open(self.local_filename,'r')
    self.closed = 0

  def fetch(self):
    if not hasattr(self,'refresh_interval'):
      self.refresh_interval = 28
    if self.refresh_interval < 0: self.refresh_interval = 100000
    self.local_file_exists = 1
    try:
      f = open(self.local_filename)
      f.close()
      print('local file',self.local_filename,'exists')
    except:
      print('local file',self.local_filename,'does NOT exist')
      self.local_file_exists = 0

    if self.local_file_exists:
      self.local_file_modify_time = os.stat(self.local_filename)[stat.ST_MTIME]
      now = time.time()
      age = (now - self.local_file_modify_time)/ (24 * 60 * 60)
      download = age > self.refresh_interval
    else:
      download = 1

    if hasattr(self.g,'download'):
      if self.g.download == 'always': download = 1
      if self.g.download == 'never': download = 0

    if download:
      try:
        print('downloading',self.remote_filename)
        # create HTTP object
        http = http.client.HTTP()

        # connect to server
        if hasattr(self,'port'):
          http.connect(self.host+':'+str(self.port))
        else:
          ftp.connect(self.host)
        print('connected to',self.host)

        # set remote directory
        to_download = self.remote_filename
        if hasattr(self,'remote_directory'):
          to_download = to_download + '/' + self.remote_directory

        # get file to a temporary
        try:
          http.putrequest('GET',to_download)
          http.putheader('Accept','text/html')
          http.putheader('Accept','text/plain')
          http.endheaders()
          errcode, errmsg, headers = http.getreply()
          if errcode != 200: raise 'http error '+str(errcode)+'; '+errmsg
          file = http.getfile()
          newlines = file.readlines()
          file.close()
          print('download complete')

          if self.local_file_exists:
            file = open(self.local_filename,'r')
            oldlines = file.readlines()
            file.close()

            if newlines != oldlines:
              print('Local file',self.local_filename,'UPDATED from',self.remote_filename)
            else:
              print('Local file',self.local_filename,'unchanged')
          else:
            print('Writing new local file',self.local_filename)

          # note that the local file is written even if it isn't changed
          # to update the time stamp
          file = open(self.local_filename,'w')
          file.writelines(newlines)
          file.close()

        except:
          print('Cannot download',self.remote_filename, end=' ')
          if hasattr(self,'remote_directory'):
            print('from directory',self.remote_directory)
          else: print('of',self.host)
          try:
            print('code',errcode,'msg',errmsg)
          except:
            pass
      except:
        pass # ignore errors from ftp attempt
    else:
      print('Skipping http download')

    self.file = open(self.local_filename,'r')

