#line 123 "interscript/src/compilers.ipk"
import os
import sys
import string
import interscript.compilers.cconfig

class python_module:
  def __init__(self,**kwds):
    self.config = interscript.compilers.cconfig.config()
    self.config.append_dict(kwds)

  def configure(self,**kwds):
    self.config.append_dict(kwds)

  def compile(self,filename, **kwds):
    config = self.config.copy()
    config.append_dict(kwds)

    base = string.join(string.split(filename,'.')[:-1],'.')
    obj = base+'.o'
    cc = 'gcc -g -O2 -fpic -fPIC -pedantic '
    inc = '-I' + sys.prefix + '/include/python1.5 '
    if sys.prefix != sys.exec_prefix:
      inc = inc + '-I' + sys.exec_prefix + '/include/python1.5 '
    cstr = str(config)+' '
    arg = cc + cstr +  inc + '-c '+filename + ' -o '+ obj
    print 'system',repr(arg)
    os.system(arg)
    return obj

  def link(self,modname, filenames, **kwds):
    config = self.config.copy()
    config.append_dict(kwds)

    dll = modname +'.so'
    cc ='gcc -shared -Xlinker -export-dynamic '
    cstr = str(config) + ' '
    lib = '-L' + sys.exec_prefix + '/lib/python1.5 '
    files = string.join(filenames) + ' '
    arg = cc + cstr + lib + files + '-o ' + dll

    print 'system',repr(arg)
    os.system(arg)
    return dll

class application:
  def __init__(self,**kwds):
    self.config = interscript.compilers.cconfig.config()
    self.config.append_dict(kwds)

  def configure(self,**kwds):
    self.config.append_dict(kwds)

  def compile(self,filename, **kwds):
    config = self.config.copy()
    config.append_dict(kwds)

    base = string.join(string.split(filename,'.')[:-1],'.')
    obj = base+'.o'
    cc ='gcc -g -O2 -fpic -fPIC -pedantic '
    inc = '-I' + sys.prefix + '/include/python1.5 '
    if sys.prefix != sys.exec_prefix:
      inc = inc + '-I' + sys.exec_prefix + '/include/python1.5 '
    cstr = str(config)+' '
    arg = cc + cstr +  inc + '-c '+filename + ' -o '+ obj
    print 'system',repr(arg)
    os.system(arg)
    return obj

  def link(self,appname, filenames, **kwds):
    config = self.config.copy()
    config.append_dict(kwds)

    cc ='gcc '
    cstr = str(config) + ' '
    lib = '-L' + sys.exec_prefix + '/lib/python1.5 '
    files = string.join(filenames) + ' '
    arg = cc + cstr + lib + files + '-o ' + appname

    print 'system',repr(arg)
    os.system(arg)
    return appname

