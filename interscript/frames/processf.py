#line 22 "interscript/src/process_frame.ipk"
# Note this definition must go here to avoid circular module imports
class process_fault(SystemExit): pass

from interscript.frames.site import site_frame
from interscript.frames.platform import platform
from interscript.frames.masterf import master_frame
from interscript.drivers.sources.base import eoi
import sys
import traceback
import time
import string

class process_frame:
  def __init__(self, global_frame, process_options, argument_frames):
    self.global_frame = global_frame
    self.process_options = process_options
    self.argument_frames = argument_frames

    self.trace = self.process_options.trace
    self.break_on_error = process_options.break_on_error
    self.debug_constructors = 0
    self.debug_destructors = 0
    self.update_files = 1

    self.site = site_frame(platform)
    # self.site.print_install()
    self.objects = {}

  def __del__(self):
    #print 'PROCESS TERMINATING'
    if self.objects:
      print('UNDELETED OBJECTS!!',self.objects)

  def run(self):
    oldstdout = sys.stdout
    oldstderr = sys.stderr
    f = self.process_options.logfile
    m = self.process_options.logfile_mode
    if self.process_options.logfile:
      try:
        sys.stderr = sys.stdout = open(f,m)
      except IOError:
        print('Cannot open specified logfile',f)
      except:
        print('Weird error opening specified logfile',f)
        traceback.print_exc()

    reference_date = time.time()
    local_time = time.localtime(reference_date)
    local_time_string = time.strftime("%a %d %b, %Y %H:%M:%S (%Z)",local_time)
    start_time = time.clock()
    if m: print('<CDATA>')
    print()
    print('---------------------------------')
    print('Interscript '+self.global_frame.version +\
      '['+str(self.global_frame.buildno)+'] Process',local_time_string)

    try:
      for argument_frame in self.argument_frames:
        master_frame(self,argument_frame)
    finally:
      end_time = time.clock()
      elapsed_time = end_time - start_time
      print('Elapsed Process Time',int(elapsed_time),'seconds')
      print('================================')
      print()
      sys.stdout = oldstdout
      sys.stderr = oldstderr

  def get_process_frame(self):
    "Get the current process frame"
    return self

#line 97 "interscript/src/process_frame.ipk"
  def py_exec(self,py,file,count,dict):
    # get a lock here, release it later
    try:
      if 'script' in self.trace: print('Executing',py)
      code  = compile(py,file + "[%2d]" % count,'exec')
      exec(code, dict,dict)
    except KeyboardInterrupt:
      self.update_files = 0
      raise process_fault('Keyboard Interrupt')
    except eoi: raise
    except process_fault: raise
    except:
      print('-------------------------------------')
      print('ERROR EXECUTING CLIENT PYTHON SCRIPT')
      fileid = file+'[%2d]' % count
      print('CONTEXT')
      print('File:', fileid)
      code_lines = py.split('\n')
      i = 1
      for line in code_lines:
        print('  %2d:' % i,line)
        i = i + 1
      #traceback.print_exc()
      print('TRACEBACK (innermost last)')
      exc_type, exc_value, exc_traceback = sys.exc_info()
      tr = traceback.extract_tb(exc_traceback)
      for filename, lineno, function, line in tr:
        if function != '?': location= function
        else: location= 'mainline'
        print('  File:',filename,'[%2d]'%lineno,'in',location)
        if line: print('  ->',line)
        elif fileid== filename:
          print('  +>',code_lines[lineno-1])

      print('EXCEPTION:', end=' ')
      exc_desc=traceback.format_exception_only(exc_type, exc_value)
      lines = '\n'.join(exc_desc).split('\n')
      lines = [_f for _f in lines if _f]
      if len(lines)!=1:
        print()
        for line in lines:
          if line: print(' ',line)
      else: print(' ',lines[0])

      del exc_type, exc_value, exc_traceback
      try: raise eoi
      except: pass

      if self.break_on_error:
        self.update_files = 0
        print('BREAKING ON ERROR')
        print('-------------------------------------')
        raise process_fault('Unexpected Exception')
      else:
        print('IGNORING ERROR, CONTINUING')
        print('-------------------------------------')

#line 157 "interscript/src/process_frame.ipk"
  def acquire_object(self,x, descr):
    self.objects[id(x)]=descr
    print('CREATING',hex(id(x)),descr)

  def release_object(self,x):
    print('DELETING',hex(id(x)),self.objects[id(x)])
    del self.objects[id(x)]


