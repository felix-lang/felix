#line 10 "interscript/src/interscript_options.ipk"
import sys
import traceback
import string
import glob
import os

from interscript.getoptions import getopt

#line 20 "interscript/src/interscript_options.ipk"
#option help dictionary
shortoptdict = { 'v':'verbose (trace everything)' }

longoptdict = {
  'weaver=': {
    'html': 'flat html',
    'latex': 'latex2e',
    'llambda': 'lambda',
    'text':'plain text',
    'web':'html tree',
    'lout':'lout'
  },

  'inhibit-sref':{
    0:'generate original src xref(default)',
    1:'inhibit generation of src xrefs'},
  'language=': 'two letter code for human language to weave (mandatory if weaver selected)',
  'tangler-prefix=':'absolute native os prefix prepended to tangled code filenames',
  'weaver-prefix=':'absolute native os prefix prepended to woven documentation filenames',
  'tangler-directory=':'interscript filename prefix prepended to tangled code filenames',
  'weaver-directory=':'interscript filename prefix prepended to woven documentation filenames',
  'python=':'execute python script',
  'update=':{
    0:'Allow buffered file write (default)',
    1:'Inhibit buffered file write'},
  'download=':{
    'never':'do not download from the Internet',
    'always':'force download by ftp or http'},
  'refresh_interval=':
    'download when local file is older than this number of days (default 28)',
  'tabwidth=':'column width for tab expansion (default 8)',
  'passes=':'passs on each file (default 1)',
  'logfile=':'<filename> for messages (append to old file)',
  'new-logfile=':'<filename> for messages (cleared first)',
  'nocache':'disable persistent cache usage',
  'copyright': '(prints) 1998 John Skaller, Australia',
  'licence': '(prints) Free for any use',
  'author': '(prints) mailto:skaller@users.sf.next <John Skaller>',
  'homepage': '(prints) http://interscript.sf.net',
  'executable': 'print python executable name',
  'python-version': 'print python version string',
  'title=':'set document title',
  'encoding=':'encoding of file, defaults to utf8',
  'test':'MUST BE FIRST: try to load interscript from current directory',
  'html-eol=': {
    'CRLF': 'Kludge Unix host (only) to end html lines (only) with CR/LF'
  },
  'trace=':{
    'frames'  : 'creation and destruction of architectural frames',
    'weavers' : 'creation and destruction of weavers',
    'tanglers': 'creation and destruction of tanglers',
    'sinks'   : 'creation and destructioin of sinks',
    'sources' : 'opening and closing of sources',
    'changes' : 'changed outputs',
    'script'  : 'execution of client script',
    'options' : 'dump options',
    'input'   : 'list all input lines',
    'cache'   : 'contents of persistent storage on loading and saving',
    'deps'    : 'source file dependency and change tracking'
  },
  'help':'this help',
  'usage':'this help' }


def print_help():
  print('Usage: python iscr.py [options] <filename>')
  print('Short options:')
  keys = list(shortoptdict.keys())
  keys.sort()
  for k in keys: print_help1(k)
  print('Long options:')
  keys = list(longoptdict.keys())
  keys.sort()
  for k in keys: print_help1(k)

def print_help1(k):
  if k in longoptdict:
    usek = '--'+ k
    values = longoptdict[k]
  elif k+'=' in longoptdict:
    usek = '--'+ k + '='
    values = longoptdict[k+'=']
  elif k in shortoptdict:
    usek = '-' + k
    values = shortoptdict[k]
  elif k+'=' in shortoptdict:
    usek = '-' + k + '='
    values = shortoptdict[k+'=']
  else:
    usek = k
    values = 'Unknown option'

  print('  '+usek, end=' ')
  if values is None:
    print()
  elif type(values) is type({}):
    print()
    for value in list(values.keys()):
      print('   '+str(value)+':',values[value])
  else:
    print(values)

#line 125 "interscript/src/interscript_options.ipk"
class argument_frame:
  def copy(self):
     other = argument_frame()
     for k in list(self.__dict__.keys()):
       setattr(other,k,getattr(self,k))
     return other

def getoption_frames(args): # note: has side effects!
  parsed =  getopt(args)
  process_options = argument_frame()
  process_options.logfile = None
  process_options.logfile_mode = None
  process_options.break_on_error = 0
  process_options.args = args
  process_options.trace = []
  master_frames = []

  frame = argument_frame()
  frame.update_files = 1
  frame.tabwidth = 8
  frame.download = 'regularly'
  frame.refresh_interval = 28
  frame.usecache = 1
  frame.passes = 1
  frame.weaver_prefix = ''
  frame.tangler_prefix = ''
  frame.weaver_directory= ''
  frame.tangler_directory = ''
  frame.autoweave = []
  frame.useropt = {}
  frame.encoding='utf8'
  frame.html_eol = '\n'
  frame.title = None
  frame.languages = []
  frame.inhibit_sref = 0
  frame.cache_prefix = None
  for opts,filename in parsed:
    for opt,value in opts:
      try:
        if opt == 'break-on-error': process_options.break_on_error=1
        elif opt == 'v': process_options.trace = [
          'options',
          'frames',
          'input',
          'weavers',
          'tanglers',
          'lines',
          'sources',
          'sinks',
          'script',
          'cache',
          'deps']
        elif opt == 'inhibit-sref': frame.inhibit_sref = value
        elif opt == 'noupdate': frame.update_files = 0
        elif opt == 'nocache': frame.usecache = 0
        elif opt == 'nodownload': frame.download = 'never'
        elif opt == 'download': frame.download = 'always'
        elif opt == 'tabwidth': frame.tabwidth = int(value)
        elif opt == 'passes': frame.passes = int(value)
        elif opt == 'weaver': frame.autoweave.append(value)
        elif opt == 'weaver-prefix': frame.weaver_prefix = value
        elif opt == 'title': frame.title = value
        elif opt == 'tangler-prefix': frame.tangler_prefix = value
        elif opt == 'weaver-directory': frame.weaver_directory = value
        elif opt == 'language': frame.languages.append(value)
        elif opt == 'encoding': frame.encoding=value
        elif opt == 'trace': process_options.trace.append(value)
        elif opt == 'cache-prefix': frame.cache_prefix = value
        elif opt == 'html-eol':
          if sys.platform == 'Win32':
            print('CRLF kludge ignored for Win32')
            print('Use on Unix only, to make html files in DOS format')
          else:
            frame.html_eol = '\r\n'
        elif opt == 'tangler-directory': frame.tangler_directory = value
        elif opt == 'homepage':
          print('http://interscript.sf.net')
        elif opt == 'author':
          print('mailto:skaller@users.sf.net <John Skaller>')
        elif opt == 'copyright':
          print('Copyright (C)1998 John Skaller, Australia')
        elif opt == 'licence':
          print('Free for any use')
        elif opt == 'executable':
          print(sys.executable)
        elif opt == 'python-version':
          print(sys.version)
        elif opt == 'python':
          try:
            if 'script' in process_options.trace:
              print('Executing python:')
              print(value)
            exec(value)
          except:
            print('Error in python option')
            traceback.print_exc()
        elif opt == 'logfile':
          process_options.logfile = value
          process_options.logfile_mode = 'a'
        elif opt == 'new-logfile':
          process_options.logfile = value
          process_options.logfile_mode = 'w'
        elif opt in ['help', 'usage']:
          print_help()
          print()
        else:
          # FIX: all options should be OK (user options?)
          print('Nonstandard option',opt,'value',value,'accepted as user option')
          frame.useropt[opt]=value
        if 'options' in process_options.trace: print('Option:',opt,value)
      except:
        print('Warning: Option',opt,'has bad value',value)
        prefix = ''
        while opt[0]=='-': prefix = prefix + '-'; opt=opt[1:]
        print_help1(opt)

    files = glob.glob( filename)
    for file in files:
      frame.source_prefix, frame.filename = os.path.split(file)
      if frame.source_prefix != '':
        frame.source_prefix = frame.source_prefix + os.sep
      master_frames.append(frame.copy())
  return process_options, master_frames


