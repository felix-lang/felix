
import string
import traceback
import sys
import interscript

from interscript.drivers.sources import source_open_error
from interscript.drivers.sources.disk import named_file_source
from interscript.drivers.sources.base import eoi
from interscript.frames.inputf import input_frame
from interscript.drivers.sources.base import eoi

class pass_frame:
  def __init__(self,master, passno, skiplist):
    self.skiplist = skiplist
    # the display
    self.master = master
    self.process = master.process
    if 'frames' in self.process.trace:
      self.process.acquire_object(self, 'PASS FRAME')
    self.passno = passno

    self.inhibit_sref = master.inhibit_sref

    self.ids = {}
    self.flist = []
    self.fdict = {}
    self.iflist = []
    self.toc = []
    self.include_files = []
    self.classes = {}
    self.functions = {}
    self.testno = 0
    self.sequence = 0
    self.tests = {}
    self.section_index = {}
    self.ftp_list = []

    file = self.master.filename
    encoding = self.master.encoding
    encoding = encoding.lower().replace('-','_')
    if 'frames' in self.process.trace:
      print('Processing',file,'Pass',passno+1,'Encoding',encoding)



    basename = file
    if file.find('.') != -1:
      basename = '.'.join(file.split('.')[:-1])

    weaver = None
    userdict = { }

    try:
      input_file =named_file_source(self,file, self.master.source_prefix, encoding = encoding)

    except source_open_error as filename:
      print('Cannot Open File',filename,'for input (ignored)')
      raise
    except KeyboardInterrupt:
      raise
    except:
      print("Program error opening",file)
      traceback.print_exc()
      raise

    self.include_files.append((1,'interscript',file))
    inpt = input_frame(
      self,
      input_file,
      [],
      weaver,
      userdict,
      1)
    del input_file
    del weaver
    del userdict

    inpt.set_warning_character(python='@')
    if 'input' in self.process.trace:
      print('input from',inpt.source.get_source_name())

    inpt.file_pass()
    # at this point, inpt, weaver, userdict, input_file
    # should all be released (even if 'pass_frame' is held onto,
    # these symbols are defined in the __init__ function frame)
    del inpt
    try: raise eoi
    except: pass

  def __del__(self):
    if 'frames' in self.process.trace:
      print('Processing of',self.master.filename,'Pass',self.passno+1,'complete')
      self.process.release_object(self)

  def get_pass_frame(self):
    "Get the current pass frame"
    return self

  def get_new_test_number(self):
    "Get a new test case number"
    self.testno = self.testno + 1
    return self.testno

  def get_new_sequence_number(self):
    "Get a new serial number"
    self.sequence = self.sequence + 1
    return self.sequence

