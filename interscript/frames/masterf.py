#line 23 "interscript/src/master_frame.ipk"
from interscript.frames.passf import pass_frame
from interscript.drivers.sources.disk import parse_source_filename
from interscript.languages.interscript_languages import add_translation
import pickle
import types

class master_frame:
  "Once per document master frame"
  def _cal_deps(self,t,i):
    #print 'caldeps',i
    n = len(t)
    level0, kind0, filename0, filetime0, changed0, tobuild0 = t[i]
    tobuild0 = changed0
    j=i+1
    while j<n:
      if t[j][0] <= level0: break
      tobuild1, j = self._cal_deps(t,j)
      tobuild0 = tobuild0 or tobuild1
    t[i][5]=tobuild0
    return tobuild0, j

  def __init__(self,process,argument_frame):
    self.argument_frame = argument_frame
    self.process = process
    self.site = process.site
    self.platform = process.site.platform

    if 'frames' in process.trace:
      self.process.acquire_object(self,'MASTER FRAME')
    for k in argument_frame.__dict__.keys():
      if 'options' in self.process.trace:
        print 'setting MASTER',k,'as',argument_frame.__dict__[k]
      setattr(self,k,argument_frame.__dict__[k])
    self.ids = {}
    self.iflist = []
    self.ftp_list = []
    self.flist = []
    self.fdict = {}
    self.toc = []
    self.include_files = []
    self.classes = {}
    self.functions = {}
    self.sequence_limit = -1
    self.section_index = {}
    self.tests = {}
    self.noticedict = {}
    self.persistent_frames = {}
    self.cache_age = 1 # very old :-)
    os = self.platform.os
    if self.usecache:
      self.cache_name =self.platform.map_filename(
        self.source_prefix, self.filename+'.cache')
      try:
        if 'cache' in self.process.trace:
          print 'CACHE NAME=',self.cache_name
        cache = self.platform.open(self.cache_name,'r')
        if 'cache' in self.process.trace:
          print 'CACHE FILE OPENED'
        self.persistent_frames = pickle.load(cache)
        cache.close()
        if 'cache' in self.process.trace:
          print 'GOT CACHE'
        self.cache_age = self.platform.getmtime(self.cache_name)
        if 'cache' in self.process.trace:
          print 'AGE=',self.cache_age
          self.dump_cache()
        del cache
      except KeyboardInterrupt: raise
      except:
        if 'cache' in self.process.trace:
          print 'UNABLE TO LOAD CACHE'

    include_files = self.persistent_frames.get('include files',[])
    old_options = self.persistent_frames.get('options',{})

    self.src_tree = []
    for level, kind, filename in include_files:
      f = self.platform.map_filename(self.source_prefix, filename)
      filetime = self.platform.getmtime(f)
      changed = filetime > self.cache_age
      self.src_tree.append([level, kind, filename, filetime, changed, -1])

    if 'deps' in self.process.trace:
      print 'SOURCE FILE CHANGE STATUS DEPENDENCIES'
      print 'CACHE TIME=',self.cache_age
      self.dump_entry(self.src_tree,0)
      print

    src_tree_changed = 1
    dummy = 0
    if len(self.src_tree)>0:
      src_tree_changed, dummy = self._cal_deps(self.src_tree,0)
    del dummy

    if 'deps' in self.process.trace:
      print 'COMPUTED DEPENDENCIES'
      print 'CACHE TIME=',self.cache_age,
      self.dump_entry(self.src_tree,0)
      print
      if src_tree_changed:
        print 'WORK TO BE DONE'
        for level, kind, filename, filetime, changed, tobuild in self.src_tree:
          if changed: print kind,'file',filename,'CHANGED'
        for level, kind, filename, filetime, changed, tobuild in self.src_tree:
          if tobuild: print kind,'file',filename,'WILL BE REBUILT'
        for level, kind, filename, filetime, changed, tobuild in self.src_tree:
          if not tobuild: print kind,'file',filename,'WILL BE SKIPPED'
      else:
        print 'NO FILES CHANGED'
        if self.autoweave:
          print 'WEAVING ENABLED'
        else: print 'NO WEAVING'

    if src_tree_changed:
      skiplist = []
      for level, kind, filename, filetime, changed, tobuild in self.src_tree:
        if not tobuild:
          skiplist.append((level, kind, filename))
    current_options = self.argument_frame.__dict__

    ign_opt = ['trace','passes']
    cur_opt = current_options.copy()
    old_opt = old_options.copy()
    for key in cur_opt.keys():
      if key in ign_opt: del cur_opt[key]
    for key in old_opt.keys():
      if key in ign_opt: del old_opt[key]

    options_changed = old_opt != cur_opt
    del ign_opt, cur_opt, old_opt

    old_converged = self.persistent_frames.get('converged',0)
    if 'deps' in self.process.trace:
      if old_converged:
        print 'PREVIOUS RUN CONVERGED'
      else:
        print 'PREVIOUS RUN DID NOT CONVERGE'
      if options_changed:
        print 'OPTIONS CHANGED'
      else:
        print 'SAME OPTIONS AS BEFORE'

    if (not options_changed) and old_converged and (not src_tree_changed):
      print 'NO WORK TO DO, returning'
      return

    old_skiplist = self.persistent_frames.get('skiplist',[])
    if options_changed:
      print 'PROCESSING WHOLE FILE (options changed)'
      skiplist = []
    elif self.autoweave:
      print 'PROCESSING WHOLE FILE (incremental weaving not supported yet)'
      skiplist = []
    else:
      if old_converged:
        print 'SKIPPING (newly changed files)'
        for sig in skiplist:
          print ' ',sig
      else:
        if src_tree_changed:
          print 'PROCESSING WHOLE FILE (source changed)'
          skiplist = []
        else:
          print 'SKIPPING (files skipped last run, which did not converge)'
          skiplist = old_skiplist
    self.run_passes(skiplist)
    self.persistent_frames['skiplist']=skiplist
    return

#line 216 "interscript/src/master_frame.ipk"
  def run_passes(self, skiplist):
    #print 'STARTING PASSES'
    for passno in range(self.passes):
      converged = self.process_pass(passno, skiplist)
      if converged: break
    #print 'FINISHED PASSES'
    self.persistent_frames['options']=self.argument_frame.__dict__
    self.persistent_frames['include files']=self.include_files
    self.persistent_frames['converged']=converged
    if self.usecache:
      try:
        #print 'WRITING CACHE'
        cache = self.platform.open(self.cache_name,'w')
        pickle.dump(self.persistent_frames, cache)
        cache.close()
        del cache
      except KeyboardInterrupt: raise
      except:
        print 'Pickle FAILURE saving cache',self.cache_name
      if 'cache' in self.process.trace:
        self.dump_cache()

  def __del__(self):
    if 'frames' in self.process.trace:
      self.process.release_object(self)

  def get_master_frame(self):
    "Get the current master frame"
    return self

  def get_persistent_frame(self, seq):
    "Get the persistent frame object with given index"
    if not self.persistent_frames.has_key(seq):
      self.persistent_frames[seq]={}
    return self.persistent_frames[seq]

  def set_title(self, title, **trlat):
    "Specify the document title"
    self.persistent_frames['title'] = title
    apply(add_translation,(title,),trlat)

  def add_author(self, author, **data):
    "Add an author to the list of document authors"
    if not self.persistent_frames.has_key('authors'):
      self.persistent_frames['authors']={}
    if not self.persistent_frames['authors'].has_key(author):
      self.persistent_frames['authors'][author]={}
    self.persistent_frames['authors'][author].update(data)

  def get_title(self):
    "Get the current document title"
    return self.persistent_frames.get('title',None)

  def set_native_language(self, language):
    "Set the native language in which this document is written"
    self.persistent_frames['native_language']=language

  def get_native_language(self):
    "Get the native language in which this document is written"
    return self.persistent_frames.get('native_language','en')

  def set_document_data(self, key, data):
    "Save some data under a key in the persistent store"
    self.persistent_frames[key]=data

  def get_document_data(self,key):
    "Retrive some data using a key from the persistent store"
    return self.persistent_frames.get(key,None)

  def dump_cache(self):
    "Dump out the persistent store to standard output"
    print '--- CACHE DUMP ------------------------------',
    self.dump_dict(self.persistent_frames, 0)
    print

  def dump_sequence(self,s, level):
    for entry in s[:-1]:
      print
      print ' ' * (level * 2),
      self.dump_entry(entry,level)
      print ',',
    if len(s)>0:
      print
      print ' ' * (level * 2),
      self.dump_entry(s[-1],level)

  def dump_dict(self,d, level):
    keys = d.keys()
    keys.sort()
    for key in keys[:-1]:
      print
      if level == 0: print
      print ' '*(level*2)+str(key),':',
      v = d[key]
      self.dump_entry(v, level)
      print ',',
    if len(keys)>0:
      print
      key = keys[-1]
      print ' '*(level*2)+str(key),':',
      v = d[key]
      self.dump_entry(v, level)

  def dump_entry(self,e,level):
      t = type(e)
      if t is types.DictType:
        print '<dict>',
        self.dump_dict(e,level+1)
      elif t is types.TupleType:
        print '<tuple>',
        self.dump_sequence(e, level+1)
      elif t is types.ListType:
        print '<list>',
        self.dump_sequence(e, level+1)
      else:
        print repr(e),

  def process_pass(self, passno, skiplist):
    curpass = pass_frame(self, passno, skiplist)
    self.ids = curpass.ids        # idlist
    self.ftp_list = curpass.ftp_list # ftp list
    self.flist = curpass.flist    # output file list
    self.iflist = curpass.iflist  # input file list
    self.toc = curpass.toc        # table of contents
    self.include_files = curpass.include_files # include files
    self.classes = curpass.classes # classes
    self.functions = curpass.functions # functions
    self.tests = curpass.tests # functions
    self.section_index = curpass.section_index # functions

    if self.sequence_limit == -1:
      self.sequence_limit = curpass.sequence
    elif self.sequence_limit != curpass.sequence:
      print 'WARNING: SEQUENCE COUNTER DISPARITY BETWEEN PASSES'
    fdict = curpass.fdict
    del curpass
    return self.check_convergence(passno, fdict)

  def check_convergence(self, passno, ds):
    dd = self.fdict

    file_count = 0
    stable_file_count = 0
    unstable_file_count = 0
    new_file_count = 0
    for k in ds.keys():
      file_count = file_count + 1
      #print 'Checking file',file_count,':',k,'Status',ds[k]
      if not dd.has_key(k):
        dd[k]=(ds[k],passno)

      if ds[k]=='original':
        new_file_count = new_file_count + 1
      elif ds[k]=='unchanged':
        stable_file_count = stable_file_count + 1
      else:
        unstable_file_count = unstable_file_count + 1
      if ds[k]!='unchanged' or dd[k][0]!='unchanged':
        dd[k]=(ds[k],passno)
    converged = file_count == stable_file_count
    if converged:
      print 'All',file_count,'output files stable on pass',passno+1,' -- breaking'
    else:
      print 'Pass',passno+1,'status: <not converged>'
      print '  Files    :',file_count
      print '  New      :',new_file_count
      print '  Changed  :',unstable_file_count
      print '  Unchanged:',stable_file_count
    return converged


