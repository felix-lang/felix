#line 10 "interscript/src/site_frame.ipk"
import sys
class site_frame:
  def __init__(self, platform):
    self.platform = platform
    self.python_version = sys.version[:3]
    self.builtin_module_names = sys.builtin_module_names
    self.python_module_search_path = sys.path

