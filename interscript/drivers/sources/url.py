#line 296 "interscript/src/source_drivers.ipk"
# gets input from a URL
from interscript.drivers.sources.base import file_source
from interscript.drivers.sources.base import eof
import string
class url_source(file_source):
  def __init__(self,filename,encoding='utf8',**kwds):
    file_source.__init__(*(self,encoding), **kwds)
    self.name = filename
    self.file = urllib.request.urlopen(filename)
    self.closed = 0


