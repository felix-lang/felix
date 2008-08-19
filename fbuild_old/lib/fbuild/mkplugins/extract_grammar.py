import os
import sys

from fbuild.flxbuild.process import Process

class extract_grammar(Process):
  help = 'extract the grammar as a text file from ocamlyacc src'

  def __init__(self, *args, **kwds):
    super(extract_grammar, self).__init__(*args, **kwds)

    self.ran = False

  def runme(self, pkg, pkgdict, *args):
    if pkg != 'flx_parser':
      return
    if self.ran:
      return
    self.ran = True

    print "GENERATING GRAMMAR"
    try:
      os.mkdir('misc')
    except:
      pass
    self.shell(sys.executable,
      os.path.join('script', 'get_grammar'),
      os.path.join('src', 'compiler', 'flx_parser', 'flx_parse.dyp'),
      '>',
      os.path.join('misc', 'flx_parse.grammar'),
    )
    self.shell(sys.executable,
      os.path.join('script', 'flx_flx_grgen'),
      os.path.join('misc', 'flx_parse.grammar'),
      '>',
      os.path.join('lib', 'flx_grammar.flx'),
    )
    self.shell(sys.executable,
      os.path.join('script', 'elk_flx_lexgen'),
      os.path.join('misc', 'flx_parse.grammar'),
      '>',
      os.path.join('misc', 'elk_flx_lex.cc'),
    )
    self.shell(sys.executable,
      os.path.join('script', 'flx_tokgen'),
      os.path.join('misc', 'flx_parse.grammar'),
      '>',
      os.path.join('lib', 'flx_token.flx'),
    )
