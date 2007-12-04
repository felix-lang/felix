import os
import glob

from fbuild.flxbuild.process import Process

class man(Process):
  help = 'make the man pages'

  def runme(self, *args):
    print "GENERATING MAN PAGES"
    mp = glob.glob('man/man1/*.1')
    MAN_PAGES = []
    for i in mp:
      MAN_PAGES.append (i[9:])
    try: os.mkdir("doc/htmlman")
    except: pass
    dtd = ('<!DOCTYPE HTML PUBLIC \\"-//W3C//DTD HTML 4.0 Transitional//EN\\"\\n'+
      '  \\"http://www.w3.org/TR/REC-html40/loose.dtd\\">')
    try:
      for file in MAN_PAGES:
        basename = os.path.splitext(file)[0]
        self.shell(
          "man2html man/man1/" + file +
          '| sed -e "s%<A HREF=\\"[^<]*cgi-bin/man/man2html?1+\(.*\)\\">%<A HREF=\\"\\1_1.html\\">%"' +
          '| sed -e "7d"' +
          '| sed -e "1,3d"' +
          '| sed -e "s%<html>%'+dtd+'\\n<html>%"' +
          ' >' + "doc/htmlman/" + basename+'_1.html',
        )
    except:pass
