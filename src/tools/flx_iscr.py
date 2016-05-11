#!/usr/bin/env python3
# Core interscript interpreter
import sys, os, re, io, operator, shutil, string
from glob import glob

def open_utf8(*args): return open(*args, encoding='utf-8')

# The regexes that the processor uses.
class regexes:
    felt = '[A-Za-z._${}][-A-Za-z0-9_.${}]*'
    fname = '(?:%s/)*%s' % (felt, felt)

    # A tangler definition looks like:
    # @tangler <name> = <filename>
    tangler_def = re.compile('@tangler\s*(%s)\s*=\s*(%s)' % (felt, fname))
    # To set the output we just use:
    # @tangle <name>
    tangler_use = re.compile('@tangle\s*(%s)' % felt)

class State: pass
class Doc(State): pass
class Tangling(State): pass

# Now define a type to hold a tangler file buffer.
class Tangler(io.StringIO):
    def __init__(self, filename, id, quiet, suppress_linenos):
        self.basename,self.extension = os.path.splitext(filename)
        self.filename = filename
        self.id = id
        self.quiet = quiet
        self.emit_linenos = self.extension in (
            '.flx','.c','.h','.cpp','.cxx','.hpp',
            '.fsyn','.fpc','.py'
            ) and not suppress_linenos
        super(Tangler, self).__init__()
    # Now for the save routine. We only write the buffer to
    # the file if the contents differ from the old file.
    # This is to preserve the time stamp if things don't change.
    # Note if a file is non-existent, and the tangler input
    # is empty, the file will not be created.
    def save(self):
        update = False
        self.seek(0)
        try:
            with open_utf8(self.filename) as old:
                contents = old.read()
                update = self.read() != contents
        except:
            update = True
        if update:
            if not self.quiet:
                print('Write     %s -> %s' % (self.id, self.filename))

            try:
                os.makedirs(os.path.dirname(self.filename), exist_ok=True)
            except OSError as ex:
                pass # okay for dir to already exist

            try:
                f = open_utf8(self.filename, 'w')
            except IOError as ex:
                sys.exit("Can't open output file %s: %s" % (self.filename, ex))
            with f:
                self.seek(0)
                shutil.copyfileobj(self, f)
        else:
            if not self.quiet:
                print('Unchanged %s -> %s' % (self.id, self.filename))

# The processor class.
class Processor:
    def __init__(self, iname, odir, quiet):
        self.iname = iname
        self.state = Doc
        self.odir = odir
        self.quiet = quiet
        self.tangler = None
        # We use a dictionary variable to hold all the tanglers.
        self.tanglers = {}
        self.lineno = 0
        self.suppress_linenos = False
    def parse_error(self, msg):
        sys.exit('error at line %d: %s' % (self.lineno, msg))
    # Now define the parser actions.
    def def_tangler(self, id, filename):
        'Add a new tangler.'
        join = True
        if filename[0] == '$':
            join = False
        filename = string.Template(filename).safe_substitute(os.environ)
        if os.path.isabs(filename):
            join = False
        if join:
            filename = os.path.join(self.odir, filename)
        if id in self.tanglers:
            sys.exit('Duplicate definition of tangler %s' % id)
        self.tanglers[id] = Tangler(filename, id, self.quiet, suppress_linenos=self.suppress_linenos)

    def cquote (self,f): return '"' + f.replace('\\', '/') + '"'

    def set_tangler(self, id,lineno):
        'Specify a new current tangler.'
        try:
            tangler = self.tanglers[id]
            if tangler.emit_linenos:
              if tangler.extension in (
                '.flx','.c','.h','.cpp','.cxx','.hpp',
                '.fsyn','.py'
              ):
                hashline = "#line " + str(lineno+1) + ' ' + self.cquote (self.iname)
                print(hashline,file=tangler)
              elif tangler.extension in (
                '.fpc'
              ):
                hashline = "Generated_from: " + str(lineno+1) + ' "' + self.iname+'"'
                print(hashline,file=tangler)
                
        except KeyError:
            sys.exit("Can't find tangler %s" % id)
        else:
            self.state = Tangling
            self.tangler = tangler
    def process(self, f):
        '''
        Here's the main processing routine for the input file.
        We check for an @ character at the start of a line.
        If we don't find one we either write the line to the
        current tangler or just skip over it. If we do we have
        to see what command it is: either a command to define
        a new tangler, a command to switch to a different output
        file, or a switch to document mode in which we just skip
        over the lines.
        '''
        if not self.quiet:
            print('PACKAGE   ' + f.name)
        for i, line in enumerate(f, start=1):
            self.lineno = i
            line = line.rstrip()
            if line and line[0] == '@':
                if line.startswith('@tangler'):
                    m = regexes.tangler_def.match(line)
                    if not m:
                        self.parse_error('invalid tangler definition')
                    self.def_tangler(*m.groups())
                elif line.startswith('@tangle'):
                    m = regexes.tangler_use.match(line)
                    if not m:
                        self.parse_error('invalid tangler usage')
                    self.set_tangler(m.group(1),self.lineno)
                elif line.startswith ('@@'):
                  if self.state is Tangling:
                      assert self.tangler
                      print(line[1:], file=self.tangler)
                else:
                    self.state = Doc
            else:
                if self.state is Tangling:
                    assert self.tangler
                    print(line, file=self.tangler)
    def save(self):
        for tangler in self.tanglers.values():
            tangler.save()
            tangler.close()

def process_dir(package_dir, odir, quiet):
    # iterate over packages
    for i in os.listdir(package_dir):
        i = os.path.join(package_dir, i)
        if i[-5:] == ".fdoc":
          # print debugging
          print('PACKAGE', i)

          odir = os.path.abspath(odir)
          iname = os.path.abspath(i)
          p = Processor(iname, odir, quiet)
          # Process the input file and buffer up the code.
          try:
              f = open_utf8(iname)
          except IOError as ex:
              sys.exit(str(ex))
          with f:
              p.process(f)
          p.save()


def iscr():
    # Parse the arguments.
    quiet = False
    process_many = False

    if '-h' in sys.argv:
        print('usage: %s [-q -d] <interscript file|dir> <output directory>' % sys.argv[0])
        sys.exit()

    if '-d' in sys.argv:
        process_many = True

    if '-q' in sys.argv:
        quiet = True
        sys.argv.remove('-q')

    # process multiple input files
    if process_many:
        try:
            _, _, idir, odir = sys.argv+([''] if len(sys.argv) == 3 else [])
        except ValueError:
            sys.exit('invalid number of arguments; use %s -h for help' % sys.argv[0])

        # do work and return
        process_dir(idir, odir, quiet)
        return

    # just process one input file
    try:
        _, iname, odir = sys.argv+([''] if len(sys.argv) == 2 else [])
    except ValueError:
        sys.exit('invalid number of arguments; use %s -h for help' % sys.argv[0])

    # If odir == '', abspath returns the current directory.
    odir = os.path.abspath(odir)
    iname = os.path.abspath(iname)
    p = Processor(iname,odir, quiet)
    # Process the input file and buffer up the code.
    try:
        f = open_utf8(iname)
    except IOError as ex:
        sys.exit(str(ex))
    # Finally just dump the buffers to the associated
    # files if the contents of the buffer and file differ.
    # Do nothing if the contents are the same to avoid
    # spoiling the last modification timestamp.
    with f:
        p.process(f)
    p.save()

if __name__ == '__main__':
    iscr()
