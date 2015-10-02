#!/usr/bin/env python3

from os.path import join
import sys, fnmatch, os

def tounix(s):
    return s.replace('\\', '/')

# Because *no one* though of a recursive glob before 3.5...
def rglob(dir, pat):
    for root, dirs, files in os.walk(dir):
        for fn in fnmatch.filter(files, pat):
            yield join(root, fn)

def rrglob(dir,pat):
    n = len(dir)
    for file in rglob (dir,pat):
       yield tounix(file[n+1:])
 
def main():
    # just capture the command line param and hand it to
    # library function
    run(sys.argv[1])

def run(dir):
    try:
        dir = join(dir, 'share', 'lib')
    except IndexError:
        dir = join('src', 'lib')

    stdfilename = join (dir,'grammar','grammar.files')
    extrafilename = join(dir, 'grammar', 'extra.files')

    print('[flx_find_grammar_files] ** Scanning', dir)

    gfiles = list(rrglob(dir, '*.fsyn'))
    #print("Files = "+ str(gfiles))

    with open(stdfilename) as f:
      tmp = f.readlines()
    stdfiles = []
    for file in tmp: stdfiles.append(file.rstrip())
    #print("STD FILES=" + str(stdfiles))

    try:
      with open(extrafilename) as f:
        tmp = f.readlines()
    except:
      tmp = []
    oldextrafiles = []
    for file in tmp: oldextrafiles.append(file.rstrip())
    #print("OLD Extra files = " + str(oldextrafiles))

    newextrafiles = list(filter(lambda f: f not in stdfiles, gfiles))

    #print("New Extras = " + str(newextrafiles))
    if set(newextrafiles) != set(oldextrafiles):
        print('[flx_find_grammar_files] ** Writing extra grammar files to', extrafilename)
        with open(extrafilename, 'w') as f:
            for file in newextrafiles: f.write(file+"\n")
    else:
        print('[flx_find_grammar_files] ** Unchanged')

if __name__ == '__main__':
    main()
