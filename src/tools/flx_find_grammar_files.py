#!/usr/bin/env python3

from os.path import join
import sys, fnmatch, os

def tounix(s):
    return s.replace('\\', '/')

initfsyn =["grammar/utility.fsyn", "grammar/grammar_scheme_support.fsyn", "grammar/blocks.fsyn"]
termfsyn = ["grammar/felix.fsyn", "grammar/save.fsyn"]
allfsyn = initfsyn + termfsyn

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

    print('[flx_find_grammar_files] ** Scanning', dir)

    gfiles = list(rrglob(dir, '*.fsyn'))
    with open(stdfilename, 'w') as f:
      for file in initfsyn : f.write(file+"\n")
      for file in gfiles: 
        if file not in allfsyn: f.write(file+"\n") 
      for file in termfsyn : f.write(file+"\n")

if __name__ == '__main__':
    main()
