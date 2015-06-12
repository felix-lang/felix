#!/usr/bin/env python3

from os.path import join
import sys, fnmatch, os

# Because *no one* though of a recursive glob before 3.5...
def rglob(dir, pat):
    for root, dirs, files in os.walk(dir):
        for fn in fnmatch.filter(files, pat):
            yield join(root, fn)

def main():
    try:
        dir = join(sys.argv[1], 'share', 'lib')
    except IndexError:
        dir = join('src', 'lib')
    print('[flx_find_grammar_files] ** Scanning', dir)

    gfiles = list(rglob(dir, '*.fsyn'))

    with open(join(dir, 'grammar', 'grammar.files')) as f:
        oldfiles = map(str.rstrip, list(filter(None, f)))
    print('Same=%s' % set(gfiles) == set(oldfiles))
    extras = list(filter(lambda f: f not in oldfiles, gfiles))

    extrafilename = join(dir, 'grammar', 'extra.files')
    with open(extrafilename) as f:
        oldextras = map(str.rstrip, list(filter(None, f)))
    if set(extras) != set(oldextras):
        extrastr = '\n'.join(extras)
        print('[flx_find_grammar_files] ** Writing extra grammar files to', extrafilename)
        print(extrastr)
        with open(extrafilename, 'w') as f:
            f.write(extrastr)
    else:
        print('[flx_find_grammar_files] ** Unchanged')

if __name__ == '__main__':
    main()
