#!/usr/bin/env python
import os
import sys

def run(*args):
    cmd = ' '.join(args)
    print 'RUNNING', cmd
    return os.system('%s %s' % (sys.executable, cmd))

def main():
    print "DEFAULT FELIX BUILD"
    configure = os.path.join(os.path.dirname(sys.argv[0]), 'configure')

    if run(configure, *sys.argv[1:]):
        return 1

    if run('"%s"' % os.path.join(os.getcwd(), 'mk'), 'extract'):
        return 1

    if run('"%s"' % os.path.join(os.getcwd(), 'mk')):
        return 1

if __name__ == '__main__':
    sys.exit(main())
