#!/usr/bin/env python3
import os
import optparse
import subprocess
import sys

parser = optparse.OptionParser()
parser.add_option('--clean',
    action='store_true',
    help='clean build files first')
parser.add_option('-j', '--jobs',
    metavar='N',
    type='int',
    default=1,
    help='Allow N jobs at once')
parser.add_option('--database-engine',
    default='pickle',
    help='specify the database engine')

options, args = parser.parse_args()

examples_dir = os.path.dirname(__file__) or os.getcwd()
for d in os.listdir(examples_dir):
    d = os.path.join(examples_dir, d)

    if not os.path.isdir(d):
        continue

    if options.clean:
        print('cleaning:', d)
        subprocess.call('%s %s --clean' %
            (sys.executable, os.path.join('..', '..', 'fbuild-light')),
            cwd=d, shell=True)

    print('running example:', d)
    print()
    rcode = subprocess.call('%s %s --database-engine=%s -j %i' % (
            sys.executable,
            os.path.join('..', '..', 'fbuild-light'),
            options.database_engine,
            options.jobs),
        cwd=d, shell=True)
    print()
    print('-' * 50)
    print()

    if rcode != 0:
        break

