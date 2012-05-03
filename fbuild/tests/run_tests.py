#!/usr/bin/env python3.1

import os
import sys
import doctest
import unittest

sys.path.append(os.path.join(os.path.dirname(sys.argv[0]), '..', 'lib'))

import test_fnmatch
import test_functools
import test_glob
import test_scheduler

# -----------------------------------------------------------------------------

def main():
    suite = unittest.TestSuite()

    # Load the doctests
    prefix = os.path.join('..', 'lib')
    for root, dirs, files in os.walk(prefix):
        root = root[len(prefix + os.sep):].replace(os.sep, '.')

        for file in files:
            if file == '__init__.py':
                module = root
            elif file.endswith('.py'):
                module = root + '.' + file[:-len('.py')]
            else:
                continue

            try:
                test = doctest.DocTestSuite(__import__(module, {}, {}, ['']))
            except ValueError as e:
                # no doc test exists
                pass
            except ImportError:
                # We can't portably
                if module == 'fbuild.subprocess.winprocess':
                    continue
            else:
                suite.addTest(test)

    suite.addTest(test_fnmatch.suite())
    suite.addTest(test_functools.suite())
    suite.addTest(test_glob.suite())
    suite.addTest(test_scheduler.suite())

    runner = unittest.TextTestRunner(verbosity=2)
    runner.run(suite)

    return 0

# -----------------------------------------------------------------------------

if __name__ == '__main__':
    sys.exit(main())
