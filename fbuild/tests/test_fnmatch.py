#!/usr/bin/env python3

"""Test cases for the fnmatch module."""

from test import support
import unittest

from fbuild.fnmatch import fnmatch, fnmatchcase


class FnmatchTestCase(unittest.TestCase):
    def check_match(self, filename, pattern, should_match=1):
        if should_match:
            self.assert_(fnmatch(filename, pattern),
                         "expected %r to match pattern %r"
                         % (filename, pattern))
        else:
            self.assert_(not fnmatch(filename, pattern),
                         "expected %r not to match pattern %r"
                         % (filename, pattern))

    def test_fnmatch(self):
        check = self.check_match
        check('abc', 'abc')
        check('abc', '?*?')
        check('abc', '???*')
        check('abc', '*???')
        check('abc', '???')
        check('abc', '*')
        check('abc', 'ab[cd]')
        check('abc', 'ab[!de]')
        check('abc', 'ab[de]', 0)
        check('a', '??', 0)
        check('a', 'b', 0)

        # these test that '\' is handled correctly in character sets;
        # see SF bug #???
        check('\\', r'[\]')
        check('a', r'[!\]')
        check('\\', r'[!\]', 0)

        check('abcdefghi', 'ab{cd,12*}ef{gh?,34}')
        check('ab1234ef34', 'ab{cd,12*}ef{gh?,34}')

        check('abcdefgh', 'ab{cd,12*}ef{gh?,34}', 0)
        check('ab1234ef345', 'ab{cd,12*}ef{gh?,34}', 0)

    def test_mix_bytes_str(self):
        self.assertRaises(TypeError, fnmatch, 'test', b'*')
        self.assertRaises(TypeError, fnmatch, b'test', '*')
        self.assertRaises(TypeError, fnmatchcase, 'test', b'*')
        self.assertRaises(TypeError, fnmatchcase, b'test', '*')

    def test_bytes(self):
        self.check_match(b'test', b'te*')
        self.check_match(b'test\xff', b'te*\xff')

def suite(*args, **kwargs):
    return unittest.TestLoader().loadTestsFromTestCase(FnmatchTestCase)

if __name__ == "__main__":
    unittest.main()
