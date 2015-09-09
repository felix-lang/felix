#!/usr/bin/env python3

import unittest

from fbuild.functools import *

# -----------------------------------------------------------------------------

class TestFunctionBind(unittest.TestCase):
    def testUnary(self):
        def f():
            pass

        self.assertEquals(normalize_args(f, (), {}), ((), {}))

        self.assertRaises(TypeError, normalize_args, f, (1,), {})
        self.assertRaises(TypeError, normalize_args, f, (),   {'x':1})
        self.assertRaises(TypeError, normalize_args, f, (1,), {'x':1})

    def testSingle(self):
        def f(a):
            pass

        self.assertEquals(normalize_args(f, (1,), {}),     ((1,), {}))
        self.assertEquals(normalize_args(f, (2,), {}),     ((2,), {}))
        self.assertEquals(normalize_args(f, (), {'a': 3}), ((3,), {}))

        self.assertRaises(TypeError, normalize_args, f, (),     {})
        self.assertRaises(TypeError, normalize_args, f, (1, 2), {})
        self.assertRaises(TypeError, normalize_args, f, (),     {'x':1})
        self.assertRaises(TypeError, normalize_args, f, (1,),   {'x':1})


    def testMultiple(self):
        def f(a, b, c):
            pass

        self.assertEquals(normalize_args(f, (1, 2, 3), {}), ((1, 2, 3), {}))
        self.assertEquals(normalize_args(f, (2, 3, 4), {}), ((2, 3, 4), {}))

        self.assertEquals(
            normalize_args(f, (), {'a': 1, 'b': 2, 'c': 3}),
            ((1, 2, 3), {}))

        self.assertEquals(
            normalize_args(f, (1,), {'b': 2, 'c': 3}),
            ((1, 2, 3), {}))

        self.assertEquals(
            normalize_args(f, (1, 2), {'c': 3}),
            ((1, 2, 3), {}))

        self.assertRaises(TypeError, normalize_args, f, (),           {})
        self.assertRaises(TypeError, normalize_args, f, (1, 2),       {})
        self.assertRaises(TypeError, normalize_args, f, (1, 2, 3, 4), {})
        self.assertRaises(TypeError, normalize_args, f, (),           {'x':1})
        self.assertRaises(TypeError, normalize_args, f, (1,),         {'x':1})

    def testDefault(self):
        def f(a, b, c=8, d=9):
            pass

        self.assertEquals(
            normalize_args(f, (1, 2), {}),
            ((1, 2, 8, 9), {}))

        self.assertEquals(
            normalize_args(f, (1, 2, 4), {}),
            ((1, 2, 4, 9), {}))

        self.assertEquals(
            normalize_args(f, (1, 2, 3, 4), {}),
            ((1, 2, 3, 4), {}))

        self.assertEquals(
            normalize_args(f, (), {'a': 1, 'b': 2, 'c': 3, 'd': 4}),
            ((1, 2, 3, 4), {}))

        self.assertEquals(
            normalize_args(f, (1,), {'b': 2, 'c': 3, 'd': 4}),
            ((1, 2, 3, 4), {}))

        self.assertEquals(
            normalize_args(f, (1, 2), {'c': 3, 'd': 4}),
            ((1, 2, 3, 4), {}))

        self.assertEquals(
            normalize_args(f, (1, 2, 3), {'d': 4}),
            ((1, 2, 3, 4), {}))

        self.assertRaises(TypeError, normalize_args, f, (),              {})
        self.assertRaises(TypeError, normalize_args, f, (1,),            {})
        self.assertRaises(TypeError, normalize_args, f, (1, 2, 3, 4, 5), {})
        self.assertRaises(TypeError, normalize_args, f, (),              {'x':1})
        self.assertRaises(TypeError, normalize_args, f, (1,),            {'x':1})

    def testVarArgs0(self):
        def f(*args):
            pass

        self.assertEquals(normalize_args(f, (), {}),        ((), {}))
        self.assertEquals(normalize_args(f, (1, 2, 4), {}), ((1, 2, 4), {}))

        self.assertRaises(TypeError, normalize_args, f, (),   {'x':1})
        self.assertRaises(TypeError, normalize_args, f, (1,), {'x':1})

    def testVarArgs1(self):
        def f(a, *args):
            pass

        self.assertEquals(normalize_args(f, (1,), {}),      ((1,), {}))
        self.assertEquals(normalize_args(f, (), {'a': 1}),  ((1,), {}))
        self.assertEquals(normalize_args(f, (1, 2, 4), {}), ((1,2, 4), {}))

        self.assertRaises(TypeError, normalize_args, f, (),   {})
        self.assertRaises(TypeError, normalize_args, f, (2, 4), {'a': 1})
        self.assertRaises(TypeError, normalize_args, f, (),   {'x':1})
        self.assertRaises(TypeError, normalize_args, f, (1,), {'x':1})

    def testVarArgs2(self):
        def f(a, b, c=8, d=9, *args):
            pass

        self.assertEquals(
            normalize_args(f, (1, 2), {}),
            ((1, 2, 8, 9), {}))
        self.assertEquals(
            normalize_args(f, (1, 2, 4), {}),
            ((1, 2, 4, 9), {}))

        self.assertEquals(
            normalize_args(f, (1, 2, 3, 4), {}),
            ((1, 2, 3, 4), {}))

        self.assertEquals(
            normalize_args(f, (1, 2, 3, 4, 5), {}),
            ((1, 2, 3, 4, 5), {}))

        self.assertEquals(
            normalize_args(f, (), {'a': 1, 'b': 2, 'c': 3, 'd': 4}),
            ((1, 2, 3, 4), {}))

        self.assertEquals(
            normalize_args(f, (1,), {'b': 2, 'c': 3, 'd': 4}),
            ((1, 2, 3, 4), {}))

        self.assertEquals(
            normalize_args(f, (1, 2), {'c': 3, 'd': 4}),
            ((1, 2, 3, 4), {}))

        self.assertEquals(
            normalize_args(f, (1, 2, 3), {'d': 4}),
            ((1, 2, 3, 4), {}))

        self.assertRaises(TypeError, normalize_args, f, (),   {})
        self.assertRaises(TypeError, normalize_args, f, (1,), {})
        self.assertRaises(TypeError, normalize_args, f, (),   {'x':1})
        self.assertRaises(TypeError, normalize_args, f, (1,), {'x':1})
        self.assertRaises(TypeError, normalize_args, f,
            (1, 2, 3, 4, 5), {'a': 1, 'b': 2, 'c': 3, 'd': 4})

    def testKeywordOnly(self):
        def f(*, a, b):
            pass

        self.assertEquals(
            normalize_args(f, (), {'a': 1, 'b': 2}),
            ((), {'a': 1, 'b': 2}))

        self.assertRaises(TypeError, normalize_args, f, (),   {})
        self.assertRaises(TypeError, normalize_args, f, (1,), {})
        self.assertRaises(TypeError, normalize_args, f, (),   {'x':1})
        self.assertRaises(TypeError, normalize_args, f, (1,), {'x':1})

    def testKeywordDefaults(self):
        def f(*, a, b=8, c=9):
            pass

        self.assertEquals(
            normalize_args(f, (), {'a': 1}),
            ((), {'a': 1, 'b': 8, 'c': 9}))

        self.assertEquals(
            normalize_args(f, (), {'a': 1, 'b': 2, 'c': 3}),
            ((), {'a': 1, 'b': 2, 'c': 3}))

        self.assertEquals(
            normalize_args(f, (), {'a': 1, 'b': 2}),
            ((), {'a': 1, 'b': 2, 'c': 9}))

        self.assertRaises(TypeError, normalize_args, f, (1,), {})
        self.assertRaises(TypeError, normalize_args, f, (),   {'x':1})
        self.assertRaises(TypeError, normalize_args, f, (1,), {'x':1})

    def testVarKeyword0(self):
        def f(**kwargs):
            pass

        self.assertEquals(normalize_args(f, (), {}),       ((), {}))
        self.assertEquals(normalize_args(f, (), {'a': 1}), ((), {'a':1}))

        self.assertEquals(
            normalize_args(f, (), {'a': 1, 'b': 2, 'c': 3}),
            ((), {'a': 1, 'b': 2, 'c': 3}))

        self.assertRaises(TypeError, normalize_args, f, (1,), {})
        self.assertRaises(TypeError, normalize_args, f, (1,), {'x':1})

    def testAll(self):
        def f(a, b, c=7, d=8, *args, e, f, g=9, h=0, **kwargs):
            pass

        self.assertEquals(
            normalize_args(f, (1, 2), {'e': 3, 'f': 4}),
            ((1, 2, 7, 8), {'e': 3, 'f': 4, 'g': 9, 'h': 0}))

        self.assertEquals(
            normalize_args(f, (1, 2, 3, 4, 5), dict(e=3, f=4, g=5, h=6, i=7)),
            ((1, 2, 3, 4, 5), {'e': 3, 'f': 4, 'g': 5, 'h': 6, 'i': 7}))

# -----------------------------------------------------------------------------

def suite(*args, **kwargs):
    return unittest.TestLoader().loadTestsFromTestCase(TestFunctionBind)

if __name__ == "__main__":
    unittest.main()
