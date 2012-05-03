#!/usr/bin/env python3.1

import time
import random
import unittest
import gc

from fbuild.console import Log
from fbuild.sched import Scheduler

import threading

# -----------------------------------------------------------------------------

class TestScheduler(unittest.TestCase):
    def setUp(self):
        # Make sure any latent contexts are cleaned up before we run.
        gc.collect()

        self.initial_thread_count = threading.active_count()

        self.scheduler = Scheduler(self.threads)

        if self.threads == 0:
            self.assertEquals(self.scheduler.threadcount, 1)
        else:
            self.assertEquals(self.scheduler.threadcount, self.threads)

    def tearDown(self):
        # make sure we turn off all tht threads when shutting down
        self.scheduler.shutdown()

        self.assertEquals(self.scheduler.threadcount, 0)
        self.assertEquals(threading.active_count(), self.initial_thread_count)

    def testMap(self):
        def f(x):
            time.sleep(random.random() * 0.01)
            return x + 1

        self.assertEquals(
            self.scheduler.map(f, [0,1,2,3,4,5,6,7,8,9]),
            [1,2,3,4,5,6,7,8,9,10])

        # now test if we can handle recursive scheduling
        def g(x):
            time.sleep(random.random() * 0.01)
            return self.scheduler.map(f, x)

        self.assertEquals(
            self.scheduler.map(g, [[0,1,2],[3,4,5],[6,7,8]]),
            [[1,2,3],[4,5,6],[7,8,9]])

    def run(self, *args, **kwargs):
        for i in range(10):
            self.threads = i
            super(TestScheduler, self).run(*args, **kwargs)

# -----------------------------------------------------------------------------

def suite():
    return unittest.TestLoader().loadTestsFromTestCase(TestScheduler)

if __name__ == "__main__":
    unittest.main()
