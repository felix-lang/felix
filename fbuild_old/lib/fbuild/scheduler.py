import sys
import threading

# we need the Queue from python 2.5 or greater
if sys.version_info >= (2, 5):
    import Queue
else:
    import fbuild.compat._Queue as Queue

__all__ = ['Scheduler', 'Future', 'Full', 'Empty']

Full = Queue.Full
Empty = Queue.Empty

class Scheduler(Queue.Queue):
    def __init__(self, maxsize):
        Queue.Queue.__init__(self)

        # we subtract one thread because we'll use the main one as well
        for i in range(maxsize - 1):
            thread = threading.Thread(target=self._run)
            thread.setDaemon(True)
            thread.start()

    def _run(self):
        while True:
            self._run_one()

    def _run_one(self, *args, **kwargs):
        func = self.get(*args, **kwargs)
        try:
            func()
        finally:
            self.task_done()


    def future(self, function, *args, **kwargs):
        f = Future(self, function, args, kwargs)
        self.put(f.start)
        return f


class Future(object):
    def __init__(self, pool, function, args, kwargs):
        self.__pool = pool
        self.__function = function
        self.__args = args
        self.__kwargs = kwargs
        self.__finished = False
        self.__result = None
        self.__exc = None

    def __call__(self):
        while not self.__finished:
            # we're going to block anyway, so just run another
            # future
            try:
                self.__pool._run_one(timeout=1.0)
            except Empty:
                pass

        if self.__exc:
            raise self.__exc[0], self.__exc[1], self.__exc[2]

        return self.__result

    def __repr__(self):
        return '<%s: %s, %s, %s>' % (
            self.__class__.__name__,
            self.__function.__name__,
            self.__args,
            self.__kwargs,
        )

    def start(self):
        try:
            self.__result = self.__function(*self.__args, **self.__kwargs)
        except:
            self.__exc = sys.exc_info()

        self.__finished = True
