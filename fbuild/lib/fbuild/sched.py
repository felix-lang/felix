import collections
import io
import operator
import queue
import sys
import threading
import time
import _thread

import fbuild

# ------------------------------------------------------------------------------

class DependencyLoop(fbuild.Error):
    """
    Raised when the scheduler discovers a dependency recursively depends upon
    itself.
    """

    def __init__(self, srcs):
        """
        Create a DependencyLoop.

        srcs: a list of files that recursively depends themselves.
        """

        self.srcs = srcs

    def __str__(self):
        s = io.StringIO()
        for srcs in self.srcs:
            srcs = sorted(srcs)
            if len(srcs) == 0:
                print('dependency loop', file=s)
            elif len(srcs) == 1:
                print('%s depends on itself' % srcs[0], file=s)
            else:
                print('%s and %s depend on each other' % (
                    ', '.join(srcs[0:-1]),
                    srcs[-1]
                ), file=s)

        return s.getvalue().strip()

# ------------------------------------------------------------------------------

class Scheduler:
    """
    A Scheduler asynchronously runs functions inside a thread pool. It has a
    simple interface. Here's a simple scheduler that has two underlying worker
    threads:

    >>> scheduler = Scheduler(2)
    >>> def f(x): return x * x
    >>> scheduler.map(f, [1, 2, 3])
    [1, 4, 9]

    Schedulers also support functions with dependencies. The scheduler will
    make sure that the dependencies will be processed before their dependents:

    >>> def deps(x):
    ...    return {'a': ['b', 'c'], 'b': ['c'], 'c': []}[x]
    >>> def f(x):
    ...    return x
    >>> scheduler.map_with_dependencies(deps, f, ['a', 'b', 'c'])
    ['c', 'b', 'a']

    """

    def __init__(self, threadcount=0, *, logger=None):
        # We need at least 1 thread.
        threadcount = max(1, threadcount)

        # Our threads.
        self.__threads = []

        # Our work queue of ready tasks that is shared with all the worker
        # threads. We use a lifo queue as we want to do work in the order it
        # comes in since it's less likely to have dependencies on later
        # functions.
        self.__ready_queue = queue.LifoQueue()

        # All the worker threads need to share a logger object to make sure we
        # don't have races when we're logging to the console. So we need to
        # make one if we weren't given one.
        if logger is None:
            import fbuild.console
            logger = fbuild.console.Log()

        # Spin up our threads!
        for i in range(threadcount):
            thread = WorkerThread(logger, self.__ready_queue)
            self.__threads.append(thread)
            thread.start()

    @property
    def threadcount(self):
        return len(self.__threads)

    def map(self, function, srcs):
        """Run the function over the input sources concurrently. This function
        returns the results in their initial order."""
        tasks = [Task(function, src, index) for index, src in enumerate(srcs)]
        tasks = sorted(self._evaluate(tasks), key=operator.attrgetter('index'))

        return [n.result for n in tasks]

    def map_with_dependencies(self, depends, function, srcs):
        """Calculate the dependencies between the input sources and run them
        concurrently. This function returns the results in the order that they
        finished, not their initial order."""

        # First create tasks for all the input sources and create an index from
        # src to task. We'll use this as a lookup when we invert the dependency
        # information.
        tasks = {}
        for src in srcs:
            tasks[src] = Task(function, src)

        # Evaluate the dependencies for each source.
        for dep_task in self._evaluate([Task(depends, src) for src in srcs]):
            try:
                task = tasks[dep_task.src]
            except KeyError:
                # ignore missing dependencies
                pass
            else:
                for dep in dep_task.result:
                    try:
                        task.dependencies.append(tasks[dep])
                    except KeyError:
                        # ignore missing dependencies
                        pass

        # Evaluate the functions.
        self._evaluate(list(tasks.values()))

        # Sort the functions in a depth first order. Otherwise, the order of
        # the function evaluation could change between calls, which could break
        # caching these results.
        visited = set()
        results = []

        def f(task):
            if task in visited:
                return
            visited.add(task)

            for n in task.dependencies:
                f(n)
            results.append(task.result)

        for src in srcs:
            f(tasks[src])

        return results

    def _evaluate(self, tasks):
        """Evaluate the function over these tasks and return the results."""

        # Keep a counter for the number of active tasks. When this reaches 0 we
        # know we can exit.
        count = 0

        # A lookup table of dependency to dependents.
        children = collections.defaultdict(list)

        # The queue from which we will receive function results.
        done_queue = queue.Queue()

        # Add each task to our work set and map dependencies to dependents.
        for task in tasks:
            for dep in task.dependencies:
                children[dep].append(task)

            if task.can_run():
                count += 1
                task.running = True
                self.__ready_queue.put((done_queue, task))

        # A naive threadpool scheduler can deadlock if a function the scheduler
        # is mapping also makes calls to the scheduler. The traditional way of
        # writing a scheduler has the client of the scheduler block until the
        # scheduler finishes running. This works fine until the function that
        # is scheduled also makes a call into the scheduler. This will result
        # in a deadlock because all the scheduler threads will be blocked
        # waiting for work with no one left to actually do anything.
        #
        # The way we avoid this problem is that we detect if the current thread is
        # one of our worker threads, and if so, we know we're are being used
        # recursively. When this happens, we know we can reuse this thread to
        # run another queued up function.
        current_thread = threading.current_thread()

        # The list of function results.
        results = []

        # Run until all of our tasks finished.
        while count != 0:
            if isinstance(current_thread, WorkerThread):
                # We're inside an already running thread, so we're going to run
                # until all of our tasks are done.
                try:
                    current_thread.run_one(block=False)
                except queue.Empty:
                    pass

                # See if any of our tasks finished.
                try:
                    task = done_queue.get(block=False)
                except queue.Empty:
                    # No tasks done, so loop.
                    continue
            else:
                task = done_queue.get()

            # We finished a task!
            count -= 1
            task.done = True

            # If a task raised an exception, cancel the rest of the tasks and
            # error out.
            if task.exc is not None:
                # Clear our queue of tasks.
                for t in tasks:
                    t.done = True

                raise task.exc

            # Otherwise, add it to our results.
            results.append(task)

            # If we have any dependent childs, see if they can run now. If so,
            # add them to our work queue.
            for child in children[task]:
                if child.can_run():
                    count += 1
                    child.running = True
                    self.__ready_queue.put((done_queue, child))

        # Check if we ran all of the tasks.
        if len(results) != len(tasks):
            # Uh oh, we must have a mutually dependent task. Figure out all the
            # dependencies and error out.
            recursive_srcs = set()

            for task in tasks:
                if task.done:
                    continue

                for dep in children[task]:
                    if task in dep.dependencies and dep in task.dependencies:
                        recursive_srcs.add(frozenset((task.src, dep.src)))

            raise DependencyLoop(recursive_srcs)

        return results

    def __del__(self):
        # Make sure we shutdown all our threads before we quit.
        self.shutdown()

    def shutdown(self):
        """Tell the worker threads to shut down."""

        # make sure we wake the threads before we kill them.
        for thread in self.__threads:
            self.__ready_queue.put(None)

        for thread in self.__threads:
            thread.shutdown()
            thread.join()

        # Reset our thread list.
        self.__threads = []

# ------------------------------------------------------------------------------

class WorkerThread(threading.Thread):
    """
    The scheduler's worker thread. This loops forever until there is no work
    left.
    """

    def __init__(self, logger, ready_queue):
        super().__init__()
        self.daemon = True

        self.__logger = logger
        self.__ready_queue = ready_queue
        self.__finished = False

    def shutdown(self):
        """Tell the thread to exit."""
        self.__finished = True

    def run(self):
        try:
            while not self.__finished:
                with self.__logger.log_from_thread():
                    if not self.run_one():
                        break
        except KeyboardInterrupt:
            # let the main thread know we got a SIGINT
            _thread.interrupt_main()
            raise

    def run_one(self, *args, **kwargs):
        """
        Try to run one task. Returns True if we actually ran a function,
        otherwise return False.
        """

        queue_task = self.__ready_queue.get(*args, **kwargs)

        try:
            # This should be tested in the try block so that we update the done
            # counter in the ready queue, even if we errored out.
            if queue_task is None:
                return False

            done_queue, task = queue_task
            try:
                task.run()
            finally:
                done_queue.put(task)
        finally:
            self.__ready_queue.task_done()

        return True

# ------------------------------------------------------------------------------

class Task:
    """
    Represent the state needed to run the function with one source.
    """

    def __init__(self, function, src, index=None):
        self.function = function
        self.src = src
        self.index = index
        self.running = False
        self.done = False
        self.dependencies = []
        self.exc =None

    def can_run(self):
        """Returns True if all of this task's dependencies are done. Otherwise
        return False."""

        if self.running or self.done:
            return False

        if not self.dependencies:
            return True

        return all(d.done for d in self.dependencies)

    def run(self):
        """Run the task's function."""

        try:
            self.result = self.function(self.src)
        except Exception as e:
            self.exc = e
