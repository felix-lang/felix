import queue
import sys
import threading

# ------------------------------------------------------------------------------

class RPCNotRunning(Exception):
    pass

# ------------------------------------------------------------------------------

# A unique object to distinquish  mark the ends of our streams.
class _NullType:
    pass

_NULL = _NullType()

class _Result:
    """A helper object that will hold the results of a call."""

    def __init__(self):
        self.result = _NULL

# ------------------------------------------------------------------------------

class RPC(threading.Thread):
    """A simple threaded procedure call server and client."""

    def __init__(self, handler):
        super().__init__()

        self._handler = handler
        self._lock = threading.Lock()
        self._queue = queue.Queue()
        self._events = threading.local()
        self._started = threading.Event()
        self._running = False

    def call(self, *args, **kwargs):
        """Call the function inside the rpc thread."""

        with self._lock:
            if not self._running:
                raise RPCNotRunning()

            # Create an object to hold the result of the function call.
            result = _Result()

            # Since we will block while waiting for our function to get called, we
            # can cache our events per thread.
            try:
                event = self._events.event
            except AttributeError:
                event = self._events.event = threading.Event()
            else:
                event.clear()

            msg = (event, result, args, kwargs)
            self._queue.put(msg)

        # Wait for the message to be processed.
        event.wait()

        assert result.result is not _NULL, "function failed to get called!"

        # Raise any exceptions we've received.
        if isinstance(result.result, BaseException):
            raise result.result
        else:
            return result.result

    def start(self, *args, **kwargs):
        super().start(*args, **kwargs)

        self._started.wait()

    def run(self):
        """Run the rpc thread."""

        self._started.set()
        self._running = True

        try:
            while True:
                msg = self._queue.get()
                try:
                    # If we received a null value, then we're supposed to shut
                    # down.
                    if msg is _NULL:
                        sys.stdout.flush()
                        break

                    self._process(msg)
                finally:
                    # ... and let the queue know we finished.
                    self._queue.task_done()
        except KeyboardInterrupt:
            # let the main thread know we got a SIGINT
            import _thread
            _thread.interrupt_main()
            raise
        except BaseException as err:
            raise
        finally:
            self._running = False

    def _process(self, msg):
        # Break up the message.
        event, result, args, kwargs = msg

        try:
            result.result = self._handler(*args, **kwargs)
        except Exception as err:
            result.result = err
        except BaseException as err:
            # Let the client know that we had a fatal error.
            result.result = err
            raise
        finally:
            # Let the client know that we finished.
            event.set()

    def join(self, *args, **kwargs):
        """Inform the thread to shut down."""

        if self._running:
            with self._lock:
                # Shut down the queue first.
                self._queue.put(_NULL)
                self._queue.join()

                super().join(*args, **kwargs)
