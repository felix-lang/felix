import sys
import threading
import contextlib
import collections
import ctypes

import fbuild

# ------------------------------------------------------------------------------

if sys.platform == 'win32':
    # Constants from the Windows API
    _STD_OUTPUT_HANDLE = -11
    _colorcodes = {
        'black'        : 0x0,
        'blue'         : 0x1,
        'green'        : 0x2,
        'cyan'         : 0x3,
        'red'          : 0x4,
        'magenta'      : 0x5,
        'yellow'       : 0x6,
        'white'        : 0x7,
        'gray'         : 0x8,
        'light blue'   : 0x9,
        'light green'  : 0xA,
        'light aqua'   : 0xB,
        'light red'    : 0xC,
        'light purple' : 0xD,
        'light yellow' : 0xE,
        'bright white' : 0xF,
    }
    _INVALID_HANDLE_VALUE = ctypes.c_void_p(-1).value

    def get_csbi_attributes(handle):
        # Based on IPython's winconsole.py, written by Alexander Belchenko
        import struct
        csbi = ctypes.create_string_buffer(22)
        res = ctypes.windll.kernel32.GetConsoleScreenBufferInfo(handle, csbi)
        assert res, str(ctypes.GetLastError())

        (bufx, bufy, curx, cury, wattr, left, top, right, bottom, maxxy,
            maxy) = struct.unpack('hhhhHhhhhhh', csbi.raw)
        return wattr

    def _write_colored_str(s, color):
        if color is None:
            sys.stdout.write(s)
        else:
            try:
                color = _colorcodes[color]
            except KeyError:
                # we couldn't find the color so just ignore
                sys.stdout.write(s)
                return
            try:
                handle = ctypes.windll.kernel32.GetStdHandle(_STD_OUTPUT_HANDLE)
                assert handle != _INVALID_HANDLE_VALUE, str(ctypes.GetLastError())
                reset = get_csbi_attributes(handle)
            except:
                # we may not be printing to a console; just ignore it
                sys.stdout.write(s)
            else:
                ctypes.windll.kernel32.SetConsoleTextAttribute(handle, color)
                sys.stdout.write(s)
                sys.stdout.flush()
                ctypes.windll.kernel32.SetConsoleTextAttribute(handle, reset)

else:
    _colorcodes = {
        'black'  : 30,
        'red'    : 31,
        'green'  : 32,
        'yellow' : 33,
        'blue'   : 34,
        'magenta': 35,
        'cyan'   : 36,
        'white'  : 37,
    }

    def _write_colored_str(s, color):
        if color is not None:
            try:
                color = _colorcodes[color]
            except KeyError:
                # we couldn't find the color so just ignore
                pass
            else:
                s = '\x1b[01;%.2dm%s\x1b[0m' % (color, s)

        sys.stdout.write(s)

# Add in some color aliases
_colorcodes['compile'] = _colorcodes['green']
_colorcodes['link'] = _colorcodes['cyan']

# ------------------------------------------------------------------------------

class _ThreadStack(threading.local, collections.UserList):
    def __init__(self, *args):
        threading.local.__init__(self)
        collections.UserList.__init__(self, *args)

# ------------------------------------------------------------------------------

class Log:
    def __init__(self, file=None, *,
            verbose=0,
            nocolor=False,
            threadcount=1,
            show_threads=False):
        self.file = file
        self.verbose = verbose
        self.nocolor = nocolor
        self.show_threads = show_threads

        self.maxlen = 25
        self._lock = threading.RLock()
        self._threadcount = threadcount
        self._thread_stack = _ThreadStack()

    @contextlib.contextmanager
    def log_from_thread(self):
        self._thread_stack.append([])

        try:
            yield
        finally:
            msgs = self._thread_stack.pop()
            with self._lock:
                for msg, kwargs in msgs:
                    self._write(msg, **kwargs)

    def write(self, msg, *, buffer=True, **kwargs):
        if not buffer or self._threadcount == 1:
            with self._lock:
                self._write(msg, **kwargs)
        else:
            if buffer and self._thread_stack:
                self._thread_stack[-1].append((msg, kwargs))
            else:
                self._write(msg, **kwargs)

    def _write(self, msg, color=None, verbose=0):
        # make sure message is a string
        msg = str(msg)
        if self.file:
            self.file.write(msg)

        if verbose <= self.verbose:
            if self.nocolor:
                color = None
            _write_colored_str(msg, color)
        self.flush()

    def flush(self):
        if self.file:
            self.file.flush()
        sys.stdout.flush()

    def log(self, msg, color=None, verbose=0):
        with self._lock:
            self.write(msg, verbose=verbose, color=color)
            self.write('\n', verbose=verbose)

    def check(self, msg, result=None, color=None, verbose=0):
        if self.show_threads:
            msg = '%-10s: %s' % (threading.current_thread().name, msg)

        d = len(msg)
        if d >= self.maxlen:
            self.maxlen = min(d + 1, 40)

        msg = msg.ljust(self.maxlen) + ': '

        if result is None:
            self.write(msg, color=color, verbose=verbose)
            self.write('\n', verbose=verbose + 1)
        else:
            self.write(msg, verbose=verbose)
            self.write(result, color=color, verbose=verbose)
            self.write('\n', verbose=verbose)

    def passed(self, msg='ok', color='green', **kwargs):
        self.log(msg, color=color, **kwargs)

    def failed(self, msg='failed', color='yellow', **kwargs):
        self.log(msg, color=color, **kwargs)
