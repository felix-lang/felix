import hashlib
import itertools
import pprint
import threading

import fbuild
import fbuild.functools
import fbuild.inspect
import fbuild.path
import fbuild.rpc

import fbuild.db
import fbuild.db.pickle_backend
import fbuild.db.cache_backend
import fbuild.db.sqlite_backend

# ------------------------------------------------------------------------------

class Database:
    """L{Database} persistently stores the results of argument calls."""

    def __init__(self, ctx, *, engine, explain=False):
        def handle_rpc(method, *args, **kwargs):
            return method(*args, **kwargs)

        self._ctx = ctx
        self._explain = explain
        self._connected = False

        if engine == 'pickle':
            self._backend = fbuild.db.pickle_backend.PickleBackend(self._ctx)
        elif engine == 'cache':
            self._backend = fbuild.db.cache_backend.CacheBackend(self._ctx)
        elif engine == 'sqlite':
            self._backend = fbuild.db.sqlite_backend.SqliteBackend(self._ctx)
        else:
            raise fbuild.Error('unknown backend: %s' % engine)

        self._rpc = fbuild.rpc.RPC(handle_rpc)
        self._rpc.daemon = True
        self.active_files = set()
        self.start()

    def start(self):
        """Start the server thread."""
        self._rpc.start()

    def shutdown(self, *args, **kwargs):
        """Inform and wait for the L{DatabaseThread} to shut down."""
        self._rpc.join(*args, **kwargs)

    def connect(self, *args, **kwargs):
        """Connect to the database backend."""
        assert not self._connected, 'Already connected to the backend.'

        result = self._rpc.call(self._backend.connect, *args, **kwargs)
        self._connected = True
        return result

    def close(self, *args, **kwargs):
        """Close the connection to the backend."""
        result = self._rpc.call(self._backend.close, *args, **kwargs)
        self._connected = False
        return result

    def call(self, function, *args, **kwargs):
        """Call the function and return the result, src dependencies, and dst
        dependencies. If the function has been previously called with the same
        arguments, return the cached results.  If we detect that the function
        changed, throw away all the cached values for that function. Similarly,
        throw away all of the cached values if any of the optionally specified
        "srcs" are also modified.  Finally, if any of the filenames in "dsts"
        do not exist, re-run the function no matter what."""

        # Make sure none of the arguments are a generator.
        assert all(not fbuild.inspect.isgenerator(arg)
            for arg in itertools.chain(args, kwargs.values())), \
            "Cannot store generator in database"

        fun_name, function, args, kwargs = self._find_function_name(
            function,
            args,
            kwargs)

        # Compute the function digest.
        fun_digest = self._digest_function(function, args, kwargs)

        # Find the call filenames for the function.
        call_bound, srcs, dsts, return_type = self._find_call_filenames(
            function,
            args,
            kwargs)

        fun_dirty, fun_id, call_dirty, call_id, old_result, call_file_digests, \
            external_srcs, external_dsts, external_digests = \
                self._rpc.call(self._backend.prepare,
                    fun_name,
                    fun_digest,
                    call_bound,
                    srcs,
                    dsts)

        dirty_dsts = set()

        # Check if we have a result. If not, then we're dirty.
        if not (fun_dirty or \
                call_dirty or \
                call_file_digests or \
                external_digests):
            # If the result is a dst filename, make sure it exists. If not,
            # we're dirty.
            if return_type is not None and \
                    issubclass(return_type, fbuild.db.DST):
                return_dsts = return_type.convert(old_result)
            else:
                return_dsts = ()

            for dst in itertools.chain(
                    return_dsts,
                    dsts,
                    external_dsts):
                if not fbuild.path.Path(dst).exists():
                    dirty_dsts.add(dst)
                    break
            else:
                # The call was not dirty, so return the cached value.
                all_srcs = srcs.union(external_srcs)
                all_dsts = dsts.union(external_dsts)
                all_dsts.update(return_dsts)
                # Update the active file list.
                self.active_files.update(all_srcs | all_dsts)
                return old_result, all_srcs, all_dsts

        if self._explain:
            # Explain why we are going to run the function.
            if fun_dirty:
                self._ctx.logger.log('function %s is dirty' % fun_name)

            if call_dirty:
                self._ctx.logger.log(
                    'function %s has not been called with these arguments' %
                    fun_name)

            if call_file_digests:
                self._ctx.logger.log('dirty source files:')
                for file_id, src, digest in call_file_digests:
                    self._ctx.logger.log('\t%s %s' % (digest, src))

            if external_digests:
                self._ctx.logger.log('dirty external digests:')
                for file_id, src, digest in external_digests:
                    self._ctx.logger.log('\t%s %s' % (digest, src))

            if dirty_dsts:
                self._ctx.logger.log('destination files do not exist:')
                for dst in dirty_dsts:
                    self._ctx.logger.log('\t%s' % dst)

        # Clear external srcs and dsts since they'll be recomputed inside
        # the function.
        external_srcs = set()
        external_dsts = set()

        # The call was dirty, so recompute it.
        call_result = function(*args, **kwargs)

        # Make sure the result is not a generator.
        assert not fbuild.inspect.isgenerator(call_result), \
            "Cannot store generator in database"

        # Save the results in the database.
        self._rpc.call(self._backend.cache,
            fun_dirty, fun_id, fun_name, fun_digest,
            call_id, call_bound, call_result,
            call_file_digests, external_srcs, external_dsts)

        if return_type is not None and issubclass(return_type, fbuild.db.DST):
            return_dsts = return_type.convert(call_result)
        else:
            return_dsts = ()

        all_srcs = srcs.union(external_srcs)
        all_dsts = dsts.union(external_dsts)
        all_dsts.update(return_dsts)
        # Update the active file list.
        self.active_files.update(all_srcs | all_dsts)
        return call_result, all_srcs, all_dsts

    def delete_function(self, fun_name):
        """Delete the function from the database."""

        return self._rpc.call(self._backend.delete_function, fun_name)

    def delete_file(self, file_name):
        """Delete the file from the database."""

        return self._rpc.call(self._backend.delete_file, file_name)

    def dump_database(self):
        """Print the database."""
        pprint.pprint(self._backend.__dict__)

    def _find_function_name(self, function, args, kwargs):
        """Extract the function name from the function."""

        if not fbuild.inspect.ismethod(function):
            fun_name = function.__module__ + '.' + function.__name__
        else:
            # If we're caching a PersistentObject creation, use the class's
            # name as our function name.
            if function.__name__ == '__call_super__' and \
                    isinstance(function.__self__, fbuild.db.PersistentMeta):
                fun_name = '%s.%s' % (
                    function.__self__.__module__,
                    function.__self__.__name__)
            else:
                fun_name = '%s.%s.%s' % (
                    function.__module__,
                    function.__self__.__class__.__name__,
                    function.__name__)
            args = (function.__self__,) + args
            function = function.__func__

        if not fbuild.inspect.isroutine(function):
            function = function.__call__

        return fun_name, function, args, kwargs

    # Create an in-process cache of the function digests, since they shouldn't
    # change while we're running.
    _digest_function_lock = threading.Lock()
    _digest_function_cache = {}
    def _digest_function(self, function, args, kwargs):
        """Compute the digest for a function or a function object. Cache this
        for this instance."""
        with self._digest_function_lock:
            # If we're caching a PersistentObject creation, use the class's
            # __init__ as our function.
            if fbuild.inspect.isroutine(function) and \
                    len(args) > 0 and \
                    function.__name__ == '__call_super__' and \
                    isinstance(args[0], fbuild.db.PersistentMeta):
                function = args[0].__init__

            try:
                digest = self._digest_function_cache[function]
            except KeyError:
                if fbuild.inspect.isroutine(function):
                    # The function is a function, method, or lambda, so digest
                    # the source. If the function is a builtin, we will raise
                    # an exception.
                    src = fbuild.inspect.getsource(function)
                    digest = hashlib.md5(src.encode()).hexdigest()
                else:
                    # The function is a functor so let it digest itself.
                    digest = hash(function)
                self._digest_function_cache[function] = digest

        return digest

    def _find_call_filenames(self, function, args, kwargs):
        """Return the filenames needed for the function."""

        # Bind the arguments so that we can look up normal args by name.
        bound = fbuild.functools.bind_args(function, args, kwargs)

        # Check if any of the files changed.
        return_type = None
        srcs = set()
        dsts = set()
        for akey, avalue in function.__annotations__.items():
            if akey == 'return':
                return_type = avalue
            elif issubclass(avalue, fbuild.db.SRC):
                srcs.update(avalue.convert(bound[akey]))
            elif issubclass(avalue, fbuild.db.DST):
                dsts.update(avalue.convert(bound[akey]))

        return bound, srcs, dsts, return_type

    def add_external_dependencies_to_call(self, *, srcs=(), dsts=()):
        """When inside a cached method, register additional src
        dependencies for the call. This function can only be called from
        a cached function and will error out if it is called from an
        uncached function."""

        # Hack in additional dependencies
        frame = fbuild.inspect.currentframe()

        if frame is None:
            return

        frame = frame.f_back

        while frame:
            if frame.f_code == self.call.__code__:
                frame.f_locals['external_srcs'].update(srcs)
                frame.f_locals['external_dsts'].update(dsts)

            frame = frame.f_back
