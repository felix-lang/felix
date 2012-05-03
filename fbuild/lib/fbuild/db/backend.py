import io
import pickle
import time

from fbuild.path import Path

# ------------------------------------------------------------------------------

class Backend:
    def __init__(self, ctx):
        self._ctx = ctx

    # --------------------------------------------------------------------------

    def connect(self, *args, **kwargs):
        """Connect to the database."""
        raise NotImplementedError


    def close(self, *args, **kwargs):
        """Connect to the database backend."""
        raise NotImplementedError

    # --------------------------------------------------------------------------

    def prepare(self, fun_name, fun_digest, bound, srcs, dsts):
        """Queries all the information needed to cache a function."""

        # Check if the function changed.
        fun_dirty, fun_id = self.check_function(fun_name, fun_digest)

        # Check if this is a new call and get the index.
        if fun_dirty or fun_id is None:
            call_dirty = True
            call_id = None
            old_result = None
        else:
            call_dirty, call_id, old_result = self.find_call(fun_id, bound)

        # Add the source files to the database. We always run this because it
        # adds our call files to the database for us.
        call_file_digests = self.check_call_files(call_id, srcs)

        # Check extra external call files.
        if call_id is None:
            external_srcs = frozenset()
            external_dsts = frozenset()
            external_digests = ()
        else:
            external_srcs, external_dsts, external_digests = \
                self.check_external_files(call_id)

        return (
            fun_dirty,
            fun_id,
            call_dirty,
            call_id,
            old_result,
            call_file_digests,
            external_srcs,
            external_dsts,
            external_digests)

    def cache(self,
            fun_dirty,
            fun_id,
            fun_name,
            fun_digest,
            call_id,
            bound,
            result,
            call_file_digests,
            external_srcs,
            external_dsts):
        """Saves the function call into the database."""

        # Lock the db since we're updating data structures.
        if fun_dirty:
            # Since the function changed, delete out all the related data.
            if fun_id is not None:
                self.delete_function(fun_name)

                # The fun_id is now invalid.
                fun_id = None

            fun_id = self.save_function(fun_id, fun_name, fun_digest)

        # Get the real call_id to use in the call files.
        call_id = self.save_call(call_id, fun_id, bound, result)
        self.save_call_files(call_id, call_file_digests)

        self.save_external_files(call_id, external_srcs, external_dsts)

    # --------------------------------------------------------------------------

    def check_function(self, fun_name, fun_digest):
        """Returns whether or not the function is dirty. Returns True or false
        as well as the function's digest."""

        # Make sure we got the right types.
        assert isinstance(fun_digest, str), fun_digest

        fun_id, old_digest = self.find_function(fun_name)

        # Check if the function changed. If it didn't, assume that the function
        # didn't change either (although any sub-functions could have).
        return old_digest is None or fun_digest != old_digest, fun_id


    def find_function(self, fun_name):
        """Returns the function record or None if it does not exist."""
        raise NotImplementedError


    def save_function(self, fun_id, fun_name, fun_digest):
        """Insert or update the function's digest."""
        raise NotImplementedError


    def delete_function(self, fun_name):
        """Clear the function from the database."""
        raise NotImplementedError

    # --------------------------------------------------------------------------

    def find_call(self, fun_id, bound):
        """Returns the function call index and result or None if it does not
        exist."""
        raise NotImplementedError


    def save_call(self, call_id, fun_id, bound, result):
        """Insert or update the function call."""
        raise NotImplementedError

    # --------------------------------------------------------------------------

    def check_call_files(self, call_id, file_names):
        """Returns all of the dirty call files."""

        digests = set()
        for file_name in file_names:
            d, file_id, file_digest = self.check_call_file(call_id, file_name)
            if d:
                digests.add((file_id, file_name, file_digest))

        return digests


    def save_call_files(self, call_id, digests):
        """Insert or update the call files."""

        for file_id, file_name, file_digest in digests:
            self.save_call_file(call_id, file_id, file_digest)

    # --------------------------------------------------------------------------

    def check_call_file(self, call_id, file_name):
        """Returns if the call file is dirty and the file's digest."""

        # Make sure we got the right types.
        assert isinstance(file_name, str), file_name

        # Compute the digest of the file.
        dirty, file_id, mtime, digest = self.add_file(file_name)

        # Exit early if we don't have a valid call_id.
        if call_id is None:
            return True, file_id, digest

        old_digest = self.find_call_file(call_id, file_id)

        # Now, check if the file changed from the previous run. If it did then
        # return True.
        if old_digest is not None and digest == old_digest:
            # We're okay, so return if the file's been updated.
            return dirty, file_id, digest
        else:
            # The digest's different, so we're dirty.
            return True, file_id, digest


    def find_call_file(self, call_id, file_id):
        """Returns the digest of the file from the last time we called this
        function, or None if it does not exist."""
        raise NotImplementedError


    def save_call_file(self, call_id, file_id, file_digest):
        """Insert or update the call file."""
        raise NotImplementedError

    # --------------------------------------------------------------------------

    def check_external_files(self, call_id):
        """Returns all of the externally specified call files, and the dirty
        list."""

        # Do nothing if we don't have a valid call.
        if call_id is None:
            srcs = frozenset()
            dsts = frozenset()
            external_digests = ()
        else:
            srcs = self.find_external_srcs(call_id)
            dsts = self.find_external_dsts(call_id)

            external_digests = []
            for src in srcs:
                try:
                    d, file_id, file_digest = self.check_call_file(call_id, src)
                except OSError:
                    pass
                else:
                    if d:
                        external_digests.append((file_id, src, file_digest))

        return srcs, dsts, external_digests


    def find_external_srcs(self, call_id):
        """Returns all of the externally specified call src files"""
        raise NotImplementedError


    def find_external_dsts(self, call_id):
        """Returns all of the externally specified call dst files"""
        raise NotImplementedError


    def save_external_files(self, call_id, srcs, dsts):
        """Insert or update the externally specified call files."""
        raise NotImplementedError

    # --------------------------------------------------------------------------

    def add_file(self, file_name):
        """Insert or update the file information. Returns True if the content
        of the file is different from what was in the table."""

        # Make sure we got the right types.
        assert isinstance(file_name, str), file_name

        # Look up the old data.
        file_id, old_mtime, old_digest = self.find_file(file_name)

        # Now, create a path object and find it's mtime.
        file_path = Path(file_name)
        file_mtime = file_path.getmtime()

        if old_mtime is not None:
            # If the file was modified less than 1.0 seconds ago, recompute the
            # hash since it still could have changed even with the same mtime.
            # If True, then assume the file has not been modified.
            if file_mtime == old_mtime and time.time() - file_mtime > 1.0:
                return False, file_id, file_mtime, old_digest

        # The mtime changed, so let's see if the content's changed.
        digest = file_path.digest()

        if digest == old_digest:
            # Save the new mtime.
            self.save_file(file_id, file_name, file_mtime, digest)
            return False, file_id, file_mtime, digest

        if file_id is not None:
            # Since the function changed, all of the calls that used this
            # function are dirty.
            self.delete_file(file_name)

            # The file_id is now invalid.
            file_id = None

        # Now, add the file back to the database.
        file_id = self.save_file(file_id, file_name, file_mtime, digest)

        # Returns True since the file changed.
        return True, file_id, file_mtime, digest


    def find_file(self, file_name):
        """Returns the file's old mtime and digest or None if it does not
        exist."""
        raise NotImplementedError


    def save_file(self, file_id, file_name, file_mtime, file_digest):
        """Insert or update the file."""
        raise NotImplementedError


    def delete_file(self, file_name):
        """Remove the file from the database."""
        raise NotImplementedError

# ------------------------------------------------------------------------------

class Pickler(pickle.Pickler):
    """Create a custom pickler that won't try to pickle the context."""

    def __init__(self, ctx, *args, **kwargs):
        super().__init__(*args, protocol=pickle.HIGHEST_PROTOCOL, **kwargs)
        self.ctx = ctx

    def persistent_id(self, obj):
        if obj is self.ctx:
            return b'ctx'
        else:
            return None

class Unpickler(pickle.Unpickler):
    """Create a custom unpickler that will substitute the current context."""

    def __init__(self, ctx, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.ctx = ctx

    def persistent_load(self, pid):
        if pid == b'ctx':
            return self.ctx
        else:
            raise pickle.UnpicklingError('unsupported persistent object: %r' %
                pid)


def pickle_dumps(ctx, obj):
    f = io.BytesIO()
    pickler = Pickler(ctx, f)
    pickler.dump(obj)

    return f.getvalue()


def pickle_loads(ctx, string):
    f = io.BytesIO(string)
    unpickler = Unpickler(ctx, f)
    return unpickler.load()
