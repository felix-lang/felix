import fbuild.db.backend

# ------------------------------------------------------------------------------

class CacheBackend(fbuild.db.backend.Backend):
    def connect(self):
        """Create the database cache."""

        self._functions = {}
        self._function_calls = {}
        self._files = {}
        self._call_files = {}
        self._external_srcs = {}
        self._external_dsts = {}

    def close(self):
        """Clear the database cache."""

        del self._functions
        del self._function_calls
        del self._files
        del self._call_files
        del self._external_srcs
        del self._external_dsts

    # --------------------------------------------------------------------------

    def find_function(self, fun_name):
        """Returns the function record or None if it does not exist."""

        # Make sure we got the right types.
        assert isinstance(fun_name, str), fun_name

        try:
            fun_digest = self._functions[fun_name]
        except KeyError:
            # This is the first time we've seen this function.
            fun_id = None
            fun_digest = None
        else:
            # The name is the id.
            fun_id = fun_name

        return fun_id, fun_digest


    def save_function(self, fun_id, fun_name, fun_digest):
        """Insert or update the function's digest."""

        # Make sure we have the right types.
        assert fun_id is fun_name or fun_id is None, (fun_id, fun_name)
        assert isinstance(fun_name, str), fun_name
        assert isinstance(fun_digest, str), fun_digest

        # We don't have separate code paths for existing and non-existing
        # functions.
        self._functions[fun_name] = fun_digest

        # The name is the id.
        return fun_name


    def delete_function(self, fun_name):
        """Clear the function from the database."""

        # Make sure we have the right types.
        assert isinstance(fun_name, str), fun_name

        function_existed = False
        try:
            del self._functions[fun_name]
        except KeyError:
            pass
        else:
            function_existed |= True

        # Since the function was removed, all of this function's calls and call
        # files are dirty, so delete them.
        try:
            del self._function_calls[fun_name]
        except KeyError:
            pass
        else:
            function_existed |= True

        try:
            del self._external_srcs[fun_name]
        except KeyError:
            pass
        else:
            function_existed |= True

        try:
            del self._external_dsts[fun_name]
        except KeyError:
            pass
        else:
            function_existed |= True

        # Since _call_files is indexed by filename, we need to search through
        # each item and delete any references to this function. The assumption
        # is that the files will change much less frequently compared to
        # functions, so we can have this be a more expensive call.
        remove_keys = []
        for key, value in self._call_files.items():
            try:
                del value[fun_name]
            except KeyError:
                pass
            else:
                function_existed |= True

            if not value:
                remove_keys.append(key)

        # If any of the _call_files have no values, remove them.
        for key in remove_keys:
            try:
                del self._call_files[key]
            except KeyError:
                pass
            else:
                function_existed |= True

        return function_existed

    # --------------------------------------------------------------------------

    def find_call(self, fun_id, bound):
        """Returns the function call index and result or None if it does not
        exist."""

        # Make sure we got the right types.
        assert isinstance(fun_id, str), fun_id
        assert isinstance(bound, dict), bound

        try:
            datas = self._function_calls[fun_id]
        except KeyError:
            return True, None, None

        # We've called this before, so search the data to see if we've called
        # it with the same arguments.
        for call_index, (old_bound, old_result) in enumerate(datas):
            if bound == old_bound:
                # We've found a matching call so just return the index.
                return False, (fun_id, call_index), old_result

        # Turns out we haven't called it with these args.
        return True, None, None


    def save_call(self, call_id, fun_id, bound, result):
        """Insert or update the function call."""

        # Make sure we got the right types.
        assert isinstance(call_id, tuple) or call_id is None, call_id
        assert isinstance(fun_id, str), fun_id
        assert isinstance(bound, dict), bound

        if call_id is None:
            # We use the function's name as it's id
            fun_name = fun_id
            call_index = None
        else:
            # Extract out the real fun_id and call_id
            fun_name, call_index = call_id

            assert fun_id is fun_name, (fun_id, fun_name)
            assert isinstance(call_index, int), call_index

        try:
            datas = self._function_calls[fun_name]
        except KeyError:
            # The function be new or may have been deleted. So ignore the
            # call_id and just create a new list.
            self._function_calls[fun_id] = [(bound, result)]

            call_index = 0
        else:
            if call_index is None:
                datas.append((bound, result))
                call_index = len(datas) - 1
            else:
                datas[call_index] = (bound, result)

        return (fun_id, call_index)

    # --------------------------------------------------------------------------

    def find_call_file(self, call_id, file_name):
        """Returns the digest of the file from the last time we called this
        function, or None if it does not exist."""

        # Make sure we got the right types.
        assert isinstance(call_id, tuple), call_id
        assert isinstance(file_name, str), file_name

        # Extract out the real fun_name and call_id
        fun_name, call_index = call_id

        assert isinstance(fun_name, str), fun_name
        assert isinstance(call_index, int), call_index

        # If we don't have a valid call_id, then it's a new call.
        if call_index is None:
            return None

        try:
            return self._call_files[file_name][fun_name][call_index]
        except KeyError:
            # This is the first time we've seen this file with this call.
            return None


    def save_call_file(self, call_id, file_id, file_digest):
        """Insert or update the call file."""

        # Extract out the real fun_name and call_id
        fun_name, call_index = call_id

        # Make sure we got the right types.
        assert isinstance(fun_name, str), fun_name
        assert isinstance(call_index, int), call_index
        assert isinstance(file_id, str), file_id
        assert isinstance(file_digest, str), file_digest

        self._call_files. \
            setdefault(file_id, {}).\
            setdefault(fun_name, {})[call_index] = file_digest

    # --------------------------------------------------------------------------

    def find_external_srcs(self, call_id):
        """Returns all of the externally specified call src files"""

        # Make sure we got the right types.
        assert isinstance(call_id, tuple), call_id

        # Extract out the real fun_name and call_id
        fun_name, call_index = call_id

        assert isinstance(fun_name, str), fun_name
        assert isinstance(call_index, int), call_index

        try:
            return self._external_srcs[fun_name][call_index]
        except KeyError:
            return frozenset()


    def find_external_dsts(self, call_id):
        """Returns all of the externally specified call dst files"""

        # Make sure we got the right types.
        assert isinstance(call_id, tuple), call_id

        # Extract out the real fun_name and call_id
        fun_name, call_index = call_id

        assert isinstance(fun_name, str), fun_name
        assert isinstance(call_index, int), call_index

        try:
            return self._external_dsts[fun_name][call_index]
        except KeyError:
            return frozenset()


    def save_external_files(self, call_id, srcs, dsts):
        """Insert or update the externall specified call files."""

        # Make sure we got the right types.
        assert isinstance(call_id, tuple), call_id
        assert all(isinstance(src, str) for src in srcs), srcs
        assert all(isinstance(dst, str) for dst in dsts), dsts

        # Extract out the real fun_name and call_id
        fun_name, call_index = call_id

        assert isinstance(fun_name, str), fun_name
        assert isinstance(call_index, int), call_index

        srcs = frozenset(srcs)
        dsts = frozenset(dsts)

        self._external_srcs.setdefault(fun_name, {})[call_index] = srcs
        self._external_dsts.setdefault(fun_name, {})[call_index] = dsts

        external_digests = []
        for src in srcs:
            dirty, file_id, mtime, digest = self.add_file(src)
            external_digests.append((file_id, src, digest))

        self.save_call_files(call_id, external_digests)

    # --------------------------------------------------------------------------

    def find_file(self, file_name):
        """Returns the mtime and digest of the file, or None if it does not
        exist."""

        # Make sure we got the right types.
        assert isinstance(file_name, str), file_name

        try:
            file_mtime, file_digest = self._files[file_name]
        except KeyError:
            file_mtime = None
            file_digest = None

        # We'll return the file_name as the file_id.
        return file_name, file_mtime, file_digest


    def save_file(self, file_id, file_name, file_mtime, file_digest):
        """Insert or update the file."""

        # Make sure we got the right types.
        assert file_id is file_name or file_id is None, (file_id, file_name)
        assert isinstance(file_name, str), file_name
        assert isinstance(file_mtime, float), file_mtime
        assert isinstance(file_digest, str), file_digest

        # We don't have separate code paths for existing and non-existing
        # files.
        self._files[file_name] = (file_mtime, file_digest)

        # We'll return the file_name as the file_id.
        return file_name


    def delete_file(self, file_name):
        """Remove the file from the database."""

        # Make sure we got the right types.
        assert isinstance(file_name, str), file_name

        file_existed = False
        try:
            del self._files[file_name]
        except KeyError:
            pass
        else:
            file_existed |= True

        # And delete all of the related call files.
        try:
            del self._call_files[file_name]
        except KeyError:
            pass
        else:
            file_existed |= True

        return file_existed
