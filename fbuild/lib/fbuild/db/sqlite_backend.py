import io
import pickle
import sqlite3
import weakref

import fbuild.db
import fbuild.db.backend
import fbuild.path

# ------------------------------------------------------------------------------

class _ObjectID:
    def __init__(self, cls, state):
        self.cls = cls
        self.state = state

# ------------------------------------------------------------------------------

class SqliteBackend(fbuild.db.backend.Backend):
    """
    A sqlite-based fbuild backend database.
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self._pickle_data = io.BytesIO()
        self._pickler = fbuild.db.backend.Pickler(
            self._ctx,
            self._pickle_data)

        self._unpickler = fbuild.db.backend.Unpickler(
            self._ctx,
            self._pickle_data)


    def connect(self, filename):
        """Connect to the database."""

        self._file_name = fbuild.path.Path(filename)

        self.conn = sqlite3.connect(self._file_name)
        self.cursor = self.conn.cursor()

        self._initialize_database()


    def close(self):
        self.conn.close()


    def _initialize_database(self):
        self.cursor.executescript('''
            PRAGMA foreign_keys = ON;

            CREATE TABLE IF NOT EXISTS Function (
                fun_id INTEGER PRIMARY KEY AUTOINCREMENT,
                fun_name TEXT UNIQUE,
                fun_digest TEXT);
            CREATE INDEX IF NOT EXISTS Function_name_index ON
                Function (fun_name);

            CREATE TABLE IF NOT EXISTS Call (
                call_id INTEGER PRIMARY KEY AUTOINCREMENT,
                fun_id INTEGER REFERENCES Function(fun_id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE,
                call_bound BLOB,
                call_result BLOB);
            CREATE INDEX IF NOT EXISTS Call_fun_id_index ON
                Call (fun_id);

            CREATE TABLE IF NOT EXISTS File (
                file_id INTEGER PRIMARY KEY AUTOINCREMENT,
                file_name TEXT UNIQUE,
                file_mtime INTEGER,
                file_digest TEXT);
            CREATE INDEX IF NOT EXISTS File_name_index ON
                File (file_name);

            CREATE TABLE IF NOT EXISTS CallFile (
                call_id INTEGER REFERENCES Call(call_id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE,
                file_id INTEGER REFERENCES File(file_id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE,
                file_digest TEXT,
                PRIMARY KEY (call_id, file_id));

            CREATE TABLE IF NOT EXISTS ExternalSrc (
                call_id INTEGER REFERENCES Call(call_id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE,
                file_id INTEGER REFERENCES File(file_id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE,
                PRIMARY KEY (call_id, file_id));

            CREATE TABLE IF NOT EXISTS ExternalDst (
                call_id INTEGER REFERENCES Call(call_id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE,
                file_id INTEGER REFERENCES File(file_id)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE,
                PRIMARY KEY (call_id, file_id));
            ''')

    # --------------------------------------------------------------------------

    def cache(self, *args, **kwargs):
        with self.conn:
            return super().cache(*args, **kwargs)

    # --------------------------------------------------------------------------

    def find_function(self, fun_name):
        """Returns the function record or None if it does not exist."""

        # Make sure we got the right types.
        assert isinstance(fun_name, str), fun_name

        self.cursor.execute(
            'SELECT fun_id,fun_digest FROM Function WHERE fun_name=?',
            (fun_name,))

        rows = self.cursor.fetchall()

        if not rows:
            return None, None
        else:
            (fun_id, fun_digest), = rows
            return fun_id, fun_digest


    def save_function(self, fun_id, fun_name, fun_digest):
        """Insert or update the function's digest."""

        # Make sure we got the right types.
        assert isinstance(fun_id, int) or fun_id is None, fun_id
        assert isinstance(fun_name, str), fun_name
        assert isinstance(fun_digest, str), fun_digest

        if fun_id is None:
            self.cursor.execute(
                'INSERT INTO Function (fun_name, fun_digest) VALUES (?,?)',
                (fun_name, fun_digest))

            fun_id = self.cursor.lastrowid
        else:
            self.cursor.execute(
                'UPDATE Function SET fun_digest=? WHERE fun_id=?',
                (fun_digest, fun_id))

        return fun_id


    def delete_function(self, fun_name):
        """Clear the function from the database."""

        # Make sure we got the right types.
        assert isinstance(fun_name, str), fun_name

        # Since the function was removed, all of this function's calls and call
        # files are dirty, so delete them.
        for call_id, in self.cursor.execute('''
                SELECT call_id FROM Call
                JOIN Function USING (fun_id)
                WHERE fun_name=?
                ''', (fun_name,)):
            self.cursor.execute(
                'DELETE FROM Call WHERE call_id=?',
                (call_id,))

            self.cursor.execute(
                'DELETE FROM CallFile WHERE call_id=?',
                (call_id,))

            self.cursor.execute(
                'DELETE FROM ExternalSrc WHERE call_id=?',
                (call_id,))

            self.cursor.execute(
                'DELETE FROM ExternalDst WHERE call_id=?',
                (call_id,))

        self.cursor.execute(
            'DELETE FROM Function WHERE fun_name=?',
            (fun_name,))

    # --------------------------------------------------------------------------

    def _pickle_dumps(self, obj):
        def persistent_id(obj):
            if obj is self._ctx:
                return b'ctx'
            else:
                return None

        f = io.BytesIO()
        pickler = pickle._Pickler(f, protocol=pickle.HIGHEST_PROTOCOL)
        pickler.persistent_id = persistent_id

        def persist(obj):
            if isinstance(obj, fbuild.db.PersistentObject):
                # Make sure all the values are persisted.
                state = {k: persist(v) for k, v in obj.__dict__.items()}
                return _ObjectID(obj.__class__, state)
            else:
                return obj

        obj = persist(obj)
        pickler.dump(obj)

        return f.getvalue()


    def _pickle_loads(self, value):
        def persistent_load(pid):
            if pid == b'ctx':
                return self._ctx
            else:
                raise pickle.UnpicklingError(
                    'unsupported persistent object: %r'  % pid)

        f = io.BytesIO(value)
        unpickler = pickle._Unpickler(f)
        unpickler.persistent_load = persistent_load
        obj = unpickler.load()

        def unpersist(obj):
            if isinstance(obj, _ObjectID):
                o = object.__new__(obj.cls)
                for key, value in obj.state.items():
                    setattr(o, key, unpersist(value))
                return o
            else:
                return obj

        return unpersist(obj)


    def find_call(self, fun_id, bound):
        """Returns the function call index and result or None if it does not
        exist."""

        # Make sure we got the right types.
        assert isinstance(fun_id, int), fun_id
        assert isinstance(bound, dict), bound

        # We've called this before, so search the data to see if we've called
        # it with the same arguments.
        for call_id, old_bound, old_result in self.cursor.execute('''
                SELECT call_id, call_bound, call_result
                FROM Call
                WHERE fun_id=?
                ''', (fun_id,)):
            old_bound = self._pickle_loads(old_bound)

            if bound == old_bound:
                old_result = self._pickle_loads(old_result)

                return False, call_id, old_result
        else:
            return True, None, None


    def save_call(self, call_id, fun_id, call_bound, call_result):
        """Insert or update the function call."""

        # Make sure we got the right types.
        assert isinstance(call_id, int) or call_id is None, call_id
        assert isinstance(fun_id, int), fun_id
        assert isinstance(call_bound, dict), call_bound

        call_result = self._pickle_dumps(call_result)

        # Insert or update the call result.
        if call_id is None:
            call_bound = self._pickle_dumps(call_bound)

            self.cursor.execute('''
                INSERT INTO Call (fun_id,call_bound,call_result)
                VALUES (?,?,?)
                ''', (
                    fun_id,
                    sqlite3.Binary(call_bound),
                    sqlite3.Binary(call_result)))

            call_id = self.cursor.lastrowid
        else:
            self.cursor.execute(
                'UPDATE Call SET call_result=? WHERE call_id=?',
                (sqlite3.Binary(call_result), call_id))

        return call_id

    # --------------------------------------------------------------------------

    def find_call_file(self, call_id, file_id):
        """Returns the digest of the file from the last time we called this
        function, or None if it does not exist."""

        # Make sure we got the right types.
        assert isinstance(call_id, int), call_id
        assert isinstance(file_id, int), file_id

        self.cursor.execute('''
            SELECT file_digest
            FROM CallFile
            WHERE call_id=? AND file_id=?
            ''', (call_id, file_id))

        rows = self.cursor.fetchall()

        if not rows:
            # This is the first time we've seen this call, so return None.
            return None
        else:
            (file_digest,), = rows
            return file_digest


    def save_call_file(self, call_id, file_id, file_digest):
        """Insert or update the call file."""

        # Make sure we got the right types.
        assert isinstance(call_id, int), call_id
        assert isinstance(file_id, int), file_id
        assert isinstance(file_digest, str), file_digest

        self.cursor.execute('''
            INSERT OR REPLACE INTO CallFile (call_id,file_id,file_digest)
            VALUES (?,?,?)
            ''', (call_id, file_id, file_digest))

    # --------------------------------------------------------------------------

    def find_external_srcs(self, call_id):
        """Returns all of the externally specified call src files"""

        # Make sure we got the right types.
        assert isinstance(call_id, int), call_id

        srcs = frozenset(file_name for file_name, in
            self.cursor.execute('''
                SELECT file_name
                FROM File
                JOIN ExternalSrc USING (file_id)
                WHERE call_id=?
                ''', (call_id,)))

        return srcs


    def find_external_dsts(self, call_id):
        """Returns all of the externally specified call dst files"""

        # Make sure we got the right types.
        assert isinstance(call_id, int), call_id

        dsts = frozenset(file_name for file_name, in
            self.cursor.execute('''
                SELECT file_name
                FROM File
                JOIN ExternalDst USING (file_id)
                WHERE call_id=?
                ''', (call_id,)))

        return dsts


    def save_external_files(self, call_id, srcs, dsts):
        """Insert or update the externally specified call files."""

        # Make sure we got the right types.
        assert isinstance(call_id, int), call_id
        assert all(isinstance(src, str) for src in srcs), srcs
        assert all(isinstance(dst, str) for dst in dsts), dsts

        # ----------------------------------------------------------------------

        self.cursor.execute(
            'DELETE FROM ExternalSrc WHERE call_id=?',
            (call_id,))

        self.cursor.execute(
            'DELETE FROM ExternalDst WHERE call_id=?',
            (call_id,))

        # ----------------------------------------------------------------------

        if srcs:
            # XXX: There's a python bug where you can't pass a generator that
            # itself inserts into sqlite, so we need to force the generator.
            src_call_files = [self.check_call_file(call_id, src)
                for src in srcs]

            self.cursor.executemany(
                'INSERT INTO ExternalSrc (call_id,file_id) VALUES (?,?)',
                ((call_id, file_id)
                    for dirty, file_id, file_digest in src_call_files))

            # Also make sure we saved the call file.
            self.cursor.executemany('''
                INSERT OR REPLACE INTO CallFile (call_id, file_id, file_digest)
                VALUES (?,?,?)
                ''', ((call_id, file_id, file_digest)
                    for dirty, file_id, file_digest in src_call_files if dirty))

        # ----------------------------------------------------------------------

        if dsts:
            # We don't need to save a list here because we're only using it
            # once.
            # XXX: There's a python bug where you can't pass a generator that
            # itself inserts into sqlite, so we need to force the generator.
            dst_call_files = [self.check_call_file(call_id, dst)
                for dst in dsts]

            self.cursor.executemany(
                'INSERT INTO ExternalDst (call_id,file_id) VALUES (?,?)',
                ((call_id, file_id)
                    for dirty, file_id, file_digest in dst_call_files if dirty))

    # --------------------------------------------------------------------------

    def find_file(self, file_name):
        """Returns the mtime and digest of the file, or None if it does not
        exist."""

        self.cursor.execute('''
            SELECT file_id,file_mtime,file_digest
            FROM File
            WHERE file_name=?
            ''', (file_name,))

        rows = self.cursor.fetchall()

        if not rows:
            return None, None, None

        (file_id, file_mtime, file_digest), = rows

        return file_id, file_mtime, file_digest


    def save_file(self, file_id, file_name, file_mtime, file_digest):
        """Insert or update the file."""

        # Make sure we got the right types.
        assert isinstance(file_name, str) or file_id is None, file_name
        assert isinstance(file_mtime, float), file_mtime
        assert isinstance(file_digest, str), file_digest

        if file_id is None:
            self.cursor.execute('''
                INSERT INTO File (file_name,file_mtime,file_digest)
                VALUES (?,?,?)
                ''', (file_name, file_mtime, file_digest))

            file_id = self.cursor.lastrowid
        else:
            self.cursor.execute(
                'UPDATE File SET file_mtime=?, file_digest=? WHERE file_id=?',
                (file_mtime, file_digest, file_id))

        return file_id


    def delete_file(self, file_name):
        """Remove the file from the database."""

        self.cursor.execute(
            'SELECT file_id FROM File WHERE file_name=?',
            (file_name,))

        rows = self.cursor.fetchall()

        # Exit early if we got nothing returned.
        if not rows:
            return

        (file_id,), = rows

        # And delete all of the related call files.
        self.cursor.execute(
            'DELETE FROM CallFile WHERE file_id=?',
            (file_id,))

        self.cursor.execute(
            'DELETE FROM ExternalSrc WHERE file_id=?',
            (file_id,))

        self.cursor.execute(
            'DELETE FROM ExternalDst WHERE file_id=?',
            (file_id,))

        self.cursor.execute('DELETE FROM File WHERE file_name=?', (file_name,))
