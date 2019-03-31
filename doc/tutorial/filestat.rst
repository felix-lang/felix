FileStat
========

Information about files can be found from class FileStat.
It is not open by default.

Get information about a file into a status buffer.
Sets error code at argument 3 pointer.

.. code-block:: felix

  proc stat: string * &stat_t * &int;

set access and modification time of a file.
Sets error code at argument 4 pointer.
Times are in seconds, nominally from Epoch (Jan 1 1970).

.. code-block:: felix

  proc utime: string * double * double * &int;

Change read,write permissions for group, owner etc.
Return 0 on success.
On Windows this function may silently fail to obey
unsupported operations.

.. code-block:: felix

  gen chmod: string * mode_t -> int;

set mask for subsequent permissions.
On Windows this function may silently fail to obey
unsupported operations.

.. code-block:: felix

  gen umask: mode_t -> mode_t;

Abstracted platform independent file type taxonomy.

.. code-block:: felix

  variant file_type_t = 
    | PIPE 
    | STREAM 
    | DIRECTORY 
    | BLOCK 
    | REGULAR 
    | SYMLINK 
    | SOCKET 
    | INDETERMINATE
    | NONEXISTANT
    | NOPERMISSION
  ;

Get the file type from a file stat buffer.

.. code-block:: felix

  virtual fun file_type: &stat_t -> file_type_t;

Fill a stat buffer with information about a file.

.. code-block:: felix

  gen stat(file: string, statbuf:&stat_t);

Get a file last modification time from a stat buffer.
Time is in seconds.

.. code-block:: felix

  fun mtime: &stat_t -> double = "(double)($1->st_mtime)";

Get a file creation time from a stat buffer.
Note: not available on Unix.
Time is in seconds.

.. code-block:: felix

  fun ctime: &stat_t -> double = "(double)($1->st_ctime)";

Get modification time of a file by name.
Time is in seconds.

.. code-block:: felix

  fun filetime(f:string):double;

Set the last access and modification time of a file by name.

.. code-block:: felix

  gen utime(f:string, a:double, m:double): bool;

Set the last access and modification time of a file by name,
where the two times are given by a single argument.

.. code-block:: felix

  gen utime(f:string, t:double);

Check if a file exists.

.. code-block:: felix

  fun fileexists(f:string):bool=> filetime f != 0.0;

Find the type of a file.

.. code-block:: felix

  fun filetype(f:string):file_type_t;

File time conversions.

.. code-block:: felix

  fun past_time () => -1.0;
  fun future_time () => double(ulong(-1)); // a hacky way to get a big number

  fun strfiletime0 (x:double) :string;
  fun strfiletime (x:double) : string;
  fun dfiletime(var f:string, dflt:double) : double;

