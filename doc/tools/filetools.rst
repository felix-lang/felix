=================
File System Tools
=================

Felix provides a number of tools to aid with portable
file system operations. The primary driving concept
behind these tools is that of a flat space of structured
filenames which subsets of which can be identified by
regular expressions.

The standard for regular expression is Google RE2.

All tools use `unix` standard pathname conventions
in regular expressions. So :code:`/` is the correct
separator to use, even on Windows.

All tools use `native` conventions for directory names.

flx_ls
======

The simplest tool, `flx_ls` takes up to two arguments.
The first argument is a directory name, the second 
a regular expression. It searches the directory and
all subdirectories thereof recursively for pathnames
exactly matching the regular expression, relative to the
directory, and prints those that match, one per line,
with names relative to the directory.

If the regular expression is omitted, :code:`.*` is assumed,
this matches all files. If the directory is omitted, :code:`.`,
the current directory is assumed.

Given the following directory structure:

.. code-block:: text
    top
      leafA
      leafB
      nodeX
        leafA
        leafC
 

Here are some examples:

.. code-block:: bash

    >flx_ls top
    leafA
    leafB
    nodeX/leafA
    nodeX/leafC

.. code-block:: bash

    >flx_ls top '.*leafA'
    leafA
    nodeX/leafA

.. code-block:: bash

    >flx_ls top '.*/.*'
    nodeX/leafA
    nodeX/leafC


When using bash we strongly recommend enclosing the regexp
in single quotes to ensure the regexp is treated as a single
word and not interpreted.

Remember, the regexp must the pathname completely, from the
first to last character inclusive.

This formulation for finding sets of files is much better
than the common `glob` however you must remember that 
:code:`*` means zero or more occurrences, and that :code:`.`
means any character. To find all  files with an extension
of three characters you would use:

.. code-block:: bash

    >flx_ls top '.*\.[^.]{3}'

See: https://github.com/google/re2/wiki/Syntax for details.


