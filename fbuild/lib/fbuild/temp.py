import contextlib
import tempfile as _tempfile
import textwrap

from fbuild.path import Path

# ------------------------------------------------------------------------------

@contextlib.contextmanager
def tempdir(*args, **kwargs):
    '''
    Create a temporary directory and yield it's path. When we regain context,
    remove the directory.
    '''

    path = Path(_tempfile.mkdtemp(*args, **kwargs))
    try:
        yield path
    finally:
        path.rmtree()

# ------------------------------------------------------------------------------

@contextlib.contextmanager
def tempfile(src='', suffix='', name='temp', **kwargs):
    '''
    Create a temporary file in a unique directory and yield the name of the
    file. When we regain context, remove the directory.

    @param src:    write this source in the tempfile before yielding
    @param suffix: the default suffix of the temp file
    @param name:   the name of the temp file
    '''

    with tempdir(**kwargs) as dirname:
        name = dirname / name + suffix
        with open(name, 'w') as f:
            print(textwrap.dedent(src), file=f)

        yield Path(name)
