import difflib
import sys

import fbuild
import fbuild.builders.text as text
import fbuild.path

# ------------------------------------------------------------------------------

def build(ctx):
    patterns = {'a': 1, 'b': 2}

    foo = text.format_substitute(ctx, 'foo.py', 'foo.py.in', patterns)
    ctx.execute((sys.executable, foo))

    bar = text.autoconf_config_file(ctx, 'bar.py', 'bar.py.in', patterns)
    ctx.execute((sys.executable, bar))

    baz_h = text.autoconf_config_header(ctx, 'baz.h', 'baz.h.in', {
        'foo': '"FOO"',
        'HAVE_FOO': 1,
        'HAVE_BAR': 0,
        'HAVE_BAZ': '"BAZ"',
    })

    with open(baz_h) as f:
        src = f.read()

    result = '''\
const char* foo = "FOO";

#define HAVE_FOO 1
/* #undef HAVE_BAR */
#define HAVE_BAZ "BAZ"
'''

    if src == result:
        ctx.logger.log('header matches')
    else:
        for line in difflib.ndiff(
                src.split('\n'),
                result.split('\n')):
            ctx.logger.log(line)
