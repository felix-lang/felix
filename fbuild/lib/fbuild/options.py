from optparse import OptionParser, make_option

import fbuild.target

# ------------------------------------------------------------------------------

class OptionParser(OptionParser):
    def format_epilog(self, formatter):
        return self.epilog

# ------------------------------------------------------------------------------

def make_parser():
    description = """
    Fbuild is a new kind of build system that is designed around caching
    instead of tree evaluation.
    """

    epilog = '\nTargets:\n{}\n'.format(fbuild.target.help_string())

    parser = OptionParser(
        version=fbuild.__version__,
        usage='%prog [options] target1 [target2 target3 ...]',
        description=description,
        epilog=epilog)

    parser.add_options([
        make_option('-v', '--verbose',
            action='count',
            default=0,
            help='print out extra debugging info'),
        make_option('--show',
            action='count',
            default=1,
            help='print out extra debugging info'),
        make_option('-j', '--jobs',
            dest='threadcount',
            metavar='N',
            type='int',
            default=1,
            help='Allow N jobs at once'),
        make_option('--nocolor',
            action='store_true',
            default=False,
            help='Do not use colors'),
        make_option('--show-threads',
            action='store_true',
            default=False,
            help='Show which thread is running which command'),
        make_option('--configure',
            dest='force_configuration',
            action='store_true',
            default=False,
            help='force reconfiguration'),
        make_option('--buildroot',
            action='store',
            default='build',
            help='where to store the build files (default build)'),
        make_option('--state-file',
            action='store',
            default='fbuild-state.db',
            help='the name of the state file ' \
                 '(default buildroot/fbuild-state.db)'),
        make_option('--log-file',
            action='store',
            default='fbuild.log',
            help='the name of the log file (default fbuild.log)'),
        make_option('--dump-state',
            action='store_true',
            default=False,
            help='print the state database'),
        make_option('--clean',
            dest='clean_buildroot',
            action='store_true',
            default=False,
            help='clean the build directory'),
        make_option('--delete-function',
            action='store',
            help='delete cached data for the specified function'),
        make_option('--delete-file',
            action='store',
            help='delete cached data for the specified file'),
        make_option('--do-not-save-database',
            action='store_true',
            default=False,
            help='do not save the results of the database for testing.'),
        make_option('--explain-database',
            action='store_true',
            default=False,
            help='explain why a function was not cached.'),
        make_option('--database-engine',
            action='store',
            choices=('pickle', 'sqlite', 'cache'),
            default='pickle',
            help='which database engine to use: (pickle, sqlite). pickle is ' \
                'the default'),
    ])

    return parser
