import platform
import os

import fbuild
import fbuild.builders
import fbuild.db

# ------------------------------------------------------------------------------

class UnknownPlatform(fbuild.ConfigFailed):
    def __init__(self, platform=None):
        self.platform = platform

    def __str__(self):
        if self.platform is None:
            return 'cannot determine platform'
        else:
            return 'unknown platform: "%s"' % self.platform

# ------------------------------------------------------------------------------

archmap = {
    'irix':      {'posix', 'irix'},
    'irix64':    {'posix', 'irix', 'irix64'},
    'unix':      {'posix'},
    'posix':     {'posix'},
    'linux':     {'posix', 'linux'},
    'gnu/linux': {'posix', 'linux'},
    'solaris':   {'posix', 'solaris'},
    'sunos':     {'posix', 'solaris', 'sunos'},
    'cygwin':    {'posix', 'cygwin'},
    'nocygwin':  {'posix', 'cygwin', 'nocygwin'},
    'mingw':     {'posix', 'mingw'},
    'windows':   {'windows', 'win32'},
    'nt':        {'windows', 'win32', 'nt'},
    'win32':     {'windows', 'win32'},
    'win64':     {'windows', 'win64'},
    'windows32': {'windows', 'win32'},
    'windows64': {'windows', 'win64'},
    'freebsd':   {'posix', 'bsd', 'freebsd'},
    'netbsd':    {'posix', 'bsd', 'netbsd'},
    'openbsd':   {'posix', 'bsd', 'openbsd'},
    'darwin':    {'posix', 'bsd', 'darwin', 'macosx'},
    'osx':       {'posix', 'bsd', 'darwin', 'macosx'},

    'iphone':           {'posix', 'bsd', 'darwin', 'iphone'},
    'iphone-sim':       {'posix', 'bsd', 'darwin', 'iphone', 'simulator'},
    'iphone-simulator': {'posix', 'bsd', 'darwin', 'iphone', 'simulator'},
}

# ------------------------------------------------------------------------------

@fbuild.db.caches
def guess_platform(ctx, arch=None):
    """L{guess_platform} returns a platform set that describes the various
    features of the specified I{platform}. If I{platform} is I{None}, try to
    determine which platform the system is and return that value. If the
    platform cannot be determined, return I{None}."""
    ctx.logger.check('determining platform')
    if arch is None:
        # If we're on Windows, then don't even try uname
        if os.name == 'nt':
            res = archmap[platform.system().lower()]
            ctx.logger.passed(res)
            return frozenset(res)
        # Let's see if uname exists
        try:
            uname = fbuild.builders.find_program(ctx, ['uname'], quieter=1)
        except fbuild.builders.MissingProgram:
            # Maybe we're on windows. Let's just use what python thinks is the
            # platform.
            #arch = os.name
            arch = platform.system().lower()
        else:
            # We've got uname, so let's see what platform it thinks we're on.
            try:
                stdout, stderr = ctx.execute((uname, '-s'), quieter=1)
            except fbuild.ExecutionError:
                # Ack, that failed too. Just fall back to python.
                #arch = os.name
                arch = platform.system().lower()
            else:
                arch = stdout.decode('utf-8').strip().lower()

    if arch.startswith('mingw32'):
        arch = 'mingw'
    try:
        architecture = archmap[arch]
    except KeyError:
        ctx.logger.failed()
        raise UnknownPlatform(arch)
    else:
        ctx.logger.passed(architecture)
        return frozenset(architecture)

# ------------------------------------------------------------------------------

def obj_suffix(ctx, platform=None):
    platform = platform if platform else guess_platform(ctx)
    if 'windows' in platform:
        return '.obj'
    else:
        return '.o'

# ------------------------------------------------------------------------------

def static_obj_suffix(ctx, platform=None):
    platform = platform if platform else guess_platform(ctx)
    if 'windows' in platform:
        return '_static.obj'
    else:
        return '.o'

def static_lib_prefix(ctx, platform=None):
    platform = platform if platform else guess_platform(ctx)
    if 'windows' in platform:
        return ''
    else:
        return 'lib'

def static_lib_suffix(ctx, platform=None):
    platform = platform if platform else guess_platform(ctx)
    if 'windows' in platform:
        return '.lib'
    else:
        return '.a'

# ------------------------------------------------------------------------------

def shared_obj_suffix(ctx, platform=None):
    platform = platform if platform else guess_platform(ctx)
    if 'windows' in platform:
        return '_shared.obj'
    else:
        return '.os'

def shared_lib_prefix(ctx, platform=None):
    platform = platform if platform else guess_platform(ctx)
    if platform & {'windows', 'mingw'}:
        return ''
    else:
        return 'lib'

def shared_lib_suffix(ctx, platform=None):
    platform = platform if platform else guess_platform(ctx)
    if platform & {'windows', 'mingw'}:
        return '.dll'
    elif 'darwin' in platform:
        return '.dylib'
    else:
        return '.so'

# ------------------------------------------------------------------------------

def exe_suffix(ctx, platform=None):
    platform = platform if platform else guess_platform(ctx)
    if platform & {'windows', 'mingw'}:
        return '.exe'
    else:
        return ''

# ------------------------------------------------------------------------------

def runtime_env_libpath(ctx, platform=None):
    platform = platform if platform else guess_platform(ctx)
    if platform & {'windows', 'mingw'}:
        return 'PATH'
    elif 'darwin' in platform:
        return 'DYLD_LIBRARY_PATH'
    else:
        return 'LD_LIBRARY_PATH'
