import sys
import os
import signal
import threading
import time

import fbuild
import fbuild.builders.platform
import fbuild.console
import fbuild.db.database
import fbuild.sched
import fbuild.subprocess.killableprocess

from fbuild.path import Path

# ------------------------------------------------------------------------------

class Context:
    def __init__(self, options, args):
        # Convert the paths to Path objects.
        options.buildroot = Path(options.buildroot)
        options.state_file = options.buildroot / options.state_file
        options.log_file = options.buildroot / options.log_file

        self.logger = fbuild.console.Log(
            verbose=options.verbose,
            nocolor=options.nocolor,
            threadcount=options.threadcount,
            show_threads=options.show_threads)

        self.db = fbuild.db.database.Database(self,
            engine=options.database_engine,
            explain=options.explain_database)
        self.scheduler = fbuild.sched.Scheduler(options.threadcount,
            logger=self.logger)

        self.options = options
        self.args = args

        self.install_prefix = Path('/usr/local')
        self.to_install = {'bin': [], 'lib': [], 'share': [], 'include': []}

    @property
    def buildroot(self):
        return self.options.buildroot

    def create_buildroot(self):
        # Make sure the buildroot exists before running.
        self.buildroot.makedirs()

        # Load the logger options into the logger.
        self.logger.file = open(self.options.log_file, 'w')

        # Make sure the state file directory exists.
        self.options.state_file.parent.makedirs()

    def load_configuration(self):
        # Optionally do `not` load the old database by deleting the old state
        # file.
        if self.options.force_configuration and \
                self.options.state_file.exists():
            self.options.state_file.remove()

        self.db.connect(self.options.state_file)

    def save_configuration(self):
        # Optionally do `not` save the database.
        if not self.options.do_not_save_database:
            # Remove the signal handler so that we can't interrupt saving the
            # db.
            prev_handler = signal.signal(signal.SIGINT, signal.SIG_IGN)
            try:
                self.db.close()
            finally:
                signal.signal(signal.SIGINT, prev_handler)

    def prune(self, prune_get_all, prune_get_bad):
        """Delete all destination files that were not referenced during this
           build. This will leave any files outside the build directory. To
           override this function's behavior, use prune_get_all and
           prune_get_bad."""
        all_targets = set(prune_get_all(self))
        bad_targets = prune_get_bad(self, all_targets - self.db.active_files -
                                          {self.options.state_file,
                                           self.options.log_file})
        def error_handler(func, path, exc):
            self.logger.log('error deleting file %s: %s' % (path, exc[1]),
                color='red')
        for file in bad_targets:
            file = Path(file)
            # XXX: There should be a color better than 'compile' for this.
            self.logger.check(' * prune', file, color='compile')
            if file.isdir():
                file.rmtree(ignore_errors=True, on_error=error_handler)
            else:
                try:
                    file.remove()
                except:
                    error_handler(None, file, sys.exc_info())

    # --------------------------------------------------------------------------
    # Logging wrapper functions

#    def log(self, *args, **kwargs):
#        return self.logger.log(*args, **kwargs)
#
#    def check(self, *args, **kwargs):
#        return self.logger.check(*args, **kwargs)
#
#    def passed(self, *args, **kwargs):
#        return self.logger.passed(*args, **kwargs)
#
#    def failed(self, *args, **kwargs):
#        return self.logger.failed(*args, **kwargs)

    # --------------------------------------------------------------------------

    def execute(self, cmd, msg1=None, msg2=None, *,
            color=None,
            quieter=0,
            stdout_quieter=None,
            stderr_quieter=None,
            input=None,
            stdin=None,
            stdout=fbuild.subprocess.PIPE,
            stderr=fbuild.subprocess.PIPE,
            timeout=None,
            env=None,
            runtime_libpaths=None,
            ignore_error=False,
            **kwargs):
        """Execute the command and return the output."""

        if isinstance(cmd, str):
            cmd_string = cmd
        else:
            cmd_parts = []
            # Wrap any space separated parts in quotes.
            for c in cmd:
                if ' ' in c:
                    c = "'{}'".format(c.replace("'", "\\'"))
                cmd_parts.append(c)
            cmd_string = ' '.join(cmd_parts)

        if stdout_quieter is None:
            stdout_quieter = quieter

        if stderr_quieter is None:
            stderr_quieter = quieter

        # Windows needs something in the environment, so for the moment we'll
        # just make sure everything is passed on to the executable.
        if env is None:
            env = dict(os.environ)
        else:
            env = dict(os.environ, **env)

        # Add in the runtime library search paths.
        if runtime_libpaths:
            # Look up the current architecture
            runtime_env_libpath = \
                fbuild.builders.platform.runtime_env_libpath(self)

            runtime_libpaths = os.pathsep.join(runtime_libpaths)
            try:
                libpaths = env[runtime_env_libpath]
            except KeyError:
                libpaths = runtime_libpaths
            else:
                libpaths += os.pathsep + runtime_libpaths

            env[runtime_env_libpath] = libpaths

            # Add the runtime libpaths to the command string.
            cmd_string = '{}={} {}'.format(
                runtime_env_libpath,
                libpaths,
                cmd_string)

        self.logger.write('%-10s: starting %r\n' %
            (threading.current_thread().name, cmd_string),
            verbose=4,
            buffer=False)

        if msg1:
            if msg2:
                self.logger.check(' * ' + str(msg1), str(msg2),
                    color=color,
                    verbose=quieter)
            else:
                self.logger.check(' * ' + str(msg1),
                    color=color,
                    verbose=quieter)

        # Define a function that gets called if execution times out. We will
        # raise an exception if the timeout occurs.
        if timeout:
            timed_out = False
            def timeout_function(p):
                nonlocal timed_out
                timed_out = True
                p.kill(group=True)

            # Set the timer to None for now to make sure it's defined.
            timer = None

        starttime = time.time()
        try:
            p = fbuild.subprocess.killableprocess.Popen(cmd,
                stdin=fbuild.subprocess.PIPE if input else stdin,
                stdout=stdout,
                stderr=stderr,
                env=env,
                **kwargs)

            try:
                if timeout:
                    timer = threading.Timer(timeout, timeout_function, (p,))
                    timer.start()

                stdout, stderr = p.communicate(input)
                returncode = p.wait()
            except KeyboardInterrupt:
                # Make sure if we get a keyboard interrupt to kill the process.
                p.kill(group=True, sigint=True)
                raise
            else:
                # Detect Ctrl-C in subprocess.
                if returncode == -signal.SIGINT:
                    raise KeyboardInterrupt
        except OSError as e:
            # flush the logger
            self.logger.log('command failed: ' + cmd_string, color='red')
            raise e from e
        finally:
            if timeout and timer is not None:
                timer.cancel()
        endtime = time.time()

        if returncode:
            self.logger.log(' + ' + cmd_string, verbose=quieter)
        else:
            self.logger.log(' + ' + cmd_string, verbose=1)

        if stdout:
            try:
                self.logger.log(stdout.rstrip().decode(),
                    verbose=stdout_quieter)
            except UnicodeDecodeError:
                self.logger.log(repr(stdout.rstrip()), verbose=stdout_quieter)

        if stderr:
            try:
                self.logger.log(stderr.rstrip().decode(),
                    verbose=stderr_quieter)
            except UnicodeDecodeError:
                self.logger.log(repr(stderr.rstrip()), verbose=stderr_quieter)

        self.logger.log(
            ' - exit %d, %.2f sec' % (returncode, endtime - starttime),
            verbose=2)

        if timeout and timed_out:
            raise fbuild.ExecutionTimedOut(cmd, stdout, stderr, returncode)
        elif returncode and not ignore_error:
            raise fbuild.ExecutionError(cmd, stdout, stderr, returncode)

        return stdout, stderr

    def install(self, path, category, addroot=''):
        try:
            self.to_install[category].append((Path(path).abspath(), addroot))
        except AttributeError:
            pass
        except KeyError:
            raise fbuild.Error('invalid install category: {}'.format(category))

# ------------------------------------------------------------------------------

def make_default_context(args=[]):
    """Make a default context, usually for tests."""

    import fbuild.options

    parser = fbuild.options.make_parser()
    options, args = parser.parse_args(args)

    return Context(options, args)
