__version__ = '0.2'

# ------------------------------------------------------------------------------

class Error(Exception):
    pass

class ConfigFailed(Error):
    pass

class ExecutionError(Error):
    def __init__(self, cmd='', stdout='', stderr='', returncode=-1):
        self.cmd = cmd
        self.stdout = stdout
        self.stderr = stderr
        self.returncode = returncode

    def __str__(self):
        if isinstance(self.cmd, str):
            cmd = self.cmd
        else:
            cmd = ' '.join(self.cmd)

        return 'Error running %r exited with %d' % (cmd, self.returncode)

class ExecutionTimedOut(ExecutionError):
    def __str__(self):
        if isinstance(self.cmd, str):
            cmd = self.cmd
        else:
            cmd = ' '.join(self.cmd)

        return 'Timed out running %r exited with %d' % (cmd, self.returncode)
