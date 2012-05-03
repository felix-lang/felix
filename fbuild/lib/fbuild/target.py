import functools
import operator

import fbuild

# ------------------------------------------------------------------------------

class Target:
    def __init__(self, function, *, name=None, help=None):
        self.function = function
        self.name = name if name is not None else self.function.__name__
        self.help = help if help is not None else self.function.__doc__

# ------------------------------------------------------------------------------

_targets = {}

def register(*args, **kwargs):
    """Decorator that registers a function as a target."""

    def aux(function):
        target = Target(function, *args, **kwargs)
        _targets[target.name] = target
        return function

    return aux

def find(target_name):
    """Look up and return a target."""

    try:
        return _targets[target_name]
    except KeyError:
        raise fbuild.Error('invalid target %r' % target_name)

def help_string():
    """Construct and return a description of all the targets."""

    targets = sorted(_targets.values(), key=operator.attrgetter('name'))

    # Exit early if we don't have any targets.
    if not targets:
        return ''

    # Find the maximum target name, and ljust the help string to that.
    max_width = max(len(target.name) for target in targets)

    return '\n'.join(
        '  {}\t{}'.format(
            target.name.ljust(max_width),
            target.help or '')
        for target in targets)
