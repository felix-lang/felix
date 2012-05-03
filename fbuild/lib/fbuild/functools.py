import abc
import functools
import inspect
import types

# ------------------------------------------------------------------------------

def import_module(module):
    """L{import_module} is a shortcut that will import and return L{module}
    if it is a I{str}. If not, then it is assumed that L{module} is an actual
    module, and it is returned."""

    if isinstance(module, str):
        return __import__(module, {}, {}, [''])
    return module

def import_function(function):
    """L{import_function} is a shortcut that will import and return L{function}
    if it is a I{str}. If not, then it is assumed that L{function} is callable,
    and is returned."""

    if isinstance(function, str):
        m, f = function.rsplit('.', 1)
        return getattr(__import__(m, {}, {}, ['']), f)
    return function

def call(function, *args, **kwargs):
    """L{call} is a shortcut that will call L{function} with the specified
    arguments. If L{function} is a I{str}, it is imported first. This
    eliminates the need to import the module directly."""

    return import_function(function)(*args, **kwargs)

# ------------------------------------------------------------------------------

def normalize_args(function, args, kwargs):
    '''
    L{normalize_args} returns a normalized set of args and kwargs, with all the
    defaults of the function specified.

    >>> def foo(a, b, c='a', *args, e, f='b', **kwargs):
    ...     pass
    >>> normalize_args(foo, (1, 2, 3, 4, 5), {'e': 6, 'f': 7, 'g': 8}) == (
    ...     (1, 2, 3, 4, 5),
    ...     {'e': 6, 'f': 7, 'g': 8})
    True
    >>> normalize_args(foo, (1, 2), {'e': 6}) == (
    ...     (1, 2, 'a'),
    ...     {'e': 6, 'f': 'b'})
    True
    >>> normalize_args(foo, (1, 2), {'c': 3, 'e': 6}) == (
    ...     (1, 2, 3),
    ...     {'e': 6, 'f': 'b'})
    True
    '''
    # Get the specification of the arguments for the function
    spec = inspect.getfullargspec(function)
    fn_args = spec.args
    fn_kwargs = spec.kwonlyargs
    varargs = spec.varargs is not None
    varkw = spec.varkw is not None

    # If the function is a method, then we've already got the "self" argument
    # bundled up with it, so don't try to find it in our spec.
    if inspect.ismethod(function):
        fn_args = fn_args[1:]

    # If there are no arguments, error out if extra ones were provided.
    if not fn_args and not fn_kwargs and not varargs and not varkw:
        if args or kwargs:
            raise TypeError(
                '%s.%s() takes no arguments (%d given)' % (
                    function.__module__,
                    function.__name__,
                    len(args) + len(kwargs)))
        else:
            return (), {}

    bound_args = []
    bound_kwargs = {}

    # Copy the dictionary as we'll be popping items out of it
    kwargs = dict(kwargs)

    defaults = spec.defaults or []
    defcount = len(defaults)
    fn_argcount = len(fn_args)
    fn_kwargcount = len(fn_kwargs)
    fn_totalargs = fn_argcount + fn_kwargcount

    # Error out if we specified too many arguments and we aren't taking varargs
    if len(args) > fn_totalargs and not varargs:
        raise TypeError(
            '%s.%s() takes %s %d %spositional argument%s (%d given)' % (
                function.__module__,
                function.__name__,
                'at most' if defcount else 'exactly',
                fn_totalargs,
                '' if fn_kwargcount else 'non-keyword ',
                '' if fn_totalargs == 1 else 's',
                len(args)))

    new_args = list(args)

    # For each function argument, find the arg or kwarg it corresponds with
    argcount = 0
    for i, key in enumerate(fn_args):
        # First, check if we provide a kwarg for it
        try:
            value = kwargs.pop(key)
        except KeyError:
            # No kwarg, so see if we specified an argument
            if i < len(args):
                bound_args.append(args[i])

            # no arg, so see if there's a default
            elif fn_argcount - i <= defcount:
                bound_args.append(defaults[defcount - (fn_argcount - i)])
            else:
                # we didn't find an arg so break and error out later
                break
        else:
            # We found a kwarg for it, but if there are regular args, we got
            # the same argument twice, so error out.
            if i < len(args):
                raise TypeError(
                    '%s.%s() got multiple values for keyword argument %r' % (
                        function.__module__,
                        function.__name__,
                        key))

            bound_args.append(value)
        argcount += 1

    # If we didn't get enough arguments, error out
    if argcount + defcount < fn_argcount:
        raise TypeError(
            '%s.%s() takes %s %d %spositional argument%s (%d given)' % (
                function.__module__,
                function.__name__,
                'at least' if varargs or defaults else 'exactly',
                fn_totalargs,
                '' if fn_kwargcount else 'non-keyword ',
                '' if fn_totalargs == 1 else 's',
                len(args)))

    # If we take varargs, add them to the vararg name
    if varargs and argcount < len(args):
        bound_args.extend(args[argcount:])

    # Now, add the function kwargs
    for key in fn_kwargs:
        try:
            bound_kwargs[key] = kwargs.pop(key)
        except KeyError:
            # If no kwarg was specified, so see if there's a default argument
            try:
                bound_kwargs[key] = spec.kwonlydefaults[key]
            except KeyError:
                # None found, so error out
                raise TypeError(
                    '%s.%s() needs keyword-only argument %s' %
                    (function.__module__, function.__name__, key))

    # If we take varkw, add them now
    if varkw:
        bound_kwargs.update(kwargs)
    else:
        for key, value in kwargs.items():
            # if the key isn't in the fn_args, it's unknown
            if key not in fn_args:
                raise TypeError(
                    '%s() got an unexpected keyword argument %r' %
                    (function.__name__, key))

            bound_args[key] = value

    return tuple(bound_args), bound_kwargs

# ------------------------------------------------------------------------------

def bind_args(function, args, kwargs):
    """
    Bind a function and all of it's arguments to the named values. This helps
    with annotations from arbitrary functions.

    >>> def foo(a, b, c=1, *args, d, e=2, **kwargs): pass
    >>> bind_args(foo, (1, 2), {'d': 3}) == {
    ...     'a': 1, 'b': 2, 'c': 1, 'd': 3, 'e': 2,
    ...     'args': (), 'kwargs': {}}
    True

    >>> bind_args(foo, (1, 2, 3, 4, 5), {'d': 6, 'e': 7, 'f': 8}) == {
    ...     'a': 1, 'b': 2, 'c': 3, 'd': 6, 'e': 7,
    ...     'args': (4, 5), 'kwargs': {'f': 8}}
    True
    """
    args, kwargs = normalize_args(function, args, kwargs)
    spec = inspect.getfullargspec(function)
    fn_args = spec.args

    if inspect.ismethod(function):
        fn_args = fn_args[1:]

    bound = {}
    arg_iterator = iter(args)
    for key, value in zip(fn_args, arg_iterator):
        bound[key] = value

    if spec.varargs is not None:
        bound[spec.varargs] = tuple(arg_iterator)

    for key in spec.kwonlyargs:
        bound[key] = kwargs.pop(key)

    if spec.varkw is not None:
        bound[spec.varkw] = kwargs

    return bound

# ------------------------------------------------------------------------------

class descriptor(metaclass=abc.ABCMeta):
    '''
    Create a abstract base class that describes a descriptor, and will
    automatically adapt the descriptor to copy the function attributes.

    @param function: a function, method, or callable object
    '''

    def __init__(self, function):
        # Set default values for the function wrapper arguments
        for attr in functools.WRAPPER_ASSIGNMENTS:
            if not hasattr(self, attr):
                setattr(self, attr, '')

        # Only assign members that exist on the function
        assigned = (a for a in functools.WRAPPER_ASSIGNMENTS
            if hasattr(function, a))

        functools.update_wrapper(self, function, assigned=assigned)
        self.function = function

    @abc.abstractmethod
    def __get__(self, instance, owner): pass

# ------------------------------------------------------------------------------

class decorator(descriptor, metaclass=abc.ABCMeta):
    '''
    Create an abstract base class for a decorator that also can also adapt a
    method appropriately.
    '''

    def __get__(self, instance, owner):
        if instance is None:
            return self.__call__
        return types.MethodType(self, instance)

    @abc.abstractmethod
    def __call__(self): pass
