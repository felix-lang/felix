import abc
import functools
import types

# ------------------------------------------------------------------------------

class SRC:
    """An annotation that's used to designate an argument as a source path."""
    @staticmethod
    def convert(src):
        return [src]


class SRCS(SRC):
    """An annotation that's used to designate an argument as a list of source
    paths."""
    @staticmethod
    def convert(srcs):
        return srcs


class DST:
    """An annotation that's used to designate an argument is a destination
    path."""
    @staticmethod
    def convert(dst):
        return [dst]


class DSTS(DST):
    """An annotation that's used to designate an argument is a list of
    destination paths."""
    @staticmethod
    def convert(dsts):
        return dsts


class OPTIONAL_SRC(SRC):
    """An annotation that's used to designate an argument as a source path or
    None."""
    @staticmethod
    def convert(src):
        if src is None:
            return []
        return [src]


class OPTIONAL_DST(DST):
    """An annotation that's used to designate an argument as a destination path
    or None."""
    @staticmethod
    def convert(dst):
        if dst is None:
            return []
        return [dst]

# ------------------------------------------------------------------------------

class PersistentMeta(abc.ABCMeta):
    """A metaclass that searches the db for an already instantiated class with
    the same arguments.  It subclasses from ABCMeta so that subclasses can
    implement abstract methods."""
    def __call_super__(cls, *args, **kwargs):
        return super().__call__(*args, **kwargs)

    def __call__(cls, ctx, *args, **kwargs):
        result, srcs, objs = ctx.db.call(cls.__call_super__, ctx,
            *args, **kwargs)

        return result


class PersistentObject(metaclass=PersistentMeta):
    """An abstract baseclass that will cache instances in the database."""

    def __init__(self, ctx):
        self.ctx = ctx

    def __eq__(self, other):
        if self is other:
            return True

        if not isinstance(other, type(self)):
            return False

        # Step through the members and exit if they aren't equal.
        try:
            for key in self.__dict__:
                if getattr(self, key) != getattr(other, key):
                    return False
        except AttributeError:
            return False
        else:
            return True

# ------------------------------------------------------------------------------

class caches:
    """L{caches} decorates a function and caches the results.  The first
    argument of the function must be an instance of L{database}.

    >>> import fbuild.context
    >>> ctx = fbuild.context.make_default_context(['--database=cache'])
    >>> ctx.db.connect()
    >>> @caches
    ... def test(ctx):
    ...     print('running test')
    ...     return 5
    >>> test(ctx)
    running test
    5
    >>> test(ctx)
    5
    """

    def __init__(self, function):
        functools.update_wrapper(self, function)
        self.function = function

    def __call__(self, *args, **kwargs):
        result, srcs, dsts = self.call(*args, **kwargs)
        return result

    def call(self, ctx, *args, **kwargs):
        return ctx.db.call(self.function, ctx, *args, **kwargs)


class cachemethod:
    """L{cachemethod} decorates a method of a class to cache the results.

    >>> import fbuild.context
    >>> ctx = fbuild.context.make_default_context(['--database=cache'])
    >>> ctx.db.connect()
    >>> class C:
    ...     def __init__(self, ctx):
    ...         self.ctx = ctx
    ...     @cachemethod
    ...     def test(self):
    ...         print('running test')
    ...         return 5
    >>> c = C(ctx)
    >>> c.test()
    running test
    5
    >>> c.test()
    5
    """
    def __init__(self, method):
        self.method = method

    def __get__(self, instance, owner):
        if instance is None:
            return self
        return cachemethod_wrapper(types.MethodType(self.method, instance))


class cachemethod_wrapper:
    def __init__(self, method):
        self.method = method

    def __call__(self, *args, **kwargs):
        result, srcs, dsts = self.call(*args, **kwargs)
        return result

    def call(self, *args, **kwargs):
        return self.method.__self__.ctx.db.call(self.method, *args, **kwargs)


class cacheproperty:
    """L{cacheproperty} acts like a normal I{property} but will memoize the
    result in the store.  The first argument of the function it wraps must be a
    store or a class that has has an attribute named I{store}.

    >>> import fbuild.context
    >>> ctx = fbuild.context.make_default_context(['--database=cache'])
    >>> ctx.db.connect()
    >>> class C:
    ...     def __init__(self, ctx):
    ...         self.ctx = ctx
    ...     @cacheproperty
    ...     def test(self):
    ...         print('running test')
    ...         return 5
    >>> c = C(ctx)
    >>> c.test
    running test
    5
    >>> c.test
    5
    """
    def __init__(self, method):
        self.method = method

    def __get__(self, instance, owner):
        if instance is None:
            return self
        result, srcs, dsts = self.call(instance)
        return result

    def call(self, instance):
        return instance.ctx.db.call(types.MethodType(self.method, instance))
