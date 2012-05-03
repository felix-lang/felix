"""The config system is a simple mechanism in order to model testing similar
tests."""

import fbuild.db

# ------------------------------------------------------------------------------

class _FieldTable(dict):
    """A dictionary that stores Just a dict that records the order of the
    stored items."""

    def __init__(self):
        self.field_names = []

    def __setitem__(self, key, value):
        if key not in self:
            self.field_names.append(key)

        super().__setitem__(key, value)

class TestMeta(fbuild.db.PersistentMeta):
    @classmethod
    def __prepare__(cls, name, bases):
        return _FieldTable()

    def __new__(cls, name, bases, attrs):
        parents = [base for base in bases if isinstance(base, TestMeta)]

        if not parents:
            return super().__new__(cls, name, bases, attrs)

        module = attrs.pop('__module__')
        new_class = super().__new__(cls, name, bases, {'__module__': module})
        new_class.__field_names__ = []

        for parent in parents:
            if hasattr(parent, '__field_names__'):
                for key in parent.__field_names__:
                    if key not in attrs:
                        new_class.__field_names__.append(key)

        # add the fields in the order that they were declared
        for key in attrs.field_names:
            try:
                value = attrs[key]
            except KeyError:
                pass
            else:
                new_class.add_to_class(key, attrs[key])

        return new_class

    def add_to_class(cls, key, value):
        if hasattr(value, 'contribute_to_class'):
            value.contribute_to_class(cls, key)
        else:
            setattr(cls, key, value)

class Test(metaclass=TestMeta):
    def __init__(self, ctx):
        self.ctx = ctx

    @classmethod
    def fields(cls):
        for field_name in cls.__field_names__:
            yield field_name, getattr(cls, field_name)

    def get(self, key, default=None):
        """Look in the test for an attribute named "key". If "key" contains
        any periods, recursively walk down the attributes to find the final
        value. Returns the default value if any of the attributes do not
        exist."""
        obj = self
        for key in key.split('.'):
            obj = getattr(obj, key, None)
            if obj is None:
                return default

        return obj
