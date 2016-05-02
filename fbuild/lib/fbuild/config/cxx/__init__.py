import textwrap

import fbuild
import fbuild.config.c as c
import fbuild.config.c.c90 as c90

# ------------------------------------------------------------------------------

header_test = c.header_test
function_test = c.function_test
macro_test = c.macro_test
type_test = c.type_test
int_type_test = c.int_type_test

class struct_test(c.struct_test):
    def contribute_to_class(self, cls, key):
        # c++ doesn't need "struct" prepended to the typename.
        self.__name__ = key
        if self.name is None:
            self.name = key
        cacheproperty(self).contribute_to_class(cls, key)

variable_test = c.variable_test

# ------------------------------------------------------------------------------

class Template:
    """L{Template} describes the features of a template.  It contains no
    data."""

    def __repr__(self):
        return '%s()' % self.__class__.__name__

    def __eq__(self, other):
        return type(self) is type(other)

    def __hash__(self):
        return hash(self.__class__)

class template_test(c.AbstractFieldDescriptor):
    """template_test is a descriptor that tests for the template on the first
    access. If it exists, an instance of L{Template} is memoized in the object
    and returned. Otherwise, memoize and return None."""

    def __init__(self, *, test_types=[], **kwargs):
        super().__init__(**kwargs)
        self.test_types = test_types

    def format_test(self, header=None):
        return textwrap.dedent('''
            %s

            int main() {
                %s<%s> t;
                return 0;
            }
        ''') % (
            '' if header is None else '#include <%s>' % header,
            self.name,
            ', '.join(self.test_types))

    def process_stdout(self, instance, stdout):
        if self.stdout is None or self.stdout == stdout:
            instance.ctx.logger.passed()
            return Template()

        instance.ctx.logger.failed()

# ------------------------------------------------------------------------------

class TestMeta(c.TestMeta):
    def __new__(cls, name, bases, attrs):
        namespace = attrs.pop('namespace', None)

        new_class = super().__new__(cls, name, bases, attrs)

        if namespace is not None:
            for name, field in new_class.fields():
                # Macros shouldn't get the namespace prepended.
                if not isinstance(field.method, macro_test):
                    field.method.name = namespace + '::' + field.method.name

        return new_class

class Test(c.Test, metaclass=TestMeta):
    namespace = None
