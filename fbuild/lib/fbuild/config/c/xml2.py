
import fbuild.config.c as c

# ------------------------------------------------------------------------------

class libxml2_libxml_xmlexports_h(c.Test):
    # this fucked up library requires "libxml2" on the include path
    # since it has internal #includes to <libxml/blah.h>
    # xmlexports.h is a leaf file that doesn't do include anything
    # so is suitable for an existence test
    header = c.header_test('libxml2/libxml/xmlexports.h')
