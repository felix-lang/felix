"""This module extends the C++ 2003 iterator header with common extensions."""

import fbuild.config.cxx as cxx
import fbuild.config.cxx.cxx03 as cxx03

# ------------------------------------------------------------------------------

class iterator(cxx03.iterator):
    bidirectional_iterator = cxx.template_test(test_types=['int', 'int'])
    forward_iterator = cxx.template_test(test_types=['int', 'int'])
