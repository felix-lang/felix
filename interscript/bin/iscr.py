#!/usr/bin/env python
import sys
args = sys.argv[1:]
if sys.path[0]!='':
  sys.path = ['']+ sys.path

import interscript
interscript.run_from_options(args)

