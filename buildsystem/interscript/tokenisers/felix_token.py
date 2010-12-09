#! /usr/bin/python2.4
#line 24 "lpsrc/flx_felix_tangler.pak"

#--start constants--
ENDMARKER = 0
NAME = 1
NUMBER = 2
STRING = 3
NEWLINE = 4
INDENT = 5
DEDENT = 6
ERRORTOKEN = 7
OP = 8
#line 44 "lpsrc/flx_felix_tangler.pak"
N_TOKENS = 9
#--end constants--

tok_name = {}
for _name, _value in globals().items():
    if type(_value) is type(0):
        tok_name[_value] = _name


