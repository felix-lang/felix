#! /usr/bin/python2.4
#line 24 "./lpsrc/flx_cpp_tangler.pak"

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
LESS = 17
LPAR = 18
RPAR = 19
LSQB = 20
RSQB = 21
LBRACE = 22
RBRACE = 23
COLON = 24
COMMA = 25
SEMI = 26
STAR = 27
#line 51 "./lpsrc/flx_cpp_tangler.pak"
ASM=30
#line 51 "./lpsrc/flx_cpp_tangler.pak"
AUTO=31
#line 51 "./lpsrc/flx_cpp_tangler.pak"
BOOL=32
#line 51 "./lpsrc/flx_cpp_tangler.pak"
BREAK=33
#line 51 "./lpsrc/flx_cpp_tangler.pak"
CASE=34
#line 51 "./lpsrc/flx_cpp_tangler.pak"
CATCH=35
#line 51 "./lpsrc/flx_cpp_tangler.pak"
CHARCLASS=36
#line 51 "./lpsrc/flx_cpp_tangler.pak"
CONST=37
#line 51 "./lpsrc/flx_cpp_tangler.pak"
CONST_CAST=38
#line 51 "./lpsrc/flx_cpp_tangler.pak"
CONTINUE=39
#line 51 "./lpsrc/flx_cpp_tangler.pak"
DEFAULT=40
#line 51 "./lpsrc/flx_cpp_tangler.pak"
DELETE=41
#line 51 "./lpsrc/flx_cpp_tangler.pak"
DO=42
#line 51 "./lpsrc/flx_cpp_tangler.pak"
DOUBLE=43
#line 51 "./lpsrc/flx_cpp_tangler.pak"
DYNAMIC_CAST=44
#line 51 "./lpsrc/flx_cpp_tangler.pak"
ELSE=45
#line 51 "./lpsrc/flx_cpp_tangler.pak"
ENUM=46
#line 51 "./lpsrc/flx_cpp_tangler.pak"
EXPLICIT=47
#line 51 "./lpsrc/flx_cpp_tangler.pak"
EXTERN=48
#line 51 "./lpsrc/flx_cpp_tangler.pak"
FALSE=49
#line 51 "./lpsrc/flx_cpp_tangler.pak"
FLOAT=50
#line 51 "./lpsrc/flx_cpp_tangler.pak"
FOR=51
#line 51 "./lpsrc/flx_cpp_tangler.pak"
FRIEND=52
#line 51 "./lpsrc/flx_cpp_tangler.pak"
GOTO=53
#line 51 "./lpsrc/flx_cpp_tangler.pak"
IF=54
#line 51 "./lpsrc/flx_cpp_tangler.pak"
INLINE=55
#line 51 "./lpsrc/flx_cpp_tangler.pak"
INT=56
#line 51 "./lpsrc/flx_cpp_tangler.pak"
LONG=57
#line 51 "./lpsrc/flx_cpp_tangler.pak"
MUTABLE=58
#line 51 "./lpsrc/flx_cpp_tangler.pak"
NAMESPACE=59
#line 51 "./lpsrc/flx_cpp_tangler.pak"
NEW=60
#line 51 "./lpsrc/flx_cpp_tangler.pak"
OPERATOR=61
#line 51 "./lpsrc/flx_cpp_tangler.pak"
PRIVATE=62
#line 51 "./lpsrc/flx_cpp_tangler.pak"
PROTECTED=63
#line 51 "./lpsrc/flx_cpp_tangler.pak"
PUBLIC=64
#line 51 "./lpsrc/flx_cpp_tangler.pak"
REGISTER=65
#line 51 "./lpsrc/flx_cpp_tangler.pak"
REINTERPRET_CAST=66
#line 51 "./lpsrc/flx_cpp_tangler.pak"
RETURN=67
#line 51 "./lpsrc/flx_cpp_tangler.pak"
SHORT=68
#line 51 "./lpsrc/flx_cpp_tangler.pak"
SIGNED=69
#line 51 "./lpsrc/flx_cpp_tangler.pak"
SIZEOF=70
#line 51 "./lpsrc/flx_cpp_tangler.pak"
STATIC=71
#line 51 "./lpsrc/flx_cpp_tangler.pak"
STATIC_CAST=72
#line 51 "./lpsrc/flx_cpp_tangler.pak"
STRUCT=73
#line 51 "./lpsrc/flx_cpp_tangler.pak"
SWITCH=74
#line 51 "./lpsrc/flx_cpp_tangler.pak"
TEMPLATE=75
#line 51 "./lpsrc/flx_cpp_tangler.pak"
THIS=76
#line 51 "./lpsrc/flx_cpp_tangler.pak"
THROW=77
#line 51 "./lpsrc/flx_cpp_tangler.pak"
TRUE=78
#line 51 "./lpsrc/flx_cpp_tangler.pak"
TRY=79
#line 51 "./lpsrc/flx_cpp_tangler.pak"
TYPEDEF=80
#line 51 "./lpsrc/flx_cpp_tangler.pak"
TYPEID=81
#line 51 "./lpsrc/flx_cpp_tangler.pak"
TYPENAME=82
#line 51 "./lpsrc/flx_cpp_tangler.pak"
UNION=83
#line 51 "./lpsrc/flx_cpp_tangler.pak"
UNSIGNED=84
#line 51 "./lpsrc/flx_cpp_tangler.pak"
USING=85
#line 51 "./lpsrc/flx_cpp_tangler.pak"
VIRTUAL=86
#line 51 "./lpsrc/flx_cpp_tangler.pak"
VOID=87
#line 51 "./lpsrc/flx_cpp_tangler.pak"
VOLATILE=88
#line 51 "./lpsrc/flx_cpp_tangler.pak"
WCHAR_T=89
#line 51 "./lpsrc/flx_cpp_tangler.pak"
WHILE=90
#line 51 "./lpsrc/flx_cpp_tangler.pak"
N_TOKENS =91

#--end constants--

tok_name = {}
for _name, _value in list(globals().items()):
    if type(_value) is type(0):
        tok_name[_value] = _name


