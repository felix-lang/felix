" Vim syntax file
" Language:     Felix
" Maintainer:   <John Max Skaller> skaller@users.sourceforge.net
" Last Change:  2010 Dec 25

" This vim file is adapted from the c.vim file
" for use with Felix .. it doesn't quite work,
" but is better than nothing usually ..

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match felixKeyChar          "|"
syn match felixKeyChar          "|[^\]]"me=e-1
syn match felixKeyChar          ";"
syn match felixKeyChar          "\~"
syn match felixKeyChar          "?"
syn match felixKeyChar          "\*"
syn match felixKeyChar          "->"
syn match felixKeyChar          "=>"
syn match felixKeyChar          "<-"
syn match felixKeyChar          "<="
syn match felixKeyChar          "="
syn match felixKeyChar          ":="

" error
syn match felixCommentErr       "\*\\"
syn match felixBraceErr         "}"
syn match felixBrackErr         "\]"
syn match felixParenErr         ")"
syn match felixArrErr           "|]"
syn match felixDoneErr          "\<done\>"
syn match felixThenErr          "\<then\>"
syn match felixEndmatchErr      "\<endmatch\>"
syn match felixUptoErr          "\<upto\>"
syn match felixDowntoErr        "\<downto\>"

syn cluster felixAllErrs contains=felixBraceErr,felixBrackErr,felixParenErr,felixArrErr,felixDoneErr,felixThenErr,felixEndmatchErr,felixUptoErr,felixDowntoErr

" script headers highlighted like comments
syn match felixComment "^#!.*"

" C/C++ keywords which are not Felix ones (bad style)
syn keyword felixError break
syn keyword felixError continue
syn keyword felixError repeat

syn keyword felixStatement end
syn keyword felixStatement catch 
syn keyword felixStatement try
syn keyword felixStatement endtry
syn keyword felixStatement assert
syn keyword felixStatement axiom
syn keyword felixStatement body
syn keyword felixStatement break 
syn keyword felixStatement call
syn keyword felixStatement case
syn keyword felixStatement proj
syn keyword felixStatement caseno
syn keyword felixStatement casearg
syn keyword felixStatement cfun
syn keyword felixStatement class
syn keyword felixStatement comment
syn keyword felixStatement continue
syn keyword felixStatement const
syn keyword felixStatement cproc
syn keyword felixStatement cstruct
syn keyword felixStatement ctor
syn keyword felixStatement subtype
syn keyword felixStatement supertype
syn keyword felixStatement ctypes
syn keyword felixStatement def
syn keyword felixStatement do
syn keyword felixStatement done
syn keyword felixStatement begin
syn keyword felixStatement elif
syn keyword felixStatement else
syn keyword felixStatement otherwise
syn keyword felixStatement endcase
syn keyword felixStatement endif
syn keyword felixStatement chainmatch
syn keyword felixStatement ormatch
syn keyword felixStatement endmatch
syn keyword felixStatement enum
syn keyword felixStatement cenum
syn keyword felixStatement cflags
syn keyword felixStatement expect
syn keyword felixStatement extern
syn keyword felixStatement for
syn keyword felixStatement forget
syn keyword felixStatement fork
syn keyword felixStatement functor
syn keyword felixStatement fun
syn keyword felixStatement gen
syn keyword felixStatement goto
syn keyword felixStatement halt
syn keyword felixStatement header
syn keyword felixStatement ident
syn keyword felixStatement include
syn keyword felixStatement incomplete
syn keyword felixStatement inf
syn keyword felixStatement in
syn keyword felixStatement instance
syn keyword felixStatement is
syn keyword felixStatement inherit
syn keyword felixStatement inline
syn keyword felixStatement jump
syn keyword felixStatement lemma
syn keyword felixStatement library
syn keyword felixStatement let
syn keyword felixStatement loop
syn keyword felixStatement lval
syn keyword felixStatement macro
syn keyword felixStatement module
syn keyword felixStatement namespace
syn keyword felixStatement NaN
syn keyword felixStatement new
syn keyword felixStatement noinline
syn keyword felixStatement nonterm
syn keyword felixStatement noreturn
syn keyword felixStatement not
syn keyword felixStatement package
syn keyword felixStatement pod
syn keyword felixStatement private
syn keyword felixStatement proc
syn keyword felixStatement property
syn keyword felixStatement reduce
syn keyword felixStatement ref
syn keyword felixStatement rename
syn keyword felixStatement requires
syn keyword felixStatement return
syn keyword felixStatement from
syn keyword felixStatement SCHEME
syn keyword felixStatement syntax
syn keyword felixStatement literal
syn keyword felixStatement spawn_fthread
syn keyword felixStatement spawn_pthread
syn keyword felixStatement static
syn keyword felixStatement struct
syn keyword felixStatement then
syn keyword felixStatement todo
syn keyword felixStatement to
syn keyword felixStatement typedef
syn keyword felixStatement type
syn keyword felixStatement union
syn keyword felixStatement use
syn keyword felixStatement val
syn keyword felixStatement var
syn keyword felixStatement once
syn keyword felixStatement virtual
syn keyword felixStatement where
syn keyword felixStatement when
syn keyword felixStatement with
syn keyword felixStatement yield
syn keyword felixStatement uncopyable
syn keyword felixStatement _gc_pointer
syn keyword felixStatement _gc_type
syn keyword felixStatement _svc
syn keyword felixStatement _deref
syn keyword felixStatement and
syn keyword felixStatement implies 
syn keyword felixStatement as
syn keyword felixStatement callback
syn keyword felixStatement code
syn keyword felixStatement false
syn keyword felixStatement if
syn keyword felixStatement isin
syn keyword felixStatement match
syn keyword felixStatement noexpand
syn keyword felixStatement of
syn keyword felixStatement or
syn keyword felixStatement the
syn keyword felixStatement true
syn keyword felixStatement typematch
syn keyword felixStatement typecase
syn keyword felixStatement chip
syn keyword felixStatement device
syn keyword felixStatement connect
syn keyword felixStatement connector
syn keyword felixStatement circuit
syn keyword felixStatement endcircuit
syn keyword felixStatement wire
syn keyword felixStatement _
" all the felix keywords ..
" directives
syn keyword felixDefine body
syn keyword felixDefine comment
syn keyword felixDefine export
syn keyword felixDefine header
syn keyword felixDefine finaliser encoder decoder scanner
syn keyword felixDefine include inherit inline
syn keyword felixDefine macro
syn keyword felixDefine noinline
syn keyword felixDefine open
syn keyword felixDefine package pod private publish
syn keyword felixDefine reduce rename
syn keyword felixDefine todo
syn keyword felixDefine use

" actually this is a statement, but the directive colour seems best..
syn keyword felixDefine inherit
" special identifiers
syn keyword felixDefine this
syn keyword felixDefine root
syn keyword felixDefine self

" statement keywords
syn keyword felixStatement all as attempt
syn keyword felixStatement call case caseno code const class ctypes
syn keyword felixStatement def
syn keyword felixStatement endattempt except exceptions 
syn keyword felixStatement finally fork functor fun cfun
syn keyword felixStatement goto
syn keyword felixStatement interface inf method object extends implements extend
syn keyword felixStatement jump
syn keyword felixStatement lambda loop
syn keyword felixStatement namespace module
syn keyword felixStatement NaN
syn keyword felixStatement obj of
syn keyword felixStatement proc cproc
syn keyword felixStatement raise ref regexp return regdef parse parser
syn keyword felixStatement struct
syn keyword felixStatement to type typedef typeof
syn keyword felixStatement union
syn keyword felixStatement when with
syn keyword felixStatement _

" operators
syn keyword felixOperator and not or xor

" Enclosing delimiters
syn region felixEncl transparent matchgroup=felixKeyword start="(" matchgroup=felixKeyword end=")"
syn region felixEncl transparent matchgroup=felixKeyword start="{" matchgroup=felixKeyword end="}"
syn region felixEncl transparent matchgroup=felixKeyword start="\[" matchgroup=felixKeyword end="\]"
syn region felixEncl transparent matchgroup=felixKeyword start="\[|" matchgroup=felixKeyword end="|\]"

" conditionals
syn keyword felixConditional if
syn keyword felixConditional then
syn keyword felixConditional else
syn keyword felixConditional elif
syn keyword felixConditional endif

syn keyword felixConditional match
syn keyword felixConditional typematch
syn keyword felixConditional regmatch
syn keyword felixConditional endmatch

syn keyword felixConditional typecase
syn keyword felixConditional endcase

" repeating constructs
syn keyword felixRepeat for 
syn keyword felixRepeat do done downto
syn keyword felixRepeat until upto
syn keyword felixRepeat while 
syn keyword felixRepeat next 
syn keyword felixRepeat perform

" standard library types
syn keyword felixType unit
syn keyword felixType void
syn keyword felixType bool
syn keyword felixType any

syn keyword felixType byte
syn keyword felixType address
syn keyword felixType offset

syn keyword felixType tiny
syn keyword felixType short
syn keyword felixType int
syn keyword felixType long
syn keyword felixType vlong

syn keyword felixType utiny
syn keyword felixType ushort
syn keyword felixType uint
syn keyword felixType ulong
syn keyword felixType uvlong

syn keyword felixType int8
syn keyword felixType int16
syn keyword felixType int32
syn keyword felixType int64

syn keyword felixType uint8
syn keyword felixType uint16
syn keyword felixType uint32
syn keyword felixType uint64

syn keyword felixType size
syn keyword felixType ssize
syn keyword felixType intptr
syn keyword felixType uintptr
syn keyword felixType intmax 
syn keyword felixType uintmax 
syn keyword felixType ptrdiff
syn keyword felixType offset

syn keyword felixType float
syn keyword felixType double
syn keyword felixType ldouble

syn keyword felixType char
syn keyword felixType wchar
syn keyword felixType uchar

syn keyword felixType string
syn keyword felixType ustring

syn keyword felixType cont

syn keyword felixType pin

syn keyword felixConstant true false


" standard library types
syn keyword felixType array
syn keyword felixType varray
syn keyword felixType darray
syn keyword felixType sarray
syn keyword felixType bsarray
syn keyword felixType carray
syn keyword felixType list

" strings -- make special exception to handle identifiers with quotes in them
syn region felixString          start=+\(^\|[^0-9A-Za-z_']\)\@<=\(\|[uU]\|[cC]\|[fF]\|[qQ]\|[wW]\|[uU]\|[kK]\|[nN]\)'+ end=+'+ skip=+\\\\\|\\'+ contains=felixEscape
syn region felixString          start=+\(^\|[^0-9A-Za-z_"]\)\@<=\(\|[uU]\|[cC]\|[fF]\|[qQ]\|[wW]\|[uU]\|[kK]\|[nN]\)"+ end=+"+ skip=+\\\\\|\\"+ contains=felixEscape
syn region felixString          start=+\(^\|[^0-9A-Za-z_"]\)\@<=\(\|[uU]\|[cC]\|[fF]\|[qQ]\|[wW]\|[uU]\|[kK]\|[nN]\)"""+ end=+"""+ contains=felixEscape
syn region felixString          start=+\(^\|[^0-9A-Za-z_']\)\@<=\(\|[uU]\|[cC]\|[fF]\|[qQ]\|[wW]\|[uU]\|[kK]\|[nN]\)'''+ end=+'''+ contains=felixEscape

syn region felixRawString       start=+\(^\|[^0-9A-Za-z_']\)\@<=\([rR]\|[cC][rR]\|[rR][cC]\)'+ end=+'+ skip=+\\\\\|\\'+
syn region felixRawString       start=+\(^\|[^0-9A-Za-z_"]\)\@<=\([rR]\|[cC][rR]\|[rR][cC]\)"+ end=+"+ skip=+\\\\\|\\"+
syn region felixRawString       start=+\(^\|[^0-9A-Za-z_"]\)\@<=\([rR]\|[cC][rR]\|[rR][cC]\)"""+ end=+"""+
syn region felixRawString       start=+\(^\|[^0-9A-Za-z_']\)\@<=\([rR]\|[cC][rR]\|[rR][cC]\)'''+ end=+'''+

syn match  felixEscape          +\\[abfnrtv'"\\]+ contained
syn match  felixEscape          "\\\o\{1,3}" contained
syn match  felixEscape          "\\x\x\{2}" contained
syn match  felixEscape          "\(\\u\x\{4}\|\\U\x\{8}\)" contained
syn match  felixEscape          "\\$"

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match       felixNumbers    display transparent "\<\d\|\.\d" contains=felixNumber,felixFloat,felixOctalError,felixOctal
" Same, but without octal error (for comments)
syn match       felixNumbersCom display contained transparent "\<\d\|\.\d" contains=felixNumber,felixFloat,felixOctal
syn match       felixNumber     display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match       felixNumber     display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match       felixOctal      display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=felixOctalZero
syn match       felixOctalZero  display contained "\<0"
syn match       felixFloat      display contained "\d\+f"
"floating point number, with dot, optional exponent
syn match       felixFloat      display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match       felixFloat      display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match       felixFloat      display contained "\d\+e[-+]\=\d\+[fl]\=\>"
"hexadecimal floating point number, optional leading digits, with dot, with exponent
syn match       felixFloat      display contained "0x\x*\.\x\+p[-+]\=\d\+[fl]\=\>"
"hexadecimal floating point number, with leading digits, optional dot, with exponent
syn match       felixFloat      display contained "0x\x\+\.\=p[-+]\=\d\+[fl]\=\>"


" flag an octal number with wrong digits
syn match       felixOctalError display contained "0\o*[89]\d*"
syn case match

" numbers -- skip weird error checks for now .. too hard
syn case ignore
syn match       felixNumbers    display transparent "\<\d" contains=felixNumber
syn match       felixNumber     display contained "\(0d_\=\)\=\(\d\+_\)*\d\+\(u\=\(t\|s\|l\|v\|ll\|z\)\=u\=\)\>"
syn match       felixNumber     display contained "\(\d\+_\)*\d\+[.]\(\d+_\)*\d\+\(e[+-]\=\d\+\)\=\>"
syn match       felixNumber     display contained "0x_\=\(\x\+_\)*\(\x\)\+u\=\(t\|s\|l\|v\|ll\|z\)\=u\=\>"
syn match       felixNumber     display contained "0o_\=\(\o\+_\)*\(\o\)\+u\=\(t\|s\|l\|v\|ll\|z\)\=u\=\>"
syn match       felixNumber     display contained "0b_\=\([01]\+_\)*\([01]\)\+u\=\(t\|s\|l\|v\|ll\|z\)\=u\=\>"
syn case match

" comments
syn region      fdocCommand     start="^@" end="$" contained 
"syn region      fdocComment     start="^@" end="^@tangle " contains=fdocCommand keepend
" syn region      fdocComment     start="^@" end="^@felix" contains=fdocCommand keepend
syn region      fdocComment     start="^@\%(\%(tangle \|felix\)\)\@!" end="^@\%(tangle \|felix\)" contains=fdocCommand keepend
syn region      felixComment    start="/\*" end="\*/" contains=felixComment,felixTodo
syn region      felixCommentL   start="//" skip="\\$" end="$" keepend contains=felixComment,felixTodo
syn keyword     felixTodo       contained TODO FIXME XXX NOTE KLUDGE HACK
"
" keep a // comment separately, it terminates a preproc. conditional
syntax match    felixCommentError       display "\*/"

syn region      felixPreCondit  start="^\s*#\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=felixComment,felixNumbers,felixCommentError,felixAllErrs
syn match       felixPreCondit  display "^\s*#\s*\(else\|endif\)\>"


" import/include statements
syn region      felixIncluded   display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match       felixIncluded   display contained "<[^>]*>"
syn match       felixInclude    display "^\s*#\s*include\>\s*["<]" contains=felixIncluded
syn match       felixInclude    display "^\s*#\s*import\>\s*["<]" contains=felixIncluded

" preprocessors
"syn match cLineSkip    "\\$"
syn cluster     felixPreProcGroup       contains=felixPreCondit,felixIncluded,felixInclude,felixDefine,felixUserLabel,felixOctalZero,felixCppSkip,felixNumber,felixFloat,felixOctal,felixOctalError,felixNumbersCom,felixComment,felixCommentL
syn region      felixDefine             start="^\s*#\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ALLBUT,@felixPreProcGroup
syn region      felixPreProc            start="^\s*#\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend contains=ALLBUT,@felixPreProcGroup

" Highlight User Labels
syn cluster     felixMultiGroup contains=felixIncluded,felixCommentSkip,felixCommentString,felixComment2String,@felixCommentGroup,felixUserCont,felixUserLabel,cBitField,felixOctalZero,felixCppSkip,felixNumber,felixFloat,felixOctal,felixOctalError,felixNumbersCom

" Avoid matching foo::bar() in C++ by requiring that the next char is not ':'
syn cluster     felixLabelGroup contains=felixUserLabel
syn match       felixUserCont   display "^\s*\I\i*\s*:>" contains=@felixLabelGroup
syn match       felixUserLabel  display "\I\i*" contained

if exists("c_minlines")
  let b:c_minlines = c_minlines
else
  if !exists("c_no_if0")
    let b:c_minlines = 50       " #if 0 constructs can be long
  else
    let b:c_minlines = 15       " mostly for () constructs
  endif
endif
exec "syn sync ccomment felixComment minlines=" . b:c_minlines

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_flx_syn_inits")
  if version < 508
    let did_flx_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink felixError             Error
  HiLink felixOctalError        Error
  HiLink felixCommentError      Error
  HiLink felixBraceErr          Error
  HiLink felixBrackErr          Error
  HiLink felixParenErr          Error
  HiLink felixArrErr            Error
  HiLink felixOctalZero         Error

  HiLink fdocComment            Comment
  HiLink felixComment           Comment
  HiLink felixCommentL          Comment
  HiLink fdocCommand            Number
  HiLink felixLabel             Label
  HiLink felixUserLabel         Label

  HiLink felixConstructor       Constant

  HiLink felixConditional       Keyword
  HiLink felixRepeat            Keyword
  HiLink felixKeyChar           Keyword
  HiLink felixOperator          Keyword
  HiLink felixKeyword           Keyword

  HiLink felixNumber            Number
  HiLink felixOctal             Number
  HiLink felixFloat             Float
  HiLink felixConstant          Constant
  HiLink felixType              Function

  HiLink felixString            String
  HiLink felixRawString         String
  HiLink felixEscape            Special

  HiLink felixInclude           Include
  HiLink felixPreProc           PreProc
  HiLink felixDefine            Macro
  HiLink felixIncluded          String
  HiLink felixStatement         Statement
  HiLink felixPreCondit         PreCondit
  HiLink felixTodo              Todo
  HiLink felixEncl              Keyword

  delcommand HiLink
endif

let b:current_syntax = "felix"

" vim: ts=8

