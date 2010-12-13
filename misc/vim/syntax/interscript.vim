" Vim syntax file
" Language:  Interscript
" Maintainer:   Neil Schemenauer <nascheme@acs.ucalgary.ca>
" Last change:  1998 July 17

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax") && b:current_syntax != "interscript"
  finish
endif


syn match   iscrOther /^@.*$/
syn match   iscrHead /^@head.*$/
syn match   iscrHead /^@set_title.*$/
syn match   iscrInclude /^@include.*$/
syn region  iscrSelect start=/^@select.*$/ end=/^@/me=e-1 contains=iscrCodeContainer
syn region  iscrCodeContainer start=/@select.*$/ms=e+1 end=/^@/me=e-1 contained
syn sync    minlines=200

if !exists("did_iscr_syntax_inits")
  let did_iscr_syntax_inits = 1
  hi link iscrHead Keyword
  hi iscrSelect guifg=Magenta
  hi iscrOther guifg=Brown
  hi iscrInclude guifg=DarkGreen
  hi iscrCodeContainer guifg=Blue
endif

let b:current_syntax = "interscript"
