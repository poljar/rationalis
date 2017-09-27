" Vim syntax file
" Language: rationalis rules
" Maintainer: Damir Jelić
" Latest Revision: 28 September 2017

if exists("b:current_syntax")
  finish
endif

" Keywords
syn region  rationalisRuleName start="^\s*\[" end="\]"
syn keyword rationalisObjects description comment account currency nextgroup=rationalisMatchVerbs
syn keyword rationalisAdjective payee payer nextgroup=rationalisObjects
syn keyword rationalisMatchVerbs is matches
syn keyword rationalisActionVerbs set
syn region  rationalisArgument start="\"" end="\""

syn match rationalisComment "#.*$"

hi def link rationalisComment       Comment
hi def link rationalisRuleName      Identifier
hi def link rationalisAdjective     Label
hi def link rationalisObjects       Type

hi def link rationalisMatchVerbs    Function
hi def link rationalisActionVerbs   Function
hi def link rationalisArgument      String

set commentstring=#%s
set comments=b:#

let b:current_syntax = "rationalis"
