" Syntax file
" Language: Celestia Star Catalogs
" Maintainer: Kevin Lauder
" Latest Revision: 26 April 2008

if exists("b:current_syntax")
  finish
endif

" syn keyword "=" "=>" "->" case nextgroup=syntaxElement2

syn match kw "^ *=>\|="

"hi def link celTodo        Todo
"hi def link celComment     Comment
hi def link kw              Statement
"hi def link celHip         Type
"hi def link celString      Constant
"hi def link celDesc        PreProc
"hi def link celNumber      Constant

let b:current_syntax = "m5"
