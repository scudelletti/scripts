set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
" alternatively, pass a path where Vundle should install bundles
"let path = '~/some/path/here'
"call vundle#rc(path)

" let Vundle manage Vundle, required
Bundle 'gmarik/vundle'

" Keep bundle commands between here and filetype plugin indent on.
Bundle 'tpope/vim-endwise'
Bundle 'L9'
Bundle 'FuzzyFinder'

"Color Schemes
Bundle 'sickill/vim-monokai'

filetype plugin indent on     " required
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install (update) bundles
" :BundleSearch(!) foo - search (or refresh cache first) for foo
" :BundleClean(!)      - confirm (or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle commands are not allowed.
" Put your stuff after this line

" Personal Configuration
set cursorline
set cursorcolumn
set number
set list
syntax on
set ts=2 sts=2 sw=2 expandtab
set listchars=tab:▸\ ,eol:¬,trail:.

colorscheme monokai

"Function Calls
autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()

"Functions Declarations
function! <SID>StripTrailingWhitespaces()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
     %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
endfunction

