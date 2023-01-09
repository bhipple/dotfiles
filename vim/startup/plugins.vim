call plug#begin('~/.vim/plugged')

Plug 'Raimondi/delimitMate'                              " Matching quotes, parens, and brackets
Plug 'Valloric/ListToggle'                               " Toggling quickfix and location list
Plug 'bhipple/vimux'                                     " Vim and Tmux Integration
Plug 'christoomey/vim-tmux-navigator'                    " Window/Pane switching with Vim and Tmux
Plug 'godlygeek/tabular'                                 " Align blocks of text
Plug 'luochen1990/rainbow'                               " Rainbow parenthesis coloring
Plug 'mhinz/vim-grepper'                                 " Asynchronous Grep -> QuickFix List
Plug 'tommcdo/vim-exchange'                              " cx operator for exchanging text regions
Plug 'tpope/vim-abolish'                                 " Coercion and Subvert
Plug 'tpope/vim-commentary'                              " Comment/uncomment operator
Plug 'tpope/vim-fugitive'                                " Git Wrapper
Plug 'tpope/vim-repeat'                                  " Dot operator for plugins
Plug 'tpope/vim-surround'                                " Surrounding text
Plug 'tpope/vim-tbone'                                   " vim and tmux mappings
Plug 'tpope/vim-unimpaired'                              " Pairs of keyboard mappings for common tasks
Plug 'tpope/vim-vinegar'                                 " netrw improvement

call plug#end()

"" ============================================================================
""                              Plugin Settings
"" ============================================================================
" Grepper
nmap gs :call Cdroot()<CR><plug>(GrepperOperator)
xmap gs :call Cdroot()<CR><plug>(GrepperOperator)

" Netrw
let g:netrw_sort_by = 'name'

" ListToggle
let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'

" Rainbow coloring
let g:rainbow_active = 1
let g:rainbow_conf = {
\   'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
\   'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
\   'operators': '_,_',
\   'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
\   'separately': {
\       '*': {},
\       'tex': {
\           'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
\       },
\       'lisp': {
\           'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'],
\       },
\       'vim': {
\           'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
\       },
\       'html': {
\           'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
\       },
\       'css': 0,
\   }
\}

" Vimux
let g:VimuxOrientation = "h"
let g:VimuxHeight = "35"
