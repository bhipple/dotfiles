call plug#begin('~/.vim/plugged')

Plug 'Raimondi/delimitMate'                              " Matching quotes, parens, and brackets
Plug 'Valloric/ListToggle'                               " Toggling quickfix and location list
Plug 'bhipple/vimux'                                     " Vim and Tmux Integration
Plug 'christoomey/vim-tmux-navigator'                    " Window/Pane switching with Vim and Tmux
Plug 'godlygeek/tabular'                                 " Align blocks of text
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
" Netrw
let g:netrw_sort_by = 'name'

" ListToggle
let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'

" Vimux
let g:VimuxOrientation = "h"
let g:VimuxHeight = "35"
