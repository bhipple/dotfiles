call plug#begin('~/.vim/plugged')

Plug 'Glench/Vim-Jinja2-Syntax', { 'for': 'sls' }        " Jinja2 Syntax highlighting
Plug 'LnL7/vim-nix'                                      " Nix Expression support in Vim
Plug 'Raimondi/delimitMate'                              " Matching quotes, parens, and brackets
Plug 'Shougo/denite.nvim'                                " Helm for Vim
Plug 'Shougo/vimproc', { 'do': 'make' }                  " Asynchronous command execution library
Plug 'SirVer/ultisnips'                                  " Text snippets
Plug 'Twinside/vim-hoogle', { 'for': 'haskell' }         " Haskell function information
Plug 'Valloric/ListToggle'                               " Toggling quickfix and location list
Plug 'bhipple/vim-hindent'                               " Haskell code formatter
Plug 'bhipple/vim-snippets'                              " My snippets fork
Plug 'bhipple/vimux'                                     " Vim and Tmux Integration
Plug 'bitc/vim-hdevtools', { 'for': 'haskell' }          " Haskell
Plug 'bling/vim-airline'                                 " Status line
Plug 'chazmcgarvey/vimcoder'                             " Topcoder Vim Plugin
Plug 'christoomey/vim-tmux-navigator'                    " Window/Pane switching with Vim and Tmux
Plug 'derekwyatt/vim-fswitch', { 'for': 'cpp' }          " Fastswitch (cpp/h toggle)
Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }         " Displays types and warings/errors
Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }           " Haskell completion engine
Plug 'elaforge/fast-tags', { 'for': 'haskell' }          " Ctags generation for Haskell
Plug 'godlygeek/tabular'                                 " Align blocks of text
Plug 'ivanov/vim-ipython', { 'for': 'python' }           " Vim + IPython Notebook integration
Plug 'justinmk/vim-syntax-extra'                         " Flex and Bison syntax highlighting
Plug 'kovisoft/slimv'                                    " Lisp in Vim
Plug 'lambdalisue/neovim-prompt'                         " Prompt library for vim plugins
Plug 'lambdalisue/vim-gita'                              " Magit for Vim
Plug 'lukerandall/haskellmode-vim', { 'for': 'haskell' } " Tons of useful things
Plug 'luochen1990/rainbow'                               " Rainbow parenthesis coloring
Plug 'majutsushi/tagbar'                                 " Using for JavaScript
Plug 'mhinz/vim-grepper'                                 " Asynchronous Grep -> QuickFix List
Plug 'rhysd/vim-clang-format', { 'on': 'ClangFormat' }   " Vim wrapper plugin for clang-format
Plug 'saltstack/salt-vim', { 'for': 'sls' }              " YAML + Jinja SLS
Plug 'scrooloose/syntastic'                              " Syntax checking
Plug 'tommcdo/vim-exchange'                              " cx operator for exchanging text regions
Plug 'tomtom/startup_profile_vim'                        " Profile vim startup time
Plug 'tpope/vim-speeddating'                             " Increment and decrement timestamps
Plug 'tpope/vim-abolish'                                 " Coercion and Subvert
Plug 'tpope/vim-commentary'                              " Comment/uncomment operator
Plug 'tpope/vim-dispatch'                                " Asynchronous Makes
Plug 'tpope/vim-fugitive'                                " Git Wrapper
Plug 'tpope/vim-repeat'                                  " Dot operator for plugins
Plug 'tpope/vim-rhubarb'                                 " Vim + Git + Hub
Plug 'tpope/vim-surround'                                " Surrounding text
Plug 'tpope/vim-tbone'                                   " vim and tmux mappings
Plug 'tpope/vim-unimpaired'                              " Pairs of keyboard mappings for common tasks
Plug 'tpope/vim-vinegar'                                 " netrw improvement
Plug 'vim-scripts/DrawIt'                                " Create ASCII drawings in Vim
Plug 'vim-scripts/Tabmerge'                              " Merge tabs into splits

if has('nvim')
    Plug 'haifengkao/nfasd'                                " Recent file autocompletion
    Plug 'neovimhaskell/haskell-vim'                       " Better highlighting and indentation
    Plug 'zenbro/mirror.vim'                               " Mirroring filesystems
endif


call plug#end()

" Include plugins installed with nixpkgs
set rtp+=~/.nix-profile/share/vim-plugins/youcompleteme

"" ============================================================================
""                              Plugin Settings
"" ============================================================================
" Clang-format
let g:clang_format#detect_style_file = 1
let g:clang_format#auto_formatexpr = 1
map <C-T> :ClangFormat<CR>
imap <C-T> <C-o>:ClangFormat<CR>
autocmd FileType c,cpp setlocal textwidth=0

" Denite
" Enter normal mode with jj or jk
call denite#custom#map('insert', 'jj', '<denite:enter_mode:normal>', 'noremap')
call denite#custom#map('insert', 'jk', '<denite:enter_mode:normal>', 'noremap')

" Grepper
nmap gs :call Cdroot()<CR><plug>(GrepperOperator)
xmap gs :call Cdroot()<CR><plug>(GrepperOperator)

" Haskellmode-vim
let g:haddock_browser="/usr/bin/firefox"

" HIndent
let g:hindent_style = "cramer"

" Necoghc
let g:haskellmode_completion_ghc = 0
let g:ycm_semantic_triggers = {'haskell' : ['.']}
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

let g:grepper = {
    \ 'tools':     ['git'],
    \ 'jump':      1,
    \ }

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

" SLIMV
let g:slimv_repl_split=4 " Split Vertically

" Syntastic
let g:syntastic_aggregate_errors = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_always_populate_loc_list = 1

" Tagbar
let g:tagbar_left = 1

" UltiSnips
" Magic to make the <enter> key expand snippes, even with YouCompleteMe installed.
let g:UltiSnipsExpandTrigger = "<nop>"
let g:ulti_expand_or_jump_res = 0
function! ExpandSnippetOrCarriageReturn()
    let snippet = UltiSnips#ExpandSnippetOrJump()
    if g:ulti_expand_or_jump_res > 0
        return snippet
    else
        return "\<CR>"
    endif
endfunction
inoremap <expr> <CR> pumvisible() ? "<C-R>=ExpandSnippetOrCarriageReturn()<CR>" : "\<CR>"

" Vimux
let g:VimuxOrientation = "h"
let g:VimuxHeight = "35"

" YouCompleteMe
let g:ycm_server_log_level = 'debug'
let g:ycm_server_keep_logfiles = 0
let g:ycm_confirm_extra_conf = 0
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_always_populate_location_list = 1
let g:ycm_semantic_triggers = {'haskell' : ['.']}
let g:ycm_seed_identifiers_with_syntax = 1
