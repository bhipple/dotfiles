"" ============================================================================
""                                 Settings
"" ============================================================================
set nocompatible

colorscheme desert256

syntax on
set autoindent

" Backup directory for swp files
set noswapfile
set directory=""

" runtime path search for Ex
set ru

" Fixing tabs
set tabstop=4
set expandtab
set shiftwidth=4

" Autoreload when file mod
set autoread

" Ignore whitespace on diffs
set diffopt+=iwhite

" Smart case sensitivity
set ignorecase
set smartcase

" Fix background color
set t_ut=

" When multiple completions are possible, show all
set wildmenu

" Complete only up to point of ambiguity, like the shell does
set wildmode=list:longest

" Ignoring files (see :help wildignore)
set wildignore+=*.o,*.d,00*,nohup.out,tags,.hs-tags,*.hi,*.gcno,*.gcda,*.fasl,*.pyc
