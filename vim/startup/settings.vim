"" ============================================================================
""                                 Settings
"" ============================================================================
set nocompatible

if !has('nvim')
    colorscheme desert256
endif

"" ============================================================================
""                            Editing and Moving
"" ============================================================================
syntax on
set autoindent
set backspace=indent,eol,start

" Backup directory for swp files
set noswapfile
set directory=""

" runtime path search for Ex
set ru

" Fixing tabs
set tabstop=4
set expandtab
set shiftwidth=4

" Allow switching off modified buffers without warning
set hidden

" Autosave before :make and other commands; autoreload when file mod
set autowrite
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

" Number of lines to scroll past when the cursor scrolls off the screen
set scrolloff=2

" What to use for gq
set formatprg=par\ -w80

" Additional words for the spell checker
set spellfile=~/.vim/spell/extra-words.add

"" ============================================================================
""                                Appearances
"" ============================================================================
" Show tab and trailing whitespace characters
set listchars=tab:>-,trail:-
set list!

" Set the folding method
set foldmethod=manual
set foldnestmax=3
set foldminlines=10

"" ============================================================================
""                               Auto Commands
"" ============================================================================
augroup filetypedetect
    au! BufRead,BufNewFile wscript python
augroup END
autocmd BufNewFile,BufRead wscript set ft=python
