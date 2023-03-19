" GVim Settings
if has("gui_running")
    set guioptions-=M    " Don't load Menu bar
    colorscheme desert
    set guifont=Monospace\ 11
else
    colorscheme desert256
endif

" Load each specialized settings file
source ~/.vim/startup/functions.vim
source ~/.vim/startup/settings.vim
source ~/.vim/startup/plugins.vim
source ~/.vim/startup/mappings.vim
