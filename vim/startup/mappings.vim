"" ============================================================================
""                             All Mode Mappings
"" ============================================================================
" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee % > /dev/null

"" ============================================================================
""                        Normal/Visual Mode Mappings
"" ============================================================================
" If hl search is off, starting a new search or moving enables it
nnoremap * :set hlsearch<CR>*
nnoremap # :set hlsearch<CR>#
nnoremap n :set hlsearch<CR>n
nnoremap N :set hlsearch<CR>N
nnoremap / :set hlsearch<CR>/
nnoremap ? :set hlsearch<CR>?

"" ============================================================================
""                         Leader Mappings (Sorted)
"" ============================================================================
" Also use spacebar as a leader
nmap <Space> \

nnoremap <Leader>ev :e $HOME/dotfiles/nvim/init.lua<CR>
nnoremap <Leader>cx :!chmod a+x %<CR>
nnoremap <Leader>h :set hlsearch! hlsearch?<CR>
nnoremap <Leader>rd :redraw!<CR>
nnoremap <Leader>se :sp<CR>:e %:h<CR>
nnoremap <Leader>ss :call SortSection()<CR>
nnoremap <Leader>te :tabe %:h<CR>
nnoremap <Leader>tm :Tabmerge right<CR>
nnoremap <Leader>ve :vsp<CR>:e %:h<CR>
nnoremap <Leader>w :call StripTabsAndTrailingWhitespaces()<CR>:w<CR>

" Note - l and q are used for the location list and quickfix toggle by ListToggle
