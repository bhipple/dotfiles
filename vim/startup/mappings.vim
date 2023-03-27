"" ============================================================================
""                             All Mode Mappings
"" ============================================================================
" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee % > /dev/null

"" ============================================================================
""                           Insert Mode Mappings
"" ============================================================================
" Leaving insert mode with jj or jk
inoremap jj <Esc><Right>
inoremap jk <Esc><Right>


"" ============================================================================
""                        Normal/Visual Mode Mappings
"" ============================================================================
" YouCompleteMe
nnoremap <F9> :YcmForceCompileAndDiagnostics<CR>

" If hl search is off, starting a new search or moving enables it
nnoremap * :set hlsearch<CR>*
nnoremap # :set hlsearch<CR>#
nnoremap n :set hlsearch<CR>n
nnoremap N :set hlsearch<CR>N
nnoremap / :set hlsearch<CR>/
nnoremap ? :set hlsearch<CR>?

" Swap to last buffer
nnoremap <silent> <F8> :b#<CR>

"" ============================================================================
""                         Leader Mappings (Sorted)
"" ============================================================================
" Also use spacebar as a leader
nmap <Space> \

nnoremap <Leader>cdf :call Cdfile()<CR>
nnoremap <Leader>cdr :call Cdroot()<CR>
nnoremap <Leader>cmt :call CmtSection("")<Left><Left>
nnoremap <Leader>cx :!chmod a+x %<CR>
nnoremap <Leader>db :windo diffthis<CR>
nnoremap <Leader>df :Gdiff<CR>
nnoremap <Leader>do :windo diffoff<CR>
nnoremap <Leader>dom :Gdiff origin/master<CR>
nnoremap <Leader>du :diffupdate<CR>
nnoremap <Leader>ev :e $MYVIMRC<CR>G$F/
nnoremap <Leader>ff :FSHere<CR>
nnoremap <Leader>fh :FSSplitLeft<CR>
nnoremap <Leader>fj :FSSplitBelow<CR>
nnoremap <Leader>fk :FSSplitAbove<CR>
nnoremap <Leader>fl :FSSplitRight<CR>
nnoremap <Leader>fx :call GTestFixture("")<Left><Left>
nnoremap <Leader>gg :call Cdroot()<CR>:Grepper<CR>
nnoremap <Leader>h :set hlsearch! hlsearch?<CR>
nnoremap <Leader>rd :redraw!<CR>
nnoremap <Leader>rr :w<CR>:call VimuxRunCommand('./' . bufname("%"))<CR>
nnoremap <Leader>se :sp<CR>:e %:h<CR>
nnoremap <Leader>sp :setlocal spell! spelllang=en_us<CR>
nnoremap <Leader>ss :call SortSection()<CR>
nnoremap <Leader>te :tabe %:h<CR>
nnoremap <Leader>tf :call Cdroot()<CR>:call MkGtest()<CR>
nnoremap <Leader>tm :Tabmerge right<CR>
nnoremap <Leader>vc :w<CR>:call VimuxRunCommand('chmod +x *.hs; ./*.hs < input')<CR>
nnoremap <Leader>ve :vsp<CR>:e %:h<CR>
nnoremap <Leader>vl :w<CR>:call VimuxRunCommand('(load "' . bufname("%") . '")')<CR>
nnoremap <Leader>vr :w<CR>:VimuxRunLastCommand<CR>
nnoremap <Leader>vv :w<CR>:VimuxPromptCommand<CR>
nnoremap <Leader>vx :VimuxInterruptRunner<CR>
nnoremap <Leader>w :call StripTabsAndTrailingWhitespaces()<CR>:w<CR>

" Note - l and q are used for the location list and quickfix toggle by ListToggle
