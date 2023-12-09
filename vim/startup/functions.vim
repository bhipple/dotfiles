"" ============================================================================
""                                Formatting
"" ============================================================================
" Sort all lines until a blank is encountered
" or a line with a different prefix from the left position
" of the cursor
function! SortSection()
    let startLine = line('.')
    let curLine = startLine
    let ignorePrefix = 1
    if(col('.') > 1)
        let ignorePrefix = 0
    endif
    let prefix = GetPrefix()
    let prefixLn = max([1, strlen(prefix) - 1])

    "echo "prefixLn = " . prefixLn
    "echo "prefix = " . prefix
    "echo "ignorePrefix = " . ignorePrefix
    while(getline(curLine+1) != "" && (ignorePrefix || getline(curLine+1)[0:prefixLn] ==# prefix))
        let curLine += 1
        "echo "Incremented curLine"
    endwhile
    "echo "Broke on " . getline(curLine+1)[0:prefixLn]

    call setline(startLine, sort(getline(startLine, curLine)))
endfunction

" Get the line from position 0 to the character right before the cursor
" Note that cursor columns are 1-based, while string indexes are 0-based
function! GetPrefix()
    let curCol = col('.')
    if(curCol > 1)
        return getline('.')[0:curCol-2]
    endif
    return ""
endfunction

function! StripTabsAndTrailingWhitespaces()
  let _s=@/
  retab
  %s///ge
  %s/\s\+$//e
  let @/=_s
  exec "normal ``"
endfunction
