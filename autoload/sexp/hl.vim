" FIXME: Consolidate this.
let s:bracket = '\v\(|\)|\[|\]|\{|\}'
let s:delimiter = s:bracket . '|\s'

fu! s:Dbg(...)
    call luaeval("require'dp':get'sexp':logf(unpack(_A))", a:000)
endfu

" Function that returns list of syntax groups at the specified position.
" Note: Version 7.2.446 introduced synstack(), which returns the entire stack
" of syntax groups for a given position, as well as the syntax groups of the
" position under the cursor, even if on a blank line (unlike synID(), which
" returns 0 on a blank line).
"
" Using synstack() solves the problem of "contained" syntax groups. For example,
" a syntax file or colorscheme may define custom groups like todo items or
" trailing whitespace in a comment. A test using synID() alone would fail to
" recognize a comment or string if the tested position happened to be inside one
" of these contained groups. Thus, we use synstack() if it exists, falling
" back to synID(), with the caveat that it will find no regions on a blank
" line.
function! s:get_syntax_groups(line, col)
    if exists('*synstack')
        let stack = synstack(a:line, a:col)
        return map(stack, 'synIDattr(v:val, "name")')
    else
        let id = synIDattr(synID(a:line, a:col, 0), 'name')
        " Return sole element as list.
        return id ? [synIDattr(synID(a:line, a:col, 0), 'name')] : []
    endif
endfunction

fu! s:check_syntax(patt, line, col)
    let captures = s:get_syntax_groups(a:line, a:col)
    if empty(captures)
        return v:null
    end
    for capture in captures
        if capture =~ a:patt
            return 1
        endif
    endfor
    return 0
endfu

fu! s:prefer_treesitter()
    return (!g:sexp_prefer_legacy_syntax || empty(&syntax)) && has('nvim')
endfu

" This is for legacy syntax.
fu! s:get_rgn_patt(rgn)
    return a:rgn == 'string'
                \ ? '\vstring|str_lit|regex|pattern'
                \ : a:rgn == 'comment'
                    \ ? 'comment'
                    \ : a:rgn == 'str_com_chr'
                        \ ? '\vstring|str_lit|regex|pattern|comment|character'
                        \ : a:rgn == 'str_com'
                            \ ? '\vstring|str_lit|regex|pattern|comment'
                            \ : ''
endfu

fu! s:current_region_terminal_ts(rgn, dir)
    return luaeval(
                \ "require'sexp.ts'.current_region_terminal(_A[1], _A[2])",
                \ [a:rgn, a:dir])
endfu

" Return terminal ([1,1] indexing) of rgn at cursor, null pos if rgn not at cursor.
fu! s:current_region_terminal_legacy(rgn, dir)
    " Need to find end of region in direction indicated by end
    let [_, line, col, _] = getpos('.')
    " Assumption: Caller has verified currently in region.
    let [termline, termcol] = [0, 0]
    let maxline = line('$')
    let in_rgn = 1
    while in_rgn && line <= maxline && line >= 1
        " Loop over bytes on line.
        let eol = col([line, '$'])
        while col < eol && col >= 1
            if s:is_rgn_type_legacy(a:rgn, line, col)
                "call s:Dbg("%d,%d: is_comment", line, col)
                let [termline, termcol] = [line, col]
            else
                "call s:Dbg("%d,%d: not comment!", line, col)
                let in_rgn = 0
                break
            endif
            " Note: Don't worry about redundant iterations in multi-byte chars.
            let col += a:dir ? 1 : -1
        endwhile
        if !in_rgn | break | endif
        let line += a:dir ? 1 : -1
    endwhile
    return [0, termline, termcol, 0]
endfu

fu! sexp#hl#current_region_terminal(rgn, end)
    if s:prefer_treesitter()
        return s:current_region_terminal_ts(a:rgn, a:end)
    else
        return s:current_region_terminal_legacy(a:rgn, a:end)
    endif
endfu

fu! s:is_rgn_type_ts(rgn, line, col)
    return luaeval(
                \ "require'sexp.ts'.is_rgn_type(_A[1], _A[2], _A[3])",
                \ [a:rgn, a:line, a:col])
endfu

fu! s:is_rgn_type_legacy(rgn, line, col)
    " Note: Eventually, may need to use different patterns for treesitter vs syntax and
    " for the various lisp dialects. However, until the original, simple, test is proven
    " ineffective, just use it.
    let patt = s:get_rgn_patt(a:rgn)
    if empty(patt)
        echoerr "Internal error! Empty pattern in s:is_rgn_type_legacy"
        return 0
    endif
    return s:check_syntax(patt, a:line, a:col)
endfu

" Rationale: 'str_com' is a bit of a kludge and also a misnomer: should be
" ignored_minus_character.
fu! sexp#hl#is_rgn_type(rgn, line, col)
    if s:prefer_treesitter()
        " We definitely have nvim.
        " Try treesitter first, falling back to legacy iff no treesitter tree.
        let match = s:is_rgn_type_ts(a:rgn, a:line, a:col)
        call s:Dbg("match=%s rgn=%s line=%d col=%d", string(match), a:rgn, a:line, a:col)
        if match != v:null
            return match
        end
    end
    " Arrival here means we won't or can't use treesitter.
    " Try legacy syntax first (either due to preference or because it's all we have).
    if empty(&syntax) && !get(b:, 'sexp_did_warn_no_syntax', 0)
        let b:sexp_did_warn_no_syntax = 1
        " TODO: Spruce this up...
        echoerr "vim-sexp: Warning: No syntax available" 
        return
    endif
    return s:is_rgn_type_legacy(a:rgn, a:line, a:col)
endfu

fu! s:current_atom_terminal_ts(dir)
    return luaeval(
                \ "require'sexp.ts'.current_atom_terminal(_A[1])", [a:dir])
endfu

fu! s:current_atom_terminal_legacy(end)
    if !s:is_atom(cursorline, cursorcol)
        return [0, 0, 0, 0]
    endif

    let termline = cursorline
    let termcol = cursorcol

    " FIXME: Don't use s:findpos.
    while 1
        let [line, col] = s:findpos('\v.', a:end, cursorline)

        if line < 1 | break | endif

        if s:is_atom(line, col)
            let termline = line
            let termcol = col
            call cursor(line, col)
        else
            break
        endif
    endwhile
endfu

" Returns 1 if character at position is an atom.
"
" An atom is defined as:
"
"   * A contiguous region of non-whitespace, non-bracket characters that are
"     not part of a string or comment.
"
function! sexp#hl#is_atom(line, col)
    let char = getline(a:line)[a:col - 1]

    if empty(char)
        return 0
    elseif char =~# s:delimiter && !sexp#hl#is_rgn_type('str_com_chr', a:line, a:col)
        return 0
    else
        return !sexp#hl#is_rgn_type('str_com', a:line, a:col)
    endif
endfunction

" Position of start/end of current atom: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in an atom. Assumes atoms never span multiple
" lines.
function! sexp#hl#current_atom_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')

    let [termline, termcol] = [cursorline, cursorcol]
    if s:prefer_treesitter()
        let ret = s:current_atom_terminal_ts(a:end)
    else
        let ret = s:current_atom_terminal_legacy(a:end)
    endif

    " TODO: Save/restore probably won't be needed after I refactor the legacy version.
    call cursor(cursorline, cursorcol)
    return ret
endfunction

" vim:ts=4:sw=4:et
