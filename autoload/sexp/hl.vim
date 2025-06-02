" FIXME: Consolidate this.
let s:bracket = '\v\(|\)|\[|\]|\{|\}'
let s:delimiter = s:bracket . '|\s'

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
        if capture =~ patt
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
                    \ : a:rgn == 'ignored'
                        \ ? '\vstring|str_lit|regex|pattern|comment|character'
                        \ : a:rgn == 'ignored_no_char'
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
    while line <= maxline && line >= 1
        " Loop over bytes on line.
        let eol = col([line, '$'])
        while col < eol && col >= 1
            if s:is_rgn_type_legacy(a:rgn, line, col)
                "call s:Dbg("%d,%d: is_comment", line, col)
                let [termline, termcol] = [line, col]
            else
                "call s:Dbg("%d,%d: not comment!", line, col)
                break
            endif
            " Note: Don't worry about redundant iterations in multi-byte chars.
            let col += a:dir ? 1 : -1
        endwhile
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

" Rationale: 'ignored_no_char' is a bit of a kludge and also a misnomer: should be
" ignored_minus_character.
fu! sexp#hl#is_rgn_type(rgn, line, col)
    if !s:prefer_treesitter()
        " Try legacy syntax first (either due to preference or because it's all we have).
        let match = s:is_rgn_type_legacy(a:rgn, a:line, a:col)
        if !match && has('nvim')
            " No legacy syntax regions found, but treesitter is available, so try it.
            let match = s:is_rgn_type_ts(a:rgn, a:line, a:col)
        endif
    else
        " We definitely have nvim.
        " Try treesitter first, falling back to legacy iff no treesitter captures found.
        " Design Decision: is_rgn_type() distinguishes between empty captures list ([])
        " and no captures list (v:null) (e.g., because buffer is unparsed), but the logic
        " here intentionally makes no distinction, catching both cases with empty().
        " Rationale: Falling back to legacy syntax is not harmful and probably safest.
        let match = s:is_rgn_type_ts(a:rgn, a:line, a:col)
        if !match && !empty(&syntax)
            let match = s:is_rgn_type_legacy(a:rgn, a:line, a:col)
        endif
    endif
    return match
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
    elseif char =~# s:delimiter && !sexp#hl#is_rgn_type('ignored', a:line, a:col)
        return 0
    else
        return !sexp#hl#is_rgn_type('ignored_no_char', a:line, a:col)
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
