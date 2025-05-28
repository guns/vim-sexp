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

" TODO: Consider making rgn arg accept a list of region types to be logically-OR'ed.
" Rationale: 'ignored_plus_character' is a bit of a kludge.
fu! sexp#hl#is_rgn_type(rgn, line, col)
    " Note: Eventually, may need to use different patterns for treesitter vs syntax and
    " for the various lisp dialects. However, until the original, simple, test is proven
    " ineffective, just use it.
    let patt = a:rgn == 'string'
                \ ? '\vstring|str_lit|regex|pattern'
                \ : a:rgn == 'comment'
                    \ ? 'comment'
                    \ : a:rgn == 'ignored'
                        \ ? '\vstring|str_lit|regex|pattern|comment|character'
                        \ : a:rgn == 'ignored_plus_character'
                            \ ? '\vstring|str_lit|regex|pattern|comment'
                            \ : ''
    if empty(patt)
        echoerr "Internal error! Empty pattern in sexp#hl#is_rgn_type"
        return 0
    endif
    if g:sexp_prefer_legacy_syntax && !empty(&syntax) || !has('nvim')
        " Try legacy syntax first (either due to preference or because it's all we have).
        let match = s:check_syntax(patt, a:line, a:col)
        if !match && has('nvim')
            " No legacy syntax regions found, but treesitter is available, so try it.
            let match = luaeval(
                        \ "require'sexp.ts'.is_rgn_type(_A[1], _A[2], _A[3])", [patt, a:line, a:col])
        endif
    else
        " We definitely have nvim.
        " Try treesitter first, falling back to legacy iff no treesitter captures found.
        " Design Decision: is_rgn_type() distinguishes between empty captures list ([])
        " and no captures list (v:null) (e.g., because buffer is unparsed), but the logic
        " here intentionally makes no distinction, catching both cases with empty().
        " Rationale: Falling back to legacy syntax is not harmful and probably safest.
        let match = luaeval(
                    \ "require'sexp.ts'.is_rgn_type(_A[1], _A[2], _A[3])", [patt, a:line, a:col])
        if !match && !empty(&syntax)
            let syn_match = s:check_syntax(patt, a:line, a:col)
        endif
    endif
    return match
endfu

" vim:ts=4:sw=4:et
