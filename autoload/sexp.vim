
"              o8o
"              '"'
"  oooo    ooooooo ooo. .oo.  .oo.        .oooo.o  .ooooo. oooo    ooooo.ooooo.
"   `88.  .8' `888 `888P"Y88bP"Y88b      d88(  "8 d88' `88b `88b..8P'  888' `88b
"    `88..8'   888  888   888   888 8888 `"Y88b.  888ooo888   Y888'    888   888
"     `888'    888  888   888   888      o.  )88b 888    .o .o8"'88b   888   888
"      `8'    o888oo888o o888o o888o     8""888P' `Y8bod8P'o88'   888o 888bod8P'
"                                                                      888
"                                                                     o888o
"  Author:   guns <self@sungpae.com>
"  License:  MIT
"  Homepage: https://github.com/guns/vim-sexp

if exists('g:sexp_autoloaded')
    finish
endif
let g:sexp_autoloaded = 1

" TODO:
"
" * Don't ignore virtualedit mode?
" * Comments should always be swapped to their own line
" * Ignore non-changing operators when repeating?
" * Remove unnecessary out-of-bounds handling after element-wise movement now
"   that such movement is always bounded

""" PATTERNS AND STATE {{{1

if !exists('g:sexp_maxlines')
    let g:sexp_maxlines = -1 " Use fast best-effort top list search
endif

let s:countindex = 0 " Stores current count index during sexp#docount
let s:bracket = '\v\(|\)|\[|\]|\{|\}'
let s:opening_bracket = '\v\(|\[|\{'
let s:closing_bracket = '\v\)|\]|\}'
let s:delimiter = s:bracket . '|\s'
let s:string_region = '\vstring|regex|pattern'
let s:ignored_region = s:string_region . '|comment|character'
let s:match_ignored_region_fn = 's:syntax_match(s:ignored_region, line("."), col("."))'
let s:macro_filetype_characters = {
    \ 'clojure': "#'`~@^_=",
    \ 'scheme':  "#'`,@",
    \ 'lisp':    "#'`,@",
    \ 'timl':    "#'`~@^_*"
    \ }
let s:default_macro_characters = s:macro_filetype_characters['scheme']
let s:pairs = {
    \ '(': ')',
    \ '[': ']',
    \ '{': '}',
    \ ')': '(',
    \ ']': '[',
    \ '}': '{',
    \ '"': '"'
    \ }

" Patch 7.3.590 introduced the ability to set visual marks with setpos()
let s:can_set_visual_marks = v:version > 703 || (v:version == 703 && has('patch590'))

" Return macro characters for current filetype. Defaults to Scheme's macro
" characters if 'lisp' is set, invalid characters otherwise.
function! s:macro_chars()
    if has_key(s:macro_filetype_characters, &filetype)
        return s:macro_filetype_characters[&filetype]
    elseif &lisp
        return s:default_macro_characters
    else
        return ''
    endif
endfunction

" Make a 'very magic' character class from input characters.
function! s:vm_cc(chars)
    return '[' . substitute(a:chars, '[^[0-9a-zA-Z_]]', '\\&', 'g') . ']'
endfunction

""" PRE/POST COMMAND CALLBACKS/CACHE {{{1

function! s:make_cache(mode, name)
    return {
        \ 'cvi': s:get_cursor_and_visual_info(),
        \ 'mode': a:mode,
        \ 'name': a:name,
        \ 'changedtick': b:changedtick
    \ }
endfunction

" Dirty flags: m=mode, n=command name, t=changedtick, c=cursor and/or visual range
function! s:is_dirty(...)
    let any = !a:0
    let flags = a:0 ? a:1 : ''
    let [oc, nc] = [b:sexp_cmd_prev_cache, b:sexp_cmd_cache]
    if !empty(oc)
        " We have a previous object to compare against.
        if (any || flags =~ 'm') && oc.mode != nc.mode
            return 1
        elseif (any || flags =~ 'n') && oc.name != nc.name
            return 1
        elseif (any || flags =~ 't') && oc.changedtick != nc.changedtick
            return 1
        " TODO: Document this a bit...
        elseif (any || flags =~ 'c') &&
            \ ( oc.mode ==? 'v' && nc.mode ==? 'v'
            \ ? oc.cvi.vs != nc.cvi.vs || oc.cvi.ve != nc.cvi.ve
            \ : oc.mode ==? 'v' || nc.mode ==? 'v'
            \ ? oc.mode != nc.mode
            \ : oc.cvi.cursor != nc.cvi.cursor)
            return 1
        endif
    endif
    return empty(oc)
endfunction

function! sexp#pre_op(mode, name)
    if !exists('b:sexp_cmd_prev_cache')
        let b:sexp_cmd_prev_cache = {}
    endif
    let b:sexp_cmd_cache = s:make_cache(a:mode == 'x' ? 'v' : a:mode, a:name)
endfunction

function! sexp#post_op(mode, name)
    " Note: Use actual mode in post command handler.
    let b:sexp_cmd_prev_cache = s:make_cache(mode(), a:name)
endfunction

""" QUERIES AT CURSOR {{{1

" Simple wrapper around searchpos() with flags 'nW', and optionally the
" stopline parameter.
"
" The original purpose of this function was to correct a bug in Vim where a
" backward search() from a multibyte character returned the wrong position:
"
" cf. https://groups.google.com/forum/?fromgroups=#!topic/vim_dev/s7c_Qq3K1Io
"
" This has since been fixed in 7.3.779, but this function remains for
" convenience.
function! s:findpos(pattern, next, ...)
    return searchpos(a:pattern, a:next ? 'nW' : 'bnW', a:0 ? a:1 : 0)
endfunction

" Position of nearest paired bracket: 0 for opening, 1 for closing. Returns
" [0, 0, 0, 0] if none found.
"
" In interest of performance (40x faster in some pathological cases!),
" mismatched brackets are not treated as errors. The following mess will be
" treated as if all brackets are of the same type:
"
"   [defn foo (bar]
"     {baz quux))
"
" However, your syntax engine should clearly highlight the errors. If you have
" a good argument for why this is not a good tradeoff, contact me.
"
" Accepts alternate beginning and ending patterns as optional parameters.
function! s:nearest_bracket(closing, ...)
    let flags = a:closing ? 'nW' : 'bnW'
    let stopline = g:sexp_maxlines > 0
                   \ ? max([1, line('.') + ((a:closing ? 1 : -1) * g:sexp_maxlines)])
                   \ : 0
    let open = a:0 ? a:1 : s:opening_bracket
    let close = a:0 ? a:2 : s:closing_bracket
    let [line, col] = searchpairpos(open, '', close, flags, s:match_ignored_region_fn, stopline)
    return line > 0 ? [0, line, col, 0] : [0, 0, 0, 0]
endfunction

function! s:list_open()
    let cursor = getpos('.')
    let ret = [0, 0, 0, 0]
    let isl = s:is_list(cursor[1], cursor[2])
    if !isl
        return ret
    elseif isl == 1
        call s:setcursor(s:current_macro_character_terminal(1))
        " Find the open.
        let [l, c] = s:findpos('\S', 1)
        call s:setcursor(cursor)
        return [0, l, c, 0]
    elseif isl == 2
        return cursor
    else " 3
        return s:nearest_bracket(0)
    endif
    " Restore original position.
    call s:setcursor(cursor)
    return ret
endfunction

" Position of outermost paired bracket: 0 for opening, 1 for closing.
" Returns [0, 0, 0, 0] if none found.
"
" If global variable g:sexp_maxlines is -1, a fast best-effort approach is
" used instead of a recursive searchpairpos()
function! s:current_top_list_bracket(closing)
    return g:sexp_maxlines < 0
           \ ? s:current_top_list_bracket_by_first_column(a:closing)
           \ : s:current_top_list_bracket_by_maxlines(a:closing)
endfunction

" Recursive searchpairpos() is excruciatingly slow on a large file. This can
" be addressed somewhat by providing a stopline argument, but this makes
" the call a best-effort approach. If we are sacrificing correctness for
" performance, we can do even better by assuming that all opening brackets on
" the first column of a line are toplevel.
function! s:current_top_list_bracket_by_first_column(closing)
    let cursor = getpos('.')
    let at_top = 0
    let [_b, line, col, _o] = s:current_element_terminal(0)

    if line > 0
        call cursor(line, col)
        let at_top = col == 1
    endif

    while !at_top
        let [_b, line, col, _o] = s:move_to_nearest_bracket(0)

        if line > 0 && col == 1
            let at_top = 1
        elseif line < 1
            break
        endif
    endwhile

    let pos = (at_top && getline(line)[col - 1] =~# s:opening_bracket)
              \ ? (a:closing ? s:nearest_bracket(1) : [0, line, col, 0])
              \ : [0, 0, 0, 0]

    call s:setcursor(cursor)

    return pos
endfunction

" Return current list's top-level bracket using searchpairpos() with
" g:sexp_maxlines
function! s:current_top_list_bracket_by_maxlines(closing)
    let [_b, cursorline, cursorcol, _o] = getpos('.')
    let flags = a:closing ? 'cnr' : 'bcnr'
    let stopline = g:sexp_maxlines > 0
                   \ ? max([1, cursorline + ((a:closing ? 1 : -1) * g:sexp_maxlines)])
                   \ : 0
    let [topline, topcol] = searchpairpos(s:opening_bracket, '', s:closing_bracket, flags, s:match_ignored_region_fn, stopline)

    if topline > 0
        return [0, topline, topcol, 0]
    " searchpairpos() fails to find the matching closing bracket when on the
    " outermost opening bracket and vice versa
    elseif getline(cursorline)[cursorcol - 1] =~# (a:closing ? s:opening_bracket : s:closing_bracket)
        return s:nearest_bracket(a:closing)
    else
        return [0, 0, 0, 0]
    endif
endfunction

" Position of start/end of current string: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in a string.
function! s:current_string_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')

    if !s:syntax_match(s:string_region, cursorline, cursorcol)
        return [0, 0, 0, 0]
    endif

    let termline = cursorline
    let termcol = cursorcol

    " We can't rely on va" or on searchpairpos() because they don't work well
    " on symmetric patterns.
    "
    " We also use s:findpos() while moving the cursor because using simple
    " column arithmetic breaks on multibyte characters.
    while 1
        let [line, col] = s:findpos('\v.', a:end)

        " Beginning or end of file.
        if line < 1 | break | endif

        if s:syntax_match(s:string_region, line, col)
            let termline = line
            let termcol = col
            call cursor(line, col)
        else
            break
        endif
    endwhile

    " We may be on leading macro characters if they have been defined as part
    " of the string region by the syntax engine
    if !a:end
        let [_b, l, c, _o] = s:current_macro_character_terminal(1)
        if l > 0
            let termline = l
            let termcol = c + 1
        endif
    endif

    call cursor(cursorline, cursorcol)
    return [0, termline, termcol, 0]
endfunction

" Position of start/end of current comment: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in a comment.
function! s:current_comment_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')

    if !s:is_comment(cursorline, cursorcol)
        return [0, 0, 0, 0]
    endif

    let termline = cursorline
    let termcol = cursorcol

    while 1
        let [line, col] = s:findpos('\v\_.', a:end)

        if line < 1 | break | endif

        if s:is_comment(line, col)
            let termline = line
            let termcol = col
            call cursor(line, col)
        else
            break
        endif
    endwhile

    call cursor(cursorline, cursorcol)
    return [0, termline, termcol, 0]
endfunction

" Position of start/end of current atom: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in an atom. Assumes atoms never span multiple
" lines.
function! s:current_atom_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')

    if !s:is_atom(cursorline, cursorcol)
        return [0, 0, 0, 0]
    endif

    let termline = cursorline
    let termcol = cursorcol

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

    call cursor(cursorline, cursorcol)
    return [0, termline, termcol, 0]
endfunction

" Position of start/end of current sequence of macro characters: 0 for start,
" 1 for end. Returns [0, 0, 0, 0] if not currently in a macro character
" sequence or no macro characters are defined for the current filetype.
function! s:current_macro_character_terminal(end)
    let macro = s:macro_chars()

    if empty(macro)
        return [0, 0, 0, 0]
    endif

    let [_b, cursorline, cursorcol, _o] = getpos('.')

    if stridx(macro, getline(cursorline)[cursorcol - 1]) < 0
        return [0, 0, 0, 0]
    endif

    let termline = cursorline
    let termcol = cursorcol

    while 1
        let [line, col] = s:findpos('\v.', a:end, cursorline)

        if line < 1 | break | endif

        if stridx(macro, getline(line)[col - 1]) >= 0
            let termline = line
            let termcol = col
            call cursor(line, col)
        else
            break
        endif
    endwhile

    call cursor(cursorline, cursorcol)
    return [0, termline, termcol, 0]
endfunction

" Position of start/end of current element: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in an element.
"
" An element is defined as:
"
"   * Current string if cursor is in a string
"   * Current comment if cursor is in a comment, or in the whitespace between
"     two line comments
"   * Current list if and only if cursor is on a paired bracket
"   * Current sequence of leading macro characters and following element if
"     cursor is on a macro character
"   * Current atom otherwise
"
" An element always includes leading macro characters.
function! s:current_element_terminal(end)
    let [_b, line, col, _o] = getpos('.')
    let char = getline(line)[col - 1]
    let include_macro_characters = !a:end

    if s:syntax_match(s:string_region, line, col)
        let pos = s:current_string_terminal(a:end)
    elseif s:is_comment(line, col)
        let pos = s:current_comment_terminal(a:end)
    elseif char =~# s:bracket && !s:syntax_match(s:ignored_region, line, col)
        if (a:end && char =~# s:closing_bracket) || (!a:end && char =~# s:opening_bracket)
            let pos = [0, line, col, 0]
        else
            let pos = s:nearest_bracket(a:end)
        end
    elseif s:is_macro_char(char)
        if !a:end
            " Let the rest of the function find the macro head
            let include_macro_characters = 1
            " If the macro character is at the tail of an atom, treat it as
            " part of the atom and return the head of the preceding element.
            if !s:is_atom(line, col + 1) && s:is_atom(line, col - 1)
                call cursor(line, col - 1)
                let pos = s:current_element_terminal(0)
            else
                let pos = [0, line, col, 0]
            endif
        " Otherwise search for the attached element's tail
        else
            let include_macro_characters = 0
            let macro_tail = s:current_macro_character_terminal(1)
            let elem_char = getline(macro_tail[1])[macro_tail[2]]
            if empty(elem_char) || elem_char =~# '\v\s'
                let pos = macro_tail
            else
                call cursor(macro_tail[1], macro_tail[2] + 1)
                let pos = s:current_element_terminal(1)
                call cursor(line, col)
            endif
        endif
    else
        let include_macro_characters = 0
        let pos = s:current_atom_terminal(a:end)
    endif

    if !include_macro_characters || pos[1] < 1 || pos[2] <= 1
        return pos
    else
        " Move cursor to left of start position and soak up any leading macro
        " characters
        call cursor(pos[1], pos[2] - 1)
        let pre = s:current_macro_character_terminal(0)
        call cursor(line, col)
        return pre[1] > 0 ? pre : pos
    endif
endfunction

" Returns position of previous/next element's head/tail.
" Returns current element's terminal if no adjacent element exists.
function! s:nearest_element_terminal(next, tail)
    let cursor = getpos('.')
    let pos = cursor

    try
        let terminal = s:current_element_terminal(a:next)

        if terminal[1] > 0 && s:compare_pos(pos, terminal) != 0
            let pos = terminal
            call s:setcursor(pos)
            " b moves to the head of the current word if not already on the
            " head and e moves to the tail if not on the tail. However, ge
            " does not!
            if (!a:next || a:tail) && !(!a:next && a:tail)
                throw 'sexp-error'
            endif
        endif

        let [l, c] = s:findpos('\v\S', a:next)
        let adjacent = [0, l, c, 0]

        " We are at the beginning or end of file
        if adjacent[1] < 1 || s:compare_pos(pos, adjacent) == 0
            throw 'sexp-error'
        " Or we are at the head or tail of a list
        elseif getline(l)[c - 1] =~ (a:next ? s:closing_bracket : s:opening_bracket)
            throw 'sexp-error'
        endif

        let pos = adjacent

        " We are at a head if moving forward or at a tail if moving backward
        if (a:next && !a:tail) || (!a:next && a:tail)
            throw 'sexp-error'
        else
            call s:setcursor(pos)
            let final = s:current_element_terminal(a:tail)
            if final[1] > 0
                let pos = final
            endif
        endif
    catch /sexp-error/
    finally
        call s:setcursor(cursor)
        return pos
    endtry
endfunction

""" QUERIES AT POSITION {{{1

function! s:pos_with_col_offset(pos, offset)
    let [b, l, c, o] = a:pos
    return [b, l, c + a:offset, o]
endfunction

" Return case insensitive match of the syntax group at position with pat.
"
" Version 7.2.446 introduced synstack(), which shows the entire stack of
" syntax groups for a given position. It also shows the syntax groups of the
" position under the cursor, even if on a blank line, unlike synIDattr, which
" returns 0 on a blank line.
"
" This also solves the problem of "contained" syntax groups. For example,
" a syntax file or colorscheme may define custom groups like todo items or
" trailing whitespace in a comment. In these regions the top syntax group name
" will not match 'comment', even though they are semantically still comments.
" If we know the underlying syntax group name however, we will be able to
" successfully match it.
"
" Instead of requiring that synstack() exist, we will simply use synIDattr in
" that case, even though it will return false values for empty lines within
" strings, etc.
if exists('*synstack')
    function! s:syntax_match(pat, line, col)
        let stack = synstack(a:line, a:col)
        return (synIDattr(get(stack, -1, ''), 'name') =~? a:pat) ||
             \ (synIDattr(get(stack, -2, ''), 'name') =~? a:pat)
    endfunction
else
    function! s:syntax_match(pat, line, col)
        return synIDattr(synID(a:line, a:col, 0), 'name') =~? a:pat
    endfunction
endif

" Return start of leading (0) or end of trailing (1) whitespace from pos.
" Returns pos if no such whitespace exists.
function! s:adjacent_whitespace_terminal(pos, trailing)
    let cursor = getpos('.')
    call s:setcursor(a:pos)

    let [_b, termline, termcol, _o] = a:pos

    while 1
        " Include empty lines
        let [line, col] = s:findpos('\v\_.', a:trailing)

        if line < 1 | break | endif

        let char = getline(line)[col - 1]

        if empty(char) || char =~# '\v\s'
            let termline = line
            let termcol = col
            call cursor(line, col)
        else
            break
        endif
    endwhile

    call s:setcursor(cursor)
    return [0, termline, termcol, 0]
endfunction

function! s:terminals_with_whitespace_info(start, end, leading)
    let cursor = getpos('.')
    let o = {}
    " Get text from BOL to start (exclusive).
    let bol_text = getline(a:start[1])[: a:start[2] - 1][: -2]
    let eol_text = getline(a:end[1])[a:end[2] - 1 :]

    let [o.start, o.end, o.leading] = [a:start, a:end, a:leading]
    let o.bol = bol_text =~ '^\s*$'
    let o.eol = eol_text =~ '^.\s*$'
    " Are we at beginning of sexp?
    call s:setcursor(a:start)
    let p = s:nearest_element_terminal(0, 1)
    let o.bos = s:compare_pos(p, a:start) >= 0
    " Do we follow a comment?
    let o.follows_com = !o.bos && s:is_comment(p[1], p[2])
    " Do we follow a list?
    " TODO: May not need this anymore.
    let o.follows_list = !o.bos && s:is_list(p[1], p[2])
    " Is current element a comment?
    " Make sure we're on an element.
    let p = s:current_element_terminal(0)
    if !p[1]
        let p = s:nearest_element_terminal(1, 0)
    endif
    let o.is_com = s:is_comment(p[1], p[2])
    " Are we at end of sexp?
    call s:setcursor(a:end)
    let p = s:nearest_element_terminal(1, 0)
    if s:compare_pos(p, a:end) <= 0
        let o.eos = 1
        let o.precedes_com = 0
    else
        " Another element exists
        let o.eos = 0
        let o.precedes_com = s:is_comment(p[1], p[2])
    endif

    " Find end of any sequences of whitespace immediately preceding start or
    " following end.
    let o['ws_s'] = s:adjacent_whitespace_terminal(o.start, 0)
    let o['ws_e'] = s:adjacent_whitespace_terminal(o.end, 1)
    " Set virtual start/end, which can include newlines.
    let o.ws_vs =
        \ o.ws_s[2] == 1 && o.ws_s[1] > 1
        \ ? [0, o.ws_s[1] - 1, col([o.ws_s[1] - 1, '$']), 0]
        \ : o.ws_s
    let o.ws_ve =
        \ s:offset_char(o.ws_e, 1)[1] > o.ws_e[1]
        \ ? [0, o.ws_e[1], col([o.ws_e[1], '$']), 0]
        \ : o.ws_e
    " De-normalized multi-line flag for convenience
    let o.ml = o.ws_vs[1] != o.ws_ve[1] || o.eol
    " Question: Do we need an additional test for sflags.real? Depends on how
    " we want to handle case of something up to end of line.
    " Flags:
    " ws: whitespace exists (includes blank lines but not newlines)
    " eol: newline at head/tail of whitespace
    " real: actual whitespace char at head/tail of whitespace
    " Asymmetry: When ws_s is line 1 col 1, ws_s will equal ws_vs.
    let [o.sflags, o.eflags] = [{}, {}]
    let o.sflags.ws = o.ws_s != a:start
    let o.sflags.eol = o.ws_vs != o.ws_s
    let o.sflags.real = o.sflags.ws && !o.sflags.eol && col([o.ws_s[1], '$']) > 1
    let o.sflags.spc = o.sflags.real && getline(o.ws_s[1])[o.ws_s[2] - 1] == ' '
    let o.sflags.chr = getline(o.ws_s[1])[o.ws_s[2] - 1]
    let o.eflags.ws = o.ws_e != a:end
    let o.eflags.eol = o.ws_ve != o.ws_e
    let o.eflags.real = o.eflags.ws && col([o.ws_e[1], '$']) > 1
    let o.eflags.spc = o.eflags.real && getline(o.ws_e[1])[o.ws_e[2] - 1] == ' '
    let o.eflags.chr = getline(o.ws_e[1])[o.ws_e[2] - 1]

    call s:setcursor(cursor)
    return o
endfunction

" Figures out the proper whitespace to select for either multi or single line
" join. Returns null sentinel to indicate failure. In the multi-line case,
" success indicates the lines will be joined; in single-line case, success
" simply means we were able to find suitable whitespace.
" TODO: "Join" may be misnomer, given that this handles single-line case.
function! s:get_join_whitespace(twwi)
    let o = a:twwi
    " TODO: Maybe use global (readonly) sentinel?
    let ret = [[0, 0, 0, 0], [0, 0, 0, 0]]
    " Assumption: Col pos for ecl test assumes we're deleting real ws at
    " start.
    " Note: Range ws_s..ws_e does not *necessarily* include whitespace (but
    " almost always will).
    let scl = o.sflags.real ? strdisplaywidth(o.sflags.chr, o.ws_s[2]) : 0
    let ecl = o.eflags.real ? strdisplaywidth(o.eflags.chr, o.ws_s[2]) : 0
    " Get first position to discard on first line (spos) and last position to
    " discard on last line (epos).
    " Note: Joining requires at least one actual (real) whitespace char.
    " (Blank line isn't sufficient.)
    if (ecl &&
        \ (!scl
        \  || (!empty(o.leading) && s:compare_pos(o.leading, o.ws_s) <= 0)
        \  || ecl < scl))
        " Select all leading and all but final trailing whitespace.
        let ret = [o.ws_vs,
            \ s:offset_char(o.ws_e, 0, 1)]
    elseif scl
        " Select all trailing and all but initial leading whitespace.
        " Note: Must handle special case of a kept whitespace at eol.
        let ret = [s:offset_char(o.ws_s, 1, 1), o.ws_ve]
    endif
    return ret
endfunction

" Assumption: !bos and !eos but there might not be any actual, non-newline
" whitespace between the two candidate join elements... Or the join might
" create a line that's too long. In either case, we return invalid list.
" TODO: Wordsmith this comment...
" Note: Empty lines count as whitespace.
function! s:ml_join(twwi)
    let o = a:twwi
    let [spos, epos] = s:get_join_whitespace(o)
    if !spos[1] && !o.bol && !o.eol
        " No good way to join completely, but we have no choice.
        return [o.start, o.end]
    endif
    if spos[1]
        " Get first kept char, which, for multi-line join, will always be next
        " real char past last deleted char pos.
        let eapos = s:offset_char(epos, 1)
        " Determine length of joined line.
        let jlen = spos[2] + strdisplaywidth(
            \ getline(eapos[1])[eapos[2] - 1:], spos[2])

        if jlen <= col([spos[1], '$'])
            " Go ahead and join
            return [spos, epos]
        endif
    endif
    " If here, we couldn't join...
    return s:partial_ml_join(o)
endfunction

" TODO: Join may be a bit of a misnomer in the single line case.
function! s:sl_join(twwi)
    let o = a:twwi
    " TODO: Relocate or remove the above comments...
    let ret = s:get_join_whitespace(o)
    if !ret[0][1]
        let ret = [o.start, o.end]
    endif
    return ret
endfunction

"" Note: start might be in leading ws; that's ok.
function! s:partial_ml_join(twwi)
    let o = a:twwi
    if o.eol
        " Select all whitespace but trailing newline.
        let start = o.ws_vs
        if o.eflags.eol
            let end = o.ws_e
        else
            let end = col([o.ws_e[1] - 1, '$']) == 1
                \ ? [0, o.ws_e[1] - 2, col([o.ws_e[1] - 2, '$']), 0]
                \ : [0, o.ws_e[1] - 1, col([o.ws_e[1] - 1, '$']) - 1, 0]
        endif
    else
        " !eol && bol
        " Rationale: Can't get into this function if !bol and !eol.
        " Subsequent element on same line as last selected element means
        " there's no trailing new line to leave. Select all leading whitespace
        " back to but not including first newline. I'm thinking maybe leave
        " the trailing whitespace, but haven't decided on that yet (TODO).
        " TODO: Consider leaving the trailing ws on final line...
        " Note: 1 scenario in which we don't want 1st col of line past
        " ws_vs[1]: whitespace extends to 1st char in buffer, in which case,
        " we simply select back to BOB.
        let start = o.ws_vs[1] == 1 && o.ws_vs[2] == 1
            \ ? o.ws_vs
            \ : [0, o.ws_vs[1] + 1, 1, 0]
        let end = o.end
        "let [start, end] = [o.ws_vs, [0, o.ws_vs[1] + 1, 1, 0]]
    endif
    return [start, end]
endfunction

" Given start and end positions, returns new positions [start', end']:
"
"   * If trailing whitespace after end, end' is set to include the trailing
"     whitespace up to the next element, unless start is preceded on its line
"     by something other than whitespace (but not an opening bracket), in
"     which case end' is set to include only the trailing whitespace to the
"     end of line.
"   * If start is preceded by opening bracket earlier on line (possibly with
"     intervening whitespace) or there's an element between BOL and start but
"     none between end and EOL, start' is set to include leading whitespace
"     back to the previous element or opening bracket.
"   * Otherwise start and end are returned verbatim.
"   Possible TODO: There's a pair of scenarios in which it might make sense to
"   pull in leading whitespace, even going back to an earlier line:
"   ) [<ws>] <element> [<ws>] )
"   ( [<ws>] <element> [<ws>] (
"   TODO: Completely REWORK this comment to reflect changed logic!!!!!!
"
" This behavior diverges from the behavior of the native text object aw in
" that it allows multiline whitespace selections.
function! s:terminals_with_whitespace(start, end, ...)
    let [start, end] = [a:start, a:end]
    " Note: Leading can be empty.
    let leading = a:0 ? a:1 : []

    let o = s:terminals_with_whitespace_info(start, end, leading)
    if o.follows_com || o.precedes_com
        " Keep a newline (ml) or whitespace (sl).
        return o.ml ? s:partial_ml_join(o) : s:sl_join(o)
    elseif o.bos || o.eos
        " Beginning or end of sexp and we've ruled out preceding/following
        " comment, so just clean up.
        return [o.ws_vs, o.ws_ve]
    else
        " Perform appropriate join.
        return o.ml ? s:ml_join(o) : s:sl_join(o)
    endif
endfunction

" Extend given positions to the terminals of any partially contained elements.
" If there exist any unpaired brackets in the region, the positions are
" extended to include those lists.
function! s:positions_with_element_terminals(positions)
    let cursor = getpos('.')
    let [start, end] = a:positions

    call s:move_to_element_near_position(start)
    let head = s:current_element_terminal(0)

    call s:move_to_element_near_position(end)
    let tail = s:current_element_terminal(1)

    if head[1] > 0 && tail[1] > 0
        " Find any unbalanced brackets in our selection
        let [bra, ket] = s:count_brackets(head, tail, s:bracket, s:opening_bracket)

        " Extend head for every ket
        if ket > 0
            call s:setcursor(head)
            call sexp#docount(ket, 's:move_to_nearest_bracket', 0)
            let head = getpos('.')
        endif

        " And tail for every bra
        if bra > 0
            call s:setcursor(tail)
            call sexp#docount(bra, 's:move_to_nearest_bracket', 1)
            let tail = getpos('.')
        endif
    endif

    call s:setcursor(cursor)
    return [head, tail]
endfunction

" Returns [bra, ket], which indicates the number of unpaired opening brackets
" ('bra') and the number of unpaired closing brackets ('ket') in the selection
" from start to end.
function! s:count_brackets(start, end, all_brackets, opening_brackets)
    let cursor = getpos('.')
    let bra = 0
    let ket = 0

    call s:setcursor(a:start)

    while 1
        let [line, col] = searchpos(a:all_brackets, 'cnW')

        " Start next iteration at next element if in ignored scope
        if s:syntax_match(s:ignored_region, line, col)
            call cursor(line, col)
            call s:move_to_adjacent_element(1, 0, 0)
            continue
        endif

        let cmp = s:compare_pos([0, line, col, 0], a:end)
        if cmp > 0 | break | endif

        if getline(line)[col - 1] =~# a:opening_brackets
            let bra += 1
        else
            if bra > 0
                let bra -= 1
            else
                let ket += 1
            endif
        endif

        if cmp == 0 | break | endif

        if col([line, '$']) - 1 == col
            call cursor(line + 1, 1)
        else
            call cursor(line, col + 1)
        endif
    endwhile

    call s:setcursor(cursor)
    return [bra, ket]
endfunction

" Returns the number of elements in the given range
function! s:count_elements(start, end)
    let cursor = getpos('.')
    let pos = a:start
    let n = 1

    call s:setcursor(pos)

    while 1
        let nextpos = s:move_to_adjacent_element(1, 0, 0)
        if s:compare_pos(nextpos, a:end) > 0 | break | endif
        let n += 1
        if s:compare_pos(pos, nextpos) == 0 | break | endif
        let pos = nextpos
    endwhile

    call s:setcursor(cursor)
    return n
endfunction

" Return pos offset by 1 char in requested direction
" Note: If optional flag is set, newlines between lines count, and we will
" never advance past a newline (even if we start on it).
" FIXME: This function is too complex for what it does. (Recall that offset
" used to be arbitrary, but now is limited to single char).
function! s:offset_char(pos, dir, ...)
    let cursor = getpos('.')
    let inc_nl = a:0 && a:1
    " Ensure normalized col position (1st byte in char).
    call cursor(a:pos[1], a:pos[2])
    let [l0, c0] = [line('.'), col('.')]
    let [l, c, cn] = [l0, c0, c0]
    let lim = a:dir ? col([l0, '$']) : 1
    " Loop until we've moved off reference char.
    " Note: Termination handled explicitly in line wrap case.
    while c == c0
        " Advance a byte in desired direction.
        let cn += a:dir ? 1 : -1
        " Check for line wrap...
        if a:dir && cn >= lim
            " EOL
            let [l, c] = inc_nl
                \ ? [l, col([l, '$'])]
                \ : l < line('$')
                \   ? [l + 1, 1]
                \   : [l, c0]
            break
        elseif !a:dir && cn < lim
            " BOL
            if l > 1
                let [l, c] = [l - 1, col([l - 1, '$'])]
                if !inc_nl && c > 1
                    " Goto first byte of final char.
                    call cursor(l, c - 1)
                    let [l, c] = [line('.'), col('.')]
                endif
            else
                " Can't go before first char in buffer.
                let [l, c] = [l, 1]
            endif
            break
        " No line wrap; see whether 1 byte movement constitutes char movement.
        else
            call cursor(l, cn)
            let [l, c] = [line('.'), col('.')]
        endif
    endwhile
    " Restore original position.
    call s:setcursor(cursor)
    return [0, l, c, 0]
endfunction

" Return a constrained range.
function! s:constrained_range(start, end, keep_end)
    let cursor = getpos('.')
    let [this_dir, that_dir] = [a:keep_end, !a:keep_end]
    let [this, that] = a:keep_end ? [a:end, a:start] : [a:start, a:end]
    let ret = [a:start[:], a:end[:]]
    " Set to [0, 0, 0, 0] if we determine definitively no limit needed.
    let lim = []
    " Find 'that'-side bracket containing position we know will be kept.
    call s:setcursor(this)
    let ket = s:move_to_nearest_bracket(that_dir)
    if ket[1]
        " If we found bracket matching 'this', move one level higher.
        let isl = s:is_list(this[1], this[2])
        if that_dir && isl == 2 || this_dir && isl == 3
            " Go a level higher if possible...
            let ket = s:move_to_nearest_bracket(that_dir)
        endif
    endif
    " Did we find a containing bracket?
    if ket[1]
        " Not at toplevel. Determine whether ket *could* represent a limit.
        let cmp = s:compare_pos(that, ket)
        if that_dir && cmp >= 0 || this_dir && cmp <= 0
            " Limiting *may* be required. In any case, we need to determine
            " exclusivity of limit: even if cmp alone guarantees we'll be
            " limiting, exclusivity will determine the limiting position.
            let exc = s:nearest_bracket(this_dir) != this
            if exc || cmp
                let lim = exc ? s:offset_char(ket, this_dir) : ket
            else
                let lim = [0, 0, 0, 0]
            endif
        endif
    endif
    " Are we still uncertain about limit?
    if empty(lim)
        " 'that' is either in descendant list or at same level as 'this'.
        " Search containing brackets till we hit either toplevel or bracket
        " containing this (i.e., ket), at which point, the previous found
        " bracket (if any) will be at the same level as 'this' and is the
        " sought limit. If no brackets were found, 'that' was already at same
        " level as 'this' and no limiting is required.
        call s:setcursor(that)
        let pos = []
        while 1
            let p = s:move_to_nearest_bracket(that_dir)
            if !p[1] || p == ket
                " We've hit either top or bracket containing this.
                if !empty(pos)
                    let lim = pos
                endif
                break
            endif
            let pos = p
        endwhile
    endif
    " Apply limit if one was determined.
    if !empty(lim) && lim[1]
        let ret[that_dir] = lim
    endif
    call s:setcursor(cursor)
    return ret
endfunction

""" PREDICATES AND COMPARATORS {{{1

" Returns 1 if char matches the current FileType's macro pattern
function! s:is_macro_char(char)
    " Caveat: stridx returns 0 for empty needle.
    return !empty(a:char) && stridx(s:macro_chars(), a:char) >= 0
endfunction

" Returns 1 if character at position is in a comment, or is in the whitespace
" between two line comments.
function! s:is_comment(line, col)
    if s:syntax_match('comment', a:line, a:col)
        return 1
    else
        let incomment = 0

        " We may be in the whitespace between two line comments; check if the
        " current line begins with a comment and the previous line ended with
        " a comment.
        if getline(a:line)[a:col - 1] =~# '\v\s'
            let cursor = getpos('.')
            call cursor(a:line, a:col)
            let [pline, pcol] = s:findpos('\v\S', 0, a:line - 1)
            let [cline, ccol] = s:findpos('\v\S', 1, a:line)
            if s:syntax_match('comment', pline, pcol)
                \ && s:syntax_match('comment', cline, ccol)
                let incomment = 1
            endif
            call s:setcursor(cursor)
        endif

        return incomment
    endif
endfunction

" Returns nonzero if input position is at toplevel.
function! s:at_top(line, col)
    let cursor = getpos('.')
    call cursor(a:line, a:col)
    let ret = !s:nearest_bracket(0)[1] || !s:nearest_bracket(1)[1]
    call s:setcursor(cursor)
    return ret
endfunction

" Returns nonzero if on list opening/closing chars:
"  0 => not on list head or tail
"  1 => on macro chars preceding opening bracket
"  2 => on list opening bracket
"  3 => on list closing bracket
function! s:is_list(line, col)
    let chars = getline(a:line)[a:col - 1:]
    let maybe = chars =~#
        \ '\v^' . s:vm_cc(s:macro_chars()) . '*%(' . s:opening_bracket . ')'
        \ ? chars[0] =~# s:opening_bracket ? 2 : 1
        \ : chars =~# '\v^%(' . s:closing_bracket . ')' ? 3 : 0
    " Extra test needed to ensure we're not fooled by spurious brackets within
    " ignored region.
    return maybe && !s:syntax_match(s:ignored_region, a:line, a:col)
        \ ? maybe : 0
endfunction

" Returns 1 if character at position is an atom.
"
" An atom is defined as:
"
"   * A contiguous region of non-whitespace, non-bracket characters that are
"     not part of a string or comment.
"
function! s:is_atom(line, col)
    let char = getline(a:line)[a:col - 1]

    if empty(char)
        return 0
    elseif char =~# s:delimiter && !s:syntax_match(s:ignored_region, a:line, a:col)
        return 0
    else
        return !s:syntax_match(s:string_region . '|comment', a:line, a:col)
    endif
endfunction

" Returns 1 if vmode is blank or equals 'v', 0 otherwise. Vim defaults to 'v'
" if the vmode member has not yet been set.
function! s:is_characterwise(vmode)
    return a:vmode ==# 'v' || a:vmode ==# ''
endfunction

" Returns -1 if position a is before position b, 1 if position a is after
" position b, and 0 if they are the same position. Only compares the line and
" column, ignoring buffer and offset.
function! s:compare_pos(a, b)
    if a:a[1] == a:b[1] && a:a[2] == a:b[2]
        return 0
    elseif a:a[1] != a:b[1]
        return a:a[1] < a:b[1] ? -1 : 1
    else
        return a:a[2] < a:b[2] ? -1 : 1
    endif
endfunction

""" CURSOR MOVEMENT {{{1

" Calls cursor(pos[1], pos[2]). Used in favor of setpos(), which is lower
" level than cursor(), omitting some UI niceties.
function! s:setcursor(pos)
    call cursor(a:pos[1], a:pos[2])
endfunction

" TODO: Remove...
function! s:move_char(dir)
    let pos = s:offset_char(getpos('.'), a:dir)
    call s:setcursor(pos)
endfunction

" Tries to move cursor to nearest paired bracket, returning its position.
function! s:move_to_nearest_bracket(closing)
    let pos = s:nearest_bracket(a:closing)
    if pos[1] > 0 | call s:setcursor(pos) | endif
    return pos
endfunction

" Move from list apparata to list open. Return [0, 0, 0, 0] if not on list.
function! s:move_to_list_open()
    let pos = s:list_open()
    if pos[1] | call s:setcursor(pos) | endif
    return pos
endfunction

" Tries to move cursor to outermost list's opening or closing bracket,
" returning its position; 0 for opening, 1 for closing. Does not move cursor
" if not in a list.
function! s:move_to_top_bracket(closing)
    let pos = s:current_top_list_bracket(a:closing)
    if pos[1] > 0 | call s:setcursor(pos) | endif
    return pos
endfunction

" Tries to move cursor to current element terminal, returning its position.
function! s:move_to_current_element_terminal(closing)
    let pos = s:current_element_terminal(a:closing)
    if pos[1] > 0 | call s:setcursor(pos) | endif
    return pos
endfunction

" Move cursor to adjacent element, returning its position; 0 for previous,
" 1 for next. If tail is 1, the cursor is placed on the end of the adjacent
" element, and on the head otherwise. If top is 1, moves to adjacent top-level
" element.
"
" If no such adjacent element exists, moves to beginning or end of element
" respectively. Analogous to native w, e, and b commands.
function! s:move_to_adjacent_element(next, tail, top)
    let cursor = getpos('.')

    if a:top
        let top = s:move_to_top_bracket(a:next)

        " Stop at current top element head if moving backward and did not
        " start on a top element head.
        if !a:next && top[1] > 0 && s:compare_pos(top, cursor) != 0
            let pos = top
        else
            let pos = s:nearest_element_terminal(a:next, a:tail)
        endif
    else
        let pos = s:nearest_element_terminal(a:next, a:tail)
    endif

    if pos[1] > 0 | call s:setcursor(pos) | endif
    return pos
endfunction

" Move cursor to pos, and then to the next element if in whitespace.
" FIXME: Blank line not treated like whitespace.
function! s:move_to_element_near_position(pos)
    call s:setcursor(a:pos)
    return getline('.')[col('.') - 1] =~# '\v\s'
           \ ? s:move_to_adjacent_element(1, 0, 0)
           \ : a:pos
endfunction

" Extend current selection by moving the cursor to position returned by
" evaluating func with given varargs. Detects which end of the selection
" should be extended.
function! s:move_cursor_extending_selection(func, ...)
    " Break out of visual mode, preserving cursor position
    if s:countindex > 0
        execute "normal! \<Esc>"
    endif

    let [start, end] = s:get_visual_marks()
    let omode = s:compare_pos(start, getpos('.')) == 0

    let pos = call(a:func, a:000)
    let valid = pos[1] > 0

    if omode
        call s:set_visual_marks([valid ? pos : start, end])
        call s:select_current_marks('v')
        normal! o
    else
        call s:set_visual_marks([start, valid ? pos : end])
        call s:select_current_marks('v')
    endif

    return pos
endfunction

" Move cursor to current list's terminal bracket, returning its position; 0
" for previous, 1 for next. If currently on an opening or closing bracket and
" moving backward or forward (respectively), cursor is moved to enclosing
" list's terminal bracket.
"
" The mode 'o' is handled specially:
"
"   * If moving backward, the cursor is positioned just after the opening
"     bracket so that the selection is exclusive at the head.
"   * If moving forward and the cursor is on an opening bracket, the current
"     list is treated as an element and the selection is extended up to, but
"     not including, the next outer closing bracket.
"   * If moving forward and the cursor is on a closing bracket, and the next
"     outer closing bracket is not immediately adjacent, the selection is made
"     exclusive such that neither the current bracket nor next outer closing
"     bracket are included.
"   * Otherwise the cursor is not moved and [0, 0, 0, 0] is returned.
"
" If there is no enclosing list, the cursor is not moved and [0, 0, 0, 0] is
" returned.
function! sexp#move_to_nearest_bracket(mode, next)
    if a:mode ==? 'v'
        return s:move_cursor_extending_selection('s:move_to_nearest_bracket', a:next)
    elseif a:mode ==? 'o' && !a:next
        let [_b, l, c, _o] = s:move_to_nearest_bracket(0)
        if l > 0
            let [l, c] = s:findpos('\v\_.', 1)
            call cursor(l, c)
        endif
        return [0, l, c, 0]
    elseif a:mode ==? 'o' && getline('.')[col('.') - 1] =~# s:bracket
        let cursor = getpos('.')
        let bracket = getline(cursor[1])[cursor[2] - 1]

        call s:move_to_current_element_terminal(1)
        let pos = s:nearest_bracket(1)

        if pos[1] < 1 || (bracket =~# s:closing_bracket
                          \ && cursor[1] == pos[1]
                          \ && cursor[2] == pos[2] - 1)
            call s:setcursor(cursor)
            return [0, 0, 0, 0]
        endif

        let start = getline(cursor[1])[cursor[2] - 1] =~# s:opening_bracket
                    \ ? cursor
                    \ : s:pos_with_col_offset(cursor, 1)
        call s:set_visual_marks([start, s:pos_with_col_offset(pos, -1)])
        call s:select_current_marks('o')
        return pos
    else
        return s:move_to_nearest_bracket(a:next)
    endif
endfunction

" Calls s:move_to_adjacent_element count times, with the following additional
" behaviours:
"
" * If mode == 'v', the current visual selection is extended
" * If mode == 'o'
"   - The selection is exclusive if tail is 0
"   - The selection is inclusive if tail is 1
"   - The selection is inclusive if tail is 0, next is 1, and the final
"     position of the cursor is not at an element head
"
"   The last case handles operations on head-wise forward movement that are
"   bounded by the parent list.
function! sexp#move_to_adjacent_element(mode, count, next, tail, top)
    if a:mode ==? 'n'
        return sexp#docount(a:count, 's:move_to_adjacent_element', a:next, a:tail, a:top)
    elseif a:mode ==? 'v'
        return sexp#docount(a:count, 's:move_cursor_extending_selection', 's:move_to_adjacent_element', a:next, a:tail, a:top)
    elseif a:mode ==? 'o'
        let cursor = getpos('.')
        call sexp#docount(a:count, 's:move_to_adjacent_element', a:next, a:tail, a:top)
        let pos = getpos('.')
        let nomove = s:compare_pos(cursor, pos) == 0

        " Bail out if the cursor has not moved and is resting on a delimiter
        if nomove && getline(pos[1])[pos[2] - 1] =~ s:delimiter
            return 0
        " Inclusive when:
        "   * Moving to tail
        "   * Moving forward to head but ending on tail because we are bounded
        "   * Same as above, but element is a single character so head == tail
        elseif a:tail
            \ || (a:next && s:compare_pos(pos, s:current_element_terminal(0)) != 0)
            \ || (a:next && nomove)
            " We make selections inclusive by entering visual mode
            call s:set_visual_marks([cursor, pos])
            return s:select_current_marks('o')
        endif
    endif
endfunction

" Move to [count]th next/prev bracket of type indicated by 'close', ignoring
" (skipping over) brackets of the non-specified type.
" Visual Mode: Visual command causes the destination list to be selected.
" Note: If BOF or EOF preclude [count] jumps, go as far as possible.
" Special Case: In visual mode, treat starting position as valid target if it
" happens to be of the correct bracket type and we can go no further.
" Selection Non Extension: Because flow commands intentionally cross list
" boundaries, both operator-pending commands and commands that extend the
" current visual selection would make it too easy for the user to destroy
" paren balance. For this reason, operator-pending flow commands are not
" provided at all, and the visual variants select the target rather than
" extending the current selection.
" Note: This function is complementary and orthogonal to sexp#leaf_flow, which
" flows similarly, but stops only on *non-list* (leaf) elements.
" TODO: If vim-sexp ever adds logic to handle weird things like escaped
" brackets in atoms - e.g., foo\(bar - revisit the ignore pattern used with
" searchpair.
function! sexp#list_flow(mode, count, next, close)
    let cnt = a:count ? a:count : 1
    " Loop until we've landed on [count]th non-ignored bracket of desired type
    " or exhausted buffer trying.
    " Note: Intentionally using searchpos with unmatchable start/end patterns
    " and desired target as 'middle' because it provides a simple way to apply
    " a syntax test to a match. The syntax test is needed because of the
    " possibility of brackets appearing in ignored regions such as strings,
    " character literals and comments: e.g.,
    "   "(foo)"
    "   #\)
    "   ; (( blah blah ))
    while cnt > 0 && 0 < searchpair('a\&b', a:close
        \ ? s:closing_bracket
        \ : s:opening_bracket, 'a\&b',
            \ 'W' . (a:next ? '' : 'b'),
            \ s:match_ignored_region_fn)
        let cnt -= 1
    endwhile
    if a:mode == 'v'
        if cnt < a:count
            \ || s:is_list(line('.'), col('.')) == (a:close ? 3 : 2)
            " Either we performed at least 1 jump, or we started on desired
            " bracket type. Either way, find other bracket and select list.
            let bpos = s:nearest_bracket(!a:close)
            " Re-enter visual mode with cursor on the desired side.
            " Note: No need to sort the marks, as Vim will swap as needed, and
            " we're about to set cursor pos with select_current_marks.
            call s:set_visual_marks([getpos('.'), bpos])
            call s:select_current_marks('v', a:close)
        else
            " We didn't find desired bracket, so just restore old selection.
            call s:select_current_marks('v')
        endif
    endif
endfunction

" Move to [count]th next/prev non-list (leaf) element in the buffer, flowing
" freely in and out of lists, landing on the element end indicated by 'tail'.
" Note: If BOF or EOF preclude [count] jumps, go as far as possible, landing
" on the far end of the final element in the buffer, even when doing so
" fails to honor 'tail' and/or [count] inputs.
" Note: This function is complementary with sexp#list_flow, which flows
" similarly, but stops only on list (non-leaf) elements.
" Selection Non Extension: See corresponding note in header of sexp#list_flow
" for the reason visual commands do not extend selection.
" Edge Cases:
" 1. The ambiguity that arises when an atom and list are separated by only
"    macro char(s) is solved differently by different lisp variants.
"    Example: foo'(bar)
"      Clojure: foo' (bar)
"      CL:      foo '(bar)
"    Moreover, current_element_terminal() is inconsistent, giving an answer
"    that depends on the starting position. Though the edge case is legal
"    lisp, it's not lisp a sane programmer should be writing, so I'm not going
"    to add a lot of logic to try to handle it consistently.
"    TODO: Revisit if current_element_terminal is ever updated to handle this
"    edge case consistently.
" 2. When a character literal ends in a literal space (e.g., `#\ '), special
"    logic would be required to avoid skipping over the whitespace when
"    searching backward for element tail. Since sexp_move_to_prev_element_tail
"    map handles this case incorrectly (landing on the backslash rather than
"    the following space char), and `#\Space' is much more readable than
"    `#\ ', I really can't justify adding a lot of logic to handle it
"    correctly here.
"    TODO: Revisit if move_to_adjacent_element is ever updated to handle the
"    edge case.
function! sexp#leaf_flow(mode, count, next, tail)
    " Is optimal destination near or far side of element?
    let near = !!a:next != !!a:tail
    let cnt = a:count ? a:count : 1
    let cursor = getpos('.')
    " Update nf to indicate target reached (i.e., last position attained in
    " search, not necessarily the desired position specified by 'tail').
    " Values: =-1, near=0, far=1
    let nf = -1
    " Are we starting on list (macro chars or brackets)?
    if !s:is_list(cursor[1], cursor[2])
        " The current element (if any) is not a list (or macro chars), and
        " hence *could* be target. If inside element, position on far side in
        " preparation for subsequent search (which may or may not be needed,
        " given that if far side is sought, this initial positioning may
        " actually count as jump).
        let pos = s:move_to_current_element_terminal(a:next)
        if pos[1]
            " We're on far side of non-list element. If far side is desired
            " target, and we weren't already on it, first jump is complete.
            " Either way, we've reached *acceptable* target.
            if pos != cursor && !near
                let cnt -= 1
            endif
            let nf = 1
        endif
    endif
    " We're either on list head/tail, at the far side of an element, or not on
    " anything at all. Fallback position isn't needed, since all jumps are in
    " the desired direction, and will be accepted, even if they don't get us
    " to desired target.
    while cnt > 0
        " Note: See note on use of this unconventional use of searchpair
        " in list_flow function.
        let pos = searchpair('a\&b', '\S', 'a\&b',
            \ 'W' . (a:next ? '' : 'b'),
            \ 's:is_list(line("."), col("."))')
        if pos <= 0
            " We've gone as far as we can.
            break
        endif
        " We're on near side of next element.
        let npos = getpos('.')
        if cnt > 1 || !near
            " Either we're going to search again or we're done searching but
            " target is far side: in either case, position on far side.
            call s:move_to_current_element_terminal(a:next)
            let nf = 1
        else
            " Done searching and target is near side.
            let nf = 0
        endif
        let cnt -= 1
    endwhile
    if a:mode ==? 'v'
        if nf >= 0
            " Set near pos if we started past it.
            if !exists('l:npos')
                let npos = s:current_element_terminal(!a:next)
            endif
            let fpos = nf ? getpos('.') : s:current_element_terminal(a:next)
            " Select target visually, placing cursor on target end.
            " Note: No need to sort the marks, as Vim will swap as needed, and
            " we're about to set cursor pos with select_current_marks.
            call s:set_visual_marks([npos, fpos])
            " Re-enter visual mode with cursor on the target end.
            call s:select_current_marks('v', nf ? a:next : !a:next)
        else
            " Cursor unchanged. Simply restore original selection.
            call s:select_current_marks('v')
        endif
    endif
endfunction

" Move cursor to current list start or end and enter insert mode. Inserts
" a leading space after opening bracket if inserting at head, unless there
" already is one.
function! sexp#insert_at_list_terminal(end)
    let pos = s:move_to_nearest_bracket(a:end)

    " Handle opening bracket edge cases
    if !a:end && pos[1] > 0
        let nextchar = getline(pos[1])[pos[2]]

        " This is the eol, so start insert at eol
        if empty(nextchar)
            startinsert!
            return
        " Add headspace unless there's already some there
        elseif nextchar !~# '\v\s'
            execute 'normal! a '
        " Else start after the bracket
        else
            normal! l
        endif
    endif

    startinsert
endfunction

""" VISUAL MARKS {{{1

" Return current visual marks as a list
function! s:get_visual_marks()
    return [getpos("'<"), getpos("'>")]
endfunction

if s:can_set_visual_marks
    " Set visual marks to [start, end]
    function! s:set_visual_marks(marks)
        call setpos("'<", a:marks[0])
        call setpos("'>", a:marks[1])
    endfunction
else
    " Before 7.3.590, the only way to set visual marks was to actually enter
    " and exit visual mode. The method using setpos() above is preferred
    " because there are no side effects apart from setting the marks.
    function! s:set_visual_marks(marks)
        let cursor = getpos('.')

        if mode() ==? 'v' | execute "normal! \<Esc>" | endif
        call s:setcursor(a:marks[0])
        normal! v
        call s:setcursor(a:marks[1])
        execute "normal! \<Esc>"

        call s:setcursor(cursor)
    endfunction
endif

" Set visual marks to the positions of the nearest paired brackets. Offset is
" the number of columns inwards from the brackets to set the marks.
"
" If allow_expansion is 1, the visual marks are set to the next outer pair of
" brackets under the following circumstances:
"
"   * Mode equals 'v', the cursor is on an opening bracket, the mark '< is
"     valid, and the marks '< and '> are not equal. This occurs when calling
"     this function while already having a list selected in visual mode.
"
"   * s:countindex is greater than 0 and the mark '< is valid. Occurs when
"     called by sexp#docount()
"
" Will set both to [0, 0, 0, 0] if none are found and mode does not equal 'v'.
"
" Returns 1 if marks were set successfully, and 0 if not.
function! s:set_marks_around_current_list(mode, offset, allow_expansion)
    " We may potentially move the cursor.
    let cursor = getpos('.')
    let cursor_moved = 0

    " Prepare the entrails
    let start = getpos("'<")
    let visual = a:mode ==? 'v'
    let counting = s:countindex > 0
    let start_is_valid = start[1] > 0
    let have_selection = start_is_valid && s:compare_pos(start, getpos("'>")) != 0
    let expanding = a:allow_expansion && (counting || (visual && have_selection))

    " When evaluating via sexp#docount the cursor position will not be updated
    " to '<, so do it now.
    if counting && start_is_valid
        if mode() ==? 'v' | execute "normal! \<Esc>" | endif
        call s:setcursor(start)
        let cursor = start
        let cursor_moved = 1
    endif

    " Native object selections expand when repeating inner motions as well
    if expanding
        \ && a:offset == 1
        \ && getline(cursor[1])[cursor[2] - 2] =~# s:opening_bracket
        normal! h
        let cursor = getpos('.')
        let cursor_moved = 1
    endif

    let ignored = s:syntax_match(s:ignored_region, cursor[1], cursor[2])
    let char = getline(cursor[1])[cursor[2] - 1]

    if !ignored && char =~# s:opening_bracket
        if expanding
            if s:move_to_nearest_bracket(1)[1] > 0
                let cursor_moved = 1
                call s:move_to_nearest_bracket(1) " Expansion step
            endif
            let open = s:pos_with_col_offset(s:nearest_bracket(0), a:offset)
            let close = s:pos_with_col_offset(getpos('.'), -a:offset)
        else
            let open = s:pos_with_col_offset(getpos('.'), a:offset)
            let close = s:pos_with_col_offset(s:nearest_bracket(1), -a:offset)
        endif
    elseif !ignored && char =~# s:closing_bracket
        let open = s:pos_with_col_offset(s:nearest_bracket(0), a:offset)
        let close = s:pos_with_col_offset(getpos('.'), -a:offset)
    else
        let open = s:pos_with_col_offset(s:nearest_bracket(0), a:offset)
        let close = s:pos_with_col_offset(s:nearest_bracket(1), -a:offset)
    endif

    let success = 0

    " Inner selection on adjacent brackets results in open being one character
    " past close due to offset calculations
    if open[1] > 0 && close[1] > 0 && s:compare_pos(open, close) < 0
        call s:set_visual_marks([open, close])
        let success = 1
    " Don't erase marks when in visual mode
    elseif !visual
        delmarks < >
    endif

    if cursor_moved
        call s:setcursor(cursor)
    endif

    return success
endfunction

" Set visual marks to the positions of the outermost paired brackets from the
" current location. Will set both to [0, 0, 0, 0] if none are found and mode
" does not equal 'v'.
"
" Returns 1 if marks were set successfully, and 0 if not.
function! s:set_marks_around_current_top_list(mode, offset)
    let closing = s:current_top_list_bracket(1)

    if closing[1] > 0
        " Calling searchpairpos() is faster when you start from an end
        let cursor = getpos('.')
        call s:setcursor(closing)
        let opening = s:nearest_bracket(0)
        call s:setcursor(cursor)

        " Don't delete adjacent brackets with an inner motion
        if a:offset > 0 && opening[1] == closing[1] && opening[2] == closing[2] - 1
            delmarks < >
        else
            call s:set_visual_marks([s:pos_with_col_offset(opening, a:offset),
                                   \ s:pos_with_col_offset(closing, -a:offset)])
            return 1
        endif
    elseif a:mode !=? 'v'
        delmarks < >
    endif
endfunction

" Set visual marks to the start and end of the current string. Will set both
" to [0, 0, 0, 0] if not currently in a string and mode does not equal 'v'.
function! s:set_marks_around_current_string(mode, offset)
    let end = s:current_string_terminal(1)

    if end[1] > 0
        let start = s:current_string_terminal(0)

        " Don't delete adjacent quotes with an inner motion
        if a:offset > 0 && start[1] == end[1] && start[2] == end[2] - 1
            delmarks < >
        else
            call s:set_visual_marks([s:pos_with_col_offset(s:current_string_terminal(0), a:offset),
                                   \ s:pos_with_col_offset(end, -a:offset)])
        endif
    elseif a:mode !=? 'v'
        delmarks < >
    endif
endfunction

" TODO: Document...
function! s:select_child(mode, count, next, inner)
    let cursor = getpos('.')
    " Are we on a list?
    let isl = s:is_list(cursor[1], cursor[2])
    if !isl
        " Find parent list head or tail
        let p = s:move_to_nearest_bracket(!a:next)
        if !p[1]
            " At top level. Move to buffer head/tail
            exe 'normal!' (a:next ? 'gg0' : 'G$')
        endif
    else
        " On list. Position on desired bracket (if not already there).
        if a:next
            if isl == 1
                " Move to open
                " Get to tail of macro chars.
                call s:setcursor(s:current_macro_character_terminal(1))
                call s:move_char(1)
            elseif isl == 3
                " Move to open
                call s:move_to_nearest_bracket(0)
            endif
        elseif isl != 3
            " Move to close
            call s:move_to_current_element_terminal(1)
        endif
    endif
    " On list open/close.
    " Get opposite bracket's position so we can detect null list.
    let p = s:nearest_bracket(a:next)
    " Move inside bracket.
    call s:move_char(a:next)
    if p == getpos('.')
        " Null list!
        " TODO: How to handle...
        return
    endif
    " We're inside non-null (but possibly empty) list.
    " Are we on an element? If so, get to its near side.
    let p = s:move_to_current_element_terminal(!a:next)
    if !p[1]
        " In whitespace next to bracket. Find head/tail element.
        let pp = getpos('.')
        " TODO: Fold this into the list perhaps, but need to differentiate
        " case in which nothing found.
        let p = s:move_to_adjacent_element(a:next, !a:next, 0)
        if p == pp
            " No elements in list.
            " TODO: How to handle...
            return
        endif
    endif
    " Assumption: We're sitting on first/last element (whose position is p).
    " If count > 1, find count-1'th adjacent element.
    let cnt = a:count ? a:count - 1 : 0
    while cnt
        let pp = p
        let p = s:move_to_adjacent_element(a:next, !a:next, 0)
        if p == pp
            " We've gone as far as we can go.
            break
        endif
        let cnt -= 1
    endwhile

    call s:set_marks_around_current_element(a:mode, a:inner)
endfunction

" Note: No point in calling if there's not visual selection, but handle
" gracefully if we're not.
function! s:get_cursor_and_visual_info()
    let o = {}

    let [vs, ve] = [getpos("'<"), getpos("'>")]
    " Save raw visual marks before possible adjustment.
    let [o.raw_vs, o.raw_ve] = [vs, ve]
    " Note: Since we don't really know what the command's mode was (and don't
    " really care), differentiate solely on whether visual sel exists; if it
    " doesn't, we'll return cursor pos along with some innocuous sentinel
    " values for range.
    if vs[1] && ve[1]
        " Check for visual range beginning past eol
        if vs[2] > 1 && vs[2] >= col([vs[1], '$'])
            let vs = [0, vs[1] + 1, 1, 0]
        endif
        " Check for visual range ending past eol
        if ve[2] > 1 && ve[2] >= col([ve[1], '$'])
            " Assumption: Will work even if multi-byte...
            let ve[2] -= 1
        endif
        " Ascertain (normalized) cursor position.
        " Note: If we're not in visual mode, we'll have to enter it to
        " determine which end cursor was on.
        " TODO: Does Vim provide another way?
        let mode = mode()
        if mode !=? 'v'
            let cursor = getpos('.')
            normal! gv
        endif
        " Note: When range begins past eol, exiting visual mode causes cursor to
        " fall back to last char on line (which is not actually *within* the
        " visual range). Note that this can happen at both start and end of range.
        " Note: Default to at_end when '< == '>.
        let o.at_end = s:compare_pos(getpos('.'), ve) >= 0
        if mode !=? 'v'
            exe "normal! \<Esc>"
            call s:setcursor(cursor)
        endif
        let o.cursor = o.at_end ? ve : vs
    else
        " No selection has ever been made.
        let [o.at_end, o.cursor] = [0, getpos('.')]
    endif
    " Note: Marks could be invalid sentinels (i.e., [0, 0, 0, 0])
    let [o.vs, o.ve] = [vs, ve]
    return o
endfunction

" Set visual marks to the start and end of the current element. If
" inner is 0, trailing or leading whitespace is included by way of
" s:terminals_with_whitespace().
" TODO: Update documentation to reflect changes in s:terminals_with_whitespace
" logic.
"
" If cursor is on whitespace that is not in a string or between line comments,
" the marks are set around the next element if inner is 1, and around the
" current position and end of the next element if inner is 0.
"
" Will set both to [0, 0, 0, 0] if an element could not be found and mode does
" not equal 'v'.
" Optional Args:
"   a:1  count (>= 0 implies multi-select)
"   a:2  inhibit visual selection (return range only)
function! s:set_marks_around_current_element(mode, inner, ...)
    " Extra args imply extension mode only if mode is visual.
    "let extend = a:0 && !!a:1 && a:mode ==? 'v'
    let multi = a:0 && a:1 >= 0
    let cnt = a:0 && a:1 >= 0 ? a:1 : 0
    let no_sel = a:0 > 1 ? !!a:2 : 0
    let leading = []
    if multi
        if a:mode ==? 'v'
            let vi = b:sexp_cmd_cache.cvi
            let dir = vi.at_end
            " Rationalize visual range.
            let [vs, ve] = s:constrained_range(vi.vs, vi.ve, dir)
            " In case actual cursor position has changed.
            " Note: Cursor will align with either '< or '>
            let cursor = dir ? ve : vs
            call s:setcursor(cursor)
            let p = cursor
        else
            let cursor = getpos('.')
            let [vs, ve] = [cursor, cursor]
            let dir = 1
        endif
        " Position at start of range.
        call s:setcursor(vs)
    else
        let cursor = getpos('.')
    endif
    " Search from element start to avoid errors with elements that end
    " with macro characters. e.g. Clojure auto-gensyms: `(let [s# :foo)])
    " TODO: Rework this comment...
    let start = s:move_to_current_element_terminal(0)

    if !start[1]
        " We are on whitespace; check for next element
        let p = getpos('.')
        let next = s:move_to_adjacent_element(1, 0, 0)
        if next == p
            " No next element!
            if a:mode !=? 'v'
                " Inhibit operation.
                delmarks < >
            endif
            return [[0, 0, 0, 0], [0, 0, 0, 0]]
        endif

        " *Maybe* don't include whitespace at other end.
        let leading = cursor
        let start = next
    endif

    " Position ourselves to look for end.
    " Assumption: We're at head of element.
    let end = []
    " TODO: Consider not setting vs/ve in multi when mode not visual.
    if multi && s:compare_pos(start, ve) < 0
        " Position on end of visual region, then find either end of current or
        " end of previous (if no current element).
        " TODO: Consider changing this logic when counts involved...
        call s:setcursor(ve)
        let end = s:current_element_terminal(1)
        if !end[1]
            " Weren't on an element. Get to end of previous (whose
            " existence is guaranteed).
            let end = s:move_to_adjacent_element(0, 1, 0)
        endif
    endif
    if empty(end)
        " Assumption: Still at element head.
        let end = s:current_element_terminal(1)
    endif

    " We've now established 'current' selection. Consider pulling in extra,
    " taking direction into account.
    if cnt > 0
        let p = dir ? end : start
        call s:setcursor(p)
        while cnt > 0
            let pp = p
            " Assumption: Can't get here in legacy case.
            " TODO: Consider breaking the legacy case out into legacy func...
            let p = s:move_to_adjacent_element(dir, dir, 0)
            if p == pp
                " We've gone as far as possible.
                break
            endif
            let cnt -= 1
        endwhile
        " Adjust the end we've pulled.
        let l:[dir ? 'end' : 'start'] = p
    endif

    " Handle surrounding whitespace if 'outer'.
    if !a:inner
        let [start, end] = s:terminals_with_whitespace(start, end, leading)
    endif

    call s:setcursor(cursor)
    if !no_sel
        " TODO: Need to ensure cursor at specific side, but perhas in caller?
        call s:set_visual_marks([start, end])
    endif
    return [start, end]
endfunction

" Set visual marks to the start and end of the adjacent inner element. If no
" element is adjacent in the direction specified, the marks are set around the
" current element instead via s:set_marks_around_adjacent_element().
function! s:set_marks_around_adjacent_element(mode, next)
    let cursor = getpos('.')

    if a:mode ==? 'v'
        execute "normal! \<Esc>"
    endif

    " If moving backward, first position ourselves at the head of the current
    " element.
    if !a:next
        call s:move_to_current_element_terminal(0)
    endif

    call s:move_to_adjacent_element(a:next, 0, 0)
    call s:set_marks_around_current_element(a:mode, 1)
    call s:setcursor(cursor)
endfunction

" Enter characterwise visual mode with current visual marks, unless '< is
" invalid and mode equals 'o'.
" Optional Arg:
"   a:1 - where to leave cursor after performing the visual selection:
"         0=left side ('<), 1=right side ('>)
"         Note: This arg is ignored if marks not set.
function! s:select_current_marks(mode, ...)
    if getpos("'<")[1] > 0
        if mode() !=? 'v'
            " Caveat: If we're already in visual mode, gv would revert to
            " *previous* visual marks!!!
            normal! gv
        endif
        if !s:is_characterwise(visualmode())
            normal! v
        endif
        if a:0
            " Caller has requested that cursor be left on particular side.
            " Caveat: We cannot rely on accurate '< and '> values from getpos
            " at this point: if the setpos() calls occur while visual mode is
            " linewise, getpos() will continue to return line=1 and col=-1 for
            " col positions until mapping has completed. Fortunately, we can
            " discern the true bounds of the characterwise visual region by
            " using normal! o in conjunction with getpos('.').
            let pos = getpos('.')
            " Jump to other side to see which side we're on.
            normal! o
            let cmp = s:compare_pos(getpos('.'), pos)
            if a:1 && cmp < 0 || !a:1 && cmp > 0
                " We were already on the desired end.
                normal! o
            endif
        endif
        return 1
    elseif a:mode !=? 'o'
        normal! v
        return 1
    else
        return 0
    endif
endfunction

" Convert visual marks to a characterwise selection if visualmode() is not 'v'
function! s:set_marks_characterwise()
    if !s:is_characterwise(visualmode())
        call s:select_current_marks('v')
        execute "normal! \<Esc>"
    endif
endfunction

" Set visual marks at current list's brackets, then enter visual mode with
" that selection. Selects current element if cursor is not in a list.
function! sexp#select_current_list(mode, offset, allow_expansion)
    if !s:set_marks_around_current_list(a:mode, a:offset, a:allow_expansion)
        call s:set_marks_around_current_element(a:mode, a:offset)
    endif
    return s:select_current_marks(a:mode)
endfunction

" Set visual marks at current outermost list's brackets, then enter visual
" mode with that selection. Selects current element if cursor is not in a
" list.
function! sexp#select_current_top_list(mode, offset)
    if !s:set_marks_around_current_top_list(a:mode, a:offset)
        call s:set_marks_around_current_element(a:mode, a:offset)
    endif
    return s:select_current_marks(a:mode)
endfunction

" Unlike the native text object a" we do not try to select all the whitespace
" up to the next element. This can be done with sexp#select_current_element if
" desired. If not currently in string and mode equals 'o', nothing is done.
function! sexp#select_current_string(mode, offset)
    call s:set_marks_around_current_string(a:mode, a:offset)
    return s:select_current_marks(a:mode)
endfunction

" Set visual marks around current element and enter visual mode.
function! sexp#select_current_element(mode, inner, ...)
    " TODO: Take actual command name into account, or create new command
    " (e.g., select_current_elements).
    let cnt = a:0 ? a:1 ? a:1 : 1 : -1
    if cnt > 0
        \ && b:sexp_cmd_cache.name =~ 'sexp_\%(inner\|outer\)_element'
        \ && s:is_dirty()
        let cnt -= 1
    endif
    call s:set_marks_around_current_element(a:mode, a:inner, cnt)
    " TODO: Preserve cursor position.
    return s:select_current_marks(a:mode, a:mode ==? 'v' ? b:sexp_cmd_cache.cvi.at_end : 1)
endfunction

" Set visual marks around adjacent element and enter visual mode; 0 for
" previous, 1 for next. If no such adjacent element exists, selects current
" element.
function! sexp#select_adjacent_element(mode, next)
    call s:set_marks_around_adjacent_element(a:mode, a:next)
    return s:select_current_marks(a:mode)
endfunction

" Set visual marks around count'th child of current (or parent) list; 0 to
" count backwards from tail, 1 to count forwards from head. If no such child
" element exists, selects closest one (i.e., last in the specified direction).
function! sexp#select_child(mode, count, next, rev)
    call s:select_child(a:mode, a:count, a:next, a:rev)
    return s:select_current_marks(a:mode)
endfunction

""" BUFFER MUTATION {{{1

" Insert bra and ket around current visual marks. Selection is converted to a
" characterwise selection if last visualmode() was not 'v'.
"
" If mark '< is invalid, inserts brackets at cursor.
"
" Parameter at_tail sets cursor at head or tail (0 or 1), and parameter
" headspace determines whether to insert a space after the opening bracket
" when placing cursor at the head.
function! s:insert_brackets_around_visual_marks(bra, ket, at_tail, headspace)
    call s:set_marks_characterwise()

    let [start, end] = s:get_visual_marks()

    " No selection, just insert brackets
    if start[1] < 1
        execute 'normal! i' . a:bra . a:ket
    elseif a:at_tail
        call s:setcursor(start)
        execute 'normal! i' . a:bra
        " Did we just insert a character on the same line?
        if start[1] == end[1]
            let end = s:pos_with_col_offset(end, len(a:bra))
        endif
        call s:setcursor(end)
        execute 'normal! a' . a:ket
    else
        call s:setcursor(end)
        execute 'normal! a' . a:ket
        call s:setcursor(start)
        execute 'normal! i' . a:bra . (a:headspace ? ' ' : '')
    endif
endfunction

function! s:insert_brackets_around_current_list(bra, ket, at_tail, headspace)
    call s:set_marks_around_current_list('n', 0, 0)
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:headspace)
endfunction

function! s:insert_brackets_around_current_string(bra, ket, at_tail, headspace)
    call s:set_marks_around_current_string('n', 0)
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:headspace)
endfunction

function! s:insert_brackets_around_current_element(bra, ket, at_tail, headspace)
    call s:set_marks_around_current_element('n', 1)
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:headspace)
endfunction

" Capture element adjacent to current list, given the starting position of the
" enclosing list's bracket minus leading macro characters (spos) and the
" position of the bracket itself (bpos).
function! s:stackop_capture(last, spos, bpos)
    call s:setcursor(a:spos)
    let nextpos = s:move_to_adjacent_element(a:last, 0, 0)

    " Ensure we are not trying to capture a parent list
    if nextpos[1] < 1 || s:compare_pos(a:spos, s:current_element_terminal(!a:last)) == (a:last ? 1 : -1)
        return 0
    endif

    let reg_save = @b
    let @b = getline(a:spos[1])[a:spos[2] - 1 : a:bpos[2] - 1]
    let blen = len(@b)

    " Insertion and deletion must be done from the bottom up to avoid
    " recalculating our marks
    if a:last
        let nextpos = s:current_element_terminal(1)
        call s:setcursor(nextpos)
        execute 'silent! normal! "bp'
        call s:setcursor(a:spos)
        execute 'silent! normal! "_d' . blen . 'l'
        call s:setcursor(s:pos_with_col_offset(nextpos, 1 + -(a:spos[1] == nextpos[1])))
    else
        call s:setcursor(a:spos)
        execute 'silent! normal! "_d' . blen . 'l'
        call s:setcursor(nextpos)
        execute 'silent! normal! "bP'
    endif

    let @b = reg_save
    return 1
endfunction

" Emit terminal element in current list, given the starting position of the
" enclosing list's bracket minus leading macro characters (spos) and the
" position of the bracket itself (bpos).
function! s:stackop_emit(last, spos, bpos)
    " Move inwards onto the terminal element, then find the penultimate
    " element, which will become the ultimate element after the move
    call s:setcursor(a:bpos)

    let [l, c] = s:findpos('\v\S', !a:last)
    if l < 1 | return 0 | endif

    call cursor(l, c)

    if a:last
        call s:move_to_current_element_terminal(0)
    endif

    let nextpos = s:move_to_adjacent_element(!a:last, 0, 0)
    if nextpos[1] < 1 | return 0 | end

    let nextpos = s:current_element_terminal(a:last)

    " Ensure that this new ultimate element is different than the last and
    " that it is actually contained
    if s:compare_pos(nextpos, [0, l, c, 0]) == 0
        \ || s:compare_pos(nextpos, a:spos) != (a:last ? -1 : 1)
        \ || s:compare_pos(nextpos, s:nearest_bracket(!a:last)) != (a:last ? 1 : -1)
        return 0
    endif

    let reg_save = @b
    let @b = getline(a:spos[1])[a:spos[2] - 1 : a:bpos[2] - 1]
    let blen = len(@b)

    " Insertion and deletion must be done from the bottom up to avoid
    " recalculating our marks
    if a:last
        call s:setcursor(a:spos)
        execute 'silent! normal! "_d' . blen . 'l'
        call s:setcursor(nextpos)
        execute 'silent! normal! "bp'
    else
        call s:setcursor(nextpos)
        execute 'silent! normal! "bP'
        call s:setcursor(a:spos)
        execute 'silent! normal! "_d' . blen . 'l'
        call s:setcursor(a:spos[1] == nextpos[1] ? s:pos_with_col_offset(nextpos, -1) : nextpos)
    endif

    let @b = reg_save
    return 1
endfunction

" Swap current visual selection with adjacent element. If pairwise is true,
" swaps with adjacent pair of elements. If mode is 'v', the newly moved
" selection is reselected.
function! s:swap_current_selection(mode, next, pairwise)
    let reg_save = [@a, @b]
    let visual = a:mode =~? 'v'

    " Yank selection and mark with START OF TEXT and END OF TEXT if necessary
    call s:select_current_marks(a:mode)
    silent! normal! "ay
    if a:next
        let @a = nr2char(0x02) . @a . nr2char(0x03)
    endif

    let amarks = s:get_visual_marks()

    " Record the sibling element
    call s:setcursor(amarks[!!a:next])
    call s:set_marks_around_adjacent_element('n', a:next)
    if a:pairwise && s:can_set_visual_marks
        let mark = a:next ? "'>" : "'<"
        call s:setcursor(getpos(mark))
        call setpos(mark, s:nearest_element_terminal(a:next, a:next))
        call s:set_visual_marks(s:positions_with_element_terminals(s:get_visual_marks()))
    endif
    call s:select_current_marks(a:mode)
    silent! normal! "by
    let bmarks = s:get_visual_marks()

    " Abort if we are already at the head or tail of the current list or at
    " the top or bottom of the file. In these cases the start/end mark will be
    " the same in the direction of movement.
    if s:compare_pos(amarks[a:next], bmarks[a:next]) == 0
        let [@a, @b] = reg_save
        return 0
    endif

    " We change the buffer from the bottom up so that the marks remain
    " accurate.
    if a:next
        let areg = 'a'
        let breg = 'b'
        let aswapmarks = amarks
        let bswapmarks = bmarks
    else
        let areg = 'b'
        let breg = 'a'
        let aswapmarks = bmarks
        let bswapmarks = amarks
    endif

    call s:set_visual_marks(bswapmarks)
    call s:select_current_marks('v')
    execute 'silent! normal! "' . areg . 'p'

    call s:set_visual_marks(aswapmarks)
    call s:select_current_marks('v')
    execute 'silent! normal! "' . breg . 'p'

    " Set marks around next element using the ^B and ^C markers
    if a:next
        call s:setcursor(amarks[0])

        let [sl, sc] = s:findpos(nr2char(0x02), 1)
        call cursor(sl, sc)
        normal! x
        let s = [0, sl, sc, 0]

        let [el, ec] = s:findpos(nr2char(0x03), 1)
        call cursor(el, ec)
        normal! x
        let e = [0, el, ec - 1, 0]

        call s:set_visual_marks([s, e])
    endif

    if visual
        call s:select_current_marks('v')
    elseif a:next
        call s:setcursor(getpos("'<"))
    else
        call s:setcursor(bmarks[0])
    endif

    let [@a, @b] = reg_save
    return 1
endfunction

" Adjust range start/end/inc to make range entirely inclusive, taking special
" inc value into account.
"   0 = exclusive
"   1 = inclusive
"   2 = exclusive of whitespace up to and including newline at EOL (start of
"       range) or BOL (end of range)
function! s:yankdel_range__preadjust_range(start, end, inc)
    let [start, end] = [a:start[:], a:end[:]]
    " TODO: Consider using cursor movements with search rather than this
    " programmatic approach.
    if a:inc[0] == 2 &&
        \ getline(start[1])[start[2] - 1:] =~ '^.\?\s*$'
        " Convert to start of next line inclusive.
        let start[1] += 1
        let start[2] = 1
    elseif !a:inc[0]
        " Adjustment: Make start/end reflect the operable region.
        let start = s:offset_char(start, 1, 1)
    endif
    if a:inc[1] == 2 &&
        \ getline(end[1])[:end[2] - 1] =~ '^\s*.\?$'
        " Convert to end of prev line inclusive.
        let end[1] -= 1
        " Special Case: 
        if end[1] > 1 && col([end[1], '$']) <= 1
            " Empty line. Move back just past end of previous line.
            let end[1] -= 1
            let end[2] = col([end[1], '$'])
        else
            let end[2] = col([end[1], '$']) - 1
        endif
    elseif !a:inc[1]
        let end = s:offset_char(end, 0, 1)
    endif
    return [start, end]
endfunction

function! s:yankdel_range__preadjust_positions(start, end, ps)
    " Pre-op position adjustment
    let ret = {'ps': a:ps, 'byte_offs': [], 'start': a:start}
    " Total # of bytes in file used to calculate delta later.
    let ret.bytes_in_file = s:total_bytes_in_file()
    " Calculate byte offset of start wrt BOF and end wrt start.
    let ret.start_byte = s:pos2byte(a:start)
    let ret.end_off = s:pos2byte(a:end) - ret.start_byte
    " Calculate and store offsets of positions of interest wrt start of range.
    for p in a:ps
        call add(ret.byte_offs, s:pos2byte(p) - ret.start_byte)
    endfor
    return ret
endfunction

function! s:yankdel_range__postadjust_positions(adj, splice)
    " Post-op position adjustment
    " Calculate net byte delta (added (+) / deleted (-))
    let delta = s:total_bytes_in_file() - a:adj.bytes_in_file
    let [ps, offs] = [a:adj.ps, a:adj.byte_offs]
    let [s, s_byte, e_off] = [a:adj.start, a:adj.start_byte, a:adj.end_off]
    " Iterate parallel lists ps and offs.
    for i in range(len(offs))
        let [o, p] = [offs[i], ps[i]]
        if o > 0
            " Adjustment required.
            if o < e_off && a:splice
                " Move to start
                " Caveat: Considered using byte offsets to calculate start
                " (rather than saving in adjustment struct), but this could
                " cause undesirable shift to subsequent line when end is at
                " EOL. In such cases, it's better to set position to point
                " just past EOL and rely on subsequent cursor positioning to
                " pull it back.
                let [p[1], p[2]] = s[1:2]
            else
                " Use either adjusted original pos or adjusted original
                " end (if original pos is in deleted range).
                let [p[1], p[2]] = s:byte2pos(s_byte + max([o, e_off]) + delta)[1:2]
            endif
        endif
    endfor
endfunction

" Yank, delete or splice text in range defined by start/end, returning any
" deleted text.
" Splice Note: If a string (rather than a boolean flag) is supplied for
" del_or_spl, the range will be *replaced* by the provided text and the
" deleted text will be returned.
" Range Inclusivity: Defaults to inclusive start, exclusive end, but may be
" overridden by first optional arg (a:1), which may be either a boolean that
" sets end inclusivity (1=inclusive), or a 2-element list setting inclusivity
" of start and end independently.
" Position Adjustment: If caller provides a 2nd optional arg (a:2), it is
" assumed to be a list of positions that will be modified in-place to account
" for any deletions. The goal is to preserve position in an intelligent
" manner: i.e., wherever possible, a position should point to the same
" character before and after the operation. If this is not possible, we should
" do the next best thing, which typically means adjusting the position to a
" deterministic location near the head or tail of the operated region.
function! s:yankdel_range(start, end, del_or_spl, ...)
    let ret = ''
    let cursor = getpos('.')
    " Temporarily set 'virtualedit' to onemore.
    " Rationale: Need to be able to select (visually) past EOL in certain
    " cases (e.g., non-inclusive start at EOL).
    let ve_save = &ve
    set ve=onemore
    let reg_save = [@a, @"]
    try
        let inc = a:0 ? type(a:1) == 3 ? a:1 : [1, a:1] : [1, 0]
        let [start, end] = s:yankdel_range__preadjust_range(a:start, a:end, inc)
        let cmp = s:compare_pos(start, end)
        " Special Case: Treat splice of null replacement region as a put.
        " Design Decision: Only *just empty* regions will be treated this way:
        " e.g., start == end and either end (but not both) exclusive. Though
        " there are simpler ways to accomplish it, you could use such null
        " regions to perform a simple put, whose direction is determined by
        " which end is inclusive (inclusive start => put before).
        let spl_put = cmp > 0 && s:offset_char(end, 1, 1) == start
        if cmp <= 0 || spl_put
            " Note: Since splice also deletes, del will be set for either.
            let [del, spl, spl_text] = type(a:del_or_spl) == 1
                \ ? [1, 1, a:del_or_spl]
                \ : [!!a:del_or_spl, 0, '']

            if del
                " Pre-op position adjustment
                let adj = s:yankdel_range__preadjust_positions(start, end,
                    \ a:0 > 1 ? s:concat_positions(a:2, cursor) : [cursor])
            endif

            " FIXME: This *could* be combined with the non-spl_put case.
            " Should it be??
            if spl_put
                call s:setcursor(end)
                let @a = spl_text
                normal! "ap
            else
                " Select text to be yanked/deleted
                call s:set_visual_marks([start, end])
                call s:select_current_marks('v')
                if spl
                    let @a = spl_text
                    normal! "ap
                    let ret = @"
                else
                    exe 'normal! ' . '"a' . (del ? 'd' : 'y')
                    let ret = @a
                endif
            endif
            if del
                " Post-op position adjustment
                call s:yankdel_range__postadjust_positions(adj, spl)
            endif
        endif
    finally
        " Restore some things...
        let [@a, @"] = reg_save
        let &ve = ve_save
        call s:setcursor(cursor)
    endtry

    return ret
endfu
let Ydr = function('s:yankdel_range')

" Put input text at specified position, with input flags determining whether
" paste works like p, P, gp or gP.
" Important Note: Takes special care to put text 'characterwise', even when
" string to be put ends in newline.
" Args:
" text           Text to put
" before         Nonzero puts text before input pos (default after)
" cursor_after   Nonzero leaves cursor just after put text (default at start)
" [pos]          Location at which to put the text. Defaults to cursor pos.
fu! s:put_at(text, before, cursor_after, ...)
    " Position defaults to cursor pos.
    if a:0
        call s:setcursor(a:1)
    endif
    let [reg_save, @a] = [@a, a:text]
    " Caveat: Vim's treatment of -1 string index doesn't obey POLS; use range.
    let linewise = a:text[-1:] == "\n"
    if linewise
        " Make the register non-linewise.
        let @a .= ' '
    endif
    " Note: Use g modifier unconditionally in linewise case to simplify
    " post-put logic.
    exe 'normal! "a'
        \ . (linewise || a:cursor_after ? 'g' : '') . (a:before ? 'P' : 'p')
    let @a = reg_save
    if linewise
        " Remove the space added to inhibit linewise operation.
        " Save '[ and '] before x or X modify them.
        let [start, end] = [getpos("'["), getpos("']")]
        " Space will always be at BOL; whether we're on or after the space
        " depends upon line and 'virtualedit'.
        if col('.') == 1
            " Must be at EOL.
            normal! "_x
        else
            " Backspace over the space
            normal! "_X
        endif
        " Note: When pasting a register that ends in newline, Vim leaves
        " cursor *after* newline, but sets '] mark *before*; do likewise...
        call setpos("'[", start)
        call setpos("']", end)
        " Note: In cursor_after case, position is already correct.
        if !a:cursor_after
            " Position at start of operation.
            call s:setcursor(start)
        endif
    endif
endfu

" Adjust the input view to ensure that, if possible, the cursor line doesn't
" change its screen line. (Won't be possible if desired cursor line exceeds
" buffer size.)
function! s:adjust_saved_view(view, cursor)
    " TODO: Do we need to apply constraints?
    let a:view.topline -= max([1, a:view.lnum - a:cursor[1]])
    let a:view.lnum = a:cursor[1]
    let a:view.col = a:cursor[2] - 1
    " Note: Because of the lack of one-to-one correspondence between chars and
    " screen columns, preserving horizontal shift would require an iterative
    " approach, whose overhead is probably not warranted, especially
    " considering it's *extremely* rare for a view on code to be shifted
    " horizontally. Thus, for now, simply rely on Vim to ensure the cursor
    " column is visible.
endfunction

" Indent S-Expression, maintaining cursor position. This is similar to mapping
" to =<Plug>(sexp_outer_list)`` except that it will fall back to top-level
" elements not contained in an compound form (e.g. top-level comments).
function! sexp#indent(top, count, clean, ...)
    let win = winsaveview()
    let cursor = getpos('.')
    let [_b, line, col, _o] = getpos('.')
    let force_syntax = a:0 && !!a:1

    " Move to current list tail since the expansion step of
    " s:set_marks_around_current_list() happens at the tail.
    if getline(line)[col - 1] =~ s:closing_bracket && !s:syntax_match(s:ignored_region, line, col)
        let pos = [0, line, col, 0]
    else
        let pos = s:move_to_nearest_bracket(1)
    endif

    normal! v
    if pos[1] < 1
        " At top-level. If current (or next) element is list, select it.
        " Note: When not within list, 'inner' includes brackets.
        keepjumps call sexp#select_current_element('v', 1)
    elseif a:top
        " Inside list. Select topmost list.
        keepjumps call sexp#select_current_top_list('v', 0)
    else
        " Inside list. Select [count]th containing list.
        keepjumps call sexp#docount(a:count, 'sexp#select_current_list', 'v', 0, 1)
    endif
    if force_syntax
        " Force syntax update on visual lines before running indent.
        " Rationale: Certain indent functions rely on syntax attributes to
        " calculate indent: e.g., GetClojureIndent() contains a call to
        " s:MatchPairs(), which in turn contains a call to searchpairpos(),
        " which can find the wrong bracket if pasted text has not yet had its
        " syntax recalculated (e.g., because the paste and subsequent indent
        " happen in a single command). Caller should set the force_syntax flag
        " in such scenarios to force syntax recalculation prior to the =.
        '<,'>call synID(line("."), col("."), 1)
    endif
    let cur = cursor[:]
    let [beg, end] = [getpos("'<"), getpos("'>")]
    if a:clean
        " Be sure we've selected a list: algorithm not intended for top-level
        " non-list element.
        let vs = getpos("'<")
        if s:is_list(vs[1], vs[2]) == 2
            "echomsg "Running @ " . string(ps[0])
            " Remove excess whitespace, keeping up with position changes.
            call s:cleanup_ws(beg, [end, cur])
            if cur != cursor
                " Note: To avoid visual jarring, try to keep cursor on same
                " screen line.
            endif
        endif
    endif
    " Record initial distance from cursor to end of line.
    let cur_edist = col([cur[1], '$']) - cur[2]
    " Caveat: Attempting to apply = operator in visual mode does not work
    " consistently.
    silent keepjumps exe "normal! \<Esc>" . beg[1] . 'G=' . end[1] . "G"
    " Adjust cursor pos to account for leading whitespace changes.
    let cur[2] = col([cur[1], '$']) - cur_edist
    call s:adjust_saved_view(win, cur)

    call winrestview(win)
endfunction

" Create a flat list encompassing all input positions.
" Note: The flat list is intended to facilitate iteration: the positions it
" contains are generally modifed in-place.
function! s:concat_positions(...)
    let ret = []
    for p in a:000
        let ret += type(p[0]) == 0 ? [p] : p
    endfor
    return ret
endfunction

" Convert position to corresponding file byte offset.
function! s:pos2byte(p)
    return line2byte(a:p[1]) + a:p[2] - 1
endfunction

" Convert file byte offset to corresponding position.
function! s:byte2pos(b)
    let l = byte2line(a:b)
    let c = a:b - line2byte(l) + 1
    return [0, l, c, 0]
endfunction

" Return total # of bytes in file.
function! s:total_bytes_in_file()
    return line2byte('$') + col([line('$'), '$'])
endfunction

" Modify ps in-place.
" Note: inc is always 2-element list.
" Assumption: start/end refer to actual char positions.
function! s:adjust_positions(start, end, splice, delta, ps)
    let [s, e] = [a:start[:], a:end[:]]

    for p in a:ps
        if s:compare_pos(p, s) <= 0
            " Position unaffected
            continue
        elseif s:compare_pos(p, e) < 0
            " Inside deleted/replaced region.
            if a:splice
                " Original line/col has no meaning. Move to head.
                let [p[1], p[2]] = s[1:2]
                continue
            else
                " Move to position just past deletion.
                " TODO: Could use fall-through and share this logic.
                let [p[1], p[2]] = s:byte2pos(s:pos2byte(e) + a:delta)[1:2]
            endif
        else
            " Past deleted/replaced region.
            let [p[1], p[2]] = s:byte2pos(s:pos2byte(p) + a:delta)[1:2]
        endif
    endfor
endfunction

" Assumption: We're on an open (but no guarantee form contains elements).
function! s:list_head()
    let cursor = getpos('.')
    let close = s:current_element_terminal(1)
    let ret = [0, 0, 0, 0]
    " Attempt move to first non-whitespace.
    let [l, c] = s:findpos('\S', 1)
    if [0, l, c, 0] == close
        " Empty form
        return ret
    endif
    call cursor(l, c) 
    " Just to be sure...
    let ret = s:current_element_terminal(0)
    " Restore original position.
    call s:setcursor(cursor)
    return ret
endfunction

" FIXME: Convert this version to use yankdel_range.
function! s:cleanup_ws(open, ps)
    let open = a:open[:]
    " FIXME: open is getting passed in as [0, 0, 0, 0] in toplevel case.
    " Toplevel case is actually being handled incorrectly.
    let [close, prev] = [[0, 0, 0, 0], [0, 0, 0, 0]]
    if open[1]
        call s:setcursor(open)
        " Descend into list.
        let next = s:list_head()
    else
        " At top-level. Find head element in buffer.
        " FIXME: Should be able to move_to_element_near_position, but it's
        " currently broken.
        call cursor(1, 1)
        let next = s:current_element_terminal(0)
        if !next[1]
            let next = s:nearest_element_terminal(1, 0)
        endif
    endif
    while 1
        " Note: next and close are mutually exclusive.
        if !next[1]
            let close = s:nearest_bracket(1)
        else
            let close = [0, 0, 0, 0]
        endif
        " TODO: Consider handling eff_prev in post-update (or some other way).
        " Rationale: After first iteration, it will always be prev (not open).
        let eff_prev = prev[1] ? prev : open
        if !eff_prev[1] | let eff_prev = [0, 1, 1, 0] | endif
        let eff_next = next[1] ? next : close
        if !eff_next[1] | let eff_next = getpos([line('$'), '$']) | endif

        let do_join = eff_next[1] > eff_prev[1]
            \ && (!next[1] && !prev[1]
                \ || !next[1] && (!close[1] || !s:is_comment(prev[1], prev[2]))
                \ || !prev[1] && (!open[1] || !s:is_comment(next[1], next[2])))

        if do_join || eff_next[1] - eff_prev[1] > 1
            " We're joining and/or removing empty lines.
            " TODO: Problem to have eff_next passed by ref in both spots?
            call s:yankdel_range(eff_prev, eff_next, 1,
                \ do_join ? [0, 0] : [0, 2], s:concat_positions(a:ps, eff_next))
        endif
        " FIXME: If we process in forwards direction, next needs to be
        " adjusted by yankdel_range; processing backwards would obviate need.
        " Note: Only zero/nonzero status of next is safe to use at this point
        " (since it wasn't adjusted by yankdel_range).
        if !next[1] | break | endif
        " If here, there's another element at this level.
        " Assumption: eff_next and next are the same except that the former
        " has been adjusted.
        call cursor(eff_next[1], eff_next[2])
        if s:is_list(eff_next[1], eff_next[2])
            let next = s:move_to_list_open()
            call s:cleanup_ws(next, a:ps)
            " Assumption: Restore cursor pos (potentially changed by
            " recursion) to next (which can't be invalidated by recursion).
            call s:setcursor(next)
        endif
        " Now that we've recursed (if possible), attempt to advance.
        let prev = s:move_to_current_element_terminal(1)
        let next = s:nearest_element_terminal(1, 0)
        if next == prev
            " Note: We'll go through loop once more.
            let next = [0, 0, 0, 0]
        else
            call s:setcursor(next)
        endif
    endwhile
endfunction

function! s:cleanup_ws_adjust_posns1(l1, l2, ps)
    " >>| in or past deleted lines
    for p in a:ps
        if p[1] > a:l2
            let p[1] -= a:l2 - a:l1 + 1
        elseif p[1] >= a:l1
            " Move cursor to head of line beyond deletion range.
            let p[1] = a:l2 < line('$') ? a:l2 + 1 : line('$')
            let p[2] = 1
        endif
    endfor
endfunction

function! s:cleanup_ws_adjust_posns2(l2, dbytes, ps)
    for p in a:ps
        " >>| in or after stripped ws at head of line
        if p[1] == a:l2
            " Move cursor leftward by deleted chars (limit BOL)
            let p[2] = p[2] > a:dbytes ? p[2] - a:dbytes : 1
        endif
    endfor
endfunction

function! s:cleanup_ws_adjust_posns3(l1, ps)
    " >>| on or after joined line
    for p in a:ps
        if p[1] > a:l1
            if p[1] == a:l1 + 1
                let p[2] += col([a:l1, '$']) - 1
            endif
            let p[1] -= 1
        endif
    endfor
endfunction

" TODO: Change to strategy in which counts specify # of containing lists (like
" for formatting).
function! s:cleanup_ws_working(open, ps)
    let prev = [0, 0, 0, 0]
    let open = a:open[:]
    if open[1]
        call s:setcursor(open)
        let next = s:list_head()
        if !next[1]
            let close = s:nearest_bracket(1)
        endif
    else
        " At top-level. Find head element in buffer.
        " FIXME: Should be able to move_to_element_near_position, but it's
        " currently broken.
        call cursor(1, 1)
        let next = s:current_element_terminal(0)
        if !next[1]
            let next = s:nearest_element_terminal(1, 0)
        endif
        if !next[1]
            " Empty buffer. FIXME: How to handle? Delete all?
            return
        endif
    endif
    while 1
        " Note: Valid end not guaranteed.
        if !next[1]
            " TODO: Does this always work?
            " Note: If we have next, there is no close.
            let close = s:nearest_bracket(1)
        else
            let close = [0, 0, 0, 0]
        endif
        " Note: t=trailing ws of head line, h=head ws of trailing line
        let del = {'tline': 0, 'join': 0, 'hline': 0, 'rrange': 0}
        let eff_prev = prev[1] ? prev : open[1] ? open : [0, 0, 0, 0]
        let eff_next = next[1] ? next : close[1] ? close : [0, 0, 0, 0]
        " ARRGGHHH!!!! Occurs to me I could be using s:yankdel_range()...
        let del.tline = eff_prev[1] && eff_next[1] > eff_prev[1]

        let del.join =
            \ (open[1] && open == eff_prev)
            \ && (close[1] || next[1] && !s:is_comment(next[1], next[2])) ||
            \ (close[1] && close == eff_next)
            \ && (open[1] || prev[1] && !s:is_comment(prev[1], prev[2]))

        " TODO: Do it for toplevel, even if not joining.
        " Question: What about empty buffer? 
        " Assumption: line numbers of 0 mean nothing in that direction.
        let del.hline = eff_next[1] && (del.join || !eff_prev[1])

        let del.rrange = eff_prev[1] && eff_prev[1] == eff_next[1]

        if del.rrange
            " FIXME!!!
            "echomsg "Doing rrange!"
        else
            if del.tline
                " TODO: Consider using g_, D, etc...
                " Strip any trailing whitespace after prev el.
                call setline(prev[1], substitute(getline(prev[1]), '\s*$', '', ''))
            endif
            " Get deletion range.
            " FIXME: Can open/close be [0, 0, 0, 0]? If so, handle; also, this
            " expression seems mostly duplicate with eff_prev/next. Eliminate
            " duplication.
            let [l1, l2] = [
                \ prev[1] ? prev[1] + 1 : open[1] + 1,
                \ next[1] ? next[1] - 1 : close[1] ? close[1] - 1 : line('$')]
            let dlines = l2 - l1 + 1
            if dlines
                " Delete blank lines, adjusting subsequent line numbers accordingly.
                exec l1 . ',' . l2 . 'd'
                if eff_next[1]
                    " TODO: Probably stop managing end, deferring getting it
                    " till advancement.
                    let eff_next[1] -= dlines
                endif
                " >>| in or past deleted lines
                call s:cleanup_ws_adjust_posns1(l1, l2, a:ps)
            endif
            let dbytes = 0
            if del.hline
                " Delete whitespace from head of end line
                let n = col([eff_next[1], '$'])
                call setline(eff_next[1], substitute(getline(eff_next[1]), '^\s*', '', ''))
                " FIXME: Don't like keeping up with eff_next changes manually:
                " that was the point of the cleanup_ws_adjust<...> functions.
                let eff_next[2] = 1
                let dbytes = n - col([eff_next[1], '$'])
                " >>| in or after stripped ws at head of line
                call s:cleanup_ws_adjust_posns2(eff_next[1], dbytes, a:ps)
            endif
            if del.join
                " FIXME: Don't set next here if it wasn't set. Maybe use
                " eff_next?
                let eff_next = [0, eff_prev[1], col([eff_prev[1], '$']), 0]
                " >>| on or after joined line
                " FIXME: Not accounting for rightward shift due to join.
                call s:cleanup_ws_adjust_posns3(eff_prev[1], a:ps)
                " FIXME: Ugly! This has to come after call to
                " s:cleanup_ws_adjust_posns3. Rework completely...
                " Assumption: prev[1] != 0 (since join at BOF)
                exec eff_prev[1] . 'join!'
            endif
        endif
        if !next[1]
            break
        endif
        " If here, another element at this level.
        if s:is_list(next[1], next[2])
            let next = s:move_to_list_open()
            " Assumption: Position of next can't be changed by recursive call.
            call s:cleanup_ws(next, a:ps)
        endif
        " Now that we've recursed (if possible), attempt to advance.
        call cursor(eff_next[1], eff_next[2])
        let prev = s:move_to_current_element_terminal(1)
        let next = s:nearest_element_terminal(1, 0)
        if next == prev
            " Note: We'll go through loop once more.
            let next = [0, 0, 0, 0]
        else
            call s:setcursor(next)
        endif
    endwhile
endfunction

" TODO: Comment...
" TODO: Probably rename...
" TODO: Consider adding mode arg.
" FIXME: May not need this anymore, now that I'm moving into sexp#indent.
function! sexp#cleanup_around_element(count, mode)
    " FIXME: Make this only normal mode, now that we're always doing whole
    " number of forms.
    let cursor = getpos('.')
    " Find count'th open (including any at cursor)
    call s:move_to_current_element_terminal(1)
    let cnt = a:count ? a:count : 1
    let open = [0, 0, 0, 0]
    while cnt
        let p = move_to_nearest_bracket(0)
        if !p[1]
            break
        endif
        let open = p
        let cnt -= 1
    endwhile
    " TODO: Ensure we're on open here. If not, pass [0, 0, 0, 0] sentinel.
    call s:cleanup_around_element(open, cursor)

    call s:setcursor(cursor)
    " TODO: Do the indent here or at higher level?
endfunction

" Place brackets around scope, then place cursor at head or tail, finally
" leaving off in insert mode if specified. Insert also sets the headspace
" parameter when inserting brackets.
function! sexp#wrap(scope, bra, ket, at_tail, insert)
    if a:scope ==# 'f'
        call s:insert_brackets_around_current_list(a:bra, a:ket, a:at_tail, a:insert)
    elseif a:scope ==# 'e'
        call s:insert_brackets_around_current_element(a:bra, a:ket, a:at_tail, a:insert)
    elseif a:scope ==# 'v'
        call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:insert)
    endif

    if a:insert
        startinsert
    endif
endfunction

" Replace parent list with selection resulting from executing func with given
" varargs.
function! sexp#raise(mode, func, ...)
    if a:mode ==# 'v'
        call s:select_current_marks('v')
    else
        call call(a:func, a:000)
    endif
    normal! d
    call sexp#select_current_list('n', 0, 0)
    normal! p
endfunction

" Logic:
" 1. Attempt to position at head of current element.
" 2. Cut text back to beginning of containing form.
" 3. Splice what remains of the form (saving open/close brackets).
" 4. Wrap the form <count>+1 levels up with the saved open/close brackets,
"    placing the text cut from step 2 just inside the open.
" 5. Re-indent the form *containing* the newly-added form.
" 6. Position cursor just *past* the text that was cut in step 2.
"    Note: In the general case, this will leave cursor where it was before the
"    convolute.
fu! sexp#convolute(count, ...)
    let cursor = getpos('.')

    " Var Nomenclature:
    " tpos=list tail
    " bpos=list open bracket
    " spos=list macro chars (or bracket if no macro chars)
    "  *_i=inner-most list involved in convolute
    "  *_o=outer-most list involved in convolute

    " Climb the expression tree count+1 times, recording the heads of the
    " inner/outer-most lists involved in convolute.
    " Note: v:count1 would really be better-suited to stuff like this.
    let [idx, n] = [0, a:count ? a:count + 1 : 2]
    while idx < n
        let p = s:move_to_nearest_bracket(0)
        if !p[1]
            " Warn and return without changing anything.
            echohl WarningMsg
            echomsg "Convolute impossible with count given: insufficient nesting"
            echohl None
            call s:setcursor(cursor)
            return
        endif
        if !idx
            let bpos_i = p
            " Caveat: Don't change cursor pos.
            let spos_i = s:current_element_terminal(0)
            let tpos_i = s:nearest_bracket(1)
        endif
        let idx += 1
    endwhile
    let bpos_o = p
    let spos_o = s:current_element_terminal(0)
    let tpos_o = s:nearest_bracket(1)

    " Determine dividing point for convolution: either...
    " 1. start of current element (when inside element)
    " 2. beginning of next element (when on inter-element whitespace)
    " 3. at cursor (when in whitespace preceding closing bracket)
    if cursor == tpos_i
        " Special Case: Cursor on closing bracket
        let pos = tpos_i
    else
        " Try to position on head of current element (including macro chars).
        call s:setcursor(cursor)
        let pos = s:current_element_terminal(0)
        if !pos[1]
            " Not on an element; move to next one's head.
            " Note: Returns current pos if no adjacent el, which is probably
            " as good a point as any.
            " Rationale: Emacs is extremely literal about the dividing point,
            " using cursor pos even in middle of an element!
            let pos = s:nearest_element_terminal(1, 0)
        endif
    endif
    " Record distance from dividing point to end of line to facilitate
    " subsequent cursor positioning
    " Rationale: Head of line may be changed by deletion and re-indent.
    let pos_edist = col([pos[1], '$']) - pos[2]
    " Will dividing point's distance from eol be changed by convolute?
    let edist_changing = tpos_i[1] == pos[1] && tpos_o[1] != pos[1]

    " Perform a splice killing backwards from pos on the inner form.
    " Note: Not using sexp#splice because it doesn't preserve macro
    " chars/brackets.
    " Note: Work backwards since deletions invalidate positions.
    let ket = s:yankdel_range(tpos_i, tpos_i, 1, 1)
    let del = s:yankdel_range(bpos_i, pos, 1, [0, 0])
    let bra = s:yankdel_range(spos_i, bpos_i, 1, 1)

    " Since the deleted text is going to be prepended to a list, make sure
    " that if it contains non-whitespace, it ends with whitespace. Normally,
    " our positioning on the start of an element will ensure this happens
    " naturally, but there are corner cases where it doesn't: e.g., when we're
    " positioned on a list closing bracket.
    if del =~ '\%(\S.*\)\@<=\S$'
        let del .= ' '
    endif

    " Note: Deletion above may have invalidated tpos_o; use bpos_o to find it.
    call s:setcursor(bpos_o)
    let tpos_o = s:nearest_bracket(1)
    call s:put_at(ket, 0, 0, tpos_o)
    call s:put_at(del, 1, 0, spos_o)
    call s:put_at(bra, 1, 0, spos_o)
    " If non-empty macro chars were pasted, move forward to bracket so we'll
    " know exactly what we're indenting.
    if len(bra) > 1
        call cursor(line('.'), col('.') + len(bra) - 1)
    endif

    " Indent the outer list *and* the one that contains it.
    call sexp#indent(0, 2)

    " Re-calculate pos for final cursor positioning.
    " Note: When outer list ends on a different line from inner list, the
    " convolution will decrease number of close brackets after pos by 1.
    " Assumption: Closing brackets always a single byte.
    let pos[2] = col([pos[1], '$']) - pos_edist + edist_changing
    call s:setcursor(pos)
endfu

" Return [start, end] of region to be cloned.
function! s:get_clone_target(mode, before)
    let cursor = getpos('.')
    if a:mode ==? 'v'
        return s:set_marks_around_current_element('v', 1, 0, 1)
    else
        " Get our bearings...
        let p = s:current_element_terminal(0)
        if !p[1]
            let p = getpos('.')
            " Not on an element. Find adjacent (if one exists in applicable
            " direction).
            call s:move_to_adjacent_element(!a:before, 0, 0)
            if p == getpos('.')
                " Nothing to clone at this level!
                return [[0, 0, 0, 0], [0, 0, 0, 0]]
            endif
        endif
        return s:set_marks_around_current_element('n', 1, 0, 1)
    endif
endfunction

function! sexp#clone(mode, count, before)
    let cursor = getpos('.')
    let wsv = winsaveview()
    " Get region to be cloned.
    let [start, end] = s:get_clone_target(a:mode, a:before)
    if !start[1]
        " Nothing to clone. TODO: How to handle...
        return
    endif
    " Assumption: Prior logic guarantees start and end at same level.
    let top = s:at_top(start[1], start[2])
    let multi = start[1] != end[1]
    let copy = s:yankdel_range(start, end, 0, 1)
    call s:setcursor(a:before ? start : end)
    let repl = multi
        \ ? a:before ? [copy, "\n"] : ["\n", copy]
        \ : a:before ? [copy, " "] : [" ", copy]
    let copy = join(repeat(repl, a:count ? a:count : 1), "")

    let lines_orig = line('$')
    let cur_eol = multi && !a:before && cursor[1] == end[1]
        \ ? s:offset_char(end, 1, 1)[2]
        \ : col([cursor[1], '$'])
    if a:mode ==? 'v'
        let start_eol = multi && !a:before && start[1] == end[1]
            \ ? s:offset_char(end, 1, 1)[2]
            \ : col([start[1], '$'])
        let end_eol = multi && !a:before
            \ ? s:offset_char(end, 1, 1)[2]
            \ : col([end[1], '$'])
    endif
    silent call s:put_at(copy, a:before, 0, a:before ? start : end)
    let lines_added = line('$') - lines_orig
    " Design Decision: Single line clone can't change indent.
    " Rationale: If it's wrong now, it was already wrong.
    if multi
        if top
            " Indent range
            " TODO: Consider whether to indent target or not.
            let [l1, l2] = [start[1], end[1] + lines_added]
            " How many lines were added?
            " Design Decision: Format both cloned element and clones.
            " Rationale: In some cases, a cloned element that was not the first
            " element on its line will be moved to a line of its own, and in such
            " cases, re-indenting is needed.
            " Note: := command doesn't do what you think...
            " TODO: Determine whether the special force_syntax logic added to
            " sexp#indent is required in this case as well.
            keepjumps exe 'normal! ' . l1 . 'G=' . l2 . "G"
        else
            " Indent parent
            " Note: Because of the way sexp#indent works, we need to know whether
            " cursor is on an open or close.
            let isl = s:is_list(line('.'), col('.'))
            " Caveat: Failure to set optional force_syntax flag in call to indent
            " may result in incorrect indentation.
            call sexp#indent(0, isl > 1 ? 2 : 1, 1)
        endif
        if a:before
            " Cursor has effectively moved.
            let wsv.lnum += lines_added
            " TODO: Decide whether it's better to keep view unchanged or to
            " allow text to shift down (to make it obvious something's
            " happened).
            let wsv.topline += lines_added
            " Design Decision: Don't adjust wsv.topline.
            " Rationale: When near start of file, it can prevent user from
            " noticing that anything changed (since added stuff would be above
            " view).
            " FIXME: If end is no longer in view, scroll up to put it at bottom or
            " start at top...
            if a:mode ==? 'v'
                let start[1] += lines_added
                let end[1] += lines_added
            endif
        endif
    endif

    if multi || a:before
        " Caveat: wsv.col uses zero-based index.
        let wsv.col += col([wsv.lnum, '$']) - cur_eol
        if a:mode ==? 'v'
            let start[2] += col([start[1], '$']) - start_eol
            let end[2] += col([end[1], '$']) - end_eol
        endif
    endif

    call winrestview(wsv)
    " TODO: Consider checking whether cloned element is partly below view and
    " scrolling up if so...

    " Handle visual selection.
    " Note: Restore *inner* version of original selection, since changes to
    " surrounding whitespace can make it impossible to define, let alone
    " restore, an outer selection.
    if a:mode ==? 'v'
        call s:set_visual_marks([start, end])
        " FIXME: Probably don't leave visual selection active.
        "call s:select_current_marks('v')
    endif
endfunction

" Remove brackets from current list, placing cursor at position of deleted
" first bracket. Takes optional count parameter, which specifies which pair of
" ancestor brackets to remove.
function! sexp#splice_list(...)
    call s:set_marks_characterwise()

    let marks = s:get_visual_marks()
    let cursor = getpos('.')

    " Climb the expression tree a:1 times
    if a:0 && a:1 > 1
        let idx = a:1
        let dir = getline(cursor[1])[cursor[2] - 1] =~ s:opening_bracket
        while idx > 0
            call s:move_to_nearest_bracket(dir)
            let idx -= 1
        endwhile
    endif

    call s:set_marks_around_current_list('n', 0, 0)

    let start = getpos("'<")

    if start[1] > 0
        " Delete ending bracket first so we don't mess up '<
        call s:setcursor(getpos("'>"))
        normal! dl
        call s:setcursor(start)
        normal! dl
    else
        call s:setcursor(cursor)
    endif

    call s:set_visual_marks(marks)
endfunction

" Capture or emit the first or last element into or out of the current list.
" The cursor will be placed on the new bracket position, or if mode is 'v',
" the resulting list will be selected.
"
" For implementation simplicity a list will never emit its last element, or
" capture its containing list.
function! sexp#stackop(mode, last, capture)
    let [_b, cursorline, cursorcol, _o] = getpos('.')
    let char = getline(cursorline)[cursorcol - 1]

    if a:mode ==? 'v'
        execute "normal! \<Esc>"
        let marks = s:get_visual_marks()
    endif

    " Move to element tail first so we can skip leading macro chars
    let pos = s:move_to_current_element_terminal(1)

    " Move to closing bracket unless we are on one
    if !(pos[1] > 0 && getline(pos[1])[pos[2] - 1] =~# s:closing_bracket)
        let pos = s:move_to_nearest_bracket(1)
    endif

    try
        " No paired bracket found, so not in a list
        if pos[1] < 1 | throw 'sexp-error' | endif

        if a:last
            let bpos = pos
        else
            let bpos = s:move_to_nearest_bracket(0)
            let pos = s:move_to_current_element_terminal(0)
        endif

        if !(a:capture ? s:stackop_capture(a:last, pos, bpos)
                     \ : s:stackop_emit(a:last, pos, bpos))
            throw 'sexp-error'
        endif

        if a:mode ==? 'v'
            call sexp#select_current_element('n', 1)
        endif
    catch /sexp-error/
        " Cleanup after error
        if a:mode ==? 'v'
            call s:set_visual_marks(marks)
            normal! gv
        else
            call cursor(cursorline, cursorcol)
        endif
    endtry
endfunction

" Exchange the current element with an adjacent sibling element. Does nothing
" if there is no current or sibling element.
"
" If list equals 1, the current list is treated as the selected element.
"
" If mode equals 'v' (regardless of the value of list), the current selection
" is expanded to include any partially selected elements, then is swapped
" with the next element as a unit. If the selection contains an even number
" of elements, the swap is done with the next couple of elements in order to
" maintain the original associative structure of the list. Visual marks are
" set to the new position and visual mode is re-entered.
"
" Note that swapping comments with other elements can lead to structural
" imbalance since trailing brackets may be included as part of a comment after
" a swap. Fixing this is on the TODO list.
function! sexp#swap_element(mode, next, list)
    let visual = a:mode ==? 'v'
    let cursor = getpos('.')
    let pairwise = 0

    " Moving listwise with a:mode 'v' will be treated like a regular
    " element-wise swap.
    if visual
        let marks = s:get_visual_marks()

        " Ensure visual marks are set character-wise
        call s:select_current_marks('v')
        execute "normal! \<Esc>"

        call s:set_visual_marks(s:positions_with_element_terminals(marks))
        let pairwise = (call('s:count_elements', s:get_visual_marks()) % 2) == 0
    " Otherwise select the current list or element (with leading macro chars)
    elseif a:list
        " Move to element end first in case we are on leading macro chars
        let pos = s:current_element_terminal(1)
        let tail = (pos[1] > 0 && getline(pos[1])[pos[2] - 1] =~# s:closing_bracket)
                   \ ? pos
                   \ : s:nearest_bracket(1)
        if tail[1] < 1
            delmarks < >
        else
            call s:setcursor(tail)
            call s:set_marks_around_current_element('o', 1)
        endif
    else
        call s:set_marks_around_current_element('o', 1)
    endif

    if getpos("'<")[1] < 1 || !s:swap_current_selection(a:mode, a:next, pairwise)
        " Restore visual state
        if visual
            call s:set_visual_marks(marks)
            normal! gv
        endif
        call s:setcursor(cursor)
    endif
endfunction

""" ITERATION {{{1

" Call func count times with given varargs. Will call func at least once.
" Stores current evaluation iteration (from 0 to count, exclusive) in
" s:countindex.
function! sexp#docount(count, func, ...)
    try
        for n in range(a:count > 0 ? a:count : 1)
            let s:countindex = n
            call call(a:func, a:000)
        endfor
    finally
        let s:countindex = 0
    endtry
endfunction

""" INSERTION EXPRESSIONS {{{1

" Return keys to be inserted in place of bra; this includes the closing pair,
" as well as a leading and/or trailing space to separate from other elements.
"
" Returns bra if cursor is in s:ignored_region or is preceded by a single
" backslash.
function! sexp#opening_insertion(bra)
    let [_b, line, col, _o] = getpos('.')

    if s:syntax_match(s:ignored_region, line, col)
        \ && s:compare_pos(s:current_element_terminal(0), [0, line, col, 0]) < 0
        return a:bra
    endif

    let curline = getline(line)
    let cur = curline[col - 1]
    let prev = curline[col - 2]
    let pprev = curline[col - 3]

    if prev ==# '\' && pprev !=# '\'
        return a:bra
    endif

    let buf = ''
    let buftail = ''
    let ket = s:pairs[a:bra]

    if prev =~# '\v\S'
        \ && prev !~# s:opening_bracket
        \ && !s:is_macro_char(prev)
        let buf .= ' '
    endif

    let buf .= a:bra . ket
    let buftail .= "\<C-G>U\<Left>"

    if cur =~# '\v\S' && cur !~# s:closing_bracket
        let buf .= ' '
        let buftail .= "\<C-G>U\<Left>"
    endif

    return buf . buftail
endfunction

" Return keys to be inserted in place of ket:
"
"   * Insert ket if cursor is in s:ignored_region or is preceded by a single
"     backslash
"   * Skip current char if equal to ket
"   * Jump to next closing ket if current list is balanced
"   * Insert ket if current list is unbalanced
"
function! sexp#closing_insertion(ket)
    let [_b, line, col, _o] = getpos('.')

    let curline = getline(line)
    let cur = curline[col - 1]
    let prev = curline[col - 2]
    let pprev = curline[col - 3]

    if s:syntax_match(s:ignored_region, line, col)
        \ && s:compare_pos(s:current_element_terminal(0), [0, line, col, 0]) < 0
        return a:ket
    elseif prev ==# '\' && pprev !=# '\'
        return a:ket
    elseif cur ==# a:ket
        return "\<C-G>U\<Right>"
    endif

    let bra = '\V' . s:pairs[a:ket]
    let ket = '\V' . a:ket
    let open = cur =~# s:opening_bracket
               \ ? [0, line, col, 0]
               \ : s:nearest_bracket(0, bra, ket)

    " No enclosing list; insert nothing
    if open[1] < 1
        return ''
    endif

    let close = s:nearest_bracket(1, bra, ket)

    " Brackets are balanced, jump to closing bracket
    if close[1] > 0
        return "\<C-o>:\<C-u>call cursor(" . close[1] . ", " . close[2] . ")\<CR>"
    else
        return a:ket
    endif
endfunction

" Return keys to be inserted in place of quote:
"
"   * If in a string, always insert quote if previous char is a backslash
"   * If in a string, insert quote unless current char is a quote
"   * Insert quote if cursor is in s:ignored_region or is preceded by a single
"     backslash
"   * Otherwise insert pair of quotes with a leading and/or trailing space to
"     separate from other elements.
"
function! sexp#quote_insertion(quote)
    let [_b, line, col, _o] = getpos('.')

    if s:syntax_match(s:string_region, line, col)
        let curline = getline(line)

        " User is trying to insert an escaped quote, so do it
        if curline[col - 2] ==# '\'
            return a:quote
        else
            return curline[col - 1] ==# a:quote ? "\<C-G>U\<Right>" : a:quote
        endif
    elseif s:syntax_match(s:ignored_region, line, col)
        return a:quote
    else
        let curline = getline(line)
        let cur = curline[col - 1]
        let prev = curline[col - 2]
        let pprev = curline[col - 3]

        if prev ==# '\' && pprev !=# '\'
            return a:quote
        endif

        let buf = ''
        let buftail = ''

        if prev =~# '\v\S'
            \ && prev !~# s:opening_bracket
            \ && !s:is_macro_char(prev)
            let buf .= ' '
        endif

        let buf .= a:quote . a:quote
        let buftail .= "\<C-G>U\<Left>"

        if cur =~# '\v\S' && cur !~# s:closing_bracket
            let buf .= ' '
            let buftail .= "\<C-G>U\<Left>"
        endif

        return buf . buftail
    endif
endfunction

" Return keys to be inserted when deleting backward:
"
"   * Delete adjacent double quotes when previous position is in a string,
"     unless the first quote is preceded by another quote or a backslash
"   * Delete adjacent paired brackets, unless cursor is in s:ignored_region or
"     preceded by a single backslash
"   * Normal backspace otherwise
"
function! sexp#backspace_insertion()
    let [_b, line, col, _o] = getpos('.')
    let curline = getline(line)
    let cur = curline[col - 1]
    let prev = curline[col - 2]
    let pprev = curline[col - 3]
    let ppprev = curline[col - 4]
    let escaped = pprev ==# '\' && ppprev !=# '\'

    if prev ==# '"' && cur ==# '"'
        \ && s:syntax_match(s:string_region, line, col)
        \ && !escaped
        \ && pprev !~# '"'
        return "\<BS>\<Del>"
    elseif !s:syntax_match(s:ignored_region, line, col)
        \ && !escaped
        \ && prev =~# s:opening_bracket
        \ && cur ==# s:pairs[prev]
        return "\<BS>\<Del>"
    else
        return "\<BS>"
    endif
endfunction
