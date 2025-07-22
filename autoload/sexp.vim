

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

fu! s:Dbg(...)
    "call luaeval("require'dp':get'sexp':logf(unpack(_A))", a:000)
endfu

"let s:prof_ts = 0
"fu! s:prof_start()
"    let s:prof_ts = reltime()
"endfu
"
"fu! s:prof_end(key)
"    if has('nvim')
"        call luaeval("require'sexp.prof':add(_A[1], _A[2])", [a:key, reltimefloat(reltime(s:prof_ts))])
"    endif
"endfu

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
let s:match_ignored_region_fn = 's:is_rgn_type("str_com_chr", line("."), col("."))'
let s:nomatch_ignored_region_fn = '!s:is_rgn_type("str_com_chr", line("."), col("."))'
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

" Define patterns matching the syntax groups corresponding to various special region
" types, and combinations thereof.
let s:rgn_patts = {
            \ 'string': '\vstring|str_lit|regex|pattern',
            \ 'comment': 'comment',
            \ 'str_com_chr': '\vstring|str_lit|regex|pattern|comment|character',
            \ 'str_com': '\vstring|str_lit|regex|pattern|comment'
\ }

" Default value corresponds to user weight of 5. If 'adjust' is set to 0.20, adjustments
" will be linear from 0 to twice the default, but 'adjust' may be set smaller to prevent
" the weight from dropping all the way to zero.
" TODO: Is a more complex approach warranted? E.g., an offset in addition to the slope?
let s:align_eolc_weights = {
    \ 'ngrps':   {'default': 50, 'adjust': 0.10},
    \ 'shift':   {'default': 50, 'adjust': 0.20},
    \ 'runt':    {'default': 50, 'adjust': 0.20},
    \ 'density': {'default': 50, 'adjust': 0.20},
\ }

let s:nullpos = [0,0,0,0]
let s:nullpos_pair = [s:nullpos, s:nullpos]

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

""" USER INTERFACE {{{1

" Display warning
function! s:warnmsg(s)
    try
        echohl WarningMsg
        echo a:s
    finally
        echohl None
    endtry
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

let s:sexp_ve_save = 0
function! sexp#pre_op(mode, name)
    if type(s:sexp_ve_save) == 0
        let s:sexp_ve_save = &ve
    endif
    set ve=onemore
    if !exists('b:sexp_cmd_prev_cache')
        let b:sexp_cmd_prev_cache = {}
    endif
    let b:sexp_cmd_cache = s:make_cache(a:mode == 'x' ? 'v' : a:mode, a:name)
endfunction

function! sexp#post_op(mode, name)
    " Restore original 'virtualedit' setting.
    " Assumption: This is called from finally block.
    let &ve = s:sexp_ve_save
    " Set to integer so we can tell when it's in use.
    let s:sexp_ve_save = 0
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

" Return true iff cursor is on bracket of specified type.
function! s:on_bracket(closing)
    let patt = a:closing ? s:closing_bracket : s:opening_bracket
    let [line, col] = [line('.'), col('.')]
    if getline(line)[col - 1] =~ patt
        " Maybe...
        return !s:is_rgn_type('str_com_chr', line, col)
    endif
    return 0
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
    let [_b, line, col, _o] = sexp#current_element_terminal(0)

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

fu! s:current_atom_terminal_ts(dir)
    return luaeval(
                \ "require'sexp.ts'.current_atom_terminal(_A[1])", [a:dir])
endfu

fu! s:current_atom_terminal_legacy(end)
    let [_, cursorline, cursorcol, _] = getpos('.')
    if !s:is_atom(cursorline, cursorcol)
        return [0, 0, 0, 0]
    endif

    let termline = cursorline
    let termcol = cursorcol

    " FIXME: Don't use s:findpos like this; there are faster ways to handle multi-byte.
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
endfu

" Position of start/end of current atom: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in an atom. Assumes atoms never span multiple
" lines.
function! s:current_atom_terminal(end)
    return s:current_atom_terminal_{s:ts_or_legacy()}(a:end)
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
    elseif char =~# s:delimiter && !s:is_rgn_type('str_com_chr', a:line, a:col)
        return 0
    else
        return !s:is_rgn_type('str_com', a:line, a:col)
    endif
endfunction

" Position of start/end of current string: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in a string.
function! s:current_string_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')

    let [_, termline, termcol, _] = s:current_region_terminal('string', a:end)
    if !termline
        return [0, 0, 0, 0]
    endif

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

    let ret = s:current_region_terminal('comment', a:end)
    call cursor(cursorline, cursorcol)
    return ret
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
" Important Note: This is an autoload function because it's called from Lua treesitter code.
function! sexp#current_element_terminal(end)
    let [_b, line, col, _o] = getpos('.')
    let char = getline(line)[col - 1]
    let include_macro_characters = !a:end

    if s:is_rgn_type('string', line, col)
        let pos = s:current_string_terminal(a:end)
    elseif s:is_comment(line, col)
        let pos = s:current_comment_terminal(a:end)
    elseif char =~# s:bracket && !s:is_rgn_type('str_com_chr', line, col)
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
                let pos = sexp#current_element_terminal(0)
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
                let pos = sexp#current_element_terminal(1)
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
" Returns current element's terminal if no adjacent element exists, unless optional
" 'ignore_current' argument is set, in which case, return unmodified current position.
" TODO: Currently, returns input position unmodified if no adjacent and no current. Is
" this best approach? It's always worked like this, so changing it to (eg) null pos should
" not be done without significant analysis/testing.
function! s:nearest_element_terminal(next, tail, ...)
    let cursor = getpos('.')
    let pos = cursor
    " If optional flag is set, keep original position if no adjacent element (as opposed
    " to returning current terminal in desired direction).
    let ignore_current = a:0 && a:1

    try
        let terminal = sexp#current_element_terminal(a:next)

        if terminal[1] > 0 && s:compare_pos(pos, terminal) != 0
            if !ignore_current
                let pos = terminal
            endif
            call s:setcursor(terminal)
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
            let final = sexp#current_element_terminal(a:tail)
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
" position under the cursor, even if on a blank line, unlike synID, which
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

fu! s:prefer_treesitter()
    return (!g:sexp_prefer_legacy_syntax || empty(&syn) || &syn ==? "off") && has('nvim')
endfu

fu! s:ts_or_legacy()
    return s:prefer_treesitter() ? "ts" : "legacy"
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
    " Note: Most likely, caller has already verified cursor in region, but if it isn't, we
    " should return null pos.
    let [termline, termcol] = [0, 0]
    let maxline = line('$')
    let in_rgn = 1
    while in_rgn && line <= maxline && line >= 1
        " Loop over *bytes* on line.
        " Design Decision: The following byte-based approach should be safe in the
        " presence of multi-byte because we're using the col positions only as arguments
        " to synstack() and friends, *not* using them to access individual bytes/chars.
        " The only drawback to this approach is that it will result in redundant calls
        " within multi-byte chars, but this will almost always be faster than the old
        " approach, which iterated the chars by searching for '\_.'.
        let eol = col([line, '$'])
        " Note: The col == 1 condition ensures we'll perform a single check on a blank
        " line. Rationale: Failing to check on a blank line would cause (eg) comments to
        " continue across blank lines (which might make sense, but would break with legacy
        " behavior).
        while (col < eol || col == 1) && col >= 1
            "call s:Dbg("%d,%d: is_comment", line, col)
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
        " Caveat: Don't let col go below 1 on empty line.
        let col = a:dir ? 1 : max([col([line, '$']) - 1, 1])
    endwhile
    "call s:Dbg("current_region_terminal_legacy returning %d, %d", termline, termcol)
    return [0, termline, termcol, 0]
endfu

fu! s:current_region_terminal(rgn, end)
    return s:current_region_terminal_{s:ts_or_legacy()}(a:rgn, a:end)
endfu

" Return start of leading (0) or end of trailing (1) whitespace from pos.
" Returns pos if no such whitespace exists.
function! s:adjacent_whitespace_terminal(pos, trailing)
    let cursor = getpos('.')
    call s:setcursor(a:pos)

    let [_b, termline, termcol, _o] = a:pos

    " FIXME: This is an inefficient way to do this! Fix it!
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

" Analyze the text before/after start/end and fill the return dictionary with fields that
" completely characterize it, with the goal of supporting the logic in
" s:terminals_with_whitespace() that determines the optimal visual selection for an outer
" element.
function! s:terminals_with_whitespace_info(start, end, leading)
    let cursor = getpos('.')
    let o = {}
    " Get text from BOL to start (exclusive).
    " FIXME: Get bol_text without the double indexing.
    let bol_text = getline(a:start[1])[: a:start[2] - 1][: -2]
    let eol_text = getline(a:end[1])[a:end[2] - 1 :]

    let [o.start, o.end, o.leading] = [a:start, a:end, a:leading]
    " Note: bol/eol flags apply to the start/end of selection, *not* the start/end of the
    " surrounding whitespace.
    let o.bol = bol_text =~ '^\s*$'
    let o.eol = eol_text =~ '^.\s*$'
    " Are we at beginning of sexp?
    call s:setcursor(a:start)
    let p = s:nearest_element_terminal(0, 1)
    " Caveat: This test assumes s:nearest_element_terminal() returns current position if no
    " preceding element.
    let o.bos = s:compare_pos(p, a:start) >= 0
    let o.follows_com = !o.bos && s:is_comment(p[1], p[2])
    let o.follows_list = !o.bos && s:is_list(p[1], p[2])
    " FIXME: Get rid of o.next_e if only o.next_s is needed.
    " Get bounds of prev or open.
    if !o.bos
        " Save bounds of prev element.
        call s:setcursor(p)
        let ps = sexp#current_element_terminal(0)
        let [o.prev_s, o.prev_e] = [ps, p]
        let o.open = s:nullpos
    else
        " Save position of open
        let o.open = s:nearest_bracket(0)
        let [o.prev_s, o.prev_e] = s:nullpos_pair
    endif
    call s:setcursor(o.start)
    " Is current element a comment? Make sure we're on an element before testing.
    " FIXME: Is this needed? Why do we care what current element is?
    let p = sexp#current_element_terminal(0)
    if !p[1]
        " TODO: Do we need to handle null pos here? Is it even possible?
        let p = s:nearest_element_terminal(1, 0)
    endif
    let o.is_com = s:is_comment(p[1], p[2])
    " Are we at end of sexp?
    call s:setcursor(a:end)
    let p = s:nearest_element_terminal(1, 0)
    let o.eos = s:compare_pos(p, a:end) <= 0
    let o.precedes_com = !o.eos && s:is_comment(p[1], p[2])
    let o.precedes_list = !o.eos && s:is_list(p[1], p[2])
    " Get next and prev element extents.
    if !o.eos
        " Save bounds of next element.
        " Note: Currently, this is required only in list case. Consider optimization.
        call s:setcursor(p)
        let pe = sexp#current_element_terminal(1)
        let [o.next_s, o.next_e] = [p, pe]
        let o.close = s:nullpos
    else
        " Save position of close
        let o.close = s:nearest_bracket(1)
        let [o.next_s, o.next_e] = s:nullpos_pair
    endif
    " Save number of whitespace-only lines preceding and following selection.
    " TODO: Consider calculating this with ws_<...> without open/close/next/prev.
    " Note: These expressions treat buffer start/end as (nonexistent) lines 0 and $+1.
    let o.preceding_line_gap = o.start[1] - (o.bos ? o.open[1] : o.prev_e[1])
    let o.following_line_gap =
            \ (o.eos ? o.close[1] : o.next_s[1] ? o.next_s[1] : line('$')+1)
            \ - o.end[1]
    " Find end of any sequences of whitespace immediately preceding start or following
    " end. (Returns input pos if no such whitespace.)
    " Note: s:adjacent_whitespace_terminal can return positions on blank lines.
    let o['ws_s'] = s:adjacent_whitespace_terminal(o.start, 0)
    let o['ws_e'] = s:adjacent_whitespace_terminal(o.end, 1)
    " Set virtual start/end, which is the same as non-virtual start/end except when last
    " non-newline whitespace is at bol or eol, in which case virtual pos is newline at end
    " of preceding line (bol case) or newline just past ws_e (eol case).
    " TODO: Does this rely on ve=onemore? I don't think so, but if not, what does?
    let o.ws_vs = o.ws_s[2] == 1 && o.ws_s[1] > 1
        \ ? [0, o.ws_s[1] - 1, col([o.ws_s[1] - 1, '$']), 0]
        \ : o.ws_s
    " If trailing ws ends in newline, use that, else ws_e.
    " Note: Logic to determine whether o.ws_e is at eol slightly complicated by
    " possibility of multi-byte whitespace.
    let ecol = col([o.ws_e[1], '$'])
    let o.ws_ve = s:offset_char(o.ws_e, 1, 1)[2] >= ecol
        \ ? [0, o.ws_e[1], ecol, 0]
        \ : o.ws_e
    " Set *interior* end positions, which are pulled in a bit from the end of the
    " trailing whitespace: if possible, by excluding the outermost 1 or 2 newlines
    " (option-dependent line offset value calculated below), else by excluding a single
    " whitespace char (if possible).
    " Important Note: Determine line offset for both sides, taking the pertinent option
    " into account.
    " Logic: If at least one blank following or no blanks preceding, let ws_ei handle
    " leaving the extra blank; else let ws_si handle it.
    " Note: I'm thinking we may never actually need both offsets.
    let start_assures_newline =
                \ o.following_line_gap < 2 && o.preceding_line_gap > o.following_line_gap
    " Determine option-dependent pullback line offset.
    let off = g:sexp_cleanup_keep_empty_lines + 1
    if o.ws_vs[1] < o.start[1]
        " At least one newline in leading whitespace
        if start_assures_newline
            " Note: min() prevents pullback past start.
            let l = min([o.start[1], o.ws_vs[1] + off])
            let o.ws_si = [0, l, 1, 0]
        else
            " Dependency Logic: ws_ei will assure the newline, so make sure leading
            " whitespace doesn't include any.
            " Note: The dependency logic both here and in set of ws_ei allows call site to
            " use ws_si and ws_ei in the case in which they're interdependent (bol^eol==0)
            " without considering preceding/following_line_gap, etc.
            let o.ws_si = o.ws_vs
        endif
    else
        " No newlines in leading whitespace. Exclude 1 whitespace char *if possible*.
        let o.ws_si = s:compare_pos(o.ws_s, o.start) >= 0 ? o.start[:] : s:offset_char(o.ws_s, 0)
    endif

    " Special Case: Ordinarily, ws_ve != ws_e indicates trailing whitespace ends with
    " newline; however, ws_ve == ws_e when trailing whitespace ends with blank line.
    if o.ws_ve != o.ws_e || o.ws_ve[1] > o.end[1]
            " At least one newline in trailing whitespace
        if start_assures_newline
            " Dependency Logic: ws_si will assure the newline, so include everything up to
            " beginning of last line of trailing whitespace (including the preceding
            " newline).
            let o.ws_ei = o.ws_e != o.ws_ve
                        \ ? o.ws_ve
                        \ : [0, o.ws_ve[1] - 1, col([o.ws_ve[1] - 1, '$']), 0]
        else
            " Caveat: Special handling required for blank line.
            " Explanation: On a blank line, the only possible cursor position is 1 past
            " end of line (effectively *on* the newline); thus, to exclude that newline
            " from the selection, it's necessary to go back to one past end of preceding
            " line. Note that if there were even a single whitespace char on the line, we
            " could rewind to it to exclude the newline.
            "
            " Get reference line: i.e., line from which pullback is measured. For trailing
            " whitespace ending in newline, this will be ws_ve + 1, else ws_ve.
            " Note: A special test is required for blank lines, for which ws_ve == ws_e.
            " Note: min()/max() used to prevent pulling back prior to buffer start.
            let l = col([o.ws_ve[1], '$']) == 1 || o.ws_ve != o.ws_e ? o.ws_ve[1] + 1 : o.ws_ve[1]
            " Determine how far to pull back, careful not to surpass end of selection.
            let off = min([l - o.end[1], off])
            " Note: Calculate the nominal target line, and the one prior to it, which is
            " needed only in special case of blank target line, for reasons described earlier.
            let [l_tgt, l_tgt_prev] = [l - off, max([o.end[1], l - off - 1])]
            " Ternary ensures that if target pullback line has no non-newline char, we
            " adjust back to newline of line *preceding* target line.
            let ecol = col([l_tgt, '$'])
            let o.ws_ei = ecol == 1
                        \ ? [0, l_tgt_prev, col([l_tgt_prev, '$']), 0]
                        \ : [0, l_tgt, ecol - 1, 0]
        endif
    else
        " No newlines in leading whitespace. Exclude 1 whitespace char *if possible*.
        let o.ws_ei = s:compare_pos(o.ws_e, o.end) <= 0 ? o.end[:] : s:offset_char(o.ws_e, 0)
    endif
    " De-normalized multi-line flag for convenience
    " Note: Single-line context is very restrictive: any scenario in which we'll have to
    " decide whether to include newlines will be a multi-line scenario.
    " Design Decision: Treat multi-line selection as single-line join if !bol and !eol.
    " Rationale: Looking back from start and forward from end, it's exactly the same
    " situation as the single-line selection case; thus, the handling should be identical.
    "let o.ml = o.ws_vs[1] != o.ws_ve[1] || o.eol
    let o.ml = o.ws_vs[1] != o.ws_ve[1] || o.eol
    " Set some convenience flags representing various attributes of start/end.
    " Note: In this context, head/tail refers to start/end of unbroken sequence of
    " leading/trailing whitespace (potentially on lines other than a:start/a:end).
    " Also, the final (terminal) newline is treated differently than any intermediate
    " newlines: e.g., the 'ws' flag can be set by a blank line, but not by a newline
    " separating elements. Moreover, ws_vs/ws_ve are set to point to these terminal
    " newlines, while ws_s/ws_e are not.
    " Flags:
    "   ws:    whitespace exists (including blank lines but not terminal newline)
    "   eol:   head/tail at beginning/end of line
    "   real:  actual whitespace char (not newlinw) at head/tail
    "   spc:   literal SPACE char at head/tail
    "   chr:   the char at non-virtual head/tail
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

" Return integer indicating the 0-based offset in list of the element at the specified
" position, else -1 if position not inside a list.
function! s:is_list_head(pos)
    let save_cursor = getpos('.')
    try
        call s:setcursor(a:pos)
        " Attempt to find previous element.
        let p = s:nearest_element_terminal(0, 1, 1)
        if p != a:pos
            " Previous element implies not head of list.
            " Note: In many cases, this allows short-circuiting a test for top-level.
            return 0
        endif
        " No previous element, but we still need to check whether we're in list.
        return !s:at_top(a:pos[1], a:pos[2])
    finally
        call s:setcursor(save_cursor)
    endtry
endfunction

" Using options and the input object produced by s:terminals_with_whitespace_info(),
" return true iff selection should be adjusted to ensure that, upon delete, the element
" following selection will be appended to the element before the selection.
" Note: Take all relevant options into account.
" join_affinity:
"     0 = never join
"     1 = append only to head of list
"     2 = append anywhere but top-level (subject to other constraints)
"     3 = append anywhere (including top-level, subject to other constraints)
" join_multiline
"     0 = allow append of single line elements only
"     1 = allow append of both single and multi-line elements
" join_tw
"     0 = disables checking (as with 'tw')
"     -1 use &tw
"     else override &tw
function! s:outer_element_can_append(twwi)
    let o = a:twwi
    let tw = g:sexp_cleanup_join_textwidth < 0 ? (&tw ? &tw : 80) : g:sexp_cleanup_join_textwidth
    let [affinity, ml] =
        \ [g:sexp_cleanup_join_affinity, g:sexp_cleanup_join_multiline]
    " Note: affinity shouldn't be less than zero, but err on side of disabling...
    if affinity <= 0 || !o.next_s[1]
        " Joining disabled or no next element to append.
        return 0
    endif
    " Is join precluded by context?
    " Logic:
    " * Don't append to an element that wasn't already followed by an element on its line.
    " * Don't join elements that were separated by a comment
    " * Don't append a comment to anything or anything to a comment
    if o.bol || o.is_com || o.precedes_com || o.follows_com
        return 0
    endif
    " Is join precluded by line length constraint?
    if tw > 0 && tw < o.prev_e[2] + col([o.next_s[1], '$']) - o.next_s[2]
        return 0
    endif
    if !ml && o.next_s[1] != o.next_e[1]
        " Only single-line joins permitted.
        return 0
    endif
    " Apply affinity constraints.
    if affinity == 1 && !s:is_list_head(o.prev_e)
        " Affinity is for appends to list head, but prev element is not list head.
        return 0
    endif
    " Now that all other checks have passed, do top-level check only if necessary.
    if affinity < 3 && s:at_top(o.start[1], o.start[2])
        return 0
    endif
    return 1
endfunction

" Return 1 to prioritize preservation of leading indent, 0 to prioritize removal of
" leading whitespace.
function! s:prioritize_leading_indent(twwi)
    let o = a:twwi
    let [cs, css, lsl] =
            \ [g:sexp_cleanup_colshift, g:sexp_cleanup_colshift_slope, g:sexp_cleanup_lineshift_limit]
    " Short-circuit in left-margin case.
    if o.start[2] == 1 | return 0 | endif
    " Short-circuit in top-level case.
    if s:at_top(o.start[1], o.start[2]) | return 0 | endif
    " Short-circuit if removal of leading whitespace wouldn't remove any lines.
    if o.ws_si[1] >= o.start[1] | return 1 | endif
    " Short-circuit if options preclude possibility of preferring leading blanks.
    if !cs && !css && !lsl | return 1 | endif
    " Guarantee: At least one nonzero option value needs to be considered.
    " Since g:sexp_cleanup_leading_newlines takes precedence, consider it first.
    let lshift = o.start[1] - o.ws_si[1]
    if lsl > 0 &&  lshift >= lsl
        " Prioritize leading whitespace removal.
        return 0
    endif
    " Calculate col shift threshold according to linear eq.
    let cshift = cs + css * lshift
    " Short-circuit if zero cshift precludes need for comparison.
    if !cshift | return 1 | endif
    " Use leading indent (in screen width) of start of selection to make final decision.
    " Note: The -2 offset accounts for 1-to-0-based index conversion and excludes the
    " start char itself from the calculation (since we're calculating width of what
    " precedes it).
    return strdisplaywidth(getline(o.start[1])[:o.start[2] - 2]) >= cshift
endfunction

" Given start and end positions, returns new positions [start', end'],
" according to logic described below.
" If (bos or eos) and !(precedes_com || follows_com)
" 	Include *all* leading and trailing whitespace.
" ElseIf eol
" 	If !bol
" 	  If next element isn't multiline and joining it to start line wouldn't violate 'textwidth'
" 	    end' includes all trailing whitespace up to the next element
" 	  Else
"       start' includes all leading whitespace
"       end' includes all trailing whitespace up to the end of line preceding next element.
"   Else
"     " Rationale: Don't delete leading indent, as it could discombobulate
"     If precedes_com
"       end' includes all trailing whitespace up to the end of line preceding next element.
"     Else
"       end' includes the trailing whitespace up to the next element (or EOB)
" Else " Element follows on same line
"   end' includes trailing whitespace
"
" This behavior diverges from the behavior of the native text object aw in
" that it allows multiline whitespace selections.
function! s:terminals_with_whitespace(start, end)
    let [start, end] = [a:start, a:end]

    " TODO: Rename as get_ctx or some such...
    let o = s:terminals_with_whitespace_info(start, end, [])
    if (o.bos || o.eos) && (!o.precedes_com && !o.follows_com)
        " No need to preserve any whitespace adjacent to bracket; include *all*
        " leading/trailing whitespace.
        " Note: s:outer_element_can_append() is the only other place we require special
        " handling for comments and begin/end of sexp.
        let [start, end] = [o.ws_vs, o.ws_ve]
    elseif o.eol
        " Consider join, but only if it won't violate option-dependent constraints.
        " Note: Comment constraints are applied by outer_element_can_append().
        if s:outer_element_can_append(o)
            " Note: Ideally, we would select all trailing ws, excluding leading ws to
            " preserve alignment of next element; however, in pathological scenario in
            " which there's only trailing ws, exclude it to ensure a subsequent delete
            " won't result in spurious join.
            let end = o.start == o.ws_s && o.end != o.ws_e ? o.ws_ei : o.ws_ve
        elseif !o.bol
            " Decided not to append.
            let [start, end] = [o.ws_vs, o.ws_ei]
        else " eol && bol
            " ws_si/ws_ei are not independent: terminals_with_whitespace_info() has set
            " them correctly.
            let [start, end] = [o.ws_si, o.ws_ei]
        endif
    else " !eol
        if !o.bol
            " Note: bos/eos case handled elsewhere; no need to consider here...
            " No choice about join, but leave original whitespace at head to ensure next
            " element is aligned with selection start after delete.
            " Special Case: In pathological case of no whitespace at head, try to leave it
            " at tail to prevent spurious join.
            let end = start == o.ws_e ? o.ws_ei : o.ws_ve
        else " o.bol
            " Select all trailing ws to pull next element back to selection start.
            let end = o.ws_ve
            " Use options and selection context to decide whether to pull in leading
            " whitespace.
            " Rationale: Pulling in leading whitespace entails the loss of leading indent
            " before start, which can be disorienting to user, especially when there was a
            " lot of it (e.g., inside deeply-nested form).
            if !s:prioritize_leading_indent(o)
                let start = o.ws_si
            endif
        endif
    endif
    return [start, end]
endfunction

" Extend given positions to the terminals of any partially contained elements.
" If there exist any unpaired brackets in the region, the positions are
" extended to include those lists.
function! s:positions_with_element_terminals(positions)
    let cursor = getpos('.')
    let [start, end] = a:positions

    call s:move_to_element_near_position(start)
    let head = sexp#current_element_terminal(0)

    call s:move_to_element_near_position(end)
    let tail = sexp#current_element_terminal(1)

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
        " Caveat: searchpos() returns [0,0] if no bracket found before EOF.
        if line && s:is_rgn_type('str_com_chr', line, col)
            call cursor(line, col)
            call s:move_to_adjacent_element(1, 0, 0)
            continue
        endif

        " Break if bracket found after end or EOF hit by searchpos (!line).
        let cmp = !line ? 1 : s:compare_pos([0, line, col, 0], a:end)
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
"let g:Oc = function('s:offset_char')

function! s:super_range_ts(start, end)
    let ret = luaeval(
                \ "require'sexp.ts'.super_range(_A[1], _A[2])", [a:start, a:end])
    " Note: Convert nil returned by lua function to nullpos pair.
    return !empty(ret) ? ret : [[0, 0, 0, 0], [0, 0, 0, 0]]
endfunction

function! s:super_range_legacy(start, end)
    let cursor = getpos('.')
    let [start, end] = [a:start[:], a:end[:]]
    " Find matching pair of brackets (if one exists) that contains both start and end. Set
    " shared_close to the close position, or null if no such pair exists.
    " Note: In this context, a bracket "contains" itself.
    call s:setcursor(start)
    " Seed the loop position with an open containing start (possibly start itself).
    let shared_open = s:is_list(start[1], start[2]) == 2 ? start : s:move_to_nearest_bracket(0)
    while shared_open[1]
        let shared_close = s:nearest_bracket(1)
        " Note: Null shared close implies end at top level due to unbalanced open.
        let cmp = !shared_close[1] ? 1 : s:compare_pos(shared_close, end)
        if cmp >= 0
            " Either we found shared close or we're not going to.
            break
        endif
        " Haven't yet found shared close (and haven't hit top-level trying). Adjust
        " start to current open bracket before looking higher.
        let start = shared_open
        let shared_open = s:move_to_nearest_bracket(0)
    endwhile
    " Assumptions:
    " * Null shared_open implies null shared_close
    " * shared_open == start implies end equal to a *non-null* shared_close.
    " * shared_close == end implies start equal to a *non-null* shared_open.
    " Enforce the associated constraints, with possibly redundant assignments.
    if !shared_open[1]
        " We hit top level looking for shared open containing end.
        " Note: In case of unbalanced open, this assignment will be redundant.
        let shared_close = [0, 0, 0, 0]
    elseif shared_open == start
        if shared_close[1]
            let end = shared_close
        endif
    elseif shared_close == end
        if shared_open[1]
            let start = shared_open
        endif
    endif
    " If on element, find its start.
    " Rationale: Prefer start of macro chars to open bracket.
    call s:setcursor(start)
    let p = sexp#current_element_terminal(0)
    if p[1]
        let start = p
    endif
    " Is it possible we need to adjust end upward?
    if end != shared_close
        " Special Cases:
        "   (shared_close == null)   => shared close is top-level
        "       Don't look up any further; just find end terminal
        "   (shared_close == a:end)    => end requires no adjustment
        "       The next two loops will be skipped.
        call s:setcursor(end)
        " Note: compare_pos() < 0 could be simplified to p != shared_close.
        " Rationale: Prior logic guarantees that p will eventually land *on* a non-null
        " shared_close.
        " Seed prev position var.
        let p = end
        " Treat null shared close like shared close past EOF.
        while !shared_close[1] || s:compare_pos(p, shared_close) < 0
            let end = p
            let p = s:move_to_nearest_bracket(1)
            if !p[1]
                " Top level is common ancestor
                break
            endif
        endwhile
        " As long as we can assume a form always ends with a closing bracket (e.g., no macro
        " chars following close), we can skip looking for terminal whenever the preceding loop
        " has adjusted end to a closing bracket (i.e., end != a:end).
        if end == a:end
            call s:setcursor(end)
            " Ensure end is a terminal.
            let p = sexp#current_element_terminal(1)
            if p[1]
                let end = p
            endif
        endif
    endif

    " Restore saved position.
    call s:setcursor(cursor)
    return [start, end]
endfunction

" Return a superset range containing no unbalanced brackets by adjusting one or both sides
" of the input range upward till both sides are at same level (i.e., have same parent) and
" no elements are partially included in the range.
" Exceptions:
" -If both ends of selection are in same whitespace, return unmodified selection and let
"  caller handle.
" -Return null positions if superset range would contain unbalanced brackets.
" Design Decision: We could probably just return unadjusted range in case of a single char
" selection (since caller most likely contains logic to find the terminals), but this
" would probably just postpone the inevitable, so this function attempts to find the
" terminals in the single-char non-whitespace selection case.
function! s:super_range(start, end)
    " Short-circuit optimizations
    " Are both ends of selection in run of whitespace?
    if !s:range_has_non_ws(a:start, a:end, 1)
        " Both ends of selection in same blank/whitespace
        " TODO: Ok to return the original ends, or do we need to find some sort of
        " terminals?
        "call s:Dbg("Both ends in same whitespace optimization!")
        return [a:start, a:end]
    endif
    " Ignore leading/trailing whitespace.
    let [start, end] = s:trim_range(a:start, a:end)
    " Are both ends of selection in same atom?
    if !s:range_has_ws(start, end, 1)
        " High probability selection within single atom, but need to check.
        let save_cursor = getpos('.')
        call s:setcursor(start)
        let s = sexp#current_element_terminal(0)
        let e1 = sexp#current_element_terminal(1)
        call s:setcursor(end)
        let e2 = sexp#current_element_terminal(1)
        if e1 == e2
            " start/end within same atom!
            " Caveat: Restore cursor before return!
            call s:setcursor(save_cursor)
            "call s:Dbg("Same atom optimization!")
            return [s, e1]
        endif
        call s:setcursor(save_cursor)
    endif
    " No short-circuit optimization was performed; call the more expensive function.
    let ret = s:super_range_{s:ts_or_legacy()}(start, end)
    " Finally, check for unbalanced brackets in range we plan to return.
    " TODO: Determine whether this is necessary in the Treesitter case.
    let [bra, ket] = s:count_brackets(start, end, s:bracket, s:opening_bracket)
    return bra || ket ? s:nullpos_pair : [start, end]
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
    if s:is_rgn_type('comment', a:line, a:col)
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
            if pline && cline && s:is_rgn_type('comment', pline, pcol)
                \ && s:is_rgn_type('comment', cline, ccol)
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

" Returns nonzero if input position first non-ws on line
" Note: Accepts virtual cursor pos at EOL.
function! s:at_bol(line, col)
    return getline(a:line)[:a:col - 2] !~ '\S'
endfunction

" Returns nonzero if input position last non-ws on line
" Note: Accepts virtual cursor pos at EOL.
function! s:at_eol(line, col)
    return getline(a:line)[a:col - 1:] =~ '^.\?\s*$'
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
    return maybe && !s:is_rgn_type('str_com_chr', a:line, a:col)
        \ ? maybe : 0
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

" Return true iff there's *any* whitespace in the range [beg,end].
" Note: If 'check_ignored' set, differentiate between whitespace in ignored region and
" whitespace that separates tokens.
fu! s:range_has_ws(beg, end, check_ignored)
    let save_cursor = getcurpos()
    call setpos('.', a:beg)
    " Note: Empty 'skip' skips nothing.
    let pos = searchpos('\s', 'nczW', a:end[1], a:check_ignored ? s:match_ignored_region_fn : '')
    let ret = pos[0] && pos[1] <= a:end[2]
    call setpos('.', save_cursor)
    return ret
endfu

" Return true iff there's *any* non-whitespace in the range [beg,end].
" Note: See previous function comment for usage of 'check_ignored'.
fu! s:range_has_non_ws(beg, end, check_ignored)
    let ret = 0
    let save_cursor = getcurpos()
    call setpos('.', a:beg)
    let pos = searchpos('\S', 'nczW', a:end[1])
    let ret = pos[0] && pos[1] <= a:end[2]
    if !ret && a:check_ignored
        " No true non-ws, but check for "ignored" ws, which counts as the same thing...
        let pos = searchpos('\s', 'nczW', a:end[1], s:nomatch_ignored_region_fn)
        " Return true iff we found ignored ws within region.
        let ret = pos[0] && pos[1] <= a:end[2]
    endif
    call setpos('.', save_cursor)
    return ret
endfu

" Return true iff specified SexpPos is on whitespace (or blank if allow_blank).
" Note: See previous function comment for usage of 'check_ignored'.
fu! s:in_whitespace(pos, allow_blank, check_ignored)
    local save_cursor = getcurpos()
    call setpos('.', a:pos)
    " Anchor search at cursor for efficiency (in case line is long).
    " TODO: Consider different approach: e.g., grabbing and testing the char in lieu of
    " search().
    let re = '\v%.c\s' . (a:allow_blank ? '|^$' : '')
    let pos = searchpos(re, 'nczW', a:pos[1], 0, a:check_ignored ? s:match_ignored_region_fn : '')
    let ret = pos[0] && pos == save_cursor[1:2]
    call setpos('.', save_cursor)
    return ret
endfu

" Return input range adjusted inward such that start/end are both on non-whitespace.
" If this can't be done (e.g., because start/end within same run of whitespace), return
" unadjusted input position.
" Design Decision: Don't slow down by considering whether the whitespace is ignored.
" Rationale: This function is intended to be used to get a "starting point" only; ignored
" whitespace is invariably within some region of which subsequent logic can find the
" terminals.
fu! s:trim_range(beg, end)
    let save_cursor = getcurpos()
    " Find first non-white
    call s:setcursor(a:beg)
    let s = searchpos('\S', 'nczW', a:end[1])
    let s = [0, s[0], s[1], 0]
    if s[1] && s:compare_pos(s, a:end) <= 0
        " We have at least one non-ws within range; find the last.
        " Note: Previous search guarantees success of this one.
        call s:setcursor(a:end)
        let e = searchpos('\S', 'ncbW', a:beg[1])
        let e = [0, e[0], e[1], 0]
    else
        " No non-ws in range; return original range.
        let [s, e] = [a:beg[:], a:end[:]]
    endif
    " TODO: Find out what the performance penalty is for :try..finally; if not signficant,
    " I would put the cursor restoration in a finally and simplify this function.
    " Restore cursor.
    call s:setcursor(save_cursor)
    return [s, e]
endfu

let s:MAX_CHARLEN = 8 " actually, 4 for utf-8, but no reason to cut it close.
" Return number of bytes in char at specified VimPos4.
" Important Note: This is an autoload function because it's called from Lua treesitter code.
fu! sexp#char_bytes(p)
    let c = nvim_buf_get_text(0, a:p[1]-1, a:p[2]-1, a:p[1]-1, a:p[2]-1 + s:MAX_CHARLEN, {})[0]
    let [cidx, n] = [0, 0]
    while !cidx
        let n += 1
        let cidx = charidx(c, n)
    endwhile
    return n
endfu

" Stub for Lua implementation.
fu! s:is_rgn_type_ts(rgn, line, col)
    return luaeval(
                \ "require'sexp.ts'.is_rgn_type(_A[1], _A[2], _A[3])",
                \ [a:rgn, a:line, a:col])
endfu

fu! s:is_rgn_type_legacy(rgn, line, col)
    " Note: Eventually, may need to use different patterns for treesitter vs syntax and
    " for the various lisp dialects. However, until the original, simple, test is proven
    " ineffective, just use it.
    let patt = get(s:rgn_patts, a:rgn, '')
    if empty(patt)
        echoerr "Internal error! Unknown region type in s:is_rgn_type_legacy"
        return 0
    endif
    return s:syntax_match(patt, a:line, a:col)
endfu

fu! s:is_rgn_type(rgn, line, col)
    if s:prefer_treesitter()
        let match = s:is_rgn_type_ts(a:rgn, a:line, a:col)
        "call s:Dbg("match=%s rgn=%s line=%d col=%d", string(match), a:rgn, a:line, a:col)
        if match != v:null
            " Nil return indicates no treesitter tree; fall through to try legacy.
            return match
        end
    end
    " Arrival here means we won't or can't use treesitter. If we don't have legacy syntax,
    " we're going to have a problem, so warn...
    if empty(&syntax) && !get(b:, 'sexp_did_warn_no_syntax', 0)
        let b:sexp_did_warn_no_syntax = 1
        " TODO: Spruce this up...
        echoerr "vim-sexp: Warning: No syntax available" 
        return
    endif
    return s:is_rgn_type_legacy(a:rgn, a:line, a:col)
endfu

""" CURSOR MOVEMENT {{{1

" Calls cursor(pos[1], pos[2]). Used in favor of setpos(), which is lower
" level than cursor(), omitting some UI niceties.
" TODO: Re-examine this rationale...
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
    let pos = sexp#current_element_terminal(a:closing)
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
" FIXME: Move this (or the api sexp#move_to_adjacent_element we're trying to deconflict)
" and get rid of the kludgy suffix.
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
            \ || (a:next && s:compare_pos(pos, sexp#current_element_terminal(0)) != 0)
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
    "call s:prof_start()
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
    "call s:prof_end("list_flow")
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
    "call s:prof_start()
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
                let npos = sexp#current_element_terminal(!a:next)
            endif
            let fpos = nf ? getpos('.') : sexp#current_element_terminal(a:next)
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
    "call s:prof_end("leaf_flow")
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

function! s:get_visual_beg_mark()
    " Note: In Linewise Visual mode, getpos returns 0 for col. The rest of the plugin is
    " not designed to handle this, so return 1 instead.
    " Design Decision: A call site contained logic to adjust an end of line *start* to the
    " beginning of the subsequent line. Considered subsuming that logic here, but I don't
    " think it's really needed, as Vim handles end of line visual starts just fine, even
    " without ve=onemore.
    let pos = getpos("'<")
    " Leave [0,0,0,0] (representing invalid input) alone.
    if pos[1] && !pos[2]
        let pos[2] = 1
    endif
    return pos
endfu

function! s:get_visual_end_mark()
    " Note: In Linewise Visual mode, getpos returns v:maxcol for col. The rest of the
    " plugin is not designed to handle this, so return last col position on end line
    " instead.
    let pos = getpos("'>")
    if pos[2] == v:maxcol
        " Caveat: max() prevents problems on empty lines, for which both col('.') and
        " col('$') return 1.
        let pos[2] = max([1, col("'>") - 1])
    endif
    return pos
endfu

" Return current visual marks as a list
function! s:get_visual_marks()
    return [s:get_visual_beg_mark(), s:get_visual_end_mark()]
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
    let start = s:get_visual_beg_mark()
    let visual = a:mode ==? 'v'
    let counting = s:countindex > 0
    let start_is_valid = start[1] > 0
    let have_selection = start_is_valid && s:compare_pos(start, s:get_visual_end_mark()) != 0
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

    let ignored = s:is_rgn_type('str_com_chr', cursor[1], cursor[2])
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
" FIXME: Get rid of the suffix after refactoring namespaces.
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

    " Note: Although we may be called from visual mode, child selection
    " ignores current selection by definition.
    call s:set_marks_around_current_element('n', a:inner, 0, 0)
endfunction

" Return dict representing the most recent visual selection.
" Keys:
"   vs, ve
"   cursor
"   at_end  TODO: If this is redundant with cursor and there's no compelling
"           reason for the de-normalization, remove one or the other.
" Note: No point in calling if there's no visual selection, but handle gracefully if not.
" Important Note and TODO: Ideally, this function would use 'v' and '.' with getpos() to
" get both ends of visual selection; however, this works only when you're truly in visual
" mode, which, in turn, entails use of <cmd> (not :<c-u>) for visual mappings. Thus, as
" long as we support Vim versions prior to v9 (when <Cmd> was added), this function is
" inherently more complex than it should be.
function! s:get_cursor_and_visual_info()
    let o = {}

    let [vs, ve] = s:get_visual_marks()
    " Note: Since we don't really know what the command's mode was (and don't really
    " care), differentiate solely on whether visual sel exists; if it doesn't, we'll
    " return cursor pos along with some innocuous sentinel values for range.
    if vs[1] && ve[1]
        " Ascertain (normalized) cursor position.
        " Note: If we're not in visual mode, we'll have to enter it to
        " determine which end cursor was on.
        " TODO: Does Vim provide another way?
        let mode = mode()
        if mode !=? 'v'
            " Caveat: Entering visual mode can alter the viewport, which is a
            " problem for commands that expect to be able to preserve the
            " pre-command view; use winsaveview/winrestview to save/restore.
            let wsv = winsaveview()
            normal! gv
        endif
        " Note: When range begins past eol, exiting visual mode causes cursor to
        " fall back to last char on line (which is not actually *within* the
        " visual range). Note that this can happen at both start and end of range.
        " Note: Default to at_end when '< == '>.
        let o.at_end = s:compare_pos(getpos('.'), ve) >= 0
        if mode !=? 'v'
            exe "normal! \<Esc>"
            call winrestview(wsv)
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

" TODO: Update this comment to reflect major changes with the handling of counts!!!
" Set visual marks to the start and end of the current element. If inner is 0, trailing or
" leading whitespace is included by way of s:terminals_with_whitespace().
" TODO: Update documentation to reflect changes in s:terminals_with_whitespace logic.
"
" If cursor is on whitespace that is not in a string or between line comments, the marks
" are set around the next element if inner is 1, and around the current position and end
" of the next element if inner is 0.
"
" Will set both to [0, 0, 0, 0] if an element could not be found and mode does
" not equal 'v'.
" Args:
"   count   (> 0 means expansion possible)
"   no_sel  inhibit visual selection (return range only)
" Return: adjusted position, else null pos
" FIXME: Consider using try/catch; re-examine the off-nominal handling.
" Idiosyncrasy: Hitting vie in normal mode on a single-char atom will cause two atoms to
" be selected! When I first observed this behavior, it was sufficiently disconcerting that
" I assumed it was a bug; however, upon reflection, I realized it was a natural
" consequence of the new approach to expanding selections.
" Explanation: When the v in vie is pressed, the aforementioned single-char atom is
" completely inner-selected; thus, the subsequent ie expands the selection to include the
" next atom.
" TODO: Decide whether this behavior needs to change. Yes, it was disconcerting when I
" first noticed it; otoh, typing 3 keystrokes (vie) in lieu of 1 (v) is kind of silly...
function! s:set_marks_around_current_element(mode, inner, count, no_sel)
    " Extra args imply extension mode only if mode is visual.
    let cnt = a:count > 0 ? a:count : 0
    let save_cursor = getpos('.')
    if a:mode ==? 'v'
        " TODO: Should we just use cached cvi, or perhaps have get_visual_marks() return
        " 'at_end' flag to obviate the need for cvi?
        let [vs_orig, ve_orig] = s:get_visual_marks()
        let dir = b:sexp_cmd_cache.cvi.at_end
        " Rationalize visual range.
        " TODO: Now that super_range trims surrounding whitespace from selection,
        " optimizations could be added to subsequent logic.
        let [vs, ve] = s:super_range(vs_orig, ve_orig)
        "call s:Dbg("super_range returned %s %s", string(vs), string(ve))
        if !vs[1]
            " TODO: Is this the best way to handle? Is this all it can mean? Consider
            " using exception for this, and getting rid of this if.
            call s:warnmsg("Refusing to operate on selection containing unmatched parens!")
            return s:nullpos_pair
        endif
        " Move to start of range.
        call s:setcursor(vs)
    else
        let dir = 1
    endif
    " Search from element start to avoid errors with elements that end with macro
    " characters. e.g. Clojure auto-gensyms: `(let [s# :foo)])
    " Rationale: If cursor is on the # in the example above, current_element_terminal(1)
    " will incorrectly assume the # is a leading macro char. (TODO: Should probably fix
    " the logic in current_element_terminal.)
    " TODO: In the visual mode case, super_range() has been called, so we should already
    " be on the head of an element; however, in the special case of only whitespace
    " selected, this block is still needed to move starting search position to the next
    " element. Optimization would skip if super_range has been called *and* the selection
    " *isn't* pure whitespace.
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
            " Note: No need to restore cursor position, as it shouldn't have changed.
            return s:nullpos_pair
        endif
        let start = next
    endif
    " Note: If we get here, a non-whitespace start position is guaranteed.

    " If cursor (non-visual mode) or start of selection (visual mode) is before start
    " (because of leading whitespace), save the original position, which may be needed by
    " cleanup logic.
    " Rationale: Input to terminals_with_whitespace.
    let leading = a:mode ==? 'v' ? vs_orig : save_cursor
    if s:compare_pos(leading, start) >= 0
        let leading = []
    endif

    " Position ourselves to look for (first) end, taking care to begin the search no
    " earlier than start, which could be *after* ve in certain corner cases.
    call s:setcursor(a:mode ==? 'v' && s:compare_pos(ve, start) > 0 ? ve : start)

    " Find first end, looking backwards if necessary.
    let end = sexp#current_element_terminal(1)
    if !end[1]
        " Weren't on an element. Get to end of previous (whose existence is implied by
        " existence of start).
        let end = s:move_to_adjacent_element(0, 1, 0)
    endif
    " At this point, end is on 'inner' end of the current (possibly partial) selection.
    " Subsequent logic handles cleanup and possibly expansion.

    " We've now established a 'current' *inner* selection. If cnt > 1, pull in more,
    " taking direction into account.
    " Rationale: Initial selection always counts as 1, regardless of whether cleanup is
    " required.
    if cnt > 1
        let p = dir ? end : start
        call s:setcursor(p)
        while cnt > 1
            let pp = p
            let p = s:move_to_adjacent_element(dir, dir, 0)
            if p == pp
                " We've gone as far as possible.
                break
            endif
            let cnt -= 1
        endwhile
        " Adjust the end we've expanded.
        let l:[dir ? 'end' : 'start'] = p
    endif

    if !a:inner
        " Perform whitespace cleanup.
        let [start, end] = s:terminals_with_whitespace(start, end)
    endif

    call s:setcursor(save_cursor)
    if !a:no_sel
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
    call s:set_marks_around_current_element('n', 1, 0, 0)
    call s:setcursor(cursor)
endfunction

" Enter characterwise visual mode with current visual marks, unless '< is
" invalid and mode equals 'o'.
" Optional Arg:
"   a:1 - where to leave cursor after performing the visual selection:
"         0=left side ('<), 1=right side ('>)
"         Note: This arg is ignored if marks not set.
function! s:select_current_marks(mode, ...)
    if s:get_visual_beg_mark()[1] > 0
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
    "call s:prof_start()
    if !s:set_marks_around_current_list(a:mode, a:offset, a:allow_expansion)
        " TODO: I'd really rather hard-code 1 for inner here, but need to
        " consider backwards-compatability...
        " FIXME: The thing is, "inner" means something different for lists:
        " i.e., even outer doesn't select whitespace around the list, so if
        " we're going to fall back to current element, I think it should be
        " inner, regardless of a:offset.
        call s:set_marks_around_current_element(a:mode, 1, 0, 0) "a:offset)
    endif
    "call s:prof_end("select_current_list")
    return s:select_current_marks(a:mode)
endfunction

" Set visual marks at current outermost list's brackets, then enter visual
" mode with that selection. Selects current element if cursor is not in a
" list.
function! sexp#select_current_top_list(mode, offset)
    if !s:set_marks_around_current_top_list(a:mode, a:offset)
        call s:set_marks_around_current_element(a:mode, a:offset, 0, 0)
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
    "call s:prof_start()
    let cnt = a:0 && a:1 ? a:1 : 1
    call s:set_marks_around_current_element(a:mode, a:inner, cnt, 0)
    "call s:prof_end("select_current_element")
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
    call s:set_marks_around_current_element('n', 1, 0, 0)
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:headspace)
endfunction

" Capture element adjacent to current list, given the starting position of the
" enclosing list's bracket minus leading macro characters (spos) and the
" position of the bracket itself (bpos).
function! s:stackop_capture(last, spos, bpos)
    call s:setcursor(a:spos)
    let nextpos = s:move_to_adjacent_element(a:last, 0, 0)

    " Ensure we are not trying to capture a parent list
    if nextpos[1] < 1 || s:compare_pos(a:spos, sexp#current_element_terminal(!a:last)) == (a:last ? 1 : -1)
        return 0
    endif

    let reg_save = @b
    let @b = getline(a:spos[1])[a:spos[2] - 1 : a:bpos[2] - 1]
    let blen = len(@b)

    " Insertion and deletion must be done from the bottom up to avoid
    " recalculating our marks
    if a:last
        let nextpos = sexp#current_element_terminal(1)
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

    let nextpos = sexp#current_element_terminal(a:last)

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
        call s:setcursor(s:get_visual_beg_mark())
    else
        call s:setcursor(bmarks[0])
    endif

    let [@a, @b] = reg_save
    return 1
endfunction

" Adjust range start pos to make range entirely inclusive, taking special inc
" value into account.
"   0 = exclusive
"   1 = inclusive (nop)
"   2 = exclusive of whitespace up to and including newline at EOL
"       Note: Equivalent to inc==0 if not in whitespace at EOL
" Example:
" foo)|<SPC>   ==>   foo)<SPC>
" bar                |bar
function! s:yankdel_range__preadjust_range_start(start, inc)
    let ret = a:start[:]
    if a:inc == 2 && getline(ret[1])[ret[2] - 1:] =~ '^.\?\s*$'
        " Move to start of next line (if it exists).
        if ret[1] < line('$')
            let ret[1] += 1
            let ret[2] = 1
        else
            " EOF
            let ret = getpos([line('$'), '$'])
        endif
    elseif a:inc != 1 " 0 or 2
        " Move to next position, including newline.
        let ret = s:offset_char(ret, 1, 1)
    endif
    return ret
endfunction

" Adjust range end pos to make range entirely inclusive, taking special inc
" value into account.
"   0 = exclusive
"   1 = inclusive (nop)
"   2 = exclusive of whitespace back to and including newline at BOL
"       Note: Equivalent to inc==0 if not in whitespace at BOL
" Example:
" foo)        ==>   foo)|
" <SPC>|bar         <SPC>bar
" Special Case: If end is BOF and adjustment is exclusive, return special
" non-physical position [0, 1, -1, 0].
" Caveat: Callers requiring physical positions will need to check for this.
function! s:yankdel_range__preadjust_range_end(end, inc)
    let ret = a:end[:]
    if a:inc == 2 && getline(ret[1])[:ret[2] - 1] =~ '^\s*.\?$'
        " Move to end of prev line, excluding newline.
        if ret[1] > 1
            let ret[1] -= 1
            let ret[2] = col([ret[1], '$']) - 1
            if !ret[2]
                " Empty line is special.
                let ret[2] = 1
                if ret[1] > 1
                    " Special Case: We want to exclude the newline preceding end, but
                    " col==1 on an empty line would include it; thus, back up one
                    " additional line, including its newline.
                    let ret[1] -= 1
                    let ret[2] = col([ret[1], '$'])
                endif
            endif
        else
            " BOF
            " As a special case, return sentinel (non-physical) position just
            " before beginning of first line.
            let ret[1:2] = [1, -1]
        endif
    elseif a:inc != 1 " 0 or 2
        " Move to prev position, including newline.
        if ret[1:2] == [1, 1]
            " Already at BOF, so return the special sentinel position just
            " prior to first char.
            let ret[2] = -1
        else
            let ret = s:offset_char(ret, 0, 1)
        endif
    endif
    return ret
endfunction

" Return a dict recording the following:
" * total # of bytes in the file
" * byte offset of start relative to BOF
" * byte offset of end relative to start
" * list of byte offsets of each of the provided positions relative to start
" * the positions themselves (indices correspond to indices in byte_offs[])
" Explanation: After delete/splice, this information can be used to adjust the positions
" to account for added/deleted text.
" Caveat: Because callers generally maintain references to the position tuples in the
" input list, it's vital that we never delete or replace the tuple references: i.e., when
" adjustments are made, the line/col elements are modified directly.
" Alternative Approach: This preadjustment step isn't strictly necessary, since number of
" bytes to be deleted could be calculated analytically (even before any buffer
" modifications have occurred) using pos2byte etc on start/end; however, that approach is
" significantly more complex: much simpler/safer to do it using byte2pos after the
" deletion has occurred.
function! s:yankdel_range__preadjust_positions(start, end, ps)
    " Pre-op position adjustment
    let ret = {'ps': a:ps, 'byte_offs': [], 'start': a:start}
    " Total # of bytes in file used to calculate delta later.
    let ret.bytes_in_file = s:total_bytes_in_file()
    " Calculate byte offset of start wrt BOF and end wrt start.
    let ret.start_byte = s:pos2byte(a:start)
    " Note: Considered making e_off reflect position just *past* end, so that when
    " position is deleted, it falls forward *out of* the deleted range (i.e., into first
    " non-deleted text).
    " Decision: I think it's best to position *at* original end.
    " Rationale: If splice text is non-empty, it looks best for cursor to move
    " to end of spliced text; if splice is empty (i.e., splice is actually
    " delete), end position will correspond to first char past deletion
    " naturally.
    let ret.end_off = s:pos2byte(a:end) - ret.start_byte
    " Calculate and store offsets of positions of interest wrt start of range.
    for p in a:ps
        call add(ret.byte_offs, s:pos2byte(p) - ret.start_byte)
    endfor
    return ret
endfunction

" Adjust the positions in adj (assumed to be dict built by
" yankdel_range__preadjust_positions()) to reflect the text added/removed by a
" splice/delete operation.
function! s:yankdel_range__postadjust_positions(adj, spl)
    " Post-op position adjustment
    " Calculate net byte delta (added (+) / deleted (-))
    let delta = s:total_bytes_in_file() - a:adj.bytes_in_file
    let [ps, offs] = [a:adj.ps, a:adj.byte_offs]
    let [s, s_byte, e_off] = [a:adj.start, a:adj.start_byte, a:adj.end_off]
    " Get adjusted e_off, which will be used within the loop.
    let e_off_adj = e_off + delta
    let e_adj = s:byte2pos(s_byte + e_off_adj)
    " Iterate parallel lists ps and offs.
    for i in range(len(offs))
        let [o, p] = [offs[i], ps[i]]
        if o > 0
            " Important Note: There are 3 possible cases for the position being adjusted:
            " 1. original position within both original and adjusted ranges
            " 2. original position within original but not adjusted range
            " 3. original position past original range (and thus, also past adjusted
            "    range)
            " Originally, used position calculated from original byte offset for case 1,
            " but this is problematic because converting whitespace (e.g., trailing
            " spaces) to newlines can result in an adjusted position on a different line,
            " even when the original line still exists. A better approach for cases 1 & 2
            " is to try to use the original line/col (limiting col to '$'-1), falling back
            " to the adjusted end position if the aforementioned position is past adjusted
            " end.
            " Caveat: Because callers may hold references to the position lists, it's
            " vital that we replace the list elements *without* replacing the list
            " references.
            if o <= e_off
                " Case 1 or 2
                if s:compare_pos(p, e_adj) >= 0
                    " Don't allow position to escape from adjusted range.
                    let [p[1], p[2]] = e_adj[1:2]
                else
                    " Should be able to use col-constrained original position.
                    let ecol = col([p[1], '$'])
                    if p[2] >= ecol
                        let p[2] = max([1, ecol - 1]) " max() needed to handle blank lines
                    endif
                endif
            else
                " Case 3
                let [p[1], p[2]] = s:byte2pos(s_byte + o + delta)[1:2]
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
" TODO: Idea: Could passing 'splice' arg of one or more newlines obviate need
" for the special inc==2 value??? Think on this...
function! s:yankdel_range(start, end, del_or_spl, ...)
    let ret = ''
    let cursor = getpos('.')
    " Assumption: 'virtualedit' has been set to onemore (by pre-op handler).
    " Rationale: Need to be able to select (visually) past EOL in certain
    " cases (e.g., non-inclusive start at EOL).
    let reg_save = [@a, @"]
    try
        let inc = a:0 ? type(a:1) == 3 ? a:1 : [1, a:1] : [1, 0]
        let start = s:yankdel_range__preadjust_range_start(a:start, inc[0])
        let end = s:yankdel_range__preadjust_range_end(a:end, inc[1])
        " Special Case: Treat splice of certain types of null replacement
        " regions as a put.
        " Design Decision: Only *just empty* regions will be treated this way:
        " e.g., start == end and either end (but not both) exclusive (=== 0).
        " Though there are simpler ways to accomplish it, you could use such
        " null regions to perform a simple put, whose direction is determined
        " by the inclusive side (inclusive start => put before).
        " Design Decision: If del_or_spl === 1 (in lieu of splice text), the
        " aforementioned 'just empty' null regions result in NOPs.
        let cmp = s:compare_pos(a:start, a:end)
        " Caveat: Vim 7.3 didn't have xor() function so do it manually.
        let spl_put = type(a:del_or_spl) == 1 && !cmp
            \ && (!inc[0] && inc[1] == 1 || inc[0] && !inc[1] == 1)
        if spl_put || s:compare_pos(start, end) <= 0
            " Either splice is a directional put (spl_put) or the adjusted
            " region is non-empty.
            " Note: end adjustment may have returned non-physical location; if
            " so, fix now...
            if start[2] < 0 | let start[2] = 1 | endif
            " non-empty region to be spliced/deleted...
            " Note: Since splice also deletes, del will be set for either.
            let [del, spl, spl_text] = type(a:del_or_spl) == 1
                \ ? [1, 1, a:del_or_spl]
                \ : [!!a:del_or_spl, 0, '']

            if del
                " Pre-op position adjustment
                " End: Treat deletion and non-null splice differently: for a
                " deletion, end should be *past* the deleted text, but for a
                " non-empty splice, use the end of the spliced area.
                " Rationale: Looks best to keep cursor within replacement area
                " (typically whitespace) when text is spliced, but to let it
                " fall forward into undeleted text when deleting.
                " Start: If we're doing a put before, pre-adjustment needs to
                " use a start position prior to a:start; otherwise, the
                " original start position will not be adjusted, despite coming
                " after the put text.
                " Note: Both 0 and 2 inc values are exclusive.
                let adj = s:yankdel_range__preadjust_positions(
                    \ spl_put && inc[0] ? s:offset_char(start, 0) : start,
                    \ !spl
                        \ ? inc[1] != 1 ? a:end : s:offset_char(end, 1)
                        \ : end,
                    \ a:0 > 1 ? s:concat_positions(a:2, cursor) : [cursor])
            endif
            " Perform the yank/del/splice.
            if spl_put
                call s:setcursor(a:start)
            else
                " Select text to be yanked/deleted
                call s:set_visual_marks([start, end])
                call s:select_current_marks('v')
            endif
            if spl
                let @a = spl_text
                " Caveat: Need to treat splice text ending in newline
                " specially to inhibit linewise put.
                let linewise = @a[-1:] == "\n"
                if linewise
                    " Make the register non-linewise.
                    let @a .= ' '
                endif
                " Replace selection with splice text.
                silent! exe 'normal! "a' . (spl_put && inc[0] ? 'P' : 'p')
                if linewise
                    " Save [ and ] marks for restoration after space deletion.
                    let [smark, emark] =
                        \ [getpos("'["), [0, line("']") - 1, col([line("']"), '$']) - 1, 0]]
                    " Cleanup the space that was appended to inhibit linewise put.
                    normal! `]"_x
                    " Restore the [ and ] marks, which are clobbered by normal
                    " x, even when lockmarks is used.
                    call setpos("'[", smark)
                    call setpos("']", emark)
                    let @" = @"[:-2]
                endif
                if !spl_put
                    let ret = @"
                endif
            else
                silent! exe 'normal! ' . '"a' . (del ? 'd' : 'y')
                let ret = @a
            endif
            if del " Both delete and splice set this flag
                " Post-op position adjustment
                call s:yankdel_range__postadjust_positions(adj, spl)
            endif
        endif
    finally
        " Restore options/regs/cursor...
        let [@a, @"] = reg_save
        call s:setcursor(cursor)
    endtry

    return ret
endfu
"let Ydr = function('s:yankdel_range')

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
    let ve_save = &ve
    set ve=onemore
    try
        " Position defaults to cursor pos.
        if a:0
            " FIXME: Probably need to save/restore ve=onemore if we're not going
            " to do it at higher level.
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
    finally
        let &ve = ve_save
    endtry
endfu

" Adjust the input view to ensure that, if possible, the cursor line doesn't
" change its screen line. (Won't be possible if desired screen line exceeds
" buffer size.)
function! s:adjust_saved_view(view, cursor)
    let a:view.topline = max([1, a:view.topline - (a:view.lnum - a:cursor[1])])
    let a:view.lnum = a:cursor[1]
    let a:view.col = a:cursor[2] - 1
    " Note: Because of the lack of one-to-one correspondence between chars and
    " screen columns, preserving horizontal shift would require an iterative
    " approach, whose overhead is probably not warranted, especially
    " considering it's *extremely* rare for a view on code to be shifted
    " horizontally. Thus, for now, simply rely on Vim to ensure the cursor
    " column is visible.
endfunction

function! s:indent_preadjust_positions(ps)
    let ret = {'ps': a:ps, 'line_ends': {}}
    let line_ends = ret.line_ends
    for p in a:ps
        if !has_key(line_ends, p[1])
            let line_ends[p[1]] = col([p[1], '$'])
        endif
    endfor
    return ret
endfunction

function! s:indent_postadjust_positions(adj)
    let line_ends = a:adj.line_ends
    for p in a:adj.ps
        " Note: I suppose that for a position inside leading whitespace, the
        " optimal logic might calculate the % visual distance (not byte
        " distance) into the ws and try to preserve it across indent, but this
        " is a lot of complexity for a small potential advantage.
        " Decision: For now, stay on this side of point of diminishing returns
        " by preserving byte distance from EOL.
        let p[2] -= line_ends[p[1]] - col([p[1], '$'])
    endfor
endfunction

" If specified line has eol comment, return position of its start, else null pos.
" Caveat: This function does not preserve cursor position.
" TODO: Remove if no longer needed.
function! s:has_eol_comment(line)
    let ecol = col([a:line, '$'])
    if ecol == 1
        " Empty line has no comment.
        return s:nullpos_pair
    endif
    " Position past (or on, depending on 've') last char.
    call cursor(a:line, ecol)
    " Find last non-white char on line.
    " Note: 'c' flag needed because default 've' setting doesn't allow us to start *past*
    " the last char.
    if search('\S', 'bWc', a:line)
        " We're on non-whitespace; see whether it's a comment.
        if s:is_comment(line('.'), col('.'))
            return s:current_comment_terminal(0)
        endif
    endif
    return s:nullpos
endfunction

" Go to position of last non-whitespace char on specified line, else leave position
" unchanged.
" Return col or 0 if no jump.
function! s:goto_last_non_ws(line)
    let ecol = col([a:line, '$'])
    if ecol == 1
        " Empty line has no comment.
        return 0
    endif
    " Position past (or on, depending on 've') last char.
    call cursor(a:line, ecol)
    " Find last non-white char on line.
    " Note: 'c' flag needed because default 've' setting doesn't allow us to start *past*
    " the last char.
    if search('\S', 'bWc', a:line)
        " We're on non-whitespace; see whether it's a comment.
        return col('.')
    endif
    return 0
endfunction

" Return signed percent difference between a and b, with negative result indicating a<b.
" TODO: Consider moving this to more of a general utility location.
function! s:percent_diff(a, b)
    " Note: Input values are typically integer, and the intended use case for this is such
    " that we don't require anything more than 1% accuracy; thus, since Vim 7.3 (current
    " prereq) didn't have isnan(), just return 0 if abs of difference between input values
    " is less than 1E-3.
    " TODO Consider bumping up the Vim version prereq to allow use of isnan().
    " TODO: If I switch to all integer math, this will need to be modified.
    " Note: The factor of 2.0 converts to Float and also performs averaging.
    let diff = a:a - a:b
    return abs(diff) < 1.0E-3 ? 0.0 : 2.0 * (a:a - a:b) / (a:a + a:b)
endfunction

" Return tuple with the following keys to characterize the specified line:
"   ecol:       screen col just past element preceding eol or screen col just past last
"               non-ws on line if no eol comment
"   is_com:     1 iff specified line ends in comment (eol or otherwise)
"   is_eol_com: 1 iff specified line ends in eol comment
"   com_start:  VimPos indicating start of eol comment (else s:nullpos)
" Cursor Preservation: None (caller expected to handle)
" Terminology: There's an ambiguity inherent in the phrase "ends in comment": if an inline
" (self-contained, not eol-style) comment occurs at the end of a line, but followed by
" trailing whitespace, the line would technically not end in a comment; in fact,
" technically speaking, the line would not end in a comment even if the final character
" were the inline comment terminator. However, most users would probably want an inline
" comment at end of line to be treated as an end of line comment. With the current logic,
" it will be. For one thing, the calling logic has most likely already stripped off
" trailing whitespace, with the result that the last character on the line will be part of
" the comment. But even if this doesn't happen, this function performs the s:is_comment()
" test on the final *non-ws* char of the line, not the final char of the line.
function! s:align_eolc__characterize(line)
    " Note: It's not an eol comment if there's nothing before it.
    let p = s:goto_last_non_ws(a:line)
    if !p
        " Nothing to do for blank line
        return [0, 0, 0, s:nullpos]
    endif
    " This will be adjusted later for eol comment, but if there's no eol comment, we'll
    " need screen pos just past last non-ws for alignment purposes.
    let ecol = virtcol('.') + 1
    " Figure out which type of comment (if any) we have.
    let [is_eol_com, com_start] = [0, s:nullpos]
    let is_com = s:is_comment(a:line, ecol)
    if is_com
        " Find start of comment.
        let com_start = s:current_comment_terminal(0)
        call s:setcursor(com_start)
        " Is this an eol comment? I.e., is there something before it?
        " This pattern will match the last char of the preceding non-whitespace, with
        " [0,0] indicating this is not an eol comment.
        let [_, p] = searchpos('\S', 'bW', a:line)
        let is_eol_com = !!p
        if is_eol_com
            " Important Note: What we need for alignment is screen col, not col(); use
            " virtcol(), which existed in our prerequisite Vim version 7.3. (screenpos was
            " added later.)
            " Set ecol to screen position just past end of previous element.
            " Design Decision: There's really no reason to use current_element_terminal(1)
            " or current_comment_terminal(1) here, since last non-ws is always desired for
            " alignment purposes.
            let ecol = virtcol('.') + 1
        endif
    endif
    return [ecol, is_com, is_eol_com, com_start]
endfunction

" Calculate and return the criteria weights, taking user options into account.
function! s:align_eol_comment__get_weights()
    " Design Question: Should we enforce use of integers in user weights? Probably no need
    " to, as long as values are within range.
    " Build dict of scalar weights, taking into account the plugin-defined defaults and
    " any user-requested adjustment.
    let ret = {}
    for [k, d] in items(s:align_eolc_weights)
        let user_adj = get(g:, 'sexp_align_eolc_' . k . '_weight', -1)
        if user_adj < 0
            " Either user didn't override or explicitly selected default with -1.
            " Design Decision: Treat anything negative as request for default.
            " TODO: Should we warn about values other than -1?
            let ret[k] = d.default
        else
            " Make sure user-override doesn't violate limits.
            " TODO: Warn, or silently treat like "as much as possible" in direction
            " indicated by sign?
            let user_adj = max([0, min([10, user_adj])])
            " Make linear adjustment from default weight.
            let ret[k] = d.default * (1 + d.adjust * (user_adj - 5))
            " Convert near-zero to zero to ensure fp roundoff error doesn't result in
            " spurious processing for a criterion user meant to disable.
            if abs(ret[k]) <= 1.0E-6 | let ret[k] = 0 | endif
        endif
    endfor
    return ret
endfunction

" Return an effective eolc group size, which takes into account both the number of
" comments *and* the number of lines spanned by the group.
" Rationale: Both properties influence the size perceived by the user: e.g., for a densely
" commented group, the span is probably more important than the number of comments; OTOH,
" a group with 'bookend' comments spanning a large number of lines probably seems
" significantly smaller than a dense group with the same span.
" TODO: This is currently unused! Decide whether/where it should be or remove... Does
" 'density' obviate need for it? Should runtness calculation consider effective grp size?
function! s:align_eolc__effective_grpsize(grpsize, linespan)
    " TODO: This is just a placeholder for the ultimate logic...
    return (a:grpsize + a:linespan) / 2
endfunction

" Return signed value indicating results of group cost comparison.
" < 0: cost1 < cost2
" > 0: cost1 > cost2
" = 0: cost1 == cost2
function! s:align_eolc__compare_costs(dp, grp1, grp2)
    let [g1, g2] = [a:grp1, a:grp2]
    let [c1, c2] = [g1.cost, g2.cost]
    " Each enabled criterion adjusts this variable by signed amount.
    let ret = 0
    let weights = s:align_eol_comment__get_weights()
    " -- Ngrps --
    let ret += weights.ngrps * s:percent_diff(c1.cumul.ngrps, c2.cumul.ngrps)
    " -- Area (Shift) --
    if weights.shift > 0
        let ret += weights.shift * s:percent_diff(c1.cumul.area, c2.cumul.area)
    endif
    " -- Density --
    if weights.density > 0
        let density1 = 1.0 * c1.cumul.ncoms / c1.cumul.nlines
        let density2 = 1.0 * c2.cumul.ncoms / c2.cumul.nlines
        let ret -= weights.density * s:percent_diff(density1, density2)
    endif
    " -- Runt --
    " Note: Use only the *previous* (sidx-1) groups for this one, ignoring this criterion
    " if there's no previous group for either candidate under comparison.
    " TODO: Rationale? Decide on this, noting that we should probably take the same
    " approach with both area and runt...
    if weights.runt > 0 " && g1.sidx > 0 && g2.sidx > 0
        " Use the *previous* (sidx-1) group costs (without accumulation) to prevent
        " spurious avoidance of an incipient group.
        "let runt1 = a:dp[g1.sidx - 1].grp.cost.cumul.runt
        "let runt2 = a:dp[g2.sidx - 1].grp.cost.cumul.runt
        let ret += weights.runt * s:percent_diff(c1.cumul.runt, c2.cumul.runt)
    endif
    " Return the signed comparison value.
    return ret
endfunction

" Calculate and return a dict representing the specified candidate group.
" The dict must contain a cost dict with everything needed by the comparison function to
" compare this candidate with another.
" Inputs:
"   dp:        dynamic programming state list with one element for each eol comment 
"   sidx:      index of element corresponding to start of candidate group
"   eidx:      index of element corresponding to end of candidate group
"   area:      "area under the curve" for group being created: i.e., sum of all gaps
"              between end of code and start of eol comment
"   align:     alignment column for all eol comments in this group (excluding any
"              separating spaces we may add).
function! s:align_eolc__create_group_candidate(dp, sidx, eidx, area, align)
    let dp = a:dp
    " Calculate figures of merit for the *current* group.
    let nlines = dp[a:eidx].line - dp[a:sidx].line + 1
    let ncoms = a:eidx - a:sidx + 1
    " Calculate runtness now, though it won't be used till this element is at sidx-1.
    " TODO: Should this take "effective" group size into account?
    " Note: Runtness is a step function equal to square of the delta between actual group
    " size and runt threshold when size is under the threshold, else 0.
    let runt = nlines < g:sexp_align_eolc_runt_thresh
            \ ? (g:sexp_align_eolc_runt_thresh - nlines) * (g:sexp_align_eolc_runt_thresh - nlines)
            \ : 0
    " Save the current group's (non-cumulative) values (possibly only for debugging).
    let self = {'area': a:area, 'nlines': nlines, 'ncoms': ncoms, 'runt': runt}
    if a:sidx > 0
        " Get previous grp in chain to support accumulation.
        let pgrp = dp[a:sidx - 1].grp
        " Aggregate cost-related data that applies to the current candidate and its chain
        " of predecessors.
        let cumul = {
                    \ 'ngrps': pgrp.cost.cumul.ngrps + 1,
                    \ 'nlines': pgrp.cost.cumul.nlines + nlines,
                    \ 'ncoms': pgrp.cost.cumul.ncoms + ncoms,
                    \ 'area': pgrp.cost.cumul.area + a:area,
                    \ 'runt': pgrp.cost.cumul.runt + runt,
        \ }
    else
        " First element needs no accumulation.
        " TODO: Toying with idea that we don't really need a cost structure for the first
        " element. The only thing it's really needed for is to support cost accumulation,
        " once it becomes an sidx-1 group, but this could be handled specially.
        " Rationale: There's never any comparison to be made...
        let cumul = {
            \ 'ngrps': 1, 'nlines': nlines, 'ncoms': ncoms, 'area': a:area, 'runt': 0
        \ }
    endif
    " TODO: If self ends up not being required, pull cumul up.
    " TEMP DEBUG!!!
    "let cost = {'self': self, 'cumul': cumul}
    let cost = {'cumul': cumul}
    " Wrap all into a single grp dict.
    " Note: 'sidx' is needed to support backwards traversal of group chain.
    let ret = {'align': a:align, 'sidx': a:sidx, 'cost': cost}
    return ret
endfunction

" Update the Dynamic Programming state list element corresponding to the input line.
" Each invocation chooses the best of all candidate groups that end at this line. Of
" course, there is no guarantee that the selected group will be in the final group list,
" as it may turn out that a longer group containing this element or a group beginning at
" this element is better. Only after the final element has been processed can we determine
" the globally optimal set of groups by following group start indices backwards, starting
" with the final element, which is necessarily the end of the final group.
" Inputs:
"   dp:        dynamic programming state list with one element for each eol comment
"   line:      linenr of line being evaluated
"   ecol:      screen col just past last non-white char preceding the eol comment
"   com_start: position of start of eol comment
"   pre_max:   like ecol, but considers non-eol-comment lines in preceding interval
"   sog:       start of group: don't look back
function! s:align_eolc__update_dps(dp, line, ecol, com_start, pre_max, sog)
    " TODO: Consider mechanism to validate/constrain options.
    let [maxshift, maxdist] = [g:sexp_align_eolc_maxshift, g:sexp_align_eolc_maxgap]
    " Initialize DP state for current commment and add it to list.
    " Note: The 'grp' field will be updated within loop to reflect current best group
    " candidate.
    let el = {}
    let el.line = a:line                " line represented by this dp el.
    " pre_max represents max ecol of the lines between this (inclusive) and the previous
    " covered line (exclusive).
    let el.pre_max = a:pre_max
    let el.ecol = a:ecol                " end of element prior to eol comment
    let el.com_s = a:com_start          " start of eol comment
    let el.grp = {}                     " dict to hold candidate group.
    call add(a:dp, el)

    " Keep running sum of gaps between end of code and start of aligned eol comment: i.e.,
    " integral under curve represented by the space between eol comment and end of code.
    let area = 0
    " Keep up with horizontal extents of the "box" containing end of code for all lines in
    " the group. Each time we move to earlier line, we must account for lines with no eol
    " comment if bounded by line with eol comment.
    " Also keep up with previous ecol_max, which lets us know how much the "curve" whose
    " integral we're interested in shifts upward each iteration, thereby increasing 'area'
    " by the area of the rectangle associated with the shift.
    " TODO: Consider renaming box_{min,max} or adding a suffix to avoid confusion between
    " the 2 uses of 'ecol_max' (one referring only to lines between eol comment lines).
    let [ecol_min, ecol_max, ecol_max_prev] = [el.ecol, el.ecol, -1]
    " Loop over dp state backwards, starting with current element.
    " Optimization TODO: Handle single-element group outside loop with dedicated, more
    " efficient logic, skipping the loop altogether in special case of first element or
    " start-of-group. As it is now, the single-element group is not treated specially.
    let N = len(a:dp)
    let i = N - 1
    while i >= 0
        " Get DP element representing current group start candidate.
        let el_s = a:dp[i]
        " Update bounding box as though this candidate is valid, then verify.
        if el_s.ecol < ecol_min | let ecol_min = el_s.ecol
        elseif el_s.ecol > ecol_max | let ecol_max = el_s.ecol
        endif
        " Is this a valid start candidate? I.e., is bounding box still within limits?
        if ecol_max - ecol_min > maxshift
            " We've gone too far: no more candidate groups ending at current line.
            break
        endif
        " This is a candidate group; determine its cost.
        let area += ecol_max - el_s.ecol
        if ecol_max_prev > 0 && ecol_max > ecol_max_prev
            " Account for area increase due to "curve" shifting up by constant amount.
            " Note: Difference term accounts for shift due to eol comment and/or
            " non-comment lines between elements.
            let area += (ecol_max - ecol_max_prev) * (N - 1 - i)
        endif
        let ecol_max_prev = ecol_max
        " Create group candidate and calculate its cost.
        let grp = s:align_eolc__create_group_candidate(a:dp, i, N - 1, area, ecol_max)
        if (i == N - 1)
            call s:Dbg("End       (l=%3d, i=%d): %s", el.line, i, string(grp))
        else
            call s:Dbg("Candidate (l=%3d, i=%d): %s", el_s.line, i, string(grp))
        endif
        " Note: Comparison value < 0 indicates cost of lhs arg (current best) is still the
        " lowest. In case of tie, we keep existing best, since it's later-starting.
        if a:sog || empty(el.grp) || s:align_eolc__compare_costs(a:dp, el.grp, grp) > 0
            " Make this the new best candidate.
            call s:Dbg("New best: %s", string(grp))
            let el.grp = grp
        endif
        if a:sog
            " Don't look back if this is start of group.
            " TODO: Consider treating sog as special case before the loop.
            call s:Dbg("Breaking at i=%d for sog", i)
            break
        endif
        " Before moving to previous element, adjust right edge of bounding box to include
        " any long, non-eol-comment lines in the interval between previous and current.
        " Note: ecol_{min,max} have already been adjusted for the eolc line itself.
        let ecol_max = max([el_s.pre_max, ecol_max])
        let i -= 1
    endwhile
    call s:Dbg("Finished %d", N-1)
endfunction

" Convert the dp state list to a list of groups in convenient format.
function! s:align_eolc__finalize_groups(dp)
    " Loop over elements in reverse, skipping nodes that are not the end of a group.
    " Assumption: Final element always ends a group.
    let eidx = len(a:dp) - 1
    let grps = []
    while eidx >= 0
        let el = a:dp[eidx]
        let grp = {
            \ 'align': el.grp.align,
            \ 'eolcs': [],
        \ }
        " Add elements to the group.
        let idx = el.grp.sidx
        while idx <= eidx
            " Accumulate a single eol comment line item with all the information required
            " to align it.
            let o = a:dp[idx]
            " Note: No need to save 'line', since it's inherent in the positions.
            call add(grp.eolcs, {'com_s': o.com_s, 'prev_e': [0, o.line, o.ecol, 0]})
            let idx += 1
        endwhile
        " Accumulate group, then move to last element of previous group.
        call add(grps, grp)
        let eidx = el.grp.sidx - 1
    endwhile
    " Since the list was built in reverse order...
    return reverse(grps)
endfunction

" TEMP DEBUG ONLY!!! Remove...
function! s:dbg_show_eolcs(grps)
    let idx = 0
    for grp in a:grps
        call s:Dbg("Group %d align=%d", idx, grp.align)
        " Loop over group members...
        for eolc in grp.eolcs
            call s:Dbg("com_s: %s prev_e: %s", string(eolc.com_s), string(eolc.prev_e))
        endfor
    endfor
endfunction

" Inputs:
"   i:            index of element at head of group (or one past final element) 
"   sidx:         index of first element in group that just ended
"   prev_sidx:    index of first element in group prior to one that just ended
"   prep[]:       list of dicts characterizing eol comment lines
"   ecol_maxes[]: alignment at each index in group that just ended
function! s:align_eolc__preproc_calc(i, sidx, prev_sidx, prep, ecol_maxes)
    " Factor of 2 used to ensure the first group will be at least as long as the second.
    " TODO: Decide whether lookback and threshold should be distinct.
    if i - a:sidx >= 2 * g:sexp_align_eolc_greedy_lookback
        " End of long group. Mark transition to greedy at its start.
        " Note: len and ecol_max keys added later.
        let a:prep[a:sidx].seg = {'mode': "greedy"}
    elseif i - a:sidx <= g:sexp_align_eolc_greedy_lookback
        \ && a:prev_sidx >= 0 && a:prep[a:prev_sidx].mode == "greedy"
        " End of short group following long. Mark transition to dp mode a little
        " before the end of long group. Note that explicit sentinel is unnecessary, given
        " that all greedy segments are explicitly marked with dict that records length.
        let idx = i - g:sexp_align_eolc_greedy_lookback
        " TODO: Do we even need the seg key for dp?
        let a:prep[idx].seg = {'mode': "dp"}
        " Finalize the preceding greedy segment.
        let a:prep[a:prev_sidx].seg.len = idx - a:prev_sidx
        let a:prep[a:prev_sidx].seg.ecol_max =
                    \ a:ecol_maxes[-g:sexp_align_eolc_greedy_lookback]
    endif
endfunction

function! s:align_eolc__preproc_pass2(start, end, prep, opt_level)
    " Set to prevent lookbehind (e.g., when encountering too long a span with no eol
    " comments, or when encountering line comment and g:sexp_align_eolc_break_at_linecom
    " == 1).
    let sog = 1            " start of group flag
    let line_gap = 0       " gap between eol comment lines
    let sidx = 0           " start of current group
    let prev_sidx = -1     " start of previous group
    let [ecol_min, ecol_max] = [0, 0]
    " Cumulative ecol_max at each point in group.
    let ecol_maxes = []
    " Loop over lines in range to do the following, augmenting elements in input list with
    " a seg dict that will facilitate transition between modes in the layout function.
    let [i, N] = [0, len(a:prep)]
    while i < N
        let el = a:prep[i]
        " Check for forcible group break conditions.
        if g:sexp_align_eolc_break_at_linecom && el.is_com && !el.is_eol_com
            " Break at (full) line comment.
            let sog = 1
        elseif g:sexp_align_eolc_maxgap && line_gap > g:sexp_align_eolc_maxgap
            " Gap between eol comment lines is too great to continue any open group.
            let sog = 1
        endif
        if !sog
            " Update bounding box assuming same group.
            let ecol_min = min([el.ecol, ecol_min])
            let ecol_max = max([el.ecol, el.pre_max, ecol_max])
            " Is bounding box still within limits?
            if ecol_max - ecol_min > g:sexp_align_eolc_maxshift
                " Break group.
                let [sog, ecol_maxes] = [1, []]
            else
                " Save alignment in case this ends up being final element of greedy group.
                call add(ecol_maxes, ecol_max)
            endif
        endif
        if sog " starting new group
            " Perform any requisite decoration of segment head elements.
            if opt_level > 0
                call s:align_eolc__preproc_calc(i, sidx, prev_sidx, a:prep, ecol_maxes)
            else
                " TODO: Probably change to is_greedy, or even just make presence of seg
                " imply greedy.
                let el.seg = {'mode': 'greedy', 'len': i - sidx, 'ecol_max': ecol_max}
            endif
            let prev_sidx = sidx
            let sidx = i
        endif
        " Keep up with group extents.
        let line_gap += 1
        let l += 1
    endwhile
    " Process final group.
    call s:align_eolc__preproc_calc(a:prep, i)
endfunction

" Return Dict:
"   line
"   is_com
"   is_eol_com
"   com_start
function! s:align_eolc__preproc_pass1(start, end)
    " List will contain one element for each eol comment in range.
    let ret = []
    " Set to prevent lookbehind (e.g., when encountering too long a span with no eol
    " comments, or when encountering line comment and g:sexp_align_eolc_break_at_linecom
    " == 1).
    let pre_max = 0  " longest line *between* eol comment lines
    " Loop over lines in range to do the following:
    " 1. Determine which algorithm will be used for eol comment grouping/alignment.
    " 2. Prepare the list of structs used by the algorithm-specific function
    "    alignment function.
    let [i, l] = [0, a:start[1]]
    while l <= a:end[1]
        " Get all relevant information about the current line (which may or may not
        " contain comment).
        let [ecol, is_com, is_eol_com, com_start] = s:align_eolc__characterize(l)
        if is_eol_com
            let el = {
                \ 'line': l, 'ecol', 'is_eol_com': is_eol_com, 'is_com': is_com,
                \ 'com_start': com_start, 'pre_max': pre_max}
            call add(ret, el)
            let pre_max = 0
        else
            " Adjust pre_max for non-empty, non-comment line, and increment line gap
            " unconditionally.
            " Rationale: If we're not within a group (i.e., haven't seen first eolc),
            " pre_max is irrelevant and line_gap incrementation is harmless, so don't
            " bother inhibiting adjustments.
            " Design Decision: Line comments have no impact on alignment (unless they're
            " configured to break groups).
            if ecol && !is_com
                let pre_max = max([ecol, pre_max])
            endif
        endif
        let l += 1
    endwhile
endfunction

function! s:align_eolc__opt_level(ncoms)
endfunction

function! s:align_eolc__preproc(start, end)
    let prep = s:align_eolc__preproc_pass1(a:start, a:end)
    let opt_level = s:align_eolc__opt_level(len(prep))
    if opt_level < 2
        call s:align_eolc__preproc_pass2(a:start, a:end, prep, opt_level)
    endif
endfunction

function! s:align_eolc__optimize_range(start, end)
    " Create the DP state array, which will ultimately contain one element per eol comment
    " in range, with elements added as DP optimization progresses.
    let dp = []
    " Set to prevent lookbehind (e.g., when encountering too long a span with no eol
    " comments, or when encountering line comment and g:sexp_align_eolc_break_at_linecom
    " == 1).
    " Design Decision: Since this could just as well have been named 'new_grp', set it
    " for first element, though dp function will work either way.
    let force_break = 1
    " Keep up with longest line *between* eol comment lines.
    let pre_max = 0
    " Keep up with last included line, which is needed to support (option-dependent)
    " detection of excessive gaps.
    let line_gap = 0
    " Loop over lines in range, using DP optimization as we go to determine the best
    " candidate group ending at the current line...
    let l = a:start[1]
    while l <= a:end[1]
        " Get all relevant information about the current line (which may or may not
        " contain comment).
        let [ecol, is_com, is_eol_com, com_start] = s:align_eolc__characterize(l)
        " Check for group break conditions.
        if g:sexp_align_eolc_break_at_linecom && is_com && !is_eol_com
            " Break at (full) line comment.
            call s:Dbg("Breaking due to linecom")
            let [pre_max, force_break] = [0, 1]
        elseif g:sexp_align_eolc_maxgap && line_gap > g:sexp_align_eolc_maxgap
            " Gap between eol comment lines is too great to continue any open group.
            call s:Dbg("Breaking due to maxgap")
            let [pre_max, force_break] = [0, 1]
        endif
        if is_eol_com
            " Line needs to be covered.
            call s:align_eolc__update_dps(dp, l, ecol, com_start, pre_max, force_break)
            let [line_gap, pre_max, force_break] = [0, 0, 0]
        else
            " Adjust pre_max for non-empty, non-comment line, and increment line gap
            " unconditionally.
            " Rationale: If we're not within a group (i.e., haven't seen first eolc),
            " pre_max is irrelevant and line_gap incrementation is harmless, so don't
            " bother inhibiting adjustments.
            " Design Decision: Line comments have no impact on alignment (unless they're
            " configured to break groups).
            if ecol && !is_com
                let pre_max = max([ecol, pre_max])
            endif
            let line_gap += 1
        endif
        let l += 1
    endwhile
    " Reformat the list for easy group traversal.
    return s:align_eolc__finalize_groups(dp)
endfunction

" Align end of line comments within specified range, taking all options into account.
function! s:align_eol_comments(start, end, ps)
    let ts = reltime()
    let grps = s:align_eolc__optimize_range(a:start, a:end)
    call s:dbg_show_eolcs(grps)
    for grp in grps
        let [align, eolcs] = [grp.align, grp.eolcs]
        " Loop over the comments in this group.
        "echomsg align
        for eolc in eolcs
            "echomsg eolc
            " Align the comment by splicing required number of spaces between start of eol
            " comment (exclusive) and position just past end of element preceding it
            " (inclusive).
            " Note: yankdel_range() knows how to handle the case in which those positions
            " are equal.
            call s:yankdel_range(
                    \ eolc.prev_e,
                    \ eolc.com_s,
                    \ repeat(' ', align - eolc.prev_e[2] + 2),
                    \ [1, 0],
                    \ a:ps)
        endfor
    endfor
    echomsg printf("Alignment took: %f", reltimefloat(reltime(ts)))
endfunction

" Indent S-Expression, maintaining cursor position. This is similar to mapping
" to =<Plug>(sexp_outer_list)`` except that it will fall back to top-level
" elements not contained in a compound form (e.g. top-level comments).
function! sexp#indent(mode, top, count, clean, ...)
    let win = winsaveview()
    let cursor = getpos('.')
    let [_b, line, col, _o] = getpos('.')
    let force_syntax = a:0 && !!a:1
    " If caller hasn't specified clean, defer to option.
    let clean = a:clean < 0 ? g:sexp_indent_does_clean : !!a:clean
    " Were positions supplied for adjustment?
    let ps = a:0 > 1 ? a:2 : []
    " Note: This flag can be set (but not cleared) post init.
    let at_top = a:top

    if a:mode ==? 'n'
        " Save original visual marks for restoration after adjustment.
        " Rationale: Use of visual selection to perform indent is an
        " implementation detail that should be completely transparent.
        let [vs, ve] = s:get_visual_marks()
        " Move to current list tail since the expansion step of
        " s:set_marks_around_current_list() happens at the tail.
        if getline(line)[col - 1] =~ s:closing_bracket
            \ && !s:is_rgn_type('str_com_chr', line, col)
            let pos = [0, line, col, 0]
        else
            let pos = s:move_to_nearest_bracket(1)
        endif

        normal! v
        if pos[1] < 1
            let at_top = 1
            " At top-level. If current (or next) element is list, select it.
            " Note: When not within list, 'inner' includes brackets.
            keepjumps call sexp#select_current_element('n', 1)
        elseif a:top
            " Inside list. Select topmost list.
            keepjumps call sexp#select_current_top_list('n', 0)
        else
            " Inside list. Select [count]th containing list.
            " If performing clean, select only inner list.
            " Rationale: cleanup_ws will get any open or close adjacent to
            " selection, and we want to stop at the edge of current list.
            keepjumps call sexp#docount(a:count, 'sexp#select_current_list', 'n', clean, 1)
        endif
        " Cache visual start/end; end can actually be changed by s:cleanup_ws().
        let [start, end] = s:get_visual_marks()
        " We're done with visual mode. Leave it to avoid problems below (eg,
        " with function calls).
        exe "normal! \<Esc>"
    else
        " Treat visual mode specially.
        " Rationalize visual range.
        let [vs, ve] = s:get_visual_marks()
        let [start, end] = s:super_range(vs, ve)
    endif
    if clean
        " Always force syntax update when we're modifying the buffer.
        let force_syntax = 1
        " Design Decision: Handle both non-list and list elements identically:
        " cleanup back to prev, but indent starting with current.
        " Note: Avoid unnecessary calls to at_top().
        let at_top = at_top || s:at_top(end[1], end[2])
        call s:cleanup_ws(start, at_top,
            \ s:concat_positions(ps, start, end, cursor,
                \ a:mode ==? 'n' ? [vs, ve] : []), end)
    endif
    " Caveat: Attempting to apply = operator in visual mode does not work
    " consistently.
    if force_syntax
        " Force syntax update on visual lines before running indent.
        " Rationale: Certain indent functions rely on syntax attributes to
        " calculate indent: e.g., GetClojureIndent() contains a call to
        " s:MatchPairs(), which in turn contains a call to searchpairpos(),
        " which can find the wrong bracket if pasted text has not yet had its
        " syntax recalculated (e.g., because the paste and subsequent indent
        " happen in a single command). Caller should set the force_syntax flag
        " in such scenarios to force syntax recalculation prior to the =.
        exe start[1] . ',' . end[1] . 'call synID(line("."), col("."), 1)'
    endif
    " Position pre-adjustment
    let adj = s:indent_preadjust_positions(
        \ s:concat_positions(ps, start, end, cursor,
            \ a:mode ==? 'n' ? [vs, ve] : []))
    silent keepjumps exe "normal! " . start[1] . 'G=' . end[1] . "G"
    " Position post-adjustment
    call s:indent_postadjust_positions(adj)
    " (Optional) end of line comment alignment
    if g:sexp_align_eolc
        call s:align_eol_comments(start, end, ps)
    endif
    " Adjust window view object to account for buffer changes made by the
    " indent (and possibly by s:cleanup_ws).
    " FIXME: Since this function is called internally (e.g., from sexp#clone),
    " should probably either factor out the cursor/window restoration (e.g.,
    " putting it into a static workhorse function that can be called by
    " sexp#clone as well), or make it selectable.
    let win.lnum = cursor[1]
    let win.col = cursor[2] - 1 " .col is zero-based
    " Design Decision: In normal mode, restore old (adjusted) visual
    " selection; in visual mode, restore the adjusted super-range.
    call s:set_visual_marks(a:mode ==? 'n' ? [vs, ve] : [start, end])
    call winrestview(win)
endfunction

" Create a flat list encompassing all input positions.
" Note: The flat list is intended to facilitate iteration: the positions it
" contains are generally modifed in-place.
" TODO: Consider adding position uniquifying logic.
" Rationale: The created lists are generally used in pass-by-ref position
" modification strategies, which would modify the same position multiple times
" if it somehow made it into the list multiple times.
function! s:concat_positions(...)
    let ret = []
    for p in a:000
        " p will be one of the following:
        "   1) empty list 2) position, 3) position list
        " Caveat: Discard null positions appearing either singly or in list.
        if !empty(p) && p != [0, 0, 0, 0]
            let ret += type(p[0]) == 0
                \ ? [p]
                \ : filter(copy(p), 'v:val != [0, 0, 0, 0]')
        endif
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
    let close = sexp#current_element_terminal(1)
    let ret = [0, 0, 0, 0]
    " Attempt move to first non-whitespace.
    let [l, c] = s:findpos('\S', 1)
    if [0, l, c, 0] == close
        " Empty form
        return ret
    endif
    call cursor(l, c) 
    " Just to be sure...
    let ret = sexp#current_element_terminal(0)
    " Restore original position.
    call s:setcursor(cursor)
    return ret
endfunction

" FIXME: When function called with optional end argument, we don't assume that
" start is an open; rather, start and (optional) close demarcate the cleanup
" as follows: start at element *before* start and end with element past close.
function! s:cleanup_ws(start, at_top, ps, ...)
    let end = a:0 ? a:1 : [0, 0, 0, 0]
    let [open, close, prev] = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
    " TODO: Consider factoring this first if/else into its own function: cleanup_init, or
    " some such...
    if !end[1]
        let open = a:start[:]
        " Cleanup a list.
        call s:setcursor(open)
        " Descend into list.
        let next = s:list_head()
    else
        " Cleanup specified range without assuming anything about start.
        " Set things up as though loop processing is already in progress:
        " e.g., set open, prev and next (if non-null).
        " Note: eff_* will be set in loop pre-update, so no need to set here.
        call s:setcursor(a:start)
        let next = sexp#current_element_terminal(0)
        if !next[1]
            " Not in element.
            let next = s:nearest_element_terminal(1, 0)
            " Note: nearest_element_terminal returns current pos on failure.
            if !s:compare_pos(next, getpos("."))
                " No element on or after start. Null next so that close will
                " be set in loop...
                let next = [0, 0, 0, 0]
            endif
        endif
        let prev = s:nearest_element_terminal(0, 1)
        if !s:compare_pos(prev, getpos("."))
            " no previous element
            let prev = [0, 0, 0, 0]
        endif
        if !prev[1]
            let open = s:nearest_bracket(0)
        endif
    endif
    let done = 0
    while 1
        " Note: next and close are mutually exclusive.
        let close = next[1] ? [0, 0, 0, 0] : s:nearest_bracket(1)
        " Distinction: Non-null prev/next always represent actual elements at
        " current level; eff_prev/next can be either element or open/close.
        " TODO: Consider handling eff_prev in post-update (or some other way).
        " Rationale: After first iteration, it will always be prev (not open).
        let bof = 0
        let eff_prev = prev[1] ? prev : open
        if !eff_prev[1]
            let eff_prev = [0, 1, 1, 0]
            let bof = 1
        endif
        let eff_next = next[1] ? next : close
        if !eff_next[1]
            " Assumption: ve=onemore obviates need for eof flag.
            let eff_next = getpos([line('$'), '$'])
        endif

        " Do we want to remove *all* whitespace between eff_prev and eff_next?
        let full_join =
                \ !next[1] && !prev[1] " empty form
                \ || !next[1] && (!close[1] || !s:is_comment(prev[1], prev[2]))
                \ || !prev[1] && (!open[1] || !s:is_comment(next[1], next[2]))

        " Note: A single call to yankdel_range with 'splice' arg will be used to perform
        " any required whitespace contraction: calculate the 'splice' arg, which can be
        " either 1 (delete) for a full join, or actual splice text consisting of a single
        " space or newline(s).
        let spl = 0
        if full_join
            " Delete rather than splice.
            let spl = 1
        else
            " Maybe splice...
            let gap = eff_next[1] - eff_prev[1]
            if gap
                " Multi-line
                " Contract whitespace between prev and next if any of the following
                " conditions holds true for the gap between prev and next:
                " * # of blank lines > g:sexp_cleanup_keep_empty_lines
                " * (else) trailing whitespace on prev line (remove trailing ws)
                let precedes_com = next[1] && s:is_comment(next[1], next[2])
                if gap > g:sexp_cleanup_keep_empty_lines + 1
                    \ || getline(eff_prev[1])[eff_prev[2] - 1:] =~ '.\s\+$'
                    " Replace gap with number of newlines determined by existing line gap
                    " and g:sexp_cleanup_keep_empty_lines option, followed by original
                    " whitespace on eff_next's line:
                    let spl = repeat("\n", min([gap, g:sexp_cleanup_keep_empty_lines + 1]))
                endif
            " Single-line (whitespace between colinear elements)
            " If next isn't comment and there are multiple whitespace chars between
            " eff_prev and eff_next, collapse to a single whitespace.
            " Rationale: Only before comment does extra (non-leading) ws make sense.
            " FIXME: Refusing to collapse whitespace *immediately* preceding comment
            " doesn't really solve anything, since collapsing ws earlier on the line will
            " still break alignment. Really need to add the eol comment alignment logic...
            " Cursor Logic: If cursor is in whitespace to be contracted, but not on
            " *first* whitespace in the range, we want it to end up *past* the single
            " remaining space; otherwise, on it. FIXME: Currently, this can mean past end
            " of line, but perhaps it shouldn't be allowed to move to next line.
            elseif g:sexp_cleanup_collapse_whitespace
                \ && getline(eff_prev[1])[eff_prev[2] - 1 : eff_next[2] - 1] =~ '.\s\s'
                \ && next[1] && !s:is_comment(next[1], next[2])
                " Replace multiple whitespace on single line with single space.
                " Assumption: BOF and EOF are always handled as full join
                let spl = ' '
            endif
        endif
        if !empty(spl)
            " Prevent pointless calls to s:yankdel_range (when there's no
            " whitespace to contract).
            if !(bof && eff_next[1:2] == [1, 1] ||
                \ !bof && s:offset_char(eff_prev, 1) == eff_next)
                " Perform the indicated whitespace contraction.
                " Argument Notes:
                " *Normally, range to be spliced is exclusive, but cleaning
                "  back to bof is special case. (ve=onemore obviates need for
                "  special case at eof)
                " *Never remove leading whitespace on 'next' line.
                call s:yankdel_range(eff_prev,
                        \ spl[0] == "\n" ? [0, eff_next[1], 1, 0] : eff_next,
                        \ spl,
                        \ [bof, 0],
                        \ s:concat_positions(a:ps, eff_next, end))
            endif
        endif

        " TODO: Consider processing backwards to obviate need for 'next'
        " adjustment. (Note that advantage is not as significant now that
        " position adjustment is handled by yankdel_range.)
        " Note: Only zero/nonzero status of next is safe to use at this point
        " (since it wasn't adjusted by yankdel_range).
        if done || !next[1] | break | endif
        " If here, there's another element at this level.
        " Assumption: eff_next and next are the same except that the former has been
        " adjusted.
        call cursor(eff_next[1], eff_next[2])
        if s:is_list(eff_next[1], eff_next[2])
            let next = s:move_to_list_open()
            call s:cleanup_ws(next, 0, a:ps)
            " Assumption: Restore cursor pos (potentially changed by
            " recursion) to next (which can't be invalidated by recursion).
            call s:setcursor(next)
        endif
        " Now that we've recursed (if possible), attempt to advance.
        let prev = s:move_to_current_element_terminal(1)
        let next = s:nearest_element_terminal(1, 0)
        if next == prev
            " No more elements at current level.
            " Note: Null next to ensure attempt to find close on next and
            " final iteration of this recursion.
            let next = [0, 0, 0, 0]
        elseif end[1] && s:compare_pos(next, end) > 0
            " Next element is past range. Go through once more to clean up
            " after final element.
            let done = 1
        else
            call s:setcursor(next)
        endif
    endwhile
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
            call s:warnmsg("Convolute impossible with count given: insufficient nesting")
            call s:setcursor(cursor)
            return
        endif
        if !idx
            let bpos_i = p
            " Caveat: Don't change cursor pos.
            let spos_i = sexp#current_element_terminal(0)
            let tpos_i = s:nearest_bracket(1)
        endif
        let idx += 1
    endwhile
    let bpos_o = p
    let spos_o = sexp#current_element_terminal(0)
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
        let pos = sexp#current_element_terminal(0)
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
    " Let 'clean' be determined by options.
    call sexp#indent('n', 0, 2, -1)

    " Re-calculate pos for final cursor positioning.
    " Note: When outer list ends on a different line from inner list, the
    " convolution will decrease number of close brackets after pos by 1.
    " Assumption: Closing brackets always a single byte.
    let pos[2] = col([pos[1], '$']) - pos_edist + edist_changing
    call s:setcursor(pos)
endfu

" Return [start, end] of region to be cloned.
function! s:get_clone_target_range(mode, after, list)
    let cursor = getpos('.')
    if a:mode ==? 'v'
        " Let set_marks_around_current_element adjust the range.
        return s:set_marks_around_current_element('v', 1, 0, 1)
    else
        " Select list/element to be cloned.
        if a:list
            " Are we within/on a list?
            let found = sexp#select_current_list('n', 0, 0)
            " Caveat! Don't stay in visual mode.
            exe "normal! \<Esc>"
            if found
                " Design Decision: Perform inner element selection to
                " incorporate any adjacent macro chars.
                call sexp#select_current_element('n', 1)
                let [vs, ve] = s:get_visual_marks()
                " Make sure we're on or in the found list.
                " Rationale: select_current_list can find list after cursor,
                " and we're not interested in those.
                if s:compare_pos(cursor, vs) >= 0 && s:compare_pos(cursor, ve) <= 0
                    return [vs, ve]
                endif
            endif
            " Not on or in list
            return [[0, 0, 0, 0], [0, 0, 0, 0]]
        else
            " Are we on an element?
            let p = sexp#current_element_terminal(0)
            let found = p[1]
            " Consider an element past the cursor.
            " Rationale: Feels right.
            if !found
                let p = getpos('.')
                " Not on an element. Find adjacent (if one exists in applicable
                " direction).
                call s:move_to_adjacent_element(1, 0, 0)
                let found = p != getpos('.')
            endif
            return found
                \ ? s:set_marks_around_current_element('n', 1, 0, 1)
                \ : [[0, 0, 0, 0], [0, 0, 0, 0]]
        endif
    endif
endfunction

" Clone list/element at cursor (normal mode) or range of elements partially or
" fully included in visual selection.
" Design Decision: In normal mode, change to visual selection should be a
" completely transparent and temporary side-effect of the implementation:
" thus, we restore the (adjusted) original selection (if any). In
" visual/operator modes, otoh, we restore the adjusted inner selection
" corresponding to the copied range.
function! sexp#clone(mode, count, list, after, force_sl)
    let cursor = getpos('.')
    let keep_vs = a:mode ==? 'n'
    if keep_vs
        " Save original selection for adjustment and subsequent restoration.
        let [vs, ve] = s:get_visual_marks()
    endif

    " Get region to be cloned.
    let [start, end] = s:get_clone_target_range(a:mode, a:after, a:list)
    if !start[1]
        " Nothing to clone.
        call s:warnmsg("Nothing to clone")
        return
    endif
    " Design Decision: If cursor starts in whitespace before target, move it
    " to head of target to ensure that cursor always stays with target.
    if s:compare_pos(cursor, start) < 0
        let cursor = start[:]
    endif
    " Assumption: Prior logic guarantees start and end at same level.
    let top = s:at_top(start[1], start[2])
    " Logic: By default, clone will be multi-line if any of the following
    " conditions holds:
    "  1. target is alone on its line
    "  2. target is at toplevel
    "  3. target ends in comment
    " The default logic can be overridden in 2 ways:
    "  1. explicit [count] supplied => forces multi-line
    "  2. single-line map variant used => forces single-line *unless* target
    "     ends in comment, in which case, clone is always multi-line
    let force_l = a:force_sl ? 's' : a:count ? 'm' : ''
    let multi = force_l == 'm'
        \ || s:is_comment(end[1], end[2])
        \ || force_l != 's'
        \ && (top || start[1] != end[1]
            \ || s:at_bol(start[1], start[2]) && s:at_eol(end[1], end[2]))
    " Get the text to be copied.
    let copy = s:yankdel_range(start, end, 0, 1)
    call s:setcursor(a:after ? end : start)
    let repl = multi
        \ ? a:after ? ["\n", copy] : [copy, "\n"]
        \ : a:after ? [" ", copy] : [copy, " "]
    let copy = join(repeat(repl, a:count ? a:count : 1), "")

    if !keep_vs
        " Save the target range, which will become the new selection after
        " adjustment.
        let [vs, ve] = [copy(start), copy(end)]
    endif
    " Implement put with yankdel_range to take advantage of position
    " adjustment.
    let p = a:after ? end : start
    let inc = a:after ? [0, 1] : [1, 0]
    " TODO: Consider creating a put_at wrapper for this.
    call s:yankdel_range(p, p, copy, inc,
            \ s:concat_positions([end, cursor], [vs, ve]))
    " Design Decision: Single line clone can't change indent.
    " Rationale: If it's wrong now, it was already wrong, as we haven't done
    " anything that should have any impact on indentation.
    let need_indent = multi && !!g:sexp_clone_does_indent
    if need_indent
        if top
            " At toplevel, there's no parent to constrain the indent, and we
            " may need to indent multiple toplevel forms, so select them all
            " and do visual mode indent.
            " Assumpton: multi == true
            " Note: One end of the region to be indented has been adjusted by
            " yankdel_range; the other can be obtained from [ or ] mark.
            if a:after
                " start cannot have changed
                let end = getpos("']")
            else
                " end was adjusted by yankdel_range
                " TODO: Can start ever change on a copy before? I don't think
                " so. If not, remove the else.
                let start = getpos("'[")
            endif
            call s:set_visual_marks([start, end])
            call sexp#indent('v', 1, 0, -1, 1, [start, end, cursor, vs, ve])
        else
            " Indent parent
            " Note: Because of the way sexp#indent works, we need to know
            " whether cursor is on an open or close.
            let isl = s:is_list(line('.'), col('.'))
            " Caveat: Failure to set optional force_syntax flag in call to
            " indent may result in incorrect indentation.
            call sexp#indent('n', 0, isl > 1 ? 2 : 1, -1, 1,
                    \ [start, end, cursor, vs, ve])
        endif
    endif

    call s:setcursor(cursor)

    " Adjust visual marks. See note in header on what vs/ve represent.
    call s:set_visual_marks([vs, ve])
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

    let start = s:get_visual_beg_mark()

    if start[1] > 0
        " Delete ending bracket first so we don't mess up '<
        call s:setcursor(s:get_visual_end_mark())
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
        let pos = sexp#current_element_terminal(1)
        let tail = (pos[1] > 0 && getline(pos[1])[pos[2] - 1] =~# s:closing_bracket)
                   \ ? pos
                   \ : s:nearest_bracket(1)
        if tail[1] < 1
            delmarks < >
        else
            call s:setcursor(tail)
            call s:set_marks_around_current_element('o', 1, 0, 0)
        endif
    else
        call s:set_marks_around_current_element('o', 1, 0, 0)
    endif

    if s:get_visual_beg_mark()[1] < 1 || !s:swap_current_selection(a:mode, a:next, pairwise)
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

    if s:is_rgn_type('str_com_chr', line, col)
        \ && s:compare_pos(sexp#current_element_terminal(0), [0, line, col, 0]) < 0
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

    if s:is_rgn_type('str_com_chr', line, col)
        \ && s:compare_pos(sexp#current_element_terminal(0), [0, line, col, 0]) < 0
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

    if s:is_rgn_type('string', line, col)
        let curline = getline(line)

        " User is trying to insert an escaped quote, so do it
        if curline[col - 2] ==# '\'
            return a:quote
        else
            return curline[col - 1] ==# a:quote ? "\<C-G>U\<Right>" : a:quote
        endif
    elseif s:is_rgn_type('str_com_chr', line, col)
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
        \ && s:is_rgn_type('string', line, col)
        \ && !escaped
        \ && pprev !~# '"'
        return "\<BS>\<Del>"
    elseif !s:is_rgn_type('str_com_chr', line, col)
        \ && !escaped
        \ && prev =~# s:opening_bracket
        \ && cur ==# s:pairs[prev]
        return "\<BS>\<Del>"
    else
        return "\<BS>"
    endif
endfunction

" vim:ts=4:sw=4:et:tw=90
