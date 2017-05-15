
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

" Given start and end positions, returns new positions [start', end']:
"
"   * If trailing whitespace after end, end' is set to include the trailing
"     whitespace up to the next element, unless start is preceded on its line
"     by something other than whitespace (but not an opening bracket), in
"     which case end' is set to include only the trailing whitespace to the
"     end of line.
"   * If no trailing whitespace after end, start' is set to include leading
"     whitespace up to the previous element on the same line if any exist.
"   * Otherwise start and end are returned verbatim.
"
" This behavior diverges from the behavior of the native text object aw in
" that it allows multiline whitespace selections.
function! s:terminals_with_whitespace(start, end)
    let start = a:start
    let end = a:end
    let ws_end = s:adjacent_whitespace_terminal(end, 1)

    " There is trailing whitespace
    if s:compare_pos(end, ws_end) != 0
        " ws_end is on the same line as end, so accept it
        if end[1] == ws_end[1]
            let end = ws_end
        " start begins its line or is immediately preceded by an opening
        " bracket, so include all of ws_end, which is on a subsequent line.
        " Note that the double substring slicing here is intentional in order
        " to avoid calculating the substring index.
        elseif getline(start[1])[: start[2] - 1][: -2] =~# '\v^\s*$|[([{]$'
            let end = ws_end
        " start does not begin its line, so just include any trailing
        " whitespace to eol, not to ws_end
        elseif getline(end[1])[end[2]] =~# '\v\s'
            let end = s:pos_with_col_offset(end, col([end[1], '$']) - 1 - end[2])
        " end does not have trailing whitespace on its own line, so include
        " leading whitespace to previous element on same line
        else
            let start = s:adjacent_whitespace_terminal(start, 0)
        endif
    " Otherwise include leading whitespace unless start begins its line
    elseif getline(start[1])[: start[2] - 1][: -2] !~# '\v^\s*$'
        let start = s:adjacent_whitespace_terminal(start, 0)
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

""" PREDICATES AND COMPARATORS {{{1

" Returns 1 if char matches the current FileType's macro pattern
function! s:is_macro_char(char)
    return stridx(s:macro_chars(), a:char) >= 0
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

" Tries to move cursor to nearest paired bracket, returning its position.
function! s:move_to_nearest_bracket(closing)
    let pos = s:nearest_bracket(a:closing)
    if pos[1] > 0 | call s:setcursor(pos) | endif
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

" Set visual marks to the start and end of the current element. If
" inner is 0, trailing or leading whitespace is included by way of
" s:terminals_with_whitespace().
"
" If cursor is on whitespace that is not in a string or between line comments,
" the marks are set around the next element if inner is 1, and around the
" current position and end of the next element if inner is 0.
"
" Will set both to [0, 0, 0, 0] if an element could not be found and mode does
" not equal 'v'.
function! s:set_marks_around_current_element(mode, inner)
    let cursor = getpos('.')
    let start = s:current_element_terminal(0)
    let end = [0, 0, 0, 0]
    let include_ws = !a:inner

    " We are on whitespace; check for next element
    if start[1] < 1
        let next = s:move_to_adjacent_element(1, 0, 0)

        " No next element! We are at the eof or in a blank buffer.
        if s:compare_pos(next, cursor) == 0
            if a:mode !=? 'v'
                delmarks < >
            endif
            return
        endif

        let include_ws = 0
        let start = a:inner ? next : cursor
    else
        " Search from element start to avoid errors with elements that end
        " with macro characters. e.g. Clojure auto-gensyms: `(let [s# :foo)])
        call s:setcursor(start)
    endif

    let end = s:current_element_terminal(1)

    if include_ws
        let [start, end] = s:terminals_with_whitespace(start, end)
    endif

    call s:setcursor(cursor)
    call s:set_visual_marks([start, end])
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
        normal! gv
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
function! sexp#select_current_element(mode, inner)
    call s:set_marks_around_current_element(a:mode, a:inner)
    return s:select_current_marks(a:mode)
endfunction

" Set visual marks around adjacent element and enter visual mode; 0 for
" previous, 1 for next. If no such adjacent element exists, selects current
" element.
function! sexp#select_adjacent_element(mode, next)
    call s:set_marks_around_adjacent_element(a:mode, a:next)
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

" Yank (del=0) or delete (del=1) text in range defined by start/end. Optional
" 'inc' arg allows inclusivity of range to be specified independently for
" start and end.
" Note: Allow caller to specify 'inc' either as list or bool, with a bool
" assumed to apply only to end (with start being included by default). If inc
" arg is omitted, defaults to [1, 0]: i.e., start=inclusive, end=exclusive.
" Cursor Note: If we move cursor, we'll leave it at start of operated range.
function! s:yankdel_range(start, end, del, ...)
    let inc = a:0 ? type(a:1) == 3 ? a:1 : [1, a:1] : [1, 0]
    " Make sure there's a point in continuing.
    let cmp = s:compare_pos(a:start, a:end)
    if cmp > 1 || cmp == 0 && inc != [1, 1]
        " Nothing to do!
        return ''
    endif
    let cursor = getpos('.')
    " Nomenclature: 's'=start, 'e'=end, 'l'=line, 'c'=col, 't'=text
    " Cache line/col positions in more convenient form, converting 1-based col
    " positions to 0-based equivalents.
    let [sl, el] = [a:start[1], a:end[1]]
    let [sc, ec] = [a:start[2] - 1, a:end[2] - 1]

    " Allow easy differentiation between collinear and non-collinear case.
    let co = sl == el
    " Cache the partial line(s).
    let slt = getline(sl)
    if !co | let elt = getline(el) | endif
    " Shift start/end by one char position, as indicated by corresponding inc
    " flag, taking care to allow for multi-byte chars.
    if !inc[0]
        let sc = matchend(slt, '.', sc)
    endif
    if inc[1]
        let ec = matchend(co ? slt : elt, '.', ec)
    endif

    " Get text from 'start' to either 'end' (collinear) or EOL
    let ret = slt[sc : (co ? ec - 1 : -1)]
    if a:del
        " Combine undeleted text (possibly empty) from initial and final lines
        " (which could be the same line).
        call setline(sl, strpart(slt, 0, sc) . strpart(co ? slt : elt, ec))
    endif
    if !co
        if a:del
            " Before its line number is invalidated, delete final line, part
            " of which may already have been appended to what remains of first
            " line in range.
            exe el "d _"
        endif
        " Handle multi-line case, omitting the final line, which is handled
        " specially.
        if sl < el - 1
            let reg_save = @a
            exe sl + 1 "," el - 1 a:del ? "d" : "y" "a"
            let ret .= "\n" . @a
            let @a = reg_save
        else
            " No linewise :y/:d cmd means we have to add NL before final line
            " manually.
            let ret .= "\n"
        endif
        " Accumulate in-range portion of final line.
        let ret .= strpart(elt, 0, ec)
    endif
    " Fix cursor position if it was invalidated by delete.
    if a:del
        " Reposition cursor if necessary.
        let p1 = [0, sl, sc + 1, 0]
        if s:compare_pos(cursor,  p1) > 0
            let p2 = [0, el, ec + 1, 0]
            if s:compare_pos(cursor, p2) <= 0
                " Move back to head of deletion.
                let cursor = p1
            else
                " Adjust for lines deleted and, if cursor was on final
                " line of range, account for col shift engendered by deletion.
                let cursor[1] -= el - sl
                if cursor[1] == el
                    " Collinearity determines shift direction.
                    let cursor[2] += co ? sc - ec : sc
                endif
            endif
        endif
    endif
    " Restore cursor
    call s:setcursor(cursor)
    return ret
endfu

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

" Indent S-Expression, maintaining cursor position. This is similar to mapping
" to =<Plug>(sexp_outer_list)`` except that it will fall back to top-level
" elements not contained in an compound form (e.g. top-level comments).
function! sexp#indent(top, count)
    let win = winsaveview()
    let [_b, line, col, _o] = getpos('.')

    " Move to current list tail since the expansion step of
    " s:set_marks_around_current_list() happens at the tail.
    if getline(line)[col - 1] =~ s:closing_bracket && !s:syntax_match(s:ignored_region, line, col)
        let pos = [0, line, col, 0]
    else
        let pos = s:move_to_nearest_bracket(1)
    endif

    normal! v
    if pos[1] < 1
        keepjumps call sexp#select_current_element('v', 1)
    elseif a:top
        keepjumps call sexp#select_current_top_list('v', 0)
    else
        keepjumps call sexp#docount(a:count, 'sexp#select_current_list', 'v', 0, 1)
    endif
    normal! =

    call winrestview(win)
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
