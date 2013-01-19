
"    .o            o8o                                                              o.
"   .8'            '"'                                                              `8.
"  .8' oooo    ooooooo ooo. .oo.  .oo.        .oooo.o .ooooo. oooo    ooooo.ooooo.   `8.
"  88   `88.  .8' `888 `888P"Y88bP"Y88b      d88(  "8d88' `88b `88b..8P'  888' `88b   88
"  88    `88..8'   888  888   888   888 8888 `"Y88b. 888ooo888   Y888'    888   888   88
"  `8.    `888'    888  888   888   888      o.  )88b888    .o .o8"'88b   888   888  .8'
"   `8.    `8'    o888oo888o o888o o888o     8""888P'`Y8bod8P'o88'   888o 888bod8P' .8'
"    `"                                                                   888       "'
"                                                                        o888o
" Author:   guns <self@sungpae.com>
" Version:  0.1
" License:  MIT
" Homepage: https://github.com/guns/vim-sexp

if exists('g:sexp_autoloaded')
    finish
endif
let g:sexp_autoloaded = 1

" TODO:
"
" * Deliberately set jump marks so users can `` back after undo.
" * Don't ignore virtualedit mode?
" * Use tpope's repeat.vim to enable '.' command for our <Plug> mappings
" * Comments should always be swapped to their own line
" * When selecting non-atoms as elements, include all non-delimiter chars that
"   are adjacent to them, like reader macro characters and Clojure's
"   spacing comma.
" * Don't delete closing bracket when parens are not balanced

""" PATTERNS AND STATE {{{1

if !exists('g:sexp_maxlines')
    let g:sexp_maxlines = -1
endif

let s:countindex = 0 " Stores current count index during sexp#docount
let s:bracket = '\v\(|\)|\[|\]|\{|\}'
let s:opening_bracket = '\v\(|\[|\{'
let s:closing_bracket = '\v\)|\]|\}'
let s:delimiter = s:bracket . '|\s'
let s:string_scope = '\vstring|regex|pattern'
let s:ignored_scope = s:string_scope . '|comment|char'
let s:macro_characters = {
    \ 'clojure': ['#', "\\v[#'`~@^,]"],
\ }
let s:pairs = {
    \ '(': ')',
    \ '[': ']',
    \ '{': '}',
    \ ')': '(',
    \ ']': '[',
    \ '}': '{',
    \ '"': '"'
\ }

""" QUERIES AT CURSOR {{{1

" Like searchpos(), return first pattern match from cursor as [line, col].
" Unlike searchpos(), searching backward when the cursor is on a multibyte
" character does not move the cursor too far.
"
" cf. https://groups.google.com/forum/?fromgroups=#!topic/vim_dev/s7c_Qq3K1Io
"
" One extra argument may be supplied: the stopline parameter of searchpos().
function! s:findpos(pattern, next, ...)
    if a:next
        let [line, col] = searchpos(a:pattern, 'nW', a:0 ? a:1 : 0)
    else
        let [_b, line, col, _o] = getpos('.')
        let [sline, scol] = searchpos(a:pattern, 'bnW', a:0 ? a:1 : 0)
        " Bug only occurs when match is on same line
        let possible = sline == line
                       \ && &encoding ==? 'utf-8'
                       \ && char2nr(getline(sline)[scol - 1]) > 0x7f
        if possible && s:is_backward_multibyte_search_broken()
            let col = scol + byteidx(getline(line), virtcol('.')) - col('.')
        else
            let [line, col] = [sline, scol]
        endif
    endif

    return [line, col]
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
    let skip = 's:is_ignored_scope(line("."), col("."))'
    let stopline = g:sexp_maxlines > 0
                   \ ? line('.') + (a:closing ? g:sexp_maxlines : -g:sexp_maxlines)
                   \ : 0
    let open = a:0 ? a:1 : s:opening_bracket
    let close = a:0 ? a:2 : s:closing_bracket
    let [line, col] = searchpairpos(open, '', close, flags, skip, stopline)
    return line > 0 ? [0, line, col, 0] : [0, 0, 0, 0]
endfunction

" Position of outermost paired bracket: 0 for opening, 1 for closing.
" Returns [0, 0, 0, 0] if none found.
"
" If global variable g:sexp_maxlines is -1, a fast best-effort approach is
" used instead of a recursive searchpairpos()
function! s:current_top_form_bracket(closing)
    let [_b, line, col, _o] = getpos('.')
    if g:sexp_maxlines < 0
        " Recursive searchpairpos() is excruciatingly slow on a large file.
        " This can be addressed somewhat by providing a stopline argument, but
        " this makes the call a best-effort approach. If we are sacrificing
        " correctness for performance, we can do even better by assuming that
        " all opening brackets on the first column of a line are toplevel.
        let top = 0

        " Assume we're at the top level if the current element begins on the
        " first column
        let [_b, l, c, _o] = s:current_element_terminal(0)
        if l > 0 && c == 1 | let top = 1 | endif

        while !top
            let [_b, l, c, _o] = s:move_to_nearest_bracket(0)
            if l > 0 && c == 1
                let top = 1
            elseif l < 1
                break
            endif
        endwhile

        let closing = (top && getline(l)[c - 1] =~ s:opening_bracket)
                      \ ? s:nearest_bracket(1)
                      \ : [0, 0, 0, 0]

        call cursor(line, col) " Restore position

        return closing[1] > 0
               \ ? (a:closing ? closing : [0, l, c, 0])
               \ : [0, 0, 0, 0]
    else
        let flags = a:closing ? 'cnr' : 'bcnr'
        let skip = 's:is_ignored_scope(line("."), col("."))'
        let stopline = g:sexp_maxlines > 0
                       \ ? line + ((a:closing ? 1 : -1) * g:sexp_maxlines)
                       \ : 0
        let [topline, topcol] = searchpairpos(s:opening_bracket, '', s:closing_bracket, flags, skip, stopline)

        if topline > 0
            return [0, topline, topcol, 0]
            " searchpairpos() fails to find the matching closing bracket when on the
            " outermost opening bracket and vice versa
        elseif getline(line)[col - 1] =~ (a:closing ? s:opening_bracket : s:closing_bracket)
            return s:nearest_bracket(a:closing)
        else
            return [0, 0, 0, 0]
        endif
    endif
endfunction

" Position of start / end of current string: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in a string.
function! s:current_string_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')

    if !s:syntax_match(s:string_scope, cursorline, cursorcol)
        return [0, 0, 0, 0]
    endif

    let [termline, termcol] = [cursorline, cursorcol]

    " We can't rely on va" or on searchpairpos() because they don't work well
    " on symmetric patterns. Also, we aren't searching for just double quotes
    " because then we can be generic at a small cost.
    "
    " We also use s:findpos() while moving the cursor because using simple
    " column arithmetic breaks on multibyte characters.
    while 1
        let [line, col] = s:findpos('\v.', a:end)

        " Beginning or end of file.
        if line < 1 | break | endif

        if s:syntax_match(s:string_scope, line, col)
            let [termline, termcol] = [line, col]
            call cursor(line, col)
        else
            break
        endif
    endwhile

    call cursor(cursorline, cursorcol)
    return [0, termline, termcol, 0]
endfunction

" Position of start / end of current comment: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in a comment.
function! s:current_comment_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')
    if !s:is_comment(cursorline, cursorcol) | return [0, 0, 0, 0] | endif

    let [termline, termcol] = [cursorline, cursorcol]

    while 1
        let [line, col] = s:findpos('\v\_.', a:end)

        if line < 1 | break | endif

        if s:is_comment(line, col)
            let [termline, termcol] = [line, col]
            call cursor(line, col)
        else
            break
        endif
    endwhile

    call cursor(cursorline, cursorcol)
    return [0, termline, termcol, 0]
endfunction

" Position of start / end of current atom: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in an atom. Assumes atoms never span multiple
" lines.
function! s:current_atom_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')
    if !s:is_atom(cursorline, cursorcol) | return [0, 0, 0, 0] | endif

    let [line, termline, termcol] = [cursorline, cursorline, cursorcol]

    while 1
        let [line, col] = s:findpos('\v.', a:end, line)
        if line < 1 | break | endif

        if s:is_atom(line, col)
            let [termline, termcol] = [line, col]
            call cursor(line, col)
        else
            break
        endif
    endwhile

    call cursor(cursorline, cursorcol)
    return [0, termline, termcol, 0]
endfunction

" Position of start / end of current element: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in an element.
"
" An element is defined as:
"   * Current string if cursor is in a string
"   * Current comment if cursor is in a comment
"   * Current form if and only if cursor is on a paired bracket
"   * Current atom otherwise
function! s:current_element_terminal(end)
    let [_b, line, col, _o] = getpos('.')
    let char = getline(line)[col - 1]

    if s:syntax_match(s:string_scope, line, col)
        return s:current_string_terminal(a:end)
    elseif s:is_comment(line, col)
        return s:current_comment_terminal(a:end)
    elseif char =~ s:bracket
        if (a:end && char =~ s:closing_bracket)
            \ || (!a:end && char =~ s:opening_bracket)
            return [0, line, col, 0]
        else
            return s:nearest_bracket(a:end)
        end
    else
        return s:current_atom_terminal(a:end)
    endif
endfunction

" Returns position of previous / next element head. Returns current position
" if no such element exists. There is one exception: if next is 1 and the next
" element is the parent form, the form's closing bracket position is returned
" instead.
function! s:nearest_element_head(next)
    let cursor = getpos('.')
    let pos = cursor

    " This is a goto disguised as a foreach loop.
    for _ in [0]
        let terminal = s:current_element_terminal(a:next)
        if terminal[1] > 0 && pos != terminal
            let pos = terminal
            call setpos('.', pos)
            " b command moves to head of the current word if not on the head
            if !a:next | break | endif
        endif

        let [l, c] = s:findpos('\v\S', a:next)
        let adjacent = [0, l, c, 0]
        " We are at the beginning or end of file
        if adjacent[1] < 1 || pos == adjacent
            break
        else
            let pos = adjacent
        endif

        " We are at a head if moving forward
        if a:next
            break
        " Or at a tail if moving backward
        else
            call setpos('.', pos)
            let final = s:current_element_terminal(0)
            if final[1] > 0
                let pos = final
            endif
        endif
    endfor

    call setpos('.', cursor)
    return pos
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

    call setpos('.', a:pos)
    let [_b, termline, termcol, _o] = getpos('.')

    while 1
        " Include empty lines
        let [line, col] = s:findpos('\v\_.', a:trailing)

        if line < 1 | break | endif

        let char = getline(line)[col - 1]
        if empty(char) || char =~ '\v\s'
            let [termline, termcol] = [line, col]
            call cursor(line, col)
        else
            break
        endif
    endwhile

    call setpos('.', cursor)
    return [0, termline, termcol, 0]
endfunction

" Given start and end positions, returns new positions [start', end']:
"   * If trailing whitespace after end, end' is set to include the trailing
"     whitespace up to the next element, unless start is preceded on its line
"     by something other than whitespace, in which case end' is set to include
"     only the trailing whitespace to the end of line.
"   * If no trailing whitespace after end, start' is set to include leading
"     whitespace up to the the previous element
"   * Otherwise start and end are returned verbatim
"
" This behavior diverges from the behavior of native text object aw in that it
" allows multiline whitespace selections. Also unlike aw, we do not include
" the next element if currently in whitespace, because this is somewhat
" confusing.
function! s:terminals_with_whitespace(start, end)
    let [start, end] = [a:start, a:end]
    let ws_end = s:adjacent_whitespace_terminal(end, 1)

    " There is trailing whitespace
    if end != ws_end
        " Trailing WS is on the same line as end, so accept it
        if end[1] == ws_end[1]
            let end = ws_end
        " Start begins its line, so include all of ws_end
        elseif getline(start[1])[: start[2]][: -3] =~ '\v^\s*$'
            let end = ws_end
        " Include any trailing whitespace to eol
        elseif getline(end[1])[end[2]] =~ '\v\s'
            let end = s:pos_with_col_offset(end, col([end[1], '$']) - 1 - end[2])
        " No trailing whitespace on end's line, use leading whitespace
        else
            let start = s:adjacent_whitespace_terminal(start, 0)
        endif
    " Otherwise include leading whitespace
    else
        let start = s:adjacent_whitespace_terminal(start, 0)
    endif

    return [start, end]
endfunction

" Returns dict { 'bra': number, 'ket': number }, which indicates the number
" of unpaired opening brackets ('bra') and the number of unpaired closing
" brackets ('ket') in the selection from start to end.
function! s:bracket_count(start, end, all_brackets, opening_brackets)
    let cursor = getpos('.')
    let bcount = { 'bra': 0, 'ket': 0 }

    call setpos('.', a:start)
    while 1
        let [line, col] = searchpos(a:all_brackets, 'cW')
        if s:is_ignored_scope(line, col) | break | endif
        let cmp = s:compare_pos([0, line, col, 0], a:end)
        if cmp > 0 | break | endif

        if getline(line)[col - 1] =~ a:opening_brackets
            let bcount['bra'] += 1
        else
            if bcount['bra'] > 0
                let bcount['bra'] -= 1
            else
                let bcount['ket'] += 1
            endif
        endif

        if cmp == 0 | break | endif

        if col([line, '$']) - 1 == col
            call cursor(line + 1, 1)
        else
            call cursor(line, col + 1)
        endif
    endwhile

    call setpos('.', cursor)
    return bcount
endfunction

""" PREDICATES AND COMPARATORS {{{1

" See discussion at s:findpos()
function! s:is_backward_multibyte_search_broken()
    if exists('s:backward_multibyte_search_is_broken')
        return s:backward_multibyte_search_is_broken
    else
        let cursor = getpos('.')
        silent! call append(cursor[1], '123❤sexp-bugcheck')
        call cursor(cursor[1] + 1, 4)
        let s:backward_multibyte_search_is_broken = searchpos('\v.', 'b')[1] != 3
        " FIXME: Remove this undo leaf!
        silent! normal! u
        call setpos('.', cursor)
        return s:backward_multibyte_search_is_broken
    endif
endfunction

" It is established Vim convention that matching '\cstring|comment' and so on
" is acceptable for syntax regions that are conventionally named.
function! s:is_ignored_scope(line, col)
    return s:syntax_match(s:ignored_scope, a:line, a:col)
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
        if getline(a:line)[a:col - 1] =~ '\v\s'
            let cursor = getpos('.')
            call cursor(a:line, a:col)
            let [pline, pcol] = s:findpos('\v\S', 0, a:line - 1)
            let [cline, ccol] = s:findpos('\v\S', 1, a:line)
            if s:syntax_match('comment', pline, pcol)
                \ && s:syntax_match('comment', cline, ccol)
                let incomment = 1
            endif
            call setpos('.', cursor)
        endif

        return incomment
    endif
endfunction

" Returns 1 if character at position is an atom.
"
" An atom is defined as:
"   * A contiguous region of non-whitespace, non-bracket characters that are
"     not part of a string or comment.
function! s:is_atom(line, col)
    let char = getline(a:line)[a:col - 1]

    if empty(char) || char =~ s:delimiter
        return 0
    else
        return !s:syntax_match(s:string_scope . '|comment', a:line, a:col)
    endif
endfunction

" Returns -1 if position a is before position b, 1 if position a is after
" position b, and 0 if they are the same position. Only compares the line and
" column and ignores the first and last arguments.
function! s:compare_pos(a, b)
    let [a, b] = [a:a, a:b]

    if a[1] == b[1] && a[2] == b[2]
        return 0
    elseif a[1] != b[1]
        return a[1] < b[1] ? -1 : 1
    else
        return a[2] < b[2] ? -1 : 1
    endif
endfunction

""" CURSOR MOVEMENT {{{1

" Tries to move cursor to nearest paired bracket, returning its position
function! s:move_to_nearest_bracket(closing)
    let pos = s:nearest_bracket(a:closing)
    if pos[1] > 0 | call setpos('.', pos) | endif
    return pos
endfunction

" Tries to move cursor to outermost form's opening or closing bracket,
" returning its position; 0 for opening, 1 for closing. Does not move cursor
" if not in a form.
function! s:move_to_top_bracket(closing)
    let pos = s:current_top_form_bracket(a:closing)
    if pos[1] > 0 | call setpos('.', pos) | endif
    return pos
endfunction

""" VISUAL MARKS {{{1

" Set start and end visual marks to [0, 0, 0, 0]
function! s:clear_visual_marks()
    call setpos("'<", [0, 0, 0, 0])
    call setpos("'>", [0, 0, 0, 0])
endfunction

" Set visual marks '< and '> to the positions of the nearest paired brackets.
" Offset is the number of columns inwards from the brackets to set the marks.
"
" Under the following circumstances the visual marks are set to the next outer
" pair of brackets:
"
"   * Mode equals 'v', the cursor is on an opening bracket, the mark '< is
"     valid, and the marks '< and '> are not equal. This occurs when calling
"     this function while already having a form selected in visual mode.
"
"   * s:countindex is greater than 0 and the mark '< is valid. Occurs when
"     called by sexp#docount()
"
" Will set both to [0, 0, 0, 0] if none are found and mode does not equal 'v'.
function! s:set_marks_around_current_form(mode, offset)
    " We may potentially move the cursor.
    let cursor = getpos('.')
    let cursor_moved = 0

    " Prepare the entrails
    let start = getpos("'<")
    let visual = a:mode ==? 'v'
    let counting = s:countindex > 0
    let start_is_valid = start[1] > 0
    let have_selection = start_is_valid && start != getpos("'>")
    let expanding = counting || (visual && have_selection)

    " When evaluating via sexp#docount the cursor position will not be updated
    " to '<, so we do it now to simplify the following.
    if counting && start_is_valid
        if mode() ==? 'v' | execute "normal! \<Esc>" | endif
        call setpos('.', start)
        let cursor = start
        let cursor_moved = 1
    endif

    " Native text objects expand when repeating inner motions too
    if expanding
        \ && a:offset == 1
        \ && getline(cursor[1])[cursor[2] - 2] =~ s:opening_bracket
        normal! h
        let cursor = getpos('.')
        let cursor_moved = 1
    endif

    let ignored = s:is_ignored_scope(cursor[1], cursor[2])
    let char = getline(cursor[1])[cursor[2] - 1]

    if !ignored && char =~ s:opening_bracket
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
    elseif !ignored && char =~ s:closing_bracket
        let open = s:pos_with_col_offset(s:nearest_bracket(0), a:offset)
        let close = s:pos_with_col_offset(getpos('.'), -a:offset)
    else
        let open = s:pos_with_col_offset(s:nearest_bracket(0), a:offset)
        let close = s:pos_with_col_offset(s:nearest_bracket(1), -a:offset)
    endif

    if open[1] > 0 && close[1] > 0
        call setpos("'<", open)
        call setpos("'>", close)
    " Don't erase marks when in visual mode
    elseif !visual
        call s:clear_visual_marks()
    endif

    if cursor_moved | call setpos('.', cursor) | endif
endfunction

" Set visual marks '< and '> to the positions of the outermost paired brackets
" from the current location. Will set both to [0, 0, 0, 0] if none are found
" and mode does not equal 'v'.
function! s:set_marks_around_current_top_form(mode, offset)
    let closing = s:current_top_form_bracket(1)

    if closing[1] > 0
        " Calling searchpairpos() is faster when you start from an end
        let cursor = getpos('.')
        call setpos('.', closing)
        let opening = s:nearest_bracket(0)
        call setpos('.', cursor)

        call setpos("'<", s:pos_with_col_offset(opening, a:offset))
        call setpos("'>", s:pos_with_col_offset(closing, -a:offset))
    elseif a:mode !=? 'v'
        call s:clear_visual_marks()
    endif
endfunction

" Set visual marks '< and '> to the start and end of the current string. Will
" set both to [0, 0, 0, 0] if not currently in a string and mode does not
" equal 'v'.
function! s:set_marks_around_current_string(mode, offset)
    let end = s:current_string_terminal(1)
    if end[1] > 0
        call setpos("'<", s:pos_with_col_offset(s:current_string_terminal(0), a:offset))
        call setpos("'>", s:pos_with_col_offset(end, -a:offset))
    elseif a:mode !=? 'v'
        call s:clear_visual_marks()
    endif
endfunction

" Set visual marks '< and '> to the start and end of the current comment.
" If inner is 0, trailing or leading whitespace is included by way of
" s:terminals_with_whitespace().
"
" Will set both to [0, 0, 0, 0] if not currently in a comment and mode does
" not equal 'v'.
function! s:set_marks_around_current_comment(mode, inner)
    let start = [0, 0, 0, 0]
    let end = s:current_comment_terminal(1)

    if end[1] > 0
        let start = s:current_comment_terminal(0)
    else
        if a:mode !=? 'v'
            call s:clear_visual_marks()
        endif
        return
    endif

    if !a:inner
        let [start, end] = s:terminals_with_whitespace(start, end)
    endif

    call setpos("'<", start)
    call setpos("'>", end)
endfunction

" Set visual marks '< and '> to the start and end of the current atom.
" If inner is 0, trailing or leading whitespace is included by way of
" s:terminals_with_whitespace().
"
" Will set both to [0, 0, 0, 0] if not currently in an atom and mode does
" not equal 'v'.
function! s:set_marks_around_current_atom(mode, inner)
    let start = [0, 0, 0, 0]
    let end = s:current_atom_terminal(1)

    if end[1] > 0
        let start = s:current_atom_terminal(0)
    else
        if a:mode !=? 'v'
            call s:clear_visual_marks()
        endif
        return
    endif

    if !a:inner
        let [start, end] = s:terminals_with_whitespace(start, end)
    endif

    call setpos("'<", start)
    call setpos("'>", end)
endfunction

" Set visual marks '< and '> to the start and end of the current element.
" If inner is 0, trailing or leading whitespace is included by way
" of s:terminals_with_whitespace().
"
" If cursor is on whitespace that is not in a string or comment, the marks are
" set around the next element.
"
" Will set both to [0, 0, 0, 0] if an element could not be found and mode does
" not equal 'v'.
function! s:set_marks_around_current_element(mode, inner)
    let start = [0, 0, 0, 0]
    let end = s:current_element_terminal(1)

    if end[1] > 0
        let start = s:current_element_terminal(0)
    else
        " We are on whitespace; move to next element and recurse.
        let cursor = getpos('.')
        let next = sexp#move_to_adjacent_element('n', 1, 0)

        " No next element! We are at the eof or in a blank buffer.
        if next == cursor
            if a:mode !=? 'v'
                call s:clear_visual_marks()
            endif
        else
            call s:set_marks_around_current_element(a:mode, a:inner)
            call setpos('.', cursor)
        endif

        return
    endif

    if !a:inner
        let [start, end] = s:terminals_with_whitespace(start, end)
    endif

    call setpos("'<", start)
    call setpos("'>", end)
endfunction

" Set visual marks '< and '> to the start and end of the adjacent inner
" element. If no element is adjacent in the direction specified, the marks are
" set around the current element instead via
" s:set_marks_around_adjacent_element().
function! s:set_marks_around_adjacent_element(mode, next)
    let cursor = getpos('.')

    if a:mode ==? 'v'
        execute "normal! \<C-Bslash>\<C-n>"
    endif

    " If moving backward, first position ourselves at the head of the current
    " element.
    if !a:next
        let head = s:current_element_terminal(0)
        if head[1] > 0 | call setpos('.', head) | endif
    endif

    call sexp#move_to_adjacent_element('n', a:next, 0)
    call s:set_marks_around_current_element(a:mode, 1)
    call setpos('.', cursor)
endfunction

" Enter visual mode with current visual marks, unless '< is invalid and
" mode equals 'o'
function! s:select_current_marks(mode)
    if getpos("'<")[1] > 0
        normal! gv
        return 1
    elseif a:mode !=? 'o'
        normal! v
        return 1
    else
        return 0
    endif
endfunction

""" CHARACTER INSERTION {{{1

" Insert bra and ket around current visual marks. If mark '< is invalid,
" inserts brackets at cursor.
"
" Parameter at_tail sets cursor at head or tail (0 or 1), and parameter
" headspace determines whether to insert a space after the opening bracket
" when placing cursor at the head.
function! s:insert_brackets_around_visual_marks(bra, ket, at_tail, headspace)
    let start = getpos("'<")
    let end = getpos("'>")

    " No form, just insert brackets
    if start[1] < 1
        execute 'normal! i' . a:bra . a:ket
    elseif a:at_tail
        call setpos('.', start)
        execute 'normal! i' . a:bra
        " Did we just insert a character on the same line?
        let end = start[1] == end[1]
                  \ ? s:pos_with_col_offset(end, len(a:bra))
                  \ : end
        call setpos('.', end)
        execute 'normal! a' . a:ket
    else
        call setpos('.', end)
        execute 'normal! a' . a:ket
        call setpos('.', start)
        execute 'normal! i' . a:bra . (a:headspace ? ' ' : '')
    endif
endfunction

function! s:insert_brackets_around_current_form(bra, ket, at_tail, headspace)
    " Clear marks to ensure brackets are not placed around old marks.
    call s:clear_visual_marks()
    call s:set_marks_around_current_form('n', 0)
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

""" EXPORTED FUNCTIONS {{{1

" Evaluate expr count times. Will evaluate expr at least once. Stores current
" evaluation iteration (from 0 to count, exclusive) in s:countindex.
function! sexp#docount(expr, count)
    try
        for n in range(a:count > 0 ? a:count : 1)
            let s:countindex = n
            call eval(a:expr)
        endfor
    finally
        let s:countindex = 0
    endtry
endfunction

" Set visual marks at current form's brackets, then enter visual mode with
" that selection. If no brackets are found and mode equals 'o', nothing is
" done.
function! sexp#select_current_form(mode, offset)
    call s:set_marks_around_current_form(a:mode, a:offset)
    return s:select_current_marks(a:mode)
endfunction

" Set visual marks at current outermost form's brackets, then enter visual
" mode with that selection. If no brackets are found and mode equals 'o',
" nothing is done.
function! sexp#select_current_top_form(mode, offset)
    call s:set_marks_around_current_top_form(a:mode, a:offset)
    return s:select_current_marks(a:mode)
endfunction

" Unlike the native text object a" we do not try to select all the whitespace
" up to the next element. This can be done with sexp#select_current_element if
" desired. If not currently in string and mode equals 'o', nothing is done.
function! sexp#select_current_string(mode, offset)
    call s:set_marks_around_current_string(a:mode, a:offset)
    return s:select_current_marks(a:mode)
endfunction

" Set visual marks around current comment and enter visual mode. If not
" currently in a comment and mode equals 'o', nothing is done.
function! sexp#select_current_comment(mode, inner)
    call s:set_marks_around_current_comment(a:mode, a:inner)
    return s:select_current_marks(a:mode)
endfunction

" Set visual marks around current atom and enter visual mode. If not currently
" in an atom and mode equals 'o', nothing is done.
function! sexp#select_current_atom(mode, inner)
    call s:set_marks_around_current_atom(a:mode, a:inner)
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

" Moves cursor to adjacent element, returning its position; 0 for previous, 1
" for next. If no such adjacent element exists, moves to beginning or end of
" element respectively. Analogous to native w and b commands.
function! sexp#move_to_adjacent_element(mode, next, top)
    let cursor = getpos('.')

    if a:mode ==? 'v'
        " Break out of visual mode, preserving cursor position
        if s:countindex > 0
            execute "normal! \<C-Bslash>\<C-n>"
        endif

        " Record visual state now before moving the cursor
        let start = getpos("'<")
        let end = getpos("'>")
        let omode = cursor == start
    endif

    if a:top
        let top = s:move_to_top_bracket(a:next)

        " Stop at current top element head if moving backward and did not
        " start on a top element head.
        if !a:next && top[1] > 0 && top != cursor
            let pos = top
        else
            let pos = s:nearest_element_head(a:next)
        endif
    else
        let pos = s:nearest_element_head(a:next)
    endif

    if a:mode ==? 'v'
        if omode
            call setpos("'<", pos)
            call setpos("'>", end)
            normal! gvo
        else
            call setpos("'<", start)
            call setpos("'>", pos)
            normal! gv
        endif
    else
        call setpos('.', pos)
    endif

    return pos
endfunction

" Place brackets around scope, then place cursor at head or tail, finally
" leaving off in insert mode if specified. Insert also sets the headspace
" parameter when inserting brackets.
function! sexp#wrap(scope, bra, ket, at_tail, insert)
    let original_start = getpos("'<")
    let original_end = getpos("'>")

    if a:scope ==# 'f'
        call s:insert_brackets_around_current_form(a:bra, a:ket, a:at_tail, a:insert)
    elseif a:scope ==# 'e'
        call s:insert_brackets_around_current_element(a:bra, a:ket, a:at_tail, a:insert)
    elseif a:scope ==# 'v'
        call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:insert)
    endif

    call setpos("'<", original_start)
    call setpos("'>", original_end)
    if a:insert | startinsert | endif
endfunction

" Remove brackets from current form, placing cursor at position of deleted
" first bracket.
function! sexp#splice_form()
    let original_start = getpos("'<")
    let original_end = getpos("'>")
    let cursor = getpos('.')

    " Ensure we are not deleting chars at old marks
    call s:clear_visual_marks()
    call s:set_marks_around_current_form('n', 0)

    let start = getpos("'<")

    if start[1] > 0
        " Delete ending bracket first so we don't mess up '<
        call setpos('.', getpos("'>"))
        normal! dl
        call setpos('.', start)
        normal! dl
    else
        call setpos('.' cursor)
    endif

    call setpos("'<", original_start)
    call setpos("'>", original_end)
endfunction

" Move cursor to current form start or end and enter insert mode. Inserts
" a leading space after opening bracket if inserting at head, unless there
" already is one.
function! sexp#insert_at_form_terminal(end)
    let cursor = getpos('.')
    let char = getline(cursor[1])[cursor[2] - 1]
    let on_bracket = (a:end && char =~ s:closing_bracket)
                     \ || (!a:end && char =~ s:opening_bracket)

    if on_bracket && !s:is_ignored_scope(cursor[1], cursor[2])
        let pos = cursor
    else
        let pos = s:move_to_nearest_bracket(a:end)
    endif

    " Handle opening bracket edge cases
    if !a:end && pos[1] > 0
        let nextchar = getline(pos[1])[pos[2]]

        " This is the eol, so start insert at eol
        if empty(nextchar)
            startinsert!
            return
        " Add headspace unless there's already some there
        elseif nextchar !~ '\v\s'
            execute 'normal! a '
        " Else start after the bracket
        else
            normal! l
        endif
    endif

    startinsert
endfunction

" Return keys to be inserted in place of bra; this includes the closing pair,
" as well as a leading and/or trailing space to separate from other elements.
"
" Returns bra if s:is_ignored_scope() is true at the cursor.
function! sexp#opening_insertion(bra)
    let [_b, line, col, _o] = getpos('.')

    if s:is_ignored_scope(line, col)
        return a:bra
    endif

    let ket = s:pairs[a:bra]
    let l = getline(line)
    let cur = l[col - 1]
    let prev = l[col - 2]
    let pprev = l[col - 3]
    let [dispatch, macro] = get(s:macro_characters, &filetype, ['', ''])

    let buf = ''
    let buftail = ''

    if prev =~ '\v\S'
        \ && prev !~ s:opening_bracket
        \ && (pprev !=# dispatch && prev !~ macro)
        let buf .= ' '
    endif

    let buf .= a:bra . ket
    let buftail .= "\<Left>"

    if cur =~ '\v\S' && cur !~ s:closing_bracket
        let buf .= ' '
        let buftail .= "\<Left>"
    endif

    return buf . buftail
endfunction

" Return keys to be inserted in place of ket:
"
"   * Insert ket if s:is_ignored_scope is true at the cursor
"   * Skip current char if equal to ket
"   * Jump to next closing ket if current form is balanced
"   * Insert ket if current form is unbalanced
function! sexp#closing_insertion(ket)
    let [_b, line, col, _o] = getpos('.')
    let char = getline(line)[col - 1]

    if s:is_ignored_scope(line, col)
        return a:ket
    elseif char == a:ket
        return "\<Right>"
    endif

    let bra = '\V' . s:pairs[a:ket]
    let ket = '\V' . a:ket
    let open = char =~ s:opening_bracket
               \ ? [0, line, col, 0]
               \ : s:nearest_bracket(0, bra, ket)

    " No enclosing form; insert nothing
    if open[1] < 1 | return '' | endif

    let close = s:nearest_bracket(1, bra, ket)
    if close[1] > 0
        " Brackets are balanced, jump to closing bracket
        return "\<C-o>:\<C-u>call cursor(" . close[1] . ", " . close[2] . ")\<CR>"
    else
        " Brackets are short closing brackets, insert bracket
        return a:ket
    endif
endfunction

" Return keys to be inserted in place of quote:
"
"   * Insert quote if s:is_ignored_scope is true at the cursor
"   * If in a string, insert quote unless current char is a quote
"   * If in a string, always insert quote if previous char is a backslash
"   * Insert pair of quotes otherwise
function! sexp#quote_insertion(quote)
    let [_b, line, col, _o] = getpos('.')

    if s:syntax_match(s:string_scope, line, col)
        let l = getline(line)
        " User is trying to insert an escaped quote, so do it
        if l[col - 2] == '\'
            return a:quote
        else
            return l[col - 1] == a:quote ? "\<Right>" : a:quote
        endif
    elseif s:is_ignored_scope(line, col)
        return a:quote
    else
        return a:quote . a:quote . "\<Left>"
    endif
endfunction

" Return keys to be inserted when deleting backwards:
"
"   * Delete adjacent double quotes when previous position is in a string,
"     unless the first quote is preceded by another quote or a backslash
"   * Delete adjacent paired brackets, unless s:is_ignored_scope is true at
"     the cursor
"   * Normal backspace otherwise
function! sexp#backspace_insertion()
    let [_b, line, col, _o] = getpos('.')
    let l = getline(line)
    let cur = l[col - 1]
    let prev = l[col - 2]

    if prev == '"' && cur == '"'
        \ && s:syntax_match(s:string_scope, line, col)
        \ && l[col - 3] !~ '\v[\"]'
        return "\<BS>\<Del>"
    elseif !s:is_ignored_scope(line, col)
        \ && prev =~ s:opening_bracket
        \ && cur ==# s:pairs[prev]
        return "\<BS>\<Del>"
    else
        return "\<BS>"
    endif
endfunction

" Exchange the current element with an adjacent sibling element. Does nothing
" if there is no current or sibling element.
"
" If form equals 1, the current form is treated as the current element.
"
" If mode equals 'v', the current selection is expanded to include any
" partially selected elements, then is swapped with the next element as a
" unit. The marks are set to the new position and visual mode is re-entered.
"
" If mode equals 'v' and form equals 1, then (for implementation simplicity)
" the form at the cursor position at time call is used as the selection.
"
" Note that swapping comments with other elements can lead to structural
" imbalance since trailing brackets may be included as part of a comment after
" a swap. Fixing this is on the TODO list.
"
" This implementation is conservative and verbose because I found that the
" syntax state of the buffer is not updated while doing quick successions of
" normal! commands, which is the obvious and concise implementation method.
function! sexp#swap_element(mode, next, form)
    let reg_a = @a
    let reg_b = @b
    let visual = a:mode ==? 'v'
    let cursor = getpos('.')

    if visual
        let vmarks = [getpos("'<"), getpos("'>")]
    endif

    " Extend both ends of visual selection to nearest element. If there exist
    " any unpaired brackets in the resulting selection, the selection is
    " extended to include those forms.
    "
    " Moving formwise with a:mode 'v' will be treated like a regular formwise
    " swap from the cursor position.
    if visual && !a:form
        call setpos('.', vmarks[0])
        if getline(vmarks[0][1])[vmarks[0][2] - 1] =~ '\v\s'
            call sexp#move_to_adjacent_element('n', 1, 0)
        endif
        let head = s:current_element_terminal(0)
        if head[1] > 0 | call setpos("'<", head) | endif

        call setpos('.', vmarks[1])
        if getline(vmarks[1][1])[vmarks[1][2] - 1] =~ '\v\s'
            call sexp#move_to_adjacent_element('n', 1, 0)
        endif
        let tail = s:current_element_terminal(1)
        if tail[1] > 0 | call setpos("'>", tail) | endif

        if head[1] > 0 && tail[1] > 0
            " Find any unbalanced brackets in our selection
            let bcount = s:bracket_count(head, tail, s:bracket, s:opening_bracket)

            " Expand head for every ket and tail for every bra.
            if bcount['ket'] > 0
                call setpos('.', head)
                call sexp#docount('s:move_to_nearest_bracket(0)', bcount['ket'])
                call setpos("'<", getpos('.'))
            endif

            if bcount['bra'] > 0
                call setpos('.', tail)
                call sexp#docount('s:move_to_nearest_bracket(1)', bcount['bra'])
                call setpos("'>", getpos('.'))
            endif
        endif

        let selected = s:select_current_marks('v')
    " Otherwise select the current form or element
    elseif a:form
        let selected = sexp#select_current_form('o', 0)
    else
        let selected = sexp#select_current_element('o', 1)
    endif

    " Abort if nothing selected
    if !selected
        if visual
            " Restore visual state
            call setpos("'<", vmarks[0])
            call setpos("'>", vmarks[1])
            normal! gv
        endif
        return
    endif

    " Yank selection and mark with START OF TEXT and END OF TEXT if necessary
    normal! "ay
    if a:next | let @a = nr2char(0x02) . @a . nr2char(0x03) | endif

    let marks = {}
    let marks['a'] = { 'start': getpos("'<"), 'end': getpos("'>")}

    " Record the sibling element
    call setpos('.', marks['a'][a:next ? 'end' : 'start'])
    call sexp#select_adjacent_element('n', a:next)
    normal! "by
    let marks['b'] = { 'start': getpos("'<"), 'end': getpos("'>")}

    " Abort if we are already at the head or tail of the current form; we can
    " determine this by seeing if the adjacent element envelops the original
    " element. Also abort if the selections are the same, which indicates that
    " we are at the top or bottom of the file.
    let b_cmp_a = s:compare_pos(marks['b']['start'], marks['a']['start'])
    if b_cmp_a == 0
        \ || (a:next && b_cmp_a < 0)
        \ || (!a:next && s:compare_pos(marks['b']['end'], marks['a']['end']) > 0)
        if visual
            " Restore visual state
            call setpos("'<", vmarks[0])
            call setpos("'>", vmarks[1])
            normal! gv
        endif
        call setpos('.', cursor)
        return
    endif

    " We change the buffer from the bottom up so that the marks remain
    " accurate.
    let b = a:next ? 'b' : 'a'
    let a = a:next ? 'a' : 'b'

    call setpos("'<", marks[b]['start'])
    call setpos("'>", marks[b]['end'  ])
    execute 'normal! gv"' . a . 'p'

    call setpos("'<", marks[a]['start'])
    call setpos("'>", marks[a]['end'  ])
    execute 'normal! gv"' . b . 'p'

    " Set marks around next element using the ^B and ^C markers
    if a:next
        call setpos('.', marks['a']['start'])

        let [sl, sc] = s:findpos(nr2char(0x02), 1)
        call setpos('.', [0, sl, sc, 0])
        normal! x
        call setpos("'<", [0, sl, sc, 0])

        let [el, ec] = s:findpos(nr2char(0x03), 1)
        call setpos('.', [0, el, ec, 0])
        normal! x
        call setpos("'>", [0, el, ec - 1, 0])
    endif

    if visual
        normal! gv
    elseif a:next
        call setpos('.', getpos("'<"))
    else
        call setpos('.', marks['b']['start'])
    endif

    let @a = reg_a
    let @b = reg_b
endfunction
