
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
" * Set stopline for searchpairpos()
" * Next/prev element text object
" * Top level sexp text object
" * Comment text object
" * Don't ignore virtualedit mode
" * Check synstack() for syntax scope?

""" PATTERNS {{{1

let s:bracket = '\v\(|\)|\[|\]|\{|\}'
let s:opening_bracket = '\v\(|\[|\{'
let s:closing_bracket = '\v\)|\]|\}'
let s:delimiter = s:bracket . '|\s'
let s:pairs = [['\V(','\V)'], ['\V[','\V]'], ['\V{','\V}']]

""" QUERIES AT CURSOR {{{1

" Like searchpos(), return first pattern match from cursor as [line, col].
" Unlike searchpos(), searching backwards when the cursor is on a multibyte
" character does not move the cursor too far (but the position returned may
" be in the middle of a multibyte sequence).
"
" cf. https://groups.google.com/forum/?fromgroups=#!topic/vim_dev/s7c_Qq3K1Io
"
" One extra argument may be supplied: the stopline parameter of searchpos().
function! s:findpos(pattern, next, ...)
    if a:next
        let [line, col] = searchpos(a:pattern, 'nW', a:0 ? a:1 : 0)
    else
        let [_b, line, col, _o] = getpos('.')
        if col == 1
            " Backwards search from bol still works fine
            let [line, col] = searchpos(a:pattern, 'bnW', a:0 ? a:1 : 0)
        else
            " Note that this may not be the beginning of the character
            let col -= 1
        endif
    endif

    return [line, col]
endfunction

" Return single-byte character behind cursor on the same line.
function! s:previous_char()
    return getline('.')[col('.')-1]
endfunction

" Position of nearest _paired_ bracket: 0 for opening, 1 for closing. Returns
" [0, 0, 0, 0] if none found.
function! s:nearest_bracket(closing)
    let closest = []
    let flags = a:closing ? 'nW' : 'bnW'

    for [start, end] in s:pairs
        let [line, col] = searchpairpos(start, '', end, flags, 's:is_ignored_scope(line("."), col("."))')

        if line < 1
            continue
        elseif empty(closest)
            let closest = [0, line, col, 0]
        else
            let closest = s:min_by_distance_from(getpos('.'), closest, [0, line, col, 0])
        endif
    endfor

    return empty(closest) ? [0, 0, 0, 0] : closest
endfunction

" Position of start / end of current string: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in a string.
function! s:current_string_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')
    if !s:is_string(cursorline, cursorcol) | return [0, 0, 0, 0] | endif

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

        if s:is_string(line, col)
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
"   * Current form if and only if cursor is on a _paired_ bracket
"   * Current string if cursor is in a string
"   * TODO: Current comment if cursor is in a comment
"   * Current contiguous region of whitespace if cursor is on whitespace
"   * Current atom otherwise
function! s:current_element_terminal(end)
    let [_b, line, col, _o] = getpos('.')
    let char = getline(line)[col-1]

    if s:is_string(line, col)
        return s:current_string_terminal(a:end)
    " TODO: elseif s:is_comment()
    elseif char =~ s:bracket
        if (a:end && char =~ s:closing_bracket) || (!a:end && char =~ s:opening_bracket)
            return [0, line, col, 0]
        else
            return s:nearest_bracket(a:end)
        end
    elseif char =~ '\v\s'
        return s:adjacent_whitespace_terminal([0, line, col, 0], a:end)
    elseif !s:is_atom(line, col)
        return [0, 0, 0, 0]
    else
        let [cursorline, cursorcol, termline, termcol] = [line, col, line, col]

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
    endif
endfunction

""" QUERIES AT POSITION {{{1

function! s:pos_with_col_offset(pos, offset)
    let [b, l, c, o] = a:pos
    return [b, l, c + a:offset, o]
endfunction

function! s:min_by_distance_from(pos, a, b)
    " First return closest by line difference
    let line_delta_a = abs(a:pos[1] - a:a[1])
    let line_delta_b = abs(a:pos[1] - a:b[1])
    if line_delta_a > line_delta_b
        return a:b
    elseif line_delta_a < line_delta_b
        return a:a
    " They are on the same line as the cursor
    elseif line_delta_a == 0
        let col_delta_a = abs(a:pos[2] - a:a[2])
        let col_delta_b = abs(a:pos[2] - a:b[2])
        return col_delta_a > col_delta_b ? a:b : a:a
    " They are on the same line, but not on the same line as the cursor. If
    " below the cursor, proximity is closest to bol and vice versa.
    else
        let op = a:pos[1] - a:a[1] < 0 ? '<' : '>'
        execute 'let a_is_closer = ' . a:a[2] . op . a:b[2]
        return a_is_closer ? a:a : a:b
    endif
endfunction

function! s:syntax_name(line, col)
    return synIDattr(synID(a:line, a:col, 0), 'name')
endfunction

" Return start of leading (0) or end of trailing (1) whitespace from pos.
function! s:adjacent_whitespace_terminal(pos, trailing)
    let cursor = getpos('.')

    call setpos('.', a:pos)
    let [_b, termline, termcol, _o] = getpos('.')

    while 1
        let [line, col] = s:findpos('\v.', a:trailing)
        if line < 1 | break | endif
        if getline(line)[col-1] =~ '\v\s'
            let [termline, termcol] = [line, col]
            call cursor(line, col)
        else
            break
        endif
    endwhile

    call setpos('.', cursor)
    return [0, termline, termcol, 0]
endfunction

""" PREDICATES {{{1

" It is established Vim convention that matching '\cstring|comment' and so on
" is acceptable for syntax regions that are conventionally named.
function! s:is_ignored_scope(line, col)
    return s:syntax_name(a:line, a:col) =~? '\vstring|comment|char'
endfunction

" Returns 1 if character at position is a string; handles empty lines, which
" always return a synID of 0.
function! s:is_string(line, col)
    if s:syntax_name(a:line, a:col) =~? 'string'
        return 1
    else
        let instring = 0

        " We may be on an empty line; check nearest pair of nonspace chars
        if col('$') == 1
            let cursor = getpos('.')
            call cursor(a:line, a:col)
            let [pline, pcol] = s:findpos('\v\S', 0)
            let [nline, ncol] = s:findpos('\v\S', 1)
            if s:syntax_name(pline, pcol) =~? 'string' && s:syntax_name(nline, ncol) =~? 'string'
                let instring = 1
            endif
            call setpos('.', cursor)
        endif

        return instring
    endif
endfunction

" Returns 1 if character at position is an atom.
"
" An atom is defined as:
"   * A contiguous region of non-whitespace, non-bracket characters that are
"     not part of a string or comment.
function! s:is_atom(line, col)
    if getline(a:line)[a:col-1] =~ s:delimiter
        return 0
    else
        return s:syntax_name(a:line, a:col) !~? '\vstring|comment'
    endif
endfunction

""" CURSOR MOVEMENT {{{1

" Tries to move cursor to nearest _paired_ bracket, returning its position
function! s:move_to_nearest_bracket(closing)
    let pos = s:nearest_bracket(a:closing)
    if pos[1] > 0 | call setpos('.', pos) | endif
    return pos
endfunction

""" VISUAL MARKS {{{1

" Set visual marks '< and '> to the positions of the nearest paired brackets.
" Offset is the number of columns inwards from the brackets to set the marks.
"
" If mode equals 'v', the cursor is on an opening bracket, the mark '< is
" valid, and the mark '< does not equal '>, the visual marks are set to the
" next outer pair of brackets.
"
" Will set both to [0, 0, 0, 0] if none are found and mode does not equal 'v'.
function! s:set_marks_around_current_form(mode, offset)
    " We may potentially move the cursor.
    let cursor = getpos('.')
    let cursor_moved = 0

    " If we already have some text selected, we assume that we are trying to
    " expand our selection.
    let visual = a:mode ==? 'v'
    let visual_repeat = visual && getpos("'<")[1] > 0 && getpos("'<") != getpos("'>")

    " Native text objects expand when repeating inner motions too
    if visual_repeat && a:offset == 1 && s:previous_char() =~ s:opening_bracket
        normal! h
    endif

    let ignored = s:is_ignored_scope(cursor[1], cursor[2])
    let char = getline(cursor[1])[cursor[2]-1]

    if !ignored && char =~ s:opening_bracket
        if visual_repeat
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
        call setpos("'<", [0, 0, 0, 0])
        call setpos("'>", [0, 0, 0, 0])
    endif

    if cursor_moved | call setpos('.', cursor) | endif
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
        call setpos("'<", [0, 0, 0, 0])
        call setpos("'>", [0, 0, 0, 0])
    endif
endfunction

" Set visual marks '< and '> to the start and end of the current element.
" If offset is greater than 0, the end includes whitespace up to the next
" element, or whitespace up to the previous element if no trailing whitespace
" on the same line is present.
"
" If the current element is whitespace, the visual marks are placed around the
" whitespace and also to the end of the next element if with_whitespace is 1.
"
" Will set both to [0, 0, 0, 0] if not currently in an element and mode does
" not equal 'v'.
function! s:set_marks_around_current_element(mode, with_whitespace)
    let start = [0, 0, 0, 0]
    let end = s:current_element_terminal(1)

    if end[1] > 0
        let start = s:current_element_terminal(0)
    else
        if a:mode !=? 'v'
            call setpos("'<", [0, 0, 0, 0])
            call setpos("'>", [0, 0, 0, 0])
        endif
        return
    endif

    if a:with_whitespace
        if getline(start[1])[start[2]-1] =~ '\v\s'
            let cursor = getpos('.')
            let [l, c] = s:findpos('\v\S', 1)
            call cursor(l, c)
            let end = s:current_element_terminal(1)
            call setpos('.', cursor)
        else
            let wend = s:adjacent_whitespace_terminal(end, 1)
            if end != wend && end[1] == wend[1]
                let end = wend
            else
                let wstart = s:adjacent_whitespace_terminal(start, 0)
                if start != wstart
                    let start = wstart
                endif
            endif
        endif
    endif

    call setpos("'<", start)
    call setpos("'>", end)
endfunction

" Enter visual mode with current visual marks, unless '< is invalid and
" mode equals 'o'
function! s:select_current_marks(mode)
    if getpos("'<")[1] > 0
        normal! gv
    elseif a:mode !=? 'o'
        normal! v
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
        let end = start[1] == end[1] ? s:pos_with_col_offset(end, len(a:bra)) : end
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
    " Clear visual start mark to signal that we are not trying to expand the
    " selection.
    call setpos("'<", [0, 0, 0, 0])
    call s:set_marks_around_current_form('n', 0)
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:headspace)
endfunction

function! s:insert_brackets_around_current_string(bra, ket, at_tail, headspace)
    call s:set_marks_around_current_string('n', 0)
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:headspace)
endfunction

function! s:insert_brackets_around_current_element(bra, ket, at_tail, headspace)
    call s:set_marks_around_current_element('n', 0)
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:headspace)
endfunction

""" EXPORTED FUNCTIONS {{{1

" Sets visual marks at current form's brackets, then enters visual mode with
" that selection. If no brackets are found and mode equals 'o', nothing is
" done.
function! sexp#select_current_form(mode, offset)
    call s:set_marks_around_current_form(a:mode, a:offset)
    call s:select_current_marks(a:mode)
endfunction

" Unlike the native text object a" we do not try to select all the whitespace
" up to the next element. If not currently in string and mode equals 'o',
" nothing is done.
function! sexp#select_current_string(mode, offset)
    call s:set_marks_around_current_string(a:mode, a:offset)
    call s:select_current_marks(a:mode)
endfunction

" Imitates native text objects aw and iw, but for elements. If not
" currently in an element and mode equals 'o', nothing is done.
function! sexp#select_current_element(mode, with_whitespace)
    call s:set_marks_around_current_element(a:mode, a:with_whitespace)
    call s:select_current_marks(a:mode)
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

    call setpos("'<", [0, 0, 0, 0])
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
