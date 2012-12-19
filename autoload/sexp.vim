
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

if exists('g:__sexp_autoloaded__')
    finish
endif
let g:__sexp_autoloaded__ = 1

" TODO:
"
" * Deliberately set jump marks so users can `` back after undo.

" Clojure's brackets; other Lisps have a subset, which shouldn't be an issue.
let s:bracket = '\v[\(\)\[\]\{\}]'
let s:opening_bracket = '\v[\(\[\{]'
let s:closing_bracket = '\v[\)\]\}]'
let s:pairs = [['\V(','\V)'], ['\V[','\V]'], ['\V{','\V}']]

" Does not return multibyte characters!
function! s:current_char()
    return getline('.')[col('.')-1]
endfunction

" Does not return multibyte characters!
function! s:previous_char()
    return getline('.')[col('.')-2]
endfunction

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
            let [pline, pcol] = searchpos('\v\S', 'nW')
            let [nline, ncol] = searchpos('\v\S', 'bnW')
            if s:syntax_name(pline, pcol) =~? 'string' && s:syntax_name(nline, ncol) =~? 'string'
                let instring = 1
            endif
        endif

        return instring
    endif
endfunction

" Position of nearest _paired_ bracket: 0 for opening, 1 for closing. Returns
" [0, 0, 0, 0] if none found.
function! s:nearest_bracket(closing)
    let cursor = getpos('.')
    let closest = []
    let flags = a:closing ? 'nW' : 'bnW'

    for [start, end] in s:pairs
        let [line, col] = searchpairpos(start, '', end, flags, 's:is_ignored_scope(line("."), col("."))')

        if line < 1
            continue
        elseif empty(closest)
            let closest = [0, line, col, 0]
        else
            let closest = s:min_by_distance_from(cursor, closest, [0, line, col, 0])
        endif
    endfor

    return empty(closest) ? [0, 0, 0, 0] : closest
endfunction

" Position of start / end of current string: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in a string.
"
" We can't rely on va" or on searchpairpos() because they don't work well
" on symmetric patterns. Also, we aren't searching for just double quotes
" because we'd like to work with non-Lisps.
"
" We also use search() while moving the cursor because using simple column
" arithmetic breaks on multibyte characters.
function! s:current_string_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')
    if !s:is_string(cursorline, cursorcol) | return [0, 0, 0, 0,] | endif

    let [termline, termcol] = [cursorline, cursorcol]
    let flags = a:end ? 'W' : 'bW'

    while 1
        " Test adjacent character. There is a bug in which searching backwards
        " from a multibyte character moves the cursor too far, so we have to
        " handle this separately.
        "
        " https://groups.google.com/forum/?fromgroups=#!topic/vim_dev/s7c_Qq3K1Io
        if a:end
            let [line, col] = searchpos('\v.', flags)
        else
            let [_b, line, col, _o] = getpos('.')
            " Backwards search from bol still works fine
            if col == 1
                let [line, col] = searchpos('\v.', flags)
            else
                let col -= 1
                call cursor(line, col)
            endif
        endif

        " Beginning or end of file.
        if line < 1 | break | endif

        if s:is_string(line, col)
            let [termline, termcol] = [line, col]
        else
            break
        endif
    endwhile

    call setpos('.', [0, cursorline, cursorcol, 0])
    return [0, termline, termcol, 0]
endfunction

" Tries to move cursor to nearest _paired_ bracket.
function! s:move_to_nearest_bracket(closing)
    let pos = s:nearest_bracket(a:closing)
    if pos[1] | call setpos('.', pos) | endif
endfunction

" Set visual marks '< and '> to the positions of the nearest paired brackets.
" Offset is the number of columns inwards from the brackets to set the marks.
" Will set both to [0, 0, 0, 0] if none are found.
"
" If cursor is on an opening bracket, the mark '< is valid, and the mark
" '< does not equal '>, the visual marks are set to the next outer pair of
" brackets.
function! s:set_marks_around_current_form(offset)
    " We may potentially move the cursor.
    let cursor = getpos('.')

    " If we already have some text selected, we assume that we are trying to
    " expand our selection.
    let visual_repeat = visualmode() =~# '\v^[vV]' && getpos("'<")[1] > 0 && getpos("'<") != getpos("'>")

    " Native text objects expand when repeating inner motions too
    if visual_repeat && s:previous_char() =~ s:opening_bracket
        normal! h
    endif

    let char = s:current_char()

    if char =~ s:opening_bracket
        if visual_repeat
            call s:move_to_nearest_bracket(1)
            call s:move_to_nearest_bracket(1) " Expansion step
            let open = s:pos_with_col_offset(s:nearest_bracket(0), a:offset)
            let close = s:pos_with_col_offset(getpos('.'), -a:offset)
        else
            let open = s:pos_with_col_offset(getpos('.'), a:offset)
            let close = s:pos_with_col_offset(s:nearest_bracket(1), -a:offset)
        endif
    elseif char =~ s:closing_bracket
        let open = s:pos_with_col_offset(s:nearest_bracket(0), a:offset)
        let close = s:pos_with_col_offset(getpos('.'), -a:offset)
    else
        let open = s:pos_with_col_offset(s:nearest_bracket(0), a:offset)
        let close = s:pos_with_col_offset(s:nearest_bracket(1), -a:offset)
    endif

    " Check closing position's line to determine success because opening
    " position is sometimes set erroneously!
    if close[1]
        call setpos("'<", open)
        call setpos("'>", close)
    else
        call setpos("'<", [0, 0, 0, 0])
        call setpos("'>", [0, 0, 0, 0])
    endif

    call setpos('.', cursor)
endfunction

" Set visual marks '< and '> to the start and end of the current string. Will
" set both to [0, 0, 0, 0] if not currently in a string.
function! s:set_marks_around_current_string(offset)
    let end = s:current_string_terminal(1)
    if end[1]
        call setpos("'<", s:pos_with_col_offset(s:current_string_terminal(0), a:offset))
        call setpos("'>", s:pos_with_col_offset(end, -a:offset))
    else
        call setpos("'<", [0, 0, 0, 0])
        call setpos("'>", [0, 0, 0, 0])
    endif
endfunction

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
    call s:set_marks_around_current_form(0)
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:headspace)
endfunction

function! s:insert_brackets_around_current_string(bra, ket, at_tail, headspace)
    call s:set_marks_around_current_string(0)
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:headspace)
endfunction

function! s:insert_brackets_around_current_word(bra, ket, at_tail, headspace)
    execute "normal! viw\<Esc>"
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_tail, a:headspace)
endfunction

function! sexp#select_current_form(offset)
    call s:set_marks_around_current_form(a:offset)
    execute 'normal! ' . (getpos("'<")[1] > 0 ? 'gv' : 'v')
endfunction

" Unlike the native va" we do not try to select all the whitespace up to the
" next element. We will do that when moving elements.
function! sexp#select_current_string(offset)
    call s:set_marks_around_current_string(a:offset)
    execute 'normal! ' . (getpos("'<")[1] > 0 ? 'gv' : 'v')
endfunction

" Place brackets around scope, then place cursor at head or tail, finally
" leaving off in insert mode if specified. Insert also sets the headspace
" parameter when inserting brackets.
function! sexp#wrap(scope, bra, ket, at_tail, insert)
    let original_start = getpos("'<")
    let original_end = getpos("'>")

    " Wrap form.
    if a:scope ==# 'f'
        call s:insert_brackets_around_current_form(a:bra, a:ket, a:at_tail, a:insert)
    " Wrap form if on bracket, string if in string, word otherwise.
    elseif a:scope ==# 'w'
        let [_b, line, col, _o] = getpos('.')
        if getline(line)[col-1] =~ s:bracket
            call s:insert_brackets_around_current_form(a:bra, a:ket, a:at_tail, a:insert)
        elseif s:is_string(line, col)
            call s:insert_brackets_around_current_string(a:bra, a:ket, a:at_tail, a:insert)
        else
            call s:insert_brackets_around_current_word(a:bra, a:ket, a:at_tail, a:insert)
        endif
    " Wrap current visual selection.
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
    call s:set_marks_around_current_form(0)

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
