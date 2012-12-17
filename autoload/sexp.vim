
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

" Clojure's brackets; other Lisps have a subset, which shouldn't be an issue.
let s:bracket = '\v[\(\)\[\]\{\}]'
let s:opening_bracket = '\v[\(\[\{]'
let s:closing_bracket = '\v[\)\]\}]'
let s:pairs = [['\V(','\V)'], ['\V[','\V]'], ['\V{','\V}']]

function! s:current_char()
    return getline('.')[col('.')-1]
endfunction

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

" Position of nearest bracket: 0 for opening, 1 for closing.
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

" Tries to move cursor to nearest bracket; same parameters as s:nearest_bracket
function! s:move_to_nearest_bracket(closing)
    let pos = s:nearest_bracket(a:closing)
    if pos[1] | call setpos('.', pos) | endif
endfunction

" Potentially moves the cursor!
function! sexp#set_marks_around_current_form(offset)
    " If we already have some text selected, we assume that we are trying to
    " expand our selection.
    let visual_repeat = visualmode() =~# '\v^[vV]' && getpos("'<")[1] > 0 && getpos("'<") != getpos("'>")

    " Native text objects expand when repeating inner motions too
    if visual_repeat && s:previous_char() =~ s:opening_bracket
        normal! h
    endif

    if s:current_char() =~ s:opening_bracket
        if visual_repeat
            call s:move_to_nearest_bracket(1)
            call s:move_to_nearest_bracket(1) " Expansion step
            call setpos("'<", s:pos_with_col_offset(s:nearest_bracket(0), a:offset))
            call setpos("'>", s:pos_with_col_offset(getpos('.'), -a:offset))
        else
            call setpos("'<", s:pos_with_col_offset(getpos('.'), a:offset))
            call setpos("'>", s:pos_with_col_offset(s:nearest_bracket(1), -a:offset))
        endif
    elseif s:current_char() =~ s:closing_bracket
        call setpos("'<", s:pos_with_col_offset(s:nearest_bracket(0), a:offset))
        call setpos("'>", s:pos_with_col_offset(getpos('.'), -a:offset))
    else
        let pos = s:nearest_bracket(1)
        if pos[1]
            call setpos("'<", s:pos_with_col_offset(s:nearest_bracket(0), a:offset))
            call setpos("'>", s:pos_with_col_offset(pos, -a:offset))
        endif
    endif
endfunction

function! s:with_unmoved_cursor(cmd)
    let cursor = getpos('.')
    try
        let val = eval(a:cmd)
    finally
        call setpos('.', cursor)
    endtry
    return val
endfunction

" If line of '< is less than 1, inserts brackets at cursor
function! s:insert_brackets_around_visual_marks(bra, ket, at_head)
    let start = getpos("'<")
    let end = getpos("'>")

    " No form, just insert brackets
    if start[1] < 1
        execute 'normal! i' . a:bra . a:ket
    elseif a:at_head
        call setpos('.', end)
        execute 'normal! a' . a:ket
        call setpos('.', start)
        execute 'normal! i' . a:bra . ' '
    else
        call setpos('.', start)
        execute 'normal! i' . a:bra
        " Did we just insert a character on the same line?
        let end = start[1] == end[1] ? s:pos_with_col_offset(end, len(a:bra)) : end
        call setpos('.', end)
        execute 'normal! a' . a:ket
    endif
endfunction

" Mangles visual marks!
function! s:insert_brackets_around_current_form(bra, ket, at_head)
    call setpos("'<", [0, 0, 0, 0])
    call s:with_unmoved_cursor('sexp#set_bracket_marks(0)')
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_head)
endfunction

" Mangles visual marks!
function! s:insert_brackets_around_current_word(bra, ket, at_head)
    call setpos("'<", [0, 0, 0, 0])
    execute "normal! viw\<Esc>"
    call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_head)
endfunction

" Place brackets around scope, then place cursor at head or tail.
function! sexp#wrap(scope, bra, ket, at_head)
    let original_start = getpos("'<")
    let original_end = getpos("'>")

    " Wrap form.
    if a:scope ==# 'f'
        call s:insert_brackets_around_current_form(a:bra, a:ket, a:at_head)
    " Wrap form if on bracket, word otherwise.
    elseif a:scope ==# 'w'
        if s:current_char() =~ s:bracket
            call s:insert_brackets_around_current_form(a:bra, a:ket, a:at_head)
        else
            call s:insert_brackets_around_current_word(a:bra, a:ket, a:at_head)
        endif
    " Wrap current visual selection.
    elseif a:scope ==# 'v'
        call s:insert_brackets_around_visual_marks(a:bra, a:ket, a:at_head)
    endif

    call setpos("'<", original_start)
    call setpos("'>", original_end)
endfunction

" Remove brackets from current form, placing cursor at position of now-deleted
" first bracket.
function! sexp#splice_form()
    let original_start = getpos("'<")
    let original_end = getpos("'>")
    let cursor = getpos('.')

    call setpos("'<", [0, 0, 0, 0])
    call sexp#set_marks_around_current_form(0)

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
