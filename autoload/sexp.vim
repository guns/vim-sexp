
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
" * Don't ignore virtualedit mode
" * Check synstack() for syntax scope?
" * Extract common subroutines? (not if it impedes clarity)
" * Do we really care to balance bracket types in s:nearest_bracket()? We
"   should determine if this correctness is worth the performance hit.
" * Use tpope's repeat.vim to enable '.' command for our <Plug> mappings

""" PATTERNS AND STATE {{{1

let s:bracket = '\v\(|\)|\[|\]|\{|\}'
let s:opening_bracket = '\v\(|\[|\{'
let s:closing_bracket = '\v\)|\]|\}'
let s:delimiter = s:bracket . '|\s'
let s:pairs = [['\V(','\V)'], ['\V[','\V]'], ['\V{','\V}']]
let s:countindex = 0 " Stores current count index during sexp#docount

""" QUERIES AT CURSOR {{{1

" Like searchpos(), return first pattern match from cursor as [line, col].
" Unlike searchpos(), searching backwards when the cursor is on a multibyte
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
        if sline == line && &encoding ==? 'utf-8' && s:is_backwards_multibyte_search_broken()
            let col = scol + byteidx(getline(line), virtcol('.')) - col('.')
        else
            let [line, col] = [sline, scol]
        endif
    endif

    return [line, col]
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

" Position of outermost _paired_ bracket: 0 for opening, 1 for closing.
" Returns [0, 0, 0, 0] if none found.
function! s:current_top_form_bracket(closing)
    let [_b, line, col, _o] = getpos('.')
    let skip = 's:is_ignored_scope(line("."), col("."))'

    " searchpairpos() fails to find the matching closing bracket when on the
    " outermost opening bracket and vice versa, so we decide on the search
    " directions based on the current char.
    if getline(line)[col-1] =~ s:opening_bracket
        let flags = 'bcnr'
        let dir = 0
    else
        let flags = 'cnr'
        let dir = 1
    endif

    let [line, col] = searchpairpos(s:opening_bracket, '', s:closing_bracket, flags, skip)

    if line < 1
        return [0, 0, 0, 0]
    elseif dir == a:closing
        return [0, line, col, 0]
    else
        let cursor = getpos('.')
        call cursor(line, col)
        let pos = s:nearest_bracket(!dir)
        call setpos('.', cursor)
        return pos[1] > 0 ? pos : [0, 0, 0, 0]
    endif
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

" Position of start / end of current comment: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in a comment.
function! s:current_comment_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')
    if !s:is_comment(cursorline, cursorcol) | return [0, 0, 0, 0] | endif

    let [termline, termcol] = [cursorline, cursorcol]

    while 1
        let [line, col] = s:findpos('\v.', a:end)

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
"   * Current form if and only if cursor is on a _paired_ bracket
"   * Current string if cursor is in a string
"   * Current comment if cursor is in a comment
"   * Current contiguous region of whitespace if cursor is on whitespace
"   * Current atom otherwise
function! s:current_element_terminal(end)
    let [_b, line, col, _o] = getpos('.')
    let char = getline(line)[col - 1]

    if s:is_string(line, col)
        return s:current_string_terminal(a:end)
    elseif s:is_comment(line, col)
        return s:current_comment_terminal(a:end)
    elseif char =~ s:bracket
        if (a:end && char =~ s:closing_bracket) || (!a:end && char =~ s:opening_bracket)
            return [0, line, col, 0]
        else
            return s:nearest_bracket(a:end)
        end
    elseif empty(char) || char =~ '\v\s'
        return s:adjacent_whitespace_terminal([0, line, col, 0], a:end)
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
        if pos != terminal
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
            call setpos('.', pos)
        endif

        " Cursor ends on the head of an element, unless on a closing bracket
        " while moving forward.
        if a:next && getline(pos[1])[pos[2] - 1] =~ s:closing_bracket
            break
        endif

        let final = s:current_element_terminal(0)
        if final[1] > 0
            let pos = final
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
        let a_is_closer = eval(a:a[2] . op . a:b[2])
        return a_is_closer ? a:a : a:b
    endif
endfunction

function! s:syntax_name(line, col)
    return synIDattr(synID(a:line, a:col, 0), 'name')
endfunction

" Return start of leading (0) or end of trailing (1) whitespace from pos.
" Returns pos if no such whitespace exists.
function! s:adjacent_whitespace_terminal(pos, trailing)
    let cursor = getpos('.')

    call setpos('.', a:pos)
    let [_b, termline, termcol, _o] = getpos('.')

    while 1
        " We want to include empty lines
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

""" PREDICATES {{{1

" See discussion at s:findpos()
function! s:is_backwards_multibyte_search_broken()
    if exists('s:backwards_multibyte_search_is_broken')
        return s:backwards_multibyte_search_is_broken
    else
        let cursor = getpos('.')
        silent! call append(cursor[1], '123❤sexp-bugcheck')
        call cursor(cursor[1] + 1, 4)
        let s:backwards_multibyte_search_is_broken = searchpos('\v.', 'b')[1] != 3
        " FIXME: Remove this undo leaf!
        silent! normal! u
        call setpos('.', cursor)
        return s:backwards_multibyte_search_is_broken
    endif
endfunction

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

" Returns 1 is character at position is in a comment, or is in the whitespace
" between two line comments.
function! s:is_comment(line, col)
    if s:syntax_name(a:line, a:col) =~? 'comment'
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
            if s:syntax_name(pline, pcol) =~? 'comment' && s:syntax_name(cline, ccol) =~? 'comment'
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
    if getline(a:line)[a:col - 1] =~ s:delimiter
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
    let opmode = a:mode ==? 'o'
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
    if expanding && a:offset == 1 && getline(cursor[1])[cursor[2] - 2] =~ s:opening_bracket
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
" Will set both to [0, 0, 0, 0] if not currently in an element and mode does
" not equal 'v'.
function! s:set_marks_around_current_element(mode, inner)
    let start = [0, 0, 0, 0]
    let end = s:current_element_terminal(1)

    if end[1] > 0
        let start = s:current_element_terminal(0)
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
    call s:select_current_marks(a:mode)
endfunction

" Set visual marks at current outermost form's brackets, then enter visual
" mode with that selection. If no brackets are found and mode equals 'o',
" nothing is done.
function! sexp#select_current_top_form(mode, offset)
    call s:set_marks_around_current_top_form(a:mode, a:offset)
    call s:select_current_marks(a:mode)
endfunction

" Unlike the native text object a" we do not try to select all the whitespace
" up to the next element. This can be done with sexp#select_current_element if
" desired. If not currently in string and mode equals 'o', nothing is done.
function! sexp#select_current_string(mode, offset)
    call s:set_marks_around_current_string(a:mode, a:offset)
    call s:select_current_marks(a:mode)
endfunction

" Set visual marks around current comment and enter visual mode. If not
" currently in a comment and mode equals 'o', nothing is done.
function! sexp#select_current_comment(mode, inner)
    call s:set_marks_around_current_comment(a:mode, a:inner)
    call s:select_current_marks(a:mode)
endfunction

" Set visual marks around current atom and enter visual mode. If not currently
" in an atom and mode equals 'o', nothing is done.
function! sexp#select_current_atom(mode, inner)
    call s:set_marks_around_current_atom(a:mode, a:inner)
    call s:select_current_marks(a:mode)
endfunction

" Set visual marks around current element and enter visual mode.
function! sexp#select_current_element(mode, inner)
    call s:set_marks_around_current_element(a:mode, a:inner)
    call s:select_current_marks(a:mode)
endfunction

" Set visual marks around adjacent element and enter visual mode; 0 for
" previous, 1 for next. If no such adjacent element exists, selects current
" element.
function! sexp#select_adjacent_element(mode, next)
    call s:set_marks_around_adjacent_element(a:mode, a:next)
    call s:select_current_marks(a:mode)
endfunction

" Moves cursor to adjacent element; 0 for previous, 1 for next. If no such
" adjacent element exists, moves to beginning or end of element respectively.
" Analogous to native w and b commands.
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

        " Stop at current top element head if moving backwards and did not
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
            execute 'normal! gvo'
        else
            call setpos("'<", start)
            call setpos("'>", pos)
            execute 'normal! gv'
        endif
    else
        call setpos('.', pos)
    endif
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

    " We want to ensure we are not deleting chars at old marks
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
