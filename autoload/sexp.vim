

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

" Note: Do not uncomment the luaeval in this function, as it requires a logging module
" that's not currently part of the plugin.
" TODO: Eventually, remove it and all commented calls to it.
fu! s:Dbg(...)
    "call luaeval("require'dp':get'sexp':logf(unpack(_A))", a:000)
endfu

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

let s:countindex = -1 " Stores current count index during sexp#docount
let s:bracket = '\v\(|\)|\[|\]|\{|\}'
let s:opening_bracket = '\v\(|\[|\{'
let s:closing_bracket = '\v\)|\]|\}'
let s:delimiter = s:bracket . '|\s'
let s:string_region = '\vstring|regex|pattern'
let s:ignored_region = s:string_region . '|comment|character'
let s:match_ignored_region_fn = 's:is_rgn_type("str_com_chr", line("."), col("."))'
let s:nomatch_ignored_region_fn = '!s:is_rgn_type("str_com_chr", line("."), col("."))'
let s:filetype_macro_characters = {
    \ 'clojure': "#'`~@^_=",
    \ 'scheme':  "#'`,@",
    \ 'lisp':    "#'`,@",
    \ 'timl':    "#'`~@^_*",
    \ 'fennel':  "#'`,@",
    \ }
let s:default_macro_characters = s:filetype_macro_characters['scheme']
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
let s:aligncom_weights = {
    \ 'numgroups': {'default': 25, 'adjust': 0.10},
    \ 'runtness':  {'default': 50, 'adjust': 0.20},
    \ 'shift':     {'default': 50, 'adjust': 0.20},
    \ 'textwidth': {'default': 50, 'adjust': 0.20},
    \ 'density':   {'default': 25, 'adjust': 0.20},
\ }

" Cache for yank/delete metadata: keyed by register name.
" Each entry contains:
"   is_complete_sexp_in_buffer: 1 if yanked/deleted text is a complete sexp, 0 otherwise
"   from_sexp_object:           1 if yank/delete used a sexp object, 0 otherwise
let s:yank_metadata = {}
" Inhibit persistent regput TextYankPost logic during internal vim-sexp operations.
let s:regput_internal_typ_active = 0
" Saved 'virtualedit' value pending deferred restoration at SafeState.
let s:regput_pending_ve_restore = v:null

" Temporary hint created when sexp object command executes.
" Used by TextYankPost handler to validate whether subsequent yank/delete came from
" that sexp object. Structure:
"   start:      position [bufnr, lnum, col, off] of start of selection
"   end:        position [bufnr, lnum, col, off] of end of selection
"   text:       the selected text as a string
"   changedtick: b:changedtick when hint was created (used to validate yank case)
" Empty dict {} means no active hint.
let s:sexp_object_yank_hint = {}

let s:nullpos = [0,0,0,0]
let s:nullpos_pair = [s:nullpos, s:nullpos]
let s:BOF = [0, 1, -1, 0]
let [s:NL, s:SPC, s:EMPTY] = ["\n", " ", ""]

" Since v:maxcol wasn't added till Vim9
let s:MAXCOL = 2147483647

" Patch 7.3.590 introduced the ability to set visual marks with setpos()
let s:can_set_visual_marks = v:version > 703 || (v:version == 703 && has('patch590'))
" See note in header of the version of set_visual_marks() that uses setpos() with visual
" marks for an explanation of why we might want to clear this flag.
let s:use_setpos_for_visual_marks = 1

" Return macro characters for current filetype. Defaults to Scheme's macro
" characters if 'lisp' is set, invalid characters otherwise.
function! s:macro_chars()
    " Caveat: Allow for possibility that g:sexp_filetype_macro_characters does not exist.
    return get(
        \ get(g:, 'sexp_filetype_macro_characters', {}),
        \ &filetype,
        \ get(s:filetype_macro_characters, &filetype,
        \   &lisp ? s:default_macro_characters : ''))
endfunction

" Make a 'very magic' character class from input characters.
function! s:vm_cc(chars)
    return '[' . substitute(a:chars, '[^[0-9a-zA-Z_]]', '\\&', 'g') . ']'
endfunction

""" PRE/POST COMMAND CALLBACKS/CACHE {{{1

function! s:make_cache(mode, name)
    return {
        \ 'sel_dir': s:get_sel_dir(a:mode),
        \ 'mode': a:mode,
        \ 'name': a:name,
        \ 'changedtick': b:changedtick
    \ }
endfunction

" Invoked by the vim-sexp command wrapper sexp#plug#wrapper() before executing a sexp
" command. Also, invoked by s:opfunc() before executing a sexp operator's motion handler.
" Assumption: Calls will always be paired with calls to sexp#post_op().
function! sexp#pre_op(mode, name)
    call s:regput__begin_internal_typ()
    let s:sexp_ve_save = &ve
    set ve=onemore
    " Create the msg queue used by sexp#warn#msg().
    call sexp#warn#create_msg_q()
    " TODO: Consider removing or simplifying the cache, which is currently needed only for
    " its sel_dir flag.
    let b:sexp_cmd_cache = s:make_cache(a:mode == 'x' ? 'v' : a:mode, a:name)
endfunction

" See sexp#pre_op().
function! sexp#post_op(mode, name)
    try
        " Restore original 'virtualedit' setting.
        " Assumption: This is called from finally block.
        let &ve = s:sexp_ve_save
        " Perform once-per-session new feature notification iff appropriate.
        call sexp#feat#notify_maybe(a:mode, a:name)
        " This is done last to ensure no redraws can be queue after msgs are echoed.
        call sexp#warn#echo_queued_msgs()
        " Caveat: Make sure the queue is used only when we're inside {pre,post}_op() calls
        " (i.e., not within insert-mode commands).
        call sexp#warn#destroy_msg_q()
    finally
        call s:regput__end_internal_typ()
    endtry
endfunction

" Return 1 iff we're in one of the visual modes.
function! s:in_visual_mode()
	return mode() =~ "^[vV\<C-V>]"
endfunction

" This function is necessary when <cmd> is used in mappings.
" Rationale: The old approach (:<c-u>) changed mode from visual to normal as a side
" effect. Some sexp functions break if called in visual mode, and the <cmd> modifier does
" not cause exit from visual mode. The simplest solution is to place a call to this
" function after the <cmd> (and after capture of v:count/v:prevcount, which can be changed
" by a call to this function).
" Visibility Note: This would be file-static except that it's needed in plugin/sexp.vim.
function! sexp#ensure_normal_mode()
	if s:in_visual_mode()
		exe "normal \<Esc>"
	endif
endfu

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

function! s:nearest_bracket_legacy(closing, ...)
    let flags = a:closing ? 'nW' : 'bnW'
    let stopline = g:sexp_maxlines > 0
                   \ ? max([1, line('.') + ((a:closing ? 1 : -1) * g:sexp_maxlines)])
                   \ : 0
    let open = a:0 ? a:1 : s:opening_bracket
    let close = a:0 ? a:2 : s:closing_bracket
    let [line, col] = searchpairpos(open, '', close, flags, s:match_ignored_region_fn, stopline)
    return line > 0 ? [0, line, col, 0] : [0, 0, 0, 0]
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
    return call('sexp#invoke', ['nearest_bracket', a:closing] + a:000)
endfunction

" Return position representing the bracket that *contains* the cursor position, else
" nullpos if top-level.
" Note: Differs from s:nearest_bracket() only when reference pos is *on* bracket of type
" opposite to 'closing'.
function! s:containing_bracket(closing, ...)
    let save_cursor = getcurpos()
    " TODO: Consider using s:is_list() to determine whether we need to look up a level.
    try
        let p = call('s:nearest_bracket', [a:closing] + a:000)
        " Check to see whether this is the matching bracket of input pos.
        call s:setcursor(p)
        let opos = call('s:nearest_bracket', [!a:closing] + a:000)
        if !sexp#compare_pos(opos, save_cursor)
            " We found matching bracket. Need to go up a level.
            let p = call('s:nearest_bracket', [a:closing] + a:000)
        endif
        return p
    finally
        call s:setcursor(save_cursor)
    endtry
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
        keepjumps call cursor(line, col)
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
    let [topline, topcol] = searchpairpos(s:opening_bracket, '', s:closing_bracket,
                \ flags, s:match_ignored_region_fn, stopline)

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
            keepjumps call cursor(line, col)
        else
            break
        endif
    endwhile
    keepjumps call cursor(cursorline, cursorcol)
    return [0, termline, termcol, 0]
endfu

" Position of start/end of current atom: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in an atom. Assumes atoms never span multiple
" lines.
function! s:current_atom_terminal(end)
    return sexp#invoke('current_atom_terminal', a:end)
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

    keepjumps call cursor(cursorline, cursorcol)
    return [0, termline, termcol, 0]
endfunction

" Position of start/end of current comment: 0 for start, 1 for end. Returns
" [0, 0, 0, 0] if not currently in a comment.
function! s:current_comment_terminal(end)
    let [_b, cursorline, cursorcol, _o] = getpos('.')

    let ret = s:current_region_terminal('comment', a:end)
    keepjumps call cursor(cursorline, cursorcol)
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
            " Important TODO: Consider pros/cons of use of cursor() rather than
            " s:setcursor(), or setpos('.').
            keepjumps call cursor(line, col)
        else
            break
        endif
    endwhile

    keepjumps call cursor(cursorline, cursorcol)
    return [0, termline, termcol, 0]
endfunction

" Return range as [start,end] for element under the cursor, else [nullpos, nullpos].
function! sexp#current_element_terminals()
    let s = sexp#current_element_terminal(0)
    if s[1]
        return [s, sexp#current_element_terminal(1)]
    endif
    return [s:nullpos, s:nullpos]
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
    elseif sexp#is_comment(line, col)
        let pos = s:current_comment_terminal(a:end)
    elseif char =~# s:bracket && !s:is_rgn_type('str_com_chr', line, col)
        if (a:end && char =~# s:closing_bracket) || (!a:end && char =~# s:opening_bracket)
            let pos = [0, line, col, 0]
        else
            let pos = s:nearest_bracket(a:end)
        end
    elseif sexp#is_macro_char(char)
        if !a:end
            " Let the rest of the function find the macro head
            let include_macro_characters = 1
            " If the macro character is at the tail of an atom, treat it as
            " part of the atom and return the head of the preceding element.
            if !s:is_atom(line, col + 1) && s:is_atom(line, col - 1)
                keepjumps call cursor(line, col - 1)
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
                keepjumps call cursor(macro_tail[1], macro_tail[2] + 1)
                let pos = sexp#current_element_terminal(1)
                keepjumps call cursor(line, col)
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
        keepjumps call cursor(pos[1], pos[2] - 1)
        let pre = s:current_macro_character_terminal(0)
        keepjumps call cursor(line, col)
        return pre[1] > 0 ? pre : pos
    endif
endfunction

" Returns position of previous/next element's head/tail.
" Returns current element's terminal if no adjacent element exists, unless optional
" 'ignore_current' argument is set, in which case, return unmodified current position,
" unless 'nullpos_on_fail' (2nd optional arg) is also set, in which case, return nullpos.
" TODO: Consider impact of combining 'ignore_current' and 'nullpos_on_fail' into a single
" flag requesting nullpos on fail.
" Rationale: I can't think of a scenario in which we'd want ignore_current but not
" nullpos_on_fail, and there's at least one call site
" (sexp#move_to_adjacent_element_terminal) where we simply repeat the provided
" ignore_current arg in the call to this function.
" Implementation Note: Original version made liberal use of 'sexp-error' throws to handle
" short-circuiting in non-error scenarios; although I never used profiling to determine
" the performance implications of that approach, it tended to be distracting in a
" debugger. Accordingly, I've converted the throws to bare return statements, with the
" actual return value constructed in a finally block.
" TODO: Convert the optional args to a (possibly optional) flags dict.
function! sexp#nearest_element_terminal(next, tail, ...)
    let cursor = getpos('.')
    " By function end, a non-null pos indicates the desired terminal pos iff
    " !ignore_current OR have_adj.
    " Rationale: The function logic sets 'pos' optimistically, even in cases where we
    " ultimately fail to find adjacent.
    let [pos, has_adj] = [s:nullpos, 0]
    " Cache optional flags.
    let ignore_current = a:0 && a:1
    let nullpos_on_fail = a:0 > 1 && a:1
    try
        " Attempt to find edge of current element (if applicable).
        let pos = sexp#current_element_terminal(a:next)
        if !pos[1] && sexp#range_has_non_ws(cursor, cursor, 1)
            " nullpos returned by sexp#current_element_terminal() from a non-ws pos
            " generally means unbalanced bracket!
            return
        endif
        if pos[1] > 0 && sexp#compare_pos(pos, cursor) != 0
            " Cursor was on element and not already at edge in search direction.
            " We may be done: i.e.,
            " b moves to the head of the current word if not already on the
            " head and e moves to the tail if not on the tail. However, ge
            " will never result in updated position on starting element!
            if !ignore_current && a:next == a:tail
                return
            endif
            " Still need adjacent; move to found pos before searching for it.
            call s:setcursor(pos)
        endif
        " Look for adjacent.
        let [l, c] = s:findpos('\v\S', a:next)
        if !l
            " We hit beginning or end of file without finding adjacent.
            return
        elseif getline(l)[c - 1] =~ (a:next ? s:closing_bracket : s:opening_bracket)
            \ && !s:is_rgn_type('str_com_chr', l, c)
            " Already on head or tail of list
            return
        else
            " Found near side of adjacent element.
            let pos = [0, l, c, 0]
            call s:setcursor(pos)
            if !a:next
                " Note: The s:findpos() above will find only literal non-ws, but we need
                " to treat ignored ws (e.g., escaped space) as non-ws.
                " Assumption: Ignored ws can be found at end of element, but not
                " beginning. TODO Should we skip this if not on ignored char or would it
                " be slower to check?
                let pos = sexp#move_to_current_element_terminal(1)
            endif
        endif
        " We're on near side of adjacent whose existence guarantees successful return,
        " regardless of optional input flags.
        " If movement is b or e (next == tail), move to far side of adjacent.
        let has_adj = 1
        if a:next == a:tail
            let pos = sexp#current_element_terminal(a:tail)
        endif
    finally
        call s:setcursor(cursor)
        " Note: The bare return statements above work like simple throws; the return value
        " is constructed here.
        return ignore_current && !has_adj || !pos[1]
            \ ? nullpos_on_fail ? s:nullpos : cursor
            \ : pos
    endtry
endfunction

""" QUERIES AT POSITION {{{1

" TODO: Consider performance impact of replacing calls to this with sexp#offset_char(), which
" handles multibyte.
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

" Return true iff we should attempt to use Treesitter before fallback to legacy.
fu! s:prefer_treesitter()
    return (!g:sexp_prefer_legacy_syntax || empty(&syn) || &syn ==? "off") && has('nvim')
endfu

" Dicts of language-specific flags indicating which warnings have already been displayed
" to user.
let s:dispatch_warned = {'ts': {}, 'syn': {}}

" Invoke either the Treesitter-based Lua function or the legacy syntax-based Vimscript
" function with the provided arguments, with fallback to the latter if the former returns
" nil (indicating a missing tree or some other serious issue).
" Warn user once-only about the following two conditions:
" 1) We prefer to use Treesitter but the ts.lua method returns nil (typically because
"    there's no language parser, though there could be other reasons).
" 2) We need legacy syntax but it's not available: i.e., *neither* Treesitter *nor* legacy
"    syntax.
" Note: Could move the dispatch checks upstream to the point of (user-visible) command
" invocation (e.g., to sexp#pre_op()) and refuse to do anything if we don't have a
" Treesitter node or legacy syntax. With this approach, we could set a ts/legacy dispatch
" flag once per command invocation to obviate the need for (potentially many) downstream
" checks at the point of individual implementation function invocations. Currently, this
" approach is complicated by the existence of insert-mode maps, which have nothing
" analogous to sexp#pre_op() but do invoke methods requiring dispatch.
" TODO: Considered providing mechanism for dispatch to modules other than ts.lua and
" sexp#autoload; for now, it seems unnecesary.
" Rationale: ts.lua and sexp#autoload essentially the api for the lua and vim pieces,
" respectively. If the implementation requires functions in other modules, the
" corresponding ts.lua or sexp#autoload function can wrap them.
" Note: This function is part of the API to permit its name to be used as a callback from
" other modules (currently only in sexp#parse).
fu! sexp#invoke(fn, ...)
    if s:prefer_treesitter()
        let ret = luaeval(
                    \ "require'sexp.ts'." . a:fn . "(" . 
                    \ join(map(range(a:0), '"_A[" . (v:val + 1) . "]"'), ", ")
                    \ . ")", a:000)
        " Fall through to legacy if Treesitter method returns nil.
        if ret isnot v:null
            return ret
        endif
        " Warn once only.
        call sexp#warn#msg(
            \ "Warning: Falling back to legacy syntax."
            \ . " Have you installed Treesitter parser for " . &filetype . "?",
            \ {'once': 'missing_ts'})
    endif
    " Arrival here means we won't or can't use Treesitter. If we don't have legacy syntax,
    " we're going to have a problem, so warn...
    if empty(get(b:, 'current_syntax', ''))
        " Note: Display with error highlighting, as this is more serious than missing
        " Treesitter parser, since we have nothing to fallback to.
        call sexp#warn#msg(
            \ "Warning: No syntax available for filetype '" . &filetype . "'",
            \ {'once': 'missing_syn', 'err': 1})
        " Fall through to legacy function *without* syntax, since we've no better option.
    endif
    " Use legacy approach.
    return call('s:' . a:fn . '_legacy', a:000)
endfunction

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
    return sexp#invoke('current_region_terminal', a:rgn, a:end)
endfu

" Return start of leading (0) or end of trailing (1) whitespace from pos.
" Returns pos if no such whitespace exists.
function! s:adjacent_whitespace_terminal(pos, trailing)
    let cursor = getpos('.')
    call s:setcursor(a:pos)

    let [_b, termline, termcol, _o] = a:pos

    " FIXME: Construct a single regex for this.
    while 1
        " Include empty lines
        let [line, col] = s:findpos('\v\_.', a:trailing)

        if line < 1 | break | endif

        let char = getline(line)[col - 1]

        if empty(char) || char =~# '\v\s'
            let termline = line
            let termcol = col
            keepjumps call cursor(line, col)
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
" !!! Obsolete function !!!
" TODO: Remove this after verifying it will no longer be needed!
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
    let p = sexp#nearest_element_terminal(0, 1)
    " Caveat: This test assumes sexp#nearest_element_terminal() returns current position if no
    " preceding element.
    let o.bos = sexp#compare_pos(p, a:start) >= 0
    let o.follows_com = !o.bos && sexp#is_comment(p[1], p[2])
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
        let p = sexp#nearest_element_terminal(1, 0)
    endif
    let o.is_com = sexp#is_comment(p[1], p[2])
    " Are we at end of sexp?
    call s:setcursor(a:end)
    let p = sexp#nearest_element_terminal(1, 0)
    let o.eos = sexp#compare_pos(p, a:end) <= 0
    let o.precedes_com = !o.eos && sexp#is_comment(p[1], p[2])
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
    let o.ws_ve = sexp#offset_char(o.ws_e, 1, 1)[2] >= ecol
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
        let o.ws_si = sexp#compare_pos(o.ws_s, o.start) >= 0
            \ ? o.start[:] : sexp#offset_char(o.ws_s, 0)
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
        let o.ws_ei = sexp#compare_pos(o.ws_e, o.end) <= 0
            \ ? o.end[:] : sexp#offset_char(o.ws_e, 0)
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

" Return 1 iff element at specified position is in head/tail list context: i.e., on or
" outside head/tail element.
" TODO: Consider renaming to in_terminal_context() or something that emphasizes fact that
" we don't need to be on the terminal, and can't be *inside* it. Rename would also help
" avoid confusion with "is list bracket".
" TODO: Consider making these helpers not save/restore cursor.
function! s:is_list_terminal(pos, tail)
    let save_cursor = getpos('.')
    try
        call s:setcursor(a:pos)
        " Attempt to find previous element.
        let p = sexp#nearest_element_terminal(a:tail, !a:tail, 1)
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

" Return 1 iff input position is adjacent to comment in specified direction.
" TODO: Consider making these helpers not save/restore cursor.
function! s:is_adjacent_to_comment(pos, tail)
    let save_cursor = getpos('.')
    try
        let p = sexp#nearest_element_terminal(a:tail, !a:tail, 1)
        return p != a:pos && sexp#is_comment(p[1], p[2])
    finally
        call s:setcursor(save_cursor)
    endtry
    return 0
endfunction

" Return 1 iff making both sides of the input positions colinear would not violate any
" constraints.
" Considering relevant options and surrounding buffer context, return 1 iff deleting range
" start..end would not cause a constraint-violating join.
" Relevant Options:
" join_affinity:
"     0 = never join
"     1 = append only to head of list
"     2 = append anywhere but top-level (subject to other constraints)
"     3 = append anywhere (including top-level, subject to other constraints)
" join_multiline
"     0 = allow append of single line elements only
"     1 = allow append of both single and multi-line elements
" join_tw
"     <floatnr> fraction of original line length
"     0         disables checking (as with 'tw')
"     -1        use &tw
"     else override &tw
" Cursor Positioning: No need to preserve
function! s:can_join(start, end)
    let [start, end] = [a:start, a:end]
    if has('float') && type(g:sexp_cleanup_join_textwidth) == 5
        " Treat 'textwidth' as fraction of original start line length.
        let tw = float2nr(g:sexp_cleanup_join_textwidth * col([start[1], '$']))
    else
        let tw = g:sexp_cleanup_join_textwidth < 0
            \ ? (&tw ? &tw : 80) : g:sexp_cleanup_join_textwidth
    endif
    let [affinity, ml] = [g:sexp_cleanup_join_affinity, g:sexp_cleanup_join_multiline]
    " Get prev/next.
    " TODO: Consider hiding this block in a function called get_surrounding_elements() or
    " perhaps get_surrounding_context() (in which case, it should put the elements and
    " more in a returned dict).
    keepjumps call s:setcursor(end)
    " TODO: Take advantage of the nullpos_on_fail flag!
    let p = sexp#nearest_element_terminal(1, 0, 1)
    let next_s = p == end ? s:nullpos : p
    let next_e = next_s[1] ? sexp#nearest_element_terminal(1, 1, 1) : s:nullpos
    keepjumps call s:setcursor(start)
    let p = sexp#nearest_element_terminal(0, 0, 1)
    let prev_s = p == start ? s:nullpos : p
    let prev_e = prev_s[1] ? sexp#nearest_element_terminal(0, 1, 1) : s:nullpos

    " Note: affinity shouldn't be less than zero, but err on side of disabling...
    if affinity <= 0 || !next_s[1]
        " Joining disabled or no next element to append.
        return 0
    endif
    " Is join precluded by line length constraint?
    if tw > 0 && tw < prev_e[2] + col([next_s[1], '$']) - next_s[2]
        return 0
    endif
    if !ml && (next_s[1] != next_e[1] || prev_s[1] != prev_e[1])
        " Only single-line joins permitted.
        " TODO: Original logic considered only next; should we consider both the element
        " joined and joined to?
        return 0
    endif
    " Apply affinity constraints.
    if affinity == 1 && !s:is_list_terminal(prev_e, 0)
        " Affinity is for appends to list head, but prev element is not list head.
        return 0
    endif
    " Now that all other checks have passed, do top-level check only if necessary.
    if affinity < 3 && s:at_top(start[1], start[2])
        return 0
    endif
    return 1
endfunction

" Given start and end positions, returns new positions [start', end'] reflecting start/end
" of OUTER ELEMENT selection.
" Logic: There are 3 basic categories of 'join' implemented by this function:
" 1. Full join: selection is bounded on one side with a bracket and on the other with
"    something that can be directly adjacent to the bracket.
" 2. Half join: element following selection can be pulled back to the start of the first
"    element in outer element selection.
"    Note: This type of join is the least visually disconcerting on delete since the
"    element following the selection appears to replace the first deleted element, with
"    cursor position remaining at the start of the replaced element.
" 3. No join: element following selection cannot be appended to line containing start of
"    selection, though we may be able to delete some trailing blank lines (in accordance
"    with g:sexp_cleanup_keep_empty_lines) as long as we preserve indent of line
"    containing element following selection.
function! s:terminals_with_whitespace(start, end)
    let [start, end] = [a:start, a:end]
    " Note: These functions ignore newlines, but will return the position of the NUL at
    " the start of a blank line.
    let ws_end = s:adjacent_whitespace_terminal(end, 1)
    let ws_start = s:adjacent_whitespace_terminal(start, 0)
    " Get line containing the last non-newline char before start of whitespace.
    let eff_prevl = ws_start[2] == 1 && ws_start[1] > 0 ? ws_start[1] - 1 : ws_start[1]
    " Get line containing the first non-newline char past end of whitespace.
    let eff_nextl = ws_end[2] == col([ws_end[1], '$']) - 1
            \ ? ws_end[1] + 1 : ws_end[1]
    " Determine join type.
    if (g:sexp_cleanup_join_backwards || eff_prevl == start[1])
        \ && s:is_list_terminal(start, 0)
        \ && (g:sexp_cleanup_join_comments || !s:is_adjacent_to_comment(end, 1))
        \ || s:is_list_terminal(end, 1) && !s:is_adjacent_to_comment(start, 0)
        " Full join: select everything between open or close bracket and the nearest
        " non-ws, which we've already determined can be safely juxtaposed to the bracket.
        " Extra logic is needed to pull in a newline adjacent to (and inside) the bracket.
        let start = ws_start[2] == 1 && ws_start[1] > 1
            \ ? [0, ws_start[1] - 1, col([ws_start[1] - 1, '$']), 0]
            \ : ws_start
        let end = eff_nextl > ws_end[1]
            \ ? [0, ws_end[1], col([ws_end[1], '$']), 0]
            \ : ws_end
    elseif end[1] == eff_nextl || s:at_bol(start[1], start[2]) || s:can_join(start, end)
        " Half join (at end only): select up to next non-ws, leaving start as-is to
        " prevent loss of indent.
        let end = ws_end
    else
        " No join: selecting all the way to next non-ws would result in an illegal join;
        " accordingly, select all colinear whitespace and some (but not all) of the
        " trailing newlines, taking care to respect options related to number of blank
        " lines to keep.
        " Assumption: Arrival here guarantees !bol and eol
        " Select back to colinear prev.
        let start[2] = ws_start[2]
        " Calculate line whose newline should be excluded from selection.
        " Note: keep_empty_lines == -1 yields original behavior (no empty line cleanup).
        let endline = g:sexp_cleanup_keep_empty_lines < 0
            \ ? end[1]
            \ : max([ eff_nextl - g:sexp_cleanup_keep_empty_lines - 1, end[1]])
        " Select up to but not including the newline.
        " Caveat: Must account for fact that col 1 (or 0) of empty line selects the
        " newline we wish to exclude!
        let endcol = col([endline, '$']) - 1
        let end = endcol > 0
            \ ? [0, endline, endcol, 0]
            \ : [0, endline - 1, col([endline - 1, '$']), 0]
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
            keepjumps call cursor(line, col)
            call sexp#move_to_adjacent_element_terminal(1, 0, 0)
            continue
        endif

        " Break if bracket found after end or EOF hit by searchpos (!line).
        let cmp = !line ? 1 : sexp#compare_pos([0, line, col, 0], a:end)
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
            keepjumps call cursor(line + 1, 1)
        else
            keepjumps call cursor(line, col + 1)
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
        let nextpos = sexp#move_to_adjacent_element_terminal(1, 0, 0)
        if sexp#compare_pos(nextpos, a:end) > 0 | break | endif
        let n += 1
        if sexp#compare_pos(pos, nextpos) == 0 | break | endif
        let pos = nextpos
    endwhile

    call s:setcursor(cursor)
    return n
endfunction

" Return input pos offset by 1 char in requested direction
" Note: If optional flag is set, newlines between lines count.
function! sexp#offset_char(pos, dir, ...)
    let cursor = getpos('.')
    let inc_nl = a:0 && !!a:1
    let eol = col([a:pos[1], '$'])
    try
        if a:pos == s:BOF
            " Input position is before BOF.
            let pos = [0, 1, 1, 0]
        elseif a:pos[2] >= eol
            " Input position is past EOL
            " Handle input positions past EOL require special handling.
            " Rationale: We can't begin a searchpos() from positions that don't correspond
            " to a legal cursor position.
            if a:dir
                " Regardless of inc_nl, use beginning of next line (since input pos *is*
                " the NL).
                " Special Case: If at EOF, use virtual position past EOF.
                let pos = a:pos[1] == line('$')
                    \ ? [0, line('$'), col([line('$'), '$']), 0]
                    \ : [0, a:pos[1] + 1, 1, 0]
            else " !a:dir
                if eol > 1
                    " Non-empty line; use last char on line.
                    " Use cursor() to ensure start of mb char.
                    " Note: There's a way to do this without setting cursor, but minimum
                    " required Vim version lacks charidx().
                    keepjumps call cursor(a:pos[1], eol - 1)
                    let pos = getpos('.')
                else
                    " Empty line
                    if a:pos[1] == 1
                        " Special Case: before BOF
                        let pos = s:BOF
                    else
                        " Need to consider inc_nl
                        let prev_eol = col([a:pos[1] - 1, '$'])
                        if inc_nl || prev_eol == 1
                            " Use NL of previous line.
                            let pos = [0, a:pos[1] - 1, prev_eol, 0]
                        else
                            " Use last char of previous line
                            keepjumps call cursor(a:pos[1] - 1, prev_eol - 1)
                            let pos = getpos('.')
                        endif
                    endif
                endif
            endif
        else
            " Input position on *actual* char.
            " Move to input pos and find next non-NL char (or NUL char on empty line).
            call s:setcursor(a:pos)
            " Note: 'z' flag has different meaning for forward/backward search.
            let [l, c] = searchpos('\v.|^$', 'Wn' . (a:dir ? 'z' : 'b'))
            if l
                let pos = [0, l, c, 0]
                if pos[1] != a:pos[1]
                    " Adjacent char is on different line; if inc_nl, use the passed over
                    " NL, else keep the found position.
                    if inc_nl
                        " Use passed over NL.
                        let pos = a:dir
                            \ ? [0, a:pos[1], eol, 0]
                            \ : [0, pos[1], col([pos[1], '$']), 0]
                    endif
                endif
            else
                " Beginning or end of buffer. Use virtual pos before BOF or after EOF.
                let pos = a:dir ? [0, a:pos[1], eol, 0] : [0, 1, 0, 0]
            endif
        endif
    finally
        " Restore original position.
        call s:setcursor(cursor)
    endtry
    return pos
endfunction

" See s:super_range() for function description.
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
        let cmp = !shared_close[1] ? 1 : sexp#compare_pos(shared_close, end)
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
        while !shared_close[1] || sexp#compare_pos(p, shared_close) < 0
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
    if !sexp#range_has_non_ws(a:start, a:end, 1)
        " Both ends of selection in same blank/whitespace
        " TODO: Ok to return the original ends, or do we need to find some sort of
        " terminals?
        "call s:Dbg("Both ends in same whitespace optimization!")
        return [a:start, a:end]
    endif
    " Ignore leading/trailing whitespace.
    let [start, end] = s:trim_range(a:start, a:end)
    " Are both ends of selection in same atom?
    if !sexp#range_has_ws(start, end, 1)
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
    " No short-circuit optimization was performed; call the more expensive function to
    " get a possibly expanded range.
    let [start, end] = sexp#invoke('super_range', start, end)
    if !start[1]
        return s:nullpos_pair
    endif
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
        let cmp = sexp#compare_pos(that, ket)
        if that_dir && cmp >= 0 || this_dir && cmp <= 0
            " Limiting *may* be required. In any case, we need to determine
            " exclusivity of limit: even if cmp alone guarantees we'll be
            " limiting, exclusivity will determine the limiting position.
            let exc = s:nearest_bracket(this_dir) != this
            if exc || cmp
                let lim = exc ? sexp#offset_char(ket, this_dir) : ket
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

" Calculate and return a dict representing the re-indent required for the buffer
" modification spanning start/end, taking g:sexp_auto_indent_range option into account.
" Return Dict:
"   start:  start of indent range in explicit range mode, else N/A
"   end:    end of indent range in explicit range mode, else N/A
"   top:    -1 to request explicit range mode, else argument for sexp#indent()
"   cnt:    count argument for sexp#indent() (N/A in explicit range mode)
function! s:get_reindent_range(s, e)
    let cursor = getpos('.')
    let ai_range = g:sexp_auto_indent_range
    " Return range may be adjusted below.
    let [s, e] = [a:s, a:e]
    " These will be adjusted later if parent list is to be used in lieu of explicit range.
    let [top, cnt] = [-1, 1]
    try
        " Position at start of first element of input range.
        let [s, e] = s:super_range(s, e)
        " If at top, just use [s,e] with explicit range mode.
        if !s:at_top(s[1], s[2])
            if ai_range == 0 && s:is_list_terminal(s, 0)
                " Changes to list head can propagate to the end of a list, so force
                " re-indent of parent.
                let ai_range = 1
            endif
            " Assumption: Containing if guard obviates need to validate bracket searches.
            if ai_range < 0
                " containing top-level form (should be -1, but be permissive)
                let top = 1
            elseif ai_range > 0
                " N containing forms
                let [top, cnt] = [0, ai_range]
                " If start is open bracket, increment configured count by 1.
                " Rationale: If start and end are siblings and start is a list, the first
                " call to select_current_list() will select only the start list, not the
                " common parent, but it's the common parent that the user would expect to
                " be selected by an option value of 1.
                if s:is_list(s[1], s[2]) == 2
                    let cnt += 1
                endif
            else " ai_range == 0
                " Calculate impacted range.
                " Logic: We know s is not list head, so as long as indentation was correct
                " before the operation, we should be able to keep it so by indenting up to
                " the first 'clean point' (i.e., line break between elements) after e.
                call s:setcursor(e)
                let [p, d] = s:last_colinear_sibling(1)
                let e = d.brkt[1] ? d.brkt : p
            endif
        endif
    finally
        call s:setcursor(cursor)
    endtry
    " Note: top == -1 requests explicit range indent (rather than list mode).
    return {'top': top, 'cnt': cnt, 'start': s, 'end': e}
endfunction

" Perform re-indent required for operation affecting lines from s to e, passing the
" provided position list to sexp#indent() for adjustment.
" TODO: Eventually, this should be used for commands other than register puts!
function! s:post_op_reindent(s, e, ps)
    let rng = s:get_reindent_range(a:s, a:e)
    " Note: Initially, I skipped single-line indents; however, even a single line indent
    " makes sense, as the preceding line is considered.
    if rng.top >= 0
        " Indenting a parent (or ancestor) list rather than explicit range.
        " Position cursor on start and let sexp#indent() find applicable parent.
        let mode = 'n'
        call s:setcursor(rng.start)
    else
        let mode = 'v'
        " Re-indent explicit range.
        call s:set_visual_marks([rng.start, rng.end])
        " Note: Though we use visual marks, it's important that we be in normal mode.
        call sexp#ensure_normal_mode()
    endif
    " sexp#indent() will use mode and top to determine operating mode.
    " Caveat: Convert rng.top == -1 (explicit range) to 0.
    call sexp#indent(mode, rng.top == 1, rng.cnt, -1, 1, a:ps)
endfunction

""" GENERAL UTILITIES {{{1

" Normalize regcontents to a string, joining list-form register contents with NL.
function! s:regcontents_to_text(regcontents)
    return type(a:regcontents) == type([]) ? join(a:regcontents, "\n") : a:regcontents
endfunction

" Extract text from a range in the current buffer.
" Args:
"   start: position [bufnr, lnum, col, off]
"   end:   position [bufnr, lnum, col, off]
" Returns: string containing text from start to end (inclusive)
function! s:extract_text_from_range(start, end)
    let [start_lnum, start_col] = [a:start[1], a:start[2]]
    let [end_lnum, end_col] = [a:end[1], a:end[2]]
    
    " Handle single-line case
    if start_lnum == end_lnum
        let line = getline(start_lnum)
        return strpart(line, start_col - 1, end_col - start_col + 1)
    endif
    
    " Multi-line case: extract lines and join
    let lines = getline(start_lnum, end_lnum)
    
    " Trim first line to start_col
    let lines[0] = strpart(lines[0], start_col - 1)
    
    " Trim last line to end_col
    let lines[-1] = strpart(lines[-1], 0, end_col)
    
    return join(lines, "\n")
endfunction

" Calculate byte offset from position pos1 to pos2 within current buffer.
" Args:
"   pos1: position [bufnr, lnum, col, off]
"   pos2: position [bufnr, lnum, col, off]
" Returns: byte offset (or -1 if pos2 < pos1)
function! s:calculate_offset(pos1, pos2)
    let [lnum1, col1] = [a:pos1[1], a:pos1[2]]
    let [lnum2, col2] = [a:pos2[1], a:pos2[2]]
    
    if lnum2 < lnum1 || (lnum2 == lnum1 && col2 < col1)
        return -1  " pos2 is before pos1
    endif

    if lnum1 == lnum2
        return col2 - col1
    endif

    " Add bytes from pos1 to end of its line, plus the intervening newline.
    let offset = strlen(getline(lnum1)) - (col1 - 1) + 1

    " Add bytes for all complete lines between lnum1 and lnum2.
    for lnum in range(lnum1 + 1, lnum2 - 1)
        let offset += strlen(getline(lnum)) + 1
    endfor

    " Add bytes in the final line before pos2.
    let offset += col2 - 1
    return offset
endfunction

" Trim leading/trailing whitespace from a range and return the resulting [start, end]
" positions. Return a pair of null positions if the range contains only whitespace.
function! s:trim_range_to_non_ws(start_pos, end_pos)
    let text = s:extract_text_from_range(a:start_pos, a:end_pos)
    let lead = match(text, '\S')
    if lead < 0
        return s:nullpos_pair
    endif

    let trail = match(text, '\s*$')
    let start_byte = s:pos2byte(a:start_pos) + lead
    let end_byte = s:pos2byte(a:start_pos) + trail - 1
    return [s:byte2pos(start_byte), s:byte2pos(end_byte)]
endfunction

" Return 1 iff the range spans one or more complete sibling elements, allowing
" surrounding whitespace only outside the trimmed range.
function! s:range_covers_whole_elements(start_pos, end_pos)
    let cursor = getpos('.')
    try
        call s:setcursor(a:start_pos)
        let start_term = sexp#current_element_terminal(0)
        if start_term !=# a:start_pos
            return 0
        endif

        let end_term = sexp#move_to_current_element_terminal(1)
        while 1
            let cmp = sexp#compare_pos(end_term, a:end_pos)
            if cmp == 0
                return 1
            elseif cmp > 0
                return 0
            endif

            let next_start = sexp#move_to_adjacent_element_terminal(1, 0, 0, 1)
            if !next_start[1] || sexp#compare_pos(next_start, a:end_pos) > 0
                return 0
            endif

            let end_term = sexp#move_to_current_element_terminal(1)
        endwhile
    finally
        call s:setcursor(cursor)
    endtry
endfunction

""" PREDICATES AND COMPARATORS {{{1

" Returns 1 if char matches the current FileType's macro pattern
function! sexp#is_macro_char(char)
    " Caveat: stridx returns 0 for empty needle.
    return !empty(a:char) && stridx(s:macro_chars(), a:char) >= 0
endfunction

function! sexp#current_macro_character_terminal(end)
    return s:current_macro_character_terminal(a:end)
endfunction


" Returns 1 if character at position is in a comment, or is in the whitespace
" between two line comments.
function! sexp#is_comment(line, col)
    if s:is_rgn_type('comment', a:line, a:col)
        return 1
    else
        let incomment = 0

        " We may be in the whitespace between two line comments; check if the
        " current line begins with a comment and the previous line ended with
        " a comment.
        if getline(a:line)[a:col - 1] =~# '\v\s'
            let cursor = getpos('.')
            keepjumps call cursor(a:line, a:col)
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

" Return 1 iff input position is within an end-of-line comment.
" Note: An inline comment (where such things exist) is considered eol if and only if there
" is no trailing whitespace; in fact, the only reason an inline comment is ever considered
" eol is that, in the legacy syntax case, we have no fully general way to differentiate
" between end of line comments and inline comments that extend to the end of the line. If
" this changes, it might make sense to return false unconditionally for inline comments.
function! s:is_eol_comment(line, col)
        let ret = 0
        if s:is_rgn_type('comment', a:line, a:col)
            let save_cursor = getcurpos()
            try
                call s:setcursor([0, a:line, a:col, 0])
                " Make sure comment extends to end of line.
                let p = sexp#current_element_terminal(1)
                " Is this the final char on the line? If not, we don't consider this an
                " eol comment, even if there's nothing past it but whitespace (which would
                " imply this is an inline comment).
                if s:at_eol(p[1], p[2], 1)
                    let ret = 1
                endif
            finally
                call s:setcursor(save_cursor)
            endtry
        endif
    return ret
endfunction

" Returns nonzero if input position is at toplevel.
function! s:at_top(line, col)
    let save_cursor = getcurpos()
    try
        let cursor = getpos('.')
        call s:setcursor([0, a:line, a:col, 0])
        let ret = !s:nearest_bracket(0)[1] || !s:nearest_bracket(1)[1]
        return ret
    finally
        call s:setcursor(save_cursor)
    endtry
endfunction

" Returns nonzero if input position first non-ws on line
" Note: Accepts virtual cursor pos at EOL.
function! s:at_bol(line, col)
    return getline(a:line)[:a:col - 2] !~ '\S'
endfunction

" Returns nonzero if input position is last non-ws on line (or if optional 'ignore_ws' arg
" is provided, last char of *any* type on line).
" Note: Accepts virtual cursor pos at EOL.
function! s:at_eol(line, col, ...)
    let ignore_ws = a:0 && !!a:1
    return getline(a:line)[a:col - 1:] =~ '^.\?' . (!ignore_ws ? '\s*' : '') . '$'
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
function! sexp#compare_pos(a, b)
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
fu! sexp#range_has_ws(beg, end, check_ignored)
    let save_cursor = getcurpos()
    try
        call s:setcursor(a:beg)
        " Note: Empty 'skip' skips nothing.
        let pos = searchpos('\s', 'nczW', a:end[1], 0, a:check_ignored ? s:match_ignored_region_fn : '')
        let ret = pos[0] && sexp#compare_pos([0, pos[0], pos[1], 0], a:end) <= 0
        return ret
    finally
        call s:setcursor(save_cursor)
    endtry
endfu

" Return true iff there's *any* non-whitespace in the range [beg,end].
" Note: See previous function comment for usage of 'check_ignored'.
" TODO: Make check_ignored optional, defaulting to true.
" TODO: Consider taking a true range (for consistency with is_uniform_range()).
fu! sexp#range_has_non_ws(beg, end, check_ignored)
    let ret = 0
    let save_cursor = getcurpos()
    try
        call s:setcursor(a:beg)
        let pos = searchpos('\S', 'nczW', a:end[1])
        let ret = pos[0] && sexp#compare_pos([0, pos[0], pos[1], 0], a:end) <= 0
        if !ret && a:check_ignored
            " No true non-ws, but check for "ignored" ws, which counts as the same thing...
            let pos = searchpos('\s', 'nczW', a:end[1], 0, s:nomatch_ignored_region_fn)
            " Return true iff we found ignored ws within region.
            let ret = pos[0] && sexp#compare_pos([0, pos[0], pos[1], 0], a:end) <= 0
        endif
        return ret
    finally
        call s:setcursor(save_cursor)
    endtry
endfu

" Return true iff both ends of the range are at the same level.
fu! sexp#is_uniform_range(rng)
    let save_cursor = getcurpos()
    try
        let open = [s:nullpos, s:nullpos]
        " Find *containing* open for each end.
        for i in range(2)
            call s:setcursor(a:rng[i])
            let open[i] = s:containing_bracket(0)
        endfor
        " Range is uniform if both ends are at same (possibly top) level.
        return !open[0][1] && !open[1][1] || open[0] == open[1]
    finally
        call s:setcursor(save_cursor)
    endtry
endfu

" Return true iff specified SexpPos is on whitespace (or blank if allow_blank).
" Note: See previous function comment for usage of 'check_ignored'.
fu! s:in_whitespace(pos, allow_blank, check_ignored)
    let save_cursor = getcurpos()
    try
        call s:setcursor(a:pos)
        " Anchor search at cursor for efficiency (in case line is long).
        " TODO: Consider different approach: e.g., grabbing and testing the char in lieu of
        " search().
        let re = '\v%.c\s' . (a:allow_blank ? '|^$' : '')
        let pos = searchpos(re, 'nczW', a:pos[1], 0, a:check_ignored ? s:match_ignored_region_fn : '')
        let ret = pos[0] && pos == save_cursor[1:2]
        return ret
    finally
        call s:setcursor(save_cursor)
    endtry
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
    if s[1] && sexp#compare_pos(s, a:end) <= 0
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
    return sexp#invoke('is_rgn_type', a:rgn, a:line, a:col)
endfu

""" CURSOR MOVEMENT {{{1

" Calls cursor(pos[1], pos[2]). Used in favor of setpos(), which is lower
" level than cursor(), omitting some UI niceties.
" TODO: Re-examine this rationale...
function! s:setcursor(pos)
    keepjumps call cursor(a:pos[1], a:pos[2])
endfunction

" TODO: Remove...
function! s:move_char(dir)
    let pos = sexp#offset_char(getpos('.'), a:dir)
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
function! sexp#move_to_current_element_terminal(closing)
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
" Optional Arg: If optional 'ignore_current' flag is set and there's no adjacent element,
" return nullpos without moving cursor.
" Renaming Note: This function has been renamed from s:move_to_adjacent_element() to make
" it accessible to the sexp#parse module; it would have been named
" sexp#move_to_adjacent_element, but that name was already taken by the API method invoked
" from the plugin script.
" TODO: Put API autoload functions in a sexp#api module so that the sexp module can be
" reserved for internal functionality; alternatively, put the api in sexp and the internal
" stuff in sexp#nav or sexp#impl or somesuch...
function! sexp#move_to_adjacent_element_terminal(next, tail, top, ...)
    let cursor = getpos('.')
    let ignore_current = a:0 && a:1
    try
        if a:top
            let top = s:move_to_top_bracket(a:next)

            " Stop at current top element head if moving backward and did not
            " start on a top element head.
            if !a:next && top[1] > 0 && sexp#compare_pos(top, cursor) != 0
                let pos = top
                return
            endif
        endif
        " Note: We fall through here from top case iff not stopping at current top head.
        let pos = sexp#nearest_element_terminal(
            \ a:next, a:tail, ignore_current, ignore_current)
    finally
        call s:setcursor(pos[1] ? pos : cursor)
        return pos
    endtry
endfunction

" Move cursor to pos, and then to the next element if in whitespace.
" FIXME: Blank line not treated like whitespace.
function! s:move_to_element_near_position(pos)
    call s:setcursor(a:pos)
    return getline('.')[col('.') - 1] =~# '\v\s'
           \ ? sexp#move_to_adjacent_element_terminal(1, 0, 0)
           \ : a:pos
endfunction

" Extend current selection by moving the cursor to position returned by
" evaluating func with given varargs. Detects which end of the selection
" should be extended.
function! s:move_cursor_extending_selection(func, ...)
    " Break out of visual mode, preserving cursor position
    if s:countindex > 0
        sexp#ensure_normal_mode()
    endif

    let [start, end] = s:get_visual_marks()
    let omode = sexp#compare_pos(start, getpos('.')) == 0

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
            keepjumps call cursor(l, c)
        endif
        return [0, l, c, 0]
    elseif a:mode ==? 'o' && getline('.')[col('.') - 1] =~# s:bracket
        let cursor = getpos('.')
        let bracket = getline(cursor[1])[cursor[2] - 1]

        call sexp#move_to_current_element_terminal(1)
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

" Calls sexp#move_to_adjacent_element_terminal count times, with the following additional
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
        return sexp#docount(a:count, 'sexp#move_to_adjacent_element_terminal',
            \ a:next, a:tail, a:top)
    elseif a:mode ==? 'v'
        return sexp#docount(a:count, 's:move_cursor_extending_selection',
            \ 'sexp#move_to_adjacent_element_terminal', a:next, a:tail, a:top)
    elseif a:mode ==? 'o'
        let cursor = getpos('.')
        call sexp#docount(a:count, 'sexp#move_to_adjacent_element_terminal',
            \ a:next, a:tail, a:top)
        let pos = getpos('.')
        let nomove = sexp#compare_pos(cursor, pos) == 0

        " Bail out if the cursor has not moved and is resting on a delimiter
        if nomove && getline(pos[1])[pos[2] - 1] =~ s:delimiter
            return 0
        " Inclusive when:
        "   * Moving to tail
        "   * Moving forward to head but ending on tail because we are bounded
        "   * Same as above, but element is a single character so head == tail
        elseif a:tail
            \ || (a:next && sexp#compare_pos(pos, sexp#current_element_terminal(0)) != 0)
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
        let pos = sexp#move_to_current_element_terminal(a:next)
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
            call sexp#move_to_current_element_terminal(a:next)
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

if s:can_set_visual_marks && s:use_setpos_for_visual_marks
    " Set visual marks to [start, end]
    " Important Caveat: I'm beginning to think the original, pre-7.3.590 approach may have
    " been better. The problem with using '< and '> with setpos() is that it doesn't seem
    " to have *enough* side-effects: specifically, if you're already in visual mode, it
    " doesn't update the visual selection immediately. Through trial-and-error, I've
    " discovered a workaround: make sure there's a transition *into* visual mode following
    " the set of the visual marks. However, there appear to be many dark corners in Vim's
    " visual selection logic, which it's probably best not to hang out in any more than
    " necessary...
    " Design Decision: Remain in whatever mode we were called in and if we're not
    " remaining in visual mode, restore cursor position. Note that this behavior differs
    " from the legacy version of set_visual_marks(), which always ends up in normal mode.
    function! s:set_visual_marks(marks)
        " See note in header for explanation.
        let in_visual = s:in_visual_mode()
        if in_visual
            " Exit visual mode before setting marks.
            exe "normal! \<Esc>"
        else
            " We'll want to restore cursor position if we're not in visual mode.
            let save_cursor = getpos('.')
        endif
        let x = setpos("'<", a:marks[0])
        let x = setpos("'>", a:marks[1])
        " Transition to visual mode to cause marks to take effect. (See note in header.)
        normal! gv
        if !in_visual
            " Restore mode and cursor to what they were upon entry.
            exe "normal! \<Esc>"
            call s:setcursor(save_cursor)
        endif
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
" brackets when either of the following conditions is met:
"
"   * Mode equals 'v', the visual selection is more than one column wide, and selecting
"     the requested inner/outer list would not expand the visual selection.
"     Note: The *current* inner/outer list is determined by the *cursor side* of the
"     visual selection; this matters when the selection crosses list boundaries (i.e.,
"     when the selection ends are not within the same sexp tree node).
"     Example: With selection given by < > and cursor by |...
"       <(foo (bar|>))
"     ...the (bar) list will be selected: i.e., this function doesn't call super_range().
"
"   * s:countindex is greater than 0 and the visual marks are valid.
"     Use Case: Counted expansion by sexp#docount().
"
" Visual Marks: Both are deleted if there's no list to select and mode does not equal 'v'.
" Rationale: Erase < > only to prevent operation (o-pending mode) on something that's not
" a list. I could see an argument for erasing visual marks even in visual mode, but this
" is the way it's always worked.
" Note: If the optimal inner/outer list doesn't exist, but a suitable fallback does, set
" both marks and the 'success' flag, but also set the 'stopiter' flag to allow a higher
" level loop to terminate early.
"
" Important Note: Originally, the first of the 2 expansion conditions listed above
" worked only if cursor was at the *start* of the visual selection. This approach stopped
" working when visual mode maps transitioned from using :<c-u> to <cmd>. The problem was
" that :<c-u>, unlike <cmd>, forcibly sets cursor position to the start of the visual
" region, whereas <cmd> leaves it unmodified. When visual mode mappings started using
" <cmd>, cursor position was left at the end of the visual selection, with the result that
" subsequent `af` commands didn't cause expansion (unless user hit `o` in between).
" Possible Solutions: One possible solution would be to modify the command wrapper to
" ensure that we always set cursor to the start of the visual region when executing a
" visual mode map. However, this feels like a kludge. A better solution is to implement
" the same expansion logic whether cursor is at start or end of visual selection. The
" current version of this function takes this approach.
" Return: Pair: [success, stopiter]
"         success:  1 if visual marks were set
"         stopiter: 1 if there's no point in continuing
function! s:set_marks_around_current_list(mode, offset, allow_expansion)
    " Save/restore cursor.
    let cursor = getpos('.')
    let [error, stopiter] = [0, 0]
    let visual = a:mode ==? 'v'
    try
        " Prepare the entrails
        let [vs, ve] = s:get_visual_marks()
        let counting = s:countindex > 0
        " Note: Historically, single-char visual selection did not permit expansion.
        " Rationale: Makes sense for non-list elements, as it would allow you to hit `v`
        " and execute an inner/outer object without worrying about whether the element is
        " single or multi-char. Not sure it makes as much sense for lists, but that's the
        " way it's always worked.
        let have_selection = vs[1] && ve[1] && sexp#compare_pos(vs, ve) != 0
        " Note: This flag being set doesn't guarantee expansion; it's just a more
        " restrictive version of the input flag.
        let expanding = a:allow_expansion && have_selection && (counting || visual)

        " To prevent unintentional changes to visual selection, make sure we're not in
        " visual mode before moving cursor.
        " Note: We'll be in visual mode at this point if we were invoked from the
        " sexp#docount loop.
        if mode() ==? 'v' | execute "normal! \<Esc>" | endif

        " Determine innermost containing list.
        let isl = s:is_list(cursor[1], cursor[2])
        if isl == 2
            " Cursor on open
            let [open, close] = [cursor, s:nearest_bracket(1)]
        elseif isl == 3
            " Cursor on close
            let [open, close] = [s:nearest_bracket(0), cursor]
        else
            " Not on list brackets, so look up.
            let open = s:nearest_bracket(0)
            let close = open[1] ? s:nearest_bracket(1) : s:nullpos
        endif
        if !open[1]
            " Not on or in a list; nothing to do.
            throw 'sexp-error'
        endif
        " Note: If we get here, we have something we can select, even if it's not optimal:
        " i.e., error flag should not be set below.

        if expanding && a:offset && (isl == 2 || isl == 3)
            " Cursor is on a list bracket and we're looking for an *inner* list, so look
            " higher.
            call s:setcursor(open)
            let p = s:move_to_nearest_bracket(0)
            if p[1]
                let [open, close] = [p, s:nearest_bracket(1)]
            else
                let stopiter = 1
            endif
        endif
        " Determine a *candidate* range: either the containing list or just inside it.
        " Design Decision: Don't inhibit inner selection here if stopiter was set above.
        " Rationale: Might as well make the selection inner as requested, even if we would
        " have preferred to expand: e.g., if on a toplevel list, hitting a sequence of
        " `af` and `if` should toggle between inner/outer.
        let s = a:offset ? sexp#offset_char(open, 1, 1) : open
        let e = a:offset ? sexp#offset_char(close, 0, 1) : close

        " Native object selections expand when repeating inner motions, so we do too.
        " Attempt expansion only if visual selection includes *all* of candidate range:
        " i.e., only if selecting the current target would not expand visual selection.
        if expanding && !stopiter
            \ && sexp#compare_pos(vs, s) <= 0 && sexp#compare_pos(ve, e) >= 0
            call s:setcursor(open)
            let p = s:move_to_nearest_bracket(0)
            if p[1]
                " Adjust selection.
                let [s, e] = [p, s:nearest_bracket(1)]
                if a:offset
                    let s = sexp#offset_char(s, 1, 1)
                    let e = sexp#offset_char(e, 0, 1)
                endif
            else
                let stopiter = 1
            endif
        endif

        if sexp#compare_pos(s, e) > 0
            " Special Case: Inner selection on adjacent brackets results in s being one
            " character past e. Handle by reversing range.
            let [s, e] = [e, s]
        endif
        call s:set_visual_marks([s, e])
    catch /sexp-error/
        let [error, stopiter] = [1, 1]
    finally
        " Don't erase marks when in visual mode. (See note in header.)
        if error && !visual
            delmarks < >
        endif
        if getpos('.') != cursor
            call s:setcursor(cursor)
        endif
        return [!error, stopiter]
    endtry
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


" Return range of sibling n elements away from pos. If requested element does not exist,
" 'missing' key will indicate how many siblings were lacking and range will be for the
" final element in desired direction.
" -- Args --
" pos:    starting position
" n:      desired number of steps from input pos
" tail:   direction of traversal
" Return Dict:
" remaining     0 if returned range corresponds to requested element, else number of
"               missing siblings
" range         [start, end] of element n steps from pos
function! s:nth_from(pos, n, tail)
    let cursor = getpos('.')
    let ret = {'missing': 0, 'range': [s:nullpos, s:nullpos]}
    try
        " Move to starting position.
        call s:setcursor(a:pos)
        if !a:n
            " Note: This probably represents internal error.
            let ret.range =
                \ [sexp#current_element_terminal(0), sexp#current_element_terminal(1)]
        else
            " Start on outside of terminal element and search inward.
            let cnt = a:n
            while cnt
                " Land on near side of elements.
                let p = sexp#move_to_adjacent_element_terminal(a:tail, !a:tail, 0, 1)
                if !p[1]
                    " We've gone as far as we can go.
                    break
                endif
                let cnt -= 1
            endwhile
            " We're positioned on near side of desired element (or if count was too high,
            " the final element in list in desired direction).
            let ret.range[!a:tail] = getpos('.')
            let ret.range[a:tail] = sexp#current_element_terminal(a:tail)
            let ret.missing = cnt
        endif
        return ret
    finally
        call s:setcursor(cursor)
    endtry
endfunction

" Return range of positions for the requested child of the current list, else nullpos
" pair. If optional 'top_is_list' arg is set and cursor is at toplevel, treat buffer
" itself as a list.
" -- Args --
" count:   1-based index of desired child, counted from 'tail' side of list
" tail:    side of list from which we look for [count]th element inward
" inner:   1 iff "inner element" is desired
" -- Optional Dict --
" top_is_list      Treat toplevel as list if cursor at toplevel
" list_info        If not empty or omitted, use to skip call to s:list_info()
" exact_count      If the requested child does not exist, return nullpos pair: i.e., don't
"                  simply return the terminal element.
" inner_only       pass-through to s:list_info()
" TODO: Add option that won't consider a list whose brackets/macros we're on.
" TODO: Probably accept position other than curpos.
function! s:child_range(count, tail, inner, ...)
    let cursor = getpos('.')
    let ret = {'range': [s:nullpos, s:nullpos], 'missing': a:count}
    try
        " Extract optional flags
        let top_is_list = a:0 ? get(a:1, 'top_is_list', 0) : 0
        let li = a:0 ? get(a:1, 'list_info', {}) : {}
        let exact_count = a:0 ? get(a:1, 'exact_count', 0) : 0
        let inner_only = a:0 ? get(a:1, 'inner_only', 0) : 0
        " Get relevant info about current list, including desired terminal.
        let li = empty(li) ? s:list_info(a:tail, {'inner_only': inner_only}) : li
        " Attempt to get non-null terminal range as starting point for search.
        " Caveat: Don't update return dict till desired child is found.
        let range = li.terminal_range
        if !range[0][1] && !li.brackets[0][1] && top_is_list
            " Check for buffer head/tail.
            let range = s:buffer_terminal(a:tail)
        endif
        " Do we have a starting point?
        if range[0][1]
            " Assumption: We're sitting on head/tail element.
            " If count > 1, find count-1'th adjacent element.
            let cnt = a:count ? a:count - 1 : 0
            if cnt
                " Start on outside of terminal element and search inward.
                let p = range[a:tail]
                call s:setcursor(p)
                while cnt
                    " Land on outside of elements.
                    let p = sexp#move_to_adjacent_element_terminal(!a:tail, a:tail, 0, 1)
                    if !p[1]
                        " We've gone as far as we can go.
                        break
                    endif
                    let cnt -= 1
                endwhile
                " We're positioned on outside of desired element (or if count was too
                " high, the terminal element in desired direction).
                if !cnt || !exact_count
                    " Assign to return dict.
                    let ret.range[a:tail] = getpos('.')
                    let ret.range[!a:tail] = sexp#current_element_terminal(!a:tail)
                endif
                let ret.missing = cnt
            else
                " Terminal is sought child.
                let [ret.range, ret.missing] = [range, 0]
            endif
            if !a:inner
                " Perform whitespace cleanup.
                let ret.range = s:terminals_with_whitespace(ret.range[0], ret.range[1])
            endif
        endif
        return ret
    finally
        call s:setcursor(cursor)
    endtry
endfunction

" Return 1 iff the most recent (possibly active) visual selection has cursor at end.
" Design Decision: If current command isn't visual, just default to cursor at end.
" Rationale: Sel direction is used only for expansions of a visual selection, and has
" little meaning outside that context. Moreover, determining sel dir may require
" re-entering the *last* visual mode, which for non visual commands, may take cursor
" to a different part of the buffer, thereby necessitating view save/restore! As long
" as we enter visual mode only for a visual command (and do so only immediately after
" command invocation), view should be unaffected.
function! s:get_sel_dir(mode)
    if a:mode !~ '[xv]'
        return 1
    endif
    try
        let enter_visual = !s:in_visual_mode()
        if enter_visual
            " Assumption: Because we're restoring visual sel active at instant command was
            " invoked, view should be unchanged (from the one at command invocation).
            let save_cursor = getpos('.')
            " Enter visual mode
            normal! gv
        endif
        " Use relative position of '.' and 'v' marks to determine whether cursor is at end
        " of visual sel.
        " Design Decision: Single-char selection considered at 'at end'.
        return sexp#compare_pos(getpos('.'), getpos('v')) >= 0
    finally
        if enter_visual
            call sexp#ensure_normal_mode()
            call setpos('.', save_cursor)
        endif
    endtry
endfunction

" Set visual marks to the start and end of the current element. If inner is 0, trailing or
" leading whitespace is included by way of s:terminals_with_whitespace().
" TODO: Update documentation to reflect changes in s:terminals_with_whitespace logic.
" TODO: Update this comment to reflect major changes with the handling of counts!!!
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
" Return: adjusted range, else null range
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
        let dir = b:sexp_cmd_cache.sel_dir
        " Rationalize visual range.
        " TODO: Now that super_range trims surrounding whitespace from selection,
        " optimizations could be added to subsequent logic.
        let [vs, ve] = s:super_range(vs_orig, ve_orig)
        "call s:Dbg("super_range returned %s %s", string(vs), string(ve))
        if !vs[1]
            " TODO: Is this the best way to handle? Is this all it can mean? Consider
            " using exception for this, and getting rid of this if.
            call sexp#warn#msg("Refusing to operate on selection containing unmatched parens!")
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
    let start = sexp#move_to_current_element_terminal(0)
    if !start[1]
        " We are on whitespace; check for next element
        let next = sexp#move_to_adjacent_element_terminal(1, 0, 0, 1)
        if !next[1]
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
    if sexp#compare_pos(leading, start) >= 0
        let leading = []
    endif

    " Position ourselves to look for (first) end, taking care to begin the search no
    " earlier than start, which could be *after* ve in certain corner cases.
    call s:setcursor(a:mode ==? 'v' && sexp#compare_pos(ve, start) > 0 ? ve : start)

    " Find first end, looking backwards if necessary.
    let end = sexp#current_element_terminal(1)
    if !end[1]
        " Weren't on an element. Get to end of previous (whose existence is implied by
        " existence of start).
        let end = sexp#move_to_adjacent_element_terminal(0, 1, 0)
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
            let p = sexp#move_to_adjacent_element_terminal(dir, dir, 0)
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
        call sexp#move_to_current_element_terminal(0)
    endif

    call sexp#move_to_adjacent_element_terminal(a:next, 0, 0)
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
            " TODO: Remove the keepjumps if I determine gv can't affect jumplist.
            keepjumps normal! gv
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
            " TODO: Remove the keepjumps if I determine o can't affect jumplist.
            keepjumps normal! o
            let cmp = sexp#compare_pos(getpos('.'), pos)
            if a:1 && cmp < 0 || !a:1 && cmp > 0
                " We were already on the desired end.
                keepjumps normal! o
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
" -- Optional Args --
" a:1  list_only - Set to inhibit fallback to element if no list.
" TODO: Consider reworking commands to set list_only in more cases.
" Rationale: I don't like the fact that something like daf will delete arbitrary,
" non-listy forms.
function! sexp#select_current_list(mode, offset, allow_expansion, ...)
    let list_only = a:0 && !!a:1
    let [success, stopiter] =
        \ s:set_marks_around_current_list(a:mode, a:offset, a:allow_expansion)
    if !success && !list_only
        " Fallback to current element (for historical reasons).
        " Hard-code 1 for inner.
        " Rationale: "inner" means something different for lists: i.e., even outer
        " doesn't select whitespace around the list, so if we're going to fall back to
        " current element, I think it should be inner, regardless of a:offset.
        let success = !!s:set_marks_around_current_element(a:mode, 1, 0, 0)[0][1]
    endif
    let success = success && s:select_current_marks(a:mode)
    if stopiter && get(s:, 'countindex', -1) >= 0
        " Don't continue sexp#docount() loop.
        throw "stop-iter"
    endif
    return success
endfunction

" Use this function in lieu of sexp#docount('sexp#select_current_list', ...) when you want
" early termination when the count is too large. The sexp#docount() call will just keep
" going unconditionally until the count is exhausted, which is problematic since at least
" one plugin option recommends setting a count to a large number (e.g., 1000) to select
" "top-level".
" Another difference is that this function doesn't fall back to current element when not
" within a list.
" Note: The allow_expansion arg is omitted, as this function would be pointless without
" expansion.
" Return: 1 if a list (not necessarily nth) has been selected, else 0
" TODO: Refactor of set_marks_around_current_list() has obviated the need for this. Remove
" after testing...
function! sexp#select_nth_list(count, mode, offset)
    try
        " Maintain prev range so we can detect when expansion stops.
        let [vs_, ve_] = [s:nullpos, s:nullpos]
        for i in range(a:count > 0 ? a:count : 1)
            " Note: Need to set the script-local counter used by sexp#docount because
            " s:set_marks_around_current_list()'s logic uses it to prevent expansion on
            " the first (0th) iteration.
            " TODO: This feels wrong and should probably be changed, but any change there
            " would entail significant re-validation, so tread carefully...
            let s:countindex = i
            " Set optional 'list_only' flag to prevent fallback to nearest element.
            if !sexp#select_current_list(a:mode, a:offset, 1, 1)
                " TODO: Decide how best to handle failure. Note that inability to expand
                " further is not considered failure, and is handled below.
                return 0
            endif
            " Did expansion occur?
            let [vs, ve] = s:get_visual_marks()
            if vs_ == vs
                " No expansion occurred! Short-circuit.
                break
            endif
            let [vs_, ve_] = [vs, ve]
        endfor
        return 1
    finally
        let s:countindex = -1
    endtry
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
    let cnt = a:0 && a:1 ? a:1 : 1
    call s:set_marks_around_current_element(a:mode, a:inner, cnt, 0)
    " TODO: Should this really be called unconditionally, given that
    " s:set_marks_around_current_element() may have failed and deleted < > marks?
    return s:select_current_marks(a:mode, a:mode ==? 'v' ? b:sexp_cmd_cache.sel_dir : 1)
endfunction

" Set visual marks around adjacent element and enter visual mode; 0 for
" previous, 1 for next. If no such adjacent element exists, selects current
" element.
function! sexp#select_adjacent_element(mode, next)
    call s:set_marks_around_adjacent_element(a:mode, a:next)
    return s:select_current_marks(a:mode)
endfunction

" Set visual marks around count'th inner/outer child of current (or parent) list; 0 to
" count backwards from tail, 1 to count forwards from head. If no such child element
" exists, selects closest one (i.e., last in the specified direction).
function! sexp#select_child(mode, count, tail, inner)
    let cr = s:child_range(a:count, a:tail, a:inner, {'exact_child': 0})
    let [s, e] = cr.range
    if s[1]
        " Note: Although we may be called from visual mode, child selection ignores
        " current selection by definition.
        call s:set_visual_marks([s, e])
        call s:select_current_marks(a:mode)
    endif
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

" Pass-through to sexp#parse autoload function, which wraps the call in a temporary parse
" buffer resource if and only if the 'needbuf' flag is set.
function! s:analyze_codestr_legacy(codestr, filetype, needbuf)
    if a:needbuf
        return sexp#parse#do_in_buf('sexp#parse#analyze_codestr', a:codestr, a:filetype,
            \ a:codestr, a:filetype)
    else
        " Assumption: Already inside the temporary parse buffer
        return sexp#parse#analyze_codestr(a:codestr, a:filetype)
    endif
endfunction

" Parse the input 'codestr' as 'filetype' and return a dictionary whose format is
" described by ts.lua.ParseResult.
" Intended Use Case: Use the returned dictionary to validate a register put.
function! s:analyze_codestr(codestr, filetype)
    " Note: The final boolean arg to analyze_codestr() has a slightly different meaning
    " for vim/lua:
    "   vim: true => need to create the temporary buffer
    "   lua: true => use string parser
    if !s:prefer_treesitter() || !g:sexp_regput_use_string_parser
        " Either we're using legacy syntax, which always uses a temporary buffer for
        " parsing, or we're using Treesitter and user hasn't set the flag requesting use
        " of a string parser. In either case, wrap the analyze_codestr invocation in a
        " function that manages the temporary buffer as an RAII resource (think Python
        " Context Manager).
        return sexp#parse#do_in_buf('sexp#invoke', a:codestr, a:filetype,
            \ 'analyze_codestr', a:codestr, a:filetype, v:false)
    else
        " Looks like a Treesitter string parse is in order; however, we still use
        " sexp#invoke() (rather than calling sexp.ts.analyze_codestr() directly) to permit
        " fallback in the event of Treesitter failure.
        return sexp#invoke('analyze_codestr', a:codestr, a:filetype, v:true)
    endif
endfunction

" Create and return a context dict with a superset of the keys required by the various
" regput modes, providing reasonable defaults where they exist, and giving precedence to
" any defaults provided by caller in optional override dict.
" Objective: Mitigate risk of error by ensuring all regput functions can rely on a
" consistent format.
" Note: Keys for which no reasonable default exists, and for which no initial value is
" provided, will be set to invalid values (e.g., nullpos).
function! s:regput__ctx_init(mode, put_mode, count, ...)
    let ctx = {
        \ 'mode': a:mode,
        \ 'put_mode': a:put_mode,
        \ 'count': a:count,
        \ 'curpos': getpos('.'),
        \ 'regname': v:register,
        \ 'backtick': getpos("'`"),
        \ 'P': -1,
        \ 'tail': -1,
        \ 'vrange': s:get_visual_marks(),
        \ 'orange': [getpos("'["), getpos("']")],
        \ 'range': [s:nullpos, s:nullpos],
        \ 'inner_range': [s:nullpos, s:nullpos],
        \ 'is_bra': [0, 0], 'is_ele': [0, 0],
        \ 'force_nl': [0, 0],
    \ }
    if a:0
        let ctx = extend(ctx, a:1)
    endif
    return ctx
endfunction

" A simplistic version of analyze_codestr(), which performs some rudimentary regex
" matching in lieu of a full parse. It is used as fallback if the Treesitter/legacy parse
" fails *or* user has disabled register parsing via option.
" Return: See sexp#parse#analyze_codestr() or sexp.ts.ParseResult.
function! s:analyze_codestr_simple(codestr, filetype)
    " Note: Processed text added later.
    " TODO: Some of these fields won't be set properly until we're creating a hidden
    " buffer in which to perform the analysis.
    let ret = {
        \ 'elem_count': 1,
        \ 'is_ml': 0, 'linewise': 0,
        \ 'has_com': 0, 's_is_com': 0, 'e_is_com': 0,
        \ 'node_ranges': [],
        \ 'err_loc': s:nullpos, 'err_hint': '',
        \ 'text': '',
        \ 'linewise': g:sexp_regput_untrimmed_is_linewise
            \ ? a:codestr =~ '^\s\|\s$'
            \ : a:codestr =~ '\n$'
    \ }
    let ret.s_is_com = a:codestr =~ '^\s*;'
    let ret.e_is_com = a:codestr =~ ';.*$'
    " TODO: Come up with less simple patterns that at least attempt to differentiate
    " between string and non-string context.
    " Flag indicating whether register is *only* a (potentially multi-line) comment.
    let ret.has_com = a:codestr =~ ';'
    " TODO: Decide whether this should be single line only, given use case.
    " TODO: If this is not needed, remove!
    let ret.is_com = a:codestr =~ '\(\_^\s*;.*\_$\)\+'
    " Trim *all* whitespace at both ends since we're adding deterministic amount.
    " FIXME: Needs to account for possibility of escaped whitespace; have this done by
    " function that performs language-specific validation.
    let ret.text = substitute(a:codestr, '^\_s\+\|\_s\+$', '', 'g')
    " Design Decision: Stripped leading/trailing whitespace should not influence
    " multline test.
    " Caveat: Vim =~ operator treats rhs like a single line; thus, literal NL ("\n") (not
    " '\n') must be used to match interior NLs. Also, ^ and $ work only at beginning/end
    " of multiline string.
    let ret.is_ml = ret.text =~ "\n"
    return ret
endfunction

" Return 'curpos' option value that applies to the specified put_mode.
" TODO: Consider whether curpos_child should inherit from curpos if user has set only the
" latter.
function! s:regput__get_curpos_opt(put_mode)
    " Default values
    let optdefs = {
        \ 'sexp_regput_curpos': 0,
        \ 'sexp_regput_curpos_child': 0,
        \ 'sexp_regput_curpos_op': 2
    \ }
    " Get applicable option name.
    let optname = a:put_mode =~ 'op'
        \ ? 'sexp_regput_curpos_op'
        \ : a:put_mode == 'put_child'
            \ ? 'sexp_regput_curpos_child'
            \ : 'sexp_regput_curpos'
    " Get and validate the applicable option value.
    let ret = get(g:, optname, optname)
    if !(type(ret) == type(0) && ret >=0 && ret <= 2)
        " User provided invalid value!
        let def = get(optdefs, optname)
        call sexp#warn#msg(
            \ printf("sexp-warning: Ignoring invalid option setting: %s=%s."
                \ . " Defaulting to %d",
                \ optname, string(ret), def)
            \ {'once': optname})
        let ret = def
    endif
    return ret
endfu

" Build and return a dict characterizing the text in the put register and canonicalize the
" register contents. Use Treesitter if applicable, falling back to legacy syntax
" highlighting, and even to simplistic regex-based parser if neither Treesitter nor legacy
" syntax is usable. When deciding how to handle invalid lisp in register, consider
" relevant user options, warning user if appropriate and throwing "sexp-abort" if we
" cannot proceed.
" Args: None (register and filetype determined from Vim var/option)
" Return: A dict whose format can be found in the analyze_codestr() functions in both the
" sexp#parse autoload and sexp.ts Lua modules.
" Exceptions:
"   sexp-abort:  register contents cannot be parsed as valid code and user options (and
"                possibly user response to input()) prohibit proceeding with paste.
"                buffer's 'filetype
"   sexp-noop:   register is empty (or whitespace-only); no reason to continue with put
function! s:regput__get_reginfo(regname)
    let ret = {}
    let regstr = getreg(a:regname)
    if regstr =~ '^\s*$'
        " No-op!
        throw "sexp-noop"
    endif
    if !g:sexp_regput_inhibit_regparse
        " Attempt proper parse.
        let ret = s:analyze_codestr(regstr, &ft)
    endif
    if empty(ret)
        " Either something went wrong or register parsing is inhibited by option.
        " Assumption: The simpler fallback method will always return non-empty dict.
        let ret = s:analyze_codestr_simple(regstr, &ft)
    endif
    if ret.err_loc[1]
        " Register contents are not valid lisp. Decide how to handle...
        " Assumption: Can't get here if g:sexp_regput_inhibit_regparse disables parsing.
        let errmsg = printf("Error encountered at or near %s%s.",
            \ "(" . string(ret.err_loc[1:2]) . ")",
            \ !empty(ret.err_hint) ? " (Hint: " . ret.err_hint . ")" : "" )
        " Enumeration defined in help doc.
        let action = g:sexp_regput_invalid_register_action
        if action < 0
            " Defer continue/abort decision to user.
            let ans = ''
            while ans !~? '^\v\s*%(y%[es]|n%[o])\s*$'
                let ans = input("Warning: Register text does not appear to be valid lisp:\n"
                    \ . errmsg
                    \ . "\nPaste anyways? y(es)/n(o)")
            endwhile
            let proceed = ans =~ '^\s*y'
        else
            let proceed = action >= 1
        endif
        " Warn if action requires warning and we haven't already presented prompt to user.
        if action == 0 || action == 1
            " Display warning!
            call sexp#warn#msg("Warning: Paste register contains invalid form(s): "
                \ . errmsg)
        endif
        if !proceed
            " Abort!
            throw "sexp-abort"
        endif
    endif
    return ret
endfunction

" Build and return dict characterizing the context in which a register put occurs. To
" minimize duplication across the various put commands, this function expects a tgt dict,
" which characterizes the put target slightly differently, as a function of put type.
" Since this tgt dict is simply merged into the dict returned by this shared function, its
" format is described here.
" Args:
"   tgt_info:        dict containing information on the target of the put, which is simply
"                    merged into the dict returned by this function.
" Return Dict:
"   Note: List keys are pairs whose indices correspond to the elements of range[].
"   --[[ Beginning of merged-in tgt_info dict
"   count:           [count] associated with the operation
"   put_mode:        one of the following:
"                    'put', 'put_child', 'put_op', 'put_op_tele',
"                    'replace', 'replace_op', 'replace_op_tele'
"   tail:            At the time of command invocation, 'tail' indicates either the put
"                    *direction* (non-child put) or the *side* of the list from which the
"                    insert location is calculated (child put). At this point, however,
"                    this flag has roughly the same meaning for both put classes, since in
"                    the child put modes, we've already used the "side" to determine range.
"   P:               (replace modes only) 1 iff command variant inhibits update of unnamed
"                    register
"   vrange[]:        position pair specifying pre-op visual range (used for restore)
"   range[]:         for non-replace modes, a position pair specifying exclusive range for
"                    the put; for replace modes, this range is used by positioning logic,
"                    but the replacement splice itself uses the inclusive inner_range[].
"   inner_range[]:   (replace modes only) inclusive range for the replacement splice
"   is_bra[]:        1 iff corresponding end of range is bracket
"   is_ele[]:        1 iff coreesponding end of range is element
"   tail:            Meaning varies slightly across put modes, but in some sense, this
"                    value determines direction of put. Can be adjusted by special case
"                    logic; hence, its inclusion here.
"                    FIXME: I don't like setting this to 0/1 for a mode like 'replace',
"                    for which it has no meaning: make sure it can be set to -1 safely.
"   force_nl[]:      1 iff subsequent processing should force use of NL separator.
"                    Currently, used only for a normal (targeted) put when cursor is not
"                    *on* an element, but just before or after it.
"   --]] End of merged-in tgt_info Dict
"   empty_list:      putting into empty list
"   empty_buffer:    putting into empty buffer
"   -- Flags: The following flag pairs are N/A if either of empty_{list,buffer} set.
"   alone[]:         1 iff element at corresponding range endpoint is alone on its line,
"                    with possible exception of open or close, but not both
"   colinear[]:      1 iff element at corresponding range endpoint is colinear with its
"                    adjacent element (in the direction of the other range endpoint). For
"                    non-replacement put modes, both values in the pair will be identical,
"                    since the same 2 elements are used for both comparisons; for
"                    replacement put modes, however, the values can differ because each
"                    comparison involves a range endpoint and its nearest inner range
"                    endpoint.
"                    Design Decision: Only elements (not brackets) can be colinear.
" Cursor Preservation: Caller handles.
function! s:regput__get_context(tgt_info)
    " Merge target information into return dict.
    let ret = extend({
        \ 'alone': [0, 0], 'colinear': [0, 0],
        \ 'empty_list': 0, 'empty_buffer': 0,
    \ }, a:tgt_info)

    " Note: is_bra[] and is_ele[] have been merged into ret from input a:tgt.
    let ret.empty_list = ret.is_bra[0] && ret.is_bra[1]
    let ret.empty_buffer = !ret.is_ele[0] && !ret.is_ele[1] && !ret.empty_list

    " Set colinear[] and alone[] flag pairs.
    " Note: Flags are N/A for empty buffer/list special cases.
    if !ret.empty_buffer && !ret.empty_list
        " Set side-specific 'alone' flags in loop.
        for i in range(2)
            if ret.put_mode =~ 'replace'
                " Cache range comprising the current outer range endpoint and the inner
                " range endpoint closest to it.
                let range = i
                    \ ? [ret.inner_range[1], ret.range[1]]
                    \ : [ret.range[0], ret.inner_range[0]]
                let is_ele = i ? [1, ret.is_ele[1]] : [ret.is_ele[0], 1]
                let is_bra = i ? [0, ret.is_bra[1]] : [ret.is_bra[0], 0]
            else " put_mode in ['put', 'put_child']
                let range = ret.range
                let [is_ele, is_bra] = [ret.is_ele, ret.is_bra]
            endif
            " Note: For non-replace modes, colinear[] is effectively a scalar, but this
            " assignment is kept within the loop because in the 'replace' modes, is_ele[]
            " can change from one iteration to the next.
            let ret.colinear[i] = is_ele[0] && is_ele[1] && range[0][1] == range[1][1]

            " 'outer' refers to the element or bracket just outside the range on current
            " side. May be nullpos if current range endpoint is at buffer extremity.
            let [outer, outer_is_bra] = [s:nullpos, 0]
            if !ret.is_bra[i]
                " No point in looking for adjacent or containing bracket if at BOF.
                " TODO: Do we also need an EOF check?
                if i || ret.range[i] != s:BOF
                    " Attempt to find outer adjacent.
                    call s:setcursor(ret.range[i])
                    let outer = sexp#nearest_element_terminal(i, !i, 1, 1)
                    if !outer[1]
                        " Pre-position on outside of element to ensure that if element is
                        " list, nearest_bracket doesn't find its match.
                        call sexp#move_to_current_element_terminal(i)
                        let outer = s:nearest_bracket(i)
                        if outer[1]
                            let outer_is_bra = 1
                        endif
                    endif
                endif
            endif
            let ret.alone[i] = is_ele[i]
                \ && (!is_ele[!i] || range[0][1] != range[1][1])
                \ && (outer_is_bra || !outer[1] || outer[1] != range[i][1])
                \ && (!outer_is_bra || !is_bra[!i] || outer[1] != range[!i][1])
        endfor
    endif
    return ret
endfunction

" If context indicates a put into first, second or third position of a list satisfying all
" of the conditions required for the list position special case (see below), return index
" indicating the 1-based insert position, else return 0.
" List Position Special Case Conditions: (all must be met)
" * First element is single-line (most likely name of function).
" * First and second element are colinear.
" * Third element *not* colinear with second element.
" * (optional) List is not a 'let' form.
"   TODO: The 'reg' parameter, currently unused, was added to support this.
" Rationale: This function is used to determine whether calculated separators may require
" modification to ensure we don't destroy the following pattern at the start of a function
" call: '(' <func-name> <arg1> '\n'
function! s:regput__check_list_context(ctx, reg)
    let [ctx, reg] = [a:ctx, a:reg]
    let [sidx, ps] = [0, []]
    if !ctx.is_bra[0]
        if !ctx.is_ele[0]
            " Shouldn't happen, but just to be safe...
            return
        endif
        " Traverse preceding siblings till either hitting an open bracket or visiting a
        " number of siblings that precludes possibility of start of range being within
        " first three elements of list. After exiting loop, sidx will represent 1-based
        " child index for start of range.
        let sidx = 1
        call s:setcursor(ctx.range[0])
        " Add start of range element to list of ranges.
        call add(ps, [sexp#current_element_terminal(0), ctx.range[0]])
        while sidx <= 2
            let prev_e = sexp#move_to_adjacent_element_terminal(0, 1, 0, 1)
            if prev_e[1]
                call add(ps, [sexp#current_element_terminal(0), prev_e])
                let sidx += 1
            else
                " FIXME: Check for `(' and return 0 if it's not what we hit!!!
                break
            endif
        endwhile
        if sidx > 2
            " Start of range was not within 1st 2 elements of list (and possibly not even
            " within list).
            return 0
        endif
        " Since list was built moving backwards...
        call reverse(ps)
    endif
    " Arrival here implies start within first two elements of list.
    if !ctx.is_bra[1]
        " Find last element of interest.
        let eidx = sidx + 1
        call s:setcursor(ctx.range[1])
        call add(ps, [ctx.range[1], sexp#current_element_terminal(1)])
        while eidx < 3
            let next_s = sexp#move_to_adjacent_element_terminal(1, 0, 0, 1)
            if next_s[1]
                call add(ps, [next_s, sexp#current_element_terminal(1)])
                let eidx += 1
            else
                break
            endif
        endwhile
    else
        " Start of range was tail element of list.
        let eidx = sidx
    endif

    " Special case requires at least 3 elements.
    " Rationale: We're looking for a list "shape" defined by 3 elements:
    "   (func arg1 arg2 NL arg3 ...)
    " FIXME: Need to consider bracket type as well: only lists with ( ) should qualify.
    if eidx >= 3
        " TODO: Use s:is_list() and regex to test for (let* [ ...) here...
        " Cache some useful vars pertaining to the first 3 elements of list.
        let [s1, e1] = ps[0]
        let [s2, e2] = ps[1]
        let [s3, e3] = ps[2]
        " Design Decision: Special case applies only to single-line head elements.
        let colinear12 = s1[1] == e1[1] && e1[1] == s2[1]
        let colinear23 = e2[1] == s3[1]
        if colinear12 && !colinear23
            if sidx == 0
                " Inserting to head position.
                " Design Decision: Don't treat as special case.
                " Rationale: Handling would entail inserting a line break after the
                " current head element, which can't be accomplished with separators around
                " the put text. Keep it simple. Users don't often paste the name of a
                " function anyways...
                return 1
            elseif sidx == 1
                return 2
            elseif sidx == 2
                return 3
            endif
        endif
    endif
    " Nothing special.
    return 0
endfunction

" Return a list containing 4 types of separators required for the put indicated by the
" inputs.
" Args:
"   ctx:  put context dict
"   reg:  register characterization dict
" Return List:
"   [0]:  separator needed before put text
"   [1]:  separator needed after put text
"   [2]:  separator used to join the individual instances of register contents when
"         [count] > 1 is used.
"   [3]:  1st interior separator, left as empty string if all separators should be the
"         same
function! s:regput__get_seps(ctx, reg)
    " Default start/end sep to NL; overrides below.
    let [ctx, reg] = [a:ctx, a:reg]
    let tail = ctx.tail
    let ret = [s:NL, s:NL, s:NL, '']
    let force_ml = g:sexp_regput_linewise_forces_multiline && reg.linewise
    " Determine whether put is into one of the special slots in a 'shaped' list.
    " Note: Inhibit this special case logic if more than one toplevel element in register.
    " Also, inhibit for replace modes, in which case, we have an existing shape we don't
    " really want to override.
    " Rationale: For the special logic to be meaningful in the multiple toplevel element
    " case, we'd need not only to parse the register text (already doing so), but also
    " potentially *modify* the whitespace between elements, which is analogous to the
    " 'interior separators' used for [count] > 1.
    let lpi = g:sexp_regput_ignore_list_shape || ctx.put_mode =~ 'replace'
        \ || reg.elem_count > 1 || ctx.empty_buffer || ctx.empty_list
        \ ? 0 : s:regput__check_list_context(ctx, reg)
    " Override the NL separators as needed.
    if ctx.empty_buffer
        let ret[0:1] = [s:EMPTY, s:EMPTY]
    else
        for i in range(2)
            " Compute side-dependent 'want_spc' flag used to determine SPC-insertion at both
            " start/end.
            " Note: These flags do not incorporate comment checks, which are applied in
            " separate step.
            " Note: There's a slight asymmetry between near/far side of range, but only for
            " put modes that differentiate between tgt and adjacent (i.e., directional puts).
            " In these modes, the 'force_nl' flag is set only on the target side, and the
            " side-dependent colinearity check reflects our unwillingness to make put text
            " colinear with adj unless adj was already colinear with target.
            " Note: Interior sep(s) are handled by separate logic after the loop.
            if lpi == 2
                " Slot 2 needs a leading SPC and a trailing NL (the latter of which is
                " requested implicitly by clearing this flag).
                let want_spc = !force_ml && !i
            elseif lpi == 3 && !i
                " Slot 3 needs a NL at the leading side. The !i guard condition ensures
                " that normal (else) logic will apply to trailing side.
                let want_spc = 0
            else
                " TODO: Decide on elem_count criterion.
                let want_spc = !force_ml
                    \ && ctx.is_ele[i] && !ctx.alone[i] && !ctx.force_nl[i]
                    \ && !reg.is_ml && reg.elem_count <= 1
                    \ && (tail == -1 || (i != tail || ctx.colinear[i]))
            endif
            " Override NL (if necessary).
            " Note: Special logic for replace mode puts may subsequently override this.
            " TODO: Change {e,s}_is_com to is_com[] to obviate need for this.
            let is_com = i ? reg.e_is_com : reg.s_is_com
            let allow_com_append = g:sexp_regput_allow_comment_append == 2
                \ || g:sexp_regput_allow_comment_append == 1 && !reg.linewise
            if ctx.is_bra[i]
                " No override for post sep if reg ends in comment.
                if !is_com || !i
                    let ret[i] = is_com ? s:SPC : s:EMPTY
                endif
            elseif !ctx.is_ele[i]
                " No bracket or element requiring separation on this side
                let ret[i] = s:EMPTY
            elseif want_spc && (i == 0 && allow_com_append || !is_com)
                let ret[i] = s:SPC
            endif
            if ctx.put_mode =~ 'replace'
                " Replace Mode Separator Adjustment: Inhibit separator unless it's a NL
                " and we're currently colinear at this end.
                " Rationale: Ideally, the put text would *exactly* replace the existing
                " text (delineated by ctx.inner_range[]), thereby minimizing visual
                " disturbance; however, if NL sep has been requested on a side whose range
                " endpoint is currently colinear with its nearest innner_range endpoint,
                " we need to leave the NL request intact to ensure separation.
                if ret[i] != s:NL || ctx.range[i][1] != ctx.inner_range[i][1]
                    let ret[i] = s:EMPTY
                endif
            endif
        endfor
    endif

    " -- Interior separator
    " Note: We don't need to know count yet, since interior_sep is ignored if [count]=1.
    " Separate the [count] register copies from each other with NL if any of the following
    " conditions are met:
    " * register contents multiline
    "   Rationale: Stacking many multiline forms horizontally could be problematic.
    " * register *contains* comment
    "   Rationale: For multiline register, previous condition makes this one irrelevant,
    "   but we definitely don't want multiple single line comments placed on the same
    "   line.
    " * register contains multiple toplevel forms
    " * put is directional (tail != -1) and near side sep is NL
    "   *OR* putting forward into slot 2 and 
    "   Rationale: Testing has shown that failing to separate the clones from each other
    "   when the entire put was separated from the target violates POLS: i.e., extending
    "   the NL insertion down through the clones just feels right, especially when putting
    "   forward from head.
    " * (optional) register contains single toplevel form whose length (or possibly length
    "   x count) exceeds configurable threshold.
    " TODO: Add the textwidth condition after working out the details and adding option.
    " Hmmm: This condition doesn't make quite as much sense now that I've re-added the
    " previous one, with which it would be redundant when near side sep is NL. I suppose
    " it would still make sense to keep it for the case in which we're putting onto the
    " same line as target.
    let ret[2] = reg.is_ml || reg.has_com || reg.elem_count > 1
        \ || (tail != -1 && (lpi == 2 && tail || ret[!tail] == s:NL)) ? s:NL : s:SPC
    " Provide distinct separator for 1st interior separator if applicable.
    " Note: This element will be N/A if [count] == 1
    let ret[3] = lpi == 2 ? s:NL : ''
    return ret
endfunction

" Starting at current position, look for the outside terminal position of the most distant
" sibling that can be reached without crossing line break between elements, returning a
" pair of the following form:
"   [<sought-pos>, {
"       next:   near end of subsequent sibling, else nullpos
"       brkt:   enclosing bracket if next == nullpos and not toplevel, else nullpos
"   }]
" Note: The point of this return structure is to make it easy for caller to ignore all but
" the sought pos if desired.
" TODO: This probably doesn't belong in this section...
function! s:last_colinear_sibling(tail)
    let curpos = getpos('.')
    let [pos, next, brkt] = [curpos, s:nullpos, s:nullpos]
    try
        " At loop termination, pos will be the position we're seeking.
        while 1
            let next = sexp#move_to_adjacent_element_terminal(a:tail, a:tail, 0, 1)
            if !next[1]
                " No more elements. Check for enclosing bracket in desired direction.
                let brkt = s:nearest_bracket(a:tail)
                break
            else
                if next[1] > pos[1]
                    " Found non-colinear sibling; use prior element.
                    break
                endif
                " Move to other end and continue.
                let next = sexp#move_to_current_element_terminal(a:tail)
            endif
            let pos = next
        endwhile
    finally
        call s:setcursor(curpos)
        return [pos, {'next': next, 'brkt': brkt}]
    endtry
endfunction

" Build and return string consisting of count copies of text, wrapped and joined with the
" separators in the provided list (whose layout is documented in header of
" put__get_seps().
function! s:build_splice_string(text, count, sep)
    let ret = a:text
    if a:count > 1
        " Append first interior separator, which *may* be different from the rest.
        let ret .= !empty(a:sep[3]) ? a:sep[3] : a:sep[2]
        " Append any remaining copies with the normal interior separator.
        " Note: Sep won't be used if a:count - 1 == 1.
        let ret .= join(repeat([a:text], a:count - 1), a:sep[2])
    endif
    " Apply outer separators.
    return a:sep[0] . ret . a:sep[1]
endfunction

" Return a dict that characterizes the splice (or directed put) to be performed as the set
" of arguments to s:yankdel_range().
function! s:regput__get_splice_info(count, ctx, reg, sep, flags)
    let ret = {
        \ 'range': a:ctx.put_mode =~ 'replace' ? a:ctx.inner_range : a:ctx.range,
        \ 'text': '', 'inc': [0, 0]
    \ }
    let [ctx, reg, sep] = [a:ctx, a:reg, a:sep]
    let tail = ctx.tail
    " TODO: Perhaps these flags should be added to ctx for convenience.
    let [is_repl, has_tgt] = [ctx.put_mode =~ 'replace', ctx.put_mode =~ 'put']

    " No range/inclusivity adjustments required in empty buffer case.
    if !ctx.empty_buffer
        " TODO: Decide whether/how tail matters for replace/child put modes.
        if !is_repl && tail != -1 && sep[tail] == s:NL
            " Determine number of newlines needed between put text and adjacent as a
            " function of the number of blank lines *currently* between target and
            " adjacent (taking options into account).
            " Design Decision: Configuration-controlled preservation of existing line gap
            " applies only on the *far* side of the put text.
            " Question: Should we preserve trailing whitespace in colinear case too? (I'm
            " thinking of trailing comments with lots of aligning space... OTOH, we have
            " no way of knowing how much is right, so probably just rely on subsequent
            " re-align (auto or explicitly-commanded)...
            let gap = ret.range[1][1] - ret.range[0][1]
            " Note: Because relevant sep has been determined to be NL, we must ensure a
            " min gap of 1, even if tgt and adj are currently colinear.
            " Question: Does num_nl need to take exc_typ into account?
            let num_nl = min([max([gap, 1]), g:sexp_cleanup_keep_empty_lines + 1])
            let sep[tail] = repeat("\n", num_nl)
        endif
        if is_repl
            " For replacements, ctx.inner_range is inclusive.
            let ret.inc = [1, 1]
        else
            " If necessary, adjust exclusion type for range end.
            " Logic: Always use adjacent whitespace-exclusive mode at end of range when the
            " range terminal is a bracket or element, which is not *currently* colinear with
            " start (i.e., there's leading indent at end), and we're inserting at least 1 NL
            " at end.
            " Rationale: Permits earlier "clean point" for re-indent; discarding the leading
            " indent would require inclusion of one extra element at end (tgt if !tail, adj if
            " tail).
            " Note: Can't use adj_colinear flag because we care about colinearity of brackets
            " too, and adj_colinear implies both tgt/adj are elements.
            let exc_typ = 0
            if (ctx.is_bra[1] || ctx.is_ele[1]) && ctx.range[0] != ctx.range[1]
                \ && sep[1][0] == s:NL
                let exc_typ = 2
            endif
            let ret.inc = [0, exc_typ]
        endif
    endif
    let ret.text = s:build_splice_string(reg.text, a:count, sep)
    return ret
endfunction

" Calculate and return a dict indicating the following (assuming regput operation has just
" completed):
"   curpos:   desired cursor position (applies only to non-visual commands)
"   orange:   the operated on range: '[ to '] minus any surrounding whitespace
"   irange:   the re-indent range (i.e., range provided to s:post_op_reindent())
" Pre Condition: '[ and '] delineate the put text (including any whitespace separators).
function! s:regput__postop(ctx, sep, orig_range)
    " Initialize return dict to values that may subsequently be overridden.
    let ret = {
        \ 'curpos': a:ctx.curpos,
        \ 'orange': [getpos("'["), getpos("']")],
        \ 'irange': [getpos("'["), getpos("']")]}
    " Adjust both ends of operated-on range to exclude non-ignored whitespace.
    " Assumption: Upstream short-circuit precludes possibility of empty or whitespace-only
    " put, thereby guaranteeing success of subsequent searches for non-ws.
    for i in range(2)
        " Move past any surrounding whitespace separators to non-ws.
        if a:sep[i] != s:EMPTY
            call s:setcursor(ret.orange[i])
            " Note: Upstream logic guarantees this will succeed.
            call searchpos('\S', (i ? 'b' : '') . 'cW')
            " Make sure we're not on ignored whitespace.
            let ret.orange[i] = sexp#current_element_terminal(i)
        endif
    endfor
    " If put mode-specific option requests curpos target of head or tail, make adjustment.
    let opt = s:regput__get_curpos_opt(a:ctx.put_mode)
    let idx = !!opt
    if opt == idx
        " Desired position is 0 (head) or 1 (tail).
        " Caveat: Copy the position to prevent double-adjustment if caller adds both these
        " positions to an auto-adjustment list.
        let ret.curpos = ret.orange[idx][:]
    endif
    " If necessary, adjust end of re-indent range.
    " Assumption: Indent logic always looks back to non-empty line preceding start, so
    " there's no need for start adjustment.
    " Special Case: If ele/bra following put text was originally colinear with ele/bra on
    " other side, adjust reindent range to include ele/bra past put text.
    " Rationale: s:post_op_reindent() assumes a NL creates a 'clean point', but this is
    " true only for *pre-existing* NLs, not ones inserted by the put! For non-colinear
    " tgt/adj, logic in put__get_splice_info() ensures use of adjacent
    " whitespace-exclusive mode to preserve leading indent for element past the put,
    " thereby obviating the need to include that element in the reindent. In the colinear
    " tgt/adj case, however, there can be no 'clean point' before whatever follows the
    " put, so we must include it.
    if a:orig_range[0][1] == a:orig_range[1][1]
        " Use adjusted range end.
        " Assumption: Re-indent logic will pull in the entire element automatically; no
        " need to find its end here.
        let ret.irange[1] = a:ctx.range[1]
    endif
    return ret
endfunction

" Perform register put for *all* put modes: i.e.,
"     put, put_op, put_op_tele, replace, replace_op, replace_op_tele, put_child
" After the put, implement mode/option-dependent logic for the following:
"   - visual mode restoration
"   - cursor positioning
"   - 'last jump' mark update
"
" -- Latest Jump Mark Logic --
" We can't use the s:defplug() mechanism to update 'last jump' mark because it sets the
" mark *prior to* the paste, *before* the adjusted position is known; thus, we use
" Def{oper,plug}N for the regput commands and update the mark here as appropriate.
" Conditional Logic: Set "latest jump" mark only for combinations of put_mode and 'curpos'
" option that tend to move cursor a significant distance. Actually, even the put
" before/after commands *can* move cursor significantly (e.g., if register contents are
" large and cursor is being placed at far side). But normal Vim put with gp doesn't update
" '` even for multiline puts and I've elected to treat the basic regput commands
" analogously. Treat only the put/replace *operators* and put child specially, though even
" with them, I'm thinking it might make sense to update '` only if movement exceeds a
" (configurable?) number of lines. For now, however, use minimum of 1 line, just as Vim
" does for / or ? searches.
function! s:regput__impl(tgt, count)
    try
        " Analyze the register.
        " Note: This function can throw sexp-abort and sexp-noop; in the case of
        " sexp-abort, its responsible for any associated warnings to user.
        " Caveat: Due to the indirect ways we can get here (e.g., using TextYankPost
        " autocmd), it's important that we use saved regname, not v:register.
        let reg = s:regput__get_reginfo(a:tgt.regname)
        let ctx = s:regput__get_context(a:tgt)
        let sep = s:regput__get_seps(ctx, reg)
        let spl = s:regput__get_splice_info(a:count, ctx, reg, sep, {})
        " TODO: Consider having put__get_context() build the position list.
        let ps = s:concat_positions(a:tgt.curpos, ctx.curpos, ctx.vrange, ctx.range)
        " Re-indent logic will need the original (unadjusted) range.
        " TODO: Consider having both ranges be part of context dict: e.g., range and
        " arange
        let orig_range = deepcopy(ctx.range)
        " Note: Set 'failsafe override' for put modes that can change non-ws.
        let repl_text = s:yankdel_range(spl.range[0], spl.range[1], spl.text, spl.inc, ps,
            \ ctx.put_mode =~ 'replace')
        " If not inhibited, update unnamed register.
        " TODO: Consider integrating this into yankdel_range(), which, in its current
        " form, *never* updates unnamed register (since it's been used exclusively for
        " things that shouldn't update any registers).
        if a:tgt.put_mode =~ 'replace' && !a:tgt.P
            let @" = repl_text
        endif
        " Calculate reindent range and final curpos.
        let d = s:regput__postop(ctx, sep, orig_range)
        " Perform re-indent with newly-calculated positions added to adjustment list.
        call s:post_op_reindent(d.irange[0], d.irange[1], [ps, d.curpos, d.orange])
        if ctx.mode ==? 'v'
            " Design Decision: Original selection may have been partial; set marks to
            " encompass operated-on range, excluding any surrounding whitespace.
            call s:set_visual_marks(d.orange)
        else
            " Restore original (but adjusted) visual marks and set post-op cursor
            " position.
            call s:set_visual_marks(ctx.vrange)
        endif
        " Since visual mode commands end in normal mode, we can always respect
        " option-configurable desired curpos.
        call s:setcursor(d.curpos)
        " See note in header for rationale behind the conditions for setting 'latest jump'
        " mark.
        if (a:tgt.put_mode =~ '_op' && g:sexp_regput_curpos_op != 2
            \ || a:tgt.put_mode =~ '_child' && g:sexp_regput_curpos_child != 2)
            \ && a:tgt.curpos[1] != d.curpos[1]
            call setpos("'`", a:tgt.curpos)
        endif
    catch
        " Don't warn about sexp-{abort,noop}, which are expected.
        if v:exception !~ 'sexp-\%(abort\|noop\)'
            call sexp#warn#msg(printf("regput__impl: Unexpected error '%s' at '%s'",
                \ v:exception, v:throwpoint))
        endif
        " Restore pre-op curpos.
        " Note: We use the adjusted curpos in tgt since there's no guarantee ctx exists.
        call s:setcursor(a:tgt.curpos)
    endtry
endfunction

" Calculate and return the tgt_info dict needed by s:regput__get_context() for a 'put'
" command.
" Note: In addition to the basic put before/after, this function also handles
" put_op[_tele], in that case, caller supplies a pre-initialized context dict for us to
" augment.
" Note: For details, on the format of the context dict, see header of
" s:regput__get_context().
" -- Args --
" count:        the sexp command count
" tail:         put direction flag (0=before 1=after)
" [ctx]:        (optional) dict this function should augment
" -- Logic --
" Nominal Case: If cursor is *on* element, use that element as the target, and let
" direction be determined by put command (p/P).
" Special Case: There are 3 distinct scenarios in which cursor is not *on* an element:
"   1. cursor inside (not on brackets of) empty list
"   2. cursor in empty (apart from whitespace) buffer
"   3. cursor in whitespace adjacent to one or more elements
" In the interest of simplifying downstream logic, we attempt to make the special cases
" look somewhat like the nominal case (e.g., by allowing tgt to be a bracket or a special
" BOF/EOF sentinel), but we also set flags (e.g., empty_{list,buffer}, force_nl[]) that
" allow downstream logic to discriminate when necessary.
" The handling of case 3 depends on the colinearity of cursor pos with whatever comes
" before/after (i.e., effective prev/next, which can be either element or bracket, or even
" BOF/EOF) and is outlined below:
"
" If cursor colinear with both effective prev/next
"   p = put after effective prev
"   P = put before effective next
" ElseIf cursor colinear with effective prev
"   put after effective prev
"   force NL between effective prev and put text iff op == 'p'
" ElseIf cursor colinear with effective next
"   put before effective next
"   force NL between effective next and put text iff op == 'P'
" Else
"   non-directional put
" Design Decision: Although we could handle p and P the same way in the 2 ElseIf's above,
" treating the one that puts *away from* the colinear target as a request for extra
" separation seems intuitive and gives extra flexibility.
function! s:put__get_tgt(count, tail, ...)
    let curpos = getpos('.')
    let ret = a:0 && !empty(a:1)
        \ ? a:1
        \ : s:regput__ctx_init('n', a:tail ? 'put_after' : 'put_before', a:count)
    try
        " First, attempt to get natural target: i.e., side of *current* element in
        " direction of put. If no such element, special logic will determine a 'virtual'
        " target.
        let ret.range[!a:tail] = sexp#move_to_current_element_terminal(a:tail)
        let no_current = !ret.range[!a:tail][1]
        " Attempt to set both ends of range.
        for i in range(2)
            if !ret.range[i][1]
                let ret.range[i] = sexp#move_to_adjacent_element_terminal(i, !i, 0, 1)
                if ret.range[i][1]
                    let ret.is_ele[i] = 1
                else
                    " If other end of range is already determined, we should be on its
                    " near side, thereby ensuring call to nearest_bracket() from list will
                    " not find the list's own bracket.
                    let ret.range[i] = s:nearest_bracket(i)
                    if ret.range[i][1]
                        let ret.is_bra[i] = 1
                    endif
                endif
            else
                let ret.is_ele[i] = 1
            endif
        endfor
        " At this point, range endpoints can still be nullpos, but is_ele[] and is_bra[] are
        " set correctly.
        let empty_list = ret.is_bra[0] && ret.is_bra[1]
        let empty_buffer = !ret.is_ele[0] && !ret.is_ele[1] && !empty_list
        " Unless superseded by empty_{list,buffer} special logic, use special logic described
        " in header to determine target if cursor was not *on* element.
        " Note: This logic may set tgt to pre-BOF/post-EOF sentinel position.
        if !empty_list && !empty_buffer && no_current
            " The conditional blocks below can...
            "   * reverse range
            "   * set flag forcing insertion of NL at near side.
            " Note: The following logic allows for possibility of nullpos next/prev.
            " Design Decision: Never force NL around brackets.
            let [prev, next] = [ret.range[0], ret.range[1]]
            if curpos[1] != prev[1] && curpos[1] != next[1]
                " Design Decision: The fact that we're not on the element (or even near it)
                " implies direction change: intuitively, the direction of p or P in empty line
                " selects the target element.
                " FIXME: Hmm... Maybe not so intuitive. Consider changing to
                " non-directional (tail == -1).
                "let ret.tail = !ret.tail
                let ret.tail = -1
            elseif curpos[1] == prev[1] && curpos[1] != next[1]
                let [ret.force_nl[0], ret.tail] = [a:tail && ret.is_ele[0], 1]
            elseif curpos[1] == next[1] && curpos[1] != prev[1]
                let [ret.force_nl[1], ret.tail] = [!a:tail && ret.is_ele[1], 0]
            endif
        endif
        " If either range endpoint is still null, set it to corresponding buffer extremity.
        if !ret.range[0][1]
            let ret.range[0] = s:BOF
        endif
        if !ret.range[1][1]
            let ret.range[1] = [0, line('$'), col([line('$'), '$']), 0]
        endif
        return ret
    finally
        call s:setcursor(curpos)
    endtry
endfunction

" Return 1 iff all conditions are met for converting a put before/after to the
" corresponding put into list.
function! s:regput__handle_as_put_into_empty_list(count, tail)
    if !g:sexp_regput_bracket_is_target
        " Feature disabled by user config
        return 0
    endif
    let isl = s:is_list(line('.'), col('.'))
    if !isl
        " Not *on* list
        return 0
    endif
    " We're on list bracket or macro chars. Short-circuit if put is away from list
    " interior.
    if !a:tail && isl != 3 || a:tail && isl == 3
        return 0
    endif
    " Treat as put into empty list!
    return 1
endfunction

" Put before/after (both normal and operator variants)
" Note: Optional context dict provided by caller if invoked internally by operator
" mechanism.
function! sexp#put(count, tail, ...)
    if s:regput__handle_as_put_into_empty_list(a:count, a:tail)
        " All conditions met for converting put to put_child!
        " Caveat: Toggle tail due to change in meaning: i.e.,
        "   put (forwards) from open bracket (tail == 1) ==> put at head (tail == 0)
        "   put (backwards) from close bracket (tail == 0) ==> put at tail (tail == 1)
        call sexp#put_child(a:count, !a:tail, a:0 ? a:1 : {})
    else
        let cnt = a:count ? a:count : 1
        let tgt = s:put__get_tgt(cnt, a:tail, a:0 ? a:1 : {})
        call s:regput__impl(tgt, cnt)
    endif
endfunction

" Augment provided tgt/ctx dict with several range related fields, calculated from the raw
" input range, which might represent a visual selection, an operator motion or a sexp
" object.
function! s:replace__process_inner_range(tgt)
    let tgt = a:tgt
    " Determine the range that *contains* inner_range.
    for i in range(2)
        call s:setcursor(tgt.inner_range[i])
        let p = sexp#nearest_element_terminal(i, !i, 1, 1)
        if p[1]
            let tgt.is_ele[i] = 1
        else
            let p = s:nearest_bracket(i)
            if p[1]
                let tgt.is_bra[i] = 1
            else
                " Default to buffer extremity.
                let p = i ? [0, line('$'), col([line('$'), '$']), 0] : s:BOF
            endif
        endif
        let tgt.range[i] = p
    endfor
endfunction

" Either create and initialize a context dict, or populate the one provided by caller. If
" no context dict is provided by caller, command is assumed to be normal or visual replace
" command; otherwise, it's the replace operator variant indicated by input context dict's
" 'put_mode' field.
" Cursor Assumptions: For certain command/operator variants, the cursor position on entry
" to this function determines the replace target; in those cases, we assume caller has
" pre-positioned cursor appropriately.
" Cursor Preservation: Saved/restored
function! s:replace__get_tgt(mode, count, tail, P, ...)
    let ret = a:0 && !empty(a:1)
        \ ? a:1
        \ : s:regput__ctx_init(a:mode, 'replace', a:count, {'P': a:P})
    let curpos = getpos('.')
    try
        " Note: Normal mode replace command and telescopic replace operator use cursor
        " position; all other commands use either visual or operator range and are handled
        " in the else.
        if a:mode ==? 'n' && ret.put_mode == 'replace'
            \ || ret.put_mode == 'replace_op_tele'
            " Assumption: Caller has positioned cursor correctly.
            let rng = sexp#current_element_terminals()
            if !rng[0][1]
                throw 'sexp-abort: No valid target for sexp replace'
            endif
        else " visual mode or non-telescopic replace operator.
            " Validate and adjust range to include only complete forms.
            let [s, e] = ret.put_mode == 'replace_op' ? ret.orange : ret.vrange
            if !sexp#range_has_non_ws(s, e, 1)
                throw 'sexp-abort: Invalid attempt to perform replacement'
                    \ . ' on pure whitespace'
            endif
            let rng = [s, e]
            if !sexp#is_uniform_range(rng)
                throw 'sexp-abort: Invalid attempt to perform replacement'
                    \ . ' on range that crosses list boundaries'
            endif
            for i in range(2)
                call s:setcursor(rng[i])
                let rng[i] = sexp#move_to_current_element_terminal(i)
                if !rng[i][1]
                    " Attempt to find nearest terminal in inward direction.
                    let rng[i] = sexp#move_to_adjacent_element_terminal(!i, i, 0)
                endif
            endfor
            " Design Decision: Be less strict with visual selections.
            " Rationale: Errors are less likely when selection is visually apparent before
            " command execution.
            if !g:sexp_regput_replace_expanded && ret.put_mode =~ 'replace_op'
                " Make sure original selection contained only complete S-Expressions.
                if sexp#compare_pos(s, rng[0]) > 0 || sexp#compare_pos(e, rng[1]) < 0
                    throw 'sexp-abort: Current setting of g:sexp_regput_replace_expanded prohibits'
                        \ . ' selecting only part of an S-Expression for replacement.'
                        \ . ' :help g:sexp_regput_replace_expanded'
                endif
            endif
        endif
        let ret.inner_range = rng
        " Post-process inner_range.
        call s:replace__process_inner_range(ret)
        return ret
    finally
        call s:setcursor(curpos)
    endtry
endfunction

" Assuming the input range represents '[ and '] after a yank, and the inclusive/visual
" flags were extracted from the TextYankPost autocmd v:event, return an adjusted range
" representing the actual yank range, which may include one additional character: e.g.,
" after a yvw, the
" input range would not include the first char of the subsequent word but the returned
" range would.
" Motivation: Regput "telescopic" operators need to know what was actually searched for
" when the search is exclusive. Unfortunately, an 'opfunc' is not provided any information
" on the inclusivity of the motion that triggered the opfunc. Thus, we use a yank in lieu
" of g@, explicitly invoking what would have been the 'opfunc' in the TextYankPost
" handler where we have access to v:event.inclusive, which is passed to this function.
function! s:fix_operator_range(rng, inclusive, visual)
    " Note: Deepcopy not really necessary, but doesn't hurt.
    let ret = deepcopy(a:rng)
    if !a:visual && a:inclusive
        " Offset final position in range.
        let ret[1] = sexp#offset_char(ret[1], 0)
    endif
    return ret
endfunction

" Update the provided context dict to reflect the operator motion/object stored therein,
" using both the relevant option(s) and the nature of the motion/object itself to choose
" between "telescopic mode" and normal mode.
" Cursor Preservation: Set cursor position appropriately if the function that performs the
" put or replace will use it; otherwise, restore it to curpos at function entry.
" The following fields are subject to update:
"   put_mode
"   orange
" See s:regput_op__handle() for parameter descriptions.
function! s:regput_op__process_motion(ctx, inclusive, visual, motion)
    " Initial curpos used to determine whether motion may have been used.
    let [curpos, rng] = [a:ctx.curpos, a:ctx.orange]
    " Sexp objects inhibit telescopic mode.
    let is_sexp_motion = a:motion =~ 'sexp_\%(inner\|outer\)_'
    " In some cases, we'll want to restore original cursor pos; set to nullpos to inhibit.
    let save_curpos = getpos('.')
    try
        " Which direction was the motion search? (-1 if 'object' not motion).
        let dir = rng[0] == curpos ? 1 : rng[1] == curpos ? 0 : -1
        " Cache effective 'tail' flag (defaulting to 0)
        let tail = a:ctx.tail != -1 ? a:ctx.tail : 0
        if a:inclusive != -1
            " TextYankPost (not g@) mechanism was used.
            if g:sexp_regput_tele_motion && !is_sexp_motion && dir != -1
                " Telescopic mode enabled, not sexp object/motion, and motion anchored at
                " curpos.
                if g:sexp_regput_tele_motion >= (a:ctx.put_mode =~ 'replace_op' ? 3 : 2)
                    \ || !sexp#is_uniform_range(rng)
                    " Either telescopic mode unconditionally enabled or tree levels crossed.
                    " Treat as telescopic, provided the reached position is actually *on* a
                    " sexp.
                    call s:setcursor(rng[dir])
                    " Make sure we're on a sexp.
                    " Note: Intentionally keeping the searched position (not the terminal)
                    " since it can matter for downstream logic (e.g., in put mode, if
                    " target was open/close bracket).
                    let p = sexp#current_element_terminal(0)
                    if !p[1]
                        throw "sexp-abort: Regput telescopic mode motion must target non-ws."
                    endif
                    " Keep current position.
                    let save_curpos = s:nullpos
                    " Record conversion to telescopic mode.
                    let a:ctx.put_mode .= "_tele"
                    " Note: Always return here if operator handled as telescopic.
                    return
                endif
            endif
            " Not telescopic mode, but because TextYankPost mechanism was used, we need to
            " adjust range to account for motion exclusivity: i.e., adjust range to make
            " it appear that g@ was used.
            let a:ctx.orange = s:fix_operator_range(a:ctx.orange, a:inclusive, a:visual)
        endif
        " Not telescopic mode, and any adjustments required for operator range have been
        " performed.
        let rng = a:ctx.orange
        if a:ctx.put_mode =~ 'put_op'
            " Note: sexp#replace() performs validation on the operator range; however,
            " sexp#put() uses cursor pos, even if it's in whitespace, and we don't want to
            " allow that for non-telescopic put operator. Also, we don't want to allow
            " selections that cross list boundaries in non-telescopic mode.
            if !sexp#is_uniform_range(rng)
                throw "sexp-abort: put operator requires uniform range."
                    \ . " Did you mean to enable telescopic mode?"
                    \ . " (:help g:sexp_regput_tele_motion)"
            endif
            if !sexp#range_has_non_ws(rng[0], rng[1], 1)
                throw "sexp-abort: put operator requires non-empty range"
            endif
            " Position cursor on target of put, making sure we're on actual sexp.
            call s:setcursor(rng[dir])
            let p = sexp#move_to_current_element_terminal(tail)
            if !p[1]
                " Edge of range lies in whitespace; look inwards.
                " Assumption: Prior call to range_has_non_ws() guarantees success.
                let p = sexp#move_to_adjacent_element_terminal(!tail, tail, 0)
            elseif sexp#compare_pos(rng[dir], p) < 0
                " Selection doesn't include terminal of target element in put direction!
                throw "sexp-abort: put operator object/motion must include"
                    \ . " target element terminal."
                    \ . " Did you mean to enable telescopic mode?"
                    \ . " (:help g:sexp_regput_tele_motion)"
            endif
            " Keep current position.
            let save_curpos = s:nullpos
        endif
    finally
        " If pos is non-null, desired cursor position has already been set.
        if save_curpos[1]
            call s:setcursor(save_curpos)
        endif
    endtry
endfunction

" Augment the provided context dict, taking the type of regput operator, and if
" applicable, the 'inclusive' flag extracted from the TextYankPost autocmd event, into
" account.
" -- Args --
" ctx:        context dict (see get_context() header for format)
" inclusive:  flag from v:event indicating inclusivity of motion/object (-1 if unknown)
"             Note: If opfunc mechanism (rather than TextYankPost autocmd) was used,
"             inclusive flag is set to -1 to reflect fact that Vim has normalized the
"             operator range without telling us whether the motion was inclusive.
" visual:     flag from v:event indicating selection is visual (not from motion)
" motion:     name of sexp plug command if motion/object provided by sexp, else ""
function! s:regput_op__handle(ctx, inclusive, visual, motion)
    " Cached 'orange' reflects '[ '] at the time operator was invoked; update to reflect
    " operand.
    let a:ctx.orange = [getpos("'["), getpos("']")]
    " Implementation Note: To avoid redundancy and eliminate risk of inconsistent
    " behavior, we delegate to non-operator-specific functions that handle both operator
    " and non-operator commands the same way. First, however, we must ensure the function
    " will see the correct cursor position and/or range.
    call s:regput_op__process_motion(a:ctx, a:inclusive, a:visual, a:motion)
    if a:ctx.put_mode =~ 'replace'
        " Replace operator
        call sexp#replace('n', 1, a:ctx.P, a:ctx)
    else
        call sexp#put(1, a:ctx.tail, a:ctx)
    endif
endfunction

" Note: This function is used for both replace_op and put_op and "P" has meaning (albeit
" different meaning) for both.
" Important Note: Originally, telescopic searches used a bare `y` operator, but this
" resulted in an ambiguity involving the two special cases discussed in the section on
" exclusive and inclusive motions (:help inclusive). One way to eliminate the ambiguity is
" to use `yv` to force characterwise motion, thereby inhibiting the special case logic
" capable of making the motion linewise. Unfortunately, use of `v` has the side-effect of
" toggling inclusivity. When telescopic motion is ultimately selected, inclusivity is
" irrelevant because the range is all we care about and it's always inclusive. However, if
" we end up converting the yank to a traditional (non-telescopic) motion, we need to know
" whether the *original* motion was inclusive so that we can ensure the range provided to
" the vim-sexp operator is consistent with the user's intent. You might assume we could
" simply take the complement of v:event.inclusive to determine the inclusivity of the
" motion/object; unfortunately, this won't work because operator-pending commands that use
" visual mode to delineate the region cause v:event.inclusive to be set, even when an
" inclusivity-toggling operator such as `yv` is used. The following two examples
" illustrate the ambiguity:
" 1. <operator> iw => v:event.inclusive == false
" 2. <operator> ie => v:event.inclusive == true
" To resolve the ambiguity, we must also consider v:event.visual, which will be set in the
" second case but not the first.
" Rationale: A visual selection in the operator-pending command effectively overrides the
" effect of `yv` on inclusivity.
function! sexp#regput_op(is_replace, P, ...)
    " Sexp operators don't use counts.
    " TODO: Currently, warning is given by plug wrapper, but consider creating a
    " sexp#warn#if_count_provided() or some such, which could eventually be easily invoked
    " from other sexp operators (but only the ones for which it makes sense).
    " TODO: Consider whether the aforementioned warning would be best coming before or
    " after the motion/object.
    if !a:0
        " This initial call is made by wrapper function helper s:opfunc, not Vim's opfunc
        " engine. When the completing call is made by either vim's 'opfunc' or the
        " TextYankPost mechanism, this if block will be skipped.
        " Note: Set tail to -1 for inherently non-directional replace op; otherwise, determine
        " its value from the P flag (P=before p=after - note the inversion).
        " TODO: Consider an init function that creates these for everyone, with default
        " values, or perhaps allowing optional list of keys to be passed in.
        let ctx = s:regput__ctx_init('n', a:is_replace ? 'replace_op' : 'put_op', 1, {
            \ 'P': a:is_replace ? a:P : -1,
            \ 'tail': a:is_replace ? -1 : !a:P,
        \ })
        " Important Note: Ideally, we'd use the black hole register for this, but we need
        " TextYankPost to fire, and it doesn't for the black hole register; thus, use z
        " register and ensure the handlers save/restore both it and the unnamed register.
        " Note: See note in header for rationale behind use of yv.
        " TODO: Consider just returning boolean flag indicating the mode to use and
        " letting opfunc handle the details?
        let op = v:version >= 801 && g:sexp_regput_tele_motion ? '"zyv' : 'g@'
        " The call via 'opfunc' or TextYankPost will get the original args + the context
        " dict and the operator itself.
        return [op, function('sexp#regput_op', [a:is_replace, a:P, ctx, op])]
    endif
    try
        " We've been invoked by our opfunc wrapper, called either as true 'opfunc' or in
        " response to a TextYankPost.
        let [ctx, op, type, inclusive, visual, motion] = a:000
        if type != 'char'
            call sexp#warn#msg("sexp#replace_op:"
                \ " Invalid use of mode '" . type . "' with replace operator."
                \ " Only charwise mode supported")
            return
        endif
        call s:regput_op__handle(ctx, inclusive, visual, motion)
    catch /sexp-\%(warning\|abort\):/
        call sexp#warn#msg(v:exception)
    catch
        " Show throwpoint only for unexpected errors.
        call sexp#warn#msg(v:exception . " at " . v:throwpoint)
    endtry
endfunction

" Perform both replace and replace_op: in the latter case, ctx dict is provided by caller.
" Note: For replace modes, P indicates which p command was used: 0 == 'p', 1 == 'P'.
" Although it doesn't affect the result of the put, it does determine whether the unnamed
" register is updated.
function! sexp#replace(mode, count, P, ...)
    try
        let cnt = a:count ? a:count : 1
        " Target determination is mode-specific.
        let tgt = s:replace__get_tgt(a:mode, cnt, -1, a:P, a:0 ? a:1 : {})
        " Design Decision: Let tail==1 represent put with P.
        call s:regput__impl(tgt, cnt)
    catch /sexp-warning:/
        call sexp#warn#msg(v:exception)
    catch
        " Show throwpoint only for non-warning errors.
        call sexp#warn#msg(v:exception . " at " . v:throwpoint)
    endtry
endfunction

" Calculate and return the tgt_info dict needed by s:regput__get_context() for the case of
" a 'put_child' command.
" Note: For details on this dict, see header of s:regput__get_context().
" Tail Logic: Although we could treat a child put as a non-directional put, doing so would
" lead to the following ambiguity: in the absence of a target, how do we decide which
" existing element to append/prepend to when our contextual logic indicates
" appending/prepending makes sense. OTOH, if we make the put directional, which direction
" makese sense? I.e., which element should serve as the target? Any answer to this
" question is bound to be somewhat arbitrary, but here's what I'm thinking makes sense:
" when the put is *between* elements, the target is the element furthest from the
" reference bracket (i.e., the bracket on the side from which put occurs). As long as we
" take this approach, it's not necessary to toggle 'tail' in the nominal case (put between
" child elements): i.e.,
"   -- Command --                  -- Implemented As --
"   put at head (a:tail == 0)      put before target (P) (tail == 0)
"   put at tail (a:tail == 1)      put after target (p) (tail == 1)
" However, when the [count] is so large that the desired element doesn't exist, we put
" between terminal child and list bracket, treating the terminal child as the target.
" Since the put in this case is in the opposite direction relative to the target, we
" toggle 'tail' in the return dict to reflect the reversal.
function! s:put_child__get_tgt(count, tail, ...)
    let cursor = getpos('.')
    " Note: Because we may be converting an existing context dict to a put_child one, we
    " need to ensure 'tail' and 'put_mode' fields are overridden.
    let ret = a:0 && !empty(a:1)
        \ ? extend(a:1, {'tail': a:tail, 'put_mode': 'put_child'})
        \ : s:regput__ctx_init('n', 'put_child', a:count, { 'tail': a:tail })
    try
        let li = s:list_info(a:tail, {'inner_only': g:sexp_regput_bracket_is_child}) 
        " Make sure there's actually a list target.
        if !li.brackets[0][1]
            throw 'sexp-abort: No valid target for put into list'
        endif
        if li.terminal_range[0][1]
            " Non-empty list
            let r = ret.range
            if a:count > 1
                " Child other than terminal element requested.
                " Find element before/after which to insert.
                let c = s:child_range(a:count, a:tail, 1,
                    \ {'list_info': li, 'exact_count': 0})
                if c.missing
                    " Requested child doesn't exist, so use limiting list bracket and
                    " bracket end of terminal child.
                    let r[a:tail] = c.range[!a:tail]
                    call s:setcursor(r[a:tail])
                    let r[!a:tail] = li.brackets[!a:tail]
                    let ret.is_bra[!a:tail] = 1
                    let ret.is_ele[a:tail] = 1
                    " See notes in header for rationale behind this toggle.
                    let ret.tail = !ret.tail
                else
                    " Requested child exists; use it and adjacent on near side.
                    let r[!a:tail] = c.range[a:tail]
                    let ret.is_ele[!a:tail] = 1
                    " Position on outside of element and look for adjacent, with fallback
                    " to bracket.
                    call s:setcursor(r[!a:tail])
                    let r[a:tail] = sexp#nearest_element_terminal(a:tail, !a:tail)
                    if !r[a:tail][1]
                        " No adjacent; use bracket.
                        let r[a:tail] = li.brackets[a:tail]
                        let ret.is_bra[a:tail] = 1
                    else
                        let ret.is_ele[a:tail] = 1
                    endif
                endif
            else
                " Terminal element requested.
                " Use bracket and terminal element, treating terminal element as target.
                let r[!a:tail] = li.terminal_range[a:tail]
                let r[a:tail] = li.brackets[a:tail]
                let ret.is_bra[a:tail] = 1
                let ret.is_ele[!a:tail] = 1
            endif
        elseif li.brackets[0][1]
            " Empty list
            let ret.range = li.brackets
            let ret.is_bra = [1, 1]
            let ret.is_ele = [0, 0]
        endif
        return ret
    finally
        call s:setcursor(cursor)
    endtry
endfunction

" Put at head/tail of list.
function! sexp#put_child(count, tail, ...)
    let cnt = a:count ? a:count : 1
    let tgt = s:put_child__get_tgt(cnt, a:tail, a:0 ? a:1 : {})
    " Note: [count] is used only for child location, so hardcode to 1.
    call s:regput__impl(tgt, 1)
endfunction

" The following pair of functions suppresses the TextYankPost handler used to update
" yank_metadata when a vim-sexp function is in progress.
" Rationale: Only user initiated yanks/deletes (not those initiated by internal plugin
" logic) should cause metadata update. Failure to apply this constraint can result in the
" spurious clearing of metadata flags like 'is_complete_sexp_in_buffer'.
function! s:regput__begin_internal_typ()
    let s:regput_internal_typ_active = 1
endfunction

function! s:regput__end_internal_typ()
    let s:regput_internal_typ_active = 0
endfunction

" Defer restoration of 'virtualedit' after a delete TextYankPost handler temporarily
" widens it to preserve the still-live operator range.
" Rationale: On the delete path, the TextYankPost handler may move the cursor while the
" target delete range is still live. In this window, cursor() appears to trigger
" normalization of that range. If the range was created with ve=onemore but the
" normalization occurs after restoring a less permissive 'virtualedit' setting, a boundary
" that should remain one past end of line can collapse onto the line's final actual
" character. Defer restoration of 've' till SafeState to preserve the virtualedit
" semantics under which the delete range was established.
function! s:regput__schedule_ve_restore(ve_save)
    if s:regput_pending_ve_restore isnot v:null
        return
    endif
    let s:regput_pending_ve_restore = a:ve_save
    augroup SexpRegputVeRestore
        autocmd!
        autocmd SafeState * call <SID>regput__maybe_restore_ve()
    augroup END
endfunction

" Restore 'virtualedit' once Vim reaches a quiescent post-operator state.
" Design Decision: Rely on SafeState alone rather than additionally requiring mode() ==# 'n'.
" Rationale: This shortens the lifetime of the pending restore state. If experience later
" shows that SafeState can still be too early in some non-Normal mode scenario, a
" `mode() !=# 'n'` early return can be re-enabled here.
function! s:regput__maybe_restore_ve()
    if s:regput_pending_ve_restore isnot v:null
        " Conservative alternative:
        " if mode() !=# 'n'
        "     return
        " endif

        if &ve ==# 'onemore'
            let &ve = s:regput_pending_ve_restore
        endif

        let s:regput_pending_ve_restore = v:null
    endif

    augroup SexpRegputVeRestore
        autocmd!
    augroup END
endfunction

" Determine whether a regput command should fall back to builtin paste.
" Args:
"   plug_name: the sexp plug command name (e.g., 'sexp_put_after')
"   mode: the mode the command is being invoked from
" Return:
"   1 if should use builtin, 0 if should use smart paste
function! s:regput__should_use_builtin(plug_name, mode)
    " Don't use builtin if this isn't even a command/mode with an associated builtin.
    let regput_builtin_commands = sexp#data#get_regput_builtin_commands()
    if !has_key(regput_builtin_commands, a:plug_name)
        \ || stridx(regput_builtin_commands[a:plug_name].modes, a:mode) < 0
        return 0
    endif
    " Validate and cache option flags.
    let source_flags = s:regput__get_flag_opt('sexp_regput_fallback_source')
    let target_flags = s:regput__get_flag_opt('sexp_regput_fallback_target')
    " Check for special case involving "outward" put from string/comment terminal.
    if s:regput__force_smart_target(a:plug_name, a:mode)
        return 0
    endif
    " Does target context argue for builtin put?
    if !empty(target_flags) && s:regput__target_prefers_builtin(a:plug_name, a:mode, target_flags)
        return 1
    endif
    " Does source register argue for builtin put?
    let reg = v:register
    let metadata = get(s:yank_metadata, reg, {})

    if !empty(source_flags)
        " Check register metadata against flags
        if stridx(source_flags, 's') >= 0
            " Single-character register (handle multi-byte correctly)
            let reg_lines = getreg(reg, 1, 1)
            if len(reg_lines) == 1 && strcharlen(reg_lines[0]) == 1
                return 1  " Single-char register, use builtin
            endif
        endif

        if stridx(source_flags, 'l') >= 0
            " Linewise register
            let reg_type = getregtype(reg)
            if reg_type[0] ==# 'V'
                return 1  " Linewise register, use builtin
            endif
        endif

        if stridx(source_flags, 'c') >= 0
            " Non-complete-sexp-in-buffer register (incomplete sexp)
            if !get(metadata, 'is_complete_sexp_in_buffer', 0)
                return 1  " Incomplete sexp, use builtin
            endif
        endif

        if stridx(source_flags, 'o') >= 0
            " Register captured via sexp object - use smart paste (NOT builtin)
            if !get(metadata, 'from_sexp_object', 0)
                return 1  " Not from sexp object, use builtin
            endif
        endif
    endif

    return 0  " Doesn't match any flags, use smart paste
endfunction

" Validate a flag-string regput option, warning once and returning empty string if the
" configured value is not a string.
function! s:regput__get_flag_opt(optname)
    let value = get(g:, a:optname)
    if type(value) == type('')
        return value
    endif
    call sexp#warn#msg(
        \ printf("sexp-warning: Ignoring invalid option setting: %s=%s."
            \ . " Expected string value.",
            \ a:optname, string(value)),
        \ {'once': a:optname})
    return ''
endfunction

" Return 1 iff the contextual regput command named by plug_name puts text "forward"
" relative to the cursor/target: i.e., like `p` rather than `P`.
function! s:regput__puts_forward(plug_name)
    return a:plug_name !~# '_P\>' && a:plug_name !~# '_before\>'
endfunction

" Return 1 iff the target position is a comment/string boundary from which the command
" puts outward, in which case smart paste is forced regardless of fallback flags.
function! s:regput__point_forces_smart_target(plug_name, mode, pos)
    if !a:pos[1]
        return 0
    endif
    " Special case applies only for normal mode puts from comment/string terminal.
    if a:mode ==# 'x'
        return 0
    endif

    let cursor = getpos('.')
    let forward = s:regput__puts_forward(a:plug_name)
    try
        call s:setcursor(a:pos)

        if sexp#is_comment(a:pos[1], a:pos[2])
            let head = sexp#current_element_terminal(0)
            return sexp#compare_pos(a:pos, head) == 0 && !forward
        endif

        if s:is_rgn_type('string', a:pos[1], a:pos[2])
            let start = sexp#current_element_terminal(0)
            let end = sexp#current_element_terminal(1)
            if sexp#compare_pos(a:pos, start) == 0
                return !forward
            elseif sexp#compare_pos(a:pos, end) == 0
                return forward
            endif
        endif
    finally
        call s:setcursor(cursor)
    endtry

    return 0
endfunction

" Return 1 iff any target position relevant to the current command is a hard smart-paste
" boundary case. For visual replace, no such cases are recognized.
function! s:regput__force_smart_target(plug_name, mode)
    let points = a:mode ==# 'x' ? [getpos("'<"), getpos("'>")] : [getpos('.')]
    for pos in points
        if s:regput__point_forces_smart_target(a:plug_name, a:mode, pos)
            return 1
        endif
    endfor
    return 0
endfunction

" Return 1 iff the target fallback flags indicate builtin paste at cursor position (normal
" mode) or *either* of '< '> (visual mode). Unlike the special boundary logic involving
" outward paste from comment/string endpoints, this behavior is enabled only when the
" corresponding flags are present in g:sexp_regput_fallback_target.
function! s:regput__target_prefers_builtin(plug_name, mode, flags)
    let points = a:mode ==# 'x' ? [getpos("'<"), getpos("'>")] : [getpos('.')]
    for pos in points
        if !pos[1]
            continue
        endif
        if stridx(a:flags, 'c') >= 0 && sexp#is_comment(pos[1], pos[2])
            return 1
        endif
        if stridx(a:flags, 's') >= 0 && s:is_rgn_type('string', pos[1], pos[2])
            return 1
        endif
    endfor
    return 0
endfunction

" Get the fallback builtin command for a regput command if appropriate.
" Returns the builtin command to execute, or empty string if should use smart paste.
function! sexp#regput__get_fallback_cmd(plug_name, mode)
    if s:regput__should_use_builtin(a:plug_name, a:mode)
        " Look up builtin from sexp_mappings
        let sexp_mappings = sexp#data#get_sexp_mappings()
        let builtin_entry = get(sexp_mappings, a:plug_name, {})
        " Convert map entry to denormalized form supporting lookup by single mode char.
        " TODO: In lieu of re-parsing the entry (which we know to be valid), consider just
        " finding the key containing a:mode.
        let [builtin_map, _] = sexp#plug#parse_map_entry(a:plug_name, builtin_entry, a:mode)
        let builtin_cmd = get(builtin_map, a:mode, '')
        if empty(builtin_cmd)
            " Note: Arrival here probably indicates problem with s:sexp_mappings.
            call sexp#warn#msg(printf(
                \ "Warning: Could not find builtin keybinding for %s in mode %s",
                \ a:plug_name, a:mode))
            return ''
        endif
        return builtin_cmd
    endif
    return ''
endfunction

" Execute the builtin put command corresponding to the contextual regput command if the
" current fallback logic selects builtin behavior. Return 1 iff builtin was executed.
function! sexp#regput__maybe_execute_builtin(plug_name, mode, count, regname)
    let cmd = sexp#regput__get_fallback_cmd(a:plug_name, a:mode)
    if empty(cmd)
        return 0
    endif
    " Make sure builtin uses any provided register/count.
    let prefix = (a:regname ==# '' || a:regname ==# '"' ? '' : ('"' . a:regname))
        \ . (a:count > 0 ? a:count : '')
    if a:mode ==# 'x'
        " TODO: Verify that we can always trust range here.
        execute 'normal! gv' . prefix . cmd
    else
        execute 'normal! ' . prefix . cmd
    endif
    return 1
endfunction

" Record metadata each time a sexp object is used to select a range.
" Called by sexp object commands (e.g., sexp_inner_element) to create a metadata hint,
" which the TextYankPost handler will use to determine whether a yank/delete was performed
" on a sexp object (as opposed to a non-sexp motion/object).
" Args:
"   start: position [bufnr, lnum, col, off]
"   end:   position [bufnr, lnum, col, off]
function! sexp#regput__set_object_yank_hint(start, end)
    let s:sexp_object_yank_hint = {
        \ 'bufnr': bufnr('%'),
        \ 'start': a:start,
        \ 'end': a:end,
        \ 'text': s:extract_text_from_range(a:start, a:end),
        \ 'changedtick': b:changedtick,
    \ }
endfunction

" Check if the sexp object hint is still valid and matches the current yank/delete.
" Validation relies on content matching plus range matching.
" Args:
"   is_delete: 1 if this is a delete event, 0 if yank
"   regcontents: v:event.regcontents (the yanked/deleted text)
"   start_pos: position [bufnr, lnum, col, off] from '['
"   end_pos:   position [bufnr, lnum, col, off] from ']'
" Returns: 1 if hint is valid and matches, 0 otherwise
function! s:regput__match_object_yank_hint(is_delete, regcontents, start_pos, end_pos)
    if empty(s:sexp_object_yank_hint)
        return 0  " No active hint
    endif

    let hint = s:sexp_object_yank_hint
    let regtext = s:regcontents_to_text(a:regcontents)

    " Make sure the sexp object invocation was in current buffer.
    if bufnr('%') != get(hint, 'bufnr', -1)
        return 0
    endif

    " Check content match
    if regtext !=# hint.text
        return 0  " Content mismatch
    endif

    " Verified: For a delete, '[ '] range in TextYankPost is *pre-delete* range.
    if a:start_pos[1:2] !=# hint.start[1:2] || a:end_pos[1:2] !=# hint.end[1:2]
        return 0  " Range mismatch
    endif

    if b:changedtick != hint.changedtick
        return 0  " Buffer was modified before yank/delete completed
    endif

    return 1
endfunction

" Clear the sexp object yank hint (clear after use or decision not to use due to inhibit).
" See s:regput__set_object_yank_hint().
function! s:regput__clear_object_yank_hint()
    let s:sexp_object_yank_hint = {}
endfunction

" Compute the is_complete_sexp_in_buffer metadata flag for a yank/delete event, using the
" live TextYankPost range and permitting surrounding whitespace outside complete elements.
function! s:regput__compute_complete_metadata(is_delete, start_pos, end_pos, regcontents)
    " Verified: In TextYankPost, delete events still expose the pre-delete buffer state,
    " so we can validate both yanks and deletes directly against the live range.
    let [trimmed_start, trimmed_end] = s:trim_range_to_non_ws(a:start_pos, a:end_pos)
    if !trimmed_start[1]
        return 0
    endif
    " Does the trimmed range represent one or more complete sexps in current buffer?
    return s:range_covers_whole_elements(trimmed_start, trimmed_end)
endfunction

" Store yank/delete metadata for the destination register and, when appropriate, for the
" unnamed register as well.
function! s:regput__store_yank_metadata(regname, metadata)
    if a:regname ==# '' || a:regname ==# '"'
        let s:yank_metadata['"'] = a:metadata
        return
    endif

    " Update the named register.
    let s:yank_metadata[a:regname] = a:metadata
    " If unnamed register was updated, update its metadata entry as well.
    let reg_info = getreginfo(a:regname)
    if get(reg_info, 'isunnamed')
        let s:yank_metadata['"'] = a:metadata
    endif
endfunction

" TextYankPost autocmd handler for capturing yank/delete metadata.
" This runs on every yank and delete in sexp-enabled buffers.
" Populates s:yank_metadata[regname] with metadata for subsequent smart-paste decisions.
function! sexp#regput__TextYankPost()
    if v:event.operator ==# 'd' && &ve !=# 'onemore'
        call s:regput__schedule_ve_restore(&ve)
        set ve=onemore
    endif
    try
        if s:regput_internal_typ_active
            return
        endif
        if sexp#plug#typ_op_ipg()
            " Ignore the yv used in lieu of g@ by the sexp regput operator mechanism.
            call s:regput__clear_object_yank_hint()
            return
        endif

        let regname = v:event.regname
        if v:event.operator !~# '^[yd]$'
            call s:regput__clear_object_yank_hint()
            return
        endif

        let is_delete = v:event.operator ==# 'd'
        let regcontents = v:event.regcontents
        
        " Determine if '[ and '] marks represent a valid range
        let start_pos = getpos("'[")
        let end_pos = getpos("']")
        
        if start_pos[1] == 0 || end_pos[1] == 0
            " Invalid range; skip
            return
        endif
        
        " Does range correspond to complete sexp(s) in buffer?
        let is_complete = s:regput__compute_complete_metadata(
            \ is_delete, start_pos, end_pos, regcontents)
        " Is target of yank/delete a sexp object?
        let from_sexp_object = s:regput__match_object_yank_hint(
            \ is_delete, regcontents, start_pos, end_pos)
        let metadata = {
            \ 'is_complete_sexp_in_buffer': is_complete,
            \ 'from_sexp_object': from_sexp_object,
        \ }
        call s:regput__store_yank_metadata(regname, metadata)
        call s:regput__clear_object_yank_hint()
    catch
        " Silently catch errors; TextYankPost handler shouldn't disrupt normal workflow
        " Note: Restore of 've` intentionally deferred. See note in
        " s:regput__schedule_ve_restore() header.
    endtry
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
        call setpos(mark, sexp#nearest_element_terminal(a:next, a:next))
        call s:set_visual_marks(s:positions_with_element_terminals(s:get_visual_marks()))
    endif
    call s:select_current_marks(a:mode)
    silent! normal! "by
    let bmarks = s:get_visual_marks()

    " Abort if we are already at the head or tail of the current list or at
    " the top or bottom of the file. In these cases the start/end mark will be
    " the same in the direction of movement.
    if sexp#compare_pos(amarks[a:next], bmarks[a:next]) == 0
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
        keepjumps call cursor(sl, sc)
        normal! x
        let s = [0, sl, sc, 0]

        let [el, ec] = s:findpos(nr2char(0x03), 1)
        keepjumps call cursor(el, ec)
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
"   2 = exclusive of adjacent non-NL whitespace
"       Note: Equivalent to inc==0 if no adjacent whitespace
" Note: Handles virtual pre-BOF/post-EOF positions
" Example:
" foo|)   bar   ==>   foo)   |bar
function! s:yankdel_range__preadjust_range_start(start, inc)
    let ret = a:start[:]
    if a:inc == 2
        " Get index of whatever (possibly NL) follows whitespace adjacent to start.
        " This match produces results identical to normal-exclusive if no adjacent ws.
        " Note: The dot in pattern must be optional to handle empty line case.
        " Note: max() prevents use of negative index for input position before BOF.
        let eidx = match(getline(ret[1])[max([0, ret[2] - 1]):], '\v^.?\s*\zs(.|$)')
        let ret[2] += eidx
    elseif !a:inc " normal-exclusive
        " Move to next position, including newline.
        " Assumption: sexp#offset_char can handle s:BOF.
        let ret = sexp#offset_char(ret, 1, 1)
    endif
    return ret
endfunction

" Adjust range end pos to make range entirely inclusive, taking special inc
" value into account.
"   0 = exclusive
"   1 = inclusive (nop)
"   2 = exclusive of whitespace back to but not including newline at BOL
"       Note: Equivalent to inc==0 if no adjacent whitespace
" Example:
" foo)   |bar       ==>   foo|)   bar
" Special Case: If end is BOF and adjustment is exclusive, return special
" non-physical position [0, 1, -1, 0].
" Caveat: Callers requiring physical positions will need to check for this.
function! s:yankdel_range__preadjust_range_end(end, inc)
    let ret = a:end[:]
    " Adjust input position for non-inclusive inc.
    if a:inc == 2
        " adjacent whitespace-exclusive
        " Caveat: Using strpart rather than [:] indexing to avoid problems with negative
        " indices.
        " Note: max() not actually needed, but negative length handling isn't documented.
        let sidx = match(
            \ strpart(getline(ret[1]), 0, max([0, ret[2] - 1])), '\v\S\s*$')
        if sidx < 0
            " Adjacent whitespace extends back to BOL.
            if ret[1] > 1
                " Use previous line's NL.
                let ret = [0, ret[1] - 1, col([ret[1] - 1, '$']), 0]
            else
                " pre-BOF virtual pos
                let ret = s:BOF
            endif
        else
            " Use position of found non-ws.
            let ret[2] = sidx + 1
        endif
    elseif !a:inc " normal-exclusive
        " Move to prev position, including newline.
        let ret = sexp#offset_char(ret, 0, 1)
    endif
    return ret
endfunction

" Build and return a dict with information designed to facilitate requested yank/splice/del.
" Return Dict:
"   start:     start of inclusive range for operation (N/A for directed put)
"   end:       end of inclusive range for operation (N/A for directed put)
"   pos:       target position for a directed put (else nullpos)
"   text:      text for a splice (else empty string)
"              Note: This function can modify this text in one special case.
"   cmd:       One of the following characters, indicating the operation to be performed:
"              used to perform the operation: <none> p P s y d
"              Note: May or may not equal the actual Vim command used.
"   err:       1 iff inputs are invalid
"   errmsg:    error description iff err == 1, else ""
" Args: See descriptions in s:yankdel_range() header comment.
function! s:yankdel_range__preadjust_range(start, end, del_or_spl, inc)
    if sexp#compare_pos(a:start, a:end) > 0
        " Shouldn't happen! How to handle? Internal error?
        " 'err' flag with msg?
        return {'err': 1, 'errmsg': 'Invalid (reversed) range'}
    elseif a:start == a:end
        \ && ((a:inc[0] == 1 && a:inc[1] == 2)
        \     || (a:inc[0] == 2 && a:inc[1] == 1))
        " Ambiguous Intent: a:inc requests both inclusion and exclusion of common
        " start/end position, but this is valid only for a directed put, which, by
        " convention, uses normal-exclusive mode.
        return {'err': 1, 'errmsg': 'Ambiguous intent'}
    endif
    " If here, input range seems sane; make it inclusive.
    let start = s:yankdel_range__preadjust_range_start(a:start, a:inc[0])
    let end = s:yankdel_range__preadjust_range_end(a:end, a:inc[1])
    let [is_str, is_num] =
        \ [type(a:del_or_spl) == type(''), type(a:del_or_spl) == type(0)]
    " Initialize return dict; positions and cmd may be adjusted below.
    " Note: Initial setting of 'cmd' flag will never indicate directed put.
    let ret = {
        \ 'start': s:nullpos, 'end': s:nullpos, 'pos': s:nullpos,
        \ 'text': is_str ? a:del_or_spl : '',
        \ 'cmd': is_num ? a:del_or_spl ? 'd' : 'y' : 's', 
        \ 'err': 0, 'errmsg': ''
    \ }
    " Is adjusted (inclusive) range empty?
    if sexp#compare_pos(start, end) > 0
        " Adjusted start/end are reversed, yielding empty target region.
        " Treat *non-empty* splice as a directed put, everything else as NO-OP.
        if ret.cmd != 's' || len(ret.text) == 0
            " Empty range is NO-OP for all but non-empty splice!
            let ret.cmd = ''
            return ret
        endif
        " We have a non-empty splice.
        let [i0, i1] = a:inc
        if i0 == 1 || i1 == 1
            " One side inclusive, the other side some form of exclusive
            " Rationale: Getting inside the empty range `if' above guarantees at least one
            " side was exclusive.
            if i0 != 2 && i1 != 2
                " By convention, empty range with one side inclusive and the other side
                " normal-exclusive requests a directed put with direction given by the
                " inclusive side.
                let [ret.pos, ret.cmd] = i0 == 1 ? [start, 'P'] : [end, 'p']
            else
                " One side inclusive, the other side adjacent whitespace-exclusive.
                " Design Decision: Treat like splice over inclusive position.
                " Rationale: Initial validation guarantees input start was *before* input
                " end; thus, the fact that adjusted positions overlap implies that the
                " inclusive end was *within* the whitespace excluded by the other end.
                " There's probably no real use case for this, but safest and most natural
                " thing is just to exclude only the whitespace in the open range
                " determined by start..end: in other words, just splice over the inclusive
                " position.
                let [ret.start, ret.end] = i0 == 1 ? [a:start, a:start] : [a:end, a:end]
            endif
        else
            " Both sides some form of exclusive
            " Handle as directed "put after" from adjusted end, unless adjusted end is
            " before start of buffer, in which case we need to make it a "put before" from
            " start.
            " Rationale: Intended use cases of adjacent whitespace-exclusive are such that
            " when a choice must be made (because adjacent whitespace start/end overlap),
            " it's better to preserve all of the adjacent whitespace at end. As for
            " special BOF case, consider that an adjusted end at virtual BOF position will
            " be forcibly adjusted to [0,1,1,0] before the put, with the result that a
            " forward put from adjusted end would leave the initial buffer char *before*
            " the put text, which is definitely not what we want!
            let [ret.pos, ret.cmd] = end == s:BOF ? [start, 'P'] : [end, 'p']
        endif
    else
        " Adjusted range non-empty; no need to adjust cmd, which is one of [yds]. (All
        " directed puts handled in the if above.)
        let [ret.start, ret.end] = [start, end]
    endif
    return ret
endfunction

" Return a dict recording the following:
" ps:            list of positions requiring adjustment
" op:            dict returned by s:yankdel_range__preadjust_range()
" anchor:        anchor position (start of range for splice/delete, else position at which
"                directed put is performed)
" anchor_byte:   bytes offset of anchor relative to BOF
" end_off:       (optional) byte offset of end of range relative to anchor
"                N/A for directed put
" bytes_in_file: total # of bytes in the file
" byte_offs:     list of byte offsets of each of the provided positions relative to anchor
" delta:         signed byte offset indicating how much the end of file is moved by the
"                operation for which we're adjusting.
" Motivation: After delete/splice, this information can be used to adjust the positions to
" account for added/deleted text.
" Caveat: Because callers generally maintain references to the position tuples in the
" input list, it's vital that we never delete or replace the tuple references: i.e., when
" adjustments are made, the line/col values are modified directly.
" Alternative Approach: This preadjustment step isn't strictly necessary, since number of
" bytes to be deleted could be calculated analytically (even before any buffer
" modifications have occurred) using pos2byte etc on start/end; however, that approach is
" significantly more complex: much simpler/safer to do it using byte2pos after the
" deletion has occurred.
function! s:yankdel_range__preadjust_positions(op, ps)
    " Pre-op position adjustment
    let ret = {'ps': a:ps, 'op': a:op, 'byte_offs': [],
                \ 'anchor': s:nullpos,
                \ 'anchor_byte': -1, 'end_off': -1, 'delta': 0}
    let ret.anchor = a:op.cmd =~? 'p' ? a:op.pos : a:op.start
    " Total # of bytes in file is used to calculate delta later.
    " TODO: Calculate delta up-front (using range and splice text if applicable) to
    " obviate need for bytes_in_file.
    let ret.bytes_in_file = s:total_bytes_in_file()
    " Calculate byte offset of anchor pos wrt BOF and (if not directed put) end wrt anchor
    " pos (i.e., start).
    let ret.anchor_byte = s:pos2byte(ret.anchor)
    " Note: end_off is N/A for directed put.
    if a:op.cmd !~? 'p'
        let ret.end_off = s:pos2byte(a:op.end) - ret.anchor_byte
    endif
    " Calculate delta.
    " TODO: Once this approach to delta calculation has been thoroughly vetted, remove
    " 'bytes_in_file' and associated code (both here and in postadjust function).
    let ret.delta = a:op.cmd =~? '[sp]' ? len(a:op.text) : 0
    if a:op.cmd =~ '[sd]'
        " Assumption: We have a non-empty range.
        " Note: Use position *past* end to account for bytes in final char of range.
        let ret.delta -=
            \ (s:pos2byte(sexp#offset_char(a:op.end, 1, 1)) - s:pos2byte(a:op.start))
    endif
    " Calculate and store offsets of positions of interest wrt start of range.
    for p in a:ps
        " Decision Decision: Add even the negative offsets, which require no adjustment.
        " Rationale: Discarding them would entail removing elements from a:ps to avoid
        " breaking index correlation.
        call add(ret.byte_offs, s:pos2byte(p) - ret.anchor_byte)
    endfor
    return ret
endfunction

" Adjust the positions in the input dict, assumed to have been built by
" yankdel_range__preadjust_positions()) to reflect the text added/removed by a
" splice/delete operation.
function! s:yankdel_range__postadjust_positions(adj)
    " Post-op position adjustment
    " Calculate net byte delta (added (+) / deleted (-))
    " FIXME: Remove this after validating the new (preferred) approach (i.e., calculating
    " delta in preadjust function).
    let delta = s:total_bytes_in_file() - a:adj.bytes_in_file
    if delta != a:adj.delta
        " FIXME: Resolve this!!!!!!
        let delta = a:adj.delta
        "throw "Mismatch in calculated deltas: post-delta=" . delta . " pre-delta=" . a:adj.delta
    endif
    let [op, ps, offs] = [a:adj.op, a:adj.ps, a:adj.byte_offs]
    let [anchor, anchor_byte] = [a:adj.anchor, a:adj.anchor_byte]
    if op.cmd !~? 'p'
        " Perform end offset processing (not applicable to directed puts).
        " Get adjusted e_off, which will be used within the loop.
        let e_off = a:adj.end_off
        let e_off_adj = e_off + delta
        let e_adj = s:byte2pos(anchor_byte + e_off_adj)
        if sexp#compare_pos(anchor, e_adj) > 0
            " Ensure that if range is completely deleted (either with empty splice or
            " delete), positions don't fall backwards past the original start, but forward
            " to first char of non-deleted text.
            let e_adj = anchor
        endif
    endif
    " Iterate parallel lists ps and offs.
    for i in range(len(offs))
        let [o, p] = [offs[i], ps[i]]
        " Distinction: For 'put before' case, adjustments required for all positions >=
        " start (o >= 0); for all other cases (including put after), only positions
        " *after* start (o > 0) require adjustment.
        if o > (op.cmd ==# 'P' ? -1 : 0)
            " Important Note: For non-directed put (i.e., splice or delete), there are 3
            " possible cases for the position being adjusted:
            " 1. original position within both original and adjusted ranges
            " 2. original position within original but not adjusted range
            " 3. original position past original range (and thus, also past adjusted
            "    range)
            " Both put before and put after can be handled as case #3; the only difference
            " between the two is the offset threshold (checked above).
            " Design Decision: Originally, used position calculated from original byte
            " offset for case 1, but this is problematic because cleaning up whitespace
            " (e.g., trailing spaces) can result in an adjusted position on a different
            " line, even when the original line still exists. A better approach for cases
            " 1 & 2 is to try to use the original line/col (limiting col to '$'-1),
            " falling back to the adjusted end position if the aforementioned position is
            " past adjusted end.
            " Caveat: Because callers may hold references to the position lists, it's
            " *vital* that we replace the list elements *without* replacing the list
            " references.
            if op.cmd !=? 'p' && o <= e_off
                " Case 1 or 2
                if sexp#compare_pos(p, e_adj) >= 0
                    " Case 2: Don't allow position to escape from adjusted range.
                    let [p[1], p[2]] = e_adj[1:2]
                else
                    " Case 1: Attempt to use original position, but limit col to the
                    " current line length, which could differ from the original.
                    let ecol = col([p[1], '$'])
                    if p[2] >= ecol
                        let p[2] = max([1, ecol - 1]) " max() needed to handle blank lines
                    endif
                endif
            else
                " Case 3 *and* directed puts
                let [p[1], p[2]] = s:byte2pos(anchor_byte + o + delta)[1:2]
            endif
        endif
    endfor
endfunction

" Yank, delete or splice text in range defined by start/end, returning any deleted text,
" and optionally adjusting input positions to account for deletions.
" Splice Note: If a string (rather than a boolean flag) is supplied for del_or_spl, the
" range will be *replaced* by the provided text and the replaced text will be returned.
" -- Optional Args --
" a:1  range inclusivity
"      Enum:
"        0=exclusive
"        1=inclusive
"        2=exclusive of cursor pos as well as any adjacent end-of-line whitespace,
"          including the newline itself
"      Formats:
"        both_inclusive
"      | [start_inclusive, end_inclusive]
"      Defaults to inclusive start, exclusive end (i.e., [1, 0].
"      FIXME: This design choice feels a bit risky, as it violates the POLS. Consider
"      defaulting both ends to the same thing.
" a:2  list of positions to adjust
"      Note: As a convenience to caller, will be passed through s:concat_positions().
" a:3  failsafe_override - unless this flag is set, function will not delete
"      non-whitespace. (defaults to 0)
" Position Adjustment: Any provided list of positions is modified in-place to account for
" any deletions. The goal is to preserve position in an intelligent manner: i.e., wherever
" possible, a position should point to the same character before and after the operation.
" If this is not possible, we should do the next best thing, which typically means
" adjusting the position to a deterministic location near the head or tail of the operated
" region.
" TODO: Idea: Could passing 'splice' arg of one or more newlines obviate need
" for the special inc==2 value??? Think on this...
" Failsafe Override: This feature was added to ensure that a bug cannot result in the
" deletion of non-whitespace text in a user's buffer. The manifestation of a bug may be
" annoying, but loss of user's text could be catastrophic. (Of course, undo can be used to
" remedy the loss of text, but probably not the loss of confidence.)
" Failsafe override can be set either by user (g:sexp_inhibit_failsafe == 1) *or* via
" plugin logic.
" Note: Currently, plugin logic never sets it because there are no commands that modify
" non-whitespace.
" Cursor Positioning: Let cursor position fall where it naturally would after
" yank/put/delete.
" TODO: In lieu of optional args, use replace everything beyond 'end' with a dict of
" optional key/value pairs. This will help make call sites more self-documenting.
function! s:yankdel_range(start, end, del_or_spl, ...)
    let ret = ''
    " TODO: Probably stop saving and adjusting cursor.
    let cursor = getpos('.')
    " Assumption: 'virtualedit' has been set to onemore (by pre-op handler).
    " Rationale: Need to be able to select (visually) past EOL in certain
    " cases (e.g., non-inclusive start at EOL).
    let reg_save = [@a, @"]
    try
        " FIXME: I don't like using a:1 only for tail default!!!!
        let inc = a:0 ? type(a:1) == 3 ? a:1 : [1, a:1] : [1, 0]
        let failsafe_override = g:sexp_inhibit_failsafe || (a:0 > 2 ? a:3 : 0)
        " Canonicalize start/end/range.
        let op = s:yankdel_range__preadjust_range(a:start, a:end, a:del_or_spl, inc)
        if op.err
            call sexp#warn#msg("yankdel_range: Internal error detected: " . op.errmsg)
            return
        elseif empty(op.cmd)
            " NO-OP
            return
        endif
        " See header comment on failsafe mechanism.
        " Note: Failsafe doesn't apply to a directed put.
        if !failsafe_override && op.cmd =~# '[ds]'
            \ && sexp#range_has_non_ws(op.start, op.end, 1)
            call sexp#warn#msg("yankdel_range: Internal error detected:"
                \ . " refusing to modify non-whitespace!")
            return
        endif
        " Perform splice, directed put, delete or yank.
        if op.cmd != 'y'
            " Buffer is being changed...
            " Perform pre-op position adjustment
            " Start: If we're doing a put before, pre-adjustment needs to use a start
            " position prior to a:start; otherwise, the original start position will
            " not be adjusted, despite coming after the put text.
            " End: Treat deletion and non-null splice differently: for a deletion, end
            " should be *past* the deleted text, but for a non-empty splice, use the end
            " of the spliced area.
            " TODO: Decide whether *past* means newline or beginning of next line.
            " Rationale: Looks best to keep cursor within replacement area (typically
            " whitespace) when text is spliced, but to let it fall forward into undeleted
            " text when deleting.
            " TODO: Don't use a:0 this far into function! Rework...
            " TODO: Evaluate the special logic here. Is it necessary? Correct since refactor?
            let adj = s:yankdel_range__preadjust_positions(op,
                \ a:0 > 1 ? s:concat_positions(a:2, cursor) : [cursor])
        endif
        " Perform cursor positioning or visual selection for yank/del/splice.
        if op.cmd =~? 'p'
            " directed put
            call s:setcursor(op.pos)
        else
            " Select text to be yanked/deleted/spliced
            call s:set_visual_marks([op.start, op.end])
            call s:select_current_marks('v')
        endif
        if op.cmd =~# '[spP]'
            " Splice or directed put
            let @a = op.text
            " Caveat: Need to treat splice text ending in newline specially to inhibit
            " linewise put.
            let linewise = @a[-1:] == "\n"
            if linewise
                " Make the register non-linewise.
                let @a .= ' '
            endif
            " Note: We can be in either visual or normal mode at this point. Action to
            " perform depends on op.cmd:
            "   [pP]: directed put from cursor pos
            "   s: splice over visual selection (implemented with visual `p').
            silent! exe 'normal! "a' . (op.cmd ==# 'P' ? 'P' : 'p')
            if @a =~ '^\n'
                " Vim Idiosyncrasy Workaround: If the head of the put text is newline, Vim
                " sets '[ to whatever precedes the newline, but we want it to be the
                " newline itself.
                " Assumption: Upstream has set ve=onemore.
                call s:setcursor(sexp#offset_char(getpos("'["), 1, 1))
                normal! m[
            endif
            if linewise
                " Delete the space we appended to inhibit linewise put.
                " Save [ and ] marks for restoration after space deletion.
                " Note: Using [gs]etpos() with the [ and ] marks is problematic, as it
                " always sets/gets '[ and '] (not `[ and `]).
                " Solution: Use getpos('.') to obtain positions reached by `[ and `], then
                " m[ and m] to set `[ and `] to those positions after the deletion.
                " TODO: Decide whether ] should be backed up to end of preceding line.
                execute 'normal! `[' | let smark = getpos('.')
                execute 'normal! `]' | let emark = getpos('.')
                " Cleanup the space that was appended to inhibit linewise put.
                normal! `]"_x
                " Restore the [ and ] marks, which are clobbered by normal
                " x, even when lockmarks is used.
                call s:setcursor(smark) | normal! m[
                call s:setcursor(emark) | normal! m]
                let @" = @"[:-2]
            endif
            if op.cmd ==# 's'
                " Return the spliced-over text.
                let ret = @"
            endif
        else
            " Perform yank or delete.
            silent! exe 'normal! ' . '"a' . op.cmd
            " Return the deleted/yanked text.
            let ret = @a
        endif
        if op.cmd !=# 'y'
            " Post-op position adjustment (required whenever buffer changes).
            call s:yankdel_range__postadjust_positions(adj)
        endif
    finally
        " Restore options/regs/cursor...
        let [@a, @"] = reg_save
        " TODO: Decide on this.
        "call s:setcursor(cursor)
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
    " FIXME: We're already doing this in sexp#pre_op(); is there a reason to do it here
    " too?
    let ve_save = &ve
    set ve=onemore
    try
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

" Build and return a dict containing both the input position list and a dict mapping the
" line numbers represented to the corresponding line end (col('$')).
" Rationale: This information is required by indent_postadjust_positions() to adjust
" positions after indentation has been performed on the lines containing the positions.
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

" Use the information in the input dict (recorded by indent_preadjust_positions()) to
" adjust the positions in adj.ps[] for the indent that has just occurred.
function! s:indent_postadjust_positions(adj)
    let line_ends = a:adj.line_ends
    for p in a:adj.ps
        " Note: I suppose that for a position inside leading whitespace, the
        " optimal logic might calculate the % visual distance (not byte
        " distance) into the ws and try to preserve it across indent, but this
        " is a lot of complexity for a small potential advantage.
        " Decision: For now, stay on this side of point of diminishing returns
        " by preserving byte distance from EOL.
        " TODO: Consider using screen position as I do with comment alignment.
        let p[2] -= line_ends[p[1]] - col([p[1], '$'])
    endfor
endfunction

" Go to position of last non-whitespace char on specified line, else leave position
" unchanged.
" Return col of last non-ws, else 0.
function! s:goto_last_non_ws(line)
    let ecol = col([a:line, '$'])
    if ecol == 1
        " Empty line
        return 0
    endif
    " Position past (or on, depending on 've') last char.
    keepjumps call cursor(a:line, ecol)
    " Find last non-white char on line.
    " Note: 'c' flag needed because default 've' setting doesn't allow us to start *past*
    " the last char.
    if search('\S', 'bWc', a:line)
        " We're on non-whitespace.
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

" Return a dict with the following keys to characterize the specified line:
"   ecol:         alignment col (earliest screen col at which aligned comment could begin,
"                 taking options into account.
"                 Note: Return the align col even for a line without an eol comment, since
"                 in at least one configuration, non-eol comment lines affect alignment.
"   is_com:       1 iff specified line ends in comment (eol or otherwise)
"   is_eol_com:   1 iff specified line ends in eol comment
"   prev_e:       position of last non-ws preceding eol comment
"   com_s:        position of start of eol comment (else s:nullpos)
"   comlen:       length of trailing comment (else 0)
" Cursor Preservation: None (caller expected to handle)
" Terminology: There's an ambiguity inherent in the phrase "ends in comment": if an
" *inline* (self-contained, not eol-style) comment occurs at the end of a line, but is
" followed by trailing whitespace, the line would technically not *end in* a comment; in
" fact, technically speaking, the line would not end in a comment even if the final
" character were the inline comment terminator. However, most users would probably want an
" eol inline comment following a sexp to be treated as an end of line comment. With the
" current logic, it will be. For one thing, the calling logic has most likely already
" stripped off trailing whitespace, with the result that the last character on the line
" will be part of the comment. But even if it hasn't, this function performs the
" sexp#is_comment() test on the final *non-ws* char of the line, not the final char of the
" line.
function! s:aligncom__characterize(line)
    " Note: It's not an eol comment if there's nothing before it.
    let c = s:goto_last_non_ws(a:line)
    if !c
        " Effectively empty line
        return {
            \ 'ecol': 1, 'is_com': 0, 'is_eol_com': 0,
            \ 'prev_e': s:nullpos, 'com_s': s:nullpos, 'comlen': 0
        \ }
    endif
    " This will be adjusted later for eol comment, but if there's no eol comment, we'll
    " need screen pos just past last non-ws for alignment purposes.
    let eol_ecol = virtcol('.') + 1
    " These two will be adjusted below for lines with eol comments.
    let ecol = eol_ecol
    let [eff_linelen, comlen] = [0, 0]
    " Figure out which type of comment (if any) we have.
    let [is_eol_com, prev_e, com_s] = [0, s:nullpos, s:nullpos]
    let is_com = sexp#is_comment(a:line, c)
    if is_com
        " Find start of comment.
        let com_s = s:current_comment_terminal(0)
        call s:setcursor(com_s)
        " Is this an eol comment? I.e., is there something before it?
        " This pattern will match the last char of the preceding non-whitespace, with
        " [0,0] indicating this is not an eol comment.
        let [l, c] = searchpos('\S', 'bW', a:line)
        let is_eol_com = !!l
        if is_eol_com
            " Important Note: What we need for alignment is screen col, not col(); use
            " virtcol(), which existed in our prerequisite Vim version 7.3. (screenpos was
            " added later.)
            " Design Decision: There's really no reason to use current_element_terminal(1)
            " or current_comment_terminal(1) here, since last non-ws is always desired for
            " alignment purposes.
            let prev_e = [0, l, c, 0]
            " Save the *alignment* screen col, taking preferred separation and 'stop
            " modulus' into account.
            let ecol = virtcol('.') + g:sexp_aligncom_min_separation + 1
            if g:sexp_aligncom_colstops
                " Adjust alignment col for 'colstops'.
                let mod = ecol % g:sexp_aligncom_colstops
                if mod
                    let ecol += g:sexp_aligncom_colstops - mod
                endif
            endif
            " Calculate *effective* line length as the shortest possible line (in screen
            " cols), taking any required pre-eolc comment spacing into account.
            " Note: ecol is the first screenpos at which aligned eol comment could be
            " located.
            " TODO: Do we need to consider possibility that comment could contain tabs,
            " whose screen width could change if comment is shifted? Possibly not, since
            " eff_linelen is used for cost calculation, not alignment.
            let eff_linelen = eol_ecol - 1 - (virtcol([com_s[1], com_s[2]]) - ecol)
            " Also calculate length of trailing comment, which is needed by overrun logic.
            let comlen = eff_linelen - prev_e[2]
        endif
    endif
    return {
            \ 'ecol': ecol, 'is_com': is_com, 'is_eol_com': is_eol_com,
            \ 'comlen': comlen, 'prev_e': prev_e, 'com_s': com_s
    \ }
endfunction

" Calculate values for aligncom options whose value determination is deferred till the
" point of execution and return them in a dict.
" Motivation: Avoid repeated recalculation within inner loops.
function! s:aligncom__get_deferred_opts()
    return {
            \ 'weights': s:aligncom__get_weights(),
            \ 'textwidth': s:aligncom__textwidth(),
            \ 'maxshift': s:aligncom__maxshift(),
    \ }
endfunction

" Calculate and return the criteria weights, taking user options into account.
function! s:aligncom__get_weights()
    " Design Question: Should we enforce use of integers in user weights? Probably no need
    " to, as long as values are within range.
    " Build dict of scalar weights, taking into account the plugin-defined defaults and
    " any user-requested adjustment.
    let ret = {}
    for [k, d] in items(s:aligncom_weights)
        let user_adj = get(g:, 'sexp_aligncom_' . k . '_weight', -1)
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

" Return an effective eolc group len, which takes into account both the number of comments
" *and* the number of lines spanned by the group.
" Rationale: Both properties influence the size perceived by the user: e.g., for a densely
" commented group, the span is probably more important than the number of comments; OTOH,
" a group with 'bookend' comments spanning a large number of lines probably seems
" significantly smaller than a dense group with the same span.
" TODO: This is currently unused! Decide whether/where it should be or remove... Does
" 'density' obviate need for it? Should runtness calculation consider effective grp len?
function! s:aligncom__effective_grplen(grplen, linespan)
    " TODO: For now, just use average, but if this is kept, consider whether this is best.
    return (a:grplen + a:linespan) / 2
endfunction

" Return signed value indicating results of group cost comparison.
" < 0: cost1 <  cost2
" > 0: cost1 >  cost2
" = 0: cost1 == cost2
function! s:aligncom__compare_costs(dp, grp1, grp2, opts)
    let [g1, g2] = [a:grp1, a:grp2]
    let [c1, c2] = [g1.cost, g2.cost]
    " Each enabled criterion adjusts this variable by signed amount.
    " Sign Logic: For negative (cost) criteria (e.g., overrun or runtness), subtracting g2
    " from g1 yields a negative result when group 1 is the better option.
    let ret = 0
    let weights = a:opts.weights
    " Note: These variables are maintained for algorithm evaluation/debugging.
    " TODO: Consider removal after development...
    let [ngrps, shift, density, runt, overrun, max_overrun] = [0, 0, 0, 0, 0, 0]
    let [ngrps1, ngrps2, spl1, spl2, density1, density2,
                \ runt1, runt2, overrun1, overrun2, max_overrun1, max_overrun2] =
                \ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    " -- Numgroups --
    " Note: numgroups can't be totally disabled, else alignment would be a NOP.
    " Normalize to a *multiple* of the number of groups corresponding to all groups having
    " the minimum non-runt length.
    " Note: The 'default' weight for ngrps is reduced relative to the other criteria to
    " prevent disporportionate weighting. Testing has shown this to be unnecessary when we
    " use a constant normalization (e.g., straight pct diff) for area/shift, but when we
    " use maxshift to normalize the shift, the reduced weight is required to prevent shift
    " from being effectively ignored.
    let ngrps1 = c1.cumul.ngrps / (1.0 * c1.cumul.nlines / g:sexp_aligncom_runt_thresh)
    let ngrps2 = c2.cumul.ngrps / (1.0 * c2.cumul.nlines / g:sexp_aligncom_runt_thresh)
    let ngrps = weights.numgroups * (ngrps1 - ngrps2)
    let ret += ngrps
    " -- Shift (area under curve) --
    if weights.shift > 0
        " Note: The pct diff approach gives decent results, but doesn't take the maxshift
        " into account; intuitively, although less shift is generally better, how much
        " better should depend on the max allowed shift. Thus, I'm using the normalization
        " to maxshift approach until I find a reason not to.
        let use_pct_diff = 0
        if use_pct_diff
            " Use pct diff between the cumulative areas.
            let [shift1, shift2] = [c1.cumul.area, c2.cumul.area]
            let norm = 0.5 * (shift1 + shift2)
            let [shift1, shift2] = [shift1 / norm, shift2 / norm]
        else
            " Normalize to max possible shift.
            let shift1 = 1.0 * c1.cumul.area / (c1.cumul.ncoms * a:opts.maxshift)
            let shift2 = 1.0 * c2.cumul.area / (c2.cumul.ncoms * a:opts.maxshift)
        endif
        let shift = weights.shift * (shift1 - shift2)
        let ret += shift
    endif
    " -- Density --
    " Note: The 'default' weight for density is reduced relative to the other criteria to
    " prevent its overpowering more important criteria.
    if weights.density > 0
        " Normalize to number of lines.
        let density1 = 1.0 * c1.cumul.ncoms / c1.cumul.nlines
        let density2 = 1.0 * c2.cumul.ncoms / c2.cumul.nlines
        let density = weights.density * (density2 - density1)
        let ret += density
    endif
    " -- Runtness --
    if weights.runtness > 0 && (c1.cumul.runt > 0 || c2.cumul.runt > 0)
        " Use pct diff normalization.
        let norm = 0.5 * (c1.cumul.runt + c2.cumul.runt)
        let [runt1, runt2] = [c1.cumul.runt / norm, c2.cumul.runt / norm]
        let runt = weights.runtness * (runt1 - runt2)
        let ret += runt
    endif
    " -- Textwidth overrun --
    if weights.textwidth > 0
        " Note: The bias (currently zero), was introduced as a way of softening the impact
        " of a zero in the numerator of a pct diff calculation.
        " TODO: Consider removal if it it's not needed.
        let bias = 0
        let cumul_bias = bias * max([c1.cumul.ncoms, c2.cumul.ncoms])
        if c1.cumul.overrun > 0 || c2.cumul.overrun > 0
            let [overrun1, overrun2] = [c1.cumul.overrun + cumul_bias, c2.cumul.overrun + cumul_bias]
            let norm = 0.5 * (overrun1 + overrun2)
            let overrun = weights.textwidth * 1.0 * (overrun1 - overrun2) / norm
            let ret += overrun
        endif
        if c1.cumul.max_overrun > 0 || c2.cumul.max_overrun > 0
            let [max_overrun1, max_overrun2] = [c1.cumul.max_overrun + bias, c2.cumul.max_overrun + bias]
            " Design Decision: Consider max overrun, but give it only a fraction of the weight
            " of average overrun (hence, the 0.25).
            let norm = 0.5 * (max_overrun1 + max_overrun2)
            let max_overrun = weights.textwidth * 0.25 * (max_overrun1 - max_overrun2) / norm
            let ret += max_overrun
        endif
    endif
    "call s:Dbg("weights: overrun=%f", weights.textwidth)
    "call s:Dbg("       ngrps: %f %f", ngrps1, ngrps2)
    "call s:Dbg("       shift: %f %f", shift1, shift2)
    "call s:Dbg("        runt: %f %f", runt1, runt2)
    "call s:Dbg("    overruns: %f %f", overrun1, overrun2)
    "call s:Dbg("max_overruns: %f %f", max_overrun1, max_overrun2)
    "call s:Dbg("Criteria Impacts:\n\tngrps:\t\t%f\n\tshift:\t\t%f\n\tdensity:\t%f"
    "            \ . "\n\truntness:\t%f\n\toverrun:\t%f\n\tmaxover:\t%f",
    "            \ ngrps, shift, density, runt, overrun, max_overrun)
    " Return the signed comparison value.
    return ret
endfunction


" Calculate and return a dict representing the specified candidate group.
" The dict must contain a cost dict with everything needed by the comparison function to
" compare this candidate with another.
" Inputs:
"   dp:         dynamic programming state list with one element for each eol comment 
"   sidx:       index of element corresponding to start of candidate group
"   eidx:       index of element corresponding to end of candidate group
"   area:       "area under the curve" for group being created: i.e., sum of all gaps
"               between end of code and start of eol comment
"   margin:     cumulative (min) margin, negative indicating textwidth overrun
"   comlen_sum: sum of trailing comment lengths, needed to calculate average
"   align:      alignment column for all eol comments in this group
"   dp_sidx:    start of region on which dp[] is being performed (always 0 for opt_lvl 2)
"               Note: Prevents looking back too far in opt_lvl 1 ("Greedy with Lookback").
"   opts:       dict of options whose value calculation is deferred
function! s:aligncom__create_group_candidate(
        \ dp, sidx, eidx, area, margin, comlen_sum, align, dp_sidx, opts)
    " Calculate figures of merit for the *current* group.
    let nlines = a:dp[a:eidx].line - a:dp[a:sidx].line + 1
    let ncoms = a:eidx - a:sidx + 1
    " Calculate runtness now, though it won't be used till this element is at sidx-1.
    " TODO: Should this take "effective" group len into account?
    " Note: Runtness is a step function equal to square of the delta between actual group
    " size and runt threshold when size is under the threshold, else 0.
    let runt = nlines < g:sexp_aligncom_runt_thresh
            \ ? (g:sexp_aligncom_runt_thresh - nlines) * (g:sexp_aligncom_runt_thresh - nlines)
            \ : 0
    " Convert negative margin to positive overrun, with no penalty for nonnegative margin.
    let max_overrun = a:margin < 0 ? -a:margin : 0
    " Augment the worst-case figure of merit by adding a value that reflects the *total*
    " amount of overrun.
    " Rationale: Considering only worst-case overrun tends to allow large groups with lots
    " of overrun, since once the penalty is incurred for worst-case (as it must be for a
    " long line), it might as well be amortized... To avoid this, we need a penalty for
    " additional overrunning lines, but calculating it for all lines individually is
    " problematic for performance reasons. A simpler, yet still effective strategy, is to
    " use the product of the *average* trailing comment length and the number trailing
    " comments in group.
    " Note: The float2nr() is needed to avoid mixing float and numbers error E805.
    let overrun = max(
        \ [0, float2nr(a:align + 1.0 * a:comlen_sum / ncoms - a:opts.textwidth - 1)]) * ncoms
    " Save the current group's (non-cumulative) values
    let self = {'area': a:area, 'overrun': overrun, 'nlines': nlines,
                \ 'ncoms': ncoms, 'runt': runt, 'comlen_sum': a:comlen_sum}
    " Note: dp_sidx may be nonzero for opt_lvl 1 ("greedy with lookback").
    if a:sidx > a:dp_sidx
        " Get previous grp in chain to support accumulation.
        let pgrp = a:dp[a:sidx - 1].grp
        " Aggregate cost-related data that applies to the current candidate and its chain
        " of predecessors.
        "call s:Dbg("Creating cumul reflecting pgrp ending at idx=%d line %d, runt=%f",
        "            \ a:sidx - 1, a:dp[a:sidx - 1].line, pgrp.cost.cumul.runt)
        let cumul = {
                    \ 'ngrps': pgrp.cost.cumul.ngrps + 1,
                    \ 'nlines': pgrp.cost.cumul.nlines + nlines,
                    \ 'ncoms': pgrp.cost.cumul.ncoms + ncoms,
                    \ 'area': pgrp.cost.cumul.area + a:area,
                    \ 'max_overrun': max([pgrp.cost.cumul.max_overrun, max_overrun]),
                    \ 'overrun': pgrp.cost.cumul.overrun + overrun,
                    \ 'runt': pgrp.cost.cumul.runt + runt,
        \ }
    else
        " First element needs no accumulation.
        " TODO: Toying with idea that we don't really need a cost structure for the first
        " element. The only thing it's really needed for is to support cost accumulation,
        " once it becomes an sidx-1 group, but this could be handled specially.
        " Rationale: An only element is always best.
        let cumul = {
            \ 'ngrps': 1, 'nlines': nlines, 'ncoms': ncoms, 'area': a:area,
            \ 'max_overrun': max_overrun, 'overrun': overrun, 'runt': runt
        \ }
    endif
    " TODO: If self ends up not being required, pull cumul up.
    let cost = {'self': self, 'cumul': cumul}
    " Wrap all into a single grp dict.
    " Note: 'sidx' is needed to support backwards traversal of group chain.
    let ret = {'align': a:align, 'sidx': a:sidx, 'cost': cost}
    return ret
endfunction

" Return the effective maxshift, which may depend on effective 'textwidth'.
" Use 75% of nonzero effective 'textwidth' if global option is -1 (default); otherwise,
" use the value specified by user.
" Design Decision: Although 'maxshift' of zero would effectively disable alignment,
" respect user's wishes.
" Design Decision: Use default of 80 cols if effective 'textwidth' is zero (equivalent to
" texwidth_weight == 0).
" Rationale: It would make no sense to default 'maxshift' to zero.
function! s:aligncom__maxshift()
    let tw = s:aligncom__textwidth()
    return g:sexp_aligncom_maxshift < 0
            \ ? 75 * (tw > 0 ? tw : 80) / 100
            \ : g:sexp_aligncom_maxshift
endfunction

" Return the preferred max line length (in screen cols) for lines with trailing comments.
" Note: Returning zero effectively causes textwidth to be ignored as alignment criteria:
" i.e., same as setting weight to 0.
function! s:aligncom__textwidth()
    return g:sexp_aligncom_textwidth < 0 ? (&tw ? &tw : 80) : g:sexp_aligncom_textwidth
endfunction

" Return a possibly updated min_margin, taking both current min and most-recently added
" element of candidate group into account.
" Inputs:
"   min_margin:  the candidate group's current worst-case margin
"   el:          element of eol comment being added to group
"   align:       current align col
"   shift_inc:   amount that new element has shifted the align col (0 if no shift)
"   opts:        dict of options whose value calculation is deferred
function! s:aligncom__update_min_margin(min_margin, el, align, shift_inc, opts)
    let min_margin = a:min_margin
    if a:shift_inc > 0
        " Current worst-case margin just got worse.
        let min_margin -= a:shift_inc
    endif
    " See whether new element's margin is worse than current worst.
    let el_margin = a:opts.textwidth - (a:align + a:el.comlen - 1)
    if el_margin < min_margin
        let min_margin = el_margin
    endif
    return min_margin
endfunction

" Update the Dynamic Programming state list element corresponding to the input line.
" Each invocation chooses the best of all candidate groups that end at this line. Of
" course, there is no guarantee that the selected group will be in the final group list,
" as it may happen that a longer group containing this element or a group *beginning* at
" this element is better. Only after the final element has been processed can we determine
" the globally optimal set of groups by following group start indices backwards, starting
" with the final element, which is necessarily the end of the final group.
" Inputs:
"   dp:          dynamic programming state list with one element for each eol comment
"   idx:         current index
"   dp_sidx:     start of the region on which to perform DP (always 0 for opt_lvl 3)
"   opts:        dict of options whose value calculation is deferred
function! s:aligncom__update_dps(dp, idx, dp_sidx, opts)
    let el = a:dp[a:idx]
    " Initialize DP state for current comment and add it to list.
    " Note: The 'grp' field will be updated within loop to reflect current best group
    " candidate.
    let el.grp = {}

    " Keep running sum of gaps between end of code and start of aligned eol comment: i.e.,
    " integral under curve represented by the space between eol comment and end of code.
    let area = 0
    " Keep up with sum of trailing comment lengths to support overrun logic.
    let comlen_sum = 0
    " Keep up with worst-case textwidth overrun per candidate group.
    let tw = a:opts.textwidth
    let min_margin = tw > 0 ? s:MAXCOL : 0
    " Keep up with longest line in group for the purpose of overrun calculation.
    let maxcol = 0
    " Keep up with horizontal extents of the "box" containing end of code for all lines in
    " the group. Each time we move to earlier line, we must account for lines with no eol
    " comment if bounded by line with eol comment.
    " Also keep up with previous ecol_max, which lets us know how much the "curve" whose
    " integral we're interested in shifts upward each iteration, thereby increasing
    " cumulative 'area' by the area of the rectangle associated with the shift.
    " TODO: Consider renaming box_{min,max} or adding a suffix to avoid confusion between
    " the 2 uses of 'ecol_max' (one referring only to lines between eol comment lines).
    let [ecol_min, ecol_max, ecol_max_prev] = [el.ecol, el.ecol, -1]
    " Loop over dp state backwards, starting with current element.
    " Optimization TODO: Handle single-element group outside loop with dedicated, more
    " efficient logic, skipping the loop altogether in special case of first element or
    " start-of-group. As it is now, the single-element group is not treated specially.
    let i = a:idx
    while i >= 0
        " Get DP element representing current group start candidate.
        let el_s = a:dp[i]
        " Update bounding box assuming this candidate is valid, then verify.
        if el_s.ecol < ecol_min | let ecol_min = el_s.ecol
        elseif el_s.ecol > ecol_max | let ecol_max = el_s.ecol
        endif
        "call s:Dbg("Trying new head: i=%d line=%d min=%d max=%d", i, el_s.line, ecol_min, ecol_max)
        " Is this a valid start candidate? I.e., is bounding box still within limits?
        if ecol_max - ecol_min > a:opts.maxshift
            " We've gone too far: no more candidate groups ending at current line.
            break
        endif
        " This is a candidate group. Determine its effect on integral.
        let area += ecol_max - el_s.ecol
        let shift_inc = 0
        if  g:sexp_aligncom_shift_weight > 0 && ecol_max_prev > 0 && ecol_max > ecol_max_prev
            " Account for area increase due to entire "curve" shifting up by constant amount.
            " Note: Shift could be due to either the newly-considered eol comment *or*
            " non-comment lines between elements.
            let shift_inc = ecol_max - ecol_max_prev
            let area += (ecol_max - ecol_max_prev) * (a:idx - i)
        endif
        " As we work backwards, keep up with the worst-case margin.
        if g:sexp_aligncom_textwidth_weight > 0 && tw > 0
            let min_margin = s:aligncom__update_min_margin(min_margin, el_s, ecol_max, shift_inc, a:opts)
            let comlen_sum += el_s.comlen
            "call s:Dbg("min_margin=%d comlen_sum=%d", min_margin, comlen_sum)
        endif
        let ecol_max_prev = ecol_max
        " Create group candidate and calculate its cost.
        let grp = s:aligncom__create_group_candidate(
                \ a:dp, i, a:idx, area, min_margin, comlen_sum, ecol_max, a:dp_sidx, a:opts)
        "call s:Dbg("Created candidate grp for comparison:")
        "call s:Dbg("\texisting (%02d-%02d): %s",
        "            \ el.sog || empty(el.grp)
        "            \ ? el.line : a:dp[el.grp.sidx].line, el.line, string(el.grp))
        "call s:Dbg("\tcandidat (%02d-%02d): %s", el_s.line, el.line, string(grp))
        " Note: Comparison value < 0 indicates cost of lhs arg (current best) is still the
        " lowest. In case of tie, we keep existing best, since it's later-starting.
        if el.sog || empty(el.grp) || s:aligncom__compare_costs(a:dp, el.grp, grp, a:opts) > 0
            " Make this the new best candidate.
            let el.grp = grp
            "call s:Dbg("New best (%d-%d)", el_s.line, el.line)
        else
            "call s:Dbg("Kept existing grp (%d-%d)", a:dp[el.grp.sidx].line, el.line)
        endif
        if el_s.sog
            " Don't look back any further if preprocessing has determined this to be an
            " unconditional start of group.
            " TODO: Consider treating sog as special case before the loop.
            break
        endif
        if !g:sexp_aligncom_ignore_non_comments
            " Before moving to previous element, adjust right edge of bounding box to
            " include any long, non-eol-comment lines in the interval between previous and
            " current.
            " Note: ecol_{min,max} have already been adjusted for the eolc line itself.
            let ecol_max = max([el_s.pre_max, ecol_max])
        endif
        let i -= 1
    endwhile
    "call s:Dbg("Finished optimizing idx=%d (line=%d)", a:idx, a:dp[a:idx].line)
    "call s:Dbg("Best grp: %s", string(a:dp[a:idx]))
endfunction

" Convert the dp state list, which maintains groups in a reverse chain) to a list of
" groups in the following more convenient format:
" [
"   {'align': <number>,
"    'eolcs': [
"      {'prev_e': <pos>,
"       'com_s': <pos>}, ...]}, ... ]
" -- Args --
" dp:      the list produced by optimization/layout
function! s:aligncom__finalize_groups(dp, opts)
    " Loop over elements in reverse, skipping nodes that are not the end of a group.
    " Assumption: Final element always ends a group.
    let eidx = len(a:dp) - 1
    let grps = []
    while eidx >= 0
        let el = a:dp[eidx]
        let grp = {'align': el.grp.align, 'eolcs': []}
        " Add elements to the group.
        let idx = el.grp.sidx
        while idx <= eidx
            " Accumulate a single eol comment line item with all the information required
            " to align it.
            let o = a:dp[idx]
            " Note: No need to save 'line', since it's inherent in the positions.
            call add(grp.eolcs, {'com_s': o.com_s, 'prev_e': o.prev_e})
            let idx += 1
        endwhile
        " Accumulate group, then move to last element of previous group.
        call add(grps, grp)
        "call s:Dbg("Finalized grp: sidx=%d eidx=%d align=%d", el.grp.sidx, eidx, el.grp.align)
        let eidx = el.grp.sidx - 1
    endwhile
    " Since the list was built in reverse order...
    return reverse(grps)
endfunction

" TODO: This function is used only for debug. Eventually remove...
function! s:dbg_show_eolcs(grps)
    let idx = 0
    for grp in a:grps
        "call s:Dbg("Group %d align=%d", idx, grp.align)
        " Loop over group members...
        for eolc in grp.eolcs
            "call s:Dbg("com_s: %s prev_e: %s", string(eolc.com_s), string(eolc.prev_e))
        endfor
        let idx += 1
    endfor
endfunction

" Iterate over the elements in the output of the preprocessor stage, optimizing eol
" comment layout, taking optimization level into account. Note that we don't technically
" need optimization level, as the requisite information is all in the preproc dict, but
" having it allows us to skip checks when full optimization is requested.
" Post Condition: This function modifies input list to build a reverse chain of groups in
" the format required by s:aligncom__finalize_groups().
" Note: Handles all 3 optimization levels.
" Optimization Level Specific Logic:
" 0 ("Greedy")               all segments have a 'seg' key containing eidx and alignment,
"                            which can be used to build the reverse list.
" 1 ("Greedy with Lookback)  Alternate between greedy and dp-optimization mode: each time
"                            a 'seg' key with is_greedy set is encountered, fully process
"                            as greedy segment and advance past the segment. Process all
"                            elements which aren't part of a greedy segment with
"                            s:aligncom__update_dps(). (End of non-greedy segment will
"                            be determined by next element with 'seg' key.)
" 2 (Full DP)                All elements processed by s:aligncom__update_dps().
"
function! s:aligncom__optimize_range(prep, opts)
    let [i, N] = [0, len(a:prep)]
    "call s:Dbg("\n\nPre-Prep: %s\n\n", json_encode(a:prep))
    let greedy_eidx = -1
    while i < N
        let el = a:prep[i]
        " Check for mode change indicated by seg key.
        " Assumption: 'seg' will always be present on first element.
        " TODO: Consider changing is_greedy to is_dp.
        if g:sexp_aligncom_optlevel < 2 && has_key(el, 'seg') && el.seg.is_greedy
            " Augment grp dict of final element in the greedy group to add this group to
            " reverse chain.
            let a:prep[el.seg.eidx].grp = { 'align': el.seg.ecol_max, 'sidx': i }
            " Allow natural incrementation at loop end.
            let i = el.seg.eidx
            let greedy_eidx = el.seg.eidx
        else
            " Use DP to find best of candidate groups ending at this element.
            call s:aligncom__update_dps(
                    \ a:prep, i, g:sexp_aligncom_optlevel < 2 ? greedy_eidx + 1 : 0, a:opts)
        endif
        let i += 1
    endwhile
    " Reformat the list for easy group traversal.
    "call s:Dbg("\n\nPost-Prep: %s\n\n", json_encode(a:prep))
endfunction

" Update the 'seg' dict for a group that has just ended (and if necessary, its
" predecessor).
" Inputs:
"   i:            index of element at head of group just begun
"   sidx:         index of first element in group that just ended
"   prev_sidx:    index of first element in group prior to one that just ended, else -1
"   prep[]:       list of dicts characterizing eol comment lines
"   ecol_max:     alignment for group just ended (TODO: rename 'align' or 'aligncol')
"   ecol_maxes[]: alignment at each index in group *prior to* one that just ended
"   opts:         dict of options whose value calculation is deferred
" Note: ecol_maxes[] is used to determine new alignment value if we need to resize a long
" greedy group.
function! s:aligncom__preproc_finalize_seg(
    \ i, sidx, prev_sidx, prep, ecol_max, ecol_maxes, opts)
    " If short group follows long, create a DP optimized region that includes all of the
    " short group and a configurable number of elements at the end of the long group.
    " Factor of 2 used to ensure the first group will be at least as long as the second.
    " TODO: Decide whether lookback and threshold should be distinct.
    "call s:Dbg("finalize_seg: i=%d sidx=%d prev_sidx=%d", a:i, a:sidx, a:prev_sidx)
    if g:sexp_aligncom_optlevel == 0
        " With opt lvl 0, all segments are greedy.
        let a:prep[a:sidx].seg = {'is_greedy': 1, 'eidx': a:i - 1, 'ecol_max': a:ecol_max}
    elseif a:i - a:sidx <= g:sexp_aligncom_greedy_lookback
        \ && a:prev_sidx >= 0 && a:prep[a:prev_sidx].seg.is_greedy
        \ && a:prep[a:prev_sidx].seg.eidx - a:prev_sidx + 1 >= 2 * g:sexp_aligncom_greedy_lookback
        " End of short group following long. Mark transition to dp mode a little before
        " the end of long group.
        " Note: Unlike greedy case, no need to save eidx, since DP logic processes every
        " element up to next 'seg' key.
        " TODO: Would it make sense to reach back even further into the long group,
        " taking care not to make it shorter than g:sexp_aligncom_greedy_lookback?
        let idx = a:sidx - g:sexp_aligncom_greedy_lookback
        let a:prep[idx].seg = {'is_greedy': 0}
        " Prevent DP algorithm from looking back into greedy group.
        let a:prep[idx].sog = 1
        " Shorten the preceding greedy segment.
        let a:prep[a:prev_sidx].seg.eidx = idx - 1
        let a:prep[a:prev_sidx].seg.ecol_max = a:ecol_maxes[-g:sexp_aligncom_greedy_lookback]
    else
        " No special case; mark transition to greedy at start of group just ended.
        " Note: eidx and ecol_max keys may be adjusted later.
        let a:prep[a:sidx].seg =
            \ {'is_greedy': 1, 'eidx': a:i - 1, 'ecol_max': a:ecol_max}
    endif
endfunction

" Fully characterize all eol comments in range, adding a dict for each to the returned
" list. The dict is the one returned by s:aligncom__characterize(), augmented with
" the following:
"   line:     1-based line number
"   pre_max:  the screen column of the longest non-eol comment line in the interval
"             between and element and its predecessor.
" Note that no attempt is made at this stage to break eol comments into groups.
function! s:aligncom__preproc_pass1(start, end, opts)
    " List will contain one element for each eol comment in range.
    let ret = []
    let pre_max = 0  " longest line *between* eol comment lines
    " Loop over lines in range to build the list of dicts used by the algorithm-specific
    " alignment function.
    let [i, l] = [0, a:start[1]]
    while l <= a:end[1]
        " Get all relevant information about the current line (which may or may not
        " contain comment).
        let o = s:aligncom__characterize(l)
        if o.is_eol_com
            " Augment the dict returned by s:aligncom__characterize() to create the
            " element added to list.
            let o.line = l
            let o.pre_max = pre_max
            call add(ret, o)
            let pre_max = 0
        else
            " Adjust pre_max for non-empty, non-comment line.
            " Rationale: If we're not within a group (i.e., haven't seen first eolc),
            " pre_max is irrelevant.
            " Design Decision: Line comments have no impact on alignment (unless they're
            " configured to break groups).
            if o.ecol && !o.is_com
                let pre_max = max([o.ecol, pre_max])
            endif
        endif
        let l += 1
    endwhile
    return ret
endfunction

" Decorate the eol comment dicts in the list produced by preproc pass1 with information
" that facilitates dividing the eol comments into groups. At optimization level 0,
" performs 'greedy' grouping of all elements into 'segments', leaving only the creation of
" the reverse chain for subsequent stages. For optimization level 1, performs greedy
" grouping, but may also create an arbitrary number of segments for which DP optimization
" will be performed. For optimization level 2, implements only the (option-specific) logic
" used to limit the lookback performed by the DP optimization algorithm (e.g., due to line
" comments or too long a run of lines with no eol comments). The 'sog' (start of group)
" key is used to set lookback limits.
function! s:aligncom__preproc_pass2(prep, opts)
    " Distinction: el.sog is needed only by DP optimization loop; unlike local sog, it
    " will not reflect bounding box processing, since the optimization loop does its own.
    let sog = 1                       " start of group flag
    let sidx = -1                     " start of current group
    let prev_sidx = -1                " start of previous group
    let [ecol_min, ecol_max] = [0, 0] " bounding box extents
    let [ecol_min_prev, ecol_max_prev] = [0, 0]
    " Cumulative list of ecol_max at each point in group. Needed only for opt_lvl 1.
    " Note: The _prev version is needed because it applies to the group *before* the one
    " that just ended.
    let [ecol_maxes, ecol_maxes_prev] = [[], []]
    " Loop over lines in range, augmenting segment head elements in input list with a seg
    " dict that will facilitate transition between modes in the optimization/layout
    " function.
    let [i, N] = [0, len(a:prep)]
    while i < N
        let el = a:prep[i]
        " Check for forcible group break conditions.
        if g:sexp_aligncom_break_at_comment && el.is_com && !el.is_eol_com
            " Break at (full) line comment.
            let [sog, el.sog] = [1, 1]
        elseif g:sexp_aligncom_maxgap > 0
            \ && i > 0 && el.line - a:prep[i - 1].line > g:sexp_aligncom_maxgap
            " Gap between eol comment lines is too great to continue any open group.
            let [sog, el.sog] = [1, 1]
        else
            " TODO: Probably do this only for opt_lvl == 2
            let el.sog = 0
        endif
        " opt_lvl 2 contains its own bounding box logic, and doesn't use 'seg' key.
        if g:sexp_aligncom_optlevel < 2
            if !sog
                " See whether bounding box can accommodate this element.
                let test_min = min([el.ecol, ecol_min])
                " TODO: Consider using option to determine whether pre_max is considered.
                let test_max = max([el.ecol, el.pre_max, ecol_max])
                " Is bounding box still within limits?
                if test_max - test_min > a:opts.maxshift
                    " Break group.
                    let sog = 1
                else
                    " Continuation of current group.
                    let [ecol_min, ecol_max] = [test_min, test_max]
                    " Save max in case this ends up being final element of greedy group.
                    if g:sexp_aligncom_optlevel == 1
                        call add(ecol_maxes, ecol_max)
                    endif
                endif
            endif
            if sog 
                " Skip prev group finalization if this is first element.
                if i > 0
                    " Perform any requisite decoration of previous segment(s).
                    call s:aligncom__preproc_finalize_seg(
                        \ i, sidx, prev_sidx, a:prep, ecol_max, ecol_maxes_prev, a:opts)
                endif
                " Make next group current (or current group prev).
                let sog = 0
                let [prev_sidx, sidx] = [sidx, i]
                let [ecol_min, ecol_max] = [el.ecol, el.ecol]
                " These are used only with a lag.
                let [ecol_max_prev, ecol_maxes_prev, ecol_maxes] = [ecol_max, ecol_maxes, []]
            endif
        endif
        let i += 1
    endwhile
    " Caveat: Guard needed in case there are no eol comments in range.
    if g:sexp_aligncom_optlevel < 2 && i > 0
        " Process final group.
        call s:aligncom__preproc_finalize_seg(
            \ i, sidx, prev_sidx, a:prep, ecol_max, ecol_maxes_prev, a:opts)
    endif
endfunction

" Preprocess range of lines potentially containing eol comments, building a list of dicts
" intended to streamline the subsequent alignment logic in aligncom__optimize_range().
" At the end of preprocessing, each element will have the following keys:
"   line, ecol, is_eol_com, is_com, com_s, pre_max, prev_e, com_s
" Additionally...
" Opt lvl < 2:
"   If element is head of a 'greedy' segment, it will contain the following:
"     seg: {is_greedy: 1, eidx: <end_of_greedy_grp>, ecol_max: <greedy_grp_align>}
" Opt lvl 1:
"   If element is head of a 'dp' segment, it will contain the following:
"     seg: {is_greedy: 0}
" Return: [<prep_list>, <opt_level>]
function! s:aligncom__preproc(start, end, opts)
    let prep = s:aligncom__preproc_pass1(a:start, a:end, a:opts)
    call s:aligncom__preproc_pass2(prep, a:opts)
    return prep
endfunction

" Calculate useful state (primarily target range) for a command that operates on either
" forms (possibly more than one, as determined by count) or visual range and needs to
" preserve view and cursor position across the operation: e.g., indent and align.
" Return: a dict containing the calculated state, else {} if we can't determine a valid
" range.
" Note: Currently, this function also handles saving window state, but I'm thinking
" perhaps that should be pulled out.
" TODO: Rename this to make it more general...
function! s:pre_align_or_indent(mode, top, count, clean, ps)
    let win = winsaveview()
    let cursor = getpos('.')
    let [line, col] = [cursor[1], cursor[2]]
    " Note: This flag can be set (but not cleared) post init.
    " Rationale: Even if user hasn't requested toplevel operation, it should be set if we
    " are, in fact, at toplevel
    let at_top = a:top

    " Save original visual marks for restoration after adjustment.
    " Rationale: For Normal mode invocation, we use visual selection to perform indent,
    " but this is an implementation detail that should be transparent to user.
    let [vs, ve] = s:get_visual_marks()
    let [start, end] = [s:nullpos, s:nullpos]
    if a:mode ==? 'n'
        " Move to current list tail since the expansion step of
        " s:set_marks_around_current_list() happens at the tail.
        " TODO: It's no longer the case that expansion happens only at tail; look at
        " refactoring this entire block!
        if getline(line)[col - 1] =~ s:closing_bracket
            \ && !s:is_rgn_type('str_com_chr', line, col)
            let pos = [0, line, col, 0]
        else
            let pos = s:move_to_nearest_bracket(1)
        endif

        " TODO: Need rationale for this `v`, given that the functions below ensure entry
        " into visual mode. I'm not sure it's needed, but it's been here a *long* time.
        "normal! v
        if pos[1] < 1
            let at_top = 1
            " At top-level. Select current (or next) element.
            " Note: On list close, the move_to_nearest_bracket() above will return
            " nullpos, even though we're effectively on a list; in that case, the
            " following call will select the list, including brackets.
            " TODO: Consider whether it ever makes sense to select *next* element when
            " we're not within element or list.
            call sexp#select_current_element('n', 1)
        elseif a:top
            " Inside list. Select topmost list.
            call sexp#select_current_top_list('n', 0)
        else
            " Inside list. Select [count]th containing list.
            " If performing clean, select only inner list.
            " Rationale: cleanup_ws will get any open or close adjacent to
            " selection, and we want to stop at the edge of current list.
            call sexp#docount(a:count, 'sexp#select_current_list', 'n', a:clean, 1)
        endif
        " Cache visual start/end; end can actually be changed by s:cleanup_ws().
        let [start, end] = s:get_visual_marks()
        " We're done with visual mode. Leave it to avoid problems below (eg,
        " with function calls).
        call sexp#ensure_normal_mode()
    else
        " Treat visual mode specially. Rationalize visual range.
        let [start, end] = s:super_range(vs, ve)
    endif
    return {'win': win, 'at_top': at_top, 'ps': a:ps, 'cursor': cursor,
            \ 'vs': vs, 've': ve, 'start': start, 'end': end}
endfunction

" Function containing common post-operation logic for both align and indent.
" FIXME: Since this function is called internally (e.g., from sexp#clone), should probably
" either factor out the cursor/window restoration (e.g., putting it into a static
" workhorse function that can be called by sexp#clone as well), or make it selectable.
function! s:post_align_or_indent(mode, state)
    " Adjust window view object to account for buffer changes made by the
    " indent (and possibly by s:cleanup_ws).
    let a:state.win.lnum = a:state.cursor[1]
    let a:state.win.col = a:state.cursor[2] - 1 " .col is zero-based
    " Design Decision: In normal mode, restore old (adjusted) visual selection; in visual
    " mode, restore the adjusted super-range (if valid).
    call s:set_visual_marks(a:mode ==? 'n' || !a:state.start[1] || !a:state.end[1]
            \ ? [a:state.vs, a:state.ve] : [a:state.start, a:state.end])
    call winrestview(a:state.win)
endfunction

" API function implementing standalone eol comment alignment (i.e., not triggered by indent).
function! sexp#align_comments(mode, top, count)
    try
        " CursorMoved autocmd callbacks for CursorMoved/TextChanged events (e.g.,
        " matchparen's s:Highlight_Matching_Pair()) can really slow things down...
        let ei_save = &ei
        set ei=CursorMoved,TextChanged
        " eol comment alignment doesn't support 'clean'; if user wants ws cleanup, he should
        " be using indent commands, which can be configured to do both ws cleanup and eol
        " comment alignment.
        let state = s:pre_align_or_indent(a:mode, a:top, a:count, 0, [])
        " Don't attempt alignment on null range.
        if !empty(state) && state.start[1] && state.end[1]
            call s:align_comments(state.start, state.end, state.ps)
            call s:post_align_or_indent(a:mode, state)
        endif
    finally
        " Re-enable autocmds.
        let &ei = ei_save
    endtry
endfunction

" Align end of line comments within specified range, taking all relevant options into account.
" Inputs:
"   start:  start of range
"   end:    end of range
"   ps:     list of positions requiring adjustment
function! s:align_comments(start, end, ps)
    "let ts = reltime()
    let opts = s:aligncom__get_deferred_opts()
    " Preprocess the range, building a list of dicts that will facilitate layout.
    let prep = s:aligncom__preproc(a:start, a:end, opts)
    " Perform the optimization/layout.
    call s:aligncom__optimize_range(prep, opts)
    " Iterate the reverse chain of groups in prep to produce a forward list of groups.
    let grps = s:aligncom__finalize_groups(prep, opts)
    "call s:dbg_show_eolcs(grps)
    " Iterate groups, performing alignment of each eol comment.
    for grp in grps
        let [align, eolcs] = [grp.align, grp.eolcs]
        " Loop over the comments in this group.
        "echomsg align
        for eolc in eolcs
            "echomsg eolc
            " Align the comment by splicing required number of spaces between start of eol
            " comment (exclusive) and end of element preceding it (exclusive).
            "call s:Dbg("yankdel_range: prev_e=%s com_s=%s", string(eolc.prev_e), string(eolc.com_s))
            call s:yankdel_range(
                    \ eolc.prev_e,
                    \ eolc.com_s,
                    \ repeat(' ', align - eolc.prev_e[2]),
                    \ [0, 0],
                    \ a:ps)
        endfor
    endfor
    "echomsg printf("Alignment took: %f", reltimefloat(reltime(ts)))
endfunction

" Indent S-Expression, maintaining cursor position. This is similar to mapping
" to =<Plug>(sexp_outer_list)`` except that it will fall back to top-level
" elements not contained in a compound form (e.g. top-level comments).
" -- Args --
" mode:  command mode
" top:   1 to indent top-level form
" count: number of containing forms to indent
" clean: 1 to perform cleanup before indent
"        -1 to let g:sexp_indent_does_clean decide
" Optional Args:
" 0=force syntax flag: 1 to force syntax update. Should always be set when buffer is
"                      being modified
" 1=positions list:    list of positions to be adjusted in-place to account for indent
function! sexp#indent(mode, top, count, clean, ...)
    try
        " CursorMoved autocmd callbacks for CursorMoved/TextChanged events (e.g.,
        " matchparen's s:Highlight_Matching_Pair()) can really slow things down,
        " especially if we're cleaning up whitespace.
        let ei_save = &ei
        set ei=CursorMoved,TextChanged
        " If caller hasn't specified clean, defer to option.
        let clean = a:clean < 0 ? g:sexp_indent_does_clean : !!a:clean
        let state = s:pre_align_or_indent(a:mode, a:top, a:count, clean, a:0 > 1 ? a:2 : [])
        let force_syntax = a:0 && !!a:1
        if clean
            " Always force syntax update when we're modifying the buffer.
            let force_syntax = 1
            " Design Decision: Handle both non-list and list elements identically:
            " cleanup back to prev, but indent starting with current.
            " Note: Avoid unnecessary calls to at_top().
            let at_top = state.at_top || s:at_top(state.end[1], state.end[2])
            call s:cleanup_ws(state.start,
                \ s:concat_positions(state.ps, state.start, state.end, state.cursor,
                    \ a:mode ==? 'n' ? [state.vs, state.ve] : []), state.end)
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
            exe state.start[1] . ',' . state.end[1] . 'call synID(line("."), col("."), 1)'
        endif
        " Position pre-adjustment
        let adj = s:indent_preadjust_positions(
            \ s:concat_positions(state.ps, state.start, state.end, state.cursor,
                \ a:mode ==? 'n' ? [state.vs, state.ve] : []))
        " Perform the indent.
        silent exe "keepjumps normal! " . state.start[1] . 'G=' . state.end[1] . "G"
        " Position post-adjustment
        call s:indent_postadjust_positions(adj)
        " (Optional) end of line comment alignment
        if g:sexp_indent_aligns_comments
            call s:align_comments(state.start, state.end, state.ps)
        endif
        " Restore window and such.
        call s:post_align_or_indent(a:mode, state)
    finally
        " Re-enable autocmds.
        let &ei = ei_save
    endtry
endfunction

" Build and a return a list containing all the non-null VimPos4's provided as input,
" flattening any provided lists as required to reach the (possibly deeply) nested
" VimPos4's.
" Important Note: We do not flatten the VimPos4's themselves, as their list identity must
" be preserved.
" TODO: Consider adding position uniquifying logic.
" Rationale: The created lists are generally used in pass-by-ref position
" modification strategies, which would modify the same position multiple times
" if it somehow made it into the list multiple times.
function! s:concat_positions(...)
    let ret = []
    for p in a:000
        " If current element is a valid position, append it; otherwise, recurse on list
        " (possibly nested) of VimPos4 and append the returned list.
        " Discard nullpos and empty lists; assume all other terminals are valid positions.
        if !empty(p) && p != [0, 0, 0, 0]
            let ret += type(p[0]) == type(0)
                \ ? [p]
                \ : call('s:concat_positions', p)
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

" Return total # of bytes in buffer.
function! s:total_bytes_in_file()
    " Compatibility Note: Vim 7.4 seem not to support use of '$' with line2byte(). It does
    " seem to support use of line2byte(line('$') + 1) to get buffer length + 1, but this
    " behavior is not documented in that version, so I'm choosing not to rely on it.
    " Idiosyncrasy/Bug Note: When an empty file is first opened, line2byte(1) returns -1,
    " though docs imply it should be 1. Adding (and subsequently removing) a line, or even
    " a char that triggers a popup menu fixes the condition, such that 1 is returned.
    " Workaround: To avoid issues, simply constrain the value returned by line2byte() to
    " be >= 1.
    return max([1, line2byte(line('$'))]) + col([line('$'), '$']) - 1
endfunction

" Modify ps in-place.
" Note: inc is always 2-element list.
" Assumption: start/end refer to actual char positions.
function! s:adjust_positions(start, end, splice, delta, ps)
    let [s, e] = [a:start[:], a:end[:]]

    for p in a:ps
        if sexp#compare_pos(p, s) <= 0
            " Position unaffected
            continue
        elseif sexp#compare_pos(p, e) < 0
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

" Calculate and return a dict describing the current list, defined as the smallest list
" containing the test position, where the containment test is exclusive of brackets/macro
" chars iff optional 'inner_only' flag is set.
" Design Decision: Macro chars considered part of the list, just like brackets.
" -- Args --
"   tail:   determines whether terminal_range should reflect head (0) or tail (1)
" -- Optional Dict --
"   pos                position to test, defaults to curpos
"   inner_only         if set, current list must contain (in bracket-exclusive sense) the
"                      test position; defaults to 0
" Return Dict:
"   macro:           position of start of macro chars, else nullpos
"   brackets[]:      [open, close], else [s:nullpos, s:nullpos] if no list
"   terminal_range:  pos pair representing extents of terminal element requested by
"                    a:tail, else nullpos_pair
"   fallback:        1 iff caller wanted parent of the list represented by brackets[]
" TODO: Consider allowing caller to provide count to get non-terminal; let this be a
" workhorse wrapped by child_range(), etc...
" Rationale: Sometimes, you need both list info and also specific child; too much
" duplication as it is now.
" Partial Solution: child_range() accepts this function's return as optional arg, thereby
" avoiding most of the duplication.
function! s:list_info(tail, ...)
    let save_cursor = getpos('.')
    let ret = {
        \ 'macro': s:nullpos, 'brackets': [s:nullpos, s:nullpos],
        \ 'terminal_range': s:nullpos_pair,
        \ 'fallback': 0,
    \ }
    try
        " Extract optional flags.
        let inner_only = a:0 ? get(a:1, 'inner_only', 0) : 0
        " Has caller specified a (non-cursor) position to test?
        let pos = a:0 ? get(a:1, 'pos', s:nullpos) : s:nullpos
        if pos[1] | call s:setcursor(pos) | endif
        " Assumption: We're on test position.
        " Move to its head, which is a safe place from which to search for containing open
        " bracket if necessary.
        let p = sexp#move_to_current_element_terminal(0)
        let isl = p[1] ? s:is_list(p[1], p[2]) : 0
        if !isl || inner_only
            " Either not on list structure, or we are, but want its parent list.
            " Look for nearest open bracket.
            let pb = s:move_to_nearest_bracket(0)
            if pb[1]
                " Found parent open, but we need to know if there are macro chars.
                let p = sexp#move_to_current_element_terminal(0)
                if p != pb
                    " Must be macro chars.
                    let ret.macro = p
                    " Move to open.
                    call s:setcursor(pb)
                endif
                " Ensure subsequent macro char handling is skipped.
                let isl = 2
            elseif isl
                " No parent, but we're on either open or macros of original list, which
                " will have to suffice for current.
                let ret.fallback = 1
            else
                " No current or parent list.
                return ret
            endif
        endif
        " Assumption: Arrival here guarantees we're on either open bracket or macro chars,
        " as indicated by isl.
        " Note: The following 'if' can be entered only if we started on macro chars and
        " didn't look for (or looked for and failed to find) containing open.
        if isl == 1
            " On start of macro chars preceding list.
            " Save macro char start pos and move to open bracket.
            let ret.macro = p
            call s:setcursor(s:current_macro_character_terminal(1))
            call s:move_char(1)
        endif
        " We're on open bracket. Save it and find close.
        let ret.brackets[0] = getpos('.')
        let ret.brackets[1] = s:nearest_bracket(1)
        " Begin search for list terminal from bracket indicated by a:tail.
        call s:setcursor(ret.brackets[a:tail])
        " Look *inward* for first non-whitespace.
        let [l, c] = s:findpos('\S', !a:tail)
        if [0, l, c, 0] != ret.brackets[!a:tail]
            " Since we didn't hit opposite bracket, not an empty list.
            " Position on the found element and get its range, allowing for possibility
            " that what we hit was not element terminal (due to ignored whitespace).
            call s:setcursor([0, l, c, 0]) 
            let ret.terminal_range =
                \ [sexp#current_element_terminal(0), sexp#current_element_terminal(1)]
        endif
        return ret
    finally
        " Restore original position.
        call s:setcursor(save_cursor)
    endtry
endfunction

" Return the boundaries of the current/containing list.
" -- Optional Dict --
"   pos                position to test, defaults to curpos
"   inner_only         if set, current list must contain (in bracket-exclusive sense) the
"                      test position; defaults to 0
" Return:
"   [macro_start, open_bracket, close_bracket]
"   where macro_start is nullpos if there are no leading macro chars.
function! s:containing_list_bounds(...)
    let save_cursor = getpos('.')
    let ret = [s:nullpos, s:nullpos, s:nullpos]
    try
        let inner_only = a:0 ? get(a:1, 'inner_only', 0) : 0
        let pos = a:0 ? get(a:1, 'pos', s:nullpos) : s:nullpos
        if pos[1]
            call s:setcursor(pos)
        endif

        let p = sexp#move_to_current_element_terminal(0)
        let isl = p[1] ? s:is_list(p[1], p[2]) : 0
        if !isl || inner_only
            let pb = s:move_to_nearest_bracket(0)
            if pb[1]
                let p = sexp#move_to_current_element_terminal(0)
                if p != pb
                    let ret[0] = p
                    call s:setcursor(pb)
                endif
                let isl = 2
            elseif !isl
                return ret
            endif
        endif

        if isl == 1
            let ret[0] = p
            call s:setcursor(s:current_macro_character_terminal(1))
            call s:move_char(1)
        endif

        let ret[1] = getpos('.')
        let ret[2] = s:nearest_bracket(1)
        return ret
    finally
        call s:setcursor(save_cursor)
    endtry
endfunction

" Like s:list_terminal() but treating entire buffer as a list
function! s:buffer_terminal(tail)
    let cursor = getpos('.')
    let ret = s:nullpos_pair
    try
        call s:setcursor(a:tail
            \ ? [0, line('$'), col([line('$'), '$']), 0], [0, 1, 1, 0])
        " Look inward for first non-whitespace (possibly under cursor).
        let [l, c] = searchpos('\S', (a:tail ? 'b' : '') . 'cW')
        if l
            " Found an element. Get its extents.
            let ret = [sexp#current_element_terminal(0), sexp#current_element_terminal(1)]
        endif
        return ret
    finally
        call s:setcursor(cursor)
    endtry
endfunction

" Return range of first/last element of current list as position pair, else nullpos pair
" if not on or within a non-empty list.
" -- Args --
" tail:  0=head 1=tail
" -- Optional Args --
"   pos                position to test, defaults to curpos
"   inner_only         if set, current list must contain (in bracket-exclusive sense) the
"                      test position; defaults to 0
" Note: This is really just a convenience wrapper around s:list_info(); alternatively,
" could use s:child_range() with count of 0 or 1.
function! s:list_terminal(tail, ...)
    let li = s:list_info(a:tail, a:0 ? a:1 : {})
    return li.terminal_range
endfunction

" Remove extra whitespace in the range determined as follows:
"   a:0 == 0: list whose open bracket is at a:start
"   a:0 == 1: element before a:start to element past a:1
" Args:
"   a:start     start of range (a:0==1) or position of open bracket (a:0==0)
"   a:ps        list of positions to be updated
"   a:1 (end)   (optional) end of range
" TODO: Update this function to take advantage of recently added 'ignore_current' and
" 'nullpos_on_fail' arguments to nearest_element_terminal().
function! s:cleanup_ws(start, ps, ...)
    let end = a:0 ? a:1 : [0, 0, 0, 0]
    let [open, close, prev] = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
    " TODO: Decide whether/how to warn for the internal errors resulting in early exit.
    if a:0 && !end[1]
        " Something's wrong: nullpos shouldn't be passed as end.
        return
    endif
    if !a:start[1]
        " Something's really wrong: start shouldn't be nullpos.
        return
    endif
    if !end[1]
        " Cleanup a list.
        " Should be on open bracket, but need to be sure.
        call s:setcursor(a:start)
        let open = s:list_open()
        if !open[1]
            " Not on or within list and range wasn't provided.
            return
        endif
        " Descend into list.
        let next = s:list_terminal(0)[0]
    else
        " Cleanup specified range without assuming anything about start.
        " Set things up as though loop processing is already in progress:
        " e.g., set open, prev and next (if non-null).
        " Note: eff_* will be set in loop pre-update, so no need to set here.
        call s:setcursor(a:start)
        let next = sexp#current_element_terminal(0)
        if !next[1]
            " Not in element.
            let next = sexp#nearest_element_terminal(1, 0)
            " Note: nearest_element_terminal returns current pos on failure.
            if !sexp#compare_pos(next, getpos("."))
                " No element on or after start. Null next so that close will
                " be set in loop...
                let next = [0, 0, 0, 0]
            endif
        endif
        let prev = sexp#nearest_element_terminal(0, 1)
        if !sexp#compare_pos(prev, getpos("."))
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
            " Caveat: We can get here before reaching EOF if an unbalanced close bracket
            " prevents finding either next or close. In that case, safest thing to do is
            " short-circuit with a warning that will help user locate the issue.
            let [l, c] = s:findpos('\S', 1)
            if l
                " Design Decision: Don't try to prevent the re-indent that most likely
                " follows the cleanup, even though it may invalidate the line numbers
                " reported here.
                call sexp#warn#msg(
                    \ printf("cleanup_ws: Unexpected token (possibly unbalanced"
                    \ . " close bracket) at or near (%d,%d).", l, c))
                return
            else
                " Assumption: ve=onemore obviates need for eof flag.
                let eff_next = getpos([line('$'), '$'])
            endif
        endif

        " Do we want to remove *all* whitespace between eff_prev and eff_next?
        let full_join =
                \ !next[1] && !prev[1]
                \ || !next[1] && (!close[1] || !sexp#is_comment(prev[1], prev[2]))
                \ || !prev[1] && (!open[1] || !sexp#is_comment(next[1], next[2]))

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
                " Note: Treating negative 'keep_empty_lines' as infinity ensures removal
                " of non-NL whitespace at EOL.
                let precedes_com = next[1] && sexp#is_comment(next[1], next[2])
                if gap > g:sexp_cleanup_keep_empty_lines + 1
                    \ || getline(eff_prev[1])[eff_prev[2] - 1:] =~ '.\s\+$'
                    " Replace gap with number of newlines determined by existing line gap
                    " and g:sexp_cleanup_keep_empty_lines option, followed by original
                    " whitespace on eff_next's line.
                    let spl = repeat("\n", g:sexp_cleanup_keep_empty_lines < 0
                        \ ? gap : min([gap, g:sexp_cleanup_keep_empty_lines + 1]))
                endif
            " Single-line (whitespace between colinear elements)
            " If next isn't comment and there are multiple whitespace chars between
            " eff_prev and eff_next, collapse to a single whitespace.
            " Rationale: Only before comment does extra (non-leading) ws make sense.
            " Note: Refusing to collapse whitespace *immediately* preceding comment
            " isn't a complete solution, since collapsing ws earlier on the line will
            " still break alignment. Now that we support auto comment alignment, we could
            " make the comment test conditional...
            " Cursor Logic: If cursor is in whitespace to be contracted, but not on
            " *first* whitespace in the range, we want it to end up *past* the single
            " remaining space; otherwise, on it. FIXME: Currently, this can mean past end
            " of line, but perhaps it shouldn't be allowed to move to next line.
            elseif g:sexp_cleanup_collapse_whitespace
                \ && getline(eff_prev[1])[eff_prev[2] - 1 : eff_next[2] - 1] =~ '.\s\s'
                \ && next[1] && !sexp#is_comment(next[1], next[2])
                " Replace multiple whitespace on single line with single space.
                " Assumption: BOF and EOF are always handled as full join
                let spl = ' '
            endif
        endif
        if !empty(spl)
            " Prevent pointless calls to s:yankdel_range (when there's no
            " whitespace to contract).
            if !(bof && eff_next[1:2] == [1, 1] ||
                \ !bof && sexp#offset_char(eff_prev, 1) == eff_next)
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
        " Assumption: eff_next and next are the same except that the former
        " has been adjusted.
        keepjumps call cursor(eff_next[1], eff_next[2])
        if s:is_list(eff_next[1], eff_next[2])
            let next = s:move_to_list_open()
            call s:cleanup_ws(next, a:ps)
            " Assumption: Restore cursor pos (potentially changed by
            " recursion) to next (which can't be invalidated by recursion).
            call s:setcursor(next)
        endif
        " Now that we've recursed (if possible), attempt to advance.
        let prev = sexp#move_to_current_element_terminal(1)
        let next = sexp#nearest_element_terminal(1, 0)
        if next == prev
            " No more elements at current level.
            " Note: Null next to ensure attempt to find close on next and
            " final iteration of this recursion.
            let next = [0, 0, 0, 0]
        elseif end[1] && sexp#compare_pos(next, end) > 0
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

" Callback invoked at start of sexp_raise.
" Contains mode-specific logic to determine the initial raise target of a sexp_raise that
" may use a count. Because each iteration of a counted operation adjusts the visual marks
" around the raised target, the logic is needed only on the first iteration.
function! sexp#raise__init(mode, func, ...)
    if a:mode ==# 'v'
        " Important Note: Blindly using visual selection could unbalance forms; expand
        " selection as necessary to ensure it contains one or more elements at the same
        " level.
        let range = s:super_range(getpos("'<"), getpos("'>"))
        if !range[0][1] || !range[1][1]
            " No valid range! Do nothing.
            return
        endif
        " Select the super range.
        call s:set_visual_marks([range[0], range[1]])
    else
        " Perform selection using provided func and args.
        call call(a:func, a:000)
    endif
endfunction

" Callback invoked to complete sexp_raise.
" Performs automatic re-indent (if appropriate), and leaves the cursor/visual selection in
" proper state: i.e., for visual mode command, leaves the raised target selected; for
" normal mode command, positions at start of raised target.
function! sexp#raise__final(ex, state, mode, func, ...)
    let [s, e] = s:get_visual_marks()
    " Perform indent if auto-indent enabled *and* raise target is multiline.
    " Rationale: Raised target should start exactly where replaced list did; thus,
    " need for re-indent depends on what was raised, not what was replaced.
    if ((g:sexp_auto_indent != -1 && g:sexp_auto_indent) || g:sexp_raise_does_indent)
        \ && s[1] != e[1]
        " Note: Though we use visual marks, it's important that we be in normal mode.
        call sexp#ensure_normal_mode()
        call sexp#indent('v', 0, 1, -1, 1, [s, e])
        " Adjust visual selection to account for re-indent.
        call s:set_visual_marks([s, e])
    endif
    " Depending on command mode, either select the raised target or position on its start.
    if a:mode ==# 'v'
        call s:select_current_marks(a:mode)
    else
        call s:setcursor(s)
    endif
endfunction

" Replace parent list with selection resulting from executing func with given varargs.
" Precondition: Visual marks have been set around the element(s) to be raised.
" Postcondition: Visual marks have been adjusted to reflect raised target to ensure that
" next iteration has the information it needs. (This approach obviates the need for state
" dict and calls to *__init() and *__update() functions.)
function! sexp#raise(state, mode, func, ...)
    " Before deleting anything, be sure there's a parent list by moving to start of visual
    " range and looking for nearest open.
    call sexp#ensure_normal_mode()
    call s:setcursor(getpos("'<"))
    let pos = s:nearest_bracket(0)
    if !pos[1]
        " Will be caught by docount_stateful().
        throw 'sexp-done'
    endif
    " Re-select the element(s) to be raised.
    " Assumption: s:nearest_bracket() didn't alter visual marks.
    call s:select_current_marks(a:mode)
    normal! d
    call sexp#select_current_list('n', 0, 0, 1)
    normal! p
    " Set marks around raised target (pasted text).
    call s:set_visual_marks([getpos("'["), getpos("']")])
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
            call sexp#warn#msg("Convolute impossible with count given: insufficient nesting")
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
            let pos = sexp#nearest_element_terminal(1, 0)
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
    " FIXME: Refactor yankdel_range() take optional args dict!!!
    let ket = s:yankdel_range(tpos_i, tpos_i, 1, 1, [], 1)
    let del = s:yankdel_range(bpos_i, pos, 1, [0, 0], [], 1)
    let bra = s:yankdel_range(spos_i, bpos_i, 1, 1, [], 1)

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
        keepjumps call cursor(line('.'), col('.') + len(bra) - 1)
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
            " TODO: Consider adding an optional inhibit_select flag to
            " sexp#select_current_element and sexp#select_current_list, which would
            " obviate need for this. The problem with remaining in visual mode is that
            " *any* subsequent cursor movement would alter the marks we've just set.
            exe "normal! \<Esc>"
            if found
                " Design Decision: Perform inner element selection to
                " incorporate any adjacent macro chars.
                call sexp#select_current_element('n', 1)
                " Caveat! Don't stay in visual mode.
                exe "normal! \<Esc>"
                let [vs, ve] = s:get_visual_marks()
                " Make sure we're on or in the found list.
                " Rationale: select_current_list can find list after cursor,
                " and we're not interested in those.
                if sexp#compare_pos(cursor, vs) >= 0 && sexp#compare_pos(cursor, ve) <= 0
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
                call sexp#move_to_adjacent_element_terminal(1, 0, 0)
                let found = p != getpos('.')
            endif
            return found
                \ ? s:set_marks_around_current_element('n', 1, 0, 1)
                \ : [[0, 0, 0, 0], [0, 0, 0, 0]]
        endif
    endif
endfunction

" Return a dict representing the operation context (single or multi) and position of eol
" comment iff it should be considered part of target by clone operation.
" Note: eol comments ignored if ignore_eolc flag is set
" Logic: By default, operation will be multi-line if any of the following conditions
" holds:
" * target spans multiple lines
" * target is on a line by itself, possibly (if !ignore_eolc) followed by trailing
"   comment)
" * target is the first or last element of a list whose open and close brackets are not
"   both colinear with target, and none of the target's sibling elements (ignoring any
"   trailing comment if !ignore_eolc) are colinear with target
" * target is at toplevel
" * final (or only) element of target is an end-of-line comment
" The default logic can be overridden by sl_ / ml_ command variants, but if target ends
" with comment, we must always perform multi-line, and should probably warn if sl command
" variant was used.
" -- Args --
"   start  position of start of operation target
"   end    position of end of operation target
"   flags  dict of flags:
"          force_sl_or_ml:
"            Non-empty to force a specific context, in which case, this function is called
"            only for its eolc functionality (currently, needed only for clone operation).
"            Values: '', 'sl', or 'ml' (default '')
"          ignore_eolc:
"            Set to ignore presence of trailing comment *after* target.
"            Note: Whether target itself ends in trailing comment is always
"            considered.
"          ignore_top:
"            Set to treat target at top-level like any other (default 0).
"            If left unset, a top-level target will always have ml context.
" Return Dict:
"   multi:   1 iff multi-line context
"   eolc:    {} if no eol comment to clone, else a dict with start/end keys containing
"            VimPos4's delineating the eol comment
" Cursor Preservation: None (caller expected to handle)
function! s:get_clone_context(start, end, flags)
    let [multi, eolc] = [0, {}]
    let force_sl_or_ml = get(a:flags, 'force_sl_or_ml', '')
    let ignore_eolc = get(a:flags, 'ignore_eolc', 0)
    let ignore_top = get(a:flags, 'ignore_top', 0)
    " Note: If ignoring top, set flag to 0 without calling s:at_top().
    let top = ignore_top ? 0 : s:at_top(a:start[1], a:start[2])
    " Cache some information required by subsequent logic.
    let bol = s:at_bol(a:start[1], a:start[2])
    let eol = s:at_eol(a:end[1], a:end[2])
    call s:setcursor(a:end)
    let next = sexp#nearest_element_terminal(1, 0)
    " Is there a next sibling?
    let last = a:end == next
    if !last && next[1] == a:end[1]
        " There's a next sibling and it's colinear. Is it an eol comment?
        if !ignore_eolc && s:is_eol_comment(next[1], next[2])
            " Note: Deferring decision on whether to clone the comment
            let eolc.start = next[:]
            let eolc.end = [0, next[1], col([next[1], '$']) - 1, 0]
        endif
    endif
    " Note: It's *always* possible to force a multi-line clone.
    if force_sl_or_ml == 'm'
        let multi = 1
    elseif s:is_eol_comment(a:end[1], a:end[2])
        " Final (or only) element of target is an eol comment.
        " This is the only case in which default logic forces multi-line context.
        let multi = 1
    " If here, single-line context won't be overridden by default logic.
    elseif force_sl_or_ml != 's'
        " Use default selection logic.
        if top || a:start[1] != a:end[1] || bol && (eol || !empty(eolc))
            " One of the following conditions is met.
            "   * target spans multiple lines
            "   * target is toplevel
            "   * target is on line by itself (possibly followed by eol comment)
            let multi = 1
        else
            " Check the condition involving targets that are first/last element of list.
            call s:setcursor(a:start)
            let prev = sexp#nearest_element_terminal(0, 1)
            " Is there a prev sibling?
            let first = a:start == prev
            " Check multi-line conditions.
            let multi = first && (eol || !empty(eolc)) || last && bol
        endif
    endif
    if multi && force_sl_or_ml == 's'
        " Warn user we're overriding _sl command
        call sexp#warn#msg("Overriding single-line clone command in multi-line context.")
    endif
    return {'multi': multi, 'eolc': multi ? eolc : {}}
endfunction

" Clone list/element at cursor (normal mode) or range of elements partially or
" fully included in visual selection.
" Design Decision: In normal mode, change to visual selection should be a completely
" transparent and temporary side-effect of the implementation: thus, we restore the
" (adjusted) original selection (if any). In visual/operator modes, otoh, we restore the
" adjusted inner selection corresponding to the copied range.
" TODO: Probably get rid of 'after' arg, which isn't really used.
function! sexp#clone(mode, count, list, after, force_sl_or_ml)
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
        call sexp#warn#msg("Nothing to clone")
        return
    endif
    " Design Decision: If cursor starts in whitespace before target, move it
    " to head of target to ensure that cursor always stays with target.
    if sexp#compare_pos(cursor, start) < 0
        let cursor = start[:]
    endif
    " Assumption: Prior logic guarantees start and end at same level.
    " Determine whether to perform single or multi-line clone, and whether eol comment
    " should be cloned along with the target.
    let ctx = s:get_clone_context(start, end, {'force_sl_or_ml': a:force_sl_or_ml})
    if ctx.multi && !empty(ctx.eolc)
        " Update end to include the eol comment.
        let end = ctx.eolc.end
    endif
    " Get the text to be copied.
    let copy = s:yankdel_range(start, end, 0, 1)
    call s:setcursor(a:after ? end : start)
    let repl = ctx.multi
        \ ? a:after ? ["\n", copy] : [copy, "\n"]
        \ : a:after ? [" ", copy] : [copy, " "]
    let copy = join(repeat(repl, a:count ? a:count : 1), "")

    if !keep_vs
        " Save the target range, which will become the new selection after adjustment.
        let [vs, ve] = [copy(start), copy(end)]
    endif
    " Implement put with yankdel_range to take advantage of position adjustment.
    let p = a:after ? end : start
    let inc = a:after ? [0, 1] : [1, 0]
    " TODO: Consider creating a put_at wrapper for this.
    call s:yankdel_range(p, p, copy, inc,
            \ s:concat_positions([end, cursor], [vs, ve]))
    " Design Decision: Single line clone can't change indent.
    " Rationale: If it's wrong now, it was already wrong, as we haven't done
    " anything that should have any impact on indentation.
    " TODO: Decide whether it would make sense to do indent anyways, possibly only if the
    " 'indent_does_clean' option is set.
    let need_indent = ctx.multi && !!g:sexp_clone_does_indent
    if need_indent
        if s:at_top(start[1], start[2])
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
            call sexp#indent('n', 0, isl > 1 ? 2 : 1, -1, 1, [start, end, cursor, vs, ve])
        endif
    endif

    " Adjust visual marks. See note in header on what vs/ve represent.
    call s:set_visual_marks([vs, ve])
    " Restore adjusted cursor position.
    call s:setcursor(cursor)
endfunction

" Remove brackets from current list, placing cursor at position of deleted
" first bracket. Takes optional count parameter, which specifies which pair of
" ancestor brackets to remove.
function! sexp#splice_list(...)
    call s:set_marks_characterwise()

    let marks = s:get_visual_marks()
    let cursor = getpos('.')

    " Climb the expression tree a:1 times (or until top-level hit).
    if a:0 && a:1 > 1
        let idx = a:1
        let dir = getline(cursor[1])[cursor[2] - 1] =~ s:opening_bracket
        while idx > 0
            let pos = s:nearest_bracket(dir)
            if !pos[1]
                " Top-level reached.
                break
            endif
            call s:setcursor(pos)
            let idx -= 1
        endwhile
    endif

    call s:set_marks_around_current_list('n', 0, 0)

    let [start, end] = [s:get_visual_beg_mark(), s:get_visual_end_mark()]

    if start[1] > 0
        " Delete ending bracket first so we don't mess up '<
        call s:setcursor(end)
        normal! dl
        call s:setcursor(start)
        normal! dl
    else
        call s:setcursor(cursor)
    endif
    " Perform indent if auto-indent enabled *and* multiple lines affected by splice.
    if g:sexp_auto_indent != -1 ? g:sexp_auto_indent : g:sexp_splice_does_indent
        \ && start[1] != end[1]
        " Assumption: Visual marks should still span correct line range.
        " Note: Though we use visual marks, it's important that we be in normal mode.
        call sexp#ensure_normal_mode()
        call sexp#indent('v', 0, 1, -1, 1, [cursor, marks, start, end])
    endif

    " Restore original visual marks (if there were any), possibly adjusted for re-indent.
    if marks[0][1]
        call s:set_visual_marks(marks)
    endif
endfunction

" Invoked by docount_stateful() to initialize state object, which will be provided to each
" invocation of stackop__update(), and ultimately, to stackop__final().
function! sexp#stackop__init(mode, last, capture)
    " Make sure stackop__final() has the information it needs to finalize the operation.
    " mode:    command invocation mode
    " range:   affected range, used to determine re-indent
    " curpos:  initially, cursor pos at invocation, but subject to change by operation
    " vmarks:  initially, visual marks at invocation, but subject to change by operation
    "          Logic:
    "            visual command: changed to reflect operated on list
    "            normal command: adjusted only to reflect bracket relocation/re-indent
    return {
            \ 'mode': a:mode,
            \ 'range': [s:nullpos, s:nullpos],
            \ 'curpos': getpos('.'),
            \ 'vmarks': s:get_visual_marks(),
    \ }
endfunction

" Invoked by docount_stateful() after each iteration to perform state update.
" -- Args --
" state:    The state dict created by stackop__init() and updated by stackop__update().
" result:   Dict returned by sexp#stackop() to reflect the operation performed (see below)
" <rest>:   The invocation arguments, unmodified
" The 'result' dict contains optional keys, whose names generally correspond to names in
" the state dict. Provided keys are used in a possibly mode and/or option-dependent way to
" update the state dict.
function! sexp#stackop__update(state, result, mode, last, capture)
    if has_key(a:result, 'range')
        if !a:state.range[0][1]
            \ || sexp#compare_pos(a:result.range[0], a:state.range[0]) < 0
            let a:state.range[0] = a:result.range[0]
        endif
        if !a:state.range[1][1]
            \ || sexp#compare_pos(a:result.range[1], a:state.range[1]) > 0
            let a:state.range[1] = a:result.range[1]
        endif
    endif
    if a:state.mode ==? 'v' && has_key(a:result, 'vmarks')
        " Update visual marks to reflect updated list boundary.
        " Note: If command mode is non-visual, vmarks are adjusted only for bracket
        " relocations, and that happens automatically elsewhere.
        let a:state.vmarks = a:result.vmarks
    endif
    " Cursor modification logic
    if !a:capture && g:sexp_emitting_bracket_is_sticky
        " Don't let cursor be emitted by list.
        if sexp#compare_pos(a:result.vmarks[a:last], a:state.curpos) == (a:last ? -1 : 1)
            " Pull cursor inward to list boundary before next iteration.
            let a:state.curpos = a:result.vmarks[a:last]
        endif
    endif
    if a:capture && g:sexp_capturing_bracket_is_sticky && a:result.can_ride_bracket
        " Since cursor started on capturing bracket, let it relocate with it.
        let a:state.curpos = a:result.vmarks[a:last]
    endif
endfunction

" Invoked by docount_stateful() at the end of all iterations to restore appropriate cursor
" pos and/or visual state, and to perform re-indent if multiple lines were affected and
" user has enabled auto-reindent for capture/emit.
" Handles exceptional (!empty(ex) && ex != 'sexp-done') as well as normal termination.
function! sexp#stackop__final(ex, state, mode, last, capture)
    if empty(a:ex) || a:ex == 'sexp-done'
        " Normal termination
        if g:sexp_auto_indent != -1 ? g:sexp_auto_indent : g:sexp_capture_emit_does_indent
            " Logic: In most cases, accurate re-indent is ensured by using the range in
            " the state dict; however, there are corner cases (most notably, capture/emit
            " of head) in which it's necessary to re-indent the parent. Thus, if last == 0
            " and there is no sibling preceding the range, re-indent parent; otherwise,
            " re-indent the range in state dict.
            " Assumption: For an emit at head, range includes the emitted element.
            let indent_parent = 0
            let [s, e] = a:state.range
            if !a:last
                " Look for sibling preceding range.
                call s:setcursor(s)
                let p = sexp#current_element_terminal(0)
                " Note: nearest_element_terminal() returns current el terminal if no adjacent.
                let s = sexp#nearest_element_terminal(0, 0)
                if !sexp#compare_pos(p, s)
                    " No preceding sibling; re-indent parent (if it exists).
                    let p = s:move_to_nearest_bracket(0)
                    if p[1]
                        let s = p
                        " Position on parent close bracket for sexp#indent().
                        let e = s:move_to_nearest_bracket(1)
                        let indent_parent = 1
                    endif
                endif
            endif
            " Skip single-line indents.
            if s[1] != e[1]
                if indent_parent
                    " Assumption: Cursor is positioned on parent list bracket.
                    let mode = 'n'
                else
                    let mode = 'v'
                    " Range spans multiple lines, so re-indent.
                    call s:set_visual_marks([s, e])
                    " Note: Though we use visual marks, it's important that we be in normal mode.
                    call sexp#ensure_normal_mode()
                endif
                call sexp#indent(mode, 0, 1, -1, 1, [a:state.vmarks, a:state.curpos])
            endif
        endif
    endif
    " Restore visual marks whether we've adjusted only for bracket relocations or more
    " substantially (e.g., to prevent cursor escaping form when
    " g:sexp_emitting_bracket_is_sticky is set).
    " Assumption: vmarks and curpos have been adjusted for any bracket relocations.
    call sexp#ensure_normal_mode()
    call s:set_visual_marks(a:state.vmarks)
    " TODO: Should we handle the abnormal termination scenario any differently?
    if a:mode ==? 'v'
        " TODO: Any advantage to using select_current_marks() instead?
        normal! gv
    else
        keepjumps call s:setcursor(a:state.curpos)
    endif
endfunction

" Capture nearest non-comment sibling of current list, given the starting position of the
" list's bracket minus any leading macro characters (spos) and the position of the bracket
" itself (bpos).
" Note: For tail capture, spos == bpos.
" Return: Dict with the following keys:
"   vmarks  start/end of list that did the capture
"           Used in visual mode to select the list after operation completes
"   range   range affected by the operation
"           Used to calculate reindent
" Cursor Preservation: Caller handles.
function! s:stackop_capture(last, spos, bpos, ps)
    " Find position of matching bracket (needed only for returned range).
    call s:setcursor(a:bpos)
    " Note: Could defer this till after bracket relocation.
    let opp_bpos = s:nearest_bracket(!a:last)
    if !opp_bpos[1]
        " Note: Should happen only if there are unmatched brackets.
        throw 'sexp-error'
    endif
    " Position on outer edge of bracket construct (possibly macro chars) to be relocated.
    call s:setcursor(a:spos)
    let nextpos = a:spos
    " Move outwards, landing on successive elements' outer edges, looking for a
    " non-comment element, which will become the new terminal.
    while 1
        let prevpos = nextpos
        " Move to outside edge of adjacent element.
        let nextpos = sexp#move_to_adjacent_element_terminal(a:last, a:last, 0)
        " Make sure the call above actually moved us to a different element.
        " Note: sexp#move_to_adjacent_element_terminal() returns terminal position of
        " *current* element if no next element exists, and we started on the outside edge
        " of an element (i.e., the terminal position we'll end up on when there's no
        " adjacent element).
        " Assumption: sexp#move_to_adjacent_element_terminal() will not return nullpos
        " unless a:top is set.
        if sexp#compare_pos(prevpos, nextpos) == 0
            " Nothing to capture
            return {}
        endif
        if !sexp#is_comment(nextpos[1], nextpos[2])
            " Found a capturable (non-comment) element.
            break
        endif
    endwhile
    " Note: Augment adjusted positions to eliminate modification order constraints.
    let ps = s:concat_positions(a:ps, opp_bpos, nextpos)
    " Yank and delete the bracket (and possibly leading macro chars) to be relocated.
    let btext = s:yankdel_range(a:spos, a:bpos, 1, 1, ps, 1)

    " Put the yanked bracket construct on the *outside* of nextpos using directed put.
    call s:yankdel_range(nextpos, nextpos, btext, a:last ? [0, 1] : [1, 0], ps)

    " New bracket pos can be obtained from either '. or '] mark
    let bpos = getpos('.')
    " Update visual marks to reflect updated list boundary.
    let vmarks = a:last ? [opp_bpos, bpos] : [bpos, opp_bpos]
    " Note: For capture, operated-on range determined by capturing list bounds.
    " Assumption: No need to pull in leading macro chars, given how range is used.
    let range = vmarks
    return {'vmarks': vmarks, 'range': range}
endfunction

" Emit outermost non-comment element (along with any surrounding comments) from current
" list, given the starting position of the list's bracket minus any leading macro
" characters (spos) and the position of the bracket itself (bpos).
" Note: If only comments remain within the list, they will be emitted.
" Rationale: Never make a comment the terminal element of the emitting list, neither at
" head nor tail.
" Note: For tail capture, spos == bpos.
" Return: Dict with the same keys as stackop_capture()
" Design Decision: Although we could allow comment as terminal, provided we inserted a
" newline before bracket, let's stick as closely to original behavior as possible, and
" also do what was agreed upon in Github issue #13.
" Cursor Preservation: Caller handles.
function! s:stackop_emit(last, spos, bpos, ps)
    " Start on the list bracket.
    call s:setcursor(a:bpos)
    " Get opposite bracket, needed for empty list test.
    " Note: Could defer this till after bracket relocation.
    let opp_bpos = s:nearest_bracket(!a:last)
    " Move inwards onto the terminal element's outer edge.
    let [l, c] = s:findpos('\v\S', !a:last)
    " TODO: l == -1 shouldn't happen in well-formed buffer. Throw exception?
    if l < 1 | return {} | endif
    let nextpos = [0, l, c, 0]
    " Make sure findpos didn't find other bracket pos.
    if !sexp#compare_pos(opp_bpos, nextpos)
        " List is empty. Nothing to emit. Let caller decide how to handle.
        return {}
    endif
    call s:setcursor(nextpos)
    " Save pos of outermost element to be emitted.
    let outerpos = nextpos
    " Set this if we need to insert separating space around bracket.
    let insert_space = 0

    " Continue inwards, landing on successive elements' outer edges, looking for a
    " non-comment element, which can become the new terminal.
    " Note: When only one non-comment element remains, opposite bracket serves as terminal.
    while 1
        let prevpos = nextpos
        " Move to outside edge of adjacent element.
        let nextpos = sexp#move_to_adjacent_element_terminal(!a:last, a:last, 0)
        " Make sure we actually moved to a new element.
        " Rationale: sexp#move_to_adjacent_element_terminal() will not return nullpos
        " unless a:top is set.
        if sexp#compare_pos(sexp#current_element_terminal(a:last), prevpos) == 0
            " Final element being emitted; let bracket serve as "terminal".
            let nextpos = opp_bpos
            " Special Case: If no whitespace adjacent to opposite bracket, we need to
            " insert a space to avoid creating something like `()foo' or `foo()'.
            call s:setcursor(opp_bpos)
            if getline(opp_bpos[1])[a:last ? opp_bpos[2] : opp_bpos[2] - 2] !~ '\s'
                let insert_space = 1
            endif
            break
        endif
        if !sexp#is_comment(nextpos[1], nextpos[2])
            " Found non-comment element that can serve as terminal.
            break
        endif
    endwhile

    " Note: Augment adjusted positions to eliminate modification order constraints.
    let ps = s:concat_positions(a:ps, opp_bpos, outerpos, nextpos)
    " Yank and delete the bracket (and possibly leading macro chars) to be relocated.
    let btext = s:yankdel_range(a:spos, a:bpos, 1, 1, ps, 1)
    if insert_space
        let btext = a:last ? btext . ' ' : ' ' . btext
    endif

    " Put the yanked bracket construct on the *outside* of nextpos using directed put.
    " Note: If emitting final element, nextpos will be opposite bracket.
    call s:yankdel_range(nextpos, nextpos, btext, a:last ? [0, 1] : [1, 0], ps)

    " Get location of relocated bracket.
    " Note: getpos('.') and getpos("']") should be identical.
    let bpos = getpos('.')
    " Note: When emitting at end, inserted space necessitates adjustment.
    if insert_space && a:last
        let bpos[2] -= 1
    endif
    " Update visual marks to reflect updated list boundary.
    let vmarks = a:last ? [opp_bpos, bpos] : [bpos, opp_bpos]
    " Update the superset range of affected lines using outermost *emitted* element and
    " opposite bracket (whose position has not been affected by bracket relocation).
    let range = a:last ? [opp_bpos, outerpos] : [outerpos, opp_bpos]
    return {'vmarks': vmarks, 'range': range}
endfunction

" Capture or emit the first or last element into or out of the current list, unless
" current list is empty, in which case, we capture/emit from containing list, recursively.
" For normal mode command, cursor position is preserved (subject to constraints imposed by
" current setting of option g:sexp_emitting_bracket_is_sticky); for visual mode, operated
" on list will be selected.
"
" Note: docount_stateful() makes [count] calls to this function, followed by a single call
" to the __final() function to handle cleanup; thus, this function need only ensure that
" cursor position in state dict is correct after each iteration. Currently, the cursor
" position and/or visual marks we'll need to restore can be changed by emit/capture, but
" the modification logic is confined to the __update() function to obviate the need to
" make the implementation functions state dict-aware.
"
" Throws 'sexp-error' in case of abnormal termination and 'sexp-done' for a normal
" termination (when there are no more siblings *at any level* to emit/capture).
function! sexp#stackop(state, mode, last, capture)
    " Note: capture/emit function can change a:state.curpos: changes made by capture are
    " strictly adjustments for bracket relocation, but emit can make more dramatic
    " changes, depending on g:sexp_emitting_bracket_is_sticky option.
    call s:setcursor(a:state.curpos)
    let [_b, cursorline, cursorcol, _o] = getpos('.')
    let char = getline(cursorline)[cursorcol - 1]

    " Move to element tail first so we can skip leading macro chars
    let pos = sexp#move_to_current_element_terminal(1)

    " Move to closing bracket unless we are on one
    if !(pos[1] > 0 && getline(pos[1])[pos[2] - 1] =~# s:closing_bracket)
        let pos = s:move_to_nearest_bracket(1)
    endif

    " No paired bracket found, so not in a list
    if pos[1] < 1 | throw 'sexp-error' | endif

    if a:last
        let bpos = pos
    else
        let bpos = s:move_to_nearest_bracket(0)
        let pos = sexp#move_to_current_element_terminal(0)
    endif

    " Is cursor *on* bracket performing capture? Note that this test could be performed
    " once, up front (e.g., in __init()), though it's safe to perform it here since the
    " nature of captures precludes the result changing.
    let can_ride_bracket = a:capture && a:state.curpos == bpos
    " Loop till successful capture/emit performed or deemed impossible.
    while 1
        let result = s:{a:capture ? 'stackop_capture' : 'stackop_emit'}(
            \ a:last, pos, bpos,
            \ s:concat_positions(a:state.curpos, a:state.vmarks, a:state.range))
        if !empty(result)
            " Note: This is needed because stackop_capture doesn't know initial pos.
            let result.can_ride_bracket = can_ride_bracket
            return result
        endif
        " Blocked at current level; ascend if possible and retry.
        " Prevent riding higher level brackets.
        let can_ride_bracket = 0
        let bpos = s:move_to_nearest_bracket(a:last)
        if !bpos[1]
            throw 'sexp-done'
        endif
        " TODO: Validate the call to sexp#current_element_terminal()?
        let pos = a:last ? bpos : sexp#current_element_terminal(0)
    endwhile

    return result
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
    catch /stop-iter/
        " Provide a way for funcs to request early termination when they know there's no
        " point in continuing. So far, only select_current_list takes advantage of this.
    finally
        let s:countindex = -1
    endtry
endfunction

" Stateful version of sexp#docount. Client defines 3 optional functions, whose names are
" formed from a:func by appending one of the following suffixes:
"   __init:     Create and return a state object to be augmented by subsequent iterations.
"   __update:   Accepts result of current iteration and updates state object accordingly.
"   __final:    Performs cleanup at the end of all iterations. Called in both nominal and
"               off-nominal scenarios. Has access to the state object and exception if it
"               occurred.
function! sexp#docount_stateful(count, func, ...)
    " Synthesize the function names.
    " Design Decision: Intentional avoiding some newer Funcref capabilities to avoid
    " increasing Vim version requirement.
    let [init_fn, update_fn, final_fn] =
        \ map(['init', 'update', 'final'], "a:func . '__' . v:val")
    " Call init func (if it exists) to initialize state object, else use empty dict.
    let state = exists('*' . init_fn) ? call(init_fn, a:000) : {}
    let ex = ''
    try
        for n in range(a:count > 0 ? a:count : 1)
            " Perform single iteration.
            let ret = call(a:func, [state] + a:000)
            if exists('*' . update_fn)
                " Pass results of current iteration to update function.
                call call(update_fn, [state, ret] + a:000)
            endif
        endfor
    catch /sexp-/
        let ex = v:exception
    catch
        call sexp#warn#msg(printf("Internal error at %s: %s", v:throwpoint, v:exception))
    finally
        if exists('*' . final_fn)
            call call(final_fn, [ex, state] + a:000)
        endif
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
        \ && sexp#compare_pos(sexp#current_element_terminal(0), [0, line, col, 0]) < 0
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
        \ && !sexp#is_macro_char(prev)
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
        \ && sexp#compare_pos(sexp#current_element_terminal(0), [0, line, col, 0]) < 0
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
        " FIXME: keepjumps needed here?
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
            \ && !sexp#is_macro_char(prev)
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
