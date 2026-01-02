" This autoload module contains functions for parsing arbitrary lisp using the
" legacy syntax engine. It is the legacy Vim counterpart to ts.lua.

" Script-local vars facilitating reuse of a vim-sexp parse buffer.
let s:bufnr = -1
let s:parsebuf_name = "vim-sexp-parser-playground"

" Symbolic constants
let s:nullpos = [0,0,0,0]

" Return 1 iff we're pretty certain the specified buffer is ours.
function! s:is_valid_parse_buffer(bufnr)
    return a:bufnr > 0 && bufexists(a:bufnr) && bufname(a:bufnr) == s:parsebuf_name
        \ && getbufvar(a:bufnr, '&buftype') == 'nofile'
        \ && getbufvar(a:bufnr, '&bufhidden') == 'hide'
        \ && !getbufvar(a:bufnr, '&swapfile')
endfunction

function! s:enter_parse_buffer()
    " Save and invalidate s:bufnr, re-assigning only after we know we're in a valid target
    " buffer.
    let [bufnr, s:bufnr] = [s:bufnr, -1]
    " Do we have an old buffer we can *safely* reuse (i.e, hasn't been repurposed by user
    " since we last used it)?
    if s:is_valid_parse_buffer(bufnr)
        try
            " Although parse buffer will most likely never be displayed, we can't
            " guarantee a curious user won't open it in a window. If he does, we need to
            " leave it open to keep the parsing implementation completely invisible to the
            " user. The simplest way to accomplish this is to use 'switchbuf' to ensure we
            " always open the parse buffer in a new window (even if it's already
            " displayed), thereby permitting an unconditional close after parsing.
            let switchbuf_save = &switchbuf
            set switchbuf=
            silent exe bufnr . "sb"
            " Design Decision: Require buffer to be empty.
            " Vim Idiosyncrasy: line2byte should never return -1, but sometimes does. This
            " logic accounts for that.
            if line2byte(line('$') + 1) - 1 <= 0
                let s:bufnr = bufnr()
            else
                " Buffer not empty! Silently refuse to use it (by closing and leaving
                " s:bufnr invalid).
                hide
            endif
        finally
            let &switchbuf = switchbuf_save
        endtry
    endif
    if s:bufnr < 0
        " Create parse buffer.
        silent exe "new" s:parsebuf_name
        setlocal buftype=nofile bufhidden=hide noswapfile
        let s:bufnr = bufnr()
    endif
endfunction

function! s:exit_parse_buffer()
    " Note: This is being overly cautious; we should always be in the parse buffer at this
    " point.
    if s:is_valid_parse_buffer(s:bufnr)
        " No need to keep the parsed text in memory.
        " TODO: Consider a simple undo...
        "%d _
        silent undo
        hide
    else
        call sexp#warn#msg("vim-sexp: Refusing to unload parse buffer " . s:bufnr
            \ . " because it's in an unexpected state."
            \ . " If warning persists, please open an issue.")
    endif
endfunction

function! s:analyze_codestr(ret)
    " Cache ref to the dict we're responsible for tweaking/filling in.
    let ret = a:ret
    " Start parsing on first char, which is guaranteed to be non-ws.
    normal! gg0
    let p = getpos('.')
    while p[1]
        " We're on the start of an element. Find its end.
        let ps = p
        let pe = sexp#move_to_current_element_terminal(1)
        if !pe[1]
            " Hint: Probably unbalanced open
            "echomsg printf("Hint: Unbalanced open at %s", string(ps))
            let ret.err_loc = ps
            let ret.err_hint = "Unbalanced open"
            break
        endif
        " Accumulate valid top-level node.
        call add(ret.node_ranges, [ps, pe])
        let ret.elem_count += 1
        if !ret.has_com
            let ret.has_com = sexp#is_comment(ps[1], ps[2])
            if ret.has_com && ret.elem_count == 0
                let ret.s_is_com = 1
            endif
        endif
        " Attempt to find near side of next element.
        let p = sexp#move_to_adjacent_element_terminal(1, 0, 1, 1)
        if !p[1]
            " Are we at the end? If not, we've likely encountered unbalanced close.
            let [curpos, epos] = [getpos('.'), [0, line('$'), col([line('$'), '$']), 0]]
            if sexp#compare_pos(curpos, epos) < 0
                \ && sexp#range_has_non_ws(sexp#offset_char(curpos, 1), epos, 1)
                " Hint: Unbalanced open at curpos.
                "echomsg printf("Hint: Unbalanced close after %s", string(curpos))
                let ret.err_loc = curpos
                let ret.err_hint = "Unbalanced close"
                break
            endif
        endif
    endwhile
    if ret.elem_count > 0 && !ret.err_loc[1]
        let ret.e_is_com =
            \ sexp#is_comment(ret.node_ranges[-1][0][1], ret.node_ranges[-1][0][2])
    endif
    let ret.is_ml = ret.elem_count > 0
        \ && ret.node_ranges[0][0][1] < ret.node_ranges[-1][1][1]
    return ret
endfunction

function! sexp#parse#analyze_codestr(codestr, filetype)
    " Note: Several fields are initialized to final values here; the ones requiring a
    " parse are filled in by s:analyze_codestr().
    let ret = {
        \ 'elem_count': 0,
        \ 'is_ml': 0,
        \ 'has_com': 0, 's_is_com': 0, 'e_is_com': 0,
        \ 'node_ranges': [],
        \ 'err_loc': s:nullpos, 'err_hint': '',
        \ 'text': substitute(a:codestr, '^\s*\|\s*$', '', 'g'),
        \ 'linewise': g:sexp_regput_untrimmed_is_linewise
            \ ? a:codestr =~ '^\s\|\s$'
            \ : a:codestr =~ '\n$'
    \ }
    " Be sure we end up in starting window, with original window layout.
    let [winnr, wrc] = [winnr(), winrestcmd()]
    " 'lz' should already be set, but just in case...
    let lz_save = &lazyredraw
    try
        set lz
        " Make sure we're in a scratch buffer we can use for parsing.
        call s:enter_parse_buffer()
        " Note: Inserting with P ensures we won't insert leading blank line if ret.text
        " ends in newline (thereby triggering a linewise put); however, we currently trim
        " the surrounding whitespace to simplify logic in s:analyze_codestr(), so either
        " type of of put would work.
        silent exe "normal! \"=ret.text\<cr>P"
        " Make sure we have syntax regions for parsing.
        let &l:ft = a:filetype
        " Parse the buffer and fill in the return dict.
        call s:analyze_codestr(ret)
    finally
        " Hide the parse buffer and return to the original window.
        call s:exit_parse_buffer()
        exe winnr . 'wincmd w'
        " Restore window sizes.
        " Assumption: Open window set is identical to what it was when wrc was captured.
        exe wrc
        let &lz = lz_save
    endtry
    " Note: Empty dict will be returned on error.
    return ret
endfunction

" vim:ts=4:sw=4:et:tw=90
