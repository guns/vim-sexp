" This autoload module contains shared functions that might be needed by the plugin script
" (e.g., for presenting warnings to user), but which needn't trigger load of the much
" larger autoload/sexp.
" TODO: Consider renaming this autoload module to ui or some such.

" Note: Do not uncomment the luaeval in this function, as it requires a logging module
" that's not currently part of the plugin.
" TODO: Eventually, remove it and all commented calls to it.
fu! sexp#warn#dbg(...)
    call luaeval("require'dp':get'sexp':logf(unpack(_A))", a:000)
endfu

" UTILITIES
" Return a composite string that contains all the inputs and preserves the boundaries
" between them: e.g., H("foo", "bar") is different from H("fo", "obar"), etc...
function! sexp#warn#join_hashable(...)
    " Separate elements with *untranslated* Ctrl-A.
    return join(map(a:000[:], "strtrans(v:val)"), "\x01")
endfunction

""" USER INTERFACE {{{1

" Display warning with requested highlighting.
function! s:warnmsg_impl(s, hl)
    try
        exe 'echohl' a:hl
        echomsg a:s
    finally
        echohl None
    endtry
endfunction

" Called by client to display warning to user via echomsg with appropriate highlighting
" (WarningMsg unless optional error flag is provided, in which case, use ErrorMsg).
" If we're inside {pre,post}_op() calls, there will be an active message queue, and we
" simply add the msg to it; otherwise (e.g., called from insert-mode command, which
" currently doesn't use the {pre,post}_op() mechanism), we echo the msg immediately.
" Rationale: Deferring the echomsg call is preferable because it greatly increases the
" probability it will be seen by the user before being overwritten (e.g., by an already
" pending redraw).
" TODO: Support printf-style formatting.
function! sexp#warn#msg(s, ...)
    let hl = a:0 && a:1 ? 'ErrorMsg' : 'WarningMsg'
    if exists('s:msg_q')
        " Add to queue.
        call add(s:msg_q, {'hl': hl, 'msg': a:s})
    else
        " Don't defer: display now.
        call s:warnmsg_impl(a:s, hl)
    endif
endfunction

" Wrapper for sexp#warn#msg(), which takes an arbitrary key representing the warning and
" ensures the warning is displayed only once per buffer.
" -- Optional Arg(s) --
" a:1  1 if warning should be displayed with error highlighting
fu! sexp#warn#msg_once(key, msg, ...)
    let b:sexp_did_warn = get(b:, 'sexp_did_warn', {})
    if get(b:sexp_did_warn, a:key, 0)
        " Already warned! Do nothing.
        return
    endif
    call sexp#warn#msg(a:msg, a:0 && !!a:1)
    " Make sure we don't display this one again for this buffer.
    let b:sexp_did_warn[a:key] = 1
endfu

" Display to user any messages queued during command execution.
function! sexp#warn#echo_queued_msgs()
    if empty(get(s:, 'msg_q', []))
        return
    endif
    " Perform any pending redraws now, to ensure they won't overwrite our message(s).
    redraw
    " Send all queued msgs.
    for m in s:msg_q
        " Get msg and highlight, noting that queue entry can be either a string (defaults
        " to warning) or a dict.
        let [msg, hl] = type(m) == type("") ? [m, 'WarningMsg'] : [m.msg, m.hl]
        call s:warnmsg_impl(msg, hl)
    endfor
    " Remove the processed msgs, but don't delete the queue, as only the creator knows
    " whether more messages may be queued.
    let s:msg_q = []
endfunction

function! sexp#warn#create_msg_q()
    let s:msg_q = []
endfunction

function! sexp#warn#destroy_msg_q()
    unlet! s:msg_q
endfunction

" vim:ts=4:sw=4:et:tw=90
