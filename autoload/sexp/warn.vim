" This autoload module contains shared functions that might be needed by the plugin script
" (e.g., for presenting warnings to user), but which needn't trigger load of the much
" larger autoload/sexp.
" TODO: Consider renaming this autoload module to ui or some such.

" Caveat: Do not set g:sexp_enable_debug_output in release contexts, as the luaeval
" requires a logging module that's not currently part of the plugin.
" TODO: Eventually, remove it and all commented calls to it.
fu! sexp#warn#dbg(...)
    if get(g:, 'sexp_enable_debug_output', 0)
        call luaeval("require'dp':get'sexp':logf(unpack(_A))", a:000)
    endif
endfu

" Return a composite string that contains all the inputs and preserves the boundaries
" between them: e.g., H("foo", "bar") is different from H("fo", "obar"), etc...
" TODO: Refactor of sexp#warn#msg() obviates need for this. Remove...
function! sexp#warn#join_hashable(...)
    " Separate elements with *untranslated* Ctrl-A.
    return join(map(a:000[:], "strtrans(v:val)"), "\x01")
endfunction

" Display warning with requested highlighting.
function! s:warnmsg_impl(s, hl)
    try
        exe 'echohl' a:hl
        echomsg a:s
    finally
        echohl None
    endtry
endfunction

" Display warning to user via echomsg (using message queue if active) with appropriate
" highlighting (ErrorMsg if optional error flag is provided, else WarningMsg).
" Message Queue Logic: If we're inside {pre,post}_op() calls, there will be an active
" message queue, and we simply add the msg to it, thereby deferring its display till
" command has completed; otherwise (e.g., called from insert-mode command, which currently
" doesn't use the {pre,post}_op() mechanism), we echo the msg immediately.
" Rationale: Deferring the echomsg call is preferable because it greatly increases the
" probability it will be seen by the user before being overwritten (e.g., by an already
" pending redraw).
" TODO: Support printf-style formatting.
function! s:display_msg(s, ...)
    let hl = a:0 && a:1 ? 'ErrorMsg' : 'WarningMsg'
    if exists('s:msg_q')
        " Add to queue.
        call add(s:msg_q, {'hl': hl, 'msg': a:s})
    else
        " Don't defer: display now.
        call s:warnmsg_impl(a:s, hl)
    endif
endfunction

" Display provided message with :echomsg, taking flags in options dict into account.
" -- Option Dict Keys --
" once:    either explicit enable/disable requesting "once-only" msg display (in which
"          case, the msg itself is used as uniqueness key) or an object used to generate
"          uniqueness key.
"          Logic: Treat things that look like explicit enable/disable flags (e.g.,
"          boolean/null/0/1) specially. Also, since empty things aren't effective
"          uniqueness keys, treat them as false.
"          Design Decision: 0 and 1 are the only numbers treated as boolean.
"          Rationale: Legacy (pre v:{true,false}) usage
" session: (applies only in "once-only" mode) true for once-per-session, false (or
"          omitted) for once-per-buffer
" err:     true requests ErrorMsg highlighting
fu! sexp#warn#msg(msg, ...)
    let opts = extend(
        \ {'once': v:null, 'session': 0, 'err': 0},
        \ a:0 ? a:1 : {}, "force")
    let key =
        \ empty(opts.once) || opts.once is v:null || opts.once is v:false
        \ ? v:null
        \ : opts.once is v:true || opts.once is 1
        \ ? msg
        \ : string(opts.once)
    " Assumption: A non-empty key implies once-only.
    if !empty(key)
        let scope = opts.session ? s: : b:
        " Auto-vivify if necessary.
        let scope.sexp_did_warn = get(scope, 'sexp_did_warn', {})
        if get(scope.sexp_did_warn, key, 0)
            " Already warned! Do nothing.
            return
        endif
        " Make sure we don't display this one again for this buffer.
        let scope.sexp_did_warn[key] = 1
    endif
    " Arrival here means we're not skipping output due to "once-only" mechanism.
    call s:display_msg(a:msg, opts.err)
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
