" This autoload module contains infrastructure/boilerplate that facilitates the execution
" of plug mappings in a consistent manner.

" Autoload and detect repeat.vim
" Note: Keeping this in an autoload module avoids loading repeat module before it's
" needed.
silent! call repeat#set('')
let s:have_repeat_set = exists('*repeat#set')

function! s:opfunc(mode, name, cnt, expr, ...)
    if !a:0
        " Let the operator function provide a stateful callback to be invoked to complete
        " the operation.
        " Note: Since the arguments required by the callback function are embedded in the
        " a:expr string, we can't really access them, but we don't really need to, so long
        " as the initial invocation packages up the required args with function().
        let [op, SexpFn] = eval(a:expr)
        let Fn = function('s:opfunc', [a:mode, a:name, a:cnt, a:expr, op, SexpFn])
        if op[0] == 'g'
            let &opfunc = Fn
            " Cache the operator for repeat mechanism.
            call sexp#warn#dbg("Caching OP")
            call s:OP_cache(a:name, a:cnt)
        else
            " Cache operator and configure TextYankPost autocmd.
            call sexp#warn#dbg("Calling OP_setup()")
            call s:OP_setup(Fn, a:name, a:cnt)
        endif
        return op
    endif
    call sexp#pre_op(a:mode, a:name)
    " The callback function that completes the operation was packaged as the first
    " variadic arg in the rhs of the assignment to &opfunc above. The type flag is
    " provided by Vim's opfunc engine.
    " Caveat: Only when called by opfunc mechanism will we receive 3rd vararg ('type').
    let [op, Fn, type] = a:000[0:2]
    let inclusive = op[0] == 'g' ? -1 : a:000[3]
    try
        " Note: The Fn callback contains everything it needs but type.
        call sexp#warn#dbg("Invoking the sexp op callback... %s", string(s:OP))
        call Fn(type, inclusive)
        let RepFn = get(s:OP, 'RepFn', v:null)
        if type(RepFn) == v:t_func
            call sexp#warn#dbg("Invoking RepFn()")
            call RepFn()
        endif
    finally
        call sexp#post_op(a:mode, a:name)
    endtry
endfunction

" This function is a wrapper for sexp <Plug> commands used to ensure common boilerplate
" runs at the appropriate time. It is invoked by the <Plug> mappings defined by
" s:defplug() in the plugin script; thus, it will not force load of this autoload file
" until a mapping is invoked.
" Note: If we eventually split the functions in this file across more autoload scripts, it
" might make sense to move this into a dedicated autoload module to avoid loading
" functionality unnecessarily.
" Several points to note...
" RE: vv
"   Due to a ?bug? in vim, we need to set curwin->w_curswant to the
"   current cursor position by entering and exiting character-wise visual
"   mode before completing an operator-pending command so that the cursor
"   returns to its original position after an = command.
" RE: v:count
"   Currently, the count arg is captured before this function escapes to normal mode (if
"   it does). In particular, neither <cmd> nor :<c-u> (even from visual mode) reset
"   v:count, so we should never need to use v:prevcount.
" TODO: Consider making this an autoload function like s:opfunc().
function! sexp#plug#wrapper(flags, mapmode, name, count, rhs)
    let opmode = a:mapmode[0] ==# 'o'
    " Assumption: v:count does not change before this call.
    call sexp#ensure_normal_mode()
    " Caveat: For a sexp operator (e.g., replace_op), the {pre,post}_op wrapper
    " invocations are deferred till operation completion.
    if !a:flags.asexpr
        call sexp#pre_op(a:mapmode[0], a:name)
    endif
    call sexp#warn#dbg("sexp#plug#wrapper: %s: OP=%s", a:name, string(get(s:, 'OP', {})))
    try
        " Caveat: Can't set marks in an <expr> mapping. If it's necessary, the opfunc
        " should save the old '` and restore it at completion of operation.
        if !a:flags.nojump && !a:flags.asexpr
            " TODO: This may need special handling with g:sexp_regput{_into}_curpos == 2.
            exe "normal! " . (opmode ? 'vv' : '') . "m`"
        endif
        let op_ipg = get(s:, 'OP', {})
        "let cnt = max([1, a:count])
        let cnt = a:count
        " Don't set repeat if this is a sexp operator, but if it's an operator-pending
        " sexp command, set it in a way that takes any *active* operator (including a sexp
        " operator) into account. Note that for this to work, we have to save both the
        " plug cmd and any count associated with the sexp operator when it's first
        " invoked. If the object/motion belongs to sexp, we'll concatenate the following
        " for repeat:
        "   {op count} {sexp operator} {object/motion count} {sexp object/motion}
        " If the object/motion is non-sexp, we'll have no way of knowing what it was, and
        " will simply discard the repeat information without ever calling repeat#set().
        if !a:flags.asexpr && a:flags.repeat && s:have_repeat_set
            " Note: The identical sequence must be used for both setreg() and set().
            " Special Case: If the operator was c (change), the dot register should
            " contain the text we wish to insert over the sexp motion/object. The
            " repeat effectively does `c<Plug>(some_sexp_object)', leaving us in
            " insert mode; thus, we use `<C-R>' to insert the dot register, then use
            " `<C-\><C-n>' to ensure our return to normal mode doesn't trigger a beep.
            let opstr = opmode
                \ ? !empty(get(s:, 'OP', {}))
                    \ ? (s:OP.cnt > 1 ? s:OP.cnt : '') . "\<Plug>(" . s:OP.plug_cmd . ")"
                    \ : v:operator
                \ : ""
            " TODO: Decide on best way to handle the 2 relevant counts.
            let cntstr = !empty(op_ipg) ? (cnt > 1 ? cnt : '') : ''
            let seq = opmode && v:operator ==? "c"
                \ ? opstr . cntstr . "\<Plug>(" . a:name . ")\<C-r>.\<C-Bslash>\<C-n>"
                \ : opstr . cntstr . "\<Plug>(" . a:name . ")"
            " The repeat plugin defaults to the unnamed register, so don't bother setting
            " reg unless current register is named.
            let reg = v:register != '"' ? v:register : ''
            " Design Decision: Always pass repeat#set() a count of 1.
            " Rationale: Allows us to embed distinct counts for operator and motion/object
            " in the actual repeat sequence.
            " Note: Use a lambda with a closure to simplify the operator callback.
            let RepFn = {->
                \ repeat#set(seq, cnt) && !empty(reg)
                \ ? repeat#setreg(seq, reg) : 0
            \ }
            if !empty(op_ipg)
                " A sexp op is in progress! Allow repeat to be set *after* it completes.
                " Note: If the motion/object for a sexp operator is not a sexp
                " motion/object, RepFn won't be set and repeat won't be configured.
                call sexp#warn#dbg(
                    \ "Setting op_ipg.RepFn for %s with seq=%s reg=%s", a:name, seq, reg)
                let op_ipg.RepFn = RepFn
            else
                " No need to defer setting repeat.
                call sexp#warn#dbg(
                    \ "non-deferred repeat#set on %s with seq=%s reg=%s", a:name, seq, reg)
                call RepFn()
            endif
        endif
        " Note: v:count may already have been reset (and transferred to v:prevcount), but
        " we snapshotted it in the call to this function, so use that.
        let rhs = substitute(a:rhs, '\<v:\%(prev\)\?count\>', cnt, 'g')
        if a:flags.asexpr
            " Let opfunc decide what the mapping looks like (i.e., g@ or *yv).
            return call('s:opfunc', [a:mapmode[0], a:name, cnt, rhs])
        else
            " No need for deferred execution
            exe 'call' rhs
        endif
    finally
        if !a:flags.asexpr
            call sexp#post_op(a:mapmode[0], a:name)
        endif
    endtry
endfunction

" Remove all autocmds used as part of the TextYankPost-based mechanism for handling
" operators and make the associated state dict empty; this is necessary because in
" addition to carrying information explicitly, it serves as an indication to the wrapper
" mechanism that a sexp operator is in progress.
function! s:OP_cleanup()
    let s:OP = {}
    call sexp#warn#dbg("OP_cleanup: mode=%s", mode(1))
    au! SexpTextYankPost
endfunction

" Cache the information needed to support repeat for a sexp operator.
" Pre-condition: Called when the operator is first triggered and only when the
" TextYankPost mechanism isn't in use.
function! s:OP_cache(name, cnt)
    let s:OP = {'plug_cmd': a:name, 'cnt': a:cnt}
endfunction

" Configure use of TextYankPost (and other autocmds) to handle sexp operators in a way
" that permits detection of object/motion inclusivity.
function! s:OP_setup(Fn, name, cnt)
    let s:OP = {
        \ 'plug_cmd': a:name,
        \ 'cnt': a:cnt,
        \ 'unnamed_reg': @",
        \ 'z_reg': @z,
        \ 'fn': a:Fn,
        \ 'range': [[0,0,0,0],[0,0,0,0]],
        \ 'inclusive': -1,
    \}
    call sexp#warn#dbg("Configuring TYP callback OP=%s", string(s:OP))
    augroup SexpTextYankPost
        au! TextYankPost <buffer> ++once call <SID>OP_TextYankPost()
        " This one may fire before TYP, but if it fires when we're in normal mode, the
        " yank has probably been canceled.
        au! SafeState <buffer> call s:OP_SafeState()
        " Note: ModeChanged is problematic because we can go from nov->n *before*
        " TextYankPost fires: e.g., this happens when a sexp object is used to specify the
        " yank region. Better to rely on SafeState, which shouldn't fire in Normal mode
        " until the yank completes or is canceled; note, however, that it can fire in (eg)
        " command-line mode if user hits / to perform the search.
        "au! ModeChanged nov:n call <SID>TYP_mode_changed()
    augroup END
endfunction

" Monitor SafeState autocmd, invoking our operator callback upon return to Normal mode.
" Explanation: Ideally, we would simply invoke our operator callback from the TextYankPost
" autocmd, but since that autocmd prohibits buffer changes, we wait until we're back in
" Normal mode after the yank has completed.
" Note: The SafeState autocmd may fire many times (e.g., in command mode) before the yank
" completes.
function! s:OP_SafeState()
    call sexp#warn#dbg("OP_SafeState: mode=%s", mode(1))
    if mode() == 'n'
        " Yank is either complete or canceled; s:OP_handle() can tell the difference.
        call s:OP_handle()
    endif
endfunction

function! s:OP_handle()
    " If we get here without a valid range having been set in OP_TextYankPost(), something
    " unexpected happened...
    if !empty(s:OP) && s:OP.range[0][1]
        call sexp#warn#dbg("OP_handle: Handling op with cached TYP!")
        " Perform callback.
        let Fn = s:OP.fn
        call Fn('char', s:OP.inclusive)
    endif
    call s:OP_cleanup()
endfunction

" Cache the yank information and restore the registers clobbered by the yank, letting the
" operation be performed in the SafeState handler.
" Rationale: TextYankPost autocmd prohibits buffer modifications.
function! s:OP_TextYankPost()
    let [@", @z] = [s:OP.unnamed_reg, s:OP.z_reg]
    let s:OP.range = [getpos("'["), getpos("']")]
    let s:OP.inclusive = v:event.inclusive
    call sexp#warn#dbg("OP_TextYankPost: Cached TYP %s", string(s:OP))
endfunction

" Calls repeat#set() and registers a one-time CursorMoved handler to correctly
" set the value of g:repeat_tick.
"
" cf. https://github.com/tpope/vim-repeat/issues/8#issuecomment-13951082
function! s:repeat_set(buf, count)
    call repeat#set(a:buf, a:count)
    " TODO: Should probably remove this autocmd after validating the new
    " sexp#plug#wrapper().
    " Rationale: The purpose of the autocmd is to set g:repeat_tick *after* the sexp
    " command has completed, but sexp#plug#wrapper() already does this. Previously, the
    " autocmd was needed because repeat#set was called *before* the sexp command ran.
    " Note: Another problem with using the CursorMoved autocmd this way is that the Vim
    " help says "Not always triggered... while executing commands in a script file", which
    " doesn't sound like a firm guarantee, and we definitely don't want g:repeat_tick set
    " before the sexp command completes.
    augroup sexp_repeat
        autocmd!
        " FIXME: Remove the debug stuff after testing.
        autocmd CursorMoved <buffer>
                    \ if b:changedtick != g:repeat_tick |
                    \ echomsg "Oops!" b:changedtick "!=" g:repeat_tick |
                    \ endif |
                    \ let g:repeat_tick = b:changedtick | autocmd! sexp_repeat
    augroup END
endfunction

" vim:ts=4:sw=4:et:tw=90
