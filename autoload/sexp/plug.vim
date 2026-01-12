" This autoload module contains infrastructure/boilerplate that facilitates the execution
" of plug mappings in a consistent manner.

" Autoload and detect repeat.vim
" Note: Keeping this in an autoload module avoids loading repeat module before it's
" needed.
silent! call repeat#set('')
let s:have_repeat_set = exists('*repeat#set')

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
    "echomsg "before:" v:count v:prevcount a:count
    " Assumption: v:count does not change before this call.
    call sexp#ensure_normal_mode()
    "echomsg "after:" v:count v:prevcount a:count
    " Caveat: For a sexp operator (e.g., replace_op), the {pre,post}_op wrapper
    " invocations are deferred till operation completion.
    if !a:flags.asexpr
        call sexp#pre_op(a:mapmode[0], a:name)
    endif
    try
        " Caveat: Can't set marks in an <expr> mapping. If it's necessary, the opfunc
        " should save the old '` and restore it at completion of operation.
        if !a:flags.nojump && !a:flags.asexpr
            " TODO: This may need special handling with g:sexp_regput{_into}_curpos == 2.
            exe "normal! " . (opmode ? 'vv' : '') . "m`"
        endif
        " repeat (register) setup (prolog)
        " Note: Only sexp motions/objects (not sexp operators such as replace) should be repeated.
        " Rationale: The repeat sequence concatenates the operator and motion/object; an
        " operator by itself is nothing until combined with a target.
        if !a:flags.asexpr && a:flags.repeat && s:have_repeat_set
            " Note: The identical sequence should be used for both setreg() and set().
            " FIXME: Reevaluate the assumptions made wrt repeat: e.g., should we simply
            " assume this is operator-pending map? Also, need to configure repeat for sexp
            " operators in opfunc once the motion/object is known.
            " Special Case: If the operator was c (change), the dot register should
            " contain the text we wish to insert over the sexp motion/object. The
            " repeat effectively does `c<Plug>(some_sexp_object)', leaving us in
            " insert mode; thus, we use `<C-R>' to insert the dot register, then use
            " `<C-\><C-n>' to ensure our return to normal mode doesn't trigger a beep.
            let repseq = opmode && v:operator ==? "c"
                \ ? v:operator . "\<Plug>(" . a:name . ")\<C-r>.\<C-Bslash>\<C-n>"
                \ : v:operator . "\<Plug>(" . a:name . ")"
            " The repeat plugin defaults to unnamed register, so don't bother setting
            " unless current register is named.
            if v:register != '"'
                call repeat#setreg(repseq, v:register)
            endif
        endif
        " Note: v:count may already have been reset (and transferred to v:prevcount);
        " fortunately, we snapshotted it in the call to this function, so use that.
        let rhs = substitute(a:rhs, '\<v:\%(prev\)\?count\>', a:count, 'g')
        if a:flags.asexpr
            return call('s:opfunc', [a:mapmode[0], a:name, rhs])
        else
            exe 'call' rhs
        endif
        " repeat setup (epilog)
        if !a:flags.asexpr && a:flags.repeat && s:have_repeat_set
            call repeat#set(repseq, a:count)
        endif
    finally
        if !a:flags.asexpr
            call sexp#post_op(a:mapmode[0], a:name)
        endif
    endtry
endfunction

function! s:opfunc(mode, name, expr, ...)
    if !a:0
        " Let the operator function provide a stateful callback to be invoked to complete
        " the operation.
        " Note: Since the arguments required by the callback function are embedded in the
        " a:expr string, we can't really access them, but we don't really need to, so long
        " as the initial invocation packages up the required args with function().
        let Fn = eval(a:expr)
        " FIXME: This will get the extra type arg, which we need to pass to replace_op.
        let &opfunc = function('s:opfunc', [a:mode, a:name, a:expr, Fn])
        return 'g@'
    endif
    call sexp#pre_op(a:mode, a:name)
    " The callback function that completes the operation was packaged as the first
    " variadic arg in the rhs of the assignment to &opfunc above. The type flag is
    " provided by Vim's opfunc engine.
    let [Fn, type] = a:000
    try
        " Note: The Fn callback contains everything it needs but type.
        call Fn(type)
    finally
        call sexp#post_op(a:mode, a:name)
    endtry
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
