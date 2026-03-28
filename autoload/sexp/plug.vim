" This autoload module contains infrastructure/boilerplate that facilitates the execution
" of plug mappings in a consistent manner.

" Autoload and detect repeat.vim
" Note: Keeping this in an autoload module avoids loading repeat module before it's
" needed.
silent! call repeat#set('')
let s:have_repeat_set = exists('*repeat#set')

" Configure dot repeat for the specified command.
function! s:configure_repeat(plug_name, opmode, count)
    let [op_ipg, op_info] = [s:OP_ipg(), s:OP_get()]
    " Determine the sequence to pass to repeat#set.
    " Note: The identical sequence must be used for both setreg() and set().
    " Special Case: If the operator was c (change), the dot register should contain the
    " text we wish to insert over the sexp motion/object; however, the dot repeat will
    " simply do `c<Plug>(some_sexp_object)', leaving us in insert mode with nothing
    " inserted. To complete the desired operation, we must then use `<C-R>' to insert the
    " dot register, then use `<C-\><C-n>' to ensure our return to normal mode doesn't
    " trigger a beep.
    let opstr = a:opmode
        \ ? op_ipg
            \ ? "\<Plug>(" . op_info.plug_cmd . ")"
            \ : v:operator
        \ : ""
    let seq = a:opmode && v:operator ==? "c"
        \ ? opstr . "\<Plug>(" . a:plug_name . ")\<C-r>.\<C-Bslash>\<C-n>"
        \ : opstr . "\<Plug>(" . a:plug_name . ")"
    " Have separate counts have been supplied to operator and operand? 
    " Assumption: When separate counts are provided, both will be > 1 and the operand
    " count will be greater than the operator count (due to Vim's multiplication).
    if op_ipg && op_info.cnt > 1 && a:count > op_info.cnt
        " This is unlikely to do what the user expects.
        " Note that the test is designed to permit a non-unity count to be used on the
        " operator alone. If we didn't permit this, there would be no way to provide a
        " count to the sexp object in a custom mapping whose rhs looked like this:
        " <sexp_op> <sexp_object>.
        call sexp#warn#msg(
            \ "Warning: Separate counts have been provided to a sexp operator and"
            \ . " its operand. This is unlikely to work the way you expect: vim-sexp's"
            \ . " operators do not support counts and Vim multiplies the motion/object"
            \ . " count by the operator count. It's best to provide counts only to"
            \ . " motion/object commands.", {'once': 'operator counts'})
        call sexp#warn#dbg("Counts mismatch: operator: %d operand: %d", op_info.cnt, a:count)
    endif
    " If sexp operator in progress, use cached v:register.
    " Design Decision: Always pass register, even if unnamed.
    " Rationale: Failure to set register explicitly inhibits the repeat plugin's register
    " logic, with the result that a register specification for the "." command would be
    " ignored.
    " Note: Use a lambda with a closure to simplify the operator callback.
    let reg = op_ipg ? op_info.reg : v:register
    let RepFn = {->
        \ repeat#set(seq, a:count) && repeat#setreg(seq, reg)
    \ }
    if op_ipg
        " A sexp op is in progress! Allow repeat to be set *after* it completes.
        " Note: If the motion/object for a sexp operator is not a sexp motion/object, we
        " won't get here, so RepFn won't be set and repeat won't be configured. This is an
        " inherent limitation.
        call sexp#warn#dbg(
            \ "Setting op_ipg.RepFn for %s with seq=%s reg=%s", a:plug_name, seq, reg)
        let op_info.RepFn = RepFn
        " Record the fact that the operator motion/object is provided by sexp, as this
        " has implications for range handling.
        let op_info.motion = a:plug_name
    else
        " No need to defer setting repeat.
        call sexp#warn#dbg(
            \ "non-deferred repeat#set on %s with seq=%s reg=%s", a:plug_name, seq, reg)
        call RepFn()
    endif
endfunction

function! s:opfunc(mode, name, cnt, expr, ...)
    if !a:0
        " Let the operator function provide a stateful callback to be invoked to complete
        " the operation.
        " Note: Since the arguments required by the callback function are embedded in the
        " a:expr string, we can't really access them, but we don't really need to, so long
        " as the initial invocation packages up the required args with function().
        let [op, SexpFn] = eval(a:expr)
        " Note: Both 'opfunc' and TextYankPost handler will use this function as callback.
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
    call sexp#warn#dbg("!!!getpos(\"']\")=%s", string(getpos("']")))
    call sexp#pre_op(a:mode, a:name)
    let op_info = s:OP_get()
    " The callback function that completes the operation was packaged as the first
    " variadic arg in the rhs of the assignment to &opfunc above. The type flag is
    " provided by Vim's opfunc engine.
    " Caveat: Only when called by opfunc mechanism will we receive 3rd vararg
    " ('type').
    let [op, Fn, type] = a:000[0:2]
    let inclusive = op[0] == 'g' ? -1 : a:000[3]
    try
        " Note: The Fn callback contains everything it needs but type.
        call sexp#warn#dbg("Invoking the sexp op callback... %s ']=%s '>=%s",
            \ string(s:OP), string(getpos("']")), string(getpos("'>")))
        call Fn(type, inclusive, op_info.motion)
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
"   This function needs to determine command count from either v:count or v:prevcount.
"   Since neither <cmd> nor :<c-u> (even from visual mode) reset v:count, I was originally
"   taking a snapshot of it before the call to sexp#ensure_normal_mode(). However, this
"   approach breaks when the popular which-key plugin is in use; the reason, apparently,
"   is that which-key's mapping exits and restores visual mode before this wrapper gets a
"   chance to run, with the result that v:count has already been zeroed and its count
"   transferred to v:prevcount.
"   Solution: If we defer the snapshot till *after* ensuring exit to normal mode, we can use
"   v:prevcount for visual maps and v:count otherwise.
function! sexp#plug#wrapper(flags, mapmode, name, rhs)
    let opmode = a:mapmode[0] ==# 'o'
    call sexp#ensure_normal_mode()
    " Note: See note in header on v:count logic.
    let cnt = a:mapmode[0] ==# 'x' ? v:prevcount : v:count
    " Note: This commented block was added to fix a regression that occurred when we
    " started using <cmd> in lieu of :<c-u> for visual maps, but a refactor of
    " set_marks_around_current_list() has obviated the need for it. For details, see
    " 'Important Note' in that function's header.
    "if a:mapmode[0] =~? '[xv]'
    "    let p = getpos("'<")
    "    keepjumps call cursor(p[1], p[2])
    "endif
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
            " TODO: This may need special handling with g:sexp_regput_curpos* == 2.
            exe "normal! " . (opmode ? 'vv' : '') . "m`"
        endif
        " Don't set repeat if this is a sexp operator, but if it's an operator-pending
        " sexp command, set it in a way that takes any *active* operator (including a sexp
        " operator) into account. If the object/motion belongs to sexp, we'll concatenate
        " the following for repeat: {sexp operator} {sexp object/motion}
        " If the object/motion is non-sexp, we'll have no way of knowing what it was
        " (since Vim doesn't provide it), and will simply discard the repeat information
        " without ever calling repeat#set().
        if !a:flags.asexpr && a:flags.repeat && s:have_repeat_set
            call s:configure_repeat(a:name, opmode, cnt)
        endif
        " Note: v:count may already have been reset (and transferred to v:prevcount), but
        " cnt contains the value we want to use.
        " TODO: Remove 'prev' from the pattern now that it's been removed from cmd defs.
        let rhs = substitute(a:rhs, '\<v:\%(prev\)\?count\>', cnt, 'g')
        if a:flags.asexpr
            " Let opfunc decide what the mapping looks like (i.e., g@ or *yv).
            return call('s:opfunc', [a:mapmode[0], a:name, cnt, rhs])
        else
            " Not an operator: no need for deferred execution
            exe 'call' rhs
        endif
    finally
        if !a:flags.asexpr
            call sexp#post_op(a:mapmode[0], a:name)
        endif
    endtry
endfunction

" Return dict for a sexp operator in progress, else {}.
function! s:OP_get()
    return get(s:, 'OP', {})
endfunction

" Return true iff sexp operator is in progress.
function! s:OP_ipg()
    return !empty(get(s:, 'OP', {}))
endfunction

" Remove all autocmds used as part of the TextYankPost-based mechanism for handling
" operators and make the associated state dict empty; this is necessary because in
" addition to carrying information explicitly, it serves as an indication to the wrapper
" mechanism that a sexp operator is in progress.
function! s:OP_cleanup()
    let &report = s:OP.report_save
    let s:OP = {}
    call sexp#warn#dbg("OP_cleanup: mode=%s", mode(1))
    au! SexpTextYankPost
endfunction

" Cache the information needed to support repeat for a sexp operator.
" Pre-condition: Called when the operator is first triggered and only when the
" TextYankPost mechanism *isn't* in use.
" Note: If a sexp motion/object is used as operand, 'motion' key will be added.
function! s:OP_cache(name, cnt)
    let s:OP = {'plug_cmd': a:name, 'cnt': a:cnt, 'reg': v:register, 'motion': ''}
endfunction

" Configure use of TextYankPost (and other autocmds) to handle sexp operators in a way
" that permits detection of object/motion inclusivity.
function! s:OP_setup(Fn, name, cnt)
    " Note: If a sexp command provides the object/motion, 'motion' will be set to the plug
    " cmd.
    let s:OP = {
        \ 'plug_cmd': a:name,
        \ 'cnt': a:cnt,
        \ 'reg': v:register,
        \ 'unnamed_reg': getreg('"', 1, 1),
        \ 'z_reg': getreg('z', 1, 1),
        \ 'fn': a:Fn,
        \ 'range': [[0,0,0,0],[0,0,0,0]],
        \ 'inclusive': -1,
        \ 'motion': '',
        \ 'report_save': &report,
    \}
    if exists('*getreginfo')
        " Save/restore more complete information if reginfo() available.
        " Note: setreg() accepts values in various formats.
        let s:OP.unnamed_reg = getreginfo('"')
        let s:OP.z_reg = getreginfo('z')
    endif
    " Temporarily set 'report' to something very large to prevent "yanked N lines" msgs.
    " Caveat: Must ensure original value (saved in s:OP) is restored by cleanup.
    set report=1000000
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
    " Now that the yank is complete, restore all potentially impacted registers to their
    " pre-yank values.
    " Note: The unnamed register is clobbered in legacy Vim only, but since this behavior
    " is undocumented, safest approach is to restore unconditionally for both.
    call setreg('"', s:OP.unnamed_reg)
    call setreg('z', s:OP.z_reg)
    "call sexp#warn#dbg("OP_SafeState: mode=%s", mode(1))
    if mode() == 'n'
        " Yank is either complete or canceled; s:OP_handle() can tell the difference.
        call s:OP_handle()
    endif
endfunction

function! s:OP_handle()
    try
        " If we get here without a valid range having been set in OP_TextYankPost(), something
        " unexpected happened...
        if !empty(s:OP) && s:OP.range[0][1]
            "call sexp#warn#dbg("OP_handle: Handling op with cached TYP!")
            " Perform callback.
            let Fn = s:OP.fn
            call Fn('char', s:OP.inclusive)
        endif
    finally
        " Kill the autocmd unconditionally to eliminate possibility of endless error loops.
        call s:OP_cleanup()
    endtry
endfunction

" Cache yank information that will be needed by the operation, which is deferred to the
" SafeState handler.
" Rationale: TextYankPost autocmd prohibits buffer modifications.
" Caveat: Must defer restoration of @" and @z to the SafeState handler.
" Rationale: Legacy Vim overwrites the @" register with yanked text *after* TextYankPost
" handler returns; Neovim seems not to update @" at all when another register is the
" target of the yank, though neither behavior is documented explicitly. To ensure we
" restore @" to the value it had *before* the yank used to simulate g@, we defer the
" restore till SafeState fires.
function! s:OP_TextYankPost()
    let s:OP.range = [getpos("'["), getpos("']")]
    let s:OP.inclusive = v:event.inclusive
    "call sexp#warn#dbg("OP_TextYankPost: Cached TYP %s", string(s:OP))
    "call sexp#warn#dbg("OP_TextYankPost: v:event: %s", string(v:event))
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

" Validate and convert a single value in the sexp_mappings dict to a denormalized form
" conducive to use in map creation.
" Args:
"   plug:         command name (i.e., the ... in <Plug>(...))
"   entry:        lhs specification in one of the following forms:
"                 '<lhs>'
"                 | {'<modes1>': '<lhs1>', ..., '<modesN>': '<lhsN>'}
"   valid_modes:  string of chars in [nxo] constraining the modes for this command
"                 default: 'nox'
" Return: A pair representing the denormalized map and a string of ignored modes: e.g.,
"   [{'x': <lhs>, 'n': <lhs>, ...}, 'o']
" Exception: throw 'sexp-error' with appropriate message if entry format is irremediably
" invalid. If, OTOH, the problem is merely valid Vim modes that aren't in a:valid_modes,
" don't throw exception, but add the ignored modes to the string returned as second
" element of pair.
" Examples:
"   entry:         {'xn': 'lhs'}
"   valid_modes:   'x'
"   => [{'x': 'lhs'}, 'n']
"   entry:         {'xn': 'lhs1', 'o': 'lhs2'}
"   valid_modes:   'xo'
"   => [{'x': 'lhs1', 'o': 'lhs2'}, 'n'}
" TODO: Consider returning 2 denormalized maps: i.e., [valid_map, ignored_map].
function! sexp#plug#parse_map_entry(plug, entry, valid_modes)
    " Initialize return values.
    let [maps, ignored_modes] = [{}, '']
    let valid_modes = empty(a:valid_modes) ? 'novx' : a:valid_modes
    let entry = type(a:entry) == type({})
        \ ? a:entry
        \ : type(a:entry) == type('')
        \ ? {valid_modes: a:entry}
        \ : v:null
    if entry is v:null
        throw "sexp-error: Invalid user map entry: must be string or dict"
    endif
    " At this point, we have a dict, which may or may not be usable as a map override.
    for [modes, lhs] in items(entry)
        if type(modes) != type('')
            throw "sexp-error: Invalid mode string: " . string(modes)
        endif
        " Convert v to x: e.g., 'nvo' => 'nxo'
        let modes = substitute(modes, 'v', 'x', 'g')
        " Collapse multiple occurrences of same mode: e.g., {'x': ..., 'xo': ...}.
        let modes = substitute(
            \ join(sort(split(modes, '\zs')), ''), '\v(.)\1+', '\1', 'g')
        " Loop over sorted/uniquified mode chars.
        for mode in split(modes, '\zs')
            if mode !~ '[' . valid_modes . ']'
                " Differentiate between weird mode and one that's simply not valid for
                " this command: e.g., 'w' is not a valid mode for *any* sexp commands, but
                " 'x' may or may not be valid for this particular command.
                if mode !~ '[xno]'
                    throw printf("sexp-error: Invalid mode `%s'", mode)
                endif
                if stridx(ignored_modes, mode) < 0
                    let ignored_modes .= mode
                endif
            endif
            " We have a valid mode, but has it already been specified for this plug?
            " Design Decision: Treat only a conflicting specification (i.e., different
            " LHS's) as error.
            if has_key(maps, mode) && lhs != maps[mode]
                throw printf("sexp-error: Conflicting LHSs for mode %s: old=%s new=%s",
                    \ mode, maps[mode], lhs)
            endif
            " We have a valid, non-conflicting mode spec.
            let maps[mode] = lhs
        endfor
    endfor
    return [maps, ignored_modes]
endfunction

" vim:ts=4:sw=4:et:tw=90
