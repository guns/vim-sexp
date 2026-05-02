
"              o8o
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

if exists('g:sexp_loaded')
    finish
endif
let g:sexp_loaded = 1

""" Helper functions {{{1
" Echo warning to message history if any of the option variables in the input list have
" been set globally.
"   optnames: list of option names without the g: prefix
"   obsolete: 1=obsolete, 0=deprecated
"   helphint: help hint to display after the warning
function! s:deprecate_options(optnames, obsolete, helphint)
    " Build list of opts requiring warning.
    let opts = []
    for opt in a:optnames
        if exists('g:' . opt)
            call add(opts, opt)
        endif
    endfor
    if len(opts)
        " Warn!
        echohl ErrorMsg
        echomsg printf("Warning: The following sexp option%s been %s:",
            \ len(opts) > 1 ? "s have" : " has",
            \ a:obsolete ? "removed" : "deprecated")
        echomsg "  " . join(map(opts, "'g:' . v:val"), ", ")
        echomsg printf("Remove the corresponding assignment%s"
                    \ . " (e.g., from your vimrc) to disable this warning.",
                    \ len(opts) > 1 ? "s" : "")
        if len(a:helphint)
            " Display the provided help hint.
            echomsg "  " . a:helphint
        endif
        echohl None
    endif
endfunction

" Caveat: This must be called *prior to* option/mapping processing.
call sexp#feat#record_user_awareness()

" Note: The following options were introduced by PR #34 and removed in PR #51. Hopefully,
" not many users have overridden them, but just in case...
" TODO: Remove this after a few releases.
call s:deprecate_options([
            \ 'sexp_cleanup_lineshift_limit',
            \ 'sexp_cleanup_colshift',
            \ 'sexp_cleanup_colshift_slope'],
            \ 1, ":help sexp-outer-element-selection-logic")

""" Global State {{{1

if !exists('g:sexp_filetypes')
    let g:sexp_filetypes = 'clojure,scheme,lisp,timl,fennel'
endif

if !exists('g:sexp_enable_insert_mode_mappings')
    let g:sexp_enable_insert_mode_mappings = 1
endif

if !exists('g:sexp_insert_after_wrap')
    let g:sexp_insert_after_wrap = 1
endif

if !exists('g:sexp_indent_does_clean')
    let g:sexp_indent_does_clean = 0
endif

if !exists('g:sexp_clone_does_indent')
    let g:sexp_clone_does_indent = 1
endif

if !exists('g:sexp_capture_emit_does_indent')
    let g:sexp_capture_emit_does_indent = 1
endif

if !exists('g:sexp_raise_does_indent')
    let g:sexp_raise_does_indent = 1
endif

if !exists('g:sexp_splice_does_indent')
    let g:sexp_splice_does_indent = 1
endif

if !exists('g:sexp_emitting_bracket_is_sticky')
    let g:sexp_emitting_bracket_is_sticky = 0
endif

if !exists('g:sexp_capturing_bracket_is_sticky')
    let g:sexp_capturing_bracket_is_sticky = 0
endif

if !exists('g:sexp_auto_indent')
    let g:sexp_auto_indent = -1
endif

if !exists('g:sexp_prefer_legacy_syntax')
    let g:sexp_prefer_legacy_syntax = 0
endif

if !exists('g:sexp_cleanup_keep_empty_lines')
    let g:sexp_cleanup_keep_empty_lines = 1
endif

if !exists('g:sexp_cleanup_collapse_whitespace')
    let g:sexp_cleanup_collapse_whitespace = 0
endif

if !exists('g:sexp_cleanup_join_affinity')
    let g:sexp_cleanup_join_affinity = 1
endif

if !exists('g:sexp_cleanup_join_textwidth')
    let g:sexp_cleanup_join_textwidth = -1
endif

if !exists('g:sexp_cleanup_join_multiline')
    let g:sexp_cleanup_join_multiline = 0
endif

if !exists('g:sexp_cleanup_join_comments')
    let g:sexp_cleanup_join_comments = 1
endif

if !exists('g:sexp_cleanup_join_backwards')
    let g:sexp_cleanup_join_backwards = 1
endif

if !exists('g:sexp_indent_aligns_comments')
    let g:sexp_indent_aligns_comments = 0
endif

if !exists('g:sexp_auto_indent_range')
    " Rationale: Larger (non-optimized) indent ranges are best when aligning comments, but
    " toplevel can be noticeably slow for operations on large functions.
    let g:sexp_auto_indent_range = 0
endif

if !exists('g:sexp_aligncom_maxshift')
    let g:sexp_aligncom_maxshift = -1
endif

if !exists('g:sexp_aligncom_maxgap')
    let g:sexp_aligncom_maxgap = 7
endif

if !exists('g:sexp_aligncom_break_at_comment')
    let g:sexp_aligncom_break_at_comment = 0
endif

if !exists('g:sexp_aligncom_ignore_non_comments')
    let g:sexp_aligncom_ignore_non_comments = 0
endif

if !exists('g:sexp_aligncom_textwidth')
    let g:sexp_aligncom_textwidth = -1
endif

if !exists('g:sexp_aligncom_colstops')
    let g:sexp_aligncom_colstops = 2
endif

" Set these weights to values between 0 and 10 to adjust default weights.
" Value of 5 corresponds to plugin-defined default. Each increment above or below 5
" adjusts up or down by an plugin-defined, criterion-specific adjustment value.
" TODO: Probably encapsulate in a dict.
if !exists('g:sexp_aligncom_numgroups_weight')
    let g:sexp_aligncom_numgroups_weight = 5
endif

if !exists('g:sexp_aligncom_shift_weight')
    let g:sexp_aligncom_shift_weight = 5
endif

if !exists('g:sexp_aligncom_density_weight')
    let g:sexp_aligncom_density_weight = 5
endif

if !exists('g:sexp_aligncom_runtness_weight')
    let g:sexp_aligncom_runtness_weight = 5
endif

if !exists('g:sexp_aligncom_textwidth_weight')
    let g:sexp_aligncom_textwidth_weight = 5
endif

if !exists('g:sexp_aligncom_runt_thresh')
    let g:sexp_aligncom_runt_thresh = 5
endif

" TODO: Consider renaming this 'spacing' to avoid overloading 'margin'.
if !exists('g:sexp_aligncom_min_separation')
    let g:sexp_aligncom_min_separation = 2
endif

if !exists('g:sexp_aligncom_greedy_lookback')
    let g:sexp_aligncom_greedy_lookback = 4
endif

if !exists('g:sexp_aligncom_optlevel')
            \ || type(g:sexp_aligncom_optlevel) != type(0)
            \ || g:sexp_aligncom_optlevel > 2 || g:sexp_aligncom_optlevel < 0
    let g:sexp_aligncom_optlevel = 2
endif

if !exists('g:sexp_regput_bracket_is_target')
    let g:sexp_regput_bracket_is_target = 1
endif

if !exists('g:sexp_regput_bracket_is_child')
    let g:sexp_regput_bracket_is_child = 0
endif

if !exists('g:sexp_regput_allow_comment_append')
    let g:sexp_regput_allow_comment_append = 0
endif

if !exists('g:sexp_regput_untrimmed_is_linewise')
    let g:sexp_regput_untrimmed_is_linewise = 0
endif

if !exists('g:sexp_regput_linewise_forces_multiline')
    let g:sexp_regput_linewise_forces_multiline = 1
endif

if !exists('g:sexp_regput_ignore_list_shape')
    let g:sexp_regput_ignore_list_shape = 0
endif

if !exists('g:sexp_regput_fallback_source')
    let g:sexp_regput_fallback_source = 'slc'
endif

if !exists('g:sexp_regput_fallback_target')
    let g:sexp_regput_fallback_target = ''
endif

if !exists('g:sexp_regput_curpos')
    let g:sexp_regput_curpos = 0
endif

if !exists('g:sexp_regput_curpos_child')
    let g:sexp_regput_curpos_child = 0
endif

if !exists('g:sexp_regput_curpos_op')
    let g:sexp_regput_curpos_op = 2
endif

if !exists('g:sexp_regput_invalid_register_action')
    let g:sexp_regput_invalid_register_action = -1
endif

if !exists('g:sexp_regput_inhibit_regparse')
    let g:sexp_regput_inhibit_regparse = 0
endif

if !exists('g:sexp_regput_replace_expanded')
    let g:sexp_regput_replace_expanded = 0
endif

if !exists('g:sexp_regput_tele_motion')
    let g:sexp_regput_tele_motion = 2
else
    if g:sexp_regput_tele_motion && v:version < 801
        call sexp#warn#msg(
            \ "Warning: Replace operator's telescopic mode requires Vim version >= 8.1.")
    endif
endif

if !exists('g:sexp_regput_use_string_parser')
    let g:sexp_regput_use_string_parser = 0
endif

if !exists('g:sexp_regput_silence_notification')
    let g:sexp_regput_silence_notification = 0
endif

" Expert options
if !exists('g:sexp_inhibit_failsafe')
    let g:sexp_inhibit_failsafe = 0
endif

if !exists('g:sexp_mappings')
    let g:sexp_mappings = {}
endif

if !empty(g:sexp_filetypes)
    augroup sexp_filetypes
        autocmd!
        execute 'autocmd FileType ' . g:sexp_filetypes . ' call s:sexp_create_mappings()'
    augroup END
endif

" If it's available, use <Cmd> modifier at the head of command rhs.
" Rationale: The idiomatic (but now obsolete) `:<c-u>` has the undesirable side-effect of
" generating a 'CmdlineChanged' autocmd event for *every character* in the command line
" executed by the map. Beyond the obvious efficiency concerns, this is especially
" problematic for users of plugins like "smear-cursor", whose cursor animation logic
" assumes 'CmdlineChange' means the user is typing on the command line.
let s:have_cmd = has('nvim') || v:version >= 900
""" Functions {{{1

" Convert list of bools to flags dict provided to s:defplug.
" Rationale: Serializing the dict in the plug function arglist aids in readability.
" -- Args --
"   asexpr: create <expr> mapping
"           TODO: Consider renaming this "asoper" since it's currently meant exclusively
"           for use with operators.
"   repeat: set up repeat (if repeat plugin available)
"   nojump: inhibit set of '`
function! s:defplug_flags(asexpr, repeat, nojump)
    return {'asexpr': a:asexpr, 'repeat': a:repeat, 'nojump': a:nojump}
endfunction

" These commands invoke s:defplug() with arguments that will ensure creation of the
" desired <Plug> mappings, which in turn, invoke sexp#plug#wrapper() when the map is
" invoked.
" Note: Defoper* relies on <expr> maps to define sexp operators.
command! -nargs=+ -bang Defoper  call <SID>defplug(s:defplug_flags(1, !empty('<bang>'), 0), <f-args>)
command! -nargs=+ -bang DefoperN call <SID>defplug(s:defplug_flags(1, !empty('<bang>'), 1), <f-args>)
command! -nargs=+ -bang Defplug  call <SID>defplug(s:defplug_flags(0, !empty('<bang>'), 0), <f-args>)
command! -nargs=+ -bang DefplugN call <SID>defplug(s:defplug_flags(0, !empty('<bang>'), 1), <f-args>)

" Create a <Plug> mapping. The 'flags' dict provided by the Def* map influences behavior:
function! s:defplug(flags, mapmode, name, ...)
    let rhs = join(a:000)
    let asexpr = a:flags.asexpr
    let prefix = a:mapmode . (asexpr ? ' <expr>' : '')
        \ . ' <silent> <Plug>(' . a:name . ')'

    " Note: sexp#plug#wrapper() is called when map is invoked to handle both regular maps
    " and sexp operators.
    " Note: %s intentionally used to serialize the 'flags' dict.
    execute prefix . printf(
        \ '%s sexp#plug#wrapper(%s, "%s", "%s", "%s")%s',
        \ asexpr ? '' : (s:have_cmd ? ' <cmd>' : ' :<c-u>') . ' call',
        \ a:flags, a:mapmode, a:name, rhs,
        \ (!asexpr ? '<cr>' : ''))
endfunction

" Warn once-only (per buffer) for the specified map conflict (or ambiguity).
" -- Optional Args --
" a:1  ambiguous (not conflict)
function! s:map_conflict_warn_once(lhs, rhs1, rhs2, mode, ...)
    " Note: Unambiguously order the components of the key to ensure we don't warn twice
    " for the same conflict when s:sexp_create_mappings() is called twice.
    " Explanation: In the initial call, the second map overwrites the first, but then in
    " the second call, the first map will overwrite the second.
    let uniq_key = [a:mode, a:lhs] + sort([a:rhs1, a:rhs2]))
    call sexp#warn#msg(printf(
        \ "Mapping %s => %s %s with existing mapping to %s in mode %s",
        \ a:lhs, a:rhs2, (a:0 ? "is ambiguous" : "conflicts"), a:rhs1, a:mode),
        \ {'once': uniq_key})
endfunction

" Warn user if the input args represent a mapping that would conflict or be ambiguous with
" an existing map (either global or buffer).
" Return:
"   0  no problem
"   1  ambiguity
"   2  conflict
" Design Decision: Warn about only *buffer* conflicts/ambiguities, silently overwriting a
" global map with the same lhs.
" Rationale: Buffer maps are typically filetype-specific, and thus, should generally take
" precedence over global maps.
" TODO: Decide whether these checks belong in the plugin and remove this function if not.
" For now, the call to this function has been commented out due to the confusion the
" warnings caused during smart-paste beta testing. If it's kept at all, the call should
" probably be guarded by an expert option (disabled by default), which allows the user to
" enable and customize warnings. Such an option could be enabled while user is attempting
" to hash out a good set of keybindings.
function! s:check_for_map_conflicts(lhs, mode, plug)
    let rhs = '<Plug>(' . a:plug . ')'
    " Assumption: maparg() can handle distinct but equivalent forms of lhs (e.g.,
    " <LocalLeader> vs \, <C-...> vs <c-...>, etc...)
    " TODO: Could alternatively check only mappings created by this plugin, but
    " that would entail canonicalizing lhs and storing in dict of some sort.
    " Assumption: maparg() returns a buffer map before a global one, but in the absence of
    " a buffer map, will return a global one.
    " Caveat: The check against <plug>(...) ensures we don't warn about our own mapping if
    " this function is called multiple times.
    " Note: 'rhs' key won't exist if the rhs is a Lua callback (stored in 'callback').
    let m = maparg(a:lhs, a:mode, 0, 1)
    if !empty(m) && m.buffer && has_key(m, 'rhs') && m.rhs !=? '<nop>' && m.rhs !=? rhs
        " Warn before overwriting existing map.
        " TODO: Consider adding option for this.
        call s:map_conflict_warn_once(a:lhs, m.rhs, rhs, a:mode)
        return 2
    endif
    " Also check for map ambiguities.
    let m = mapcheck(a:lhs, a:mode)
    " Caveat: Don't warn if the ambiguous map appears to be this one, created on an
    " earlier call to sexp_create_mappings().
    " Note: Although it should be safe to compare full rhs (including <Plug>(...)
    " wrapper), Vim docs don't explicitly state that "<Plug>" will appear untranslated in
    " the rhs returned by mapcheck; thus, to be safe, just compare the plug name itself.
    if !empty(m) && m !~ '(' . a:plug . ')'
        " Pass optional 'ambiguous' flag to indicate not true conflict.
        call s:map_conflict_warn_once(a:lhs, m, rhs, a:mode, 1)
        return 1
    endif
    return 0
endfunction

" Bind <Plug> mappings in current buffer to values in g:sexp_mappings or
" s:sexp_mappings
" TODO: Consider moving more of this infrastructure into the plug autoload module.
function! s:sexp_create_mappings()
    if sexp#parse#in_buf()
        " Skip map creation in the special parse buffer.
        return
    endif
    let sexp_mappings = sexp#data#get_sexp_mappings()
    " Note: {s,g}entry stand for {s,g}:sexp_mappings entry, respectively.
    for [plug, sentry] in items(sexp_mappings)
        " Get corresponding user override if it exists.
        let gentry = get(g:sexp_mappings, plug, {})
        " Parse entry into a flat dict of modechar => lhs: e.g.,
        " {'nx': '\s', 'o': '\t'} => {'n': '\s', 'x': '\s', 'o': \t'}
        let [sm, _] = sexp#plug#parse_map_entry(plug, sentry, '')
        " Default map determines the valid keys.
        let valid_modes_arr = sort(keys(sm))
        let valid_modes_str = join(valid_modes_arr, '')
        " Now parse any user-defined override.
        " Note: We'll want gm to be empty in exception scenario.
        let gm = {}
        try
            let [gm, invalid_modes_str] =
                \ sexp#plug#parse_map_entry(plug, gentry, valid_modes_str)
            if !empty(invalid_modes_str)
                call sexp#warn#msg(printf("Ignoring unexpected modes in"
                    \ . " user map override: `%s'", invalid_modes_str),
                    \ {'once': [plug, gentry]})
            endif
        catch /sexp-error/
            " Note: Leave gm empty to ensure default is used.
            call sexp#warn#msg(printf("Ignoring invalid user map override: %s: %s",
                    \ string(gentry),
                    \ substitute(v:exception, '^sexp-error:\s*', '', '')),
                    \ {'once': [plug, gentry]})
        endtry
        " Loop over all modes which are valid for this command.
        " Design Decision: To simplify the merge, distinct modes within sm/gm are stored
        " as keys, rather than list elements; an implication of this (probably good) is
        " that user can't specify multiple LHS per mode.
        for mode in valid_modes_arr
            " Use mode-specific override if it exists, else default, which must exist.
            let lhs = get(gm, mode, get(sm, mode))
            if empty(lhs)
                " No lhs mapping for this one, so skip it.
                continue
            endif

            " TODO: Decide whether the plugin should warn, or simply override...
            "call s:check_for_map_conflicts(lhs, mode, plug)
            " Create the mapping.
            if plug !~ '^sexp_'
                " A builtin override, which needs to be "noremapped" to prevent triggering
                " a first-level sexp map.
                " Note: This special case is implemented to provide a convenient way for
                " user to create aliases to overridden builtins.
                execute mode . 'noremap <silent><buffer> ' . lhs . ' ' . plug
            else
                " A true plug mapping
                execute mode . 'map <silent><buffer> ' . lhs . ' <Plug>(' . plug . ')'
            endif
        endfor
    endfor

    " Insert-mode mappings
    if g:sexp_enable_insert_mode_mappings
        imap <silent><buffer> (    <Plug>(sexp_insert_opening_round)
        imap <silent><buffer> [    <Plug>(sexp_insert_opening_square)
        imap <silent><buffer> {    <Plug>(sexp_insert_opening_curly)
        imap <silent><buffer> )    <Plug>(sexp_insert_closing_round)
        imap <silent><buffer> ]    <Plug>(sexp_insert_closing_square)
        imap <silent><buffer> }    <Plug>(sexp_insert_closing_curly)
        imap <silent><buffer> "    <Plug>(sexp_insert_double_quote)
        imap <silent><buffer> <BS> <Plug>(sexp_insert_backspace)
    endif

    " Create persistent TextYankPost autocommand for smart-paste metadata collection.
    " This is kept separate from the operator-specific TextYankPost to avoid entanglement
    " with the already-validated operator mechanism.
    augroup SexpRegputSmartPaste
        autocmd! TextYankPost <buffer> call sexp#regput__TextYankPost()
    augroup END
endfunction

""" Text Object Selections {{{1

" Current list (compound FORM)
Defplug  xnoremap sexp_outer_list sexp#docount(v:count, 'sexp#select_current_list', 'v', 0, 1)
Defplug! onoremap sexp_outer_list sexp#docount(v:count, 'sexp#select_current_list', 'o', 0, 1)
Defplug  xnoremap sexp_inner_list sexp#docount(v:count, 'sexp#select_current_list', 'v', 1, 1)
Defplug! onoremap sexp_inner_list sexp#docount(v:count, 'sexp#select_current_list', 'o', 1, 1)

" Current top-level list (compound FORM)
Defplug  xnoremap sexp_outer_top_list sexp#select_current_top_list('v', 0)
Defplug! onoremap sexp_outer_top_list sexp#select_current_top_list('o', 0)
Defplug  xnoremap sexp_inner_top_list sexp#select_current_top_list('v', 1)
Defplug! onoremap sexp_inner_top_list sexp#select_current_top_list('o', 1)

" Current string
Defplug  xnoremap sexp_outer_string sexp#select_current_string('v', 0)
Defplug! onoremap sexp_outer_string sexp#select_current_string('o', 0)
Defplug  xnoremap sexp_inner_string sexp#select_current_string('v', 1)
Defplug! onoremap sexp_inner_string sexp#select_current_string('o', 1)

" Current element
Defplug  xnoremap sexp_outer_element sexp#select_current_element('v', 0, v:count)
Defplug! onoremap sexp_outer_element sexp#select_current_element('o', 0, v:count)
Defplug  xnoremap sexp_inner_element sexp#select_current_element('v', 1, v:count)
Defplug! onoremap sexp_inner_element sexp#select_current_element('o', 1, v:count)

Defplug  xnoremap sexp_outer_child_head sexp#select_child('v', v:count, 0, 0)
Defplug! onoremap sexp_outer_child_head sexp#select_child('o', v:count, 0, 0)
Defplug  xnoremap sexp_inner_child_head sexp#select_child('v', v:count, 0, 1)
Defplug! onoremap sexp_inner_child_head sexp#select_child('o', v:count, 0, 1)

Defplug  xnoremap sexp_outer_child_tail sexp#select_child('v', v:count, 1, 0)
Defplug! onoremap sexp_outer_child_tail sexp#select_child('o', v:count, 1, 0)
Defplug  xnoremap sexp_inner_child_tail sexp#select_child('v', v:count, 1, 1)
Defplug! onoremap sexp_inner_child_tail sexp#select_child('o', v:count, 1, 1)
""" Text Object Motions {{{1

" Nearest bracket
Defplug  nnoremap sexp_move_to_prev_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'n', 0)
DefplugN xnoremap sexp_move_to_prev_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'v', 0)
Defplug! onoremap sexp_move_to_prev_bracket sexp#move_to_nearest_bracket('o', 0)
Defplug  nnoremap sexp_move_to_next_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'n', 1)
DefplugN xnoremap sexp_move_to_next_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'v', 1)
Defplug! onoremap sexp_move_to_next_bracket sexp#move_to_nearest_bracket('o', 1)

" Adjacent element head
"
" Visual mappings must break out of visual mode in order to detect which end
" the user is using to adjust the selection.
DefplugN  nnoremap sexp_move_to_prev_element_head sexp#move_to_adjacent_element('n', v:count, 0, 0, 0)
DefplugN  xnoremap sexp_move_to_prev_element_head sexp#move_to_adjacent_element('v', v:count, 0, 0, 0)
DefplugN! onoremap sexp_move_to_prev_element_head sexp#move_to_adjacent_element('o', v:count, 0, 0, 0)
DefplugN  nnoremap sexp_move_to_next_element_head sexp#move_to_adjacent_element('n', v:count, 1, 0, 0)
DefplugN  xnoremap sexp_move_to_next_element_head sexp#move_to_adjacent_element('v', v:count, 1, 0, 0)
DefplugN! onoremap sexp_move_to_next_element_head sexp#move_to_adjacent_element('o', v:count, 1, 0, 0)

" Adjacent element tail
"
" Inclusive operator pending motions require a visual mode selection to
" include the last character of a line.
DefplugN  nnoremap sexp_move_to_prev_element_tail sexp#move_to_adjacent_element('n', v:count, 0, 1, 0)
DefplugN  xnoremap sexp_move_to_prev_element_tail sexp#move_to_adjacent_element('v', v:count, 0, 1, 0)
DefplugN! onoremap sexp_move_to_prev_element_tail sexp#move_to_adjacent_element('o', v:count, 0, 1, 0)
DefplugN  nnoremap sexp_move_to_next_element_tail sexp#move_to_adjacent_element('n', v:count, 1, 1, 0)
DefplugN  xnoremap sexp_move_to_next_element_tail sexp#move_to_adjacent_element('v', v:count, 1, 1, 0)
DefplugN! onoremap sexp_move_to_next_element_tail sexp#move_to_adjacent_element('o', v:count, 1, 1, 0)

" List flow commands
Defplug   nnoremap sexp_flow_to_prev_close sexp#list_flow('n', v:count, 0, 1)
DefplugN  xnoremap sexp_flow_to_prev_close sexp#list_flow('v', v:count, 0, 1)
Defplug   nnoremap sexp_flow_to_prev_open sexp#list_flow('n', v:count, 0, 0)
DefplugN  xnoremap sexp_flow_to_prev_open sexp#list_flow('v', v:count, 0, 0)
Defplug   nnoremap sexp_flow_to_next_open sexp#list_flow('n', v:count, 1, 0)
DefplugN  xnoremap sexp_flow_to_next_open sexp#list_flow('v', v:count, 1, 0)
Defplug   nnoremap sexp_flow_to_next_close sexp#list_flow('n', v:count, 1, 1)
DefplugN  xnoremap sexp_flow_to_next_close sexp#list_flow('v', v:count, 1, 1)

" Leaf flow commands
DefplugN  nnoremap sexp_flow_to_prev_leaf_head sexp#leaf_flow('n', v:count, 0, 0)
DefplugN  xnoremap sexp_flow_to_prev_leaf_head sexp#leaf_flow('v', v:count, 0, 0)
DefplugN  nnoremap sexp_flow_to_next_leaf_head sexp#leaf_flow('n', v:count, 1, 0)
DefplugN  xnoremap sexp_flow_to_next_leaf_head sexp#leaf_flow('v', v:count, 1, 0)
DefplugN  nnoremap sexp_flow_to_prev_leaf_tail sexp#leaf_flow('n', v:count, 0, 1)
DefplugN  xnoremap sexp_flow_to_prev_leaf_tail sexp#leaf_flow('v', v:count, 0, 1)
DefplugN  nnoremap sexp_flow_to_next_leaf_tail sexp#leaf_flow('n', v:count, 1, 1)
DefplugN  xnoremap sexp_flow_to_next_leaf_tail sexp#leaf_flow('v', v:count, 1, 1)

" Adjacent top element
Defplug  nnoremap sexp_move_to_prev_top_element sexp#move_to_adjacent_element('n', v:count, 0, 0, 1)
DefplugN xnoremap sexp_move_to_prev_top_element sexp#move_to_adjacent_element('v', v:count, 0, 0, 1)
Defplug! onoremap sexp_move_to_prev_top_element sexp#move_to_adjacent_element('o', v:count, 0, 0, 1)
Defplug  nnoremap sexp_move_to_next_top_element sexp#move_to_adjacent_element('n', v:count, 1, 0, 1)
DefplugN xnoremap sexp_move_to_next_top_element sexp#move_to_adjacent_element('v', v:count, 1, 0, 1)
Defplug! onoremap sexp_move_to_next_top_element sexp#move_to_adjacent_element('o', v:count, 1, 0, 1)

" Adjacent element selection
"
" Unlike the other directional motions, calling this from normal mode places
" us in visual mode, with the adjacent element as our selection.
Defplug  nnoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'n', 0)
Defplug  xnoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'v', 0)
Defplug! onoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'o', 0)
Defplug  nnoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'n', 1)
Defplug  xnoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'v', 1)
Defplug! onoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'o', 1)

""" Commands {{{1

" Indent S-Expression
" Question: Would 'repeat' make any sense for visual variants?
" Answer: I think it would, but for some reason, vim-sexp has historically not
" supported repeat for visual operations, so I guess I'll be consistent for
" now.
Defplug! nnoremap sexp_indent                sexp#indent('n', 0, v:count, -1)
Defplug  xnoremap sexp_indent                sexp#indent('x', 0, v:count, -1)
Defplug! nnoremap sexp_indent_top            sexp#indent('n', 1, v:count, -1)
Defplug! nnoremap sexp_indent_and_clean      sexp#indent('n', 0, v:count, 1)
Defplug  xnoremap sexp_indent_and_clean      sexp#indent('x', 0, v:count, 1)
Defplug! nnoremap sexp_indent_and_clean_top  sexp#indent('n', 1, v:count, 1)

" TODO: Should these have dedicated default mappings, or just default to having it done by
" indent and let user configure explicit maps if desired?
Defplug! nnoremap sexp_align_comments        sexp#align_comments('n', 0, v:count)
Defplug  xnoremap sexp_align_comments        sexp#align_comments('x', 0, v:count)
Defplug! nnoremap sexp_align_comments_top    sexp#align_comments('n', 1, v:count)

" Wrap list
Defplug! nnoremap sexp_round_head_wrap_list  sexp#wrap('f', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_round_head_wrap_list  sexp#wrap('v', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_round_tail_wrap_list  sexp#wrap('f', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_round_tail_wrap_list  sexp#wrap('v', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_square_head_wrap_list sexp#wrap('f', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_square_head_wrap_list sexp#wrap('v', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_square_tail_wrap_list sexp#wrap('f', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_square_tail_wrap_list sexp#wrap('v', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_curly_head_wrap_list  sexp#wrap('f', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_curly_head_wrap_list  sexp#wrap('v', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_curly_tail_wrap_list  sexp#wrap('f', '{', '}', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_curly_tail_wrap_list  sexp#wrap('v', '{', '}', 1, g:sexp_insert_after_wrap)

" Wrap element
Defplug! nnoremap sexp_round_head_wrap_element  sexp#wrap('e', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_round_head_wrap_element  sexp#wrap('v', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_round_tail_wrap_element  sexp#wrap('e', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_round_tail_wrap_element  sexp#wrap('v', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_square_head_wrap_element sexp#wrap('e', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_square_head_wrap_element sexp#wrap('v', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_square_tail_wrap_element sexp#wrap('e', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_square_tail_wrap_element sexp#wrap('v', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_curly_head_wrap_element  sexp#wrap('e', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_curly_head_wrap_element  sexp#wrap('v', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_curly_tail_wrap_element  sexp#wrap('e', '{', '}', 1, g:sexp_insert_after_wrap)
Defplug  xnoremap sexp_curly_tail_wrap_element  sexp#wrap('v', '{', '}', 1, g:sexp_insert_after_wrap)

" Insert at list terminal
Defplug! nnoremap sexp_insert_at_list_head sexp#insert_at_list_terminal(0)
Defplug! nnoremap sexp_insert_at_list_tail sexp#insert_at_list_terminal(1)

" Raise list
Defplug! nnoremap sexp_raise_list    sexp#docount_stateful(v:count, 'sexp#raise', 'n', 'sexp#select_current_list', 'n', 0, 0)
Defplug  xnoremap sexp_raise_list    sexp#docount_stateful(v:count, 'sexp#raise', 'v', '')
Defplug! nnoremap sexp_raise_element sexp#docount_stateful(v:count, 'sexp#raise', 'n', 'sexp#select_current_element', 'n', 1)
Defplug  xnoremap sexp_raise_element sexp#docount_stateful(v:count, 'sexp#raise', 'v', '')

" Convolute
" Note: convolute takes pains to preserve cursor position: hence, 'nojump'.
DefplugN! nnoremap sexp_convolute sexp#convolute(v:count, 'n')

" Clone list
DefplugN  nnoremap sexp_clone_list    sexp#clone('n', v:count, 1, 0, '')
DefplugN  xnoremap sexp_clone_list    sexp#clone('v', v:count, 1, 0, '')
DefplugN  nnoremap sexp_clone_list_sl sexp#clone('n', v:count, 1, 0, 's')
DefplugN  xnoremap sexp_clone_list_sl sexp#clone('v', v:count, 1, 0, 's')
DefplugN  nnoremap sexp_clone_list_ml sexp#clone('n', v:count, 1, 0, 'm')
DefplugN  xnoremap sexp_clone_list_ml sexp#clone('v', v:count, 1, 0, 'm')

" Clone element
DefplugN  nnoremap sexp_clone_element    sexp#clone('n', v:count, 0, 0, '')
DefplugN  xnoremap sexp_clone_element    sexp#clone('v', v:count, 0, 0, '')
DefplugN  nnoremap sexp_clone_element_sl sexp#clone('n', v:count, 0, 0, 's')
DefplugN  xnoremap sexp_clone_element_sl sexp#clone('v', v:count, 0, 0, 's')
DefplugN  nnoremap sexp_clone_element_ml sexp#clone('n', v:count, 0, 0, 'm')
DefplugN  xnoremap sexp_clone_element_ml sexp#clone('v', v:count, 0, 0, 'm')

" Splice list
Defplug! nnoremap sexp_splice_list sexp#splice_list(v:count)

" Swap list
Defplug! nnoremap sexp_swap_list_backward sexp#docount(v:count, 'sexp#swap_element', 'n', 0, 1)
DefplugN xnoremap sexp_swap_list_backward sexp#docount(v:count, 'sexp#swap_element', 'v', 0, 1)
Defplug! nnoremap sexp_swap_list_forward  sexp#docount(v:count, 'sexp#swap_element', 'n', 1, 1)
DefplugN xnoremap sexp_swap_list_forward  sexp#docount(v:count, 'sexp#swap_element', 'v', 1, 1)

" Swap element
Defplug! nnoremap sexp_swap_element_backward sexp#docount(v:count, 'sexp#swap_element', 'n', 0, 0)
DefplugN xnoremap sexp_swap_element_backward sexp#docount(v:count, 'sexp#swap_element', 'v', 0, 0)
Defplug! nnoremap sexp_swap_element_forward  sexp#docount(v:count, 'sexp#swap_element', 'n', 1, 0)
DefplugN xnoremap sexp_swap_element_forward  sexp#docount(v:count, 'sexp#swap_element', 'v', 1, 0)

" Emit/capture element
Defplug! nnoremap sexp_emit_head_element    sexp#docount_stateful(v:count, 'sexp#stackop', 'n', 0, 0)
Defplug  xnoremap sexp_emit_head_element    sexp#docount_stateful(v:count, 'sexp#stackop', 'v', 0, 0)
Defplug! nnoremap sexp_emit_tail_element    sexp#docount_stateful(v:count, 'sexp#stackop', 'n', 1, 0)
Defplug  xnoremap sexp_emit_tail_element    sexp#docount_stateful(v:count, 'sexp#stackop', 'v', 1, 0)
Defplug! nnoremap sexp_capture_prev_element sexp#docount_stateful(v:count, 'sexp#stackop', 'n', 0, 1)
Defplug  xnoremap sexp_capture_prev_element sexp#docount_stateful(v:count, 'sexp#stackop', 'v', 0, 1)
Defplug! nnoremap sexp_capture_next_element sexp#docount_stateful(v:count, 'sexp#stackop', 'n', 1, 1)
Defplug  xnoremap sexp_capture_next_element sexp#docount_stateful(v:count, 'sexp#stackop', 'v', 1, 1)

" Put register before/after
DefplugN! nnoremap sexp_put_before  sexp#put(v:count, 0)
DefplugN! nnoremap sexp_put_after   sexp#put(v:count, 1)
" Put register before/after (force smart paste)
DefplugN! nnoremap sexp_put_before_smart  sexp#put(v:count, 0)
DefplugN! nnoremap sexp_put_after_smart   sexp#put(v:count, 1)
" Replace operator
DefoperN! nnoremap sexp_replace_op   sexp#regput_op(1, 0)
DefoperN! nnoremap sexp_replace_op_P sexp#regput_op(1, 1)
" Replace selection with register
DefplugN! xnoremap sexp_replace   sexp#replace('v', v:count, 0)
DefplugN! xnoremap sexp_replace_P sexp#replace('v', v:count, 1)
" Replace selection with register (force smart paste)
DefplugN! xnoremap sexp_replace_smart   sexp#replace('v', v:count, 0)
DefplugN! xnoremap sexp_replace_P_smart sexp#replace('v', v:count, 1)
" Replace element under cursor with register
" TODO: Decide whether to map this by default...
DefplugN! nnoremap sexp_replace   sexp#replace('n', v:count, 0)
DefplugN! nnoremap sexp_replace_P sexp#replace('n', v:count, 1)
" Replace element under cursor with register (force smart paste)
DefplugN! nnoremap sexp_replace_smart   sexp#replace('n', v:count, 0)
DefplugN! nnoremap sexp_replace_P_smart sexp#replace('n', v:count, 1)
" Put before/after operators
DefoperN! nnoremap sexp_put_before_op sexp#regput_op(0, 1)
DefoperN! nnoremap sexp_put_after_op  sexp#regput_op(0, 0)
" Put register into list
DefplugN! nnoremap sexp_put_at_head sexp#put_child(v:count, 0)
DefplugN! nnoremap sexp_put_at_tail sexp#put_child(v:count, 1)

""" Insert mode mappings {{{1

" This wrapper was created to ensure that even insert-mode maps use {pre,post}_op().
" Rationale: For now, it simply ensures that warnings from insert-mode maps will use the
" deferred msg queue, but eventually, may also be used to allow for a more efficient
" dispatch mechanism: e.g., one that relies on once-only decision upon command invocation,
" as opposed to repeated tests at the point of each ts/legacy function call.
function! s:insert_map_wrapper(plug, expr)
    try
        call sexp#pre_op("i", a:plug)
        exe 'let ret =' a:expr
    finally
        call sexp#post_op("i", a:plug)
    endtry
    return ret
endfunction

" Note: The expression passed to this function should typically be a single argument, but
" just in case, we use variadic args and join.
function! s:defplug_ins(plug, ...)
    let expr = join(a:000, ' ')
    exe 'inoremap <silent><expr> <Plug>(' . a:plug 
        \ . ') <SID>insert_map_wrapper("' . a:plug . '", "' . escape(expr, '"') . '")'
endfunction

" Command to simplify map definition
command! -nargs=+       DefplugI  call <SID>defplug_ins(<f-args>)

" Insert opening delimiter
DefplugI sexp_insert_opening_round sexp#opening_insertion('(')
DefplugI sexp_insert_opening_square sexp#opening_insertion('[')
DefplugI sexp_insert_opening_curly  sexp#opening_insertion('{')

" Insert closing delimiter
DefplugI sexp_insert_closing_round  sexp#closing_insertion(')')
DefplugI sexp_insert_closing_square sexp#closing_insertion(']')
DefplugI sexp_insert_closing_curly  sexp#closing_insertion('}')

" Insert double quote
DefplugI sexp_insert_double_quote sexp#quote_insertion('"')

" Delete paired delimiters
DefplugI sexp_insert_backspace sexp#backspace_insertion()

""" Cleanup {{{1

delcommand DefplugN
delcommand Defplug
delcommand DefoperN
delcommand Defoper

" vim:ts=4:sw=4:et:tw=90
