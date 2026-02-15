
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

" TODO: Consider encapsulating related options in a dict.
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

if !exists('g:sexp_regput_override_builtins')
    let g:sexp_regput_override_builtins = 0
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
    let g:sexp_regput_tele_motion = 0
else
    if g:sexp_regput_tele_motion && v:version < 801
        call sexp#warn#msg(
            \ "Warning: Replace operator's telescopic mode requires Vim version >= 8.1.")
    endif
endif

" Expert options
if !exists('g:sexp_inhibit_failsafe')
    let g:sexp_inhibit_failsafe = 0
endif

if !exists('g:sexp_mappings')
    let g:sexp_mappings = {}
endif

" Keymap Presets
" Motivation: Give user an easy way to enable a (usually feature-specific) set of mappings
" we don't dare create without permission (usually because they override builtin maps). A
" keymap preset is selected by a user option: e.g.,
"   let g:sexp_regput_override_builtins = 1
" From all such options, logic in sexp_create_mappings() determines a list of enabled
" presets, each of which causes a dict by the name of s:sexp_mapping_preset__{feat-name},
" with the same format as s:sexp_mappings, to be searched for overrides.
" Constraint: A given plug rhs should never belong to multiple presets, as this would give
" rise to ambiguity when both presets were enabled.
" Note: Currently, a preset entry completely *replaces* the corresponding plugin-defined
" command entry.
" TODO: Decide whether it should be merged with it instead...
" TODO: Decide whether the normal mode replace commands should have default mappings.
let s:sexp_mapping_preset__regput = {
    \ 'sexp_put_before':                   {'n': 'P'},
    \ 'sexp_put_after':                    {'n': 'p'},
    \ 'sexp_replace':                      {'x': 'p', 'n': '<LocalLeader>p'},
    \ 'sexp_replace_P':                    {'x': 'P', 'n': '<LocalLeader>P'},
    \ 'sexp_put_at_head':                  {'n': '[p'},
    \ 'sexp_put_at_tail':                  {'n': ']p'},
\ }

let s:sexp_mappings = {
    \ 'sexp_outer_list':                   {'xo': 'af'},
    \ 'sexp_inner_list':                   {'xo': 'if'},
    \ 'sexp_outer_top_list':               {'xo': 'aF'},
    \ 'sexp_inner_top_list':               {'xo': 'iF'},
    \ 'sexp_outer_string':                 {'xo': 'as'},
    \ 'sexp_inner_string':                 {'xo': 'is'},
    \ 'sexp_outer_element':                {'xo': 'ae'},
    \ 'sexp_inner_element':                {'xo': 'ie'},
    \ 'sexp_outer_child_tail':             {'xo': 'aC'},
    \ 'sexp_outer_child_head':             {'xo': 'ac'},
    \ 'sexp_inner_child_tail':             {'xo': 'iC'},
    \ 'sexp_inner_child_head':             {'xo': 'ic'},
    \ 'sexp_move_to_prev_bracket':         {'nxo': '('},
    \ 'sexp_move_to_next_bracket':         {'nxo': ')'},
    \ 'sexp_move_to_prev_element_head':    {'nxo': '<M-b>'},
    \ 'sexp_move_to_next_element_head':    {'nxo': '<M-w>'},
    \ 'sexp_move_to_prev_element_tail':    {'nxo': 'g<M-e>'},
    \ 'sexp_move_to_next_element_tail':    {'nxo': '<M-e>'},
    \ 'sexp_flow_to_prev_close':           {'nx': '<M-[>'},
    \ 'sexp_flow_to_next_open':            {'nx': '<M-]>'},
    \ 'sexp_flow_to_prev_open':            {'nx': '<M-{>'},
    \ 'sexp_flow_to_next_close':           {'nx': '<M-}>'},
    \ 'sexp_flow_to_prev_leaf_head':       {'nx': '<M-S-b>'},
    \ 'sexp_flow_to_next_leaf_head':       {'nx': '<M-S-w>'},
    \ 'sexp_flow_to_prev_leaf_tail':       {'nx': '<M-S-g>'},
    \ 'sexp_flow_to_next_leaf_tail':       {'nx': '<M-S-e>'},
    \ 'sexp_move_to_prev_top_element':     {'nxo': '[['},
    \ 'sexp_move_to_next_top_element':     {'nxo': ']]'},
    \ 'sexp_select_prev_element':          {'nxo': '[e'},
    \ 'sexp_select_next_element':          {'nxo': ']e'},
    \ 'sexp_indent':                       {'n': '==', 'x': '='},
    \ 'sexp_indent_top':                   {'n': '=-'},
    \ 'sexp_indent_and_clean':             {'n': '<M-=>'},
    \ 'sexp_indent_and_clean_top':         {'n': '<M-->'},
    \ 'sexp_align_comments':               {'n': '<LocalLeader>a'},
    \ 'sexp_align_comments_top':           {'n': '<LocalLeader>A'},
    \ 'sexp_round_head_wrap_list':         {'nx': '<LocalLeader>i'},
    \ 'sexp_round_tail_wrap_list':         {'nx': '<LocalLeader>I'},
    \ 'sexp_square_head_wrap_list':        {'nx': '<LocalLeader>['},
    \ 'sexp_square_tail_wrap_list':        {'nx': '<LocalLeader>]'},
    \ 'sexp_curly_head_wrap_list':         {'nx': '<LocalLeader>{'},
    \ 'sexp_curly_tail_wrap_list':         {'nx': '<LocalLeader>}'},
    \ 'sexp_round_head_wrap_element':      {'nx': '<LocalLeader>w'},
    \ 'sexp_round_tail_wrap_element':      {'nx': '<LocalLeader>W'},
    \ 'sexp_square_head_wrap_element':     {'nx': '<LocalLeader>e['},
    \ 'sexp_square_tail_wrap_element':     {'nx': '<LocalLeader>e]'},
    \ 'sexp_curly_head_wrap_element':      {'nx': '<LocalLeader>e{'},
    \ 'sexp_curly_tail_wrap_element':      {'nx': '<LocalLeader>e}'},
    \ 'sexp_insert_at_list_head':          {'n': '<LocalLeader>h'},
    \ 'sexp_insert_at_list_tail':          {'n': '<LocalLeader>l'},
    \ 'sexp_splice_list':                  {'n': '<LocalLeader>@'},
    \ 'sexp_convolute':                    {'n': '<LocalLeader>?'},
    \ 'sexp_clone_list':                   {'nx': '<LocalLeader>c'},
    \ 'sexp_clone_list_sl':                {'nx': ''},
    \ 'sexp_clone_list_ml':                {'nx': ''},
    \ 'sexp_clone_element':                {'nx': '<LocalLeader>C'},
    \ 'sexp_clone_element_sl':             {'nx': ''},
    \ 'sexp_clone_element_ml':             {'nx': ''},
    \ 'sexp_raise_list':                   {'nx': '<LocalLeader>o'},
    \ 'sexp_raise_element':                {'nx': '<LocalLeader>O'},
    \ 'sexp_swap_list_backward':           {'nx': '<M-k>'},
    \ 'sexp_swap_list_forward':            {'nx': '<M-j>'},
    \ 'sexp_swap_element_backward':        {'nx': '<M-h>'},
    \ 'sexp_swap_element_forward':         {'nx': '<M-l>'},
    \ 'sexp_emit_head_element':            {'nx': '<M-S-j>'},
    \ 'sexp_emit_tail_element':            {'nx': '<M-S-k>'},
    \ 'sexp_capture_prev_element':         {'nx': '<M-S-h>'},
    \ 'sexp_capture_next_element':         {'nx': '<M-S-l>'},
    \ 'sexp_put_before':                   {'n': '<LocalLeader>P'},
    \ 'sexp_put_after':                    {'n': '<LocalLeader>p'},
    \ 'sexp_replace_op':                   {'n': '<M-p>'},
    \ 'sexp_replace_op_P':                 {'n': '<M-P>'},
    \ 'sexp_replace':                      {'x': '<LocalLeader>p', 'n': '<LocalLeader><LocalLeader>p'},
    \ 'sexp_replace_P':                    {'x': '<LocalLeader>P', 'n': '<LocalLeader><LocalLeader>P'},
    \ 'sexp_put_before_op':                {'n': '<p'},
    \ 'sexp_put_after_op':                 {'n': '>p'},
    \ 'sexp_put_at_head':                  {'n': '<LocalLeader><p'},
    \ 'sexp_put_at_tail':                  {'n': '<LocalLeader>>p'},
    \ }

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
        \ '%s sexp#plug#wrapper(%s, "%s", "%s", v:count, "%s")%s',
        \ asexpr ? '' : (s:have_cmd ? ' <cmd>' : ' :<c-u>') . ' call',
        \ a:flags, a:mapmode, a:name, rhs,
        \ (!asexpr ? '<cr>' : ''))
endfunction

" Display warning with requested highlighting.
function! s:warn(msg, ...)
    let hl = a:0 ? a:1 : 'WarningMsg'
    try
        exe 'echohl' hl
        echomsg a:msg
    finally
        echohl None
    endtry
endfunction

" Simplify warning displays for mappings.
" -- Args --
"   plug: command name specified as the ... in <plug>(...)
"   msg:  warning msg
function! s:mapwarn(plug, msg)
    call s:warn(printf("g:sexp_mappings['%s']: %s", a:plug, a:msg))
endfunction

" Warn once-only (per buffer) for the specified map conflict (or ambiguity).
" -- Optional Args --
" a:1  ambiguous (not conflict)
function! s:map_conflict_warn_once(lhs, rhs1, rhs2, mode, ...)
    " Note: Unambiguously order the components of the key to ensure we don't warn twice
    " for the same conflict when s:sexp_create_mappings() is called twice.
    " Explanation: In the initial call, the second map overwrites the first, but then in
    " the second call, the first map will overwrite the second.
    let s = call(function('sexp#warn#join_hashable'),
                \ [a:mode, a:lhs] + sort([a:rhs1, a:rhs2]))
    call sexp#warn#msg_once(s, printf(
        \ "Mapping %s => %s %s with existing mapping to %s in mode %s",
        \ a:lhs, a:rhs2, (a:0 ? "is ambiguous" : "conflicts"), a:rhs1, a:mode))
endfunction

" Convert a single value in the sexp_mappings dict to a denormalized form conducive to use
" in map creation.
" Args:
"   plug:         command name (i.e., the ... in <Plug>(...))
"   entry:        lhs specification in one of the following forms:
"   entry:        '<lhs>'
"                 | {'<modes1>': '<lhs1>', ..., '<modesN>': '<lhsN>'}
"   valid_modes:  string of chars in [nxo] constraining the modes for this command
"                 default: 'nox'
"                 override: subset of modes defined by default
function! s:parse_map_entry(plug, entry, valid_modes)
    let maps = {}
    let valid_modes = empty(a:valid_modes) ? 'novx' : a:valid_modes
    for [modes, lhs] in items(a:entry)
        " Convert v to x: e.g., 'nvo' => 'nxo'
        let modes = substitute(modes, 'v', 'x', 'g')
        " Collapse multiple occurrences of same mode: e.g., {'x': ..., 'xo': ...}.
        let modes = substitute(
            \ join(sort(split(modes, '\zs')), ''), '\v(.)\1+', '\1', 'g')
        " Loop over sorted/uniquified mode chars.
        for mode in split(modes, '\zs')
            " TODO: Distinguish between defaults and user here?
            if mode !~ '[' . valid_modes . ']'
                call s:mapwarn(a:plug, printf("Unrecognized mode %s", mode))
                continue
            endif
            " We have a valid mode, but has it already been specified for this plug?
            if has_key(maps, mode)
                " TODO: Consider just ignoring malformed entry altogether since this is
                " effectively UB without ordered keys.
                call s:mapwarn(a:plug, printf(
                    \ "Conflicting lhs specifications for mode %s: old=%s new=%s",
                    \ mode, maps[mode], lhs))
                continue
            endif
            " We have a valid mode that hasn't appeared in another key.
            let maps[mode] = lhs
        endfor
    endfor
    return maps
endfunction

" Return preset mapping override for specified plug, else {}.
" See note on Keymap Presets.
function! s:check_for_mapping_preset(plug)
    " TODO: Add more preset groups as necessary.
    let feats = {'regput': g:sexp_regput_override_builtins}
    for [feat, enable] in items(feats)
        if enable
            " Get preset dict whose format is same as s:sexp_mappings[].
            let o = s:sexp_mapping_preset__{feat}
            " Does this preset group define an override for this plug?
            if has_key(o, a:plug)
                " Replace the default with the enabled override.
                " Note: A plug should never be represented in 2 distinct feature sets.
                return o[a:plug]
            endif
        endif
    endfor
    return {}
endfunction

" Warn user if the input args represent a mapping that would conflict or be ambiguous with
" an existing map (either global or buffer).
" Return:
"   0  no problem
"   1  ambiguity
"   2  conflict
function! s:check_for_map_conflicts(lhs, mode, plug)
    let rhs = '<Plug>(' . a:plug . ')'
    " Assumption: maparg() can handle distinct but equivalent forms of lhs (e.g.,
    " <LocalLeader> vs \, <C-...> vs <c-...>, etc...)
    " TODO: Could alternatively check only mappings created by this plugin, but
    " that would entail canonicalizing lhs and storing in dict of some sort.
    " Assumption: maparg returns a buffer map before a global one, but in the absence of a
    " buffer map, will return a global one.
    " Design Decision: Warn about both buffer and global map conflicts/ambiguities.
    " Caveat: The check against <plug>(...) ensures we don't warn about our own mapping if
    " this function is called multiple times.
    " Note: 'rhs' key won't exist if the rhs is a Lua callback (stored in 'callback').
    let m = maparg(a:lhs, a:mode, 0, 1)
    if !empty(m) && has_key(m, 'rhs') && m.rhs !=? '<nop>'
        \ && m.rhs !=? rhs
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
    call sexp#feat#create_notifications()
    " Note: {s,g}entry stand for {s,g}:sexp_mappings entry, respectively.
    for [plug, sentry] in items(s:sexp_mappings)
        " Check for preset override.
        let override = s:check_for_mapping_preset(plug)
        if !empty(override)
            let sentry = override
        endif
        " Get corresponding user override if it exists.
        let gentry = get(g:sexp_mappings, plug, {})
        " Parse entry into a flat dict of modechar => lhs: e.g.,
        " {'nx': '\s', 'o': '\t'} => {'n': '\s', 'x': '\s', 'o': \t'}
        let sm = s:parse_map_entry(plug, sentry, '')
        " Default map determines the valid keys.
        let valid_modes_arr = sort(keys(sm))
        let valid_modes_str = join(valid_modes_arr, '')
        " Now parse any user-defined override
        let gm = {}
        if type(gentry) == type({})
            " Empty dict indicates no user-override.
            if !empty(gentry)
                let gm = s:parse_map_entry(plug, gentry, valid_modes_str)
            endif
        elseif type(gentry) == type('')
            " Create dict that uses specified lhs for all valid modes.
            for mode in valid_modes_arr
                let gm[mode] = gentry
            endfor
        else
            " Note: Leave gm empty to ensure default is used.
            call s:mapwarn(sprintf(
                \ "Invalid format: must be string or dict."
                \ . " (:help g:sexp_mappings)", string(gentry)))
        endif
        " Loop over all modes which are valid for this command.
        for mode in valid_modes_arr
            " Use mode-specific override if it exists, else default, which must exist.
            let lhs = get(gm, mode, get(sm, mode))
            if empty(lhs)
                " No lhs mapping for this one, so skip it.
                continue
            endif
            call s:check_for_map_conflicts(lhs, mode, plug)
            " Create the mapping.
            execute mode . 'map <silent><buffer> ' . lhs . ' <Plug>(' . plug . ')'
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
" Replace operator
DefoperN! nnoremap sexp_replace_op   sexp#regput_op(1, 0)
DefoperN! nnoremap sexp_replace_op_P sexp#regput_op(1, 1)
" Replace selection with register
DefplugN! xnoremap sexp_replace   sexp#replace('v', v:count, 0)
DefplugN! xnoremap sexp_replace_P sexp#replace('v', v:count, 1)
" Replace element under cursor with register
" TODO: Decide whether to map this by default...
DefplugN! nnoremap sexp_replace   sexp#replace('n', v:count, 0)
DefplugN! nnoremap sexp_replace_P sexp#replace('n', v:count, 1)
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
