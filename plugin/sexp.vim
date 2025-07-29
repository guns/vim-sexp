
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

""" Global State {{{1

if !exists('g:sexp_filetypes')
    let g:sexp_filetypes = 'clojure,scheme,lisp,timl'
endif

if !exists('g:sexp_enable_insert_mode_mappings')
    let g:sexp_enable_insert_mode_mappings = 1
endif

if !exists('g:sexp_insert_after_wrap')
    let g:sexp_insert_after_wrap = 1
endif

if !exists('g:sexp_indent_does_clean')
    let g:sexp_indent_does_clean = 1
endif

if !exists('g:sexp_clone_does_indent')
    let g:sexp_clone_does_indent = 1
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

if !exists('g:sexp_cleanup_lineshift_limit')
    let g:sexp_cleanup_lineshift_limit = 2
endif

if !exists('g:sexp_cleanup_colshift')
    let g:sexp_cleanup_colshift = 4
endif

if !exists('g:sexp_cleanup_colshift_slope')
    let g:sexp_cleanup_colshift_slope = 8
endif

" TODO: Consider encapsulating related options in a dict.
if !exists('g:sexp_align_eolc')
    let g:sexp_align_eolc = 1
endif

if !exists('g:sexp_align_eolc_maxshift')
    let g:sexp_align_eolc_maxshift = 40
endif

if !exists('g:sexp_align_eolc_maxgap')
    let g:sexp_align_eolc_maxgap = 0
endif

if !exists('g:sexp_align_eolc_break_at_linecom')
    let g:sexp_align_eolc_break_at_linecom = 0
endif

" Set these weights to values between 0 and 10 to adjust default weights.
" Value of 5 corresponds to plugin-defined default. Each increment above or below 5
" adjusts up or down by an plugin-defined, criterion-specific adjustment value.
" TODO: Probably encapsulate in a dict.
if !exists('g:sexp_align_eolc_ngrps_weight')
    let g:sexp_align_eolc_ngrps_weight = 5
endif

if !exists('g:sexp_align_eolc_shift_weight')
    let g:sexp_align_eolc_shift_weight = 5
endif

if !exists('g:sexp_align_eolc_density_weight')
    let g:sexp_align_eolc_density_weight = 5
endif

if !exists('g:sexp_align_eolc_runt_weight')
    let g:sexp_align_eolc_runt_weight = 5
endif

if !exists('g:sexp_align_eolc_runt_thresh')
    let g:sexp_align_eolc_runt_thresh = 5
endif

if !exists('g:sexp_align_eolc_preferred_spaces')
    let g:sexp_align_eolc_preferred_spaces = 2
endif

if !exists('g:sexp_align_eolc_allow_split_group')
    let g:sexp_align_eolc_allow_split_group = 1
endif

if !exists('g:sexp_align_eolc_split_group_maxgap')
    let g:sexp_align_eolc_split_group_maxgap = 3
endif

if !exists('g:sexp_align_eolc_greedy_lookback')
    let g:sexp_align_eolc_greedy_lookback = 4
endif

" TODO: Better names...
if !exists('g:sexp_align_eolc_optlvl_fallback1')
    let g:sexp_align_eolc_optlvl_fallback1 = 0
endif

if !exists('g:sexp_align_eolc_optlvl_fallback2')
    let g:sexp_align_eolc_optlvl_fallback2 = 0
endif

if !exists('g:sexp_mappings')
    let g:sexp_mappings = {}
endif

let s:sexp_mappings = {
    \ 'sexp_outer_list':                'af',
    \ 'sexp_inner_list':                'if',
    \ 'sexp_outer_top_list':            'aF',
    \ 'sexp_inner_top_list':            'iF',
    \ 'sexp_outer_string':              'as',
    \ 'sexp_inner_string':              'is',
    \ 'sexp_outer_element':             'ae',
    \ 'sexp_inner_element':             'ie',
    \ 'sexp_outer_child_tail':          'aC',
    \ 'sexp_outer_child_head':          'ac',
    \ 'sexp_inner_child_tail':          'iC',
    \ 'sexp_inner_child_head':          'ic',
    \ 'sexp_move_to_prev_bracket':      '(',
    \ 'sexp_move_to_next_bracket':      ')',
    \ 'sexp_move_to_prev_element_head': '<M-b>',
    \ 'sexp_move_to_next_element_head': '<M-w>',
    \ 'sexp_move_to_prev_element_tail': 'g<M-e>',
    \ 'sexp_move_to_next_element_tail': '<M-e>',
    \ 'sexp_flow_to_prev_close':        '<M-[>',
    \ 'sexp_flow_to_next_open':         '<M-]>',
    \ 'sexp_flow_to_prev_open':         '<M-{>',
    \ 'sexp_flow_to_next_close':        '<M-}>',
    \ 'sexp_flow_to_prev_leaf_head':    '<M-S-b>',
    \ 'sexp_flow_to_next_leaf_head':    '<M-S-w>',
    \ 'sexp_flow_to_prev_leaf_tail':    '<M-S-g>',
    \ 'sexp_flow_to_next_leaf_tail':    '<M-S-e>',
    \ 'sexp_move_to_prev_top_element':  '[[',
    \ 'sexp_move_to_next_top_element':  ']]',
    \ 'sexp_select_prev_element':       '[e',
    \ 'sexp_select_next_element':       ']e',
    \ 'sexp_indent':                    '==',
    \ 'sexp_indent_top':                '=-',
    \ 'sexp_indent_and_clean':          '<M-=>',
    \ 'sexp_indent_and_clean_top':      '<M-->',
    \ 'sexp_round_head_wrap_list':      '<LocalLeader>i',
    \ 'sexp_round_tail_wrap_list':      '<LocalLeader>I',
    \ 'sexp_square_head_wrap_list':     '<LocalLeader>[',
    \ 'sexp_square_tail_wrap_list':     '<LocalLeader>]',
    \ 'sexp_curly_head_wrap_list':      '<LocalLeader>{',
    \ 'sexp_curly_tail_wrap_list':      '<LocalLeader>}',
    \ 'sexp_round_head_wrap_element':   '<LocalLeader>w',
    \ 'sexp_round_tail_wrap_element':   '<LocalLeader>W',
    \ 'sexp_square_head_wrap_element':  '<LocalLeader>e[',
    \ 'sexp_square_tail_wrap_element':  '<LocalLeader>e]',
    \ 'sexp_curly_head_wrap_element':   '<LocalLeader>e{',
    \ 'sexp_curly_tail_wrap_element':   '<LocalLeader>e}',
    \ 'sexp_insert_at_list_head':       '<LocalLeader>h',
    \ 'sexp_insert_at_list_tail':       '<LocalLeader>l',
    \ 'sexp_splice_list':               '<LocalLeader>@',
    \ 'sexp_convolute':                 '<LocalLeader>?',
    \ 'sexp_clone_list':                '<LocalLeader>c',
    \ 'sexp_clone_list_sl':             '<LocalLeader><LocalLeader>c',
    \ 'sexp_clone_element':             '<LocalLeader>C',
    \ 'sexp_clone_element_sl':          '<LocalLeader><LocalLeader>C',
    \ 'sexp_raise_list':                '<LocalLeader>o',
    \ 'sexp_raise_element':             '<LocalLeader>O',
    \ 'sexp_swap_list_backward':        '<M-k>',
    \ 'sexp_swap_list_forward':         '<M-j>',
    \ 'sexp_swap_element_backward':     '<M-h>',
    \ 'sexp_swap_element_forward':      '<M-l>',
    \ 'sexp_emit_head_element':         '<M-S-j>',
    \ 'sexp_emit_tail_element':         '<M-S-k>',
    \ 'sexp_capture_prev_element':      '<M-S-h>',
    \ 'sexp_capture_next_element':      '<M-S-l>',
    \ }

if !empty(g:sexp_filetypes)
    augroup sexp_filetypes
        autocmd!
        execute 'autocmd FileType ' . g:sexp_filetypes . ' call s:sexp_create_mappings()'
    augroup END
endif

" Autoload and detect repeat.vim
silent! call repeat#set('')
let s:have_repeat_set = exists('*repeat#set')
" If it's available, use <Cmd> modifier at the head of command rhs.
" Rationale: The idiomatic (but now obsolete) `:<c-u>` has the undesirable side-effect of
" generating a 'CmdlineChanged' autocmd event for *every character* in the command line
" executed by the map. Beyond the obvious efficiency concerns, this is especially
" problematic for users of plugins like "smear-cursor", whose cursor animation logic
" assumes 'CmdlineChange' means the user is typing on the command line.
let s:have_cmd = has('nvim') || v:version >= 900
""" Functions {{{1

" This function is necessary when <cmd> is used in mappings.
" Rationale: The old approach (:<c-u>) changed mode from visual to normal as a side
" effect. Some sexp functions break if called in visual mode, and the <cmd> modifier does
" not cause exit from visual mode. The simplest solution is to place a call to this
" function after the <cmd> (and after capture of v:count/v:prevcount, which can be changed
" by a call to this function).
function! s:ensure_normal_mode()
	let mode = mode()
	if mode =~ "^[vV\<C-V>]"
		exe "normal \<Esc>"
	endif
endfu

command! -nargs=+       DEFPLUG  call <SID>defplug('000', <f-args>)
command! -nargs=+ -bang Defplug  call <SID>defplug('1' . string(!empty('<bang>')) . '0', <f-args>)
command! -nargs=+ -bang DefplugN call <SID>defplug('1' . string(!empty('<bang>')) . '1', <f-args>)

" Create a <Plug> mapping. The 'flags' faux bitfield dictates behavior:
"
"   * flags == 0**: Map rhs as a key sequence
"   * flags == 100: Map rhs as an expression
"   * flags == 110: Map rhs as an expression, and setup repeat
"   * flags == 101: Map rhs as an expression, and do not set '`
"   * flags == 111: Map rhs as an expression, set up repeat, and do not set '`
"
" We don't use an actual bitfield because the bitwise functions and() and or()
" were not introduced until patch 7.3.377.
"
function! s:defplug(flags, mapmode, name, ...)
    let lhs = a:mapmode . ' <silent> <Plug>(' . a:name . ')'
    let rhs = join(a:000)

    let asexpr = a:flags[0] == '1'
    let repeat = a:flags[1] == '1'
    let nojump = a:flags[2] == '1'
    let opmode = a:mapmode[0] ==# 'o'

    " Build prefix/postfix for wrapping rhs.
    let prefix = 'call sexp#pre_op("' . a:mapmode[0] . '", "' . a:name . '")'
    \ . ' \| try \| '
    let postfix = ' \| finally'
    \ . ' \| call sexp#post_op("' . a:mapmode[0] . '", "' . a:name . '")'
    \ . ' \| endtry'

    " Key sequence
    if !asexpr
	" Note: All the DEFPLUGs have been converted to DefplugN to ensure they handle
	" counts correctly. Since this block has no special v:count handling, it's unlikely
	" maps defined with DEFPLUG would even work correctly.
	" TODO: Consider removing DEFPLUG and simplifying this function accordingly: e.g.,
	" the prefix/postfix assignments could be inlined below.
        execute lhs . ' ' . rhs
        return 1
    endif

    " Common mapping prefix
    " RE: vv
    "   Due to a ?bug? in vim, we need to set curwin->w_curswant to the
    "   current cursor position by entering and exiting character-wise visual
    "   mode before completing an operator-pending command so that the cursor
    "   returns to its original position after an = command.
    " RE: b:sexp_count
    "   v:count and v:prevcount can change while the concatenated commands are executing.
    "   To ensure the count passed to functions is the one corresponding to the executed
    "   map, we cache either v:count or v:prevcount (whichever is present in the raw
    "   command) just after the cmd leader and replace all references to the vim count
    "   with references to the cached var.
    let use_count = rhs =~ 'v:prevcount' && !s:have_cmd ? 'v:prevcount' : 'v:count'
    let prefix = lhs . ' '
                 \ . (s:have_cmd ? '<cmd>' : ':<c-u>') . ' let b:sexp_count = ' . use_count
		 \ . (s:have_cmd ? ' \| call <SID>ensure_normal_mode()' : '') . ' \| '
                 \ . prefix
                 \ . (nojump ? '' : 'execute "normal! ' . (opmode ? 'vv' : '') . 'm`" \| ')
                 \ . 'call ' . substitute(rhs, 'v:\%(prev\)\?count', 'b:sexp_count', 'g')
    " Expression, non-repeating
    if !repeat || !s:have_repeat_set
        execute prefix . postfix . '<CR>'
    " Expression, repeating, operator-pending mode
    elseif opmode
        execute prefix
                \ . ' \| if v:operator ==? "c" \| '
                \ . '  call <SID>repeat_set(v:operator . "\<Plug>(' . a:name . ')\<lt>C-r>.\<lt>C-Bslash>\<lt>C-n>", b:sexp_count) \| '
                \ . 'else \| '
                \ . '  call <SID>repeat_set(v:operator . "\<Plug>(' . a:name . ')", b:sexp_count) \| '
                \ . 'endif'
                \ . postfix . '<CR>'
    " Expression, repeating, non-operator-pending mode
    else
        execute prefix . ' \| call <SID>repeat_set("\<Plug>(' . a:name . ')", b:sexp_count)'
                \ . postfix . '<CR>'
    endif
endfunction

" Calls repeat#set() and registers a one-time CursorMoved handler to correctly
" set the value of g:repeat_tick.
"
" cf. https://github.com/tpope/vim-repeat/issues/8#issuecomment-13951082
function! s:repeat_set(buf, count)
    call repeat#set(a:buf, a:count)
    augroup sexp_repeat
        autocmd!
        autocmd CursorMoved <buffer> let g:repeat_tick = b:changedtick | autocmd! sexp_repeat
    augroup END
endfunction

" Bind <Plug> mappings in current buffer to values in g:sexp_mappings or
" s:sexp_mappings
function! s:sexp_create_mappings()
    for plug in ['sexp_outer_list',           'sexp_inner_list',
               \ 'sexp_outer_top_list',       'sexp_inner_top_list',
               \ 'sexp_outer_string',         'sexp_inner_string',
               \ 'sexp_outer_element',        'sexp_inner_element',
               \ 'sexp_outer_child_tail',     'sexp_outer_child_head',
               \ 'sexp_inner_child_tail',     'sexp_inner_child_head']
        let lhs = get(g:sexp_mappings, plug, s:sexp_mappings[plug])
        if !empty(lhs)
            execute 'xmap <silent><buffer> ' . lhs . ' <Plug>(' . plug . ')'
            execute 'omap <silent><buffer> ' . lhs . ' <Plug>(' . plug . ')'
        endif
    endfor

    for plug in ['sexp_move_to_prev_bracket',      'sexp_move_to_next_bracket',
               \ 'sexp_move_to_prev_element_head', 'sexp_move_to_next_element_head',
               \ 'sexp_move_to_prev_element_tail', 'sexp_move_to_next_element_tail',
               \ 'sexp_move_to_prev_top_element',  'sexp_move_to_next_top_element',
               \ 'sexp_select_prev_element',       'sexp_select_next_element']
        let lhs = get(g:sexp_mappings, plug, s:sexp_mappings[plug])
        if !empty(lhs)
            execute 'nmap <silent><buffer> ' . lhs . ' <Plug>(' . plug . ')'
            execute 'xmap <silent><buffer> ' . lhs . ' <Plug>(' . plug . ')'
            execute 'omap <silent><buffer> ' . lhs . ' <Plug>(' . plug . ')'
        endif
    endfor

    for plug in ['sexp_insert_at_list_head', 'sexp_insert_at_list_tail',
               \ 'sexp_convolute',           'sexp_splice_list',
               \ 'sexp_indent_top',          'sexp_indent_and_clean_top']
        let lhs = get(g:sexp_mappings, plug, s:sexp_mappings[plug])
        if !empty(lhs)
            execute 'nmap <silent><buffer> ' . lhs . ' <Plug>(' . plug . ')'
        endif
    endfor

    for plug in ['sexp_round_head_wrap_list',     'sexp_round_tail_wrap_list',
               \ 'sexp_square_head_wrap_list',    'sexp_square_tail_wrap_list',
               \ 'sexp_curly_head_wrap_list',     'sexp_curly_tail_wrap_list',
               \ 'sexp_round_head_wrap_element',  'sexp_round_tail_wrap_element',
               \ 'sexp_square_head_wrap_element', 'sexp_square_tail_wrap_element',
               \ 'sexp_curly_head_wrap_element',  'sexp_curly_tail_wrap_element',
               \ 'sexp_raise_list',               'sexp_raise_element',
               \ 'sexp_swap_list_backward',       'sexp_swap_list_forward',
               \ 'sexp_swap_element_backward',    'sexp_swap_element_forward',
               \ 'sexp_emit_head_element',        'sexp_emit_tail_element',
               \ 'sexp_capture_prev_element',     'sexp_capture_next_element',
               \ 'sexp_flow_to_prev_close',       'sexp_flow_to_next_open',
               \ 'sexp_flow_to_prev_open',        'sexp_flow_to_next_close',
               \ 'sexp_flow_to_prev_leaf_head',   'sexp_flow_to_next_leaf_head',
               \ 'sexp_flow_to_prev_leaf_tail',   'sexp_flow_to_next_leaf_tail',
               \ 'sexp_clone_list',               'sexp_clone_list_sl',
               \ 'sexp_clone_element',            'sexp_clone_element_sl',
               \ 'sexp_indent',                   'sexp_indent_and_clean']
        let lhs = get(g:sexp_mappings, plug, s:sexp_mappings[plug])
        if !empty(lhs)
            execute 'nmap <silent><buffer> ' . lhs . ' <Plug>(' . plug . ')'
            if plug =~ '^sexp_indent' && lhs == '=='
                " Special Case: Just as == overrides Vim's default normal mode
                " command, = must override Vim's default visual mode command.
                " Rationale: Prevents ambiguity that leads to map delay.
                let lhs = '='
            endif
            execute 'xmap <silent><buffer> ' . lhs . ' <Plug>(' . plug . ')'
        endif
    endfor

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
Defplug  xnoremap sexp_outer_list sexp#docount(v:prevcount, 'sexp#select_current_list', 'v', 0, 1)
Defplug! onoremap sexp_outer_list sexp#docount(v:count, 'sexp#select_current_list', 'o', 0, 1)
Defplug  xnoremap sexp_inner_list sexp#docount(v:prevcount, 'sexp#select_current_list', 'v', 1, 1)
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
Defplug  xnoremap sexp_outer_element sexp#select_current_element('v', 0, v:prevcount)
Defplug! onoremap sexp_outer_element sexp#select_current_element('o', 0, v:count)
Defplug  xnoremap sexp_inner_element sexp#select_current_element('v', 1, v:prevcount)
Defplug! onoremap sexp_inner_element sexp#select_current_element('o', 1, v:count)

Defplug  xnoremap sexp_outer_child_head sexp#select_child('v', v:prevcount, 1, 0)
Defplug! onoremap sexp_outer_child_head sexp#select_child('o', v:count, 1, 0)
Defplug  xnoremap sexp_inner_child_head sexp#select_child('v', v:prevcount, 1, 1)
Defplug! onoremap sexp_inner_child_head sexp#select_child('o', v:count, 1, 1)

Defplug  xnoremap sexp_outer_child_tail sexp#select_child('v', v:prevcount, 0, 0)
Defplug! onoremap sexp_outer_child_tail sexp#select_child('o', v:count, 0, 0)
Defplug  xnoremap sexp_inner_child_tail sexp#select_child('v', v:prevcount, 0, 1)
Defplug! onoremap sexp_inner_child_tail sexp#select_child('o', v:count, 0, 1)
""" Text Object Motions {{{1

" Nearest bracket
Defplug  nnoremap sexp_move_to_prev_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'n', 0)
DefplugN xnoremap sexp_move_to_prev_bracket sexp#docount(v:prevcount, 'sexp#move_to_nearest_bracket', 'v', 0)
Defplug! onoremap sexp_move_to_prev_bracket sexp#move_to_nearest_bracket('o', 0)
Defplug  nnoremap sexp_move_to_next_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'n', 1)
DefplugN xnoremap sexp_move_to_next_bracket sexp#docount(v:prevcount, 'sexp#move_to_nearest_bracket', 'v', 1)
Defplug! onoremap sexp_move_to_next_bracket sexp#move_to_nearest_bracket('o', 1)

" Adjacent element head
"
" Visual mappings must break out of visual mode in order to detect which end
" the user is using to adjust the selection.
DefplugN  nnoremap sexp_move_to_prev_element_head sexp#move_to_adjacent_element('n', v:count, 0, 0, 0)
DefplugN  xnoremap sexp_move_to_prev_element_head sexp#move_to_adjacent_element('v', v:prevcount, 0, 0, 0)
DefplugN! onoremap sexp_move_to_prev_element_head sexp#move_to_adjacent_element('o', v:count, 0, 0, 0)
DefplugN  nnoremap sexp_move_to_next_element_head sexp#move_to_adjacent_element('n', v:count, 1, 0, 0)
DefplugN  xnoremap sexp_move_to_next_element_head sexp#move_to_adjacent_element('v', v:prevcount, 1, 0, 0)
DefplugN! onoremap sexp_move_to_next_element_head sexp#move_to_adjacent_element('o', v:count, 1, 0, 0)

" Adjacent element tail
"
" Inclusive operator pending motions require a visual mode selection to
" include the last character of a line.
DefplugN  nnoremap sexp_move_to_prev_element_tail sexp#move_to_adjacent_element('n', v:count, 0, 1, 0)
DefplugN  xnoremap sexp_move_to_prev_element_tail sexp#move_to_adjacent_element('v', v:prevcount, 0, 1, 0)
DefplugN! onoremap sexp_move_to_prev_element_tail sexp#move_to_adjacent_element('o', v:count, 0, 1, 0)
DefplugN  nnoremap sexp_move_to_next_element_tail sexp#move_to_adjacent_element('n', v:count, 1, 1, 0)
DefplugN  xnoremap sexp_move_to_next_element_tail sexp#move_to_adjacent_element('v', v:prevcount, 1, 1, 0)
DefplugN! onoremap sexp_move_to_next_element_tail sexp#move_to_adjacent_element('o', v:count, 1, 1, 0)

" List flow commands
Defplug   nnoremap sexp_flow_to_prev_close sexp#list_flow('n', v:count, 0, 1)
DefplugN  xnoremap sexp_flow_to_prev_close sexp#list_flow('v', v:prevcount, 0, 1)
Defplug   nnoremap sexp_flow_to_prev_open sexp#list_flow('n', v:count, 0, 0)
DefplugN  xnoremap sexp_flow_to_prev_open sexp#list_flow('v', v:prevcount, 0, 0)
Defplug   nnoremap sexp_flow_to_next_open sexp#list_flow('n', v:count, 1, 0)
DefplugN  xnoremap sexp_flow_to_next_open sexp#list_flow('v', v:prevcount, 1, 0)
Defplug   nnoremap sexp_flow_to_next_close sexp#list_flow('n', v:count, 1, 1)
DefplugN  xnoremap sexp_flow_to_next_close sexp#list_flow('v', v:prevcount, 1, 1)

" Leaf flow commands
DefplugN  nnoremap sexp_flow_to_prev_leaf_head sexp#leaf_flow('n', v:count, 0, 0)
DefplugN  xnoremap sexp_flow_to_prev_leaf_head sexp#leaf_flow('v', v:prevcount, 0, 0)
DefplugN  nnoremap sexp_flow_to_next_leaf_head sexp#leaf_flow('n', v:count, 1, 0)
DefplugN  xnoremap sexp_flow_to_next_leaf_head sexp#leaf_flow('v', v:prevcount, 1, 0)
DefplugN  nnoremap sexp_flow_to_prev_leaf_tail sexp#leaf_flow('n', v:count, 0, 1)
DefplugN  xnoremap sexp_flow_to_prev_leaf_tail sexp#leaf_flow('v', v:prevcount, 0, 1)
DefplugN  nnoremap sexp_flow_to_next_leaf_tail sexp#leaf_flow('n', v:count, 1, 1)
DefplugN  xnoremap sexp_flow_to_next_leaf_tail sexp#leaf_flow('v', v:prevcount, 1, 1)

" Adjacent top element
Defplug  nnoremap sexp_move_to_prev_top_element sexp#move_to_adjacent_element('n', v:count, 0, 0, 1)
DefplugN xnoremap sexp_move_to_prev_top_element sexp#move_to_adjacent_element('v', v:prevcount, 0, 0, 1)
Defplug! onoremap sexp_move_to_prev_top_element sexp#move_to_adjacent_element('o', v:count, 0, 0, 1)
Defplug  nnoremap sexp_move_to_next_top_element sexp#move_to_adjacent_element('n', v:count, 1, 0, 1)
DefplugN xnoremap sexp_move_to_next_top_element sexp#move_to_adjacent_element('v', v:prevcount, 1, 0, 1)
Defplug! onoremap sexp_move_to_next_top_element sexp#move_to_adjacent_element('o', v:count, 1, 0, 1)

" Adjacent element selection
"
" Unlike the other directional motions, calling this from normal mode places
" us in visual mode, with the adjacent element as our selection.
Defplug  nnoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'n', 0)
Defplug  xnoremap sexp_select_prev_element sexp#docount(v:prevcount, 'sexp#select_adjacent_element', 'v', 0)
Defplug! onoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'o', 0)
Defplug  nnoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'n', 1)
Defplug  xnoremap sexp_select_next_element sexp#docount(v:prevcount, 'sexp#select_adjacent_element', 'v', 1)
Defplug! onoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'o', 1)

""" Commands {{{1

" Indent S-Expression
" Question: Would 'repeat' make any sense for visual variants?
" Answer: I think it would, but for some reason, vim-sexp has historically not
" supported repeat for visual operations, so I guess I'll be consistent for
" now.
Defplug! nnoremap sexp_indent                sexp#indent('n', 0, v:count, -1)
Defplug  xnoremap sexp_indent                sexp#indent('x', 0, v:prevcount, -1)
Defplug! nnoremap sexp_indent_top            sexp#indent('n', 1, v:count, -1)
Defplug! nnoremap sexp_indent_and_clean      sexp#indent('n', 0, v:count, 1)
Defplug  xnoremap sexp_indent_and_clean      sexp#indent('x', 0, v:prevcount, 1)
Defplug! nnoremap sexp_indent_and_clean_top  sexp#indent('n', 1, v:count, 1)

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
Defplug! nnoremap sexp_raise_list    sexp#docount(v:count, 'sexp#raise', 'n', 'sexp#select_current_list', 'n', 0, 0)
Defplug  xnoremap sexp_raise_list    sexp#docount(v:prevcount, 'sexp#raise', 'v', '')
Defplug! nnoremap sexp_raise_element sexp#docount(v:count, 'sexp#raise', 'n', 'sexp#select_current_element', 'n', 1)
Defplug  xnoremap sexp_raise_element sexp#docount(v:prevcount, 'sexp#raise', 'v', '')

" Convolute
" Note: convolute takes pains to preserve cursor position: hence, 'nojump'.
DefplugN! nnoremap sexp_convolute sexp#convolute(v:count, 'n')

" Clone list
DefplugN  nnoremap sexp_clone_list    sexp#clone('n', v:count, 1, 0, 0)
DefplugN  xnoremap sexp_clone_list    sexp#clone('v', v:prevcount, 1, 0, 0)
DefplugN  nnoremap sexp_clone_list_sl sexp#clone('n', v:count, 1, 0, 1)
DefplugN  xnoremap sexp_clone_list_sl sexp#clone('v', v:prevcount, 1, 0, 1)

" Clone element
DefplugN  nnoremap sexp_clone_element    sexp#clone('n', v:count, 0, 0, 0)
DefplugN  xnoremap sexp_clone_element    sexp#clone('v', v:prevcount, 0, 0, 0)
DefplugN  nnoremap sexp_clone_element_sl sexp#clone('n', v:count, 0, 0, 1)
DefplugN  xnoremap sexp_clone_element_sl sexp#clone('v', v:prevcount, 0, 0, 1)

" Splice list
Defplug! nnoremap sexp_splice_list sexp#splice_list(v:count)

" Swap list
Defplug! nnoremap sexp_swap_list_backward sexp#docount(v:count, 'sexp#swap_element', 'n', 0, 1)
DefplugN xnoremap sexp_swap_list_backward sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 0, 1)
Defplug! nnoremap sexp_swap_list_forward  sexp#docount(v:count, 'sexp#swap_element', 'n', 1, 1)
DefplugN xnoremap sexp_swap_list_forward  sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 1, 1)

" Swap element
Defplug! nnoremap sexp_swap_element_backward sexp#docount(v:count, 'sexp#swap_element', 'n', 0, 0)
DefplugN xnoremap sexp_swap_element_backward sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 0, 0)
Defplug! nnoremap sexp_swap_element_forward  sexp#docount(v:count, 'sexp#swap_element', 'n', 1, 0)
DefplugN xnoremap sexp_swap_element_forward  sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 1, 0)

" Emit/capture element
Defplug! nnoremap sexp_emit_head_element    sexp#docount(v:count, 'sexp#stackop', 'n', 0, 0)
Defplug  xnoremap sexp_emit_head_element    sexp#docount(v:prevcount, 'sexp#stackop', 'v', 0, 0)
Defplug! nnoremap sexp_emit_tail_element    sexp#docount(v:count, 'sexp#stackop', 'n', 1, 0)
Defplug  xnoremap sexp_emit_tail_element    sexp#docount(v:prevcount, 'sexp#stackop', 'v', 1, 0)
Defplug! nnoremap sexp_capture_prev_element sexp#docount(v:count, 'sexp#stackop', 'n', 0, 1)
Defplug  xnoremap sexp_capture_prev_element sexp#docount(v:prevcount, 'sexp#stackop', 'v', 0, 1)
Defplug! nnoremap sexp_capture_next_element sexp#docount(v:count, 'sexp#stackop', 'n', 1, 1)
Defplug  xnoremap sexp_capture_next_element sexp#docount(v:prevcount, 'sexp#stackop', 'v', 1, 1)

""" Insert mode mappings {{{1

" Insert opening delimiter
inoremap <silent><expr> <Plug>(sexp_insert_opening_round)  sexp#opening_insertion('(')
inoremap <silent><expr> <Plug>(sexp_insert_opening_square) sexp#opening_insertion('[')
inoremap <silent><expr> <Plug>(sexp_insert_opening_curly)  sexp#opening_insertion('{')

" Insert closing delimiter
inoremap <silent><expr> <Plug>(sexp_insert_closing_round)  sexp#closing_insertion(')')
inoremap <silent><expr> <Plug>(sexp_insert_closing_square) sexp#closing_insertion(']')
inoremap <silent><expr> <Plug>(sexp_insert_closing_curly)  sexp#closing_insertion('}')

" Insert double quote
inoremap <silent><expr> <Plug>(sexp_insert_double_quote) sexp#quote_insertion('"')

" Delete paired delimiters
inoremap <silent><expr> <Plug>(sexp_insert_backspace) sexp#backspace_insertion()

""" Cleanup {{{1

delcommand DefplugN
delcommand Defplug
delcommand DEFPLUG

" vim:ts=4:sw=4:et:tw=90
