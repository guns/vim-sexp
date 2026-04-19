" This file is for data that would be script-local except that it's needed by multiple
" modules: i.e., plugin-global data. The data are defined script-locally and public
" accessors are provided.

" Define the plugin's default mappings.
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
    \ 'sexp_put_before':                   {'n':  'P'},
    \ 'sexp_put_after':                    {'n':  'p'},
    \ 'sexp_put_before_smart':             {'n':  '<LocalLeader>P'},
    \ 'sexp_put_after_smart':              {'n':  '<LocalLeader>p'},
    \ 'sexp_put_before_op':                {'n':  '<p'},
    \ 'sexp_put_after_op':                 {'n':  '>p'},
    \ 'sexp_replace':                      {'x':  'p', 'n': 'gp'},
    \ 'sexp_replace_P':                    {'x':  'P', 'n': 'gP'},
    \ 'sexp_replace_smart':                {'x':  '<LocalLeader>p', 'n': '<LocalLeader>gp'},
    \ 'sexp_replace_P_smart':              {'x':  '<LocalLeader>P', 'n': '<LocalLeader>gP'},
    \ 'sexp_replace_op':                   {'n':  '<M-p>'},
    \ 'sexp_replace_op_P':                 {'n':  '<M-P>'},
    \ 'sexp_put_at_head':                  {'n':  '<LocalLeader><p'},
    \ 'sexp_put_at_tail':                  {'n':  '<LocalLeader>>p'},
    \ 'p':                                 {'nx': ''},
    \ 'P':                                 {'nx': ''},
    \ 'gp':                                {'n':  ''},
    \ 'gP':                                {'n':  ''},
    \ }

" Dictionary mapping contextual regput commands to their builtin-overridden modes.
" TODO: Consider synthesizing this dict from s:sexp_mappings and a list of builtins
" (possibly associated with modes).
let s:regput_builtin_commands = {
    \ 'sexp_put_before': {
    \     'modes': 'n',
    \ },
    \ 'sexp_put_after': {
    \     'modes': 'n',
    \ },
    \ 'sexp_replace': {
    \     'modes': 'xn',
    \ },
    \ 'sexp_replace_P': {
    \     'modes': 'xn',
    \ },
\ }

" Return the default sexp mappings dictionary
function! sexp#data#get_sexp_mappings()
    return s:sexp_mappings
endfunction

" Return the default regput builtins dictionary
function! sexp#data#get_regput_builtin_commands()
    return s:regput_builtin_commands
endfunction

" vim:ts=4:sw=4:et:tw=90
