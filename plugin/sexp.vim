
"    .o            o8o                                                              o.
"   .8'            '"'                                                              `8.
"  .8' oooo    ooooooo ooo. .oo.  .oo.        .oooo.o .ooooo. oooo    ooooo.ooooo.   `8.
"  88   `88.  .8' `888 `888P"Y88bP"Y88b      d88(  "8d88' `88b `88b..8P'  888' `88b   88
"  88    `88..8'   888  888   888   888 8888 `"Y88b. 888ooo888   Y888'    888   888   88
"  `8.    `888'    888  888   888   888      o.  )88b888    .o .o8"'88b   888   888  .8'
"   `8.    `8'    o888oo888o o888o o888o     8""888P'`Y8bod8P'o88'   888o 888bod8P' .8'
"    `"                                                                   888       "'
"                                                                        o888o
" Author:   guns <self@sungpae.com>
" License:  MIT
" Homepage: https://github.com/guns/vim-sexp

if exists('g:sexp_loaded')
    finish
endif
let g:sexp_loaded = 1

""" Global State {{{1

if !exists('g:sexp_filetypes')
    let g:sexp_filetypes = 'clojure,scheme,lisp'
endif

if !exists('g:sexp_enable_insert_mode_mappings')
    let g:sexp_enable_insert_mode_mappings = 1
endif

if !exists('g:sexp_insert_after_wrap')
    let g:sexp_insert_after_wrap = 1
endif

if !exists('g:sexp_mappings')
    let g:sexp_mappings = {}
endif

let s:sexp_default_mappings = {
    \ 'sexp_select_form':                 'f',
    \ 'sexp_select_top_form':             'F',
    \ 'sexp_select_string':               's',
    \ 'sexp_select_element':              'e',
    \ 'sexp_move_to_prev_bracket':        '(',
    \ 'sexp_move_to_next_bracket':        ')',
    \ 'sexp_move_to_prev_element':        '<M-b>',
    \ 'sexp_move_to_next_element':        '<M-w>',
    \ 'sexp_move_to_end_of_next_element': '<M-e>',
    \ 'sexp_move_to_prev_top_element':    '[[',
    \ 'sexp_move_to_next_top_element':    ']]',
    \ 'sexp_select_prev_element':         '[e',
    \ 'sexp_select_next_element':         ']e',
    \ 'sexp_form_wrap_round_head':        '<LocalLeader>i',
    \ 'sexp_form_wrap_round_tail':        '<LocalLeader>I',
    \ 'sexp_form_wrap_square_head':       '<LocalLeader>[',
    \ 'sexp_form_wrap_square_tail':       '<LocalLeader>]',
    \ 'sexp_form_wrap_curly_head':        '<LocalLeader>{',
    \ 'sexp_form_wrap_curly_tail':        '<LocalLeader>}',
    \ 'sexp_element_wrap_round_head':     '<LocalLeader>W',
    \ 'sexp_element_wrap_round_tail':     '<LocalLeader>w',
    \ 'sexp_element_wrap_square_head':    '<LocalLeader><M-[>',
    \ 'sexp_element_wrap_square_tail':    '<LocalLeader><M-]>',
    \ 'sexp_element_wrap_curly_head':     '<LocalLeader><M-{>',
    \ 'sexp_element_wrap_curly_tail':     '<LocalLeader><M-}>',
    \ 'sexp_lift_form':                   '<LocalLeader>o',
    \ 'sexp_splice_form':                 '<LocalLeader>O',
    \ 'sexp_insert_at_form_head':         '<LocalLeader>h',
    \ 'sexp_insert_at_form_tail':         '<LocalLeader>l',
    \ 'sexp_swap_form_backward':          '<M-k>',
    \ 'sexp_swap_form_forward':           '<M-j>',
    \ 'sexp_swap_element_backward':       '<M-h>',
    \ 'sexp_swap_element_forward':        '<M-l>',
    \ 'sexp_emit_first_element':          '<M-S-j>',
    \ 'sexp_emit_last_element':           '<M-S-k>',
    \ 'sexp_capture_prev_element':        '<M-S-h>',
    \ 'sexp_capture_next_element':        '<M-S-l>',
    \ }

augroup sexp_filetypes
    autocmd!
augroup END

silent! call repeat#set('') " Autoload repeat.vim

""" Utility functions {{{1

function! s:filetype_autocmd(...)
    if !empty(g:sexp_filetypes)
        augroup sexp_filetypes
            for cmd in a:000
                execute 'autocmd FileType ' . g:sexp_filetypes . ' ' . cmd
            endfor
        augroup END
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

" Create a <Plug> mapping. The 'mode' parameter dictates the behavior:
"
"   * mode == '' : Map to calling rhs as expression
"   * mode == '!': Map to calling rhs as expression, setting up repeat
"   * mode == '*': Map to rhs as a key sequence
"
function! s:defplug(mode, mapmode, name, ...)
    let lhs = a:mapmode . ' <silent> <Plug>' . a:name
    let rhs = join(a:000)
    let should_repeat = a:mode ==# '!'

    if a:mode ==# '*'
        execute lhs . ' ' . rhs
    elseif empty(a:mode) || (should_repeat && !exists('*repeat#set'))
        execute lhs . ' :<C-u>call ' . rhs . '<CR>'
    elseif should_repeat && a:mapmode[0] ==# 'o'
        " Due to a bug in vim, we need to set curwin->w_curswant to the
        " current cursor position by entering and exiting character-wise
        " visual mode before completing the operator-pending command so that
        " the cursor returns to it's original position after an = command.
        execute lhs . ' '
                \ . ':<C-u>let b:sexp_count = v:count \| '
                \ . 'execute "normal! vv" \| '
                \ . 'call ' . rhs . ' \| '
                \ . 'if v:operator ==? "c" \| '
                \ . '  call <SID>repeat_set(v:operator . "\<Plug>' . a:name . '\<lt>C-r>.\<lt>C-Bslash>\<lt>C-n>", b:sexp_count) \| '
                \ . 'else \| '
                \ . '  call <SID>repeat_set(v:operator . "\<Plug>' . a:name . '", b:sexp_count) \| '
                \ . 'endif<CR>'
    elseif should_repeat
        execute lhs . ' '
                \ . ':<C-u>let b:sexp_count = v:count \| '
                \ . 'call ' . rhs . ' \| '
                \ . 'call <SID>repeat_set("\<Plug>' . a:name . '", b:sexp_count)<CR>'
    endif
endfunction

command! -nargs=+ -bang Defplug call <SID>defplug('<bang>', <f-args>)
command! -nargs=+ -bang DEFPLUG call <SID>defplug('<bang>*', <f-args>)

""" Text objects {{{1

" Current form
Defplug  vnoremap sexp_select_form_outer sexp#docount(v:count, 'sexp#select_current_form', 'v', 0, 1)
Defplug! onoremap sexp_select_form_outer sexp#docount(v:count, 'sexp#select_current_form', 'o', 0, 1)
Defplug  vnoremap sexp_select_form_inner sexp#docount(v:count, 'sexp#select_current_form', 'v', 1, 1)
Defplug! onoremap sexp_select_form_inner sexp#docount(v:count, 'sexp#select_current_form', 'o', 1, 1)

" Current top-level form
Defplug  vnoremap sexp_select_top_form_outer sexp#select_current_top_form('v', 0)
Defplug! onoremap sexp_select_top_form_outer sexp#select_current_top_form('o', 0)
Defplug  vnoremap sexp_select_top_form_inner sexp#select_current_top_form('v', 1)
Defplug! onoremap sexp_select_top_form_inner sexp#select_current_top_form('o', 1)

" Current string
Defplug  vnoremap sexp_select_string_outer sexp#select_current_string('v', 0)
Defplug! onoremap sexp_select_string_outer sexp#select_current_string('o', 0)
Defplug  vnoremap sexp_select_string_inner sexp#select_current_string('v', 1)
Defplug! onoremap sexp_select_string_inner sexp#select_current_string('o', 1)

" Current element
Defplug  vnoremap sexp_select_element_outer sexp#select_current_element('v', 0)
Defplug! onoremap sexp_select_element_outer sexp#select_current_element('o', 0)
Defplug  vnoremap sexp_select_element_inner sexp#select_current_element('v', 1)
Defplug! onoremap sexp_select_element_inner sexp#select_current_element('o', 1)

for s:plug in ['sexp_select_form', 'sexp_select_top_form', 'sexp_select_string', 'sexp_select_element']
    let s:lhs = get(g:sexp_mappings, s:plug, s:sexp_default_mappings[s:plug])
    if !empty(s:lhs)
        call s:filetype_autocmd(
             \ 'vmap <silent><buffer> a' . s:lhs . ' <Plug>' . s:plug . '_outer',
             \ 'omap <silent><buffer> a' . s:lhs . ' <Plug>' . s:plug . '_outer',
             \ 'vmap <silent><buffer> i' . s:lhs . ' <Plug>' . s:plug . '_inner',
             \ 'omap <silent><buffer> i' . s:lhs . ' <Plug>' . s:plug . '_inner')
    endif
endfor

""" Directional motions {{{1

" Nearest bracket
Defplug  nnoremap sexp_move_to_prev_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'n', 0)
DEFPLUG  vnoremap sexp_move_to_prev_bracket <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_nearest_bracket', 'v', 0)<CR>
Defplug! onoremap sexp_move_to_prev_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'o', 0)
Defplug  nnoremap sexp_move_to_next_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'n', 1)
DEFPLUG  vnoremap sexp_move_to_next_bracket <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_nearest_bracket', 'v', 1)<CR>
Defplug! onoremap sexp_move_to_next_bracket sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'o', 1)

" Adjacent element
"
" NOTES:
"
"   * Ctrl-\_Ctrl-N breaks us directly out of visual mode into normal mode
"     without setting the cursor position to '<. This is necessary to detect
"     which end the user is using to adjust the selection.
"
Defplug  nnoremap sexp_move_to_prev_element sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'n', 0, 0, 0)
DEFPLUG  vnoremap sexp_move_to_prev_element <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_adjacent_element', 'v', 0, 0, 0)<CR>
Defplug! onoremap sexp_move_to_prev_element sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'o', 0, 0, 0)
Defplug  nnoremap sexp_move_to_next_element sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'n', 1, 0, 0)
DEFPLUG  vnoremap sexp_move_to_next_element <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_adjacent_element', 'v', 1, 0, 0)<CR>
Defplug! onoremap sexp_move_to_next_element sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'o', 1, 0, 0)
"
"   * Inclusive operator pending motions require a visual mode selection to
"     include the last character of a line.
"
Defplug  nnoremap sexp_move_to_end_of_next_element sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'n', 1, 1, 0)
DEFPLUG  vnoremap sexp_move_to_end_of_next_element <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_adjacent_element', 'v', 1, 1, 0)<CR>
Defplug! onoremap sexp_move_to_end_of_next_element setpos("'<", getpos('.')) \| call setpos("'>", getpos('.')) \|
                                                 \ call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'v', 1, 1, 0)

" Adjacent top element
Defplug  nnoremap sexp_move_to_prev_top_element sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'n', 0, 0, 1)
DEFPLUG  vnoremap sexp_move_to_prev_top_element <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_adjacent_element', 'v', 0, 0, 1)<CR>
Defplug! onoremap sexp_move_to_prev_top_element sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'o', 0, 0, 1)
Defplug  nnoremap sexp_move_to_next_top_element sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'n', 1, 0, 1)
DEFPLUG  vnoremap sexp_move_to_next_top_element <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_adjacent_element', 'v', 1, 0, 1)<CR>
Defplug! onoremap sexp_move_to_next_top_element sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'o', 1, 0, 1)

" Adjacent element selection
"
" Unlike the other directional motions, calling this from normal mode places
" us in visual mode, with the adjacent element as our selection.
Defplug  nnoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'n', 0)
Defplug  vnoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'v', 0)
Defplug! onoremap sexp_select_prev_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'o', 0)
Defplug  nnoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'n', 1)
Defplug  vnoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'v', 1)
Defplug! onoremap sexp_select_next_element sexp#docount(v:count, 'sexp#select_adjacent_element', 'o', 1)

for s:plug in ['sexp_move_to_prev_bracket', 'sexp_move_to_next_bracket',
             \ 'sexp_move_to_prev_element', 'sexp_move_to_next_element', 'sexp_move_to_end_of_next_element',
             \ 'sexp_move_to_prev_top_element', 'sexp_move_to_next_top_element',
             \ 'sexp_select_prev_element', 'sexp_select_next_element']
    let s:lhs = get(g:sexp_mappings, s:plug, s:sexp_default_mappings[s:plug])
    if !empty(s:lhs)
        call s:filetype_autocmd(
             \ 'nmap <silent><buffer> ' . s:lhs . ' <Plug>' . s:plug,
             \ 'vmap <silent><buffer> ' . s:lhs . ' <Plug>' . s:plug,
             \ 'omap <silent><buffer> ' . s:lhs . ' <Plug>' . s:plug)
    endif
endfor

""" S-expression commands {{{1

" Wrap form
Defplug! nnoremap sexp_form_wrap_round_head  sexp#wrap('f', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_form_wrap_round_head  sexp#wrap('v', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_form_wrap_round_tail  sexp#wrap('f', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_form_wrap_round_tail  sexp#wrap('v', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_form_wrap_square_head sexp#wrap('f', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_form_wrap_square_head sexp#wrap('v', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_form_wrap_square_tail sexp#wrap('f', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_form_wrap_square_tail sexp#wrap('v', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_form_wrap_curly_head  sexp#wrap('f', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_form_wrap_curly_head  sexp#wrap('v', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_form_wrap_curly_tail  sexp#wrap('f', '{', '}', 1, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_form_wrap_curly_tail  sexp#wrap('v', '{', '}', 1, g:sexp_insert_after_wrap)

" Wrap element
Defplug! nnoremap sexp_element_wrap_round_head  sexp#wrap('e', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_element_wrap_round_head  sexp#wrap('v', '(', ')', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_element_wrap_round_tail  sexp#wrap('e', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_element_wrap_round_tail  sexp#wrap('v', '(', ')', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_element_wrap_square_head sexp#wrap('e', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_element_wrap_square_head sexp#wrap('v', '[', ']', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_element_wrap_square_tail sexp#wrap('e', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_element_wrap_square_tail sexp#wrap('v', '[', ']', 1, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_element_wrap_curly_head  sexp#wrap('e', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_element_wrap_curly_head  sexp#wrap('v', '{', '}', 0, g:sexp_insert_after_wrap)
Defplug! nnoremap sexp_element_wrap_curly_tail  sexp#wrap('e', '{', '}', 1, g:sexp_insert_after_wrap)
Defplug  vnoremap sexp_element_wrap_curly_tail  sexp#wrap('v', '{', '}', 1, g:sexp_insert_after_wrap)

" Insert at form terminal
Defplug! nnoremap sexp_insert_at_form_head sexp#insert_at_form_terminal(0)
Defplug  vnoremap sexp_insert_at_form_head sexp#insert_at_form_terminal(0)
Defplug! nnoremap sexp_insert_at_form_tail sexp#insert_at_form_terminal(1)
Defplug  vnoremap sexp_insert_at_form_tail sexp#insert_at_form_terminal(1)

" Swap form
Defplug! nnoremap sexp_swap_form_backward sexp#docount(v:count, 'sexp#swap_element', 'n', 0, 1)
DEFPLUG  vnoremap sexp_swap_form_backward <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 0, 1)<CR>
Defplug! nnoremap sexp_swap_form_forward  sexp#docount(v:count, 'sexp#swap_element', 'n', 1, 1)
DEFPLUG  vnoremap sexp_swap_form_forward  <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 1, 1)<CR>

" Swap element
Defplug! nnoremap sexp_swap_element_backward sexp#docount(v:count, 'sexp#swap_element', 'n', 0, 0)
DEFPLUG  vnoremap sexp_swap_element_backward <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 0, 0)<CR>
Defplug! nnoremap sexp_swap_element_forward  sexp#docount(v:count, 'sexp#swap_element', 'n', 1, 0)
DEFPLUG  vnoremap sexp_swap_element_forward  <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 1, 0)<CR>

" Emit/capture element
Defplug! nnoremap sexp_emit_first_element   sexp#docount(v:count, 'sexp#stackop', 'n', 0, 0)
Defplug  vnoremap sexp_emit_first_element   sexp#docount(v:count, 'sexp#stackop', 'v', 0, 0)
Defplug! nnoremap sexp_emit_last_element    sexp#docount(v:count, 'sexp#stackop', 'n', 1, 0)
Defplug  vnoremap sexp_emit_last_element    sexp#docount(v:count, 'sexp#stackop', 'v', 1, 0)
Defplug! nnoremap sexp_capture_prev_element sexp#docount(v:count, 'sexp#stackop', 'n', 0, 1)
Defplug  vnoremap sexp_capture_prev_element sexp#docount(v:count, 'sexp#stackop', 'v', 0, 1)
Defplug! nnoremap sexp_capture_next_element sexp#docount(v:count, 'sexp#stackop', 'n', 1, 1)
Defplug  vnoremap sexp_capture_next_element sexp#docount(v:count, 'sexp#stackop', 'v', 1, 1)

" Lift form
Defplug! nnoremap sexp_lift_form sexp#docount(v:count, 'sexp#lift_form', 'n')
Defplug  vnoremap sexp_lift_form sexp#docount(v:count, 'sexp#lift_form', 'v')

" Splice form
Defplug! nnoremap sexp_splice_form sexp#docount(v:count, 'sexp#splice_form')
Defplug  vnoremap sexp_splice_form sexp#docount(v:count, 'sexp#splice_form')

for s:plug in ['sexp_form_wrap_round_head', 'sexp_form_wrap_round_tail',
             \ 'sexp_form_wrap_square_head', 'sexp_form_wrap_square_tail',
             \ 'sexp_form_wrap_curly_head', 'sexp_form_wrap_curly_tail',
             \ 'sexp_element_wrap_round_head', 'sexp_element_wrap_round_tail',
             \ 'sexp_element_wrap_square_head', 'sexp_element_wrap_square_tail',
             \ 'sexp_element_wrap_curly_head', 'sexp_element_wrap_curly_tail',
             \ 'sexp_insert_at_form_head', 'sexp_insert_at_form_tail',
             \ 'sexp_swap_form_backward', 'sexp_swap_form_forward',
             \ 'sexp_swap_element_backward', 'sexp_swap_element_forward',
             \ 'sexp_emit_first_element', 'sexp_emit_last_element',
             \ 'sexp_capture_prev_element', 'sexp_capture_next_element',
             \ 'sexp_lift_form', 'sexp_splice_form']
    let s:lhs = get(g:sexp_mappings, s:plug, s:sexp_default_mappings[s:plug])
    if !empty(s:lhs)
        call s:filetype_autocmd(
             \ 'nmap <silent><buffer> ' . s:lhs . ' <Plug>' . s:plug,
             \ 'vmap <silent><buffer> ' . s:lhs . ' <Plug>' . s:plug)
    endif
endfor

""" Insert mode mappings {{{1

" Insert opening delimiter
inoremap <silent><expr> <Plug>sexp_insert_opening_round  sexp#opening_insertion('(')
inoremap <silent><expr> <Plug>sexp_insert_opening_square sexp#opening_insertion('[')
inoremap <silent><expr> <Plug>sexp_insert_opening_curly  sexp#opening_insertion('{')

" Insert closing delimiter
inoremap <silent><expr> <Plug>sexp_insert_closing_round  sexp#closing_insertion(')')
inoremap <silent><expr> <Plug>sexp_insert_closing_square sexp#closing_insertion(']')
inoremap <silent><expr> <Plug>sexp_insert_closing_curly  sexp#closing_insertion('}')

" Insert double quote
inoremap <silent><expr> <Plug>sexp_insert_double_quote sexp#quote_insertion('"')

" Delete paired delimiters
inoremap <silent><expr> <Plug>sexp_insert_backspace sexp#backspace_insertion()

if g:sexp_enable_insert_mode_mappings
    call s:filetype_autocmd(
         \ 'imap <buffer> (    <Plug>sexp_insert_opening_round',
         \ 'imap <buffer> [    <Plug>sexp_insert_opening_square',
         \ 'imap <buffer> {    <Plug>sexp_insert_opening_curly',
         \ 'imap <buffer> )    <Plug>sexp_insert_closing_round',
         \ 'imap <buffer> ]    <Plug>sexp_insert_closing_square',
         \ 'imap <buffer> }    <Plug>sexp_insert_closing_curly',
         \ 'imap <buffer> "    <Plug>sexp_insert_double_quote',
         \ 'imap <buffer> <BS> <Plug>sexp_insert_backspace')
endif

""" Cleanup {{{1

delcommand Defplug
delcommand DEFPLUG
delfunction s:defplug
delfunction s:filetype_autocmd
