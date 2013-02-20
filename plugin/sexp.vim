
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
" Version:  0.1
" License:  MIT
" Homepage: https://github.com/guns/vim-sexp

if exists('g:sexp_loaded')
    finish
endif
let g:sexp_loaded = 1

""" Default options {{{1

if !exists('g:sexp_filetypes')
    let g:sexp_filetypes = 'clojure,scheme,lisp'
endif

if !exists('g:sexp_insert_after_wrap')
    let g:sexp_insert_after_wrap = 1
endif

if !exists('g:sexp_enable_insert_mode_mappings')
    let g:sexp_enable_insert_mode_mappings = 1
endif

if !exists('g:sexp_mappings')
    let g:sexp_mappings = {}
endif

let s:sexp_default_mappings = {
    \ 'sexp_select_form':                 'f',
    \ 'sexp_select_top_form':             'F',
    \ 'sexp_select_string':               's',
    \ 'sexp_select_element':              'e',
    \ 'sexp_move_to_prev_element':        '<M-b>',
    \ 'sexp_move_to_next_element':        '<M-w>',
    \ 'sexp_move_to_end_of_next_element': '<M-e>',
    \ 'sexp_move_to_prev_top_element':    '[[',
    \ 'sexp_move_to_next_top_element':    ']]',
    \ 'sexp_move_to_prev_bracket':        '(',
    \ 'sexp_move_to_next_bracket':        ')',
    \ 'sexp_select_prev_element':         '[e',
    \ 'sexp_select_next_element':         ']e',
    \ 'sexp_form_wrap_round_head':        '<Leader>i',
    \ 'sexp_form_wrap_round_tail':        '<Leader>I',
    \ 'sexp_form_wrap_square_head':       '<Leader>[',
    \ 'sexp_form_wrap_square_tail':       '<Leader>]',
    \ 'sexp_form_wrap_curly_head':        '<Leader>{',
    \ 'sexp_form_wrap_curly_tail':        '<Leader>}',
    \ 'sexp_element_wrap_round_head':     '<Leader>W',
    \ 'sexp_element_wrap_round_tail':     '<Leader>w',
    \ 'sexp_element_wrap_square_head':    '',
    \ 'sexp_element_wrap_square_tail':    '',
    \ 'sexp_element_wrap_curly_head':     '',
    \ 'sexp_element_wrap_curly_tail':     '',
    \ 'sexp_lift_form':                   '<Leader>o',
    \ 'sexp_splice_form':                 '<Leader>O',
    \ 'sexp_insert_at_form_head':         '<Leader>h',
    \ 'sexp_insert_at_form_tail':         '<Leader>l',
    \ 'sexp_swap_form_backward':          '<M-k>',
    \ 'sexp_swap_form_forward':           '<M-j>',
    \ 'sexp_swap_element_backward':       '<M-h>',
    \ 'sexp_swap_element_forward':        '<M-l>',
    \ 'sexp_emit_first_element':          '<M-J>',
    \ 'sexp_emit_last_element':           '<M-K>',
    \ 'sexp_capture_prev_element':        '<M-H>',
    \ 'sexp_capture_next_element':        '<M-L>',
    \ }

augroup sexp_autocommands
    autocmd!
augroup END

""" Utility functions {{{1

function! s:filetype_autocmd(...)
    if empty(g:sexp_filetypes) | return | endif
    augroup sexp_autocommands
        for cmd in a:000
            execute 'autocmd FileType ' . g:sexp_filetypes . ' ' . cmd
        endfor
    augroup END
endfunction

""" Text objects {{{1

" Current form
vnoremap <silent> <Plug>sexp_select_form_outer :<C-u>call sexp#docount(v:count, 'sexp#select_current_form', 'v', 0)<CR>
onoremap <silent> <Plug>sexp_select_form_outer :<C-u>call sexp#docount(v:count, 'sexp#select_current_form', 'o', 0)<CR>
vnoremap <silent> <Plug>sexp_select_form_inner :<C-u>call sexp#docount(v:count, 'sexp#select_current_form', 'v', 1)<CR>
onoremap <silent> <Plug>sexp_select_form_inner :<C-u>call sexp#docount(v:count, 'sexp#select_current_form', 'o', 1)<CR>

" Current top-level form
vnoremap <silent> <Plug>sexp_select_top_form_outer :<C-u>call sexp#select_current_top_form('v', 0)<CR>
onoremap <silent> <Plug>sexp_select_top_form_outer :<C-u>call sexp#select_current_top_form('o', 0)<CR>
vnoremap <silent> <Plug>sexp_select_top_form_inner :<C-u>call sexp#select_current_top_form('v', 1)<CR>
onoremap <silent> <Plug>sexp_select_top_form_inner :<C-u>call sexp#select_current_top_form('o', 1)<CR>

" Current string
vnoremap <silent> <Plug>sexp_select_string_outer :<C-u>call sexp#select_current_string('v', 0)<CR>
onoremap <silent> <Plug>sexp_select_string_outer :<C-u>call sexp#select_current_string('o', 0)<CR>
vnoremap <silent> <Plug>sexp_select_string_inner :<C-u>call sexp#select_current_string('v', 1)<CR>
onoremap <silent> <Plug>sexp_select_string_inner :<C-u>call sexp#select_current_string('o', 1)<CR>

" Current element
vnoremap <silent> <Plug>sexp_select_element_outer :<C-u>call sexp#select_current_element('v', 0)<CR>
onoremap <silent> <Plug>sexp_select_element_outer :<C-u>call sexp#select_current_element('o', 0)<CR>
vnoremap <silent> <Plug>sexp_select_element_inner :<C-u>call sexp#select_current_element('v', 1)<CR>
onoremap <silent> <Plug>sexp_select_element_inner :<C-u>call sexp#select_current_element('o', 1)<CR>

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

" Adjacent element
"
" NOTES:
"
"   * Ctrl-\_Ctrl-N breaks us directly out of visual mode into normal mode
"     without setting the cursor position to '<. This is necessary to detect
"     which end the user is using to adjust the selection.
"
nnoremap <silent> <Plug>sexp_move_to_prev_element   :<C-u>call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'n', 0, 0, 0)<CR>
vnoremap <silent> <Plug>sexp_move_to_prev_element   <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_adjacent_element', 'v', 0, 0, 0)<CR>
onoremap <silent> <Plug>sexp_move_to_prev_element   :<C-u>call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'o', 0, 0, 0)<CR>
nnoremap <silent> <Plug>sexp_move_to_next_element   :<C-u>call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'n', 1, 0, 0)<CR>
vnoremap <silent> <Plug>sexp_move_to_next_element   <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_adjacent_element', 'v', 1, 0, 0)<CR>
onoremap <silent> <Plug>sexp_move_to_next_element   :<C-u>call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'o', 1, 0, 0)<CR>
"
"   * Inclusive operator pending motions require a visual mode selection to
"     include the last character of a line.
"
nnoremap <silent> <Plug>sexp_move_to_end_of_next_element :<C-u>call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'n', 1, 1, 0)<CR>
vnoremap <silent> <Plug>sexp_move_to_end_of_next_element <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_adjacent_element', 'v', 1, 1, 0)<CR>
onoremap <silent> <Plug>sexp_move_to_end_of_next_element :call setpos("'<", getpos('.')) \|
                                                        \ call setpos("'>", getpos('.')) \|
                                                        \ call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'v', 1, 1, 0)<CR>

" Adjacent top element
nnoremap <silent> <Plug>sexp_move_to_prev_top_element :<C-u>call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'n', 0, 0, 1)<CR>
vnoremap <silent> <Plug>sexp_move_to_prev_top_element <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_adjacent_element', 'v', 0, 0, 1)<CR>
onoremap <silent> <Plug>sexp_move_to_prev_top_element :<C-u>call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'o', 0, 0, 1)<CR>
nnoremap <silent> <Plug>sexp_move_to_next_top_element :<C-u>call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'n', 1, 0, 1)<CR>
vnoremap <silent> <Plug>sexp_move_to_next_top_element <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_adjacent_element', 'v', 1, 0, 1)<CR>
onoremap <silent> <Plug>sexp_move_to_next_top_element :<C-u>call sexp#docount(v:count, 'sexp#move_to_adjacent_element', 'o', 1, 0, 1)<CR>

" Nearest bracket
nnoremap <silent> <Plug>sexp_move_to_prev_bracket :<C-u>call sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'n', 0)<CR>
vnoremap <silent> <Plug>sexp_move_to_prev_bracket <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_nearest_bracket', 'v', 0)<CR>
onoremap <silent> <Plug>sexp_move_to_prev_bracket :<C-u>call sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'o', 0)<CR>
nnoremap <silent> <Plug>sexp_move_to_next_bracket :<C-u>call sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'n', 1)<CR>
vnoremap <silent> <Plug>sexp_move_to_next_bracket <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#move_to_nearest_bracket', 'v', 1)<CR>
onoremap <silent> <Plug>sexp_move_to_next_bracket :<C-u>call sexp#docount(v:count, 'sexp#move_to_nearest_bracket', 'o', 1)<CR>

" Adjacent element selection
"
" Unlike the other directional motions, calling this from normal mode places
" us in visual mode, with the adjacent element as our selection.
nnoremap <silent> <Plug>sexp_select_prev_element :<C-u>call sexp#docount(v:count, 'sexp#select_adjacent_element', 'n', 0)<CR>
vnoremap <silent> <Plug>sexp_select_prev_element :<C-u>call sexp#docount(v:count, 'sexp#select_adjacent_element', 'v', 0)<CR>
onoremap <silent> <Plug>sexp_select_prev_element :<C-u>call sexp#docount(v:count, 'sexp#select_adjacent_element', 'o', 0)<CR>
nnoremap <silent> <Plug>sexp_select_next_element :<C-u>call sexp#docount(v:count, 'sexp#select_adjacent_element', 'n', 1)<CR>
vnoremap <silent> <Plug>sexp_select_next_element :<C-u>call sexp#docount(v:count, 'sexp#select_adjacent_element', 'v', 1)<CR>
onoremap <silent> <Plug>sexp_select_next_element :<C-u>call sexp#docount(v:count, 'sexp#select_adjacent_element', 'o', 1)<CR>

for s:plug in ['sexp_move_to_prev_element', 'sexp_move_to_next_element', 'sexp_move_to_end_of_next_element',
             \ 'sexp_move_to_prev_top_element', 'sexp_move_to_next_top_element',
             \ 'sexp_move_to_prev_bracket', 'sexp_move_to_next_bracket',
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
nnoremap <silent> <Plug>sexp_form_wrap_round_head  :<C-u>call sexp#wrap('f', '(', ')', 0, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_round_head  :<C-u>call sexp#wrap('v', '(', ')', 0, g:sexp_insert_after_wrap)<CR>
nnoremap <silent> <Plug>sexp_form_wrap_round_tail  :<C-u>call sexp#wrap('f', '(', ')', 1, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_round_tail  :<C-u>call sexp#wrap('v', '(', ')', 1, g:sexp_insert_after_wrap)<CR>
nnoremap <silent> <Plug>sexp_form_wrap_square_head :<C-u>call sexp#wrap('f', '[', ']', 0, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_square_head :<C-u>call sexp#wrap('v', '[', ']', 0, g:sexp_insert_after_wrap)<CR>
nnoremap <silent> <Plug>sexp_form_wrap_square_tail :<C-u>call sexp#wrap('f', '[', ']', 1, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_square_tail :<C-u>call sexp#wrap('v', '[', ']', 1, g:sexp_insert_after_wrap)<CR>
nnoremap <silent> <Plug>sexp_form_wrap_curly_head  :<C-u>call sexp#wrap('f', '{', '}', 0, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_curly_head  :<C-u>call sexp#wrap('v', '{', '}', 0, g:sexp_insert_after_wrap)<CR>
nnoremap <silent> <Plug>sexp_form_wrap_curly_tail  :<C-u>call sexp#wrap('f', '{', '}', 1, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_curly_tail  :<C-u>call sexp#wrap('v', '{', '}', 1, g:sexp_insert_after_wrap)<CR>

" Wrap element
nnoremap <silent> <Plug>sexp_element_wrap_round_head  :<C-u>call sexp#wrap('e', '(', ')', 0, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_round_head  :<C-u>call sexp#wrap('v', '(', ')', 0, g:sexp_insert_after_wrap)<CR>
nnoremap <silent> <Plug>sexp_element_wrap_round_tail  :<C-u>call sexp#wrap('e', '(', ')', 1, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_round_tail  :<C-u>call sexp#wrap('v', '(', ')', 1, g:sexp_insert_after_wrap)<CR>
nnoremap <silent> <Plug>sexp_element_wrap_square_head :<C-u>call sexp#wrap('e', '[', ']', 0, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_square_head :<C-u>call sexp#wrap('v', '[', ']', 0, g:sexp_insert_after_wrap)<CR>
nnoremap <silent> <Plug>sexp_element_wrap_square_tail :<C-u>call sexp#wrap('e', '[', ']', 1, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_square_tail :<C-u>call sexp#wrap('v', '[', ']', 1, g:sexp_insert_after_wrap)<CR>
nnoremap <silent> <Plug>sexp_element_wrap_curly_head  :<C-u>call sexp#wrap('e', '{', '}', 0, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_curly_head  :<C-u>call sexp#wrap('v', '{', '}', 0, g:sexp_insert_after_wrap)<CR>
nnoremap <silent> <Plug>sexp_element_wrap_curly_tail  :<C-u>call sexp#wrap('e', '{', '}', 1, g:sexp_insert_after_wrap)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_curly_tail  :<C-u>call sexp#wrap('v', '{', '}', 1, g:sexp_insert_after_wrap)<CR>

" Insert at form terminal
nnoremap <silent> <Plug>sexp_insert_at_form_head :<C-u>call sexp#insert_at_form_terminal(0)<CR>
vnoremap <silent> <Plug>sexp_insert_at_form_head :<C-u>call sexp#insert_at_form_terminal(0)<CR>
nnoremap <silent> <Plug>sexp_insert_at_form_tail :<C-u>call sexp#insert_at_form_terminal(1)<CR>
vnoremap <silent> <Plug>sexp_insert_at_form_tail :<C-u>call sexp#insert_at_form_terminal(1)<CR>

" Swap form
nnoremap <silent> <Plug>sexp_swap_form_backward :<C-u>call sexp#docount(v:count, 'sexp#swap_element', 'n', 0, 1)<CR>
vnoremap <silent> <Plug>sexp_swap_form_backward <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 0, 1)<CR>
nnoremap <silent> <Plug>sexp_swap_form_forward  :<C-u>call sexp#docount(v:count, 'sexp#swap_element', 'n', 1, 1)<CR>
vnoremap <silent> <Plug>sexp_swap_form_forward  <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 1, 1)<CR>

" Swap element
nnoremap <silent> <Plug>sexp_swap_element_backward :<C-u>call sexp#docount(v:count, 'sexp#swap_element', 'n', 0, 0)<CR>
vnoremap <silent> <Plug>sexp_swap_element_backward <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 0, 0)<CR>
nnoremap <silent> <Plug>sexp_swap_element_forward  :<C-u>call sexp#docount(v:count, 'sexp#swap_element', 'n', 1, 0)<CR>
vnoremap <silent> <Plug>sexp_swap_element_forward  <C-Bslash><C-n>:<C-u>call sexp#docount(v:prevcount, 'sexp#swap_element', 'v', 1, 0)<CR>

" Emit/capture element
nnoremap <silent> <Plug>sexp_emit_first_element   :<C-u>call sexp#docount(v:count, 'sexp#stackop', 'n', 0, 0)<CR>
vnoremap <silent> <Plug>sexp_emit_first_element   :<C-u>call sexp#docount(v:count, 'sexp#stackop', 'v', 0, 0)<CR>
nnoremap <silent> <Plug>sexp_emit_last_element    :<C-u>call sexp#docount(v:count, 'sexp#stackop', 'n', 1, 0)<CR>
vnoremap <silent> <Plug>sexp_emit_last_element    :<C-u>call sexp#docount(v:count, 'sexp#stackop', 'v', 1, 0)<CR>
nnoremap <silent> <Plug>sexp_capture_prev_element :<C-u>call sexp#docount(v:count, 'sexp#stackop', 'n', 0, 1)<CR>
vnoremap <silent> <Plug>sexp_capture_prev_element :<C-u>call sexp#docount(v:count, 'sexp#stackop', 'v', 0, 1)<CR>
nnoremap <silent> <Plug>sexp_capture_next_element :<C-u>call sexp#docount(v:count, 'sexp#stackop', 'n', 1, 1)<CR>
vnoremap <silent> <Plug>sexp_capture_next_element :<C-u>call sexp#docount(v:count, 'sexp#stackop', 'v', 1, 1)<CR>

" Lift form
nmap <silent> <Plug>sexp_lift_form d<Plug>sexp_select_form_outerv<Plug>sexp_select_form_outerp
vmap <silent> <Plug>sexp_lift_form dv<Plug>sexp_select_form_outerp

" Splice form
nnoremap <silent> <Plug>sexp_splice_form :<C-u>call sexp#splice_form()<CR>
vnoremap <silent> <Plug>sexp_splice_form :<C-u>call sexp#splice_form()<CR>

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
