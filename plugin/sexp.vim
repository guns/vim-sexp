
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

if !exists('g:sexp_wrap_insert')
    let g:sexp_wrap_insert = 1
endif

if !exists('g:sexp_enable_insert_mode_mappings')
    let g:sexp_enable_insert_mode_mappings = 1
endif

if !exists('g:sexp_textobj_mappings')
    " TODO: Document that 's' mapping overrides inner/outer sentence
    let g:sexp_textobj_mappings = {
        \ 'form':                             'f',
        \ 'top_form':                         'F',
        \ 'string':                           's',
        \ 'comment':                          'c',
        \ 'atom':                             '',
        \ 'element':                          'e',
        \ 'prev_element':                     '[w',
        \ 'next_element':                     ']w',
        \ 'prev_top_element':                 '[[',
        \ 'next_top_element':                 ']]',
        \ 'exclusive_prev_element_selection': '[W',
        \ 'exclusive_next_element_selection': ']W',
    \ }
endif

if !exists('g:sexp_mappings')
    " FIXME: Set to LocalLeader before release!
    let g:sexp_mappings = {
        \ 'sexp_form_wrap_round_head':     '<Leader>i',
        \ 'sexp_form_wrap_round_tail':     '<Leader>I',
        \ 'sexp_form_wrap_square_head':    '<Leader>[',
        \ 'sexp_form_wrap_square_tail':    '<Leader>]',
        \ 'sexp_form_wrap_curly_head':     '<Leader>{',
        \ 'sexp_form_wrap_curly_tail':     '<Leader>}',
        \ 'sexp_element_wrap_round_head':  '<Leader>W',
        \ 'sexp_element_wrap_round_tail':  '<Leader>w',
        \ 'sexp_element_wrap_square_head': '',
        \ 'sexp_element_wrap_square_tail': '',
        \ 'sexp_element_wrap_curly_head':  '',
        \ 'sexp_element_wrap_curly_tail':  '',
        \ 'sexp_lift_form':                '<Leader>o',
        \ 'sexp_splice_form':              '<Leader>O',
        \ 'sexp_insert_at_form_head':      '<Leader>h',
        \ 'sexp_insert_at_form_tail':      '<Leader>l',
        \ 'sexp_swap_form_backward':       '<M-k>',
        \ 'sexp_swap_form_forward':        '<M-j>',
        \ 'sexp_swap_element_backward':    '<M-h>',
        \ 'sexp_swap_element_forward':     '<M-l>',
        \ 'sexp_capture_prev_element':     '<Leader>H',
        \ 'sexp_capture_next_element':     '<Leader>L',
        \ 'sexp_emit_first_element':       '<Leader><M-h>',
        \ 'sexp_emit_last_element':        '<Leader><M-l>',
    \ }
endif

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

""" Text object mappings {{{1

" Current form
vnoremap <silent> <Plug>sexp_textobj_outer_form :<C-u>call sexp#docount("sexp#select_current_form('v', 0)", v:count)<CR>
onoremap <silent> <Plug>sexp_textobj_outer_form :<C-u>call sexp#docount("sexp#select_current_form('o', 0)", v:count)<CR>
vnoremap <silent> <Plug>sexp_textobj_inner_form :<C-u>call sexp#docount("sexp#select_current_form('v', 1)", v:count)<CR>
onoremap <silent> <Plug>sexp_textobj_inner_form :<C-u>call sexp#docount("sexp#select_current_form('o', 1)", v:count)<CR>

" Current top-level form
vnoremap <silent> <Plug>sexp_textobj_outer_top_form :<C-u>call sexp#select_current_top_form('v', 0)<CR>
onoremap <silent> <Plug>sexp_textobj_outer_top_form :<C-u>call sexp#select_current_top_form('o', 0)<CR>
vnoremap <silent> <Plug>sexp_textobj_inner_top_form :<C-u>call sexp#select_current_top_form('v', 1)<CR>
onoremap <silent> <Plug>sexp_textobj_inner_top_form :<C-u>call sexp#select_current_top_form('o', 1)<CR>

" Current string
vnoremap <silent> <Plug>sexp_textobj_outer_string :<C-u>call sexp#select_current_string('v', 0)<CR>
onoremap <silent> <Plug>sexp_textobj_outer_string :<C-u>call sexp#select_current_string('o', 0)<CR>
vnoremap <silent> <Plug>sexp_textobj_inner_string :<C-u>call sexp#select_current_string('v', 1)<CR>
onoremap <silent> <Plug>sexp_textobj_inner_string :<C-u>call sexp#select_current_string('o', 1)<CR>

" Current comment
vnoremap <silent> <Plug>sexp_textobj_outer_comment :<C-u>call sexp#select_current_comment('v', 0)<CR>
onoremap <silent> <Plug>sexp_textobj_outer_comment :<C-u>call sexp#select_current_comment('o', 0)<CR>
vnoremap <silent> <Plug>sexp_textobj_inner_comment :<C-u>call sexp#select_current_comment('v', 1)<CR>
onoremap <silent> <Plug>sexp_textobj_inner_comment :<C-u>call sexp#select_current_comment('o', 1)<CR>

" Current atom
vnoremap <silent> <Plug>sexp_textobj_outer_atom :<C-u>call sexp#select_current_atom('v', 0)<CR>
onoremap <silent> <Plug>sexp_textobj_outer_atom :<C-u>call sexp#select_current_atom('o', 0)<CR>
vnoremap <silent> <Plug>sexp_textobj_inner_atom :<C-u>call sexp#select_current_atom('v', 1)<CR>
onoremap <silent> <Plug>sexp_textobj_inner_atom :<C-u>call sexp#select_current_atom('o', 1)<CR>

" Current element
vnoremap <silent> <Plug>sexp_textobj_outer_element :<C-u>call sexp#select_current_element('v', 0)<CR>
onoremap <silent> <Plug>sexp_textobj_outer_element :<C-u>call sexp#select_current_element('o', 0)<CR>
vnoremap <silent> <Plug>sexp_textobj_inner_element :<C-u>call sexp#select_current_element('v', 1)<CR>
onoremap <silent> <Plug>sexp_textobj_inner_element :<C-u>call sexp#select_current_element('o', 1)<CR>

" Adjacent element
"
" Note that Ctrl-\_Ctrl-N breaks us directly out of visual mode into normal
" mode without setting the cursor position to '<. This is necessary to detect
" which end the user is using to adjust the selection.
nnoremap <silent> <Plug>sexp_textobj_prev_element :<C-u>call sexp#docount("sexp#move_to_adjacent_element('n', 0, 0)", v:count)<CR>
vnoremap <silent> <Plug>sexp_textobj_prev_element <C-Bslash><C-n>:<C-u>call sexp#docount("sexp#move_to_adjacent_element('v', 0, 0)", v:prevcount)<CR>
onoremap <silent> <Plug>sexp_textobj_prev_element :<C-u>call sexp#docount("sexp#move_to_adjacent_element('o', 0, 0)", v:count)<CR>
nnoremap <silent> <Plug>sexp_textobj_next_element :<C-u>call sexp#docount("sexp#move_to_adjacent_element('n', 1, 0)", v:count)<CR>
vnoremap <silent> <Plug>sexp_textobj_next_element <C-Bslash><C-n>:<C-u>call sexp#docount("sexp#move_to_adjacent_element('v', 1, 0)", v:prevcount)<CR>
onoremap <silent> <Plug>sexp_textobj_next_element :<C-u>call sexp#docount("sexp#move_to_adjacent_element('o', 1, 0)", v:count)<CR>

" Adjacent top element
nnoremap <silent> <Plug>sexp_textobj_prev_top_element :<C-u>call sexp#docount("sexp#move_to_adjacent_element('n', 0, 1)", v:count)<CR>
vnoremap <silent> <Plug>sexp_textobj_prev_top_element <C-Bslash><C-n>:<C-u>call sexp#docount("sexp#move_to_adjacent_element('v', 0, 1)", v:prevcount)<CR>
onoremap <silent> <Plug>sexp_textobj_prev_top_element :<C-u>call sexp#docount("sexp#move_to_adjacent_element('o', 0, 1)", v:count)<CR>
nnoremap <silent> <Plug>sexp_textobj_next_top_element :<C-u>call sexp#docount("sexp#move_to_adjacent_element('n', 1, 1)", v:count)<CR>
vnoremap <silent> <Plug>sexp_textobj_next_top_element <C-Bslash><C-n>:<C-u>call sexp#docount("sexp#move_to_adjacent_element('v', 1, 1)", v:prevcount)<CR>
onoremap <silent> <Plug>sexp_textobj_next_top_element :<C-u>call sexp#docount("sexp#move_to_adjacent_element('o', 1, 1)", v:count)<CR>

" Adjacent element exclusive selection.
"
" Unlike other text objects, calling this from normal moves us to visual mode,
" with the adjacent element as our selection. I opted to make this a text
" object instead of a command since it makes sense to have an operator pending
" version of this movement.
nnoremap <silent> <Plug>sexp_textobj_exclusive_prev_element_selection :<C-u>call sexp#docount("sexp#select_adjacent_element('n', 0)", v:count)<CR>
vnoremap <silent> <Plug>sexp_textobj_exclusive_prev_element_selection :<C-u>call sexp#docount("sexp#select_adjacent_element('v', 0)", v:count)<CR>
onoremap <silent> <Plug>sexp_textobj_exclusive_prev_element_selection :<C-u>call sexp#docount("sexp#select_adjacent_element('o', 0)", v:count)<CR>
nnoremap <silent> <Plug>sexp_textobj_exclusive_next_element_selection :<C-u>call sexp#docount("sexp#select_adjacent_element('n', 1)", v:count)<CR>
vnoremap <silent> <Plug>sexp_textobj_exclusive_next_element_selection :<C-u>call sexp#docount("sexp#select_adjacent_element('v', 1)", v:count)<CR>
onoremap <silent> <Plug>sexp_textobj_exclusive_next_element_selection :<C-u>call sexp#docount("sexp#select_adjacent_element('o', 1)", v:count)<CR>

if !empty('g:sexp_textobj_mappings')
    for s:key in ['form', 'top_form', 'string', 'comment', 'atom', 'element']
        if has_key(g:sexp_textobj_mappings, s:key) && !empty(g:sexp_textobj_mappings[s:key])
            call s:filetype_autocmd(
                \ 'vmap <silent><buffer> a' . g:sexp_textobj_mappings[s:key] . ' <Plug>sexp_textobj_outer_' . s:key,
                \ 'omap <silent><buffer> a' . g:sexp_textobj_mappings[s:key] . ' <Plug>sexp_textobj_outer_' . s:key,
                \ 'vmap <silent><buffer> i' . g:sexp_textobj_mappings[s:key] . ' <Plug>sexp_textobj_inner_' . s:key,
                \ 'omap <silent><buffer> i' . g:sexp_textobj_mappings[s:key] . ' <Plug>sexp_textobj_inner_' . s:key)
        endif
    endfor

    for s:key in ['next_element', 'prev_element', 'prev_top_element', 'next_top_element',
                \ 'exclusive_prev_element_selection', 'exclusive_next_element_selection']
        if has_key(g:sexp_textobj_mappings, s:key) && !empty(g:sexp_textobj_mappings[s:key])
            call s:filetype_autocmd(
                \ 'nmap <silent><buffer> ' . g:sexp_textobj_mappings[s:key] . ' <Plug>sexp_textobj_' . s:key,
                \ 'vmap <silent><buffer> ' . g:sexp_textobj_mappings[s:key] . ' <Plug>sexp_textobj_' . s:key,
                \ 'omap <silent><buffer> ' . g:sexp_textobj_mappings[s:key] . ' <Plug>sexp_textobj_' . s:key)
        endif
    endfor
endif

""" S-expression mappings {{{1

" These definitions could be created in a loop, but we'll keep them in long
" form for easy grepping.

" Wrap form
nnoremap <silent> <Plug>sexp_form_wrap_round_head  :<C-u>call sexp#wrap('f', '(', ')', 0, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_round_head  :<C-u>call sexp#wrap('v', '(', ')', 0, g:sexp_wrap_insert)<CR>
nnoremap <silent> <Plug>sexp_form_wrap_round_tail  :<C-u>call sexp#wrap('f', '(', ')', 1, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_round_tail  :<C-u>call sexp#wrap('v', '(', ')', 1, g:sexp_wrap_insert)<CR>
nnoremap <silent> <Plug>sexp_form_wrap_square_head :<C-u>call sexp#wrap('f', '[', ']', 0, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_square_head :<C-u>call sexp#wrap('v', '[', ']', 0, g:sexp_wrap_insert)<CR>
nnoremap <silent> <Plug>sexp_form_wrap_square_tail :<C-u>call sexp#wrap('f', '[', ']', 1, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_square_tail :<C-u>call sexp#wrap('v', '[', ']', 1, g:sexp_wrap_insert)<CR>
nnoremap <silent> <Plug>sexp_form_wrap_curly_head  :<C-u>call sexp#wrap('f', '{', '}', 0, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_curly_head  :<C-u>call sexp#wrap('v', '{', '}', 0, g:sexp_wrap_insert)<CR>
nnoremap <silent> <Plug>sexp_form_wrap_curly_tail  :<C-u>call sexp#wrap('f', '{', '}', 1, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_form_wrap_curly_tail  :<C-u>call sexp#wrap('v', '{', '}', 1, g:sexp_wrap_insert)<CR>

" Wrap element
nnoremap <silent> <Plug>sexp_element_wrap_round_head  :<C-u>call sexp#wrap('e', '(', ')', 0, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_round_head  :<C-u>call sexp#wrap('v', '(', ')', 0, g:sexp_wrap_insert)<CR>
nnoremap <silent> <Plug>sexp_element_wrap_round_tail  :<C-u>call sexp#wrap('e', '(', ')', 1, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_round_tail  :<C-u>call sexp#wrap('v', '(', ')', 1, g:sexp_wrap_insert)<CR>
nnoremap <silent> <Plug>sexp_element_wrap_square_head :<C-u>call sexp#wrap('e', '[', ']', 0, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_square_head :<C-u>call sexp#wrap('v', '[', ']', 0, g:sexp_wrap_insert)<CR>
nnoremap <silent> <Plug>sexp_element_wrap_square_tail :<C-u>call sexp#wrap('e', '[', ']', 1, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_square_tail :<C-u>call sexp#wrap('v', '[', ']', 1, g:sexp_wrap_insert)<CR>
nnoremap <silent> <Plug>sexp_element_wrap_curly_head  :<C-u>call sexp#wrap('e', '{', '}', 0, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_curly_head  :<C-u>call sexp#wrap('v', '{', '}', 0, g:sexp_wrap_insert)<CR>
nnoremap <silent> <Plug>sexp_element_wrap_curly_tail  :<C-u>call sexp#wrap('e', '{', '}', 1, g:sexp_wrap_insert)<CR>
vnoremap <silent> <Plug>sexp_element_wrap_curly_tail  :<C-u>call sexp#wrap('v', '{', '}', 1, g:sexp_wrap_insert)<CR>

" Lift form
nmap <silent> <Plug>sexp_lift_form d<Plug>sexp_textobj_outer_formv<Plug>sexp_textobj_outer_formp
vmap <silent> <Plug>sexp_lift_form dv<Plug>sexp_textobj_outer_formp

" Splice form
nnoremap <silent> <Plug>sexp_splice_form :<C-u>call sexp#splice_form()<CR>
vnoremap <silent> <Plug>sexp_splice_form :<C-u>call sexp#splice_form()<CR>

" Insert at form terminal
nnoremap <silent> <Plug>sexp_insert_at_form_head :<C-u>call sexp#insert_at_form_terminal(0)<CR>
vnoremap <silent> <Plug>sexp_insert_at_form_head :<C-u>call sexp#insert_at_form_terminal(0)<CR>
nnoremap <silent> <Plug>sexp_insert_at_form_tail :<C-u>call sexp#insert_at_form_terminal(1)<CR>
vnoremap <silent> <Plug>sexp_insert_at_form_tail :<C-u>call sexp#insert_at_form_terminal(1)<CR>

" Exchange form
nnoremap <silent> <Plug>sexp_swap_form_backward :<C-u>call sexp#swap_element('n', 0, 1)<CR>
vnoremap <silent> <Plug>sexp_swap_form_backward <C-Bslash><C-n>:<C-u>call sexp#swap_element('v', 0, 1)<CR>
nnoremap <silent> <Plug>sexp_swap_form_forward  :<C-u>call sexp#swap_element('n', 1, 1)<CR>
vnoremap <silent> <Plug>sexp_swap_form_forward  <C-Bslash><C-n>:<C-u>call sexp#swap_element('v', 1, 1)<CR>

" Exchange element
nnoremap <silent> <Plug>sexp_swap_element_backward :<C-u>call sexp#swap_element('n', 0, 0)<CR>
vnoremap <silent> <Plug>sexp_swap_element_backward <C-Bslash><C-n>:<C-u>call sexp#swap_element('v', 0, 0)<CR>
nnoremap <silent> <Plug>sexp_swap_element_forward  :<C-u>call sexp#swap_element('n', 1, 0)<CR>
vnoremap <silent> <Plug>sexp_swap_element_forward  <C-Bslash><C-n>:<C-u>call sexp#swap_element('v', 1, 0)<CR>

" Emit / capture element
nnoremap <silent> <Plug>sexp_capture_prev_element :<C-u>call sexp#docount("sexp#stackop('n', 0, 1)", v:count)<CR>
vnoremap <silent> <Plug>sexp_capture_prev_element :<C-u>call sexp#docount("sexp#stackop('v', 0, 1)", v:count)<CR>
nnoremap <silent> <Plug>sexp_capture_next_element :<C-u>call sexp#docount("sexp#stackop('n', 1, 1)", v:count)<CR>
vnoremap <silent> <Plug>sexp_capture_next_element :<C-u>call sexp#docount("sexp#stackop('v', 1, 1)", v:count)<CR>
nnoremap <silent> <Plug>sexp_emit_first_element   :<C-u>call sexp#docount("sexp#stackop('n', 0, 0)", v:count)<CR>
vnoremap <silent> <Plug>sexp_emit_first_element   :<C-u>call sexp#docount("sexp#stackop('v', 0, 0)", v:count)<CR>
nnoremap <silent> <Plug>sexp_emit_last_element    :<C-u>call sexp#docount("sexp#stackop('n', 1, 0)", v:count)<CR>
vnoremap <silent> <Plug>sexp_emit_last_element    :<C-u>call sexp#docount("sexp#stackop('v', 1, 0)", v:count)<CR>

if !empty(g:sexp_mappings)
    for s:plug in keys(g:sexp_mappings)
        if !empty(g:sexp_mappings[s:plug])
            call s:filetype_autocmd(
                \ 'nmap <silent><buffer> ' . g:sexp_mappings[s:plug] . ' <Plug>' . s:plug,
                \ 'vmap <silent><buffer> ' . g:sexp_mappings[s:plug] . ' <Plug>' . s:plug)
        endif
    endfor
endif

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
