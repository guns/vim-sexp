
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

if exists('g:__sexp_loaded__')
    finish
endif
let g:__sexp_loaded__ = 1

""" Default options {{{1

if !exists('g:sexp_filetypes')
    let g:sexp_filetypes = 'clojure,scheme,lisp'
endif

if !exists('g:sexp_wrap_insert')
    let g:sexp_wrap_insert = 1
endif

if !exists('g:sexp_textobj_mapping')
    let g:sexp_textobj_mapping = 'f'
endif

if !exists('g:sexp_mappings')
    " FIXME: Set to LocalLeader before release!
    let g:sexp_mappings = {
        \ 'sexp_form_wrap_round_head':  '<Leader>i',
        \ 'sexp_form_wrap_round_tail':  '<Leader>I',
        \ 'sexp_form_wrap_square_head': '<Leader>[',
        \ 'sexp_form_wrap_square_tail': '<Leader>]',
        \ 'sexp_form_wrap_curly_head':  '<Leader>{',
        \ 'sexp_form_wrap_curly_tail':  '<Leader>}',
        \ 'sexp_word_wrap_round_head':  '<Leader>W',
        \ 'sexp_word_wrap_round_tail':  '<Leader>w',
        \ 'sexp_word_wrap_square_head': '',
        \ 'sexp_word_wrap_square_tail': '',
        \ 'sexp_word_wrap_curly_head':  '',
        \ 'sexp_word_wrap_curly_tail':  '',
        \ 'sexp_raise_form':            '<Leader>o',
        \ 'sexp_splice_form':           '<Leader>O',
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

""" Textobj mappings {{{1

vnoremap <silent> <Plug>sexp_textobj_outer_form :<C-u>call sexp#set_bracket_marks(0) \| normal! gv<CR>
omap     <silent> <Plug>sexp_textobj_outer_form :<C-u>execute "normal v\<Plug>sexp_textobj_outer_form"<CR>
vnoremap <silent> <Plug>sexp_textobj_inner_form :<C-u>call sexp#set_bracket_marks(1) \| normal! gv<CR>
omap     <silent> <Plug>sexp_textobj_inner_form :<C-u>execute "normal v\<Plug>sexp_textobj_inner_form"<CR>

if !empty(g:sexp_textobj_mapping)
    call s:filetype_autocmd(
        \ 'vmap <silent><buffer> a' . g:sexp_textobj_mapping . ' <Plug>sexp_textobj_outer_form',
        \ 'omap <silent><buffer> a' . g:sexp_textobj_mapping . ' <Plug>sexp_textobj_outer_form',
        \ 'vmap <silent><buffer> i' . g:sexp_textobj_mapping . ' <Plug>sexp_textobj_inner_form',
        \ 'omap <silent><buffer> i' . g:sexp_textobj_mapping . ' <Plug>sexp_textobj_inner_form')
endif

""" Sexp mappings {{{1

" These definitions could be created in a loop, but we'll keep them in long
" form for easy grepping.

" Wrap form
nnoremap <silent> <Plug>sexp_form_wrap_round_head  :<C-u>call sexp#wrap('f', '(', ')', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_form_wrap_round_head  :<C-u>call sexp#wrap('v', '(', ')', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
nnoremap <silent> <Plug>sexp_form_wrap_round_tail  :<C-u>call sexp#wrap('f', '(', ')', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_form_wrap_round_tail  :<C-u>call sexp#wrap('v', '(', ')', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
nnoremap <silent> <Plug>sexp_form_wrap_square_head :<C-u>call sexp#wrap('f', '[', ']', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_form_wrap_square_head :<C-u>call sexp#wrap('v', '[', ']', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
nnoremap <silent> <Plug>sexp_form_wrap_square_tail :<C-u>call sexp#wrap('f', '[', ']', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_form_wrap_square_tail :<C-u>call sexp#wrap('v', '[', ']', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
nnoremap <silent> <Plug>sexp_form_wrap_curly_head  :<C-u>call sexp#wrap('f', '{', '}', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_form_wrap_curly_head  :<C-u>call sexp#wrap('v', '{', '}', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
nnoremap <silent> <Plug>sexp_form_wrap_curly_tail  :<C-u>call sexp#wrap('f', '{', '}', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_form_wrap_curly_tail  :<C-u>call sexp#wrap('v', '{', '}', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>

" Wrap word
nnoremap <silent> <Plug>sexp_word_wrap_round_head  :<C-u>call sexp#wrap('w', '(', ')', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_word_wrap_round_head  :<C-u>call sexp#wrap('v', '(', ')', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
nnoremap <silent> <Plug>sexp_word_wrap_round_tail  :<C-u>call sexp#wrap('w', '(', ')', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_word_wrap_round_tail  :<C-u>call sexp#wrap('v', '(', ')', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
nnoremap <silent> <Plug>sexp_word_wrap_square_head :<C-u>call sexp#wrap('w', '[', ']', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_word_wrap_square_head :<C-u>call sexp#wrap('v', '[', ']', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
nnoremap <silent> <Plug>sexp_word_wrap_square_tail :<C-u>call sexp#wrap('w', '[', ']', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_word_wrap_square_tail :<C-u>call sexp#wrap('v', '[', ']', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
nnoremap <silent> <Plug>sexp_word_wrap_curly_head  :<C-u>call sexp#wrap('w', '{', '}', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_word_wrap_curly_head  :<C-u>call sexp#wrap('v', '{', '}', 1) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
nnoremap <silent> <Plug>sexp_word_wrap_curly_tail  :<C-u>call sexp#wrap('w', '{', '}', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>
vnoremap <silent> <Plug>sexp_word_wrap_curly_tail  :<C-u>call sexp#wrap('v', '{', '}', 0) \| if g:sexp_wrap_insert \| startinsert \| endif<CR>

" Raise form
nmap <silent> <Plug>sexp_raise_form d<Plug>sexp_textobj_outer_formv<Plug>sexp_textobj_outer_formp
vmap <silent> <Plug>sexp_raise_form dv<Plug>sexp_textobj_outer_formp

" Splice form
nnoremap <silent> <Plug>sexp_splice_form :<C-u>call sexp#splice_form()<CR>
vnoremap <silent> <Plug>sexp_splice_form :<C-u>call sexp#splice_form()<CR>

if !empty(g:sexp_mappings)
    for s:plug in keys(g:sexp_mappings)
        if !empty(g:sexp_mappings[s:plug])
            call s:filetype_autocmd(
                \ 'nmap <silent><buffer> ' . g:sexp_mappings[s:plug] . ' <Plug>' . s:plug,
                \ 'vmap <silent><buffer> ' . g:sexp_mappings[s:plug] . ' <Plug>' . s:plug)
        endif
    endfor
endif
