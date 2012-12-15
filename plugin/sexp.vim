
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

if exists('g:loaded_vim_sexp')
    finish
endif
let g:loaded_vim_sexp = 1

""" Default options {{{1

let g:sexp_default_options = {
    \ 'filetypes': 'clojure,scheme,lisp',
    \ 'textobj_mappings': ['af', 'if']
    \ }

if !exists('g:sexp_options')
    let g:sexp_options = deepcopy(g:sexp_default_options)
endif

function! s:filetype_autocmd(...)
    if !has_key(g:sexp_options, 'filetypes') | return | endif
    for cmd in a:000
        execute 'autocmd FileType ' . g:sexp_options['filetypes'] . ' ' . cmd
    endfor
endfunction

augroup sexp_autocommands
    autocmd!
augroup END

""" Text object mappings {{{1

if has_key(g:sexp_options, 'textobj_mappings')
    let [s:amap, s:imap] = g:sexp_options['textobj_mappings']

    augroup sexp_autocommands
        call s:filetype_autocmd(
            \ 'vnoremap <silent><buffer> <Plug>select_outer_bracket :<C-u>call sexp#select_bracket(0)<CR>',
            \ 'vnoremap <silent><buffer> <Plug>select_inner_bracket :<C-u>call sexp#select_bracket(1)<CR>',
            \ 'vmap <silent><buffer> ' . s:amap . ' <Plug>select_outer_bracket',
            \ 'vmap <silent><buffer> ' . s:imap . ' <Plug>select_inner_bracket',
            \ 'omap <silent><buffer> ' . s:amap . ' :normal v ' . s:amap . '<CR>',
            \ 'omap <silent><buffer> ' . s:imap . ' :normal v ' . s:imap . '<CR>'
            \ )
    augroup END
endif
