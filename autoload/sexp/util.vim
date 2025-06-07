
" Return true iff inclusive SexpPos range contains non-whitespace.
fu! sexp#util#non_empty(beg, end)
    let save_cursor = getcurpos()
    call setpos('.', a:beg)
    let pos = searchpos('\S', 'ncz', a:end[1])
    let ret = pos[0] > 0 && pos[1] <= a:end[2]
    call setpos('.', save_cursor)
    return ret
endfu

" Return true iff specified SexpPos is on whitespace (or blank if allow_blank).
fu! sexp#util#in_whitespace(pos, allow_blank)
    local save_cursor = getcurpos()
    call setpos('.', a:pos)
    let re = a:allow_blank ? '\v\s|^$' : '\s'
    let pos = searchpos(re, 'ncz', pos[1])
    let ret = pos == save_cursor[1:2]
    call setpos('.', save_cursor)
    return ret
endfu

let s:MAX_CHARLEN = 8 " actually, 4 for utf-8, but no reason to cut it close.
" Return number of bytes in char at specified VimPos4.
fu! sexp#util#char_bytes(p)
    let c = nvim_buf_get_text(0, a:p[1]-1, a:p[2]-1, a:p[1]-1, a:p[2]-1 + s:MAX_CHARLEN, {})[0]
    let [cidx, n] = [0, 0]
    while !cidx
        let n += 1
        let cidx = charidx(c, n)
    endwhile
    return n
endfu

" vim:ts=4:sw=4:et
