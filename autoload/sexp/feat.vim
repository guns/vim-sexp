" This dict supports data-driven approach to notifying user of new(-ish) features he may
" not be aware of.
" The notification is triggered by use of a builtin operator more or less related to the
" feature (e.g., `p` and friends for the "smart paste" feature), but the notification is
" inhibited if the user has set *any* vim-sexp options related to the feature, thereby
" indicating feature awareness
" Important Note: The test for user awareness occurs *once only* at plugin load, not on
" each buffer load.
" Rationale: The plugin currently has no way to distinguish between user and default
" assignments once the plugin has loaded.
" TODO: Consider keeping the defaults and user settings distinct, even after plugin load.
" This would probably entail adding s:sexp_{opt} counterparts for each global option and
" adding a query layer between the options and the client code that checks them, but it
" would permit certain user configuration changes to take effect later.
let s:feat_notify = {
    \ 'regput': {
        \ 'builtins': {
            \ 'nx': ['p', 'P', '[p', '[P', ']p', ']P', 'gp', 'gP'],
        \ },
        \ 'notify': 0,
        \ 'opts': [
            \ 'regput_override_builtins',
            \ 'regput_bracket_is_target',
            \ 'regput_bracket_is_child',
            \ 'regput_allow_comment_append',
            \ 'regput_untrimmed_is_linewise',
            \ 'regput_linewise_forces_multiline',
            \ 'regput_ignore_list_shape',
        \ ],
        \ 'msg':
            \   "Vim-sexp now provides smarter, Lisp-aware versions of `register put'"
            \ . " commands. You may wish to remap the builtin put operators to use them."
            \ . " For more information, :help sexp-smart-paste. To silence this warning in"
            \ . " future sessions, set any of the options pertaining to this feature"
            \ . " (even to its default value): e.g.,"
            \ . " let g:sexp_regput_bracket_is_target = 1"
    \ },
\ }

" Return 1 iff the user appears not to be aware of the specified feature.
function! s:notification_needed(feat)
    " Allow caller to provide either the feature name or the cfg object.
    let cfg = type(a:feat) == type("") ? s:feat_notify[a:feat] : a:feat
    " Has user set any options related to the feature?
    for opt in cfg.opts
        if exists('g:sexp_' . opt)
            return 0
        endif
    endfor
    " User hasn't set any relevant options.
    return 1
endfunction

" Present feature notification to user, then delete the all the mappings associated with
" this feature (not just the one that triggered this call) to ensure we don't notify about
" this feature again this session.
function! s:do_notification(feat)
    call sexp#warn#msg(s:feat_notify[a:feat].msg)
    call s:destroy_notification(a:feat)
endfunction

" Delete all the mappings installed to trigger notifications for the specified feature.
function! s:destroy_notification(feat)
    let cfg = s:feat_notify[a:feat]
    if cfg.notify
        for [modes, lhss] in items(cfg.builtins)
            for mode in split(modes, '\zs')
                for lhs in lhss
                    exe mode . "unmap <buffer> " . lhs
                endfor
            endfor
        endfor
    endif
endfunction

" For all features represented in s:feat_notify{}, set flag indicating whether user
" requires notification.
" Logic: Notification required iff user hasn't set *any* related options.
" Pre-Condition: Must be invoked *before* any global options are set to default values.
function! sexp#feat#record_user_awareness()
    for [feat, cfg] in items(s:feat_notify)
        let cfg.notify = s:notification_needed(cfg)
    endfor
endfunction


" For each feature represented in s:feat_notify{}, remap all associated builtin operators
" to do the following:
"   * perform requested operation
"   * notify user of new feature
"   * remove all mappings in the set (not just the one that triggered this call)
function! sexp#feat#create_notifications()
    for [feat, cfg] in items(s:feat_notify)
        " Assumption: 'notify' flag was set by record_user_awareness() prior to startup
        " option processing.
        if cfg.notify
            " Create a mapping for each relevant builtin...
            for [modes, lhss] in items(cfg.builtins)
                " ...and for each mode.
                for mode in split(modes, '\zs')
                    " ...and for each lhs
                    for lhs in lhss
                        " Note: We're still supporting Vim versions that don't provide
                        " <Cmd>, so use `:<C-U>', which should be safe for all
                        " versions/modes.
                        exe mode . "noremap <buffer> " . lhs . " " . lhs
                            \ . " :<C-U>call <SID>do_notification('" . feat . "')<cr>"
                    endfor
                endfor
            endfor
        endif
    endfor
endfunction

" vim:ts=4:sw=4:et:tw=90
