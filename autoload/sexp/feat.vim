" This dict supports a data-driven approach to notifying user of new(-ish) features he may
" not be aware of.
" -- Keys --
" aware:    set to 1 to indicate user awareness
" opts:     names of options whose override by user indicates feature awareness
" cmds:     list of plug maps provided by the feature
" builtins: dictionary of builtins overridden by the feature:
"             key=builtin
"             val=modes
"           Note: The modes are required because, in theory at least, two distinct
"           features could override the same builtin in different modes.
" msg:      message to display once-per-session when conditions for notification are met
"
" The logic for this feature exists in two distinct phases:
" 1. Plugin Load: For each notifiable feature, look at both options and g:sexp_mappings[],
"    setting the feature-specific 'aware' flag iff the existence of feature-specific
"    overrides demonstrates awareness of the feature's existence.
"    Note: The option checks *must* occur at plugin load time because default option logic
"    sets the global options to default values, with the result that we have no way of
"    knowing after load whether user has overridden the option. This is an implementation
"    detail, which could change in a subsequent revision. In the current implementation,
"    the check for user map overrides *could* be deferred till command execution time, but
"    since g:sexp_mappings[] is processed only in sexp_create_mappings(), there's no
"    reason to defer.
" 2. Command Execution: If the invoked command's plug name corresponds to a feature
"    requiring notification, present the configured notification to user unless the
"    feature-specific 'aware' flag indicates at least one of the following conditions is
"    met:
"      1. User option/map override provides evidence of feature awareness ('aware' flag
"         set).
"      2. We've already notified user about this feature during current session.
"    If feature has a notification and we display it, set its 'aware' flag to inhibit
"    subsequent notifications.
let s:feat_notify = {
    \ 'regput': {
        \ 'aware': 0,
        \ 'opts': [
            \ 'regput_silence_notification',
            \ 'regput_bracket_is_target',
            \ 'regput_bracket_is_child',
            \ 'regput_allow_comment_append',
            \ 'regput_untrimmed_is_linewise',
            \ 'regput_linewise_forces_multiline',
            \ 'regput_ignore_list_shape',
            \ 'regput_fallback_source',
            \ 'regput_fallback_target',
            \ 'regput_curpos', 'regput_curpos_child', 'regput_curpos_op',
            \ 'regput_invalid_register_action',
            \ 'regput_inhibit_regparse',
            \ 'regput_replace_expanded',
            \ 'regput_tele_motion',
            \ 'regput_use_string_parser',
        \ ],
        \ 'cmds': [
            \ 'sexp_put_before', 'sexp_put_after', 'sexp_replace', 'sexp_replace_P',
            \ 'sexp_put_before_op', 'sexp_put_after_op', 'sexp_replace_op', 'sexp_replace_op_P',
            \ 'sexp_put_at_head', 'sexp_put_at_tail',
        \ ],
        \ 'builtins': { 'p': 'xn', 'P': 'xn', 'gp': 'n', 'gP': 'n' },
        \ 'msg': "vim-sexp: You have invoked a \"Smart Paste\" command."
            \ . " (:help sexp-smart-paste)"
    \ },
\ }

" Return feature dict corresponding to plug name iff the feature is subject to
" notifications, else empty dict.
function! s:get_feat_cfg(plug)
    for [feat_name, feat_cfg] in items(s:feat_notify)
        " Does this feature provide this command?
        if index(feat_cfg.cmds, a:plug) >= 0
            return feat_cfg
        endif
    endfor
    " Command doesn't belong to feature requiring notification.
    return {}
endfunction

" Perform new feature notification (and set 'aware' flag to prevent future notifications)
" iff conditions outlined in s:feat_notify[] header comment are met.
function! sexp#feat#notify_maybe(mode, plug)
    " Short-circuit if this command doesn't belong to a feature supporting notification or
    " if user appears to be aware of the feature.
    let cfg = s:get_feat_cfg(a:plug)
    if empty(cfg) || cfg.aware
        return
    endif
    " User may still be unaware of the feature.
    call sexp#warn#msg(cfg.msg)
    " Don't notify again this session.
    " Note: The notification is per-feature, not per command within feature.
    let cfg.aware = 1
endfunction

" Return 1 iff user appears to be aware of the feature whose key/value pair from the
" feature dictionary are provided as inputs.
" Pre Condition: Called at plugin load, *BEFORE* option/map processing.
function! s:is_user_aware(feat, cfg)
    " Has user set any options related to the feature?
    for opt in a:cfg.opts
        if exists('g:sexp_' . opt)
            return 1
        endif
    endfor
    if !has_key(g:, 'sexp_mappings')
        " User hasn't customized *any* maps.
        return 0
    endif
    " Has user customized any plug maps related to the feature?
    " Note: Modes are irrelevant since we're interested only in feature *awareness*.
    for cmd in a:cfg.cmds
        if has_key(g:sexp_mappings, cmd)
            return 1
        endif
    endfor
    " Has user customized any builtins?
    for [builtin, builtin_modes] in items(a:cfg.builtins)
        " Design Decision: Default to v:null rather than (eg) {} or ''.
        " Rationale: User might set a builtin to {} or '' to disable backups (though this
        " is unnecessary, since they're disabled by default).
        " Note: There's a slight ambiguity here, since it's theoretically possible that
        " the same builtin could be used for different plug commands in different modes
        " and a value of {} or '' provides no mode information; however, this scenario
        " should be vanishingly rare, and it should be safe to err on the side of *not*
        " notifying. If we want to err on the side of notifying when in doubt, change the
        " default to {} and the `override isnot v:null' to `!empty(override)'.
        let override = get(g:sexp_mappings, builtin, v:null)
        if override isnot v:null
            " User has overridden the builtin, but we need to make sure at least one of
            " the modes of the override corresponds to one of the feature-specific modes.
            try
                let [parsed_override, _] = 
                    \ sexp#plug#parse_map_entry(builtin, override, builtin_modes)
                for m in split(builtin_modes, '\zs')
                    if has_key(parsed_override, m)
                        " User provided override for builtin in at least one mode.
                        return 1
                    endif
                endfor
            catch /sexp-error/
                " Just continue: warnings should be handled by sexp_create_mappings().
                " Design Decision: Draw no awareness conclusions from invalid override.
                continue
            endtry
        endif
    endfor
    " User seems to be unaware of the feature.
    return 0
endfunction

" For all features represented in s:feat_notify{}, set flag indicating whether user
" requires once-per-session notification upon the first invocation of a command pertaining
" to the feature.
" Logic: Notification required iff user has neither set options nor customized mappings
" related to the feature.
" Pre-Condition: Must be invoked *before* any global options are set to default values.
function! sexp#feat#record_user_awareness()
    for [feat, cfg] in items(s:feat_notify)
        if s:is_user_aware(feat, cfg)
            " Nothing more to be done for this feature.
            let cfg.aware = 1
        endif
    endfor
endfunction

" vim:ts=4:sw=4:et:tw=90
