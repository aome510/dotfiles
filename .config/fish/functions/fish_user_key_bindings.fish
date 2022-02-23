function fish_user_key_bindings
    fzf_configure_bindings --directory=\ef
    
    # other custom bindings
    bind --user -M default 'gh' beginning-of-line
    bind --user -M default 'gl' end-of-line   
    bind --user -M default -M insert \cf forward-char
    bind --user -M default -M insert \cn down-or-search
    bind --user -M default -M insert \cp up-or-search
end
