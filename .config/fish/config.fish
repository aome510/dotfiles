set -U fish_greeting

function fish_title
  true
end

# use vim key bindings for normal terminal,
# use fish default key bindings for fish in emacs.
if not test -n "$INSIDE_EMACS"
    fish_vi_key_bindings

    # user-defined keybindings
    bind --user 'gh' beginning-of-line
    bind --user -M visual 'gh' beginning-of-line
    
    bind --user 'gl' end-of-line
    bind --user -M visual 'gl' end-of-line
    
    bind --user \cH backward-kill-word
    bind --user -M insert \cH backward-kill-word
    
else
    fish_default_key_bindings
end

# conda init for fish
# source ~/.config/fish/conda.fish

function disk_usage
    command sudo du -d1 -h $argv | sort -h
end

function swap
    set tmpfile (mktemp)
    command mv $argv[1] $tmpfile && mv $argv[2] $argv[1] && mv $tmpfile $argv[2]
end

function kfzf
    if test -n "$argv"
        command kak (fd $fzf_fd_opts --search-path $argv | fzf)
    else
        command kak (fd $fzf_fd_opts | fzf)
    end
end

# allow fzf.fish to search for hidden files
set fzf_fd_opts --hidden -E .git -E node_modules -E .cache -E build -E .ccls-cache -t f

alias ksudo="sudo -e"
