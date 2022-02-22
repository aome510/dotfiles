set -U fish_greeting

function fish_title
  true
end

# conda init for fish
# source ~/.config/fish/conda.fish

# zoxide init for fish
# source ~/.config/fish/zoxide.fish

function disk_usage
    du -d1 -h $argv | sort -h
end

function swap
    set tmpfile (mktemp)
    mv $argv[1] $tmpfile && mv $argv[2] $argv[1] && mv $tmpfile $argv[2]
end

function build_pdf_from_md
    set in $argv[1]
    set out $argv[2]
    set watch $argv[3]

    pandoc $in -so $out
    
    if test "$watch" = true
        set hash (sha1sum $in | awk '{print $1}')

        while true
            set tmp (sha1sum $in | awk '{print $1}')
            if test "$hash" != "$tmp"
                pandoc $in -so $out
                set hash $tmp
            end
            sleep 1
        end
    end
end

# aliases
alias e="kak"
alias se="sudo -e"
alias cl="emacsclient -nw"
# alias l="exa --color always"
alias b="btm --basic --tree"

# git-specific aliases
alias gs="git status"
alias gd="git diff"
alias gc="git commit"
alias gC="git checkout"

# use vim key bindings for normal terminal,
# use fish default key bindings for fish in emacs.
if not test -n "$INSIDE_EMACS"
    fish_vi_key_bindings

    # user-defined keybindings
    bind --user -M default 'gh' beginning-of-line
    bind --user -M default 'gl' end-of-line   
else
    fish_default_key_bindings
end

# other custom bindings
bind --user -M default -M insert \cf forward-char
bind --user -M default -M insert \cn down-or-search
bind --user -M default -M insert \cp up-or-search
