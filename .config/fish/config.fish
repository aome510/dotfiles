set -U fish_greeting

function fish_title
  true
end

# zoxide init for fish
source ~/.config/fish/zoxide.fish

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

# setup pyenv
# status is-interactive; and pyenv init - | source
# status is-interactive; and pyenv virtualenv-init - | source

# set up fzf.fish variables
set fzf_history_opts --preview-window="bottom:5:wrap"

# aliases
alias e="kak"
alias se="sudo -e"
alias cl="emacsclient -nw"
alias l="exa --color always"
alias b="btm --basic --tree"

# git-specific aliases
alias ga="git add"
alias gs="git status"
alias gd="git diff"
alias gc="git commit"
alias gC="git checkout"
