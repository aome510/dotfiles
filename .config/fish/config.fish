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

function kak-fzf
    if test -d "$argv"
        kak (fd $fd_opts -t f --search-path $argv | fzf $fzf_otps)
    else
        kak (fd $fd_opts -t f | fzf $fzf_otps)
    end
    commandline -f repaint
end

function history-fzf
    history merge
    history -z | fzf $fzf_otps --read0 --print0 --tiebreak index -q (commandline) | read result -z
    commandline -r -- $result
    commandline -f repaint
end

function file-fzf
    set query_dir ""
    set current_cmd commandline
    
    if not string match -r '\s+$' $current_cmd
        set query_dir (commandline | awk '{print $NF}')
    end
    
    if test -d $query_dir
        fd $fd_opts --search-path $query_dir | fzf $fzf_otps | read result
    else 
        fd $fd_opts | fzf $fzf_otps | read result
    end
    
    set result (string escape $result)
    commandline -rt -- $result
    commandline -f repaint
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
    
set fd_opts --hidden -E .git -E node_modules -E .cache -E build -E .ccls-cache
set fzf_otps --reverse --height 50%

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
bind --user -M default -M insert \cr history-fzf
bind --user -M default -M insert \ef file-fzf
bind --user -M default -M insert \cf forward-char
bind --user -M default -M insert \cn down-or-search
bind --user -M default -M insert \cp up-or-search
