set -U fish_greeting

function fish_title
  true
end

# use vim key bindings for normal terminal,
# use fish default key bindings for fish in emacs.
if not test -n "$INSIDE_EMACS"
    fish_vi_key_bindings
else
    fish_default_key_bindings
end

# conda init for fish
source ~/.config/fish/conda.fish

# aliases
alias kfzf="kak (fzf)"
alias skak="sudo -e kak"
function disk_usage
    command sudo du -d1 -h $argv | sort -h
end

function swap
    set tmpfile (mktemp)
    command mv $argv[1] $tmpfile && mv $argv[2] $argv[1] && mv $tmpfile $argv[2]
end
