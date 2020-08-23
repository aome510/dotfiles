set -U fish_greeting
fish_vi_key_bindings

# conda init for fish
source ~/.config/fish/conda.fish

# aliases
alias kfzf="kak (fzf)"
alias skak="sudo -e kak"
function disk_usage
    command du -d1 -h $argv | sort -rh
end
