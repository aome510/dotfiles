# #
# # ~/.bashrc
# #

# export GOPRIVATE=github.com/curvegrid/*

export EDITOR=kak
export VISUAL=kak

export PYTHONPATH="$HOME/.local/lib/python/site-packages:$PYTHONPATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# run tmux in normal shell (fish included),
# run fish in emacs shell.
if [[ $DISPLAY ]] && ! [[ $INSIDE_EMACS ]]; then
    [[ -z "$TMUX" ]] && exec tmux
else
    exec fish
fi
