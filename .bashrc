# #
# # ~/.bashrc
# #

# export GOPRIVATE=github.com/curvegrid/*

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# run tmux in normal shell (fish included),
# run fish in emacs shell.
if [[ $DISPLAY ]] && ! [[ $INSIDE_EMACS ]]; then
    [[ -z "$TMUX" ]] && exec tmux
else
    exec fish
fi
