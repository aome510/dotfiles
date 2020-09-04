# #
# # ~/.bashrc
# #

# export GOPRIVATE=github.com/curvegrid/*

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

if [[ $DISPLAY ]]; then
    [[ -z "$TMUX" ]] && exec tmux
fi

exec fish
