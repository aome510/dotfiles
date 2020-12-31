# #
# # ~/.bashrc
# #

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# run tmux in normal shell (fish included),
# run fish in emacs shell.
if [[ $DISPLAY ]] && ! [[ $INSIDE_EMACS ]]; then
    [[ -z "$TMUX" ]] && (tmux attach >/dev/null 2>&1 || tmux)
else
    exec fish
fi
