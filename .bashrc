# #
# # ~/.bashrc
# #

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# run tmux in normal shell (fish included),
run fish in emacs shell.
if [[ $DISPLAY ]] && ! [[ $INSIDE_EMACS ]]; then
    [[ -z "$TMUX" ]] && (
    UNATTACHED_SESSION=$(tmux ls 2>/dev/null | grep "(attached)$" -v | tail -n 1)
    tmux attach -t ${UNATTACHED_SESSION:0:1} 2>/dev/null || tmux
    )
else
    exec fish
fi
