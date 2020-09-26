# #
# # ~/.bashrc
# #

# export GOPRIVATE=github.com/curvegrid/*

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

S1='[\u@\h \W]\$ '

export EDITOR=kak
export VISUAL=kak

export PYTHONPATH=$(python -c "import site, os; print(os.path.join(site.USER_BASE, 'lib', 'python', 'site-packages'))"):$PYTHONPATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$(yarn global bin):$PATH"

# run tmux in normal shell (fish included),
# run fish in emacs shell.
if [[ $DISPLAY ]] && ! [[ $INSIDE_EMACS ]]; then
    [[ -z "$TMUX" ]] && exec tmux
else
    exec fish
fi
