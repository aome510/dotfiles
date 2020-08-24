#
# ~/.bash_profile
#

PS1='[\u@\h \W]\$ '

export EDITOR=kak
export VISUAL=kak

# add user base to python path
export PYTHONPATH=$(python -c "import site, os; print(os.path.join(site.USER_BASE, 'lib', 'python', 'site-packages'))"):$PYTHONPATH
# add ~/.local/bin to path
export PATH="$HOME/.local/bin:$PATH"

export PATH="$HOME/go/bin:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"

source "$HOME/.bashrc"
