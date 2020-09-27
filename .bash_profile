#
# ~/.bash_profile
#

S1='[\u@\h \W]\$ '

export EDITOR=kak
export VISUAL=kak

export PYTHONPATH=$(python -c "import site, os; print(os.path.join(site.USER_BASE, 'lib', 'python', 'site-packages'))"):$PYTHONPATH
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$(yarn global bin):$PATH"

[[ -r ~/.bashrc ]] && source ~/.bashrc
