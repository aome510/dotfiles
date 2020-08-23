#
# ~/.bash_profile
#

PS1='[\u@\h \W]\$ '

export EDITOR=kak
export VISUAL=kak

# add user base to python path
export PYTHONPATH=$(python -c "import site, os; print(os.path.join(site.USER_BASE, 'lib', 'python', 'site-packages'))"):$PYTHONPATH
# add ~/.local/bin to path
export PATH="/home/aome510/.local/bin:$PATH"

export PATH="/home/aome510/go/bin:$PATH"

export PATH="/home/aome510/.cargo/bin:$PATH"

source "/home/aome510/.bashrc"
