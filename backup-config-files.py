import os
from pathlib import Path

home = str(Path.home())

home_white_list = [
    ".bash_profile", ".bashrc", ".spacemacs", ".tmux.conf", ".vimrc",
    ".xbindkeysrc", ".xkbcomp", ".tmux", ".ssh", ".pki", ".gnupu"
]

dot_config_ignore_list = [
    "chromium",
    "discord",
    "discordcanary",
    "gtk-3.0",
    "keybase",
    "Keybase",
    "mimeapps.list",
    "nautilus",
    "nemo",
    "pavucontrol.ini",
    "pulse",
    "spotify",
    "yarn",
]

cwd = os.path.dirname(__file__)

for f in os.listdir(home):
  if f in home_white_list:
    os.system('cp -r ~/{} {}'.format(f, cwd))

os.system('mkdir -p .config')
for f in os.listdir('{}/.config'.format(home)):
  if f not in dot_config_ignore_list:
    os.system('cp -r ~/.config/{} {}/.config'.format(f, cwd))
