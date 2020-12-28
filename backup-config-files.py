import os
from pathlib import Path

home = str(Path.home())

home_config_files = [
    ".bash_profile",
    ".bashrc",
    ".spacemacs",
    ".tmux.conf",
    ".vimrc",
    ".xbindkeysrc",
    ".xkbcomp",
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

for f in os.listdir(home):
  if os.path.isfile('{}/{}'.format(home, f)):
    if f in home_config_files:
      os.system('cp ~/{} .'.format(f))

os.system('mkdir -p .config')
for f in os.listdir('{}/.config'.format(home)):
  if f not in dot_config_ignore_list:
    os.system('cp -r ~/.config/{} .config'.format(f))
