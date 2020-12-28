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
  if os.path.isfile(f):
    if f in home_config_files:
      os.system('ln -sf ~/{} {}'.format(f, f))

os.system('mkdir -p .config')
for f in os.listdir('{}/.config'.format(home)):
  if f not in dot_config_ignore_list:
    if os.path.isfile(f):
      os.system('ln -sf ~/.config/{} .config/{}'.format(f, f))
    else:
      os.system('ln -sdf ~/.config/{} .config'.format(f))
