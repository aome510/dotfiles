#!/bin/python

import os
from pathlib import Path

home = str(Path.home())

home_white_list = [
    ".bash_profile", ".bashrc", ".spacemacs", ".tmux.conf", ".vimrc",
    ".xbindkeysrc", ".xkbcomp", ".tmux", ".ssh", ".pki", ".gnupu", ".gitconfig"
]

dot_config_white_list = [
    "autorandr",
    "autostart",
    "dconf",
    "fish",
    "flake8",
    "flameshot",
    "fontconfig",
    "i3",
    "i3status",
    "kak",
    "kak-lsp",
    "kitty",
    "light",
    "mimeapps.list",
    "picom.conf",
    "yapf",
    "systemd",
]

cwd = os.path.dirname(__file__)

# add git modules first
if os.path.exists('{}/.config/kak/plugins/plug.kak'.format(cwd)):
  os.system(
      'git submodule add https://github.com/robertmeta/plug.kak.git {}/.config/kak/plugins/plug.kak 2>/dev/null'
      .format(cwd))
if os.path.exists('{}/.config/kitty/kitty-themes'.format(cwd)):
  os.system(
      'git submodule add https://github.com/dexpota/kitty-themes {}/.config/kitty/kitty-themes 2>/dev/null'
      .format(cwd))
if os.path.exists('{}/.tmux/plugins/tpm'.format(cwd)):
  os.system(
      'git submodule add https://github.com/tmux-plugins/tpm {}/.tmux/plugins/tmp 2>/dev/null'
      .format(cwd))
if os.path.exists('{}/.tmux/plugins/tmux-themepack'.format(cwd)):
  os.system(
      'git submodule add https://github.com/jimeh/tmux-themepack {}/.tmux/plugins/tmux-themepack 2>/dev/null'
      .format(cwd))

for f in os.listdir(home):
  if f in home_white_list:
    os.system('cp -r ~/{} {}'.format(f, cwd))

os.system('mkdir -p .config')
for f in os.listdir('{}/.config'.format(home)):
  if f in dot_config_white_list:
    os.system('cp -r ~/.config/{} {}/.config'.format(f, cwd))
