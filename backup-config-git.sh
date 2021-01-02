#!/bin/sh
HOME=/home/aome510
echo $HOME
cd $HOME/Backup/my-config-files/
./backup-config-files.py
git add .
git commit --allow-empty -m "backup configs $(date +"%D %T")"
git push
