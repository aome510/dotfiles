#!/bin/sh

./backup-config-files.py
cd /home/aome510/Backup/my-config-files/
git add .
git commit -m -allow-"backup configs $(date +"%D %T")"
# git push
