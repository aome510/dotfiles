#!/bin/sh

cd /home/aome510/Backup/my-config-files/
./backup-config-files.py
git add .
git commit --allow-empty -m "backup configs $(date +"%D %T")"
git push
