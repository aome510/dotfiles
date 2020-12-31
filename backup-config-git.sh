#!/bin/sh

./backup-config-files.py
git add .
git commit -m "backup configs $(date +"%D %T")"
git push
