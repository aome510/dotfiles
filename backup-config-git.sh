#!/bin/sh

./backup-config-files.py
git add .
git commit -m "$(date +"%D %T")"
git push
