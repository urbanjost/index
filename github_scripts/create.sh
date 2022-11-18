#!/bin/bash
#@(#) create a new repository on the command line

echo "# M_draw" >> README.md
git init
git add README.md
git commit -m "first commit"
git remote add origin https://github.com/urbanjost/M_draw.git
git push -u origin master
