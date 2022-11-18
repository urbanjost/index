
git commit -m "Something terribly misguided" # (0: Your Accident)
git reset HEAD~
echo '[ edit files as necessary ]'
git add .
git commit -c ORIG_HEAD
