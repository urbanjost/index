#git checkout urbanjs
#git checkout -b more_issues_for_new urbanjs
# Creates more_issues_for_new branch off urbanjs. Do your work and then
# git commit -am "complete new modifications for #109 #110 #111 #135 #138 #154 #196"
# Now merge your changes to urbanjs without a fast-forward
git checkout urbanjs
git merge --no-ff more_issues_for_new
# Now push changes to the server
git push origin urbanjs
git push origin more_issues_for_new
# And you'll see it how you want it.
git checkout more_issues_for_new
