#How to create a pull request in GitHub | Opensource.com
#Learn how to fork a repo, make changes, and ask the maintainers to review and merge it.
#So, you know how to use git. You have a GitHub repo and can push to
#it. All is well. But how the heck do you contribute to other people's
#GitHub projects? 
#I will explain how to fork a git repo, make changes, and submit a pull request.

#When you want to work on a GitHub project, the first step is to fork a repo.

#Once there, click on the Fork button in the top-right corner. This
#creates a new copy of my demo repo under your GitHub user account with
#a URL like:

#https://github.com/<YourUserName>/demo

#The copy includes all the code, branches, and commits from the original repo.

#Next, clone the repo by opening the terminal on your computer and running the command:

git clone https://github.com/urbanjost/fpm-registry

#Once the repo is cloned, you need to do two things:

#    Create a new branch by issuing the command:

cd fpm-registry
git checkout -b $USER

#    Create a new remote for the upstream repo with the command:

git remote add upstream https://github.com/fortran-lang/fpm-registry

#In this case, "upstream repo" refers to the original repo you created your fork from.

#Now you can make changes to the code. The following code creates a new
#branch, makes an arbitrary change, and pushes it to new_branch:

git checkout -b $USER
git status
vi registry.toml
############################################
# VERIFY
python3 -m venv venv
source venv/bin/activate
pip install -U pip
pip install -r requirements.txt

#To validate registry.toml, run:

python load_registry.py
# VERIFY
############################################

git add registry.toml
git commit -S -m "Adding to registry.toml"
git push -u origin $USER
#Once you push the changes to your repo, the Compare & pull request button will appear in GitHub.

#compare-and-pull-request-button.png
#GitHub's Compare & Pull Request button

#Click it and you'll be taken to this screen:

#open-a-pull-request_crop.png
#GitHub's Open pull request button

#Open a pull request by clicking the Create pull request button. This
#allows the repo's maintainers to review your contribution. From here,
#they can merge it if it is good, or they may ask you to make some changes.

#In summary, if you want to contribute to a project, the simplest way is to:

#    create a project you want to contribute
#    Fork it
#    Clone it to your local system
#    Make a new branch
#    Make your changes
#    Push it back to your repo
#    Click the Compare & pull request button
#    Click Create pull request to open a new pull request

#If the reviewers ask for changes, repeat steps 5 and 6 to add more commits to your pull request.

