#!/bin/bash
# Make sure your repository is up to date first using
git pull origin master
# create branch from master
git checkout -b urbanjost
git push origin urbanjost
git add registry.toml
git commit -m 'add to registry from @urbanjost'
git push --set-upstream origin urbanjost
exit



	Creating a Pull Request

	To create a pull request, you must have changes committed to the your new branch.
	Go to the repository page on github. And click on "Pull Request" button in the repo header.
	Pull Request Button

	Pick the branch you wish to have merged using the "Head branch" dropdown. You should leave the rest of the fields as is, unless you are working from a remote branch. In that case, just make sure that the base repo and base branch are set correctly.

	Head Branch Dropdown

	Enter a title and description for your pull request. Remember you can use Github Flavored Markdown in the description and comments

	Title and Description

	Finally, click on the green "Send pull request" button to finish creating the pull request.

	Send Pull Request

	You should now see an open pull request.

	Open Pull Request
	Using a Pull Request

	You can write comments related to a pull request,

	Writing a comment

	view all the commits by all contained by a pull request under the commits tab,

	Commits tab

	or see all the file changes from the pull request across all the commits under the "Files Changed" tab.

	Files Changed

	You can event leave a comment on particular lines in the code change simply by hovering to the left of a line and clicking on the blue note icon.

	Comment in line
	Merging a Pull Request

	Once you and your collaborators are happy with the changes, you start to merge the changes back to master. There are a few ways to do this.

	First, you can use github's "Merge pull request" button at the bottom of your pull request to merge your changes. This is only available when github can detect that there will be no merge conflicts with the base branch. If all goes well, you just have to add a commit message and click on "Confirm Merge" to merge the changes.

	Merge pull request buttonConfirm Merge
	Merging Locally

	If the pull request cannot be merged online due to merge conflicts, or you wish to test things locally before sending the merge to the repo on Github, you can perform the merge locally instead.

	You can find the instruction to do so by clicking the (i) icon on the merge bar.

	Merging Instructions

	However, there's an alternative that may be better for long standing branches.
	Squash, Rebase, and Cherry Pick

	In long standing branches, merging can often cause lots problems when updating if changes in a given branch conflict with changes recently merged into the master branch. If there are many commits to the same file, git merge may force you to fix the same merge conflicts over and over again, causing a real headache. While there are many ways to mitigate this issue, such as enabling git rerere to reuse recorded resolution of conflict merges, squashing a series of related changes into 1 commit and cherry-picking it into the master is a great solution, especially for topic branches and isolated features.

	There are several advantages of performing merges this way. First, you only have to deal with merge conflicts once, since all commits are compressed into 1. Second, each commit represents an entire set of changes required for a feature or task, which makes it easy to pin point bugs and other problems when they arise and to remove a change set when it's no longer necessary.

	There are also disadvantages of squashing commits. First, you will lose the details and information for each change, as all changes squashed are compressed together. So the net effect is the same. Second, it can be dangerous and problematic if used incorrectly, such as squashing commits that have been pushed to the remote server and others depend on for their work. Because squashing is changing the git history, you can cause many conflicts that way. However, if you are using this locally or you are the only person working on your branch and you know exactly what you are doing.

	To perform this, use the following command

	git rebase -i HEAD~10

	-i stands for interactive mode and HEAD~10 means to examine the 10 latest commits.

	If you see an fatal: Needed a single revision error, this usually means that there are not that many commits left. Try a lower number.

	This will open up an editor with git commit messages that looks something like this:

	Git Rebase

	There are many options available at this stage. These are detailed in this github help page. Here, I'm going to simply squash all changes in this pull request into one. Save and close the editor.

	Squashing commits

	The next screen will pop up asking you to edit your commit messages. You can choose to edit them or simply continue. Save and close the editor.

	Squash Result

	Once your squash completes, you can push it to the remote repo. In this case, these squashed commits have been pushed to the server. However, I'm the only user of this branch and can safely force push the commit to update the git repo.

	git push origin pull-request-demo -f

	To merge the commit, we will use git cherry-pick.

	Cherry Picking

	You are done! Github should detect the changes and update the pull request. You can then marked the pull request as merged and optionally delete the branch.
	Closing a Pull Request

	You can simply click on the "Close" button on the pull request to close it. Optionally, you can delete the branch directly using the "Delete this branch" button.

	Closing a Pull Request
