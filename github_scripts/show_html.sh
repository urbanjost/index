#!/bin/bash
exit

Displaying HTML Files in GitHub

Setting up an HTML page in GitHub is not difficult but it is a bit lengthy. Just follow these steps.

Create the HTML from your RMarkdown document and save to your local directory.
Create a new repo, or go to an existing repo, in your GitHub.
In the repo, create a new file called docs/index.html.
In the index.html file, type this code: <html><body><p>Hello World</p></body></html>
Commit the file.
In the repo, click on the Settings tab.
Scroll down to GitHub Pages Section.
Under “Source”, choose “master branch/docs folder” and save. If that doesn’t work, try the “master branch” and save.
A message will appear that your site is read to be published. Click on the link.
When Git finds the index.html file in a docs/ folder, it creates a web address for the contents. The address takes the form: http://username.github.io/repository
Go to that address. You should see your “Hello World” greeting. That is your index.html file.
Go back to the docs/ folder and open it.
Upload the HTML file you created in RMarkdown and commit.
Return to your github.io site but add the HTML file name to the address: http://username.github.io/repository/filename.html.
You should find your HTML posted there.
