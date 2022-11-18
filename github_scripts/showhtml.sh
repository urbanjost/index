#!/bin/bash
cd general-purpose-fortran
git checkout --orphan gh-pages
git push -u origin gh-pages
cygstart https://help.github.com/articles/creating-project-pages-manually
exit


It's really easy to do with github pages, it's just a bit weird the
first time you do it. Sorta like the first time you had to juggle 3
kittens while learning to knit. (OK, it's not all that bad)

You need a gh-pages branch:

Basically github.com looks for a gh-pages branch of the repository. It
will serve all HTML pages it finds in here as normal HTML directly to
the browser.

How do I get this gh-pages branch?

Easy. Just create a branch of your github repo called gh-branches. Specify
--orphan when you create this branch, as you don't actually want to
merge this branch back into your github branch, you just want a branch
that contains your HTML resources.

   git checkout --orphan gh-pages

What about all the other gunk in my repo, how does that fit in to it?

Nah, you can just go ahead and delete it. And it's safe to do now,
because you've been paying attention and created an orphan branch which
can't be merged back into your main branch and remove all your code.

I've created the branch, now what?

You need to push this branch up to github.com, so that their automation
can kick in and start hosting these pages for you.

   git push -u origin gh-pages

But.. My HTML is still not being served!

It takes a few minutes for github to index these branches and fire up
the required infrastructure to serve up the content. Up to 10 minutes
according to github.

The steps layed out by github.com

   https://help.github.com/articles/creating-project-pages-manually
