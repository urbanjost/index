#!/bin/bash
####################################################################################################################################
cd $(dirname $0)
mkdir -p src man/man1 man/man3 html
####################################################################################################################################
for NAME in fpm new build run test help 
do
   # write out help text from program, assumed to be in txt2man(1) markdown
   ffpm help $NAME >src/$NAME.1.man
   # convert file to an actual manpage
   txt2man -s fpm -t "$NAME" src/$NAME.1.man >man/man1/$NAME.1fpm
   # convert manpage to html
   man2html <man/man1/$NAME.1fpm >html/$NAME.1fpm.html
   # convert to standard HTML 
   tidy_html html/$NAME.1fpm.html
   # compress the manpage
   gzip -f man/man1/$NAME.1fpm
   chmod a=r,u+w man/man1/$NAME.1fpm.gz 
   (
   set -x
   env MANWIDTH=80 MANPATH=$(pwd)/man man $NAME
   )
done
####################################################################################################################################
mandb -c $(pwd)/man
MANPATH=$(pwd)/man man -k fpm .
####################################################################################################################################
exit
####################################################################################################################################

