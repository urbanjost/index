find faq -name '*.md'|while read NAME
do
   BASE=$(basename $NAME .md)
   md2html $NAME > html/$BASE.html
done
