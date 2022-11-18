export UFPP_DOCUMENT_DIR=/home/urbanjs/V600/github/M_commandline
mkdir -p /home/urbanjs/V600/github/M_commandline/doc/PRIVATE
ufpp -cstyle doxygen -i *.ff >M_commandline.f90
cd doc

for NAME in M_commandline commandline print_dictionary
do
   txt2man $NAME.3m_commandline.man $NAME.3|man2html >$NAME.3.html
done

tidy_html *.html
