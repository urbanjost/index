#!/bin/bash
git clone https://github.com/urbanjost/M_process.git
cd M_process
mkdir md docs
cd md
for NAME in M_process process_close process_open_read process_open_write process_readline process_writeline process_readall
do
   cp $HOME/LIBRARY/libGPF/download/tmp/html/$NAME.3.html .
   tidy_html $NAME.3.html
   (
    echo '<?'
    sed -n -e '\%^ *<body>%,\% *<.body>%{p}' $NAME.3.html|
    sed -e '/^ *$/d'
    )> $NAME.3.md
done
exit

<tr><td><a href="md/M_process.3.md">          M_process          </a></td><td> Fortran Module for calling process-related C functions from Fortran</td></tr>
<tr><td><a href="md/process_open_read.3.md">  process_open_read  </a></td><td> open a process for reading</td></tr>
<tr><td><a href="md/process_readline.3.md">   process_readline   </a></td><td> read a line from the process</td></tr>
<tr><td><a href="md/process_readall.3.md">    process_readall    </a></td><td> read all output from the process</td></tr>
<tr><td><a href="md/process_open_write.3.md"> process_open_write </a></td><td> open a process for writing</td></tr>
<tr><td><a href="md/process_writeline.3.md">  process_writeline  </a></td><td> write a line to the process</td></tr>
<tr><td><a href="md/process_close.3.md">      process_close      </a></td><td> close the process</td></tr>
