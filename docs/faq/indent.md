#indenting Fortran

##Using Common Editors

Several commonly available editors will automatically indent your
source  as you compose the code. Two of the most popular 
editors that will do so are vim(1) and emacs(1) _(assuming you
have current versions installed that were built using the most common
options)_.
 
That is, those editors  will automatically recognize your code as
Fortran if the file suffix is .F, .F90, .f, or .f90 and will apply
language-specific indenting rules as you create your code.

The editors can also be used to re-indent existing files:

###vim(1)

Assuming vim(1) has been built with internal indenting (typically it is)
To re-indent an entire file in the vim(1) editor, start editing the file
and enter

    gg=G

For more information on the "=" command enter

    :help =

while in the vim(1) editor. If the indenting is incorrect for free-format source,
look for the topic "free-format fortran in vim" and update your indent file.

To use vim(1) as a command to indent many files use

    vim -T dumb -c 'set backup' -c 'argdo execute "normal gg=G"|w' -c 'q!' --not-a-term *.f90

if you want backup files for each file, or use

    vim -T dumb  -c 'set nobackup nowritebackup noswapfile' -c 'argdo execute "normal gg=G"|w' -c 'q!' --not-a-term *.f90

if you do not need the backup files (and to make it run optimally).

###emacs(1)

To use emacs(1) to indent free-format fortran files as a command create
a small script file called indent\_emacs:

    #!/bin/bash
    for FILENAME in $*
    do
       emacs --batch $FILENAME -f mark-whole-buffer -f f90-indent-subprogram -f save-buffer
    done

And then enter "chmod u+xr indent\_emacs". Now you can use

    ./indent_emacs *.f90

to batch indent many files.

##Programs

A Fortran indenter program named
<a href="https://sourceforge.net/projects/findent/"> findent(1) </a>
can be used as a CLI (Command Line Interface) command, with a Java GUI
interface, and from several editors such as vim.

##Examples

Given the file

     program demo_expand
     !  test filter to expand escape sequences in input lines
     use M_strings, only : expand
     character(len=1024) :: line
     integer             :: ios
     READFILE: block
     do
     read(*,'(A)',iostat=ios)line
     if(ios /= 0) exit READFILE
     write(*,'(a)')trim(expand(line))
     enddo
     endblock READFILE
     end program demo_expand

Then the commands

    findent <demo_expand.f90 >demo_expand.f90.new;mv demo_expand.f90.new demo_expand.f90

produces

    program demo_expand
    !  test filter to expand escape sequences in input lines
       use M_strings, only : expand
       character(len=1024) :: line
       integer             :: ios
       READFILE: block
          do
             read(*,'(A)',iostat=ios)line
             if(ios /= 0) exit READFILE
             write(*,'(a)')trim(expand(line))
          enddo
       endblock READFILE
    end program demo_expand

And the command

    vim -T dumb -c 'normal gg=G' -c w -c 'q!' --not-a-term xxx.f90

Produces

    program demo_expand
            !  test filter to expand escape sequences in input lines
            use M_strings, only : expand
            character(len=1024) :: line
            integer             :: ios
            READFILE: block
                    do
                    read(*,'(A)',iostat=ios)line
                    if(ios /= 0) exit READFILE
                    write(*,'(a)')trim(expand(line))
                    enddo
            endblock READFILE
    end program demo_expand
