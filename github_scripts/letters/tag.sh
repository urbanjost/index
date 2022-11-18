
#git config --global credential.helper store
#    Save the username and password for a session (cache it);
git config --global credential.helper cache
#    You can also set a timeout for the above setting
git config --global credential.helper 'cache --timeout=600'

cd $HOME/github
TTY=`tty`

while read NAME COMMENT

do
(
cd $NAME
echo CURRENT
git tag
echo ADDING
git push --delete origin v1.0.1
git push --delete origin v1.0.0
git tag -d v1.0.0 
git tag -d v1.0.1 

vi README.md < $TTY
git add README.md
git commit -m "standard fpm(1) mention, add tag 1.0.1"

#git add .gitignore 
#git add */dox.in
git tag -a v1.0.1 -m "$COMMENT"
git status
git tag
git push 
git push --tags 
echo ..................................
read paws
)

done <<\EOF
M_strings       Fortran string manipulations handling splitting, case, white space, ...
M_time          Fortran module for manipulating and presenting time and date values
M_calculator    module of routines for parsing expressions and returning values
M_CLI           command line argument parsing using a prototype command
M_color         a Fortran module that lets you convert between common color models
M_history       Fortran-based Input History Editor using simple CLI interface
M_msg           converts any standard scalar type to a string
M_process       Fortran Module for calling process-related C functions from Fortran
M_system        A Fortran interface to common C system routines (primarily POSIX)
M_kracken95     Fortran 95 version of M_kracken(3fm)
EOF
exit

M_change
M_draw 
M_io           basic file operations
M_commandline
exit
