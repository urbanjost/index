#!/bin/bash --norc
################################################################################
#@(#)inpath.sh(1sh) - true if command is in path
#(John S. Urban)
################################################################################
NAME="$1"
################################################################################
TESTIT(){
set -x
# problematic 
inpath.sh  builtin;   echo   $?
inpath.sh  type;      echo   $?
inpath.sh  alias;     echo   $?
inpath.sh  command;   echo   $?
inpath.sh  hash;      echo   $?
# normal tests  
inpath.sh  notthere;  echo   $?
inpath.sh  ls;        echo   $?
inpath.sh  /;         echo   $?
alias checkme=/
inpath.sh checkme;    echo   $?
}
################################################################################
# To check if a program really exists andd is not a bash(1) built-in
# command, then command(1), type(1) and hash(1) are not appropriate
# for testing as they all return 0 exit status for built-in commands.

# Commands found in the hash table are checked for existence before being
# executed and non-existence forces a normal PATH search.

# shopt -u expand_aliases # ignores/hides these aliases 
# shopt -s expand_aliases # shows them via command -v. 

EXIT=1        # exit code to be returned by command
case "$NAME" in
   T-E-S-T-I-T) TESTIT;;
   */*) [ -x "$NAME" -a ! -d "$NAME" ] && EXIT=0 || EXIT=2 ;; # If / in name, assume full pathname
   # hash(1), type(1), command(1), and which(1) can do this depending on whether aliases should be recognized and such
   # see also bash commands like builtin, alias, $POSIX_BUILTINS,
   *) hash "$NAME" 2>/dev/null                            && EXIT=0 || EXIT=3 ;; # search path for command name using hash(1)
   *) [ -x "$(command -v $NAME)" ]                        && EXIT=0 || EXIT=3 ;; # search path for command name using command(1)
   *) [ -x "$(which $NAME 2>/dev/null)" -a ! -d "$NAME" ] && EXIT=0 || EXIT=3 ;; # search path for command name using which(1)
   *) builtin type -P $NAME 2>/dev/null                   && EXIT=0 || EXIT=3 ;;
esac
################################################################################
exit $EXIT
################################################################################
# LOOK AT
# #command -v $1 | grep -qv alias
# 
# # Portable version of Bash's type -P cmd (without output on stdout)
# typep() {
#    command -p env -i PATH="$PATH" sh -c '
#       export LC_ALL=C LANG=C
#       cmd="$1"
#       cmd="`type "$cmd" 2>/dev/null || { echo "error: command $cmd not found; exiting ..." 1>&2; exit 1; }`"
#       [ $? != 0 ] && exit 1
#       case "$cmd" in
#         *\ /*) exit 0;;
#             *) printf "%s\n" "error: $cmd" 1>&2; exit 1;;
#       esac
#    ' _ "$1" || exit 1
# }
# 
# # Get your standard $PATH value
# #PATH="$(command -p getconf PATH)"
# typep ls
# typep builtin
# typep ls-temp
# 
# shopt -s checkhash
# if [ `LANG=C type example 2>/dev/null|wc -l` = 1 ];then echo exists;else echo "not exists";fi
################################################################################
