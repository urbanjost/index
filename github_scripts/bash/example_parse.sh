#!/bin/bash
################################################################################
HELP(){
cat <<\EOF
display help
EOF
}
################################################################################
VERSION(){
cat <<\EOF
display version
EOF
}
################################################################################
# Transform long options to short by editing arguments
for arg in "$@"; do
  shift
  case "$arg" in
    "--help")    set -- "$@" "-h" ;;
    "--version") set -- "$@" "-v" ;;
    *)           set -- "$@" "$arg"
  esac
done
echo "ARGUMENTS ARE NOW $@"
################################################################################
vflag=
hflag=
OPTERR=1
while getopts hv PARAM "$@"
do
   echo operand $operand name $PARAM OPTIND $OPTIND OPTARG $OPTARG
   case "$PARAM" in
    h)    HELP; exit 0 ;;
    v)    vflag=1;; # vvalue= $OPTARG";;
    ?)   printf "$0 : Usage: %s: [-h|--help]|[-v|--version] files\n" 
   exit 2;;
   esac
done
################################################################################
echo vflag $vflag aflag $aflag
if [ ! -z "$vflag" ]; then
   VERSION
fi
shift $(($OPTIND - 1))
printf "Remaining arguments are: %s\n$*"
################################################################################
exit
################################################################################
