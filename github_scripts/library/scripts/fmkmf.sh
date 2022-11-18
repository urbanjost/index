#!/usr/bin/bash
#
# Copyright (c) The University of Edinburgh
# @(#)This is a utility to generate make files for Fortran 90.
# A basic makefile entry for bork.f90 would be
# bork.o:bork.f90
# <-tab->$(F90) -c bork.f90
#
# however if bork.f90 contains the line "use gunge" then
# (A)
# the entry has to be
# bork.o:bork.f90 garple.o <-- Forces bork to be recompiled if a module it
# <-tab->$(F90) -c bork.f90                               uses is changed
# where garple.f90 is the program containing the line "module gunge
# (B)
# The same type of entry has to be done for garple.f90
#
# We also need to generate an entry for the link step. If the main program
# was in baz.f90 then this should be
# baz:baz.o bork.o.........
# <-tab->$(F90) -o baz baz.o bork.o .....
# The list of object files to be linked should have foo.o in it once
# and only once for each foo.f90 that was compiled

# Test whether some environment variables are set. If so, use them as the
# compile command and source file tag
if [ "${FMKMF_F90}" ]
then
#    echo \# FMKMF_F90 set to ${FMKMF_F90}
    F90COMMAND=${FMKMF_F90}
else
#    echo \# FMKMF_F90 not set, using f90
    F90COMMAND=f90
fi

# Probs with different grep versions. We need a grep that recognises
# the -q flag
# set FMKMF_GREP=/usr/xpg4/bin/grep # Suns
# on Linux, the ordinary grep works, so this doesn't matter.

if [ "${FMKMF_GREP}" ]
then
#    echo \# FMKMF_GREP set to ${FMKMF_GREP}
    GREP=${FMKMF_GREP}
else
#    echo \# FMKMF_GREP not set, using f90
    GREP=grep
fi

if [ ${FMKMF_SFTAG} ]
then
#    echo \# FMKMF_SFTAG set to ${FMKMF_SFTAG}
    SFTAG=${FMKMF_SFTAG}
else
#    echo \# FMKMF_SFTAG not set, using .f90
    SFTAG=f90
fi

if [ ${FMKMF_SPATH} ]
then
#    echo \# FMKMF_SPATH set to ${FMKMF_SPATH}
    SPATH=${FMKMF_SPATH}
else
#    echo \# FMKMF_SPATH not set, using .
    SPATH=.
fi

# We don't need to test if FMKMF_LINKOPTS is set as fmkmf works if this is
# blank
LINKOPTS=${FMKMF_LINKOPTS}

# initially assume we are the instance started by the user. Change
# this if the -sub flag is used

issub=xx

# assume forward ls.
revlist=
this_fmkmf=$0
echo \# This script was called as ${this_fmkmf}
while [ $# -gt 1 ]
do
#echo \# Processing arg $1 with Dolhash $#
    if [ $1 = -p ]
    then
        shift
        SPATH=$1
    elif [ $1 = -f90 ]
    then
        shift
        F90COMMAND=$1
    elif [ $1 = -l ]
    then
        shift
        LINKOPTS=$1
    elif [ $1 = -tag ]
    then
        shift
        SFTAG=$1
    elif [ $1 = -sub ]
    then
        issub=sub
    elif [ $1 = -r ]
    then
        revlist=-r # used in ls commands.
        echo \# Reversing lists.
    else
        echo Don\'t Understand arg $1
    fi
    shift

done

echo \# revlist is ${revlist}

sourcefile=$1

# Remove colons from search path
SPATHLIST=`echo ${SPATH} | sed "s/:/ /g" `
echo \# search path $SPATHLIST

# echo \# issub is ${issub}

# get all lines with use as the first word. Make a list of the
# second word on those lines
# modulelist=`${GREP} -i  "^ *use " $1 | awk '{printf(" %s ",$2) }'  `

# improvement suggested by m. r. schaferkotter -- allows the use of
# use foomodule, only:bazfunction
# in the Fortran source.

modulelist=`${GREP} -i  "^ *use " $1 | sed -e 's/,//g' | \
    awk '{printf(" %s ",$2) }'`

modulelist=`${GREP} -i  "^ *use *::" $1 | sed -e 's/,//g' | \
    awk '{printf(" %s ",$2) }'`

echo \# file ${sourcefile} uses these modules: ${modulelist}

# Now we need to find the source file that each of those modules is in

modfilelist=
objfilelist=
for module in ${modulelist}
do
  #echo \# searching for module ${module}
  found=false
  for directory in ${SPATHLIST}
  do
    for modfile in `ls ${revlist} ${directory}/*.${SFTAG}`
    do
      #echo \# checking modfile ${modfile}
      if ${GREP} -i -q -w "^ *module *${module}" ${modfile}
      then
         echo \# module ${module} is in file ${modfile}
         if echo $modfilelist | ${GREP}  -q $modfile
         then
          echo \# ${modfile} Already in list
         else
            echo \# Adding ${modfile} to list
            modfilelist="${modfilelist} ${modfile}"
#           echo \# modfile list is now ${modfilelist}
         fi
         found=true
         break 2
      fi
    done
  done
  if [ ${found} = false ]
  then
    echo \# WARNING: source file containing module ${module} not found
  fi
done
if [ ${issub} != sub ]
then
   rm -f fmkmf_tmp_makefile
   echo \# Here are the compile steps > fmkmf_tmp_makefile
fi
# Now we can make a makefile entry for the program on the command line
objfile=`echo ${sourcefile} | sed -e"s/\.${SFTAG}/\.o/" -e "s|.*/||" `
#echo \# objfile is $objfile
objlist=
#echo \# modfilelist is ${modfilelist}
for i in ${modfilelist}
do
  foo=`echo ${i} | sed -e"s/\.${SFTAG}/.o/g" -e"s|.*/||" `
#  echo \# foo is ${foo}
  objlist="${objlist} ${foo}"
done
#echo \# objlist is ${objlist}
echo >> fmkmf_tmp_makefile
echo ${objfile}:${sourcefile} ${objlist} >> fmkmf_tmp_makefile
echo -e "\t \$(F90) -c  ${sourcefile}" | sed s"/-e //" >> fmkmf_tmp_makefile

# Now we see if any of the dependent modules are not present on the
# global list. If they are not, we add them, then we call this script
# recursively to build them a makefile entry

# Here is the global list
if [ ${issub} = sub ]
then
    globalmodfilelist=`cat fmkmf_tmp_list`
#    echo \# non-root node got ${globalmodfilelist} from file
else
#    echo \# root node blanking global list
    globalmodfilelist=${sourcefile}
    echo globalmodfilelist  > fmkmf_tmp_list
fi
#echo \# Global list is now ${globalmodfilelist}

for modfile in ${modfilelist}
do
    if echo ${globalmodfilelist} | ${GREP} -q $modfile
    then
        echo \# ${modfile} Already in global list

    else
#        echo \# Adding ${modfile} to globallist
        globalmodfilelist="${globalmodfilelist} ${modfile}"
#       echo \# Sending ${globalmodfilelist} to file
        echo ${globalmodfilelist} > fmkmf_tmp_list
        ${this_fmkmf} -sub -p ${SPATH} ${modfile}
        globalmodfilelist=`cat fmkmf_tmp_list`
#       echo \# Got ${globalmodfilelist} from file
    fi
done

# Now make makefile entry for Link step if we are not instance of this
# script called by itself

if  [ ${issub} != sub ]
then
    # link step and definition of F90 must go at top of makefile
    echo F90=${F90COMMAND}
    execname=`echo ${sourcefile} | sed s/\.${SFTAG}// `
    linklist=
    for i in ${globalmodfilelist}
    do
        foo=`echo ${i} | sed -e"s/\.${SFTAG}/.o/g" -e"s|.*/||" `
        linklist="${linklist} ${foo}"
    done
    echo
    echo ${execname}:${linklist}
    echo -e "\t \$(F90) -o ${execname} ${linklist} ${LINKOPTS}" | sed s"/-e //"
    # Next come the compile steps we put in a temporary file
    cat fmkmf_tmp_makefile
    rm -f fmkmf_tmp_makefile

#   Add entry for make clean
    echo
    echo \# This entry allows you to type \" make clean \" to get rid of
    echo \# all object and module files
    echo clean:
    echo -e \
            "\t rm -f -r f_{files,modd}* *.o *.mod *.M *.d V*.inc *.vo " \
        "V*.f *.dbg album F.err" |   sed s"/-e //"

    rm fmkmf_tmp_list
#else
#    echo \# Returning from non-root node of tree
fi
