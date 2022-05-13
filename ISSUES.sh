#!/bin/bash
(
echo '|Repository|Description|Issues|Stars|Build FORD(1) docs|ubuntu with intel|ubuntu with gfortran|macos with gfortran|windows with gfortran|windows with mingw64 gfortran|windows with msys gfortran|'
echo '| -------- | --------- | ---- | --- | ---------------- | --------------- | ------------------ | ------------------ | ------------------- | --------------------------- | ----------------------- |'
while read NAME DESCR
do
echo "\
|[$NAME](http://github.com/urbanjost/$NAME)|\
$DESCR |\
[![GitHub issues open](https://img.shields.io/github/issues/urbanjost/$NAME.svg?maxAge=2)](https://github.com/urbanjost/$NAME/issues)|\
[![GitHub stars](https://img.shields.io/github/stars/urbanjost/$NAME.svg)](https://urbanjost.github.io/$NAME/man3.html)\
|[![N/A]\
(https://github.com/urbanjost/$NAME/actions/workflows/deploy_api_docs.yml/badge.svg)\
](https://github.com/urbanjost/$NAME/actions/workflows/deploy_api_docs.yml)\
|[![N/A]\
(https://github.com/urbanjost/$NAME/actions/workflows/test_intel_ubuntu.yml/badge.svg)\
](https://github.com/urbanjost/$NAME/actions/workflows/test_intel_ubuntu.yml)\
|[![N/A]\
(https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_ubuntu.yml/badge.svg)\
](https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_ubuntu.yml)\
|[![N/A]\
(https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_macos.yml/badge.svg)\
](https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_macos.yml)\
|[![N/A]\
(https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_windows.yml/badge.svg)\
](https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_windows.yml)\
|[![N/A]\
(https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_mingw64_windows.yml/badge.svg)\
](https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_mingw64_windows.yml)\
|[![N/A]\
(https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_msys_windows.yml/badge.svg)\
](https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_msys_windows.yml)\
|"
done <<\EOF
general-purpose-fortran General Purpose Fortran Cooperative
prep Fortran pre-processor
M_strings Fortran string manipulations
M_time module of procedures that expand on the Fortran DATE_AND_TIME(3f) intrinsic
M_system Call C system routines (mostly POSIX) from Fortran
M_io A collection of procedures that create a simple interface for common I/O tasks 
M_path OOP interface to other GPF modules to manipulate and access files for use as a dependency with fpm(1) 
M_process read or write to a process from Fortran via a C wrapper
orderpack The ORDERPACK2.0 modules repackaged for fpm(1) use 
M_sort basic sorting
M_calculator parse Fortran-like double precision scalar expressions
M_anything Use polymorphism to allow promoting, casting and molding intrinsic types
M_draw low-level vector graphics library and module
M_pixel low-level graphics routines that write into a pixel array; supplemented by modules that create GIF files
M_graph A simple XY plot utility (WIP)
M_slices A module for producing slice plots requiring fpm(1) to build
M_calcomp an old graphics library used in conjunction with M_draw for work with old codes
M_color convert between RGB color values and other common color models
M_matrix interact with your Fortran program with a matlab-like scripting language
M_blas A Fortran BLAS library implemented as a free-format module.
M_factor Factors of whole numbers
M_LA A small collection of Linear Algebra routines 
M_random A collection of routines related to pseudo-random numbers
M_display An fpm(1) package for displaying small matrices based on dispmodule(3f)
M_hashkeys hash algorithms
M_args Command line parsing using a NAMELIST group  -- packages for use with fpm(1)
M_CLI Unix-like command line parsing -- prototype style converts command line to a NAMELIST group
M_CLI2 Fortran commandline-interface using a simple prototype command
M_kracken parse command line arguments and create configuration files using syntax similar to ULS commands
M_kracken95 An almost Fortran-95 version of the command line parser procedure kracken(3f)
M_readline Fortran interface to the C readline(3c) library for providing interactive command line history editing
M_history input line history editor
M_attr set terminal text attributes using ANSI escape sequences
M_escape Using in-band signaling wih ANSI control (escape) sequences to control terminal color from Fortran
M_ncurses Fortran interface to the Ncurses C library
M_msg convert all common variables to a string in Fortran using unlimited polymorphic variables 
M_stopwatch package for measuring cpu and wall clock execution time
paranoia The Fortran compiler option inspection and testing program implemented as subroutines
M_intrinsics man-page style descriptions of Fortran intrinsics for use as a reference for developers and tutorials
fortran-intrinsic-descriptions A snapshot of the markdown source for Fortran intrinsics as well as the fman(1) program
M_list maintain a small array as a list
M_match subset of Regular Expressions implemented in Fortran
M_overload Common examples of overloading of intrinsics and operators
M_uuid module of FOX routines for generating a UUID (Universally Unique ID)
spag_lapack spag_lapack is experimental WIP of LAPACK processed with plusFORT spag(1) and configured to build with fpm
plugins fpm(1) plugins (expermental)
fpm-man fpm package for building a program to display descriptions of Fortran intrinsics
fpm-search fpm-search  finds information about registered fpm (Fortran Package Manager)  packages
fpm-tools Just a personal repository for testing fpm (Fortran Package Manager)
easy steps to setup a github repository with fpm, and github actions including ford(1) documentation and unit tests
scripts scripts I miss on new platforms
M_OS Determine OS-specific information using standard Fortran -- TEST ONLY
EOF

cat <<\EOF
## See Also:
```bash
curl 'https://api.github.com/users/urbanjost/repos?per_page=1000'
w3m -dump 'https://api.github.com/users/urbanjost/repos?per_page=1000'
```
EOF
) >ISSUES.md
git add ISSUES.md ISSUES.sh
git commit -m 'refresh ISSUES'
git push
exit
