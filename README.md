---
---
# INDEX OF THIS REPOSITORY
---
---

## General Repository
 - [GPF](https://github.com/urbanjost/general-purpose-fortran) : GPF (General Purpose Fortran)

## Expression evaluation
 - [M_matrix](https://github.com/urbanjost/M_matrix) : matlab/octave-like Fortran interface
   Allows for transferring data in and out of the program for inspection, saving, loading, ...
 - [M_calculator](https://github.com/urbanjost/M_calculator) : basic numeric expression parser

## Strings
 - [M_strings](https://github.com/urbanjost/M_strings/) : string manipulation

## Regular expressions
 - [M_change](https://github.com/urbanjost/M_change/) : Basic Regular Expressions

## Posix and system interfaces
 - [M_system](https://github.com/urbanjost/M_system) : (mostly)POSIX system routine interface
 - [M_process](https://github.com/urbanjost/M_process) : ISO_C_BINDING interface to popen(3c) and related system procedures 

## Date and time
 - [M_time](https://github.com/urbanjost/M_time) date and time conversion, formatting and computation 

## Command Line parsing
 - [M_CLI2](https://github.com/urbanjost/M_CLI2) : Unix-style commandline parsing using a prototype command
 - [M_CLI](https://github.com/urbanjost/M_CLI) : Unix-style commandline parsing using a prototype command and NAMELIST (STD:f2008)
 - [M_kracken95](https://github.com/urbanjost/M_kracken95) :  command line parsing using Fortran 95 (LICENSE:PD)

## Graphics
 - [M_draw](https://github.com/urbanjost/M_draw/) : basic vector graphics package
 - [M_pixel](https://github.com/urbanjost/M_pixel/) : basic vector drawing into a pixel graphics format
 - [M_color](https://github.com/urbanjost/M_color/) : conversions between common color models 
 - [M_calcomp](https://github.com/urbanjost/M_calcomp/) : old Calcomp look-alike graphics library. Not for new large code development
 - [M_slices](https://github.com/urbanjost/M_slices/) :  produce basic slice plot on POSIX systems with X11 Windows

## Preprocessing
 - [prep](https://github.com/urbanjost/prep) : A Fortran preprocessor written in Fortran

## Fortran documentation
 - [M_intrinsics](https://github.com/urbanjost/M_intrinsics)
    + [index of intrinsics](https://urbanjost.github.io/M_intrinsics/index3.html)
    + [index of statements](https://urbanjost.github.io/M_intrinsics/index7.html)
## Miscellaneous
 - [M_escape](https://urbanjost.github.io/M_escape/) : ANSI control escape sequences for attributes like color on video displays
 - [M_history](https://urbanjost.github.io/M_history/) : Input History Editor
 - [M_intrinsics](https://urbanjost.github.io/M_intrinsics/) : module of text descriptions of Fortran intrinsics and features
 - [M_io](https://urbanjost.github.io/M_io/) : I/O-related tools
 - [M_list](https://urbanjost.github.io/M_list/) : string manipulation (STD:2008)
 - [M_match](https://urbanjost.github.io/M_match/) : Basic Regular Expressions
 - [M_msg](https://urbanjost.github.io/M_msg/) : converts any standard scalar type to a string and support unit testing
 - [M_ncurses](https://urbanjost.github.io/M_ncurses/) : Fortran-callable interfae to the C library ncurses(3c)
 - [M_path](https://urbanjost.github.io/M_path/) : basic numeric expression parser
 - [M_readline](https://urbanjost.github.io/M_readline/) : Fortran-callable interfae to the C input history editor readline(3c)
 - [M_sort](https://urbanjost.github.io/M_sort/) : sorting
 - [M_uuid](https://urbanjost.github.io/M_uuid/) : module to produce a UUID string
---
---
# EXTERNAL SITES OF INTEREST
---
---
## Documentation
 - [Modern Fortran](http://cyber.dabamos.de/programming/modernfortran)
 - [GNU gfortran intrinsic descriptions](https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html)
 - [Fortran 2018 Standard](https://j3-fortran.org/doc/year/18/18-007r1.pdf)
 - [Intro to Fortran](https://www.nics.tennessee.edu/files/pdf/hpcss13_14/01_21_intro_to_fortran_pt2.pdf)

## Lists
 - [Beliavsky's List](https://github.com/Beliavsky/Fortran-code-on-GitHub/blob/main/README.md)

## Standards
 - [J3](https://j3-fortran.org)

## Compilers
- [GNU gcc](http://gcc.gnu.org)

## Repositories
 - [netlib](http://www.netlib.org)
 - [Fortran Standard Library project](https://github.com/fortran-lang/stdlib/issues/229)
 - [index to Burkardt repository](https://github.com/Beliavsky/Burkardt-Fortran-90)

## Examples
 - [Rosetta Code](https://rosettacode.org)

## Fortran Package Manager (fpm)
 - [fpm](https://github.com/fortran-lang/fpm)
 - [repository](https://github.com/fortran-lang/fpm-registry)
 - [package listing](https://fortran-lang.org/packages).
## Packaging
 - [packing-con](http://packaging-con.org/)
###fpm packages
    + fpm-search : null
    + fpm-man : display Fortran intrinsics documentation
    + fpm-dict : fpm plug-in to look up words and descriptions on WWW using fortran-curl(3f)/libcurl(3c)
    + fpm-time : fpm plug-in to generate timing profiles using gprof(1)

## Discussion
 - [Fortran Discourse](https://fortran-lang.discourse.group)
 - [Google Fortran newsgroup](https://groups.google.com/forum/#!forum/comp.lang.fortran)
 - [Fortran Wiki](http://fortranwiki.org)

## Date and Time
 - [datetime-fortran](https://github.com/wavebitscientific/datetime-fortran)
 - datetime : null

## Configuration files and data serialization
 - [Discussion](http://degenerateconic.com/fortran-configuration-file-formats)
 - [TOML](https://github.com/toml-f/toml-f) : TOML parser implementation for data serialization and deserialization
 - [JSON](https://github.com/jacobwilliams/json-fortran)
 - jsonff : null
 - [XML]
 - [namelist]
 - [hdf5]

## Graphics
 - cairo-fortran : Fortran bindings for libcairo

## Other Packages
 - [fortran curl]( https://github.com/interkosmos/fortran-curl)
 - [spack](https://github.com/spack/spack.git)

## Presentations and Papers
 -[fortrancon-2020](http://degenerateconic.com/fortrancon-2020/)
 -[CiSE article](https://github.com/LKedward/fortran-lang-CiSE-article)
 -[Fortran Forum article](git clone https://github.com/LKedward/fortran-forum-article-template)
}

## hash tables, linked lists, ...
 - fhash : Implements a hash table type with support for generic keys and values.

iso_varying_string : null

## Expression parsing

## Strings
 - iso_varying_string : null

## System Interfaces

## Command line parsing

## Regular Expressions

 - blas : The BLAS (Basic Linear Algebra Subprograms) are routines that provide standard building blocks for performing basic vector and matrix operations.
 - fftpack : FFTPACK is a package of Fortran subprograms for the fast Fourier transform of periodic and other symmetric sequences. It includes complex, real, sine, cosine, and quarter-wave transforms.
 - lapack : LAPACK ("Linear Algebra Package") is a standard software library for numerical linear algebra. It provides routines for solving systems of linear equations and linear least squares, eigenvalue problems, and singular value decomposition. It also includes routines to implement the associated matrix factorizations such as LU, QR, Cholesky and Schur decomposition.
 - linpack : Description
 - finterp : Modern Fortran Multidimensional Linear Interpolation
 - minpack : Minpack includes software for solving nonlinear equations and nonlinear least squares problems.
 - quadpack : Quadpack is a Fortran 77 library for numerical integration of one-dimensional functions.

 - dftd4 : Generally Applicable Atomic-Charge Dependent London Dispersion Correction
 - mstore : Molecular structure store for testing

 - forlab : null
 - mctc-gcp : Geometrical Counter-Poise correction
 - mctc-lib : Modular computation tool chain library
 - multicharge : null
 - pointsets : null
 - quaff : null
 - s-dftd3 : Simple reimplementation of the DFT-D3 method
 - sqliteff : null
 - stdlib-fpm : null
 - strff : null
 - vegetables : null

<!--
* [ansi2html](https://github.com/ralphbean/ansi2html) ANSI escape codes to HTML from programs and as a bash shell
   at https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html
[Fortran Programming Language](https://fortran-lang.org).
 - [Fortran Wiki](http://fortranwiki.org)
   [Fortran Wiki](http://fortranwiki.org) http://fortranwiki.org
* [**foul**](http://foul.sourceforge.net/) A library for controlling the attributes of output text using Fortran
   * [FOX](http://fortranwiki.org/fortran/show/FoX)
[general package criteria](https://github.com/fortran-lang/fortran-lang.org/blob/master/PACKAGES.md)


> http://gcc.gnu.org/bugzilla/show_bug.cgi?id=49149
[libcurl](https://curl.haxx.se/libcurl/) for Fortran 2008. Compilation has been
or from [miniconda](https://docs.conda.io/en/latest/miniconda.html).
[PEP 8](https://www.python.org/dev/peps/pep-0008/) compliant.  We enforce
 + [stdlib discussion on POSIX interfaces](https://github.com/fortran-lang/stdlib/issues/22#issuecomment-733021530)
  </table><span class="c3">Generated by <a href="http://www.squarebox.co.uk/download/manServer.shtml">manServer 1.08</a> from
[`tag`](https://docs.github.com/en/free-pro-team@latest/desktop/contributing-and-collaborating-using-github-desktop/managing-tags)
* [terminal colors](http://www.pixelbeat.org/docs/terminal_colours/)
The conda package manager can be installed from [miniforge](https://github.com/conda-forge/miniforge/releases)
The Fortran Wiki [ fortranwiki.org ](http://fortranwiki.org) contains
The Fortran Wiki [fortranwiki.org ](http://fortranwiki.org) contains information on many Fortran
-->
