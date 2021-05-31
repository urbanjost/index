# Documenation
 - [Modern Fortran](http://cyber.dabamos.de/programming/modernfortran)
 - [M_intrinsics](https://github.com/urbanjost/M_intrinsics)
    + [index of intrinsics](https://urbanjost.github.io/M_intrinsics/index3.html)
    + [index of statements](https://urbanjost.github.io/M_intrinsics/index7.html)
 - [GNU gfortran intrinsic descriptions](https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html)
 - [Fortran 2018 Standard](https://j3-fortran.org/doc/year/18/18-007r1.pdf)
 - [Intro to Fortran](https://www.nics.tennessee.edu/files/pdf/hpcss13_14/01_21_intro_to_fortran_pt2.pdf)

# Standards
 - [J3](https://j3-fortran.org)

# Compilers
- [GNU gcc](http://gcc.gnu.org)

# Repositories
 - [netlib](http://www.netlib.org)
 - [GPF](https://github.com/urbanjost)
 - [Fortran Standard Library project](https://github.com/fortran-lang/stdlib/issues/229)
 - [index to Burkardt repository](https://github.com/Beliavsky/Burkardt-Fortran-90)

# Examples
 - [Rosetta Code](https://rosettacode.org)

# Fortran Package Manager (fpm)
 - [fpm](https://github.com/fortran-lang/fpm)
 - [repository](https://github.com/fortran-lang/fpm-registry)
 - [package listing](https://fortran-lang.org/packages).

# Discussion
 - [Fortran Discourse](https://fortran-lang.discourse.group)
 - [Google Fortran newsgroup](https://groups.google.com/forum/#!forum/comp.lang.fortran)
 - [Fortran Wiki](http://fortranwiki.org)

# Date and Time
 - [datetime-fortran](https://github.com/wavebitscientific/datetime-fortran)
 - [M_time](https://urbanjost.github.io/M_time/)

# Configuration files
 - [Discussion](http://degenerateconic.com/fortran-configuration-file-formats)
 - [TOML](https://github.com/toml-f/toml-f)
 - [JSON](https://github.com/jacobwilliams/json-fortran)
 - [LAFF](https://urbanjost.github.io/M_matrix)

# Other Packages
 - [fortran curl]( https://github.com/interkosmos/fortran-curl)
 - [spack](https://github.com/spack/spack.git)

# Presentations and Papers
 -[fortrancon-2020](http://degenerateconic.com/fortrancon-2020/)
 -[CiSE article](https://github.com/LKedward/fortran-lang-CiSE-article)
 -[Fortran Forum article](git clone https://github.com/LKedward/fortran-forum-article-template)
}

#fpm packages
iso_varying_string : null
jsonff : null

## Expression parsing
 - M_matrix : matlab/octave-like Fortran interface
 - M_calculator : basic numeric expression parser

## Strings
 - M_strings : string manipulation
 - M_change : Basic Regular Expressions

## System Interfaces
 - M_system : (mostly)POSIX system routine interface
 - M_process : ISO_C_BINDING interface to popen(3c) and related system procedures 

## Date and Time
 - M_time : date and time conversion, formatting and computation 
 - datetime : null

## Command line parsing
 - M_kracken95 :  command line parsing using Fortran 95 (LICENSE:PD)
 - M_CLI : Unix-style commandline parsing using a prototype command and NAMELIST (STD:f2008)
 - M_CLI2 : Unix-style commandline parsing using a prototype command
## Regular Expressions

## Graphics
 - M_draw : basic vector graphics package
 - M_pixel : basic vector drawing into a pixel graphics format
 - M_color : conversions between common color models 

## Other
 - blas : The BLAS (Basic Linear Algebra Subprograms) are routines that provide standard building blocks for performing basic vector and matrix operations.
 - cairo-fortran : Fortran bindings for libcairo
 - dftd4 : Generally Applicable Atomic-Charge Dependent London Dispersion Correction
 - fftpack : FFTPACK is a package of Fortran subprograms for the fast Fourier transform of periodic and other symmetric sequences. It includes complex, real, sine, cosine, and quarter-wave transforms.
 - fhash : Implements a hash table type with support for generic keys and values.
 - finterp : Modern Fortran Multidimensional Linear Interpolation
 - forlab : null
 - fpm-dict : fpm plug-in to look up words and descriptions on WWW using fortran-curl(3f)/libcurl(3c)
 - fpm-man : display Fortran intrinsics documentation
 - fpm-search : null
 - fpm-time : fpm plug-in to generate timing profiles using gprof(1)
 - general-purpose-fortran : GPF (General Purpose Fortran)
 - iso_varying_string : null
 - jsonff : null
 - lapack : LAPACK ("Linear Algebra Package") is a standard software library for numerical linear algebra. It provides routines for solving systems of linear equations and linear least squares, eigenvalue problems, and singular value decomposition. It also includes routines to implement the associated matrix factorizations such as LU, QR, Cholesky and Schur decomposition.
 - linpack : Description
 - M_calcomp : an old Calcomp look-alike graphics library. Not for new large code development
 - mctc-gcp : Geometrical Counter-Poise correction
 - mctc-lib : Modular computation tool chain library
 - M_escape : ANSI control escape sequences using an XML-like syntax for attributes like color on video displays and emulators
 - M_history : Input History Editor
 - minpack : Minpack includes software for solving nonlinear equations and nonlinear least squares problems.
 - M_intrinsics : module of text descriptions of Fortran intrinsics and features
 - M_io : I/O-related tools
 - M_list : string manipulation (STD:2008)
 - M_match : Basic Regular Expressions
 - M_msg : converts any standard scalar type to a string and support unit testing
 - M_ncurses : Fortran-callable interfae to the C library ncurses(3c)
 - M_path : basic numeric expression parser
 - M_readline : Fortran-callable interfae to the C input history editor readline(3c)
 - M_slices :  produce basic slice plot on POSIX systems with X11 Windows
 - M_sort : null
 - mstore : Molecular structure store for testing
 - multicharge : null
 - M_uuid : module to produce a UUID string
 - pointsets : null
 - quadpack : Quadpack is a Fortran 77 library for numerical integration of one-dimensional functions.
 - quaff : null
 - s-dftd3 : Simple reimplementation of the DFT-D3 method
 - sqliteff : null
 - stdlib-fpm : null
 - strff : null
 - toml-f : TOML parser implementation for data serialization and deserialization
 - vegetables : null

<!--
* [ansi2html](https://github.com/ralphbean/ansi2html) ANSI escape codes to HTML from programs and as a bash shell
   at https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html
[Fortran Programming Language](https://fortran-lang.org).
 - [Fortran Wiki](http://fortranwiki.org)
   [Fortran Wiki](http://fortranwiki.org) http://fortranwiki.org
* [**foul**](http://foul.sourceforge.net/) A library for controlling the attributes of output text using Fortran
   * [FOX](http://fortranwiki.org/fortran/show/FoX)
    + f-toml [https://github.com/toml-f/toml-f] (https://github.com/toml-f/toml-f)
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
