Frequently Asked Questions About Fortran
========================================

Contents
--------

-   Gotchas:
    -   [Gotchas: Inheritance control for CONTAIN-ed procedures in Fortran](faq/contained.md)
    -   [Gotchas: significant digits and simple assigns](faq/assign_precision.md)
    -   [Gotchas: other assignment and initialization erros] (faq/assign_precision.md)
    -   [Gotchas: conformance and elemental functions](faq/conform.md)
    -   [Gotchas: avoiding allocatables](faq/avoiding_allocatables.md)
-   Arrays:
    -   [How do I initialize an array in row-column order in Fortran?](faq/row-column.md)
    -   [Trouble initializing character arrays in Fortran;](faq/character_array_initialization.md) or why

            character(len=*),parameter :: array(*)=['one','two','three']

        is an error

    -   [\"array=\[\]\" will not work in Fortran](faq/zero_elements.md)
    -   [How do I compare arrays in Fortran?](faq/compare_arrays.md)

-   [Notes on compound Boolean expressions](faq/short-circuit.md)
-   [Procedure pointer](faq/procedure_pointer.md)
-   [How do I put block comments in Fortran source?](faq/comments.md)
-   [How do I get a file size in Fortran?](faq/file_size.md)
-   [How to issue a command to the operating system](faq/system.md)
-   [Automatically indenting a Fortran file](faq/indent.md)
-   [Build Tools](faq/make.md)
-   [Calling gnuplot(1) from Fortran](faq/gnuplot.md)
-   [Variable length CHARACTER arrays](faq/difflength.md)
-   [Notes on including metadata in programs, objects, and
    source](faq/metadata.md)
-   [Special values](faq/special_values.md)

### I/O
-   [Creating sequentially numbered filenames](faq/numbered_files.md)
-   [Writing to stderr](faq/stderr.md)
-   [How does Fortran handle a scratch file?](faq/scratch.md)
-   [Non-advancing I/O](faq/nonadvancing.md)
-   [Notes on list-directed output](faq/list_directed.md)
-   [Leading space on list-directed output](faq/leading_space.md)

------------------------------------------------------------------------

External Links
--------------

### Fortran standard

The \"web home\" of ISO/IEC JTC1/SC22/WG5 (the international Fortran
standards committee, or WG5 for short) is <https://wg5-fortran.org/>

The WG5 web site is where you\'ll find news about what\'s happening with
the Fortran standard, and links to all WG5 documents. Information on
current and past standards is also available there.

### Live Fortran pages (Online Compilers)

-   [GodBolt compiler explorer](https://godbolt.org/)
-   [fortran-lang \"play\"](https://play.fortran-lang.org/)
-   [CodersEditor.com](https://coderseditor.com/)
-   [ideone.com](https://ideone.com/)
-   [JDoodle (compiler options)](https://www.jdoodle.com/execute-fortran-online/)
-   [myCompiler.io](https://www.mycompiler.io/new/fortran)
-   [onecompiler.com](https://onecompiler.com/fortran)
-   [OnlineGDB](https://www.onlinegdb.com/online_fortran_compiler)
-   [rextester.com](https://rextester.com/l/fortran_online_compiler)
-   [techiedelight.com](https://techiedelight.com/compiler/fortran)
-   [TutorialsPoint (beautify)](https://www.tutorialspoint.com/compile_fortran_online.php)
-   [W3Schools](https://www.w3schools.com/tryit/trycompiler.asp?filename=demo_fortran)

### Fortran FAQs

-   [The Fortran Wiki FAQ](http://fortranwiki.org/fortran/show/FAQ)
-   [The Fortran FAQ](http://www.faqs.org/faqs/fortran-faq/)
-   [Fortran90.org FAQ](http://www.fortran90.org/src/faq.html)
-   [pages.mtu.edu
    FAQ](http://pages.mtu.edu/~shene/COURSES/cs201/FAQ/compile.html)

------------------------------------------------------------------------

### Fortran Compilers

-   <https://gcc.gnu.org/wiki/GFortran>

------------------------------------------------------------------------

### Repositories, Discussion Groups, Reference Sites, \...

-   [netlib mathematical algorithm repository](http://netlib.org)
-   [The Fortran Wiki](http://fortranwiki.org)
-   [comp.lang.fortran
    newsgroup](https://groups.google.com/forum/#!forum/comp.lang.fortran)
-   [J3 Fortran
    Proposals](https://github.com/j3-fortran/fortran_proposals)
-   [GitHub site for Fortran feature
    proposals](https://github.com/j3-fortran/fortran_proposals)
-   [github list of open-source Fortran
    projects](https://github.com/fortran-lang/stdlib/wiki/List-of-popular-open-source-Fortran-projects)
-   [Fortran Wikibook](http://en.wikibooks.org/wiki/Fortran)

<!-- -->

-   [Fortran Language Forum](https://fortran-lang.org/)
-   <https://github.com/fortran-lang/fortran-lang.org>
-   [Fortran Language Discourse](https://fortran-lang.discourse.group)
-   [Fortran Language Discourse
    YouTube](https://www.youtube.com/channel/UCTYRAlVmMCGGcrMkKxQLurw)
-   [Futility](https://github.com/CASL/Futility)

------------------------------------------------------------------------

### Fortran scientific model searches

[XGC](http://www.google.com/search?q=%22XGC%22%20Fortran%20code) ,
[SPECFEM](http://www.google.com/search?q=%22SPECFEM%22%20Fortran%20code)
, [ACME](http://www.google.com/search?q=%22ACME%22%20Fortran%20code) ,
[DIRAC](http://www.google.com/search?q=%22DIRAC%22%20Fortran%20code) ,
[FLASH](http://www.google.com/search?q=%22FLASH%22%20Fortran%20code) ,
[GTC](http://www.google.com/search?q=%22GTC%22%20Fortran%20code) ,
[LS-DALTON](http://www.google.com/search?q=%22LS-DALTON%22%20Fortran%20code)
, [NUCCOR](http://www.google.com/search?q=%22NUCCOR%22%20Fortran%20code)
, [NWCHEM](http://www.google.com/search?q=%22NWCHEM%22%20Fortran%20code)
, [RAPTOR](http://www.google.com/search?q=%22RAPTOR%22%20Fortran%20code)
,
[GAMESS(US)](http://www.google.com/search?q=%22GAMESS(US)%22%20Fortran%20code)
,
[GAMESS(UK)](http://www.google.com/search?q=%22GAMESS(UK)%22%20Fortran%20code)
,
[Gaussian](http://www.google.com/search?q=%22Gaussian%22%20Fortran%20code)
, [VB2000](http://www.google.com/search?q=%22VB2000%22%20Fortran%20code)
, [XMVB](http://www.google.com/search?q=%22XMVB%22%20Fortran%20code) ,
[ACES](http://www.google.com/search?q=%22ACES%22%20Fortran%20code) ,
[CFOUR](http://www.google.com/search?q=%22CFOUR%22%20Fortran%20code) ,
[MOLPRO](http://www.google.com/search?q=%22MOLPRO%22%20Fortran%20code) ,
[MOLCAS](http://www.google.com/search?q=%22MOLCAS%22%20Fortran%20code) ,

### Economic Modeling

[GEMPACK](http://www.google.com/search?q=%2GEMPACK%22%20Fortran%20code)
,

### Weather Modeling

[WRF(Weather Research and
Forecast)](http://www.google.com/search?q=%2WRF%22%20Fortran%20code),

### Geography

[geographiclib](http://www.google.com/search?q=%2geographiclib%22%20Fortran%20code),
[fortranGIS](http://www.google.com/search?q=%2fortranGIS%22%20Fortran%20code),

Best Practices
--------------

-   [https://github.com/Fortran-FOSS-Programmers/BestPractices](https://github.com/Fortran-FOSS-Programmers/Best%3Cem%3EPractices)
-   [http://www.fortran.com/FortranStyle.pdf](http://www.fortran.com/Fortran%3Cem%3EStyle.pdf)
-   <http://www.fortran90.org/src/best-practices.html>
-   [http://research.metoffice.gov.uk/research/nwp/numerical/fortran90/f90standards.html](http://research.metoffice.gov.uk/research/nwp/numerical/fortran90/f90%3Cem%3Estandards.html)
-   <https://github.com/szaghi/zen-of-fortran>

Fortran document generators
---------------------------

[fordocu](http://www.google.com/search?q=%22fordocu%22%20Fortran%20code)
,
[robodoc](http://www.google.com/search?q=%22robodoc%22%20Fortran%20code)
,
[(\$)understand](http://www.google.com/search?q=%22understand%22%20Fortran%20code)
,
[doxygen](http://www.google.com/search?q=%22doxygen%22%20Fortran%20code)
,

Repositories
------------

-   [Trending Fortran on
    GitHub](https://github.com/trending/fortran?since=monthly)

Fortran and HPC searches
------------------------

[Fortran](http://www.google.com/search?q=%22Fortran%22%20Fortran%20code)
,
[Coarray](http://www.google.com/search?q=%22Coarray%22%20Fortran%20code)
, [MPI](http://www.google.com/search?q=%22MPI%22%20Fortran%20code) ,
[OpenMP](http://www.google.com/search?q=%22OpenMP%22%20Fortran%20code) ,
[OpenACC](http://www.google.com/search?q=%22OpenACC%22%20Fortran%20code)
, [HDF5](http://www.google.com/search?q=%22HDF5%22%20Fortran%20code) ,
[HPC](http://www.google.com/search?q=%22HPC%22%20Fortran%20code) ,
[MPI-AMRVAC](http://www.google.com/search?q=%22MPI-AMRVC%22%20Fortran%20code)
,

Sites for gathering programming idioms and programming chrestomathy
-------------------------------------------------------------------

-   [Rosetta Code (multi-lingual code
    samples)](https://www.rosettacode.org)
-   [Fortran-specific Rosetta
    Code](http://rosettacode.org/wiki/Category:Fortran)
-   [Exercism](https://exercism.io/)
-   [Programming-idioms](https://www.programming-idioms.org/)
-   [codingame](https://www.codingame.com/start)

Just found
----------

-   [Simpson software](https://caps.gsfc.nasa.gov/simpson/software.html)
-   [The EDSS/Models-3 I/O
    API](https://www.cmascenter.org/ioapi/documentation/all_versions/html/index.html)
-   [SimCom commericial services](https://http://simconglobal.com)

Documentation
-------------

-   [lectures](http://www.archer.ac.uk/training/course-material/2018/02/oofortran-daresbury/Lectures/L05-ClassesAndVisibility.pd)
-   lectures\<\\a\>

------------------------------------------------------------------------

Revised on Sat, Nov 18, 2017 6:28:56 PM by
[JSU](JSU.xhtml){.existingWikiWord}
