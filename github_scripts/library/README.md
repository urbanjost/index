# make(7)

## NAME

   make(7) - various ways to use make(1), including building a library and preprocessing with whatever you want, like bash(1)

## DESCRIPTION

### want to build a library file?

Code should be as re-usable as practical and easy to use when loading.
This often leads to maintaining a _library_ file.

### preprocessing can be used to keep documentation in a simple format directly in the code file

Fortran does not currently support templating and there is no single
pre-processor supported. cpp(1) and fpp(1) are probably the most
commonly used but are not particularly oriented towards templates
(which usually requires some kind of looping).  m4(1) is more than
powerful enough for preprocessing but a bit complex to use for many
people's taste. Simple preprocessors such as cpp(1) are usually seen as
being most useful for maintaining conditional code; but are also useful
for keeping documentation in a simple format such as flat text, html,
or markdown in the source file. Imagine, for example a file layout such as

      #ifdef DOCUMENT
        my documentation
	text
      #else
      #endif
      subroutine mycode()
      end subroutine mycode

To extract just the document section could be something as simple as

      cpp -DDOCUMENT -P  --traditional  $FILENAME

while the fortran code could be processed normally (often just by being
a file ending in ".F90" instead of ".f90"

      cpp -P  --traditional -std=f95 $FILENAME

### cpp(1)/fpp(1) is probably not enough, so what preprocessors do I need?

Since the advent of modules in Fortran maintaining simple make(1) files
has become more difficult to maintain (make(1) was very suitable for
f77- Fortran).  Among other factors this has lead to the rise of cmake(1)
and other utiltities.

Most people desire a simple way to maintain at least basic documentation
in their source code. doxygen(1) is one of many utilities that can
faciliate this.

And you probably want to consider maintaining code history with something
like git(1), hg(1), ...

### How is this for a surprise? 

Out of the box most developers have make(1), probably know some POSIX 
shell scripting, and have some idea on how to keep documentation as 
flat text; maybe even as HTML or some markdown(1) variant.

And there are a few simple scripts available to help build the targets
for a make(1) file, so how about a KISS implementation?

Without much explanation (yet) this repository shows that adding a few
scripts lets you create a source directory where a library file with 
source files in different directories is maintained, keep documentation
in the files that can be automatically post-processed into manpages(1)
and most importantly use executable scripts to write Fortran source. 
So far this example shows using bash(1). bash(1) is available for 
almost all platforms and is almost always preinstalled on Linux, Unix,
and CygWin. It interfaces seamlessly with the Unix toolbox, letting you
use almost any common tool (expand, findent, ...) and between functions
and looping and being able to use essentially any system utility makes
as good a preprocessor as m4(1); but you are much more likely to already
be familiar with it.

So as an example, clone this repository and run

   makemake
   make

and you should get a library built with no "*.o" or intermediate files
left around cluttering things up, have in-line documentation turned into
manpages(1) and be wondering why there are fifty preprocessors out there
when a really powerful one is at your fingertips.

On the other hand, I use ufpp(1) (which can include an arbitrary shell
script as input) and makeout(1) for automating the build of most of the
GPF (General Purpose Fortran) repository; but I think anyone who knows
bash(1) should try it as a pre-processor.

And although this is a simple example, it can be hard to find even
documentation on how to set up a Makefile to cleanly build a library


Description Zaak 2011-05-24 19:48:59 UTC

Here is some fortran code:

MODULE utils
  USE types, ONLY: WP
  IMPLICIT NONE

CONTAINS
  ELEMENTAL FUNCTION initPI() RESULT(PI)
    REAL(WP) :: PI
    PI = ATAN(1.0_WP)*4.0_WP
  END FUNCTION initpi
END MODULE utils

When I run the following command with gfortran 4.6 I get the following error.

$ gfortran -MG -cpp modutils.f90
modutils.f90:2.21:

  USE types, ONLY: WP
                     1
Fatal Error: Can't open module file 'types.mod' for reading at (1): No such file or directory

This entirely defeats the purpose of having a preprocessor spit out makefile rules. If I want my dependencies resolved automatically, I should be able to spit out .d files which are later included in my makefile *in an arbitrary order.* GENERATION OF MAKEFILE RULES FOR AUTOMATIC DEPENDENCY RESOLUTION MUST BE ABLE TO BE DONE IN ANY ORDER BY PARSING THE SOURCE. There should not be a requirement to have .mod files present. These files are part of a separate source file and contribute zero knowledge to the dependencies of the current file. The need not be present for preprocessing, or dependency resolution. (But yes, they are needed for syntax checking.)  With the `-M` feature added to gfortran one should be able to follow the procedure outlined on the GNUmake website for automatic dependency generation to build codes with a small set of pattern rules. See this page for more info. http://theory.uwinnipeg.ca/localfiles/infofiles/make/make_43.html

If the procedure outlined on that page is attempted, the include statement in the makefile will cause the makefile to about because the include statement tries to build the files in arbitrary order (likely ascii collating sequence by file name). The makefile code listed bellow should work but doesn't because of the eroneously required .mod files:

FC=ifort
GFC = gfortran

%.o: %.f90 %.d
 $(FC) $(FCFLAGS) $(FPPFLAGS) -c $< -o $@


%.d: %.f90
 $(SHELL) -ec "$(GFC) -M -cpp $(FPPFLAGS) $< | sed '1 s/^/$@ /' > $@"

sources:=$(wildcard *.f90)
depends:=$(patsubst %.f90,%.d,$(wildcard *.f90))

include $(depends)

Dependency resolution is the bane of Fortran developers, and a huge headache. Being able to implement Makefiles like the one listed above instead of teadiously writing line after line of dependency resolutions by hand will be a boon for the Fortran community as a whole. Please make it a priority to look into this in the near future.

Many thanks, and keep up the great work.

Comment 1 kargl 2011-05-24 21:34:18 UTC

Fortran problems never have an "Importance" of "critical" unless
the Fortran problems breaks bootstrap.  Resetting to "normal".

Comment 2 Daniel Franke 2011-07-24 19:05:43 UTC

Just a thought: did you try to pass '-E' (preprocess only) as well?

Comment 3 Zaak 2011-08-31 19:49:20 UTC

When I pass -E some strange behaviour occurs. First of all the code is preprocessed with the c preprocessor and unless the -o flag is passed the output is written to standard out, so this text will get included in the .d dependency definition files which are to be included in the makefile. One can avoid this issue if one passes -o /dev/null or does something clever with sed. A second side effect of -E is that the module dependencies are no longer included in the output which again renders this useless. After passing through the sed command (as outlined in the GNU Make documentation) the last line of modtypes.d went from:

modtypes.o types.mod: modtypes.f90

to

modtypes.o: modtypes.f90

Comment 4 Zaak 2011-08-31 19:58:41 UTC

Created attachment 25155 [details]
test case files with Makefile

The Makefile.alt is configured to pass -E and -o /dev/null when building the dependency lists, while the original Makefile does not. In my opinion, the original Makefile should build any object in the project IF gfortran were bug free.

Comment 5 kargl 2011-08-31 20:47:21 UTC

(In reply to comment #4)
> Created attachment 25155 [details]
> test case files with Makefile
>
> The Makefile.alt is configured to pass -E and -o /dev/null when building the
> dependency lists, while the original Makefile does not. In my opinion, the
> original Makefile should build any object in the project IF gfortran were bug
> free.

gfortran works just fine with a properly written Makefile.
Your Makefile gives me

troutmask:sgk[213] make
"Makefile", line 14: Could not find
make: fatal errors encountered -- cannot continue

Comment 6 Zaak 2011-08-31 22:01:06 UTC

I ma not saying gfortran is entirely broken, i'm merely claiming that there is a bug in the dependency resolution feature. Please see GNU Make documentation here for more information about Generating Prerequisites Automatically: http://www.gnu.org/software/make/manual/make.html#Automatic-Prerequisites

There is nothing wrong with my makefile. GNU make looks for rules to build any included makefiles and builds and updates them before running the rest of the makefile. It is this very step that gives me problems too, but because it requires the presence of types.mod before it can run the rule to make modutils.d and myprog.d. The rule to make these files uses gfortran's dependency resolution features which is where the problem is. The following step is what is causing the failure:

gccbug $ gfortran -M -cpp  modutils.f90 | sed '1 s/^/modutils.d /' > modutils.d

This is perfectly reasonable thing to want to do and produces the following output:

modutils.f90:2.11:

  USE types
           1
Fatal Error: Can't open module file 'types.mod' for reading at (1): No such file or directory

The whole point, again, is that we should not need the binary .mod files to accomplish dependency resolution because these .mod files have dependencies which must be resolved in order to create them. The source code file should be parsed for binary objects (.o and .mod) which it produces and which it depends on. The parsing of these source codes and the extraction of this information should not require dependencies and should be order agnostic.

I hope you are less confused now.

Comment 7 Steve Kargl 2011-08-31 22:17:48 UTC

On Wed, Aug 31, 2011 at 10:01:06PM +0000, zbeekman at gmail dot com wrote:
>
> I hope you are less confused now.
>

I'm not confused.  I do, however, use the grey matter
between my ears to write my Makefiles.

gfortran requires that the *.mod are present to
parse your code.  Sorry if you cannot deal with
that fact.

Comment 8 Zaak 2011-08-31 22:27:40 UTC

(In reply to comment #7)
> On Wed, Aug 31, 2011 at 10:01:06PM +0000, zbeekman at gmail dot com wrote:
> >
> > I hope you are less confused now.
> >
>
> I'm not confused.  I do, however, use the grey matter
> between my ears to write my Makefiles.
>
> gfortran requires that the *.mod are present to
> parse your code.  Sorry if you cannot deal with
> that fact.

I didn't mean any insult, I am not trying to troll or start a flame war. I'm sorry if I offended you in any way. I would appreciate you telling me why you think my makefile is wrong rather than just insulting me. Did you read the link to the GNUmake manpage? Did you try executing the command outside of the makefile?

The whole point of having dependency generation capabilities is to do EXACTLY what I'm trying to do. The .mod files are the difficulty resolving Fortran dependencies and I don't see what the use of a tool to resolve dependencies is, if you need to already have those dependencies resolved apriori to use the tool. If you can show me how to write a makefile with pattern rules that will automatically resolve dependencies and uses only brief, terse, pattern rules you will forever be my hero.

How would you write a makefile to automatically build fortran codes, and resolve their dependencies without explicitly hand coding the dependencies?

Comment 9 Zaak 2011-08-31 22:34:46 UTC

Additionally, if my entire premise is wrong what do you anticipate the use of the -M flag will be for? It's not hard to figure out that .o files depend on the .f90 files with the same name. I don't need a tool to do that for me, so how do you envision -M being used? Not the way listed on the GNUmake online documentation?

Comment 10 Steve Kargl 2011-08-31 22:45:41 UTC

On Wed, Aug 31, 2011 at 10:27:40PM +0000, zbeekman at gmail dot com wrote:
> http://gcc.gnu.org/bugzilla/show_bug.cgi?id=49149
>
> --- Comment #8 from Zaak <zbeekman at gmail dot com> 2011-08-31 22:27:40 UTC ---
> (In reply to comment #7)
> > On Wed, Aug 31, 2011 at 10:01:06PM +0000, zbeekman at gmail dot com wrote:
> > >
> > > I hope you are less confused now.
> > >
> >
> > I'm not confused.  I do, however, use the grey matter
> > between my ears to write my Makefiles.
> >
> > gfortran requires that the *.mod are present to
> > parse your code.  Sorry if you cannot deal with
> > that fact.
>
> I didn't mean any insult, I am not trying to troll or start a flame war. I'm
> sorry if I offended you in any way. I would appreciate you telling me why you
> think my makefile is wrong rather than just insulting me. Did you read the link
> to the GNUmake manpage? Did you try executing the command outside of the
> makefile?

Yes, I scanned the GNU Make info file.  I can find no information
in that file concerning Fortran modules.

I also scanned the GNU Fortran info file.  I can find no mention of
using -M to build the dependence for Fortran modules.  In fact,
-M does not appear anywhere in the GNU Fortran info file.  So,
one needs to peer into the GNU GCC info file.

`-M'
     Instead of outputting the result of preprocessing, output a rule
     suitable for `make' describing the dependencies of the main source
     file.  The preprocessor outputs one `make' rule containing the
     object file name for that source file, a colon, and the names of
     all the included files, including those coming from `-include' or
     `-imacros' command line options.

Hmmm, no mention of a Fortran 'USE' statement.  A 'USE' statement
is a different beast than an 'INCLUDE' statement.

> How would you write a makefile to automatically build fortran codes, and
> resolve their dependencies without explicitly hand coding the dependencies?

Well, as I write the Makefile, I automatically write the correct
dependency.  I do not depend on an inadequate tool to do the
work for me.  If you absolutely must have a tool, google makedepf90.

PS: *.mod files are binary files.

Comment 11 Steve Kargl 2011-08-31 23:05:10 UTC

On Wed, Aug 31, 2011 at 10:34:46PM +0000, zbeekman at gmail dot com wrote:
> Additionally, if my entire premise is wrong what do you anticipate the use of
> the -M flag will be for? It's not hard to figure out that .o files depend on
> the .f90 files with the same name. I don't need a tool to do that for me, so
> how do you envision -M being used? Not the way listed on the GNUmake online
> documentation?

Given that I consider the -M option to be total irrelevant
for gfortran, I anticipate that the -M option is useless.
In fact there are a boat load of options listed in the GCC
info file, which are irrelevant for gfortran.  Many of these
options are historical baggage from when GCC was simply gcc
(ie., a C compiler).

Can you show me a specific passage in the GNU Make documentation
that states -M can be used to generate dependencies for
Fortran USE statements without the actual *.mod being
present?

What to you expect the -M option to do with

  program foo
    use omp_lib
    use iso_c_binding
  end program

Comment 12 Zaak 2011-09-01 01:14:40 UTC

> Can you show me a specific passage in the GNU Make documentation
> that states -M can be used to generate dependencies for
> Fortran USE statements without the actual *.mod being
> present?

Bullet number 7 in the what's new in gfortran section for the current stable release, 4.6.0: http://gcc.gnu.org/wiki/GFortran#GCC4.6

' Support the generation of Makefile dependencies via the `-M...` flags of GCC; you may need to specify additionally the -cpp option. The dependencies take modules, Fortran's include, and CPP's #include into account. Note: Using -M for the module path is no longer supported, use -J instead.'

It seems that this is a new feature and the documentation lags the implementation. Being a new feature I thought it was important to report what appears to me to be an important new bug (if I am interpreting the above statement correctly). Note also that Intel has recently added this capability to their Fortran compiler and they too have (different) bugs.

Comment 13 Zaak 2011-09-01 01:27:46 UTC

As for intrinsic F2003 modules, like ISO_C_BINDING, ISO_FORTRAN_ENV, etc. I would expect the compiler to be able to handle this appropriately, i.e. not require the presence of a iso_c_binding module in the build directory. Modules which are provided as compiler extensions to the Fortran standard should also be handled appropriately. My preference would be to exclude such intrinsic and compiler extension modules from the dependency list, but if a .mod file is installed with the compiler, the dependency could be given with a full path to its location. I can't think of an occasion when you would need a path to these intrinsic/extension modules, unless, perhaps, the tool were used while developing the compiler itself.

Also, every time I read 'The dependencies take modules, Fortran's include, and CPP's #include into account.' I can't help but think that the creators of this feature were trying to make a useful tool which could handle Fortran specific, especially module, dependency resolution.

Comment 14 Zaak 2011-09-03 14:46:57 UTC

cricket

Comment 15 Zaak 2015-03-03 21:09:47 UTC

Some very helpful person, on a different form, pointed out to me that there is a way to use include statements in makefiles, to include each individual dependency file output by the gfortran's -M option, and to set this up so that it will attempt to generate all the includes, even if one fails. Then Make will realize that the Makefile itself (via any successfuly included dependency) is out of date, and try to rebuild all the included dependency files if they are given as targets with build rules. This will recursively (and inefficiently) automatically resolve the dependencies, and update the makefile. If I get around to it, I'll post an example here

.



It appears in gfortran 4.9.2 that the issue with the intrinsic modules comment 13 has also been fixed. Apparently someone else agreed with me that this was indeed an issue.

As things seem to be fixed, and there is an acceptable work around for the main issue, I'm going to mark this as resolved.

Comment 16 Zaak 2015-03-03 21:20:53 UTC

*** Bug 49150 has been marked as a duplicate of this bug. ***


