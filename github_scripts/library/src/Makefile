#@(#) make(1) rules for maintaining a FORTRAN90 library
#
# example makefile for building a static library. 
#
# source files are assumed to be in subdirectories below where this makefile
# is placed and to end with the suffix .f90 or .F90. The output is assumed
# to be a library file and associated .mod files, which will be placed in
# the $(INSTALL) directory.
#-------------------------------------------------------------------------------
# sample configuration values
#
# name of library to build of the form lib$NAME.a
LIBNAME=sample
# directory to place output files in
INSTALL=lib

# set up conditional programming environment macro definitions
F90=gfortran
AR=ar
ARFLAGS=rU
F90FLAGS =  -J $(INSTALL)
RANLIB=ranlib
#-------------------------------------------------------------------------------
LIBRARY=$(INSTALL)/lib$(LIBNAME).a

# find Fortran procedure sources. Note they are assumed to be in subdirectories.
# you can change it as required to find other sources, but this is usually
# a nice scheme

SRCS=$(wildcard */*.f90) $(wildcard */*.F90)

# create a list of corresponding object files
LIBOBJ_F90_1=$(patsubst %.f90,$(LIBRARY)(%.o),$(SRCS))
LIBOBJ_F90=$(patsubst %.F90,$(LIBRARY)(%.o),$(LIBOBJ_F90_1))
#-------------------------------------------------------------------------------
# set the '.RECIPEPREFIX' variable to an alternate character
.RECIPEPREFIX = >
SHELL=/bin/bash
#-------------------------------------------------------------------------------
.SUFFIXES: .f90 
# .f90 -- assume free-format fortran
#-------------------------------------------------------------------------------
# list what to run and then do any teardown. Do not leave this empty
all: directorysetup $(LIBRARY)
> @echo DONE
#-------------------------------------------------------------------------------
# what to do before you start compiling
directorysetup:
> @ echo "MAKING LIBRARY:   $(LIBRARY)" 
> @ echo "SRCS:             $(SRCS)" 
> @ echo "OBJECTS:          $(LIBOBJ_F90)" 
> @ [ -d "$(INSTALL)" ] || \
>   (echo "making directory $(INSTALL)"; \
>   mkdir -p $(INSTALL))
#-------------------------------------------------------------------------------
# what to do after you have built the library
$(LIBRARY): $(LIBOBJ_F90)
> @ $(RANLIB) $(LIBRARY)
> @ echo "TARGETS FOR $(LIBRARY) UP TO DATE"
#-------------------------------------------------------------------------------
.f90.a:
> @ [ -s $(?) ] || echo "error: $(?) is empty"
> $(F90) -c $(F90FLAGS) $< -o $%
> @ $(AR) $(ARFLAGS) $(LIBRARY) $% 
> @ $(AR) $(ARFLAGS) $(LIBRARY) $<  # optionally store the source code too
> @ /bin/rm -f ${?F:.f90=.o} $%
#-------------------------------------------------------------------------------
clean:
> -/bin/rm -f core 
#-------------------------------------------------------------------------------
clobber: clean
> /bin/rm -f -i $(LIBRARY) $(INSTALL)/*.mod
#------------------------------------------------------------------------------
