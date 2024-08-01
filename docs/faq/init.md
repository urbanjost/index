## Fortran initialization 
 
### Fortran initialization on declaration and the implied SAVE attribute

In Fortran a declaration statement cannot be an assignment statement;
only a variable initialization. The declaration of CALL_COUNT in the
following program can be used to illustrate the difference:

    program called
    implicit none
    integer :: i
    do i=1,10
       call subinit() ! this has an initialization 
    enddo
    do i=1,10
       call subdeclare() ! this has an assignment instead
    enddo
 
    contains
 
    subroutine subinit()
    integer :: call_count=0  ! <= This is an initialization,
                             !    not as assignment
       call_count=call_count+1
       write(*,*) 'CALL_COUNT=',call_count
    end subroutine subinit
 
    subroutine subdeclare()
    integer :: assigned ! <= the type is declared, 
                        !    but the value is undefined at this point
       assigned=0       ! <= This really is an assignment used on each 
                        !    call to this procedure, not just an 
 		       !    initialization
       assigned=assigned+1
       write(*,*) 'ASSIGNED=',assigned
    end subroutine subdeclare
 
    program called

Execution of SUBINIT() does not print "1" ten times; it prints from "1"
to "10". On the other hand the calls to the almost identical SUBDECLARE()
prints "1" ten times.

This is because initialization takes place only once during program
execution.

Of course on the other hand a regular assignment statement occurs each
time it is encountered and clearly takes care of the reinitialization.

The CALL_COUNT variable therefore only has the value zero at the beginning
of the first call and does not get reinitialized when called again; and
any subsequent changes to its value such as the increment in SUBINIT()
are preserved throughout program execution even though it is not a global
variable.

This is particularly important to note as Today such syntax is different
than in many other widely used languages which use similar-looking
statements for assignment, not initialization.

So to reiterate if you initialize a variable at declaration

    integer :: a = 5  ! initialization with implied SAVE

it is the same as:

    integer, save :: a = 5  ! type declaration and initialization

or the old FORTRAN 77 equivalent

    integer :: a
    save a
    data /a/ 5

and not to:

   integer :: a   ! type declaration
   a = 5          ! assignment

as the functions of an initialization expression are inherited 
from old DATA statements.

### Related notes about porting legacy "FORTRAN 77" code
 
The above behavior is now all clearly specified by the Fortran standard.
It was not in early Fortran versions. There are therefore a few related
issues to watch for when porting legacy FORTRAN (ie. pre-Fortran 90)
code to a modern Fortran compiler.

Many pre-Fortran90 compilers statically allocated all variables, to the
point programmers often relied upon this behavior (ie. that all variables
were implicitly saved).

Depending on this behavior is technically a bug, as it is now specifically
standard that an unsaved value is assumed undefined upon reentry to
a procedure.

This history is why most compilers provide compile-time switches
to restore this old behavior, essentially acting as if a plain SAVE
statement occurs in all procedures.

The -fno-automatic option of gfortran, and commonly the -static or -save
option of other Fortran compilers (check the manual!) provide this option.

It is strongly recommended to not depend on the switch, but to explicitly
use the SAVE attribute or SAVE declaration to identify variables that
require being saved between calls. Even though a DATA statement or
initialization on a declaration imply the SAVE attribute it is much
clearer to specify the attribute anyway.

### Initializing Floating Point Numbers with constants

The most common mistake when initializing floating-point values is to
not make sure the RHS is generating a result with the same precision
as the LHS. This often is because the kind of a constant is not
declared. Without a declared kind a constant assumes the default kind of
the type, not the kind on the LHS (Left Hand Side) of the initialization
or assignment.

For example, assuming the definitions:

    integer,parameter :: dp=kind(0.d0) ! double precision
    integer,parameter :: sp=kind(0.0 ) ! single precision

Then the following code:

    real(dp) :: a
    a = 1.0  ! constant is missing a kind

is equivalent to:

    real(dp) :: a
    a = 1.0_sp  ! Oops! a single-precision value for a doubleprecision

and not to:

    real(dp) :: a
    a = 1.0_dp  ! correctly matches the constants' precision to the variable

However, the following code:

    real(dp) :: a
    a = 1

**is** equivalent to:

    real(dp) :: a
    a = 1.0_dp
Somewhat surprisingly because of the built-in type conversion rules
specified for an assignment assigning an integer to a double does not
loose any accuracy!

Another common case is assuming that because a lot of digits were declared
the compiler will treat a value with more precision. Try this:

      program trunc
      integer,parameter :: dp=kind(0.d0) ! double precision
      real(dp),parameter :: wrong=3.14159265358979323846264338327950
      real(dp),parameter :: better=3.14159265358979323846264338327950_dp
         write(*,*)'  3.14159265358979323846264338327950'
         write(*,*)wrong
         write(*,*)better 
      end program trunc

You will likely get something like this, without any warning about
an excessive number of digits or truncation of the value given.

      >   3.14159265358979323846264338327950
      >   3.14159274101257     
      >   3.14159265358979     

A general principle of Fortran is that the type of the RHS (Right
Hand Side) of an assignment does **not** depend on the LHS (Left Hand
Side). Once you understand this rule, a lot of things fall into place.

## Applying a floating point kind 

Another common error is to forget the decimal when creating what is
intended to be a floating point constant. In this example the declaration
of integer variables with a _dp suffix does not promote them automatically
to double precision variables. The literal "111_dp" is an INTEGER!:

    integer,parameter :: dp=kind(0.d0) ! double precision
    a = 111_dp

Kinds are just constant names for an integer and not their own type so
the compiler will likely not catch this error if the integer DP happens
to also represent a valid integer kind, which is commonly true.  (Some)
compilers can give unique values to each default kind and type pair and
so will catch this, but many will not. As noted above, because of the
conversion rules for expressions and assignments you might get lucky
and get the right value anyway, but obviously do not depend on it.
