
# Frequently Asked Questions About Fortran

## Contents

 * [Which Fortran compiler should I use?](#compilers)
 * [What are good books on Fortran?](#books)
 * [How should one capitalize "Fortran?"](#capitalization)
 * [How do I produce a library?](#libraries)
 * [Why doesn't `u == 1.4D0` produce the expected result?](#fp-equality)
 * [How do I read until the end of a file (EOF)?](#eof)
 * [How do I read a character without having the user press enter?](#getkey)
 * [How do I set the precision of real variables in Fortran 90?](#real-precision)
 * [How do I convert a numeric variable to a string, or vice-versa](#strnum)?
 * [How do I read command-line arguments?](#cmdline)
 * [How can I create a temporary file?](#tmp)
 * [Can I allocate a variable length character string?](#allocstr)
 * [Which file extension should I use for my Fortran code?](#fileext)
 * [How do I initialize an array in row-column order?](#array_constructor)


-----

## Which Fortran compiler should I use? {: #compilers }

[[GFortran]], [[G95]], [[Open Watcom]], and Silverfrost are free Fortran compilers,
while Absoft, IBM, [[Intel Fortran compiler|Intel]], [[LF Fortran|Lahey]], [[NAG Fortran compiler|NAG]], Pathscale, PGI, and [[Oracle Solaris Studio|Oracle]] produce commercial
Fortran compilers.  Polyhedron Software provides compiler comparisons
at <http://www.polyhedron.com/compare0html>.  See [[Compilers]] for more
information.

-----

## What are good books on Fortran? {: #books }

See the [[Books]] page.

-----

## How should one capitalize "Fortran?" {: #capitalization }

Standard capitalization is now the preferred way to write
Fortran for several reasons, most notably because that is
how recent versions of the [[Standards|standard]] write it.  Another reason is due to an effort to standardize the capitalization of the names of programming languages.
To quote Walt Brainerd (originally from the 1997-01-03
version of Keith Bierman's Fortran FAQ):

> The rule: if you say the letters, it is all caps (APL);
> if you pronounce it as a word, it is not (Cobol, Fortran,
> Ada).

Some choose to write FORTRAN when referring to older versions
of the language (prior to Fortran 90) to distinguish them
from newer versions.

-----

## How do I produce a library? {: #libraries }

To build a static library `libfoo.a` containing all
modules and procedures in the `.f90` files in the
current directory on Linux:

    % gfortran -c *.f90
    % ar cr libfoo.a *.o

The first command builds the object files and the
second archives the object files into a static archive.

To build a shared library `libfoo.so`:

    % gfortran -shared *.f90 -o libfoo.so -fPIC

In both cases, other compiler flags such as `-O2` can
be used.

-----

## Why doesn't `u == 1.4D0` produce the expected result? {: #fp-equality }
This has to do with the representation of real values as (binary) floating point values.

See [[Floating point arithmetic]].

-----

## How do I read until the end of a file (EOF)? {: #eof }

A common [[Fortran 95]] idiom for reading lines until the
end of file is

    integer :: stat
    character(len=100) :: buf

    open(15, file='foo.txt')
    do
       read(fh, iostat=stat) buf
       if (stat /= 0) exit
       ! process buf
    end do
    close(15)

{: lang=fortran }

This example catches all conditions, not just the end of file.
To specifically catch the EOF in [[Fortran 2003]] one can use
the [[iso_fortran_env]] module and replace the `if` condition
above with

    if (stat == iostat_end) exit

{: lang=fortran }

See also:
[EOF idiom?](http://groups.google.com/group/comp.lang.fortran/browse_thread/thread/149d94a68d30fba3)
on [[comp.lang.fortran]].

-----

## How do I read a character without having the user press enter? {: #getkey }

There isn't a portable way to do this either in Fortran or C,
in short, because the terminal controls when the input is sent
to your program and by default it is buffered.  You must request
that the terminal send each key and the method for doing so is
platform-dependent.

[Clive Page's Fortran Resources](http://www.star.le.ac.uk/~cgp/fortran.html)
has a section on "Reading single keystrokes from Fortran" which provides
a couple of short C functions
([sys_keyin.c](http://www.star.le.ac.uk/~cgp/sys_keyin.c))
which can be called from Fortran to
achieve the desired behavior on most Unix systems.

John Urban also provides a
[getkey](http://home.comcast.net/~urbanjost/LIBRARY/libCLI/Getkey/getkey.html)
function, written in C and callable from Fortran.

See also: [Get Key Function?](http://groups.google.com/groups?threadm=edf0a73b-ff05-4849-90f6-2aa93445d388@v35g2000pro.googlegroups.com)
on [[comp.lang.fortran]].

-----

## How do I set the precision of real variables in Fortran 90? {: #real-precision }

See [[Real precision]].

-----

## How do I convert a numeric variable to a string, or vice-versa? {: #strnum }

There is no intrinsic procedure for converting character strings
to numerical values, or vice-versa.  However, this can be accomplished
using internal file IO.  To obtain a string representation of a numeric
variable, one can perform a formatted write to a string, just as
one does to a file.  Similarly, a formatted read from a string can
extract a numeric value.  See the [[strnum]] program for an example.

-----

## How do I read command-line arguments? {: #cmdline }

See [[Command-line arguments]].

-----

## How can I create a temporary file? {: #tmp }

    open(7, form='unformatted', status='scratch')

{: lang=fortran }

This will create a temporary file that only lives until it is closed.
It doesn't need a filename as it will not be permanently saved to disk
(although it could be stored somewhere as a temporary file).  In this
example, the unit number is 7.

The file will be deleted when the program terminates (but may not be deleted if the program terminates abnormally, i.e. crashes).
Note that it is not permitted to prevent deletion of a scratch file by closing the file using a close statement with `status='keep'`.

-----

## Can I allocate a variable length character string? {: #allocstr }

Yes, in [[Fortran 2003]].  Declare the variable and allocate with a
given length as follows:

~~~~~~~~~~~~~~~~~~ {: lang=fortran }
character(LEN=:), allocatable :: str
integer :: n

n = 27
allocate(character(LEN=n) :: str)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A [[Fortran 95]] solution is the [[iso_varying_string]] module, or the variable length string module in [[FLIBS]].

-----

## Which file extension should I use for my Fortran code? {: #fileext }

Although there are no official file extensions for Fortran code,
there are two widely established conventions.  Some use `.f` for
fixed-form source and `.f90` for [[Free form layout]].
The latter is a reference to the [[Fortran 90]] standard, when
[[Free form layout]] was introduced.  The code contained in the
file can be [[Fortran 95]], [[Fortran 2003]], etc.
Others prefer to use file extensions that indicate the standard
under which the code was written.  For example, `.f03` for
[[Fortran 2003]] code and `.f08` for [[Fortran 2008]] code.
Unfortunately, this results in a proliferation of file
extensions and some compilers may not support the newer
extensions yet.

See [[File extensions]] for more discussion on these issues.

-----

## How do I initialize an array in row-column order?{: #array_constructor }

# Initializing small 2D numeric arrays with array constructors #

Intuitively, one might assume that if one wants to initialize a
small array by rows that something like the following will work:

~~~~~~~~~~ {: lang=fortran}
   ! DOES NOT WORK
   integer :: xx(3,5)= [ 1, 2, 3, 4, 5], &
                       [10,20,30,40,50], &
		       [11,22,33,44,55]
~~~~~~~~~~

or perhaps

~~~~~~~~~~ {: lang=fortran}
   ! DOES NOT WORK
   integer :: xx(3,5)= [ [ 1, 2, 3, 4, 5], &
                         [10,20,30,40,50], &
                         [11,22,33,44,55]  ]
~~~~~~~~~~

Someday something simpler might work, but currently the following syntax
is required to specify the values in an intuitive row-column sequence
using an array constructor:

~~~~~~~~~~ {: lang=fortran}
   integer,save :: xx(3,5)= reshape([&

       1, 2, 3, 4, 5, &
      10,20,30,40,50, &
      11,22,33,44,55  &

      ],shape(xx),order[2,1])
~~~~~~~~~~

This is because __an array constructor can be used to create and assign
values only to rank-one arrays__.  To define arrays of more than one
dimension with an array constructor, you must use the **RESHAPE(3f)** intrinsic
function.

Note that the **ORDER=** option on **RESHAPE(3f)** is used to allow the values
to be specified in row-column order instead of the default behavior,
which fills columns first.

Also note that if the expressions are of type character, Fortran 95/90
requires each expression to have the same character length (there is a
common compiler extension that extends all strings to the length of the
longest value specified, but depending on it reduces portability).

## Printing small arrays in row-column format ##

When working with small arrays the issue that there is no default Fortran
routine for printing an array in row-column order becomes apparent. So
lets create a simple solution for integer arrays **(PRINT_MATRIX_INT(3f))**:


~~~~~~~~~~ {: lang=fortran}
   program demo_array_constructor ! initializing small arrays
   implicit none
   integer,save :: xx(3,5)= reshape([&

       1, 2, 3, 4, 5, &
      10,20,30,40,50, &
      11,22,33,44,-1055  &

    ],shape(xx),order=[2,1])

   call print_matrix_int('xx array:',xx)

   contains

   subroutine print_matrix_int(title,arr)
   implicit none

   character(len=*),parameter::ident= "@(#)print_matrix_int(3f) - print small 2d integer arrays in row-column format"

   character(len=*),intent(in)  :: title
   integer,intent(in)           :: arr(:,:)
   integer                      :: i
   character(len=:),allocatable :: biggest

      write(*,*)trim(title)                                                 ! print title
      biggest='           '                                                 ! make buffer to write integer into
      write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2         ! find how many characters to use for integers
      biggest='(" > [",*(i'//trim(biggest)//':,","))'                       ! use this format to write a row
      do i=1,size(arr,dim=1)                                                ! print one row of array at a time
         write(*,fmt=biggest,advance='no')arr(i,:)
         write(*,'(" ]")')
      enddo

   end subroutine print_matrix_int

   end program demo_array_constructor
~~~~~~~~~~

Results:

     xx array:
     > [  1,  2,  3,  4,  5 ]
     > [ 10, 20, 30, 40, 50 ]
     > [ 11, 22, 33, 44, 55 ]

We could do a more robust version that handles REAL and COMPLEX values
as well as NaN values, but it has already been done.  If you need to
print a variety of small matrices see:

    dispmodule(3f), "A Fortran 95 module for pretty-printing matrices".
    Kristjan Jonasson, Department of Computer Science,
    School of Science and Engineering, University of Iceland,
    Hjardarhaga 4, 107 Reykjavik, Iceland (jonasson@hi.is).

#Initializing a 2D array using **DATA** statements

Note that **DATA** statements are very flexible, and allow for perhaps the
most intelligible way of specifying small arrays row by row. For example:

~~~~~~~~~~ {: lang=fortran}
   ! fill rows using DATA statements
   integer,save,dimension(3,5) :: gg
   data gg(1,:)/  1,  2,  3,  4,  5 /
   data gg(2,:)/ 10, 20, 30, 40, 50 /
   data gg(3,:)/ 11, 22, 33, 44, 55 /
~~~~~~~~~~

There are other ways to use a **DATA** statement to fill in row-column order,
including use of the **SIZE(3f)** function and an implied-DO:

~~~~~~~~~~ {: lang=fortran}
   ! use implied-DO so data can be declared in row-column order
   integer, dimension(3,5) :: ff
   DATA (( ff(J,I), I=1,size(ff,dim=2)), J=1,size(ff,dim=1)) / &
      01,02,03,04,05, &
      10,20,30,40,50, &
      11,22,33,44,55  /
~~~~~~~~~~

## Initializing a 2D array from a vector using EQUIVALENCE ##

Sometimes instead of using **RESHAPE(3f)** you will see someone initialize a
vector and then equivalence it to a multi-dimensional array; especially
if the code has a reason to access the data as both a vector and a matrix:

~~~~~~~~~~ {: lang=fortran}
   ! multi-dimensional row1, row2, .... by equivalence
   integer,parameter :: d1=3,d2=5
   integer           :: ee(d1,d2)
   ! note that the DATA statements could be used to initialize the array instead
   integer           :: e(d1*d2) =[1,10,11, 2,20,22, 3,30,33, 4,40,44, 5,50,55]
   equivalence       (e(1),ee(1,1))
~~~~~~~~~~

## Notes ##

Remember that for simple initializations vector statements can be used

~~~~~~~~~~ {: lang=fortran}
   real :: arr(10,20)=0.0
   ! array constructors can be used to define constants, not just vectors
   integer,parameter :: ii(10,10)=[(i,i=1,size(ii))] ! odd numbers using implied-DO
~~~~~~~~~~

and that if things are too complicated you can just set the values in the executable
body of the code.
~~~~~~~~~~ {: lang=fortran}
   program test_random_number
   real :: r(5,5)
      call random_number(r)
   end program
~~~~~~~~~~

Remember that a DATA statement does not require that all values be initialized, whereas an array constructor does; and that you cannot initialize values multiple times and be standard-conforming.
So be very careful when using DATA statements that you initialized everything you wanted to.
-----

## External Links

* [The Fortran FAQ](http://www.faqs.org/faqs/fortran-faq/)
* [Fortran FAQ Wikibook](http://en.wikibooks.org/wiki/Fortran/FAQ)
