# How do I initialize an array in row-column order?

## Initializing small 2D numeric arrays with array constructors #

Intuitively, one might assume that if one wants to initialize a
small array by rows that something like the following will work:
```fortran
     ! DOES NOT WORK
     integer :: xx(3,5)= [ 1, 2, 3, 4, 5], &
                       [10,20,30,40,50], &
  		       [11,22,33,44,55]
```
or perhaps
```fortran
     ! DOES NOT WORK
     integer :: xx(3,5)= [ [ 1, 2, 3, 4, 5], &
                         [10,20,30,40,50], &
                         [11,22,33,44,55]  ]
```
Someday something simpler might work, but currently the following syntax
is required to specify the values in an intuitive row-column sequence
using an array constructor:
```fortran
      integer,save :: xx(3,5)= reshape([&

       1, 2, 3, 4, 5, &
      10,20,30,40,50, &
      11,22,33,44,55  &

      ],shape(xx),order[2,1])
```
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
```fortran
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
```
Results:
```text
     xx array:
     > [  1,  2,  3,  4,  5 ]
     > [ 10, 20, 30, 40, 50 ]
     > [ 11, 22, 33, 44, 55 ]
```
We could do a more robust version that handles REAL and COMPLEX values
as well as NaN values, but it has already been done.  If you need to
print a variety of small matrices see:

     dispmodule(3f), "A Fortran 95 module for pretty-printing matrices".
     Kristjan Jonasson, Department of Computer Science,
     School of Science and Engineering, University of Iceland,
     Hjardarhaga 4, 107 Reykjavik, Iceland (jonasson@hi.is).

# Initializing a 2D array using **DATA** statements

Note that **DATA** statements are very flexible, and allow for perhaps the
most intelligible way of specifying small arrays row by row. For example:

      ! fill rows using DATA statements
      integer,save,dimension(3,5) :: gg
      data gg(1,:)/  1,  2,  3,  4,  5 /
      data gg(2,:)/ 10, 20, 30, 40, 50 /
      data gg(3,:)/ 11, 22, 33, 44, 55 /

There are other ways to use a **DATA** statement to fill in row-column order,
including use of the **SIZE(3f)** function and an implied-DO:

      ! use implied-DO so data can be declared in row-column order
      integer, dimension(3,5) :: ff
      DATA (( ff(J,I), I=1,size(ff,dim=2)), J=1,size(ff,dim=1)) / &
         01,02,03,04,05, &
         10,20,30,40,50, &
         11,22,33,44,55  /

## Initializing a 2D array from a vector using EQUIVALENCE ##

Sometimes instead of using **RESHAPE(3f)** you will see someone initialize a
vector and then equivalence it to a multi-dimensional array; especially
if the code has a reason to access the data as both a vector and a matrix:

      ! multi-dimensional row1, row2, .... by equivalence
      integer,parameter :: d1=3,d2=5
      integer           :: ee(d1,d2)
      ! note that the DATA statements could be used to initialize the array instead
      integer           :: e(d1*d2) =[1,10,11, 2,20,22, 3,30,33, 4,40,44, 5,50,55]
      equivalence       (e(1),ee(1,1))

## Notes ##

Remember that for simple initializations vector statements can be used

      real :: arr(10,20)=0.0
      ! array constructors can be used to define constants, not just vectors
      integer,parameter :: ii(10,10)=[(i,i=1,size(ii))] ! odd numbers using implied-DO

and that if things are too complicated you can just set the values in the executable
body of the code.

      program test_random_number
      real :: r(5,5)
         call random_number(r)
      end program

Remember that a DATA statement does not require that all values be
initialized, whereas an array constructor does; and that you cannot
initialize values multiple times and be standard-conforming.  So be very
careful when using DATA statements that you initialized everything you
wanted to.
