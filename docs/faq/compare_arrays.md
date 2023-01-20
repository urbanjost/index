# How do I compare arrays?

You cannot use a simple compare of two arrays in an *IF*(3f), because a
comparison of two arrays returns a logical array, not a single scalar
logical. So this *IF*(3f) statement will return a compiler error:
```fortran
    integer :: A(3)=[1,2,3], B(3)=[1,2,3]
    write(*,*)A.eq.B  ! This returns an array
    if(A.eq.B)then    ! SO THIS WILL NOT WORK
       write(*,*) "A and B are equal"
    endif
    end
```
## ANY(3f) and ALL(3f) are probably what you are looking for

There is not an specific intrinsic function to compare arrays in Fortran.
but you can use the very flexible and generic *ALL*(3f) and *ANY*(3f)
functions:
```fortran
    integer :: A(3)=[1,2,3], B(3)=[1,2,3]
    write(*,*)A==B    ! Note this returns an array, not a scalar
    if(all(A.eq.B)) then
       write(*,*) "A and B are equal"
    else
       write(*,*) "A and B are NOT equal"
    endif

    write(*,*) all(A.eq.B)
    write(*,*) all(A.eq.B+2)

    end
```

Results:
```text
    T T T
    A and B are equal
    T
    F
```
which works for all arrays as long as they have the same type
and length.

## DO-ing it yourself

Of course, you can loop through the elements with a DO(3f):
```fortran
    integer :: A(3)=[1,2,3], B(3)=[1,2,3]
    logical :: answer
    COMPARE: block
       integer :: i
       answer=.false.
       if(size(a).ne.size(b)) exit COMPARE
       do i=1,size(a)
          if(A(i).ne.B(i)) exit COMPARE
       enddo
       answer=.true.
    endblock COMPARE
    write(*,*)'equality of A and B is ',answer
    end
```
Results:
```text
    equality of A and B is  T
```
Writing a function and returning *.TRUE.* or. *.FALSE.* is straight-forward,
but for each type of array there has to be another function or you
have to use CLASS(*).

As an example, an alternative lacking the generic character of *ALL*(3f)
or *ANY*(3f) is:
```fortran
    integer :: A(3)=[1,2,3], B(3)=[1,2,3]
 
    if(equal(A,B))then
        write(*,*) "A and B are equal"
     else
        write(*,*) "A and B are NOT equal"
    endif
 
    contains
    pure logical function equal( array1, array2 )
    integer,dimension(:),intent(in) :: array1, array2
    integer                         :: i
    
    equal=size(array1)==size(array2)
 
    if(equal) then
       do i=1,size(array1)
          equal=array1(i) == array2(i)
          if(.not.equal)exit
       enddo
    endif
 
    end function equal
 
    end
```
Results:
```text
    A and B are equal
```
## Be careful when comparing floating-point values

If the arrays are *INTEGER* or *CHARACTER*, then the comparison can be exact.
However, if the arrays contain floating-points values such as  *REAL*,
*DOUBLEPRECISION* or *COMPLEX* variables, you should consider using a suitably
small tolerance when comparing values. For example:
```fortran
    !real :: A(3)=[1.0,2.0,3.0], B(3)=[1.0,2.0,2.9999999999999]  ! this might test as equal
    real :: A(3)=[1.0,2.0,3.0], B(3)=[1.0,2.0,2.999999]         ! this should be close enough
    real :: tolerance=0.00001  ! just a sample tolerance

    if(all(A==B))then  ! testing for exact matches can be problematic
       write(*,*) "A and B are equal"
    elseif (all( abs(A - B) < tolerance) )then
       write(*,*) "A and B are close enough to equal"
    else
       write(*,*) "A and B are NOT equal"
    endif
    end
```
Most modern compilers do a good job at allowing programmers to compare
floating point values, but there are several good sources on why you want
to compare using a tolerance and how to determine what that tolerance
should be.

## *ANY*(3f) and *ALL*(3f) may not be the most efficient method

The *ANY*(3f) and *ALL*(3f) functions may generate a logical array the size of
the input arrays or always test all elements; depending on how they are
implemented.  This could cause comparisons of large arrays to require a
significant amount of memory or do unneeded tests. The functions may or
may not take advantage of parallel or vector processing when available. So
if you are doing many array comparisons of very large arrays you might want
to create your own functions, but I suspect most *ANY*(3f) and *ALL*(3f) functions
will perform as well or better than your own routines. 

If anyone has examples using *Coarrays*, *OpenMP*, or *MPI* that would be useful.
Timing information on various methods for large arrays would also be very
interesting. If I get the time I will try to add that.
