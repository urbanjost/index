#c*onformance and elemental functions

The Fortran standard requires the programmer to make sure that arrays
conform for elemental functions.

So something like

    [.true.,.true.].eqv.[.true.,.true.,.true.]

does not have a defined behavior. It might cause an error, or return T,T
because it only compares the two arrays up to the length of the shortest
vector, or T,T,F because the vectors are not the same size.  This often
generates an error at compile time for values know at compile time, but
I have not found a compiler that catches this with allocatable arrays,
for instance.

gfortran 8.0.3 returns T,T,T and ifort 19.1 returns T,T,T,F,F for the
following program, for example.

    program testit
    integer,allocatable :: i(:)
       i=[0,1,2,3,4,5]
       write(*,*)i.eq.[0,1,2]
    end program testit
