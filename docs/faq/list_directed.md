# List-directed output

compact or fixed-width list

   The Fortran standard allows the compilers a great deal of latitude in how
   list-directed output is formatted. List-direct output might be printed in
   the minimum amount of space required, making short lists of values compact
   and easy to read. Alternatively, each intrinsic numeric type may be given
   the space required for the largest or smallest value of that type, making
   tables of values line up better. For example,
```fortran
 print*,(i,i=1,6)
 end
```
   might give
```text
 1 2 3 4 5 6
```
   or
```text
        1           2           3           4           5           6
```

   There are pros and cons for both. Assume:
```fortran
  print *,(huge(0),i=1,6)
  print*,(i,i=1,6)
  print*,(i,i=10000,60000,10000)
  end
```
   I would argue that
```text
 2147483647  2147483647  2147483647  2147483647  2147483647  2147483647
         1           2           3           4           5           6
     10000       20000       30000       40000       50000       60000
```
   is more readable than:

```text
 2147483647 2147483647 2147483647 2147483647 2147483647 2147483647
 1 2 3 4 5 6
 10000 20000 30000 40000 50000 60000
```

   There are simple formats that approximate list-directed output
```fortran
  character(len=10) :: c = "some text"
  i = 7
  b = 11.356
  print '(*(g0:,1x))', i,b,c
  end
```
   So the formatting is simple with some control over spacing.

   Here is another variation on this:
```fortran
  print'(*(xg0))',(i,i=1,6)
  end
```
Example program using various output methods

   This example program can be used as a seed to explore list-directed
   output, using the "g0" format, and formatted output. Note that
   list-directed output and namelist has a lot of latitude as to where to
   place line-breaks, and that the g0 format does not place parenthesis
   around complex values. And remember list-directed output is always
   prefixed with one space.
```fortran
 program almost_list_directed
 implicit none
 logical           :: l=.true.
 character(len=10) :: c='XXXXXXXXXX'
 real              :: r=12.3456
 integer           :: i=789
 complex           :: x=(12345.6789,9876.54321)
 doubleprecision   :: d= 123456789.123456789d0
 namelist /nlist/ l,c,r,i,x,d

    write(*,*)'LIST DIRECTED  ',r,i,x,d,l,c
    write(*,'(*(ss,g0:,1x))')   ' GENERAL FORMATTED ',r,i,x,d,l,c
    write(*,'(a,10x,f9.4,1x,i4,1x,"(",f11.4,",",f11.4,")",1x,f19.8,1x,l3,1x,a)') 'FORMATTED',r,i,x,d,l,c
    write(*,nlist)

 end program almost_list_directed
```

   One example output (using gfortran):
```text
  LIST DIRECTED     12.3456001             789 (  12345.6787    ,  9876.54297    )   123456789.12345679      T XXXXXXXXXX
  GENERAL FORMATTED 12.3456001 789 12345.6787 9876.54297 123456789.12345679 T XXXXXXXXXX
 FORMATTED          12.3456  789 ( 12345.6787,  9876.5430)  123456789.12345679   T XXXXXXXXXX
 &NLIST
  L=T,
  C="XXXXXXXXXX",
  R=  12.3456001    ,
  I=        789,
  X=(  12345.6787    ,  9876.54297    ),
  D=  123456789.12345679     ,
  /
```
Revised on Sat Feb 3 22:11:40 EST 2018 by JSU
