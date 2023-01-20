# "array=[]" will not work

If you want to initialize an array to have zero elements, something like

    integer,allocatable :: ints(:)
    ints=[]                         ! WILL NOT WORK
    ints=[ints,10] ! add element to array
    ints=[ints,20] ! add element to array
    ints=[ints,30] ! add element to array
    write(*,*) ints
    end

will produce a compiler error along the lines of  

     Error: Empty array constructor at (1) is not allowed

In Fortran a general rule is that the right-hand side of an assignment
must be evaluated first, and the simple expressions "[]" is incomplete
because there is no way to determine the type of the expression. 
Fortran does not have a NULL type. So you need to use something like

    ints=[integer ::]
    !     ^^^^^^^^^^

or

    allocate(ints(0))

For character variables something like

    chars=[character(len=0) :: ]

or  

    allocate(character(len=0) :: chars(0))

would allocate zero character elements of zero length.

The following example program creates a file with various length lines
and then reads it in to an array, allocating and reallocating the array
that starts off with zero elements until the file has been read into the
array. The length of the elements is adjusted to the longest length read
so far with each read. 

Assuming you have a *small* file called "lines.txt" (note this does a lot
of reallocations and is storing the file in memory) here is an example 
program that starts with an array called WORDS which is initially allocated
as having zero character elements of zero length and ends up having the
entire contents of the file "lines.txt".

    program dusty_corner
    implicit none
    character(len=:),allocatable :: words(:)
    character(len=4096)          :: line     ! ASSUMPTION that LINE is large enough for any input file
    integer                      :: big=0,ios,ipos,iposback=1,ilen

    words=[character(len=0)::]               ! Allocate zero-size array
    !!words=[]  ! is not standard because cannot tell type of a null array, but intuitively seems like
    !!          ! it should be a special case where LHS is used to determine type.
    open(10,file='lines.txt')

    INFINITE: do
       read(10,'(a)',iostat=ios)line
       if(ios.ne.0)exit INFINITE
       write(*,*)'LINE=',trim(line)
       ! get length of last line read ASSUMPTION: reading from standard disk file
       inquire(10,pos=ipos) ! get position of file being read
       ilen=ipos-iposback-1 ! find change from last read, which should be length of line read
       iposback=ipos        ! store end of last line

       big=max(big,ilen)    ! length of longest line read so far
       words=[ CHARACTER(LEN=big) :: words, line ] ! words reallocated to LEN=BIG and new line added
    enddo INFINITE

    write(*,*)'FINAL ARRAY' ! should print entire file
    write(*,'("[",a,"]":)')words
    end program dusty_corner
