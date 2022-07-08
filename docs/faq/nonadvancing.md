# NON-ADVANCING I/O #

The non-advancing I/O in Fortran 90 is __not__ the same as stream I/O.
Non-advancing I/O is still record-based (and there is a limit on the
length of a line!). Record-based I/O can be useful. You can use formats
that position within a partial record. You can backspace on most standard
files. You can mix advancing and non-advancing I/O on a file without
closing and reopening it. To efficiently impliment these features pretty
much defines that the file must be record-based (especially on systems
typically without system-defined file structures like Unix).

## EXPLANATION ##
   
So non-advancing Fortran I/O supports some features not supported by
Fortran stream I/O by being record-based.  But this means there is a
record length limit for non-advancing I/O, whereas true stream I/O has
no predefined record structure.  For many compilers the default record
length is very large (e.g., 2147483647), essentially giving much of the
appearance of stream I/O for files smaller than that size. But the limit
may be much smaller.

One can query the record length:

    use,intrinsic :: iso_fortran_env, only : ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT
    ! OUTPUT_UNIT is almost always six, but that is not required by the standard
    inquire(unit=OUTPUT_UNIT, recl=i)
    print *, 'recl=', i, ' stdout unit=',OUTPUT_UNIT
    end

In some programming environments it is possible to set the record length up
to a system-defined limit:
   
    character(len=256) :: message
    open(unit=6, recl=2147483646)
    inquire(unit=6, recl=i,iomsg=message,iostat=ios)
    if(iostat.ne.0)then
       write(*,*)'ERROR:'//trim(message)
    endif
    write(*,*)'recl=',i
    end

However, in some programming environments the record length is predefined,
especially for preconnected units such as stdout, and the record length
cannot be changed.

## EXAMPLES ##

    do i=1, 512
       write (unit=6,fmt='(a)',advance='no') 'X'
    enddo
    end
   
We might expect this program to print 512 X's in a row on one
line. However, if the record length for unit 6 is less than 512 the
output will appear on multiple lines.

Note that unit 6 and unit * are not necessarily the same. Although they
both may point to the default output device.  Each could keep track of
the current location in its own record separately, for example. Therefore
we advise choosing one default unit and sticking with it (either use *
or OUTPUT_UNIT(almost always unit 6).

Note that you can close and open the numeric pre-connected file and
call INQUIRE on it; and usually change some attributes of the OPEN,
unlike the * unit.

So what will this program produce?

    program nonadvancing
    implicit none
    integer :: i
    integer :: ios
    character(len=256) :: message
    
    inquire(unit=6, recl=i)
    print *, 'initial stdout recl=', i
 
    open(unit=10)
    inquire(unit=10, recl=i)
    print *, 'initial regular file recl=', i
    
    ! can you change stdout?
    open(unit=6, recl=80,iostat=ios,iomsg=message)
    if(ios.ne.0)then
       write(*,*)'ERROR:'//trim(message)
    endif
    inquire(unit=6, recl=i)
    print *, 'stdout recl=', i
    
    open(unit=6, recl=huge(0),iostat=ios,iomsg=message)
    if(ios.ne.0)then
       write(*,*)'ERROR:'//trim(message)
    endif
    inquire(unit=6, recl=i,iostat=ios,iomsg=message)
    print *, 'stdout recl=', i
    if(ios.ne.0)then
       write(*,*)'ERROR:'//trim(message)
    endif

    ! do you know whether positioning is relative to the entire line
    ! or the current partial record? Compilers handled this differently
    ! when non-advancing I/O was initially introduced. 
    ! so what will this write?
    write(*,101)'xxxxxx', 'yyyyy'
    101 format(a,t4,a)
    write(*,101,advance='no')'AAAAAA', 'BBBBB'
    write(*,101,advance='no')'CCCCCC', 'DDDDD'
    write(*,'(a)')'EEEEEE'
   
    end program nonadvancing
