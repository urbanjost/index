#How do you get the size of a file?

   Fortran does not have an intrinsic that returns the size of a file,
   but with a modern compiler the answer to this question has gotten
   much simpler than it used to be. For most external files you can
   query the size with an _INQUIRE_(3f):
 
    use :: iso_fortran_env, only : FILE_STORAGE_SIZE
    implicit none
    character(len=:),allocatable :: filename
    integer                      :: file_size

    filename='test.txt'

    INQUIRE(FILE=filename, SIZE=file_size)  ! return -1 if cannot determine file size

    write(*,*)'size of file '//filename//' is ',file_size * FILE_STORAGE_SIZE /8,' bytes'
    end

   There are some dusty corners where this might not return what you
   expect on some systems, especially if the file is currently open as a
   direct access or stream file or is a soft link; but essentially
   every method has problems with special file types. I have not had
   the INQUIRE(SIZE=...) statement fail on regular external files.
   
#Other methods

   If the _INQUIRE_(3f) statement does not yet work with _SIZE=_ in your
   programming environment, there are several alternative methods for
   obtaining system file information (some work for far more than just
   file size), each with advantages and disadvantages:
   
     * using non-standard extensions
     * opening a file at EOF and reading position
     * call C routines via ISO_C_BINDING module
     * calling system command and reading command output
     * reading the entire file and counting line lengths and lines

##Using non-standard extensions

   If you are not concerned about portability many compilers
   support at least a subset of the _POSIX_ system interface routines.
   Look for routines like _STAT_(3f) or _PXFSTAT_(3f).

##Opening a file at end-of-file and reading position

   Depending on what vintage of fortran you have available, if you
   _OPEN_(3f) the file with _POSITION='APPEND'_ and then use _INQUIRE(3f)_
   to query the position of a file you get the size of the file assuming
   it is a basic external file. You cannot use this to query the size
   of some types of files such as files being piped to your process or
   other files where positioning the files to their end position really
   does not apply.
   
   So far (f2008) it is not standard to open a file that is already open, so the 
   example _FILESIZE_(3f) procedure in the following example has to be used on 
   files that are not open. The routine could be extended to use _INQUIRE_(3f) to
   detect this (by checking if the file is already open).

    program file_size
    implicit none
    character(len=:),allocatable :: filename
    integer                      :: filename_length, ios, nchars, count, ierr
    
    do count = 1,command_argument_count()
    
       ! get filename from command line
       call get_command_argument(number=count,length=filename_length,status=ios)     ! get command line length
       if(ios.ne.0)then
          stop '*file_size* ERROR: filenames must be specified on command line'
       endif
       allocate(character(len=filename_length) :: filename)   ! allocate string big enough to hold command line
       call get_command_argument(number=count,value=filename) ! get command line as a string
       filename=trim(adjustl(filename))                       ! trim leading spaces just in case
       if(filename.eq.'')then
          write(*,'(a)')'*file_size* ERROR: blank filename '
          cycle
       endif
    
       ! call routine that should get size of file in bytes
       call filesize(filename,nchars,ierr)
       if(ierr.ne.0)then
          write(*,'("*file_size* ERROR: ierr=",i0," for file ",a)')ierr,filename
       elseif(nchars.le.0)then
          write(*,'(a)')'empty file '//trim(filename)
       else
          write(*,'(a," is ",i0," bytes")')trim(filename),nchars
       endif
    
       deallocate(filename)
    enddo
    end program file_size
    
    subroutine filesize(filename,nchars,ierr)
    implicit none
    character(len=*),intent(in) :: filename
    integer,intent(out)         :: nchars
    integer,intent(out)         :: ierr
    character(len=256)          :: message
    integer                     :: lun, ios
    
       nchars=0
       ierr=0
    
       ! open named file in stream mode positioned to append
       open (newunit=lun,     &
       & file=trim(filename), &
       & access='stream',     &
       & status='old',        &
       & position='append',   &
       & iomsg=message,       &
       iostat=ios)
    
       if(ios.eq.0)then                  ! if file was successfully opened
          ! get file size in bytes and position file to beginning of file
          inquire(unit=lun,pos=nchars)   ! get number of bytes in file plus one
          nchars=nchars-1                ! opened for append, so subtract one to get current length
       else
          write(*,'("*error*:",a)')message
       endif
    
       ierr=ios
    end subroutine filesize

###Example output

    file_size *

    empty file asdf
    block_comments.md is 31712 bytes
    character_array_initialization.html is 7239 bytes
    comments.html is 5713 bytes
    compare_arrays.html is 6337 bytes
    contained.html is 4272 bytes
    faq.html is 2782 bytes
    file_size.ff is 3464 bytes
    file_size.ff~ is 3483 bytes
    nan.md is 34 bytes
    row-column.html is 7982 bytes
    scratch.html is 28136 bytes
    zero_elements.html is 7485 bytes

##call C routines via ISO\_C\_BINDING module

With modern Fortran it is relatively standard and portable to
call C routines. There is an extensive interface in module
_M\_sytem_(3f) in the [GPF]( http://www.urbanjost.altervista.org/LIBRARY/libGPF/GPF.html)(General Purpose Fortran)
collection that includes the procedure *SYSTEM\_STAT*(3f) which, among other 
things, calls _stat_(3c) and returns system file information including file size.

## Calling system command and reading command output

   Although what system commands are available varies between programming
   environments, you can generally call a system command that prints the
   file size (such as stat(1), ls(1), dir(1), find(1), wc(1), ...)
   and read the command input.

   The stat(1) command on Unix and GNU/Linux systems can be used to return 
   many external file attributes. This is just a simple example. Note that
   a more robust method for getting a scratch file than just using the name
   "_scratch" would be needed in any production version.

      program read_command
      implicit none
      character(len=:),allocatable :: filename,cmd
      character(len=256) :: message=''
      integer            :: lun, ios=0, nchars=0, icmd, iexit
         ! assume a file called "test.txt" exists
         filename='test.txt'
         ! system command to execute
         cmd="stat --dereference --format='%s' "//filename//'>_scratch'
         ! if you do not have execute_command_line(3f) look for a system(3f) procedure
         call execute_command_line(command=cmd,exitstat=iexit,cmdstat=icmd,cmdmsg=message)
         if(iexit.ne.0.or.icmd.ne.0)then
            write(*,*)'*read_command* error '//trim(message)
         else
            open(newunit=lun,file='_scratch',iostat=ios)  ! you would want to trap errors here
            if(ios.eq.0)then
               read(lun,*)nchars                  ! you would want to trap errors here
            endif
            !!close(unit=lun,status='delete',iostat=ios)
         endif
         write(*,'(a,i0,a)')' file '//filename//' is ',nchars,' bytes'
      end program read_command

####Example output

     file test.txt is 938 bytes
     

##Reading the entire file and counting line lengths and lines

  One reason you might do this even with a modern Fortran version 
  is to get the number of lines in a sequential file. For example:

    program count_lines    
    implicit none
    integer :: line_count, ios
       line_count=0
       open(unit=10,file='test.txt')
       do
          read(10,*,iostat=ios) ! note there is no list of variables
          if(ios.ne.0)exit
          line_count=line_count+1
       enddo
       write(*,*)'file has ',count,' lines'
    end program count_lines    

   In modern Fortran in addition to _INQUIRE_(3f) with _SIZE=_
   you can open a file as a stream and read one character at a time and
   (assuming you know what the line terminator is for the file) count
   lines and words and characters; but in older FORTRAN there were no
   standard _ADVANCE='NO' options on READ(3f), no stream I/O, and no
   _INQUIRE_(3f) parameters to easily give you file size.

   The trick in older Fortran versions was generally to open the file as
   a direct-access file with RECL=1 on the _OPEN_(3f). One problem was
   that the units for RECL were not always one byte; they were often 4
   bytes or more, but there was usually a compiler option to make the unit
   1 byte. Then you just read the file from beginning to end.  I would
   replace any such code with the _INQUIRE_(3f) statement using _SIZE=_.

           CHARACTER C , FILENAME*256
           ISIZE=1
           FILENAME='test.txt'
           OPEN(10,FILE=FILENAME,IOSTAT=IOS,
          $ACCESS='DIRECT',FORM='UNFORMATTED',STATUS='OLD',RECL=1)
           IF(IOS.NE.0)THEN
              WRITE(*,*)'I/O ERROR: ',IOS, ' for ',FILENAME
              STOP
           ENDIF
     1     CONTINUE
           READ(10,IOSTAT=IOS,REC=ISIZE,ERR=999)C
           IF (IOS.NE.0) THEN
              WRITE(*,*)'ERROR ',IOS
              STOP      ! Some sort of error. 
           ELSE 
              ISIZE=ISIZE+1
           END IF 
           GOTO 1 
     999   CONTINUE
           ISIZE=ISIZE-1
           WRITE(*,*)'File ',FILENAME(:JULEN(FILENAME)),' is ',ISIZE,' bytes'
           END
           INTEGER FUNCTION JULEN(STRING)
     C     @(#) return position of last non-blank character in "string".
     C     if the string is blank, a length of 0 is returned.
     C
           CHARACTER STRING*(*)
           CHARACTER NULL*1
           INTRINSIC LEN
           NULL=CHAR(0)
           ILEN=LEN(STRING)
           IF(ILEN.GE.1)THEN ! CHECK FOR NULL STRINGS
              DO 10 I10=ILEN,1,-1
              IF(STRING(I10:I10).NE.' '.AND.STRING(I10:I10).NE.NULL)THEN
                 JULEN=I10
                 RETURN
              ENDIF
     10       CONTINUE
           ENDIF
           JULEN=0
           RETURN
           END
