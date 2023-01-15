**Q**:  How do I create numbered file names such as out_1.txt, out_2.txt, etc.?

**A**:  Use an "internal write" to create the file names, which is when you
        write into CHARACTER variables instead of files. for example

            character(len=4096) :: file_name
            write (file_name,"('out_',i0,'.txt')") i

   The string prefix and suffix may be variables as well:

        write (file_name,"(a,i0,a)") 'out_',i,'.txt'

   A format descriptor such as "I0.4" could be used instead of "I0" to
   add leading zeros to the numeric labels up to the specified number
   of digits.

   An extended working example that generates sequentially numbered
   files and then deletes them :
```fortran
program showit
implicit none
integer :: num, iostat, lun
character(len=:), allocatable :: prefix, suffix
character(len=4096) :: filename, iomsg
   prefix='out_'
   suffix='.txt'
   do lun=10, 12
      !
      ! create a filename for the specified logical unit number
      write(filename, fmt = '(a, i0, a)')prefix, lun, suffix
      !
      ! open a new formatted file for writing
      open(unit=lun, file=filename, form='formatted', &
      & action='write', status='new', access='sequential', &
      & iostat=iostat, iomsg=iomsg)
      !
      ! check the open for errors
      if(iostat /= 0)then
         write(*, *)'<error>cannot open file ', trim(filename), &
         & ':', trim(iomsg)
         stop 1
      else
         write(*, *)'opened file ', trim(filename), ': unit=', lun
      endif
   enddo
   !
   ! clean up the debris from the example program
   !
   do lun=10, 12
      close(unit=lun, status='delete', iostat=iostat, iomsg=iomsg)
      if(iostat /= 0)then
         write(*, *)'<error>did not clean up ', trim(filename), &
         & ':', trim(iomsg)
      else
         write(*, *)'closed file ', trim(filename), ': unit=', lun
      endif
   enddo

end program showit
```
output :
```text
 opened file out_10.txt: unit=          10
 opened file out_11.txt: unit=          11
 opened file out_12.txt: unit=          12
 closed file out_12.txt: unit=          10
 closed file out_12.txt: unit=          11
 closed file out_12.txt: unit=          12
```

