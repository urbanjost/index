#Convertings between Strings and numbers

Type conversion from strings to number and vice-versa is most commonly done
with internal reads and writes. You can read and write from a CHARACTER 
variable as with a file. There is no direct way to read or write from an
allocatable array currently, so you have to use a variable of a defined
length. 

##Converting simple values
```fortran
     program internal_io
     implicit none
     character (len=80) :: line
     real               :: value
     integer            :: ios
     character(len=256) :: message
     line='123.4e2'
     read(line,*,iostat=ios,iomsg=message) value
     if(ios.ne.0)then
        write(*,*)'ERROR: could not obtain numeric value from '//trim(line)
        write(*,*)'     : '//trim(message)
     else
        write(*,*)'VALUE=',value
     endif
     write(line,'("[",g0,"]")')1.0d0/3.0d0
     write(*,*)'line=',trim(line)
     end program internal_io
```
  Results:
```text
    VALUE=   12340.00    
    line=[.3333333333333333]
```
<!--
##Lists

##Reading numbers from command arguments

##Expressions

##Interpreters
-->
