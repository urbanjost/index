# Writing to stderr

Since f2003, you can use the variable ERROR_UNIT from the intrinsic
module ISO_FORTRAN_ENV to get the unit to use to write to stderr.

```fortran
    program demo_stderr
    USE ISO_FORTRAN_ENV, ONLY : ERROR_UNIT ! access computing environment ; Standard: Fortran 2003
    implicit none
    write(ERROR_UNIT,*)'Error: program will now stop'
    stop 4
    end program demo_stderr
```
