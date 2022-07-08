#Stream I/O on stdin and stdout

Fortran 2003 introduces stream I/O for Fortran; but does not 
supply a way to make stdin and stdout stream files. One method
is to call C routines to do the I/O.

It is strongly suggested you not mix I/O between Fortran and C on the
same units.

Callling C from Fortran is less problematic with the Fortran 2003 ISO\_C\_BINDING,
so this example shows that method.

##Example

This shell script makes the C routines getkeyC and putkeyC, a Fortran binding to the C
routines, and an example program; and then builds and executes the program.

     #!/bin/sh
     ####To get stream I/O out of stdin and stdout, make a getc and putc callable from Fortran
     cat > getkey.c <<\EOF
     #include <stdlib.h>
     char getkeyC(void) {
     /* @(#) Driver for reading a character from stdin */
             char c;
             read(0, &c, 1);
             return(c);
     }
     int putkeyC(char c) {
     /* @(#) Driver for writing a character to stdout */
             write(1, &c, 1);
             return(c);
     }
     /******************************************************************************/
     EOF
     ################################################################################
     cat > f2003.f90 <<\EOF
     !=======================================================================--------
     !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
     !=======================================================================--------
     ! make Fortran/C interface for C routine getkey(3C)
     module M_getkey
        use iso_c_binding
        implicit none
        public
           interface
              function getkeyI() bind(c, name='getkeyC')
                 use iso_c_binding
                 implicit none
                 integer(kind=c_char) :: getkeyI
              end function getkeyI
     
              function pkey(char) bind(c, name='putkeyC')
                 use iso_c_binding
                 implicit none
                 integer(kind=c_int) :: pkey
                 character(kind=c_char) :: char
              end function pkey
           end interface
           contains
             character*1 function gkey()
     	    gkey=char(getkeyI())
     	end function gkey
     end module M_getkey
     !=======================================================================--------
     !()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
     !=======================================================================--------
     !-------------------------------------------------------------------------------
     program test_getkey
        use M_getkey
        character :: A
        integer :: icount
        icount=0
        write(*,*)'begin striking keys to demonstrate interactive raw I/O mode'
        write(*,*)'q to quit; up to 40 characters'
        istat=pkey('|')
        do
           A=gkey()
           icount=icount+1
           istat=pkey(A)
           istat=pkey('|')
           if(A.eq.'q')stop
           if(icount.gt.40)stop
        enddo
     end program test_getkey
     EOF
     ################################################################################
     (
     exec 2>&1
     set -x
     rm -f getkey.o getkey getkey.exe
     gcc -c getkey.c
     gfortran f2003.f90 getkey.o -o getkey
     # demonstrate non-interactive behavior
     echo 'abcdefghijklmnopqrstuvwxyz'|./getkey
     ls -ltrasd getkey
     rm -f getkey.o m_getkey.mod # clean up
     rm -f getkey.c f2003.f90
     rm -f getkey.exe getkey
     )|tee getkey.log
     exit
## Alternatives

In some cases using non-advancing I/O on stdin and stdout will work.

In SOME programming environments you can trick stdin and stdout to be direct access
files of RECL=1, and read and write on RECL length at a time. Make sure your record
length for RECL=1 is 1 byte, not some other unit like 4 bytes. There is often a compiler
switch to make the unit bytes even if that is not the default.
