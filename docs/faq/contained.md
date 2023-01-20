# Gotchas: Inheritance control for CONTAIN-ed procedures
   NOTE:
     In upcoming Fortran releases see the IMPORT directive.

   New Fortran programmers using a contained procedure often do not know
   that a CONTAIN-ed procedure has access to all the variables in the
   parent procedure unless the variables are explicitly declared in the
   parent procedure. Even experienced programmers can accidently corrupt
   parent procedure values.
   
   Although there has been discussion about allowing IMPORT to be extended
   to close this oversight in F2020 (seems like a very good idea to me),
   currently it is easy to accidentally corrupt a host-associated variable,
   because there is no simple way to turn off inheritance in a CONTAIN-ed
   procedure.
   
   A CONTAIN-ed procedure may be desirable because it provides automatic 
   interfaces and creates a private routine much like a MODULE provides,
   but much more simply. And since a CONTAIN-ed procedure it only usable
   by the parent proceddure the compiler it free to agressively make optimizations such
   as in-lining the CONTAIN-ed routine.
   
   But a CONTAIN-ed procedure inherits everything the parent sees, with 
   some restrictions. When desired this can be very useful; but it is also
   prone to errors.
   
   So when you do not want to inherit values or change values from the parent
   you must be very careful to declare all the variables. Using a naming
   convention such as starting local variables with the name of the routine
   can be helpful.
   
## Sample program to test your understanding of inheritance with ...

      program testit
      implicit none
      real :: A
         A=10
         call printit1(); write(*,*)A
         call printit2(); write(*,*)A
         call printit2(); write(*,*)A
         A=30.0
         call printit3(); write(*,*)A
      contains
      
      subroutine printit1()
      ! this routine uses the same A variable as in the parent
         write(*,*)A
         A=A+1.0  ! the parent variable is changed
      end subroutine printit1
      
      subroutine printit2()
      ! this routine uses the local variable A because it was declared
      ! in the subroutine
      real :: A=20  ! this A is now a unique variable
         write(*,*)A
         A=A+2.0
      end subroutine printit2
      
      subroutine printit3()
      implicit none  ! this does NOT turn off inheritance
         write(*,*)A
         A=A+3.0
      end subroutine printit3
      
      end program testit

## Expected Output

   10.0000000    
   11.0000000    
   20.0000000    
   11.0000000    
   22.0000000    
   11.0000000    
   30.0000000    
   33.0000000    
