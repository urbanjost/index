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

      subroutine printit4()
      import,none  ! this does turn off inheritance
      real :: A=40 ! so A must be declared with IMPLICIT NONE active
         write(*,*)A
         A=A+4.0
      end subroutine printit4
      
      end program testit
