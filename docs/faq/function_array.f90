! Array of functions
! WIth a F2003 conformant compiler:
!
! save a list of functions into an array, which would be passed in and out for process.
! This is the concept of function pointers.  
program test_func_array
implicit none

   ! First create a type that contain only a procedure pointer that doesn't pass any argument.
   type pp
    procedure(func) ,pointer ,nopass :: f =>null()
   end type pp
   ! and then create an array of that type
   type(pp) :: func_array(4)

   interface
      function func(x)
         real :: func
         real, intent (in) :: x
      end function func
   end interface

   func_array(1)%f => exp
   func_array(2)%f => tan
   func_array(3)%f => cos
   func_array(4)%f => sin

   print*,func_array(1)%f(1.)
   print*,func_array(2)%f(1.)
   print*,func_array(3)%f(0.)
   print*,func_array(4)%f(0.)  

end program test_func_array
