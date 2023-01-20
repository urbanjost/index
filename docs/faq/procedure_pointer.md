# Procedure Pointers

In addition to being able to pass procedures as parameters since
FORTRAN 77, Fortran 2003+ supports procedure pointers, allowing
you to select one of a family of procedures with similar parameters

First, make some example functions

    module ExampleFuncs
       implicit none
    contains
    function f1 (x)
      real :: f1
      real, intent (in) :: x
      f1 = 2.0 * x
    end function f1
    function f2 (x)
       real :: f2
       real, intent (in) :: x
       f2 = 3.0 * x**2
    end function f2
    end module ExampleFuncs

## A simple program

    program test_func_ptrs
    use ExampleFuncs
    implicit none
    !! define an abstract template defining the family of procedures
    abstract interface
      function func (z)
         real :: func
         real, intent (in) :: z
      end function func
    end interface

    !! define a pointer of the abstract type
    procedure (func), pointer :: f_ptr => null ()

    character(len=:),allocatable :: name
    integer :: i

    write (*, '( / "Input function name (f1|f2 ")', advance="no" )
    read (*, *) name
    select(name)
    case('f1'); f_ptr => f1
    case('f2'); f_ptr => f2
    case default
       write(*,*)'bad function name, allowed names are "f1" and "f2"'
       stop
    end select

    do i=1,100
       write(*,*)'for ',i,'result is ',f_ptr (real(input))
    enddo

    end program test_func_ptrs

