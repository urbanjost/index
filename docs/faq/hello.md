#Hello World!

Generally, Fortran source is created as a plain ASCII text file with
a file suffix of .f for fixed-source style, and .f90 for free-format
style code. Any new code should be free-format. If the compiler 
supports automatically calling a pre-processor such as fpp(1) or cpp(1)
the convention most commonly followed is to capitalize the suffix 
(ie. .F or .F90). Other suffixes such as .for and .ftn are sometimes
supported.

##A simple program

Create a simple textfile called "hello.f90" with the following contents:

    program hello_demo
    implicit none
       write(*,*)'Hello World!'
    end program hello_demo

##Compiling 

Determine the name of your compiler (gfortran, ifort, g95, f90, f95, pathf90, minGW95, ...).
Compile your program according to your compiler documentation with something like

    f90 hello.f90 -o hello

Execute your program, typically by entering

    ./hello
or
    hello

###Compiler Options for development

###Compiler Options for production executables

##Making a library
Create a simple textfile called "hello.f90" with the following contents:

    program hello_demo
    implicit none
       write(*,*)'Hello World!'
       call print_real(123.45)
       call print_string('Hello Again!')
    end program hello_demo

Make a second file called "subroutines.f90":

    subroutine print_real(val)
    ! print given floating-point value
    implicit none
    real,intent(in) :: val
       write(*,*)'VALUE=',val
    end subroutine print_real

    subroutine print_string(string)
    ! print given string
    implicit none
    character(len=*),intent(in) :: string
       write(*,*)'STRING=',trim(string)
    end subroutine print_string

##Using a module

##Simple make(1) files
