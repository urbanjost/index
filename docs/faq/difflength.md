To have an array of strings of different length, define a type 
and declare an array of that type.

To have an array of strings of arbitrary length at run-time, you may
use deferred-legnth allocatable CHARACTER variables.

    program demo_deferred_length
    
    !  An array of "deferred-length" allocatable CHARACTER variables (a
    !  Fortran 2003 feature) allows the character length to change at run-time,
    !  including automatically through assignment.
    call deferred_length()
    
    !  Note that each element of the array has the same length - it is not an
    !  array of individually variable length strings. If that's what you want,
    !  you have to do it as an array of derived type where the type contains
    !  a CHARACTER(:), allocatable component.
    call defined_type()
    
    contains
    
    subroutine deferred_length()
       implicit none
    character(len=:), dimension(:),  allocatable :: array
    integer :: i
    integer,parameter :: max_len=14
    
       !if(.not.allocated(array)) allocate(character(len=max_len) :: array(3))
    
       ! force all the elements to the same length in a standard-conforming manner
       ! note that this will silently truncate strings longer than the specified length
       array = [character(len=max_len):: 'jones', 'something here','brown']
       !================
       write(*,'(*("[",a,"]":))')array
       write(*,'(*("[",a,"]":))')(trim(array(i)),i=1,size(array))
    end subroutine deferred_length
    
    subroutine defined_type()
    ! to define a type
    ! and declare an array of that type, e.g.
    !
    type string
       character(len=:), allocatable :: str
    end type string
    integer :: i
    type(string) :: array(3)
    
       array(1)%str = 'jones'
       array(2)%str = 'smith'
       array(3)%str = 'brown'
       write(*,'(a)') (array(i)%str,i=1,3)
    ! or
       array = [string('jones'), string('smith'), string('brown')]
       write(*,'(a)') (array(i)%str,i=1,3)
    end subroutine defined_type
    
    end program demo_deferred_length
