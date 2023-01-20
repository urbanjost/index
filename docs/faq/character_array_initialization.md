# Trouble initialing character arrays#
## or why 
###    character(len=\*),parameter :: array(\*)=['one','two','three']
## is an error 

In Fortran all the elements of a character array must have the same
length (well, unless the ISO\_VARYING\_STRING extension is supported).
Primarily because of that, intuitive declarations like

    character(len=*),parameter :: array(*)=['one','two','three']

will fail because the string declarations are of different lengths.
Even if you specify the LEN value the strings have to be the same length

    character(len=5),parameter :: array(*)=['one','two','three']
    
Here are things that will work ...

    program odd ! implied shape array
    implicit none
    
    !!
    !! First, examples of character parameter array declarations
    !!
    
    CASE1 : BLOCK
    ! for this syntax string length must be constant, but size of array and
    ! LEN= an asterisk. This avoids any silent truncation or counting but
    ! requires all the strings to be the same length ...
       character(len=*),parameter :: array(*)=[ &
       'one   ', &
       'two   ', &
       'three ', &
       'last  ']
       write(*,'(*("[",a,"]":))')array
    ENDBLOCK CASE1
    
    CASE2 : BLOCK
    ! the strings can be specified more naturally without trailing spaces if
    ! the length is explicitly declared but if the specified length is too
    ! short the strings will be truncated. Note that as mentioned above, specifying
    ! the LEN= value only on the left side of the assign will NOT work 
       character(len=*),parameter :: array(*)=[character(len=5) :: 'one','two','three','last']
    !                                          ^^^^^^^^^^^^^^^^^^^^
       write(*,'(*("[",a,"]":))')array
    ENDBLOCK CASE2
    
    CASE3 : BLOCK
    ! of course explicitly specifying the number of elements is fine, but tedious. If you get
    ! the count on the number of elements wrong the compiler will generate an error; but note that
    ! if you declare the values with a DATA statement instead nothing will check that you 
    ! specified all the elements
       character(len=*),parameter :: array(4)=[character(len=5) :: 'one','two','three','last']
    !                                     ^^^
       write(*,'(*("[",a,"]":))')array
    ENDBLOCK CASE3
    
    !!
    !! Next, examples for  an allocatable array 
    !!
    
    ALLOC1: BLOCK
    ! an allocatable array can change size but cannot be initialized in
    ! the declaration
    
    ! If no explicit length is given the strings all have to be the same
    ! length, which is tedious
       character(len=:),allocatable :: arrayallo(:)
       arrayallo=['one   ','two   ','three ','last  ']
       write(*,'(*("[",a,"]":))')arrayallo
    ENDBLOCK ALLOC1
    
    ALLOC2: BLOCK
    ! this is how you specify a length so the strings can be specified
    ! more naturally (although the will all be stored with the same length)
       character(len=:),allocatable :: arrayallo(:)
       arrayallo=[character(len=5) :: 'one', 'two', 'three', 'last']
       write(*,'(*("[",a,"]":))')arrayallo
    ENDBLOCK ALLOC2
    
    ALLOC3: BLOCK
    ! if everthing else is the same as in case ALLOC2 but len is set to 2 
    ! what happens (answer: truncation )?
       character(len=:),allocatable :: arrayallo(:)
       arrayallo=[character(len=2) :: 'one', 'two', 'three', 'last']
       write(*,'(*("[",a,"]":))')arrayallo
    ENDBLOCK ALLOC3

    ALLOC4: BLOCK
    character(10) :: inp( 5 )
    integer :: i
    character(:), allocatable :: out(:)        ! this is NG
    inp = [ 'aAa', 'bBb', 'cCc', 'dDd', 'eEe' ]

    !! COPY INP TO OUT WITH SAME LENGTH
    out = [character(len=len(inp(i))) :: inp]               ; call printout()  
    !! GET UP TO FIRST TWO CHARACTERS OF INP
    out = [character(len=2) :: inp]                         ; call printout()
    !! GET SECOND CHARACTER OF INP
    out = [character(len=1) :: inp(:)(2:2)]                 ; call printout()
    !! AN IMPLIED DO ALLOWS FOR FUNCTIONS AND CONCATENATION AND EXPRESSIONS
    out = [character(len=2) :: (inp(i),i=1,size(inp))]      ; call printout()
    out = [character(len=3) :: ("#"//inp(i),i=1,size(inp))] ; call printout()

    !!out = [character(len=2+1) :: inp//"Z"]                  ; call printout()
    ENDBLOCK ALLOC4

    contains
    subroutine printout()
       write(*,'(*("[",a,"]":,","))')out
    end subroutine printout
    end program odd

--------------------------------------------------------------------------------

An example using a function

      module test
      implicit none
      contains
      
      elemental function gettwo( s ) result( res )
      character(*), intent(in) :: s
      character(len(s)) :: res
         res = s( 1 : 2 )
      endfunction
      
      endmodule
      
      program main
      use test
      implicit none
      character(10) :: inp( 5 )
      character(:), allocatable :: out(:)        ! this is NG
         inp = [ 'aaa', 'bbb', 'ccc', 'ddd', 'eee' ]
      
         !out = gettwo( inp )  !! NOT ALLOWED
         out = [character(len=2) :: gettwo(inp) ]
         print *, out       ! aabbccddee 
      endprogram
    
--------------------------------------------------------------------------------
