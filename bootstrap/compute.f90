 
!>>>>> ././src/M_calculator.f90
!>
!!##NAME
!!   calculator - [M_calculator] parse calculator expression and return numeric or string value
!!   (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine calculator(inline,outlin,mssg,slast,ierr)
!!
!!    character(len=*),intent=(in)           :: inline
!!    character(len=iclen_calc),intent=(out) :: outlin
!!    character(len=iclen_calc),intent=(out) :: mssg
!!    doubleprecision, intent=(out)          :: slast
!!    integer, intent=(out)                  :: ierr
!!
!!##DESCRIPTION
!!    CALCULATOR(3f) evaluates FORTRAN-like expressions. It can be used to add
!!    calculator-like abilities to your program.
!!
!!##OPTIONS
!!     inline  INLINE is a string expression up to (iclen_calc=512) characters long.
!!             The syntax of an expression is described in
!!             the main document of the Calculator Library.
!!     outlin  Returned numeric value as a string when IERR=0.
!!     mssg    MSSG is a string that can serve several purposes
!!             o Returned string value when IERR=2
!!             o Error message string when IERR=-1
!!             o Message from 'funcs' or 'dump' command when IERR=1
!!     slast   SLAST has different meanings depending on whether a string or number
!!             is being returned
!!             o REAL value set to last successfully calculated value when IERR=0
!!             o Number of characters in returned string variable when IERR=2
!!     ierr    status flag.
!!               -1  An error occurred
!!                0  A numeric value was returned
!!                1  A message was returned
!!                2  A string value was returned
!!##EXAMPLES
!!
!!   Example calculator program
!!
!!       program demo_calculator
!!       !compute(1f): line mode calculator program (that calls calculator(3f))
!!       use M_calculator, only: calculator,iclen_calc
!!       ! iclen_calc : max length of expression or variable value as a string
!!       implicit none
!!       integer,parameter         :: dp=kind(0.0d0)
!!       character(len=iclen_calc) :: line
!!       character(len=iclen_calc) :: outlin
!!       character(len=iclen_calc) :: event
!!       real(kind=dp)             :: rvalue
!!       integer                   :: ierr
!!       ierr=0
!!       call calculator('ownmode(1)',outlin,event,rvalue,ierr)
!!       ! activate user-defined function interface
!!       INFINITE: do
!!          read(*,'(a)',end=999)line
!!          if(line.eq.'.')stop
!!          call calculator(line,outlin,event,rvalue,ierr)
!!          select case (ierr)
!!          ! several different meanings to the error flag returned by calculator
!!          case(0)
!!          ! a numeric value was returned without error
!!            write(*,'(a,a,a)')trim(outlin),' = ',trim(line)
!!          case(2)
!!          ! a string value was returned without error
!!            write(*,'(a)')trim(event(:int(rvalue)))
!!          case(1)
!!          ! a request for a message has been returned (from DUMP or FUNC)
!!            write(*,'(a,a)')'message===>',trim(event(:len_trim(event)))
!!          case(-1)
!!          ! an error has occurred
!!            write(*,'(a,a)')'error===>',trim(event(:len_trim(event)))
!!          case default
!!          ! this should not occur
!!            WRITE(6,'(A,i10)')'*CALCULATOR* UNEXPECTED IERR VALUE ',IERR
!!          end select
!!       enddo INFINITE
!!       999 continue
!!       end program demo_calculator
!!
!!##SEE ALSO
!!     see INUM0(),RNUM0(),SNUM0(),EXPRESSION().
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!>
!! AUTHOR   John S. Urban
!!##VERSION  1.0 19971123,20161218
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
module M_calculator
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdout=>OUTPUT_UNIT    ! access computing environment

!!implicit doubleprecision (a-h,o-z)
implicit none
private

integer,parameter                      :: k_dbl=kind(0.0d0)
integer,parameter                      :: dp=kind(0.0d0)

integer,parameter,public               :: iclen_calc=512           ! max length of expression or variable value as a string
integer,parameter,public               :: ixy_calc=55555           ! number of variables in X() and Y() array
real(kind=dp),save,public              :: x(ixy_calc)=0.0_dp       ! x array for procedure funcs_
real(kind=dp),save,public              :: y(ixy_calc)=0.0_dp       ! y array for procedure funcs_

integer,parameter,public                  :: icname_calc=20        ! max length of a variable name

character(len=:),allocatable,save         :: keys_q(:)             ! contains the names of string variables
character(len=:),allocatable,save,public  :: values(:)             ! string variable values
integer,save,public,allocatable           :: values_len(:)         ! lengths of the string variable values

character(len=:),allocatable,save         :: keyr_q(:)             ! contains the names of numeric variables
real(kind=dp),save,allocatable            :: values_d(:)           ! numeric variable values

public  :: calculator
private :: stuff
private :: stuffa
! CONVENIENCE ROUTINES
public :: inum0      ! resolve a calculator string into a whole integer number
public :: rnum0      ! resolve a calculator string into a real number (return 0 on errors)
public :: dnum0      ! resolve a calculator string into a doubleprecision number (return 0 on error s)
public :: snum0      ! resolve a calculator expression into a string(return blank on errors)
public :: expression ! call calculator() calculator and display messages

public :: set_mysub
public :: set_myfunc

integer,parameter                      :: ixyc_calc=50                   ! number of variables in $X() and $(Y) array
integer,parameter                      :: icbuf_calc=23*(iclen_calc/2+1) ! buffer for string as it is expanded


!  no check on whether line expansion ever causes line length to
!  exceed allowable number of characters.
!  number of characters to prevent over-expansion would currently be
!  23 digits per number max*(input number of characters/2+1).

character(len=iclen_calc)       :: mssge   ! for error message/messages /returning string value

character(len=iclen_calc),save  :: xc(ixyc_calc)=' '        ! $x array for procedure funcs_
character(len=iclen_calc),save  :: yc(ixyc_calc)=' '        ! $y array for procedure funcs_
character(len=iclen_calc),save  :: nc(ixyc_calc)=' '        ! $n array for procedure funcs_


character(len=iclen_calc),save  :: last='0.0'               ! string containing last answer (i.e. current value)
logical,save                    :: ownon=.false.            ! flag for whether to look for substitute_subroutine(3f)

integer,save                    :: ktoken                   ! count of number of token strings assembled
!
private :: a_to_d_                       ! returns a real value rval from a numeric character string chars.
private :: squeeze_
private :: stufftok_
private :: funcs_
private :: pows_
private :: given_name_get_stringvalue_
private :: parens_
private :: args_
private :: factors_
private :: expressions_
private :: help_funcs_

private :: juown1_placeholder
private :: c_placeholder

abstract interface
   subroutine juown1_interface(func,iflen,args,iargstp,n,fval,ctmp,ier)
      import k_dbl
      character(len=*),intent(in)          :: func
      integer,intent(in)                   :: iflen
      real(kind=k_dbl),intent(in)          :: args(100)
      integer,intent(in)                   :: iargstp(100)
      integer,intent(in)                   :: n
      real(kind=k_dbl)          :: fval
      character(len=*)          :: ctmp
      integer                   :: ier
   end subroutine juown1_interface
end interface

abstract interface
   real function c_interface(args,n)
      import k_dbl
      integer,intent(in)            :: n
      real(kind=k_dbl),intent(in)   :: args(n)
   end function c_interface
end interface
public c_interface
public juown1_interface

procedure(juown1_interface),pointer :: mysub => juown1_placeholder
procedure(c_interface),pointer      :: myfunc => c_placeholder

public locate        ! [M_list] find PLACE in sorted character array where value can be found or should be placed
   private locate_c
   private locate_d
   private locate_i
public insert        ! [M_list] insert entry into a sorted allocatable array at specified position
   private insert_c
   private insert_d
   private insert_i
public replace       ! [M_list] replace entry by index from a sorted allocatable array if it is present
   private replace_c
   private replace_d
   private replace_i
public remove        ! [M_list] delete entry by index from a sorted allocatable array if it is present
   private remove_c
   private remove_d
   private remove_i

!character(len=*),parameter::ident_1="&
!&@(#)M_list::locate(3f): Generic subroutine locates where element is or should be in sorted allocatable array"
interface locate
   module procedure locate_c, locate_d
end interface

!character(len=*),parameter::ident_2="&
!&@(#)M_list::insert(3f): Generic subroutine inserts element into allocatable array at specified position"
interface insert
   module procedure insert_c, insert_d, insert_i
end interface

!character(len=*),parameter::ident_3="&
!&@(#)M_list::replace(3f): Generic subroutine replaces element from allocatable array at specified position"
interface replace
   module procedure replace_c, replace_d, replace_i 
end interface

!character(len=*),parameter::ident_4="&
!&@(#)M_list::remove(3f): Generic subroutine deletes element from allocatable array at specified position"
interface remove
   module procedure remove_c, remove_d, remove_i 
end interface

!-----------------------------------------------------------------------------------------------------------------------------------
public dictionary

type dictionary
   character(len=:),allocatable :: key(:)
   character(len=:),allocatable :: value(:)
   integer,allocatable          :: count(:)
   contains
      procedure,private :: get => dict_get
      procedure,private :: set => dict_add    ! insert entry by name into a sorted allocatable character array if it is not present
      procedure,private :: del => dict_delete ! delete entry by name from a sorted allocatable character array if it is present
end type dictionary
contains
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine set_myfunc(proc)
procedure(c_interface) :: proc
   myfunc => proc
end subroutine set_myfunc
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine set_mysub(proc)
procedure(juown1_interface) :: proc
   mysub => proc
end subroutine set_mysub
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
recursive subroutine calculator(inline,outlin,mssg,slast,ierr)
!
!     The goal is to create a procedure easily utilized from other
!     programs that takes a standard Fortran value statement and reduces
!     it down to a value, efficiently and using standard Fortran
!     standards where ever feasible.
!
!  Version 2.0: 03/13/87
!  Version 3.0: 07/11/2013
!  Version 5.0: 07/16/2013
!
!  o  adjacent powers are done left to right, not right to left
!  o  code does not prevent - and + beside an other operator.
!  o  no check on whether user input more characters than allowed.
!     no check on whether line expansion ever causes line length to
!     exceed allowable number of characters.
!     number of characters to prevent over-expansion would currently be
!     23 digits per number max*(input number of characters/2+1).
!  o  allowing for ixy_calc arguments in max and min seems too high. if reducing
!     array size helps significantly in costs, do so.
!  o  parentheses are required on a function call.
!  o  square brackets [] are equivalent to parenthesis ().
!===========================================================================--------------------------------------------------------
!  2. need a generic help function to list commands and functions
!  3. allow multiple expressions per line with a semi-colon between them
!     (like the parse functions).
!  4. make a function to fill x and y arrays, or to read values into them
!     from a file; and make some statistical functions that work on the
!     arrays.
!  6. allow user-written functions to be called from funcs_ routine.
!  7. allow for user-defined arrays and array operations.
!===========================================================================--------------------------------------------------------
!  12/07/87 --- put in an implicit real (a-h,o-z) statement in each
!              procedure so that it could quickly be changed to
!              implicit real*8 (a-h,o-z) for a vax. be careful of
!              type mismatch between external functions and the
!              real variables.
!              use following xedit commands where periods denote
!              spaces
!              c/implicit real../implicit real*8./ *
! 12/11/87  --- changed ifix calls to int calls as ifix on vax does
!              not allow real*8 in ifix calls
! 12/11/87  --- moving all prints out of column 1 so it is not picked
!              out by vax as carriage control.
! 12/28/87  --- put bn format specifier into a_to_d_ routine because
!              vax assumes zero fill
! 06/23/88  --- making a first cut at allowing string variables.
!               1. string variable names must start with a dollar-sign
!               2. strings can only be up to (iclen_calc) characters long
!               3. they will be returned in the message string to
!                  the calling program
!               4. input strings must be delimited with double quotes.
!                  to place a double quote into the string, put two
!                  double quotes adjacent to each other.
!               5. a flag value for ier to distinguish between string
!                  and numeric output?
!#----------------------------------------------------------------------------------------------------------------------------------
!subroutine calculator(inline,outlin,mssg,slast,ierr)

!character(len=*),parameter::ident_1="@(#)M_calculator::calculator(3f): The procedure CALCULATOR(3f) acts like a calculator"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)            :: inline
character(len=iclen_calc),intent(out)  :: outlin
character(len=iclen_calc),intent(out)  :: mssg
real(kind=dp),intent(out)              :: slast
integer,intent(out)                    :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=icbuf_calc)              :: line
character(len=iclen_calc)              :: varnam
character(len=iclen_calc)              :: junout
real(kind=dp),save                     :: rlast=0.0_dp
integer                                :: i10
integer                                :: i20
integer                                :: idum
integer                                :: imax
integer                                :: indx
integer                                :: iplace
integer                                :: istart
integer                                :: nchar2
integer                                :: nchard
!-----------------------------------------------------------------------------------------------------------------------------------
   line=inline                                      ! set working string to initial input line
   imax=len(inline)                                 ! determine the length of the input line
   mssg=' '                                         ! set returned message/error/string value string to a blank
   outlin=' '
   BIG: do                                          ! for $A=numeric and A=string
      ierr=1                                           ! set status flag to message mode
      mssge=' '                                        ! set message/error/string value in GLOBAL to a blank
      varnam=' '
      call squeeze_(line,imax,nchard,varnam,nchar2,ierr) ! preprocess the string: remove blanks and process special characters
                                                         ! also remove all quoted strings and replace them with a token
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ierr.eq.-1)then                ! if an error occurred during preprocessing of the string, set returned message and quit
         slast=rlast                    ! set returned real value to last good calculated value
         mssg=mssge                     ! place internal message from GLOBAL into message returned to user
         return
      elseif(nchard.eq.0)then  ! if a blank input string was entered report it as an error and quit
         ierr=-1
         mssg='*calculator* input line was empty'
      elseif(line(1:nchard).eq.'dump')then ! process dump command
         write(*,*)line(1:nchard)
         write(*,*)'current value= ',last
         write(*,*)' variable name       variable value     '
         if(allocated(keyr_q))then
            do i10=1,size(keyr_q)
               if(keyr_q(i10).ne.' ')then
                  write(junout,'('' '',2a,g23.16e3)')keyr_q(i10),' ',values_d(i10)
                  write(*,*)trim(junout)
               endif
            enddo
         endif
         if(allocated(keys_q))then
            do i20=1,size(keys_q)
               if(keys_q(i20).ne.' ')then
                  write(junout,'('' '',3a)')keys_q(i20),' ',values(i20)(:values_len(i20))
                  write(*,*)trim(junout)
               endif
            enddo
         endif
         mssg='variable listing complete'
      elseif(line(1:nchard).eq.'funcs') then     ! process funcs command
         call help_funcs_()
         mssg='function listing complete'
!-----------------------------------------------------------------------------------------------------------------------------------
      else                                                ! this is an input line to process
         call parens_(line,nchard,ierr)                   ! process the command
         if(ierr.eq.0)then                 ! if no errors occurred set output string, store the value as last, store any variable
                                           ! numeric value with no errors, assume nchard is 23 or less
            outlin=line(1:nchard)                         ! set string output value
            last=line(1:nchard)                           ! store last value (for use with question-mark token)
            call a_to_d_(last(1:nchard),rlast,idum)       ! set real number output value
            if(nchar2.ne.0.and.varnam(1:1).ne.'$')then    ! if the statement defines a variable make sure variable name is stored
               call locate(keyr_q,varnam(:nchar2),indx,ierr) ! determine placement of the variable and whether it is new
               if(ierr.eq.-1)then
                  slast=rlast                             ! set returned real value to last good calculated value
                  mssg=mssge                              ! place internal message from GLOBAL into message returned to user
                  return
               endif
               if(indx.le.0)then                          ! if the variable needs added, add it
                  istart=iabs(indx)
                  call insert(keyr_q,varnam(:nchar2),istart)
                  call insert(values_d,0.0d0,istart)
               endif
               call a_to_d_(last(1:nchard),values_d(iabs(indx)),ierr)  ! store a defined variable's value
            elseif(nchar2.ne.0)then                       ! numeric value to string
               line(:)=' '
               line=varnam(:nchar2)//'="'//last(1:nchard)//'"'
               imax=len_trim(line)                        ! determine the length of the input line
               cycle BIG
            endif
         elseif(ierr.eq.2)then ! returned output is not numeric, but alphanumeric (it is a string)
!!!!!!!  could return string values directly instead of thru message field
!!!!!!!  make sure normal output values are not left indeterminate
            mssg=mssge                                    ! set returned string value to returned string value
            if(nchar2.ne.0.and.varnam(1:1).eq.'$')then    ! if the statement defines a variable make sure variable name is stored
               call locate(keys_q,varnam(:nchar2),indx,ierr) ! determine placement of the variable and whether it is new
               if(ierr.eq.-1)then
                  slast=rlast                             ! set returned real value to last good calculated value
                  mssg=mssge                              ! place internal message from GLOBAL into message returned to user
                  return
               endif
               iplace=iabs(indx)
               if(indx.le.0)then                             ! if the variable needs added, add it
                  call insert(keys_q,varnam(:nchar2),iplace) ! adding the new variable name to the variable name array
                  call insert(values,' '            ,iplace)
                  call insert(values_len,0              ,iplace)
               endif
               call replace(values,mssg,iplace)
               call replace(values_len,len_trim(mssg),iplace)
               rlast=dble(values_len(iplace))             ! returned value is length of string when string is returned
            elseif(nchar2.ne.0)then                       ! string but being stored to numeric variable
                line=varnam(:nchar2)//'='//mssg
                imax=len_trim(line)                       ! determine the length of the input line
                cycle BIG
            else                                          ! a string function with an assignment to it (for example "Hello"
               rlast=len_trim(mssg)                       ! probably should pass message length up from someplace
            endif
         endif
         mssg=mssge
      endif
      exit BIG
   enddo BIG
   slast=rlast                                            ! set returned value to last successfully calculated real value
end subroutine calculator
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine help_funcs_()

!character(len=*),parameter::ident_2="@(#)M_calculator::help_funcs_(3fp): prints help for calculator functions"

character(len=80),allocatable :: help_text(:)
integer                       :: i
help_text=[ &
&'--------------------------------------------------------------------------------',&
&'standard functions available:                                                   ',&
&'--------------------------------------------------------------------------------',&
!&' c(                   : user-defined function                                   ',&
!&' ownmode(             : call user-defined procedures                            ',&
&'--------------------------------------------------------------------------------',&
&' len_trim($value)     : number of characters trimming trailing spaces           ',&
&' index($value,$match) : return position $match occurs in $value or zero         ',&
&' sign(val1,val2)      : magnitude of val1 with the sign of val2                 ',&
&' real(value)          : conversion to real type                                 ',&
&' str($str|expr,....)  :append as strings and then convert to number             ',&
&' $str($str|expr,....) :append as strings                                        ',&
&' round(value,digits)  :                                                         ',&
&' ichar($value)        : return ASCII Decimal Equivalent of character            ',&
&' $char(value)         : return character given ASCII Decimal Equivalent         ',&
&' $f(format,value)     : using FORMAT to create it convert number to string      ',&
&' $if(expr,$val1,$val2): if expr==0 return $val1, else return $val2              ',&
&' if(expr,val1,val2)   : if expr==0 return val1, else return val2                ',&
&' hypot(x,y)           : Euclidean distance function                             ',&
&'--------------------------------------------------------------------------------',&
&'WHOLE NUMBERS:                                                                  ',&
&' aint(value) : truncation toward zero to a whole number                         ',&
&' anint(value): nearest whole number                                             ',&
&' int(value)  : conversion to integer type                                       ',&
&' nint(value) : nearest integer                                                  ',&
&' floor(A)    : greatest integer less than or equal to A                         ',&
&' ceiling(A)  : least integer greater than or equal to A                         ',&
&'--------------------------------------------------------------------------------',&
&'MISCELLANEOUS:                                                                  ',&
&' max(v1,v2,v3,...v50)  : maximum value of list                                  ',&
&' min(v1,v2,v3,...v50)  : minimum value of list                                  ',&
&' dim(x,y)    : maximum of X-Y and zero                                          ',&
&' frac(A)     : fractional part of A (A - INT(A))                                ',&
&' mod(A,P)    : remainder function                                               ',&
&' abs(value)  : absolute value                                                   ',&
&' exp(value)  : exponent of value                                                ',&
&'NUMERIC FUNCTIONS:                                                              ',&
&' bessel_j0   : Bessel function of the first kind of order 0                     ',&
&' bessel_j1   : Bessel function of the first kind of order 1                     ',&
&' bessel_jn   : Bessel function of the first kind                                ',&
&' bessel_y0   : Bessel function of the second kind of order 0                    ',&
&' bessel_y1   : Bessel function of the second kind of order 1                    ',&
&' bessel_yn   : Bessel function of the second kind                               ',&
&' sqrt(value) : return square root of value                                      ',&
&' log(v1)     : logarithm of value to base e                                     ',&
&' log10(v1)   : logarithm of value to base 10                                    ',&
&'--------------------------------------------------------------------------------',&
&'RANDOM NUMBERS:                                                                 ',&
&' srand(seed_value) : set seed value for rand()                                  ',&
&' rand()            : random number                                              ',&
&'--------------------------------------------------------------------------------',&
&'SYSTEM:                                                                         ',&
&' $getenv(name),$ge(name)               : get environment variable value         ',&
&' sh(command)                           : system command                         ',&
&'--------------------------------------------------------------------------------',&
&'ARRAY STORAGE:                                                                  ',&
&' $nstore(start_index,$value1,$value2,$value3,....) | $n(index)                  ',&
&' $xstore(start_index,$value1,$value2,$value3,....) | $x(index)                  ',&
&' $ystore(start_index,$value1,$value2,$value3,....) | $y(index)                  ',&
&' xstore(start_index,value1,value2,value3,....)     | x(index)                   ',&
&' ystore(start_index,value1,value2,value3,....)     | y(index)                   ',&
&'--------------------------------------------------------------------------------',&
&'STRING MODIFICATION:                                                            ',&
&' $l($input_string)        : convert string to lowercase                         ',&
&' $u($input_string)        : convert string to uppercase                         ',&
&' $substr($input_string,start_column,end_column)                                 ',&
&' $str($a|e,$a|e,$a|e,....):append string and value expressions into string      ',&
&'--------------------------------------------------------------------------------',&
&'CALENDAR:                                                                       ',&
&' ye(),year()   : current year                                                   ',&
&' mo(),month()  : current month                                                  ',&
&' da(),day()    : current day                                                    ',&
&' ho(),hour()   : current hour                                                   ',&
&' mi(),minute() : current minute                                                 ',&
&' se(),second() : current second                                                 ',&
&' $mo([n])      : name of month                                                  ',&
&' dw()          : day of week                                                    ',&
&' ju()          : day of year                                                    ',&
&'--------------------------------------------------------------------------------',&
&'TRIGONOMETRIC:                                                                  ',&
&' cos(radians) : cosine  | acos(x/r)   | cosh()   | acosh()   | cosd(degrees)    ',&
&' sin(radians) : sine    | asin(y/r)   | sinh()   | asinh()   | sind(degrees)    ',&
&' tan(radians) : tangent | atan(y/x)   | tanh()   | atanh()   | tand(degrees)    ',&
&'                        | atan2(x,y)  |                                         ',&
&'--------------------------------------------------------------------------------',&
&'UNIT CONVERSION:                                                                ',&
&' c2f(c) : centigrade to Fahrenheit |f2c(f) : Fahrenheit to centigrade           ',&
&' d2r(d) : degrees to radians       |r2d(r) : radians to degrees                 ',&
&'--------------------------------------------------------------------------------',&
&'LOGICAL:                                                                        ',&
&' ge(a,b) : greater than or equal to                                             ',&
&' le(a,b) : A less than or equal to B                                            ',&
&' gt(a,b) : A greater than B                                                     ',&
&' lt(a,b) : A less than B                                                        ',&
&' eq(a,b) : A equal to B                                                         ',&
&' ne(a,b) : A not equal to B                                                     ',&
&' lge($a,$b): lexically greater than or equal to                                 ',&
&' lle($a,$b): lexically A less than or equal to B                                ',&
&' lgt($a,$b): lexically A greater than B                                         ',&
&' llt($a,$b): lexically A less than B                                            ',&
&' leq($a,$b): lexically A equal to B                                             ',&
&' lne($a,$b): lexically A not equal to B                                         ',&
&' in(lower_bound,test_value,upper_bound) : test if value in given range          ',&
&'--------------------------------------------------------------------------------',&
&'                                                                                ']
   do i=1,size(help_text)
      write(*,*)trim(help_text(i))
   enddo
end subroutine help_funcs_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
! fraction : Fractional part of the model representation
! exponent :
! gamma    : Logarithm of the Gamma function
! log_gamma  Logarithm of the Gamma function
! erf Error function erfc (3fortran) -
! Complementary error function erfc_scaled
! erfc Complementary error function
! erfc_scaled  Error function
! modulo Modulo function
! btest MANIPULATION] Bit test function
! tiny Smallest positive number of a real kind
! epsilon Epsilon function
! huge Largest number of a kind
! same pads strings to same length and then calls MERGE(3f)
! flush flush I/O buffers of specified files
! unusedf
! delimx
! c
! ownmode
! mod
! scale
! len
! ifdef
! open close rewind write
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    parens_(3fp) - [M_calculator] crack out the parenthesis and solve
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    recursive subroutine parens_(string,nchar,ier)
!!    character(len=*)             :: string
!!    integer,intent(inout)        :: nchar
!!    integer,intent(out)          :: ier
!!##DESCRIPTION
!!    crack out the parenthesis and solve
!!##OPTIONS
!!    string  input string to expand and return
!!    nchar   length of string on input and output
!!    ier     status code
!!
!!              0=good numeric return
!!              2=good alphameric return
!!             -1=error occurred, message is in mssge
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
recursive subroutine parens_(string,nchar,ier)

!character(len=*),parameter::ident_3="@(#)M_calculator::parens_(3fp): crack out the parenthesis and solve"

character(len=*)             :: string
integer,intent(inout)        :: nchar
integer,intent(out)          :: ier

character(len=icbuf_calc)    :: wstrng
character(len=:),allocatable :: dummy
integer                      :: imax
integer                      :: ileft
integer                      :: iright
integer                      :: i
integer                      :: iz
integer                      :: iwnchr
real(kind=dp)                :: rdum
!#----------------------------------------------------------------------------------------------------------------------------------
   imax=nchar
   ier=0
   INFINITE: do
!#----------------------------------------------------------------------------------------------------------------------------------
   ileft=0                                    ! where rightmost left paren was found
   do i=imax,1,-1                             ! find rightmost left paren
      if(string(i:i).eq.'(')then
         ileft=i
         exit
      endif
   enddo
!#----------------------------------------------------------------------------------------------------------------------------------
      if(ileft.eq.0)then                          ! no left parenthesis was found; finish up
         if(index(string(:nchar),')').ne.0) then  ! if here there are no left paren. check for an (unmatched) right paren
            ier=-1
            mssge='*parens_* extraneous right parenthesis found'
         else
   !        no parenthesis left, reduce possible expression to a single value primitive and quit
   !        a potential problem is that a blank string or () would end up here too.
            call expressions_(string,nchar,rdum,ier)
         endif
         return
      endif
!#----------------------------------------------------------------------------------------------------------------------------------
      iright=index(string(ileft:nchar),')') ! left parenthesis was found; find matching right paren
      if(iright.eq.0) then
         ier=-1
         mssge='*parens_* right parenthesis missing'
         return
      endif
!#----------------------------------------------------------------------------------------------------------------------------------
      iright=iright+ileft-1  !there was a matched set of paren remaining in the string
      iz=1  ! check now to see if this is a function call. search for an operator
!     if ileft is 1, then last set of parenthesis,(and for an expression)
      if(ileft.ne.1)then
         do i=ileft-1,1,-1
            iz=i
            if(index('#=*/(,',string(i:i)).ne.0)then
               iz=iz+1
               goto 11
            endif
         enddo
!        if here, a function call begins the string, as iz=1 but ileft doesn't
      endif
!=======================================================================------------------------------------------------------------
!     iz=position beginning current primitive's string
!     ileft=position of opening parenthesis for this primitive
!     iright=position of end and right parenthesis for this string
11    continue
      if(iz.eq.ileft)then  ! if ileft eq iz then a parenthesized expression, not a function call
         wstrng=string(ileft+1:iright-1)
         iwnchr=iright-1-(ileft+1)+1
         call expressions_(wstrng,iwnchr,rdum,ier)
      else
         wstrng=string(iz:iright)
         iwnchr=iright-iz+1
         call funcs_(wstrng,iwnchr,ier)
      endif
      if(ier.eq.-1)return !     if an error occurred in expressions_ or funcs_, then return
      ! restring the evaluated primitive back into the main string
      ! remember that if an expression, iz=ileft
      ! last set of -matched- parentheses, and entire string was evaluated
      if(iz.eq.1.and.iright.eq.nchar)then
         dummy=wstrng(:iwnchr)
         nchar=iwnchr
!        last set of -matched- parentheses, but other characters still to right
      elseif(iz.eq.1)then
         dummy=wstrng(:iwnchr)//string(iright+1:nchar)
         nchar=iwnchr+nchar-iright
      elseif(iright.eq.nchar)then
!        last expression evaluated was at end of string
         dummy=string(:iz-1)//wstrng(:iwnchr)
         nchar=iz-1+iwnchr
      else
!        last expression evaluated was in middle of string
         dummy=string(:iz-1)//wstrng(:iwnchr)//string(iright+1:nchar)
         nchar=iz-1+iwnchr+nchar-iright
      endif
!     set last place to look for a left parenthesis to one to the left
!     of the beginning of the primitive just reduced, or to a 1 so that
!     the loop looking for the left parenthesis doesn't look for a
!     parenthesis at position 0:0
      imax=max(iz-1,1)
      string=dummy
   enddo INFINITE
end subroutine parens_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    funcs_(3fp) - [M_calculator]given string of form name(p1,p2,...) (p(i) are non-parenthesized expressions)
!!    call procedure "name" with those values as parameters.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    recursive subroutine funcs_(wstrng,nchars,ier)
!!
!!     character(len=*)                    :: wstrng
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
recursive subroutine funcs_(wstrng,nchars,ier)

!character(len=*),parameter::ident_4="&
!&@(#)M_calculator::funcs_(3fp):given string of form name(p1,p2,...) (p(i) are non-parenthesized expressions) call procedure name"

character(len=*)                    :: wstrng
integer                             :: nchars
integer                             :: ier

integer,parameter                   :: iargs=100
character(len=10),save              :: days(7)
character(len=10),save              :: months(12)
character(len=9) :: day
character(len=iclen_calc)           :: ctmp
character(len=iclen_calc)           :: ctmp2
character(len=iclen_calc)           :: junout
character(len=iclen_calc)           :: cnum
character(len=icname_calc)          :: wstrng2

real(kind=dp)                       :: args(iargs)

real                                :: acurcy
real(kind=dp)                       :: arg1
real(kind=dp)                       :: arg2
real(kind=dp)                       :: bottom
real(kind=dp)                       :: false
real(kind=dp)                       :: fval
real(kind=dp)                       :: top
real(kind=dp)                       :: true
real(kind=dp)                       :: val

real,external                       :: c

integer,save                        :: ikeepran=22
integer                             :: i
integer                             :: i1
integer                             :: i1010
integer                             :: i1033
integer                             :: i1060
integer                             :: i1066
integer                             :: i2
integer                             :: i2020
integer                             :: i3030
integer                             :: i410
integer                             :: i440
integer                             :: i520
integer                             :: i852
integer                             :: iargs_type(iargs)
integer                             :: ibegin(ixyc_calc),iterm(ixyc_calc)
integer                             :: icalen
integer                             :: icount
integer                             :: idarray(8)
integer                             :: idig
integer                             :: idum
integer                             :: iend
integer                             :: iend1
integer                             :: iend2
integer                             :: ifail
integer                             :: iflen
integer                             :: ii
integer                             :: iie
integer                             :: iii
integer                             :: iiie
integer                             :: ileft
integer                             :: ilen
integer                             :: in
integer                             :: ind
integer                             :: indexout
integer                             :: ios
integer                             :: iright
integer                             :: istart
integer                             :: istat
integer                             :: istore
integer                             :: istoreat
integer                             :: isub
integer                             :: itime(8)
integer                             :: itype
integer                             :: iunit
integer                             :: ival
integer                             :: ivalue
integer                             :: jend
integer                             :: jj
integer                             :: n
integer                             :: idat(8)
integer                             :: ierr
integer                             :: iweekday
integer                             :: ii2
integer                             :: ilen2
integer                             :: istatus

intrinsic                           :: abs,aint,anint,exp,nint,int,log,log10
intrinsic                           :: acos,asin,atan,cos,cosh,sin,sinh,tan,tanh
intrinsic                           :: sqrt,atan2,dim,mod,sign,max,min
!-----------------------------------------------------------------------------------------------------------------------------------
   data months/'January','February','March','April','May','June','July','August','September','October','November','December'/
   data days/'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'/
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc------------------------------------------------------------
   TRUE=0.0d0
   FALSE=1.0d0
   ier=0
   iright=nchars-1
   ileft=index(wstrng(1:nchars),'(')+1
   iend=ileft-2
   iflen=iright-ileft+1
!  n=number of parameters found
   if(iright-ileft.lt.0)then ! if call such as fx() expression string is null
      n=0
   else ! take string of expressions separated by commas and place values into an array and return how many values were found
      call args_(wstrng(ileft:iright),iflen,args,iargs_type,n,ier,100)
      if(ier.eq.-1)then
         goto 999
      else
         ier=0 ! ier could be 2 from args_()
      endif
   endif
   wstrng2=' '
   wstrng2(:iend)=lower(wstrng(:iend))
   fval=0.0d0
   if(ier.eq.-1)then
      goto 999
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
select case (wstrng2(:iend))
case("abs","aint","anint","ceil","ceiling","floor","frac","int","nint",&
    &"d2r","r2d",&
    &"c2f","f2c",&
    &"gamma","log_gamma",&
    &"log","log10","exp",&
    &"bessel_j0","bessel_j1","bessel_y0","bessel_y1",&
    &"erf","erfc","erfc_scaled",&
    &"sin","cos","tan",&
    &"sind","cosd","tand",&
    &"sinh","cosh","tanh",&
    &"asin","acos","atan",&
    &"asinh","acosh","atanh",&
!   &"cpu_time",&
    &"exponent","fraction",&
    &"real","sqrt")
      if(n.ne.1)then                                                    ! check number of parameters
        mssge='*funcs_* incorrect number of parameters in '//wstrng2(:iend)
        ier=-1
      elseif(iargs_type(1).ne.0)then                                    ! check type of parameters
        mssge='*funcs_* parameter not numeric in '//wstrng2(:iend)
        ier=-1
      else                                                              ! single numeric argument
         select case (wstrng2(:iend))
!=======================================================================------------------------------------------------------------
         case("acos");

         if(args(1).gt.1.or.args(1).lt.-1)then
           mssge='*acos* parameter not in range -1 >= value <=1'
           ier=-1
         else
            fval= acos(args(1))
         endif
         case("atan");   fval= atan(args(1))
         case("asin");   fval= asin(args(1))

         !case("cpu_time");   fval= cpu_time(args(1))
         case("fraction");   fval= fraction(args(1))
         case("exponent");   fval= exponent(args(1))
         case("gamma");      fval= gamma(args(1))
         case("log_gamma");  fval= log_gamma(args(1))

         case("cos");    fval= cos(args(1))
         case("sin");    fval= sin(args(1))
         case("tan");    fval= tan(args(1))

         case("acosh");    fval= acosh(args(1))
         case("asinh");    fval= asinh(args(1))
         case("atanh");    fval= atanh(args(1))

         case("cosd");   fval= cos(args(1)*acos(-1.0d0)/180.d0)
         case("sind");   fval= sin(args(1)*acos(-1.0d0)/180.d0)
         case("tand");   fval= tan(args(1)*acos(-1.0d0)/180.d0)

         case("cosh");   fval= cosh(args(1))
         case("sinh");   fval= sinh(args(1))
         case("tanh");   fval= tanh(args(1))

         case("erf");         fval= erf(args(1))
         case("erfc");        fval= erfc(args(1))
         case("erfc_scaled"); fval= erfc_scaled(args(1))

         case("d2r");    fval= args(1)*acos(-1.0d0)/180.d0
         case("r2d");    fval= args(1)*180.d0/acos(-1.0d0)

         case("c2f");    fval= (args(1)+40.0d0)*9.0d0/5.0d0 - 40.0d0
         case("f2c");    fval= (args(1)+40.0d0)*5.0d0/9.0d0 - 40.0d0

         case("bessel_j0");    fval= bessel_j0(args(1))
         case("bessel_j1");    fval= bessel_j1(args(1))
         case("bessel_y0");    fval= bessel_y0(args(1))
         case("bessel_y1");    fval= bessel_y1(args(1))

         case("abs");    fval= abs(args(1))
         case("aint");   fval= aint(args(1))
         case("anint");  fval= anint(args(1))
         case("ceil","ceiling");   fval=ceiling(real(args(1)))
         case("exp");    fval= exp(args(1))
         case("floor");  fval= floor(real(args(1)))
         case("frac");   fval= args(1)-int(args(1))
         case("int");    fval= int(args(1))
         case("nint");   fval= nint(args(1))
         case("real");   fval= real(args(1))
         case("sqrt");   fval= sqrt(args(1))
!=======================================================================------------------------------------------------------------
         case("log")
            if(args(1).le.0.0d0)then                                          ! check for appropriate value range for function
               write(*,*)'*log* ERROR: cannot take log of ',real(args(1))
            else                                                              ! call function with one positive numeric parameter
               fval= log(args(1))
            endif
!=======================================================================------------------------------------------------------------
         case("log10")
            if(args(1).le.0.0d0)then                                          ! check for appropriate value range for function
               write(*,*)'*log10* ERROR: cannot take log of ',real(args(1))
            else                                                              ! call function with one positive numeric parameter
               fval= log10(args(1))
            endif
!=======================================================================------------------------------------------------------------
         end select
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("atan2","dim","mod","bessel_jn","bessel_yn","sign","hypot","modulo","scale")
      if(n.ne.2)then                                                           ! check number of parameters
        mssge='*funcs_* incorrect number of parameters in '//wstrng2(:iend)
        ier=-1
      elseif(.not.all(iargs_type(1:2).eq.0))then                                ! check type of parameters
        mssge='*funcs_* parameters not all numeric in '//wstrng2(:iend)
        ier=-1
      else                                                                     ! single numeric argument
         select case (wstrng2(:iend))
         case("atan2");      fval=  atan2       ( args(1),       args(2)       )
         case("dim");        fval=  dim         ( args(1),       args(2)       )
         case("mod");        fval=  mod         ( args(1),       args(2)       )
         case("modulo");     fval=  modulo      ( args(1),       args(2)       )
         case("scale");      fval=  scale       ( args(1),       int(args(2))  )
         case("bessel_jn");  fval=  bessel_jn   ( int(args(1)),  args(2)       )
         case("bessel_yn");  fval=  bessel_yn   ( int(args(1)),  args(2)       )
         case("btest");      fval=  merge(TRUE, FALSE,  btest( int(args(1)),  int(args(2))  ) )
         case("sign");       fval=  sign        ( args(1),       args(2)       )
         case("hypot");      fval=  hypot       ( args(1),       args(2)       )
         end select
      endif
!=======================================================================------------------------------------------------------------
case("tiny")
      fval=tiny(0.d0)
case("epsilon")
      fval=epsilon(0.d0)
case("huge")
      fval=huge(0.d0)
!=======================================================================------------------------------------------------------------
case("x");
      ivalue=int(args(1)+0.5d0)
      if(ivalue.lt.1.or.ivalue.gt.ixy_calc)then              ! if value not at least 1, or if not less than ixy_calc, report it
        mssge='*funcs_* illegal subscript value for x array'
        ier=-1
      else
        fval= x(ivalue)
      endif
!=======================================================================------------------------------------------------------------
case("y")
      ivalue=int(args(1)+0.5d0)
!     if value not at least 1, make it 1. if not less than ixy_calc, make it ixy_calc
      if(ivalue.lt.1.or.ivalue.gt.ixy_calc)then
         mssge='*funcs_* illegal subscript value for y array'
         ier=-1
      else
         fval= y(ivalue)
      endif
!=======================================================================------------------------------------------------------------
case("max")
      if(n.lt.1)then
         ier=-1
         mssge='*max* incorrect number of parameters for '//wstrng(:iend)
      elseif(.not.all(iargs_type(1:n).eq.0))then ! check type of parameters
         ier=-1
         mssge='*max* illegal parameter type (must be numeric)'
      else
         fval=args(1)
         do i=2,n
            fval=max(fval,args(i))
         enddo
      endif
!=======================================================================------------------------------------------------------------
case("min")
       if(n.lt.1)then
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
      elseif(.not.all(iargs_type(1:n).eq.0))then ! check type of parameters
         ier=-1
         mssge='*min* illegal parameter type (must be numeric)'
       else
          fval=args(1)
          do i=2,n
             fval=min(fval,args(i))
          enddo
       endif
!=======================================================================------------------------------------------------------------
case("xstore","ystore")                                        ! xstore function===>(where_to_start,value1,value2,value3...)
      if(n.lt.2)then                                           ! need at least subscript to start storing at and a value
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
         fval=0.0d0
      else                                                     ! at least two values so something can be stored
         istoreat=int(args(1)+0.50d0)                          ! array subscript to start storing values at
         if(istoreat.lt.1.or.istoreat+n-2.gt.ixy_calc)then     ! ignore -entire- function call if a bad subscript reference was made
            mssge='*funcs_* illegal subscript value for array in '//wstrng(:iend)
            ier=-1
            fval=0.0d0
         else                                                  ! legitimate subscripts to store at
            STEPTHRU: do i1033=2,n                             ! for each argument after the first one store the argument
               select case(wstrng2(:iend))                     ! select X array or Y array
                  case("xstore");x(istoreat)=args(i1033)
                  case("ystore");y(istoreat)=args(i1033)
               end select
               istoreat=istoreat+1                             ! increment location to store next value at
            enddo STEPTHRU
            fval=args(n)                                       ! last value stored will become current value
         endif
      endif
!=======================================================================------------------------------------------------------------
case("lle","llt","leq","lge","lgt","lne")
      if(iargs_type(1).eq.2.and.iargs_type(2).eq.2)then
         do i2020=1,n
            if(args(i2020).le.0.or.args(i2020).gt.size(values))then
               ier=-1
               mssge='unacceptable locations for strings encountered'
               goto 999
            endif
         enddo
         fval=FALSE ! assume false unless proven true
         i1=int(args(1))
         i2=int(args(2))
         ier=0
         select case (wstrng2(:iend))
         case("lle")
            if(values(i1).le.values(i2))fval=TRUE
         case("llt")
            if(values(i1).lt.values(i2))fval=TRUE
         case("leq") ! if any string matches the first
            do i410=2,n
               if(iargs_type(i410).ne.2)then     ! all parameters should be a string
                  ier=-1
                  mssge='non-string value encountered'
               elseif(values(i1).eq.values(int(args(i410)+0.5d0)))then
                  fval=TRUE
               endif
            enddo
         case("lge")
            if(values(i1).ge.values(i2))fval=TRUE
         case("lgt")
            if(values(i1).gt.values(i2))fval=TRUE
         case("lne")
            do i440=2,n
               fval=TRUE
               if(iargs_type(i440).ne.2)then     ! all parameters should be a string
                  ier=-1
                  mssge='non-string value encountered'
               elseif(values(i1).eq.values(int(args(i440)+0.5d0)))then
                  fval=FALSE
               endif
            enddo
         case default
            ier=-1
            mssge='internal error in funcs_ in lexical functions'
         end select
      else
         ier=-1
         mssge='lexical functions must have character parameters'
      endif
!=======================================================================------------------------------------------------------------
case("le","lt","eq","ge","gt","ne")
      fval=FALSE
      do i520=1,n
         if(iargs_type(i520).ne.0)then  ! this parameter was not a number
            ier=-1
            mssge='*logical* parameter was not a number'
            goto 999
         endif
      enddo
      if(n.eq.2.or.n.eq.3)then
         if(n.eq.3)then
            idig=int(args(3))
            if(idig.le.0.or.idig.gt.13)then
               mssge='*logical* precision must be between 1 and 13'
               ier=-1
               goto 999
            endif
            write(junout,'(a,3(g23.16e3,1x),i5)')'args=',args(1),args(2),args(3),idig
            write(*,*)junout
            arg1=round(args(1),idig)
            arg2=round(args(2),idig)
            write(junout,'(a,3(g23.16e3,1x),i5,1x,2(g23.16e3,1x))')'b. args=',args(1),args(2),args(3),idig,arg1,arg2
            write(*,*)junout
         else
            arg1=args(1)
            arg2=args(2)
         endif
         call stuff('LOGICAL1',arg1)
         call stuff('LOGICAL2',arg2)
         call stuff('STATUS',arg2-arg1)
         select case(wstrng2(:iend))
         case("le"); if(arg1.le.arg2)fval=TRUE
         case("lt"); if(arg1.lt.arg2)fval=TRUE
         case("eq"); if(arg1.eq.arg2)fval=TRUE
         case("ge"); if(arg1.ge.arg2)fval=TRUE
         case("gt"); if(arg1.gt.arg2)fval=TRUE
         case("ne"); if(arg1.ne.arg2)fval=TRUE
         case default
            ier=-1
            mssge='*logical* internal error in funcs_'
         end select
      else
         ier=-1
         mssge='*logical* must have 2 or 3 parameters'
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("ichar")
      if(n.ne.1)then
         mssge='*ichar* takes one parameter'
         ier=-1
      elseif(iargs_type(1).ne.2)then
         mssge='*ichar* parameter must be a string'
         ier=-1
      else
         fval=ichar(values(int(args(1)))(1:1))
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("sh")
   ii=int(args(1)+0.5d0)
   ilen=values_len(ii)
   if(ilen.ge.1)then
      call execute_command_line(values(ii)(:ilen),exitstat=idum)
      fval=idum
   else
      fval=0.0d0
   endif
   ctmp=' '
   iend=len_trim(ctmp)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$ge","$getenv") ! $getenv or $ge get system environment variable
   ii=int(args(1)+0.5d0)
   ilen=values_len(ii)
   if(ilen.ge.1)then
      call get_environment_variable(values(ii)(:ilen),ctmp)
      fval=len_trim(ctmp)
      if(fval.eq.0)then ! if value comes back blank and a non-blank default string is present, use it
         if(n.ge.2)then
            ii2=int(args(2)+0.5d0)
            ctmp=values(ii2)
            fval=values_len(ii2)
         endif
      endif
      fval=max(1.0d0,fval)
   else
      ctmp=' '
      fval=1.0d0
   endif
   ier=2
   iend=len_trim(ctmp)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("if")
      if(args(1).eq. TRUE)then
        fval=args(2)
      else
        fval=args(3)
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$if")                                      ! $if function
      ier=2                                      ! returning string
!     check that 2nd and 3rd are acceptable string variables, should do generically at name lookup time
      if(args(1).eq. TRUE)then
        ii=int(args(2))
      else
        ii=int(args(3))
      endif
      ctmp=values(ii)
      iend=values_len(ii)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("in")                                                            ! in(lower_value,value,upper_value)
      fval=FALSE
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      select case(n)
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case(2)                                                         ! if two parameters test }first - second}<epsilon
        if(iargs_type(1).eq.0.and.iargs_type(2).eq.0)then
           val=abs(args(1)-args(2))
           top=epsilon(0.0d0)
           bottom=-top
        else
         mssge='*in* parameters must be numeric'
         ier=-1
        endif
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case(3)                                                         ! if three parameters test if second between first and third
        if(iargs_type(1).eq.0.and.iargs_type(2).eq.0.and.iargs_type(3).eq.0)then
           bottom=args(1)
           val=args(2)
           top=args(3)
        else
         mssge='*in* parameters must be numeric'
         ier=-1
        endif
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case default
         mssge='*in* number of parameters not valid IN(LOWER_VALUE,VALUE,UPPER_VALUE)'
         ier=-1
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      end select
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      if(ier.ge.0) then
         if(val.ge.bottom.and.val.le.top)fval=TRUE
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("index")
      ii=int(args(1))
      iii=int(args(2))
      if(iargs_type(1).eq.2.and.iargs_type(2).eq.2)then ! if parameter was a string leave it alone
         iend1=values_len(ii)
         iend2=values_len(iii)
         fval=index(values(ii)(:iend1),values(iii)(:iend2))
      endif
      ier=0   ! flag that returning a number, not a string
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("len","len_trim")
      ii=int(args(1))
      iend1=values_len(ii)
      fval=len_trim(values(ii)(:iend1))
      ier=0   ! flag that returning a number, not a string
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("rand")                                                            ! random number
      select case (n)                                                   ! check number of parameters
      case (0)                                                          ! use default method
         itype=3
      case (1)                                                          ! determine user-specified method
         itype=int(args(1)+0.5d0)
         if(itype.lt.1.or.itype.gt.3)then
            itype=3
         endif
      case default
         mssge='illegal number of arguments for rand()'
         ier=-1
         itype=-1
      end select

      select case (itype)                                               ! select various methods
      case (-1)                                                         ! an error has already occurred
      case (2)                                                          ! standard Fortran function
         call random_number(harvest=fval)
      !!case default                                                      ! "Numerical Recipes" routine
      !!fval=ran_mod(ikeepran)
      case default
         call random_number(harvest=fval)
      end select
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("srand")                                                           ! seed random number sequence
      select case (n)                                                   ! check number of parameters
      case (1)                                                          ! no user-specified type
         itype=3                                                        ! use default method
      case (2)                                                          ! determine user-specified method
         itype=int(args(2)+0.5d0)                                         ! user-specified type
      case default                                                      ! call syntax error
         mssge='illegal number of arguments for srand()'
         ier=-1
      end select
      if(ier.eq.0)then
         ivalue=int(args(1)+0.5d0)                                           ! determine seed value
         select case (itype)                                               ! select various methods
         case (2)                                                          ! standard Fortran method
            call init_random_seed(ivalue)
         !!case (3)                                                          ! default is "Numerical Recipes" method
         !!   ikeepran=-abs(ivalue)
         !!   fval=ran_mod(ikeepran)                                         ! just setting seed; fval is a dummy here
         case default
            call init_random_seed(ivalue)
            !!mssge='unknown type for srand()'
            !!ier=-1
         end select
         fval=ivalue
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$f")                              ! $f(format,value) Using single format specifier, return string
      ier=2                             ! string will be returned
      if(n.eq.0)then
         ctmp=' '
      else
         ctmp=' '
         if(iargs_type(1).eq.2)then     ! if first field is a string
            ii=int(args(1))             ! get index into values() array
            iend1=values_len(ii)            ! maximum end is at end of string
            if(n.gt.1)fval=args(n)      ! get the real value
            write(ctmp,'('// values(ii)(:iend1)//')',iostat=ios)args(2:n)
            if(ios.ne.0)then
               ctmp='*'
               ier=-1
               mssge='*$f() error writing value'
            endif
         endif
      endif
      iend=len_trim(ctmp)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$char")
      ier=2                                      ! return string
      if(n.eq.0)then
         ier=-1
         mssge='*$char* must have at least one parameter'
      else
         iend=0
         do i3030=1,n                            ! unlike FORTRAN, can take multiple characters and mix strings and numbers
            ii=int(args(i3030))
            if(iargs_type(i3030).eq.2)then       ! if parameter was a string leave it alone
               iend2=iend+values_len(ii)
               ctmp(iend+1:iend2)=values(ii)
               iend=iend2
            else                                 ! convert numeric ADE to a character
               iend=iend+1
               ctmp(iend:iend)=char(ii)
            endif
         enddo
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$substr")                                  ! $substr(string,start,end)
      ier=2                                      ! return string
      if(n.eq.0)then
         ctmp=' '
      else
         ii=int(args(1))
         istart=1
         iend1=values_len(ii)                        ! maximum end is at end of string
         ctmp=' '
         if(iargs_type(1).eq.2)then
            if(n.gt.1)istart=min(max(1,int(args(2))),iend1)
            if(n.gt.2)iend1=max(min(int(args(3)),iend1),1)
            iend=iend1-istart+1
            ctmp(:iend)=values(ii)(istart:iend1)
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$nstore","$xstore","$ystore")
      ier=2                                      ! return string
      if(n.lt.2)then
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
      else
         ivalue=int(args(1)+0.5d0)
!        $nstore function===>store $n(where_to_start,value1,value2,value3...)
!        ignore -entire- function call if a bad subscript reference was made
         if(ivalue.lt.1.or.ivalue+n-2.gt.ixyc_calc)then
           mssge='illegal subscript value for array in '//wstrng2(:iend)
           ier=-1
         else
            do i1066=ivalue,ivalue+n-2,1
               isub=i1066-ivalue+2
               select case(wstrng2(:iend))
               case("$nstore"); nc(i1066)=values(int(args(isub)))
               case("$xstore"); xc(i1066)=values(int(args(isub)))
               case("$ystore"); yc(i1066)=values(int(args(isub)))
               end select
            enddo
            ctmp=values(ivalue+n-2)
            iend=len_trim(ctmp) ! very inefficient
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("str","$str","$") ! "$str" appends numbers and strings into a new string
                   ! "str" converts string to number IF string is simple numeric value
      jend=0
      ctmp=' '
      do i1010=1,n
         istart=jend+1                                     ! where to start appended argument in output string
         if(iargs_type(i1010).eq.2)then                    ! this parameter was a string
            in=int(args(i1010))                            ! the value of a string argument is the subscript for where the string is
            jend=istart+values_len(in)-1                       ! where appended argument ends in output string
            ctmp(istart:jend)=values(in)(:values_len(in))
         elseif(iargs_type(i1010).eq.0)then                ! this parameter was a number
            if(args(i1010).ne.0)then
               call value_to_string(args(i1010),cnum,ilen,ier,fmt='(g23.16e3)',trimz=.true.) ! minimum of 23 characters required
               if(ier.ne.-1)then
                  ilen=max(ilen,1)
                  jend=istart+ilen-1
                  if(cnum(ilen:ilen).eq.'.')jend=jend-1    ! this number ends in a decimal
                  jend=max(jend,istart)
                  if(jend.gt.len(ctmp))then
                     write(*,*)'*funcs_* $str output string truncated'
                     jend=len(ctmp)
                  endif
                  ctmp(istart:jend)=cnum(:ilen)
               endif
            else                                           ! numeric argument was zero
               ctmp(istart:istart)='0'
               jend=jend+1
            endif
         else
            mssge='*funcs_* parameter to function $str not interpretable'
            ier=-1
         endif
      enddo
      if(ier.ge.0)then
         select case(wstrng2(:iend))
         case("$str","$")
             ier=2
         case("str")
             ier=0
             call a_to_d_(ctmp,fval,ier)                    ! str function
         case default
            mssge='*funcs_* internal error: should not get here'
            ier=-1
         end select
      endif
      iend=jend
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$x","$y","$n")
      ier=2                                                  ! returning string
      ivalue=int(args(1)+0.5d0)
      if(ivalue.lt.1.or.ivalue.gt.ixyc_calc)then             ! if value not at least 1, or if not less than ixyc_calc, report it
        mssge='illegal subscript value for $x array'
        ier=-1
      else
         select case(wstrng2(:iend))
            case("$x");ctmp= xc(ivalue)
            case("$y"); ctmp= yc(ivalue)
            case("$n"); ctmp= nc(ivalue)
         end select
         iend=len_trim(ctmp) ! very inefficient
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("unusedf")
      fval=0.0d0
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$l") ! $l lower(string)
      ier=2                                      ! returning string
      if(n.ne.1)then
         ctmp=' '
         ier=-1
         mssge='*$l* must have one parameter'
      else
         ctmp=lower(values(int(args(1)+0.5d0)))
         iend=len_trim(ctmp) ! very inefficient
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$u")! $u upper(string)
      ier=2                                      ! returning string
      if(n.ne.1)then
         ctmp=' '
         ier=-1
         mssge='*$u* must have one parameter'
      else
         ctmp=upper(values(int(args(1)+0.5d0)))
         iend=len_trim(ctmp) ! very inefficient
      endif
!=======================================================================------------------------------------------------------------
case("c");      fval= myfunc(args,n)                          ! c(curve_number) or c(curve_number,index)
!=======================================================================------------------------------------------------------------
case("ownmode")                                               ! specify whether to look for substitute_subroutine(3f) routine
      if(n.eq.1.and.iargs_type(1).eq.0)then
         if(args(1).gt.0)then
            ownon=.true.
         else
            ownon=.false.
         endif
         fval= args(1)
      else
        mssge='*ownmode* illegal arguments'
        ier=-1
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("delimx")                ! 'delimx(istore,line,delimiters)  parse a string into $x array and return number of tokens
      if(n.ne.3)then                                                                     ! wrong number of parameters
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
      else
         if(iargs_type(2).ne.2)then
            mssge='*delimx* second parameter not a string'
           ier=-1
         else
            ctmp=values(int(args(2)+0.5d0))                                                ! string to parse
            if(iargs_type(3).ne.2)then
               mssge='*delimx* delimiter parameter not a string'
              ier=-1
            else
               ctmp2=values(int(args(3)+0.5d0))                                            ! delimiters
               if(iargs_type(1).ne.0)then
                  mssge='*delimx* first parameter not an index number'
                 ier=-1
               else
                  istore=int(args(1)+0.5d0)                                                ! where to start storing into $n array at
                  call delim(ctmp,['#NULL#'],ixyc_calc,icount,ibegin,iterm,ilen,ctmp2)
                  if(istore.lt.1.or.istore+n-2.gt.ixyc_calc)then  ! ignore entire function call if bad subscript reference was made
                     mssge='illegal subscript value for array in delim'
                     ier=-1
                  else
                     do i1060=1,icount
                        xc(istore)=ctmp(ibegin(i1060):iterm(i1060))
                        istore=istore+1
                     enddo
                     fval=icount  ! return number of tokens found
                     ier=0
                  endif
               endif
            endif
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("round")
      if(n.ne.2)then                                                           ! check number of parameters
        mssge='*funcs_* incorrect number of parameters in '//wstrng2(:iend)
        ier=-1
      elseif(.not.all(iargs_type(1:2).eq.0))then                                ! check type of parameters
        mssge='*funcs_* parameters not all numeric in '//wstrng2(:iend)
        ier=-1
      else                                                                      ! single numeric argument
         fval=round(args(1),int(args(2)))
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("ifdef")
      fval=-1
      if(n.ne.1)then                                             ! check number of parameters
         mssge='*ifdef* incorrect number of parameters in '//wstrng(:iend)
         ier=-1
      elseif(iargs_type(1).ne.2)then                             ! the parameter should be a name of a variable
         mssge='*ifdef* name not a string:'//wstrng(:iend)
         ier=-1
      else
         ii=int(args(1))                                         ! get index into values() array
         iend1=values_len(ii)                                    ! maximum end is at end of string
         if(values(ii)(1:1).eq.'$')then
            call locate(keys_q,values(ii)(:iend1),indexout,ierr) ! determine if the string variable name exists
         else
            call locate(keyr_q,values(ii)(:iend1),indexout,ierr) ! determine if the numeric variable name exists
         endif
         if(ierr.ne.0)then                                       ! unexpected error
            ier=-1
         elseif(indexout.gt.0)then                               ! found variable name
            fval=0
         else                                                    ! did not find variable name
            fval=-1
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("ye","mo","da","ho","mi","se","timezone","year","month","day","hour","minute","second","tz")
         icalen=1                                                              ! default value that is safe even if an error occurs
         !------------------------------------------
         call date_and_time(values=idarray)
         !------------------------------------------
         if(n.eq.0)then
            select case(wstrng2(:iend))                                           ! select desired subscript of value to return
               case("ye","year");     icalen=1                                    ! year
               case("mo","month");    icalen=2                                    ! month
               case("da","day");      icalen=3                                    ! day
               case("tz","timezone"); icalen=4                                    ! days since Sunday [ 0-6]
               case("ho","hour");     icalen=5                                    ! hour
               case("mi","minute");   icalen=6                                    ! minute
               case("se","second");   icalen=7                                    ! second
               case default                                                       ! report internal error if name was not matched
                  ier=-1
                  mssge='*calendar* internal error, unknown keyword'//wstrng2(:iend)
               end select
            if(ier.eq.0)then                                                      ! if error flag not set set return value
               fval=idarray(icalen)
            else                                                                  ! error has occurred, set default return value
               fval=0.0d0
            endif
         else
            ier=-1
            mssge='*calendar* parameters not allowed'
         endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$mo")                             ! $mo(1-12) is "January, February, ... ")
      ctmp=''
      ier=2                             ! string will be returned
      if(n.lt.1)then                    ! $mo() use today
         call date_and_time(values=idarray)
         ival=idarray(2)
      elseif(n.eq.1)then                ! $mo(N) just index into month names
         ival=mod(int(args(1))-1,12)+1
         if(ival.le.0)ival=ival+12
      elseif(args(2).eq.1)then          ! $mo(YYYYMMDD,1) returns MM
         ival=int(args(1))
         ival=ival-((ival/10000)*10000) ! reduce to a four-digit value
         ival=ival/100                  ! keep two high digits out of the four
         ival=mod(ival-1,12)+1          ! ensure in range 1 to 12
         if(ival.le.0)ival=ival+12
      else
         ival=1
         ctmp='UNKNOWN'
         iend=7
         mssge='*$mo* parameter(s) not valid'
         ier=-1
      endif
      if(ctmp.eq.'')then
         ctmp=months(ival)
      endif
      iend=len_trim(ctmp(1:20))
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case default
      if(ownon)then
!        ==>wstrng(1:iend)=procedure name.
!        ==>iend=length of procedure name.
!        ==>args=array of ixy_calc elements containing procedure arguments.
!        ==>iargs_type=type of argument
!        ==>n=integer number of parameters
!        ==>x=array of ixy_calc x values
!        ==>y=array of ixy_calc y values
!        ==>ctmp is returned string if string function is called ( in which case fval is returned number of characters in ctmp)
         ier=0
         call mysub(wstrng(:iend),iend,args,iargs_type,n,fval,ctmp,ier)
!        <==fval=returned value to replace function call with
!        <=>ier=returned error flag.  Set to -1 if an error occurs.  Otherwise, user should leave it alone
         if(ier.eq.-1)then
         elseif(ier.eq.2)then
            iend=int(fval) ! string functions should return string length in fval
            if(fval.le.0)then
               mssge='*funcs_* bad length for returned string'
               ier=-1
            endif
         else
            ier=0
         endif
      else
        mssge='*funcs_* function not found: '//wstrng(:iend)
        ier=-1                           ! ya done blown it if you get here
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end select
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue             ! return based on value of ier
      select case(ier)
      case(2)   ! return string value
        call stufftok_(fval,wstrng,nchars,ctmp(:iend),iend,ier) ! add a new token variable and assign string to it
      case(0)   ! return numeric value
        call value_to_string(fval,wstrng,nchars,idum,fmt='(g23.16e3)',trimz=.true.) ! minimum of 23 characters required
      case(-1)  ! return error
      case default
         write(*,*)'*funcs_* unknown closing value ',ier
         ier=-1
      end select
end subroutine funcs_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    stufftok_(3fp)- [M_calculator] add a new token variable and assign string to it
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine stufftok_(fval,wstrng,nchars,string,iend,ier)

!character(len=*),parameter::ident_5="@(#)M_calculator::stufftok_(3fp): add a new token variable and assign string to it"

real(kind=dp)          :: fval
character(len=*)       :: wstrng
integer                :: nchars
character(len=*)       :: string
integer                :: iend
integer                :: ier

character(len=5)       :: toknam
!-----------------------------------------------------------------------------------------------------------------------------------
   ktoken=ktoken+1                            ! increment the counter of strings found to get a place to store into
   nchars=5
   write(toknam,'(''$_'',i3.3)')ktoken        ! build a unique name for the token string found for this output string
   wstrng=toknam
   call stuffa(toknam,string(:iend))          ! cannot do this earlier or indexs from call that defined args could be wrong
   fval=0.0d0
   ier=2
   mssge=string(:iend)
end subroutine stufftok_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    args_(3fp)- [M_calculator] given 'par1,par2,...' store non-parenthesized expression par(n) into a real or string array
!! (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine args_(line,ilen,array,itype,iarray,ier,mx)

!character(len=*),parameter::ident_6="&
!&@(#)M_calculator::args_(3fp):given 'par1,par2,...' store non-parenthesized expression par(n) into a real or string array"

!@ (#) record type of par(n) into itype()"
!@ (#) Commas are only legal delimiters. extra or redundant delimiters are ignored.

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)           :: line      ! input string
integer,intent(in)                    :: ilen      ! length of input string
integer,intent(in)                    :: mx        ! up to mx par(i) will be extracted. if more found an error is generated.
real(kind=dp),intent(out)             :: array(mx)
integer,intent(out)                   :: itype(mx) ! itype=0 for number, itype=2 for string
integer,intent(out)                   :: iarray    ! number of parameters found
integer                               :: ier       ! ier=-1 if error occurs, ier undefined (not changed) if no error.
!-----------------------------------------------------------------------------------------------------------------------------------
integer :: icalc
integer :: icol
integer :: iend
integer :: ilook
integer :: istart
character(len=1),parameter :: delimc=','
character(len=icbuf_calc)  :: wstrng
!-----------------------------------------------------------------------------------------------------------------------------------
   iarray=0
   if(ilen.eq.0)then  ! check if input line (line) was totally blank
      return
   endif
!  there is at least one non-delimiter character in the command.
!  ilen is the column position of the last non-blank character
!  find next non-delimiter
   icol=1
   do ilook=1,mx,1
     do
        if(line(icol:icol).ne.delimc)then
           iarray=iarray+1
           istart=icol
           iend=index(line(istart:ilen),delimc)
           if(iend.eq.0)then   ! no delimiter left
              icalc=ilen-istart+1
              wstrng=line(istart:ilen)
              ier=0
              call expressions_(wstrng,icalc,array(iarray),ier)
              itype(iarray)=ier
              return
           else
              iend=iend+istart-2
              icalc=iend-istart+1
              wstrng=line(istart:iend)
              ier=0
              call expressions_(wstrng,icalc,array(iarray),ier)
              itype(iarray)=ier
              if(ier.eq.-1)return
           endif
           icol=iend+2
           exit
        else
           icol=icol+1
           if(icol.gt.ilen)return       ! last character in line was a delimiter, so no text left
        endif
     enddo
     if(icol.gt.ilen)return       ! last character in line was a delimiter, so no text left
   enddo
   write(mssge,'(a,i4,a)')'more than ',mx,' arguments not allowed'
   ier=-1
end subroutine args_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!! expressions_(3fp) - [M_calculator] resolve a series of terms into a single value and restring
!! (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine expressions_(string,nchar,value,ier)

!character(len=*),parameter::ident_7="&
!&@(#)M_calculator::expressions_(3fp): resolve a series of terms into a single value and restring"

character(len=*),intent(inout) :: string
integer,intent(inout)          :: nchar
real(kind=dp),intent(out)      :: value
integer,intent(out)            :: ier
character(len=icbuf_calc) :: dummy            ! no single term may be over (icbuf_calc) characters
integer :: ier2
integer :: iend
integer :: iendm
integer :: iendp
integer :: ista
integer :: istat
integer :: nchar2
real(kind=dp) :: temp
!-----------------------------------------------------------------------------------------------------------------------------------
                               !!!!! what happens if the returned string is longer than the input string?
      value=0.0d0              ! initialize sum value to be returned to 0
      if(nchar.eq.0) return    ! if this is a null string return
                 ! first cut at handling string variables. assuming, with little checking, that the only string expression
                 ! that can get here is a single variable name (or variable token) and that string variable names start with a $
                 ! and that the error flag should be set to the value 2 to indicate that a string, not a number, is being returned
                 ! for the 2 to get back, it must not be changed by this routine or anything it calls
      if(string(1:1).eq.'$')then
         call given_name_get_stringvalue_(string,ier)
         if(ier.eq.-1)return
         ier=2                 ! flag that a character string is being returned
!x       return
      endif
!x!!!!!
      ista=1                                        ! initialize the position of the unary sum operator for the current term
      if(index('#=',string(1:1)).ne.0)then          ! check if input string starts with a unary (+-) operator
        istat=2                                     ! a starting unary sum operator is present, so the first term starts in column 2
      else                                          ! input string does not start with a unary sum (-+) operator
        istat=1                                     ! no initial sum operator is present, so the first term starts in column 1
      endif
      do
         iendp=index(string(istat:nchar),'#')     ! find left-most addition operator
         iendm=index(string(istat:nchar),'=')     ! find left-most subtraction operator
         iend=min(iendp,iendm)                    ! find left-most sum (+-) operator assuming at least one of each exists
         if(iend.eq.0)iend=max(iendm,iendp)       ! if one of the sum operators is not remaining, find left-most of remaining type
         if(iend.eq.0)then                        ! if no more sum operators remain this is the last remaining term
            iend=nchar                            ! find end character of remaining term
         else                                     ! more than one term remains
            iend=iend+istat-2                     ! find end character position of this (left-most) term
         endif
         dummy=string(istat:iend)                 ! set string dummy to current(left-most) term
         nchar2=iend-istat+1                      ! calculate number of characters in current term
!        given that the current term ( dummy) is an optionally signed string containing only the operators **, * an / and no
!        parenthesis, reduce the string to a single value and add it to the sum of terms (value). do not change the input string.
         call pows_(dummy,nchar2,ier)            ! evaluate and remove ** operators and return the altered string (dummy)
         if(ier.eq.-1) return                     ! if an error occurred, return
         call factors_(dummy,nchar2,temp,ier)       ! evaluate and remove * and / operators, return the evaluated -value- temp
         if(ier.eq.-1)return                      ! if an error occurred, return
         if(string(ista:ista).eq.'=')then         ! if term operator was a subtraction, subtract temp from value
            value=value-temp
         else                                     ! operator was an addition (+) , add temp to value
!           if first term was not signed, then first character will not be a subtraction, so addition is implied
            value=value+temp
         endif
         ista=iend+1                      ! calculate where next sum operator (assuming there is one) will be positioned in (string)
         istat=ista+1                     ! calculate where beginning character of next term will be (if another term remains)
         if(iend.ne.nchar)then
            if(istat.gt.nchar)then                ! a trailing sum operation on end of string
               ier=-1
               mssge='trailing sum operator'
               return
            endif
         ! if last term was not the end of (string) terms remain. keep summing terms
         else
            exit
         endif
      enddo
      call value_to_string(value,string,nchar,ier2,fmt='(g23.16e3)',trimz=.true.) ! convert sum of terms to string and return
      if(ier2.lt.0)ier=ier2
end subroutine expressions_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!! (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine pows_(wstrng,nchar,ier)
!!
!!    character(len=*),intent(inout) :: wstrng
!!    integer,intent(inout)          :: nchar
!!    integer                        :: ier
!!##DESCRIPTION
!!    given an unparenthesized string of form:
!!
!!       stringo opo fval1 ** fval2 opo2 stringo2
!!
!!    where opo is a preceding optional operator from set /,* and
!!    stringo is the string that would precede opo when it exists,
!!    and opo2 is an optional trailing operator from set /,*,**
!!    and stringo2 the string that would follow op2 when it exists,
!!    evaluate the expression fval1**fval2 and restring it; repeating
!!    from left to right until no power operators remain in the string
!!    or an error occurs
!!
!!##OPTIONS
!!    wstrng  input string returned with power operators evaluated
!!    nchar   input length of wstrng, returned corrected for new wstrng returned.
!!    ier     error status
!!
!!
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine pows_(wstrng,nchar,ier)

!character(len=*),parameter::ident_8="@(#)M_calculator::pows_(3fp): expand power functions in a string, working from left to right"

character(len=*),intent(inout) :: wstrng    ! input string returned with power operators evaluated
integer,intent(inout)          :: nchar     ! input length of wstrng, returned corrected for new wstrng returned.
integer                        :: ier       ! error status
character(len=iclen_calc)     :: tempch
character(len=icbuf_calc)      :: dummy
character(len=1)               :: z
real(kind=dp)                  :: fval1
real(kind=dp)                  :: fval2
integer                        :: i
integer                        :: id2
integer                        :: idum
integer                        :: im2
integer                        :: ip        ! position of beginning of first ** operator
integer                        :: ip2
integer                        :: iright    ! end of fval2 string
integer                        :: iz        ! beginning of fval1 string
integer                        :: nchart
!-----------------------------------------------------------------------------------------------------------------------------------
      INFINITE: do
!        find first occurrence of operator, starting at left and moving right
         ip=index(wstrng(:nchar),'**')
         if(ip.eq.0) then
            exit INFINITE
         elseif(ip.eq.1) then
            ier=-1
            mssge='power function "**" missing exponentiate'
            exit INFINITE
         elseif((ip+2).gt.nchar) then
            ier=-1
            mssge='power function "**" missing power'
            exit INFINITE
         endif
!
!        find beginning of fval1 for this operator. go back to
!        beginning of string or to any previous * or / operator
         FINDVAL: do i=ip-1,1,-1
            iz=i
            z=wstrng(i:i)
               if(index('*/',z).ne.0)then  ! note that use of index function was faster than .eq. on cyber
                  iz=iz+1
                  goto 11
               endif
         enddo FINDVAL
         iz=1
11       continue
         if(ip-iz.eq.0)then
           ier=-1
           mssge='operator / is beside operator **'
           exit INFINITE
         endif
!
!        now isolate beginning and end of fval2 string for current operator
!        note that looking for * also looks for ** operator, so checking
!        for * or / or ** to right
!
         im2=index(wstrng((ip+2):nchar),'*')
         id2=index(wstrng((ip+2):nchar),'/')
         ip2=min0(im2,id2)
         if(ip2.eq.0)ip2=max0(im2,id2)
         if(ip2.eq.0)then
           iright=nchar
         elseif(ip2.eq.1)then
           ier=-1
           mssge='two operators from set [*/**] are side by side'
           exit INFINITE
         else
           iright=ip2+ip
         endif
         call a_to_d_(wstrng(iz:ip-1),fval1,ier)
         if(ier.eq.-1)then
            exit INFINITE
         endif
         call a_to_d_(wstrng(ip+2:iright),fval2,ier)
         if(ier.eq.-1)then
            exit INFINITE
         endif
         if(fval1.lt.0.0d0)then
!           this form better/safe? if(abs( fval2-int(fval2)/fval2).le..0001)
            if(fval2-int(fval2).eq.0.0d0)then
               fval1=fval1**int(fval2)
            else
               mssge='negative to the real power not allowed'
               ier=-1
               exit INFINITE
            endif
         else
            fval1=fval1**fval2
         endif
         call value_to_string(fval1,tempch,nchart,idum,fmt='(g23.16e3)',trimz=.true.) ! minimum of 23 characters required
!        place new value back into string and correct nchar.
!        note that not checking for nchar greater than (icbuf_calc)
!        in dummy or greater than len(wstrng).
         if(iz.eq.1.and.iright.eq.nchar)then      ! there was only one operator and routine is done
            dummy=tempch(1:nchart)
            nchar=nchart
         else if(iz.eq.1)then                     ! iz was 1, but iright was nchar so
            dummy=tempch(1:nchart)//wstrng(iright+1:nchar)
            nchar=nchart+nchar-(iright+1)+1
         else if(iright.eq.nchar)then             ! iz was not 1, but iright was nchar so
            dummy=wstrng(1:iz-1)//tempch(1:nchart)
            nchar=(iz-1)+nchart
         else                                     ! iz was not 1, and iright was not nchar so
            dummy=wstrng(1:iz-1)//tempch(1:nchart)//wstrng(iright+1:nchar)
            nchar=(iz-1)+nchart+(nchar-(iright+1)+1)
         endif
         wstrng=dummy
      enddo INFINITE
end subroutine pows_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!! (LICENSE:PD)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine factors_(wstrng,nchr,fval1,ier)

!character(len=*),parameter::ident_9="&
!&@(#)M_calculator::factors_(3fp):reduce unparenthesized string with only * and / operators to val"

!
!     The input string is unaltered. for any single pass thru the routine, the string structure is assumed to be:
!         fval1 op fval2 op fval op fval op fval op fval
!     where no blanks are in the string (only significant if string structure is bad) and the only operators are * or /.
!     working from left to right:
!       1. locate and place into a real variable the fval1 string
!       2. if one exists, locate and place into a real variable the fval2 string
!       3. perform the indicated operation between fval1 and fval2
!          and store into fval1.
!       3. repeat steps 2 thru 4 until no operators are left or
!          an error occurs.
!
!     nchr   = the position of the last non-blank character in the input string wstrng
!     ip     = the position of the current operator to be used.
!              to the left of this is the fval1 string.
!     iright = the position of the last character in the fval2 string.
!     wstrng = the input string to be interpreted.
!       ier  = is a flag indicating whether an error has occurred
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*) :: wstrng
real(kind=dp)    :: fval1
real(kind=dp)    :: fval2
integer          :: id
integer          :: id2
integer          :: ier
integer          :: im
integer          :: im2
integer          :: ip
integer          :: ip2
integer          :: iright
integer          :: nchr
!-----------------------------------------------------------------------------------------------------------------------------------
      if((nchr).eq.0)then
         ier=-1
         mssge='trying to add/subtract a null string'
         return
      endif
!     find position of first operator
      im=index(wstrng(:nchr),'*')
      id=index(wstrng(:nchr),'/')
!     ip should be the position of the left-most operator
      ip=min0(im,id)
!     if one or both of the operators were not present, then
!     either im or id (or both) are zero, so look for max
!     instead of min for ip
      if(ip.eq.0) ip=max0(im,id)
      if( ip.eq.0 )then
!        no operator character (/ or *) left
         call a_to_d_(wstrng(1:nchr),fval1,ier)
         return
      elseif (ip.eq.1)then
!        if no string to left of operator, have a bad input string
         ier=-1
         mssge='first factor or quotient for "*" or "/" missing or null'
         return
      endif
!     convert located string for fval1 into real variable fval1
      call a_to_d_(wstrng(1:ip-1),fval1,ier)
      if(ier.eq.-1)return
      do
         if(ip.eq.nchr)then
!          if no string to left of operator, have a bad input string
           ier=-1
           mssge='second factor or quotient for "*" or "/" missing or null'
           return
         endif
!        locate string to put into fval2 for current operator by starting just to right of operator and ending at end of current
!        string or at next operator note that because of previous checks we know there is something to right of the operator.
         im2=index(wstrng((ip+1):nchr),'*')
         id2=index(wstrng((ip+1):nchr),'/')
         ip2=min0(im2,id2)
         if(ip2.eq.0)ip2=max0(im2,id2)
         if(ip2.eq.0)then
            iright=nchr
         elseif(ip2.eq.1)then
            ier=-1
            mssge='two operators from set [*/] are side by side'
            return
         else
            iright=ip2+ip-1
         endif
!        place located string for fval2 into real variable fval2
         call a_to_d_(wstrng(ip+1:iright),fval2,ier)
         if(ier.eq.-1)return
!        do specified operation between fval1 and fval2
         if(wstrng(ip:ip).eq.'*') then
            fval1=fval1*fval2
         else if(fval2.eq.0) then
            ier=-1
            mssge='division by zero'
            return
         else
            fval1=fval1/fval2
         endif
         if(iright.eq.nchr)return
         ip=iright+1
      enddo
end subroutine factors_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     a_to_d_(3f) - [M_calculator] returns a double precision value from a numeric character string specifically for M_calculator(3fm)
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine a_to_d_(chars,rval,ierr)
!!
!!    character(len=*),intent(in) :: chars
!!    doubleprecision,intent(out) :: rval
!!    integer,intent(out)         :: ierr
!!
!!##DESCRIPTION
!!    Convert a string representing a numeric scalar value to a numeric value, specifically
!!    for the M_calculator(3fp) module.Works with any g-format input, including integer, real, and exponential forms.
!!
!!       1. if chars=? set rval to value stored as current value, return.
!!       2. if the string starts with a $ assume it is the name of a
!!          string variable or token and return its location as a doubleprecision number.
!!       3. try to read string into a doubleprecision value. if successful, return.
!!       4. if not interpretable as a doubleprecision value, see if it is a
!!          defined variable name and use that name's value if it is.
!!       5. if no value can be associated to the string and/or if
!!          an unexpected error has occurred, set error flag and
!!          error message and set rval to zero and return.
!!       6. note that blanks are treated as null, not zero.
!!
!!##OPTIONS
!!      chars  is the input string
!!      rval   is the doubleprecision output value
!!      ierr   0 if no error occurs
!!
!!##EXAMPLE
!!
!!
!!##VERSION
!!       o 07/15/1986  J. S. Urban
!!       o 12/28/1987  modified to specify bn in formats for reads. vax
!!                    defaults to zero-fill on internal files.
!!       o 12/22/2016  Changed to generate man(1) pages via ufpp(1).
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine a_to_d_(chars,rval8,ierr)

!character(len=*),parameter::ident_10="&
!&@(#)M_calculator::a_to_d_(3f):returns a real value rval8 from a numeric character string chars."

! CAREFUL: LAST is in GLOBAL, but can be read from when passed to this routine as CHARS. DO NOT CHANGE CHARS.
character(len=*),intent(in) :: chars
character(len=:),allocatable :: chars_local
real(kind=dp),intent(out)   :: rval8
integer,intent(out)         :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=13)           :: frmt
integer                     :: ier
integer                     :: indx
integer                     :: ioerr
!-----------------------------------------------------------------------------------------------------------------------------------
   ioerr=0
   chars_local=trim(adjustl(chars))//' ' ! minimum of one character required
   if(chars_local.eq.'?')then       ! if string is a (unsigned) question mark, use value returned from last completed calculation
     read(last,'(bn,g256.40)',iostat=ioerr,err=9991)rval8   ! assuming cannot get a read error out of reading last
   elseif('$'.eq.chars_local(1:1))then                ! string is a string variable name
      call locate(keys_q,chars_local,indx,ier)        ! try to find the index in the character array for the string variable
      if(indx.le.0)then                         ! if indx is not .gt. 0 string was not a variable name
         ierr=-1
      mssge='undeclared string variable '//chars_local
      else
         rval8=real(indx)   ! set value to position of string in the string array
         !!!! flag via a value for ierr that a string, not a number, has been found
      endif
      return
   ! no error on read on Sun on character string as a number, so make sure first character not numeric and try as variable name
   elseif(index('0123456789.-+',chars_local(1:1)).eq.0)then   ! does not start with a numeric character. try as a variable name
      call locate(keyr_q,chars_local,indx,ier)
      if(indx.le.0)then                                 ! if indx is not .gt. 0 string was not a variable name
         ierr=-1
       mssge='*a_2_d_* undeclared variable ['//chars_local//']'
      else
         rval8=values_d(indx)
      endif
      return
   else                            ! string is a number or a numeric variable name that starts with a numeric character
     write(frmt,101)len(chars_local)                       ! build a format statement to try and read the string as a number with
101  format( '(bn,g',i5,'.0)' )
chars_local=chars_local//repeat(' ',512)                   ! kludge: problems if string is not long enough for format
     read(chars_local,fmt=frmt,iostat=ioerr,err=999)rval8  ! try and read the string as a number
     chars_local=trim(chars_local)
   endif
   return                                ! string has successfully been converted to a number
9991  continue                           ! string could not be read as number,so try as variable name that starts with number
999   continue                           ! string could not be read as number,so try as variable name that starts with number
   rval8=0.0d0
   indx=0
   !  either here because of a read error (too big, too small, bad characters in string) or this is a variable name
   !  or otherwise unreadable.
   !!!!! look carefully at what happens with a possible null string
   call locate(keyr_q,chars_local,indx,ier)
   if(indx.le.0)then                             ! if indx is not .gt. 0 string was not a variable name
      mssge='bad variable name or unusable value = '//chars_local
      ierr=-1
   else
      rval8=values_d(indx)
   endif
end subroutine a_to_d_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    squeeze_ - [M_calculator] change +-[] to #=(),replace strings with placeholders,delete comments
!!    (LICENSE:PD)
!!
!!##DESCRIPTION
!!    remove all blanks from input string and return position of last non-blank character in nchars using imax as the highest
!!    column number to search in.  return a zero in nchars if the string is blank.
!!
!!    replace all + and - characters with the # and = characters which will be used to designate + and - operators, as opposed to
!!    value signs.
!!
!!    replace [] with ()
!!
!!    remove all strings from input string and replace them with string tokens and store the values for the string tokens.
!!    assumes character strings are (iclen_calc) characters max.
!!    if string is delimited with double quotes, the double quote character may be represented inside the string by
!!    putting two double quotes beside each other ("he said ""greetings"", i think" ==> he said "greetings", i think)
!!
!!  !!!! if an equal sign is followed by a colon the remainder of the input line is placed into a string as-is
!!  !!!! without the need for delimiting it. ($string1=: he said "greetings", i think ==> he said "greetings", i think)
!!
!!    anything past an # is considered a comment and ignored
!!
!!    assumes length of input string is less than (icbuf_calc) characters
!!
!!    if encounters more than one equal sign, uses right-most as the
!!    end of variable name and replaces others with & and makes a
!!    variable name out of it (ie a=b=10 ===> a&b=10)
!!
!!  !!!!the length of string could actually be increased by converting quoted strings to tokens
!!
!!  !!!!maybe change this to allow it or flag multiple equal signs?
!!
!!  !!!!no check if varnam is a number or composed of characters
!!  !!!!like ()+-*/. . maybe only allow a-z with optional numeric
!!  !!!!suffix and underline character?
!!
!!  !!!!variable names ending in letter e can be confused with
!!  !!!!e-format numbers (is 2e+20 the variable 2e plus 20 or
!!  !!!!the single number 200000000000000000000?). to reduce
!!  !!!!amount of resources used to check for this, and since
!!  !!!!words ending in e are so common, will assume + and -
!!  !!!!following an e are part of an e-format number if the
!!  !!!!character before the e is a period or digit (.0123456789).
!!  !!!!and won't allow variable names of digit-e format).
!!
!!  !!!!make sure variable called e and numbers like e+3 or .e+3 are handled satisfactorily
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine squeeze_(string,imax,nchars,varnam,nchar2,ier)

!character(len=*),parameter::ident_11="&
!&@(#)M_calculator::squeeze_(3fp):change +-[] to #=(),replace strings with placeholders,delete comments"

integer, parameter                      :: ilen=(icbuf_calc)+2
character(len=*)                        :: string
integer                                 :: imax
integer,intent(out)                     :: nchars
character(len=icname_calc),intent(out)  :: varnam
integer,intent(out)                     :: nchar2
integer,intent(out)                     :: ier
character(len=ilen)                     :: dummy
character(len=1)                        :: back1
character(len=1)                        :: back2
integer                                 :: iplace
character(len=1)                        :: currnt
character(len=iclen_calc)               :: ctoken
!!character(len=10),parameter             :: list  =' +-="#[]{}'  ! list of special characters
!!character(len=10),parameter             :: list2 =' #=&  ()()'  ! list of what to convert special characters too when appropriate
character(len=5)                        :: toknam
integer                                 :: i10
integer                                 :: i20
integer                                 :: idum
integer                                 :: ilook
integer                                 :: indx
integer                                 :: instring
integer                                 :: ipoint
integer                                 :: ivar
integer                                 :: kstrln
!-----------------------------------------------------------------------------------------------------------------------------------
!  keep track of previous 2 non-blank characters in dummy for when trying to distinguish between e-format numbers
!  and variables ending in e.
   back1=' '
   back2=' '
   varnam=' '                   ! initialize output variable name to a blank string
   ivar=0
   nchar2=0
   nchars=0                     ! the position of the last non-blank character in the output string (string)
   dummy(1:2)='  '
!-----------------------------------------------------------------------------------------------------------------------------------
!  instead of just copy string to buffer, cut out rows of sign operators
!  dummy(3:)=string
   dummy=' '
   idum=3
   instring=0
   do i10=1,len_trim(string)
      ! if adjacent sign characters skip new character and maybe change sign of previous character
      if(string(i10:i10).eq.'"'.and.instring.eq.0 )then   ! starting a string
         instring=1
      elseif(string(i10:i10).eq.'"'.and.instring.eq.1)then ! ending a string
         instring=0
      endif
      if(instring.ne.1)then
         if(string(i10:i10).eq.'+')then                 ! if found a + look to see if previous a + or -
            if(dummy(idum-1:idum-1).eq.'+')then         ! last character stored was also a sign (it was +)
               cycle                                    ! skip because ++ in a row
            elseif(dummy(idum-1:idum-1).eq.'-')then     ! skip -+ and just leave -
               cycle
            endif
         elseif(string(i10:i10).eq.'-')then             ! last character stored was also a sign (it was -)
            if(dummy(idum-1:idum-1).eq.'+')then         ! +- in a row
               dummy(idum-1:idum-1)='-'                 ! change sign of previous plus
               cycle                                    ! skip because +- in a row
            elseif(dummy(idum-1:idum-1).eq.'-')then     ! skip but change sign of previous
               dummy(idum-1:idum-1)='+'                 ! change -- to +
               cycle
            endif
         endif
      endif
      ! character not skipped
      dummy(idum:idum)=string(i10:i10)            ! simple copy of character
      idum=idum+1
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   string=' '
   ipoint=2                                       ! ipoint is the current character pointer for (dummy)
   ktoken=0                                       ! initialize the number of strings found in this string
   BIG: do ilook=1,imax
      ipoint=ipoint+1                             ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)                 ! store current character into currnt
      select case(currnt)            ! check to see if current character has special meaning and requires processing ' +-="#[]{}'
!-----------------------------------------------------------------------------------------------------------------------------------
      case(" ")                                   ! current is a blank not in a string. ignore it
         cycle BIG
!-----------------------------------------------------------------------------------------------------------------------------------
      case("+")                                      ! current is a plus
         if(back1.eq.'e'.or.back1.eq.'E')then        ! if previous letter was an e it could be e-format sign or operator.
!           note not using dummy directly, as it may contain blanks letter before +- was an e. must decide if the +- is part of
!           an e-format number or intended to be the last character of a variable name.
!!!!!       what is effect on a---b or other +- combinations?
            ! if letter before e is not numeric this is a variable name and - is an operator
            if(index('0123456789.',back2).eq.0)then
              currnt="#"                        ! no digit before the e, so the e is the end of a variable name
            else                                ! digit before e, so assume this is number and do not change +- to #= operators
            endif
         else
            currnt="#"                          ! previous letter was not e, so +- is an operator so change +- to #= operators
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      case("-")                                      ! current is a minus
         if(back1.eq.'e'.or.back1.eq.'E')then        ! if previous letter was an e it could be e-format sign or operator.
!           note not using dummy directly, as it may contain blanks letter before +- was an e. must decide if the +- is part of
!           an e-format number or intended to be the last character of a variable name.
!!!!!       what is effect on a---b or other +- combinations?
            ! if letter before e is not numeric this is a variable name and - is an operator
            if(index('0123456789.',back2).eq.0)then
              currnt="="                       ! no digit before the e, so the e is the end of a variable name
            else                               ! digit before e, so assume this is number and do not change +- to #= operators
            endif
         else
            currnt="="                         ! previous letter was not e, so +- is an operator so change +- to #= operators
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      case("=")                                      ! current is a plus or minus
         currnt="&"
         ivar=nchars+1                               ! ivar is the position of an equal sign, if any
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case ("{", "[")
      currnt='('                                     ! currnt is [ or { . Replace with (
      case ("}", "]")
      currnt=')'                                     ! currnt is ] or }, . Replace with )
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case("#")                                      ! any remainder is a comment
         exit
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case('"')                                   ! if character starts a quoted string, extract it and replace it with a token
!     figure out length of string, find matching left double quote, reduce internal "" to "
      kstrln=0                                    ! initialize extracted string length
      ctoken=' '                                  ! initialize extracted string
      do i20 = ipoint+1,imax+2                    ! try to find a matching double quote to the right of the first
         ipoint=ipoint+1
         if(dummy(ipoint:ipoint).eq.'"')then         !!!!! caution : could look at dummy(imax+1:imax+1)
            if(dummy(ipoint+1:ipoint+1).ne.'"')then  ! this is the end of the string
               goto 30
            else                                     ! this is being used to try and represent an internal double-quote
               kstrln=kstrln+1                               ! determine length of string to remove
               ctoken(kstrln:kstrln)=dummy(ipoint:ipoint)    ! store the character into the current string storage
               ipoint=ipoint+1
            endif
         else                                             ! this is an internal character of the current string
            kstrln=kstrln+1                               ! determining length of string to remove
            ctoken(kstrln:kstrln)=dummy(ipoint:ipoint)    ! store the character into the current string storage
         endif
      enddo
      ier=-1                                         ! if you get here an unmatched string delimiter (") has been detected
      mssge='unmatched quotes in a string'
      return
30    continue
!!!!! check that current token string is not over (iclen_calc) characters long . what about the string "" or """" or """ ?
      ktoken=ktoken+1                             ! increment the counter of strings found
      write(toknam,'(''$_'',i3.3)')ktoken         ! build a unique name for the token string found for this input string
      nchars=nchars+1                             ! increment counter of characters stored
      string(nchars:nchars+4)=toknam              ! replace original delimited string with its token
      nchars=nchars+4
!                                                    store the token name and value in the string variable arrays
      call locate(keys_q,toknam,indx,ier)         ! determine storage placement of the variable and whether it is new
      if(ier.eq.-1)return
      iplace=iabs(indx)
      if(indx.le.0)then                           ! check if the token name needs added or is already defined
         call insert(keys_q,toknam, iplace)   ! adding the new variable name to the variable name array
         call insert(values,' '   , iplace)
         call insert(values_len,0     , iplace)
      endif
      call replace(values,ctoken(:kstrln),iplace)          ! store a defined variable's value
      call replace(values_len,kstrln,iplace)                   ! store length of string
!!!!! note that reserving variable names starting with $_ for storing character token strings
      cycle BIG
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case default                                   ! current is not one of the special characters in list
      end select
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                                                  ! for all but blank characters and strings
      back2=back1
      back1=currnt
      nchars=nchars+1
      string(nchars:nchars)=currnt
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   enddo BIG
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  end of string or hit beginning of comment

   if(ivar.ne.0)then                   ! check to see if a variable name was defined:
   nchar2=ivar-1                       ! variable was declared nchar2 is the position of the last character in the variable name
     if(nchar2.gt.20)then
      ier=-1
      mssge='new variable names must be 20 characters long or less'
     else if(nchar2.eq.0)then
      ier=-1
      mssge='input starts with =; cannot define a null variable name'
     else                              ! split up variable name and expression
                                       ! legal length variable name
       if(index('eE',string(nchar2:nchar2)).ne.0.and.nchar2.ne.1)then ! could be an unacceptable variable name
           if(index('0123456789',string(nchar2-1:nchar2-1)).ne.0)then
!            an unacceptable variable name if going to avoid conflict with
!            e-format numbers in a relatively straight-forward manner
             mssge='variable names ending in digit-e not allowed'
             ier=-1
           endif
        endif
        dummy=string
        varnam=dummy(1:ivar-1)
        if(nchars.ge.ivar+1)then
           string=dummy(ivar+1:nchars)
        else
           string=' '
        endif
        nchars=nchars-ivar
     endif
   endif
end subroutine squeeze_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!       [M_calculator] given_name_get_stringvalue_(3fp) - return associated value for variable name"
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine given_name_get_stringvalue_(chars,ierr)
!!
!!    character(len=*),intent(in)  :: chars
!!    integer,intent(out)          :: ierr
!!##DESCRIPTION
!!       return the actual string when given a string variable name or token
!!       the returned string is passed thru the message/string/error GLOBAL variable
!!##OPTIONS
!!       CHARS
!!       IER     ierr is set and returned as
!!
!!                 -1  an error occurs
!!                  2  a string is returned
!!##RETURNS
!!       MSSGE  when successful the variable value is returned through the global variable MSSGE
!!
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine given_name_get_stringvalue_(chars,ierr)

!character(len=*),parameter::ident_12="@(#)M_calculator::given_name_get_stringvalue_(3fp):return associated value for variable name"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars
integer,intent(out)          :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: index
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   index=0
   call locate(keys_q,chars,index,ierr)
   if(ierr.eq.-1) then
   elseif(index.le.0)then
      ierr=-1
!!!!  what if len(chars) is 0? look carefully at what happens with a possible null string
      mssge=' variable '//trim(chars)//' is undefined'
   else
      ierr=2
      mssge=values(index)
   endif
end subroutine given_name_get_stringvalue_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    stuff(3f) - [M_calculator] directly store value into calculator dictionary for efficiency
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine stuff(varnam,val)
!!
!!    class(*),intent(in)         :: varnam
!!    character(len=*),intent(in) :: val
!!
!!##DEFINITION
!!    breaking the rule of only accessing the calculator thru calculator(3f):
!!
!!    a direct deposit of a value into the calculator assumed to
!!    be used only by friendly calls, for efficiency and to avoid
!!    problems with recursion if a routine called by the calculator
!!    in substitute_subroutine(3f) wants to store something back into the calculator
!!    variable table
!!
!!    Normally values are stored or defined in the calculator module
!!    M_calculator(3fm) using the calculator(3f) routine or the convenience
!!    routines in the module M_calculator(3fm). For efficiency when
!!    large numbers of values require being stored the stuff(3f) procedure
!!    can be used to store numeric values by name in the calculator
!!    dictionary.
!!
!!    breaking the rule of only accessing the calculator thru calculator(3f):
!!
!!    stuff(3f) is assumed to only be used when needed for efficiency and to
!!    avoid problems with recursion if a routine called by the calculator
!!    in substitute_subroutine(3f) wants to store something back into the
!!    calculator variable table.
!!
!!##OPTIONS
!!    varnam  name of calculator variable to define or replace val
!!    numeric value to associate with the name VARNAME. May be
!!            integer, real, or doubleprecision.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_stuff
!!    use M_calculator, only : stuff, dnum0
!!    implicit none
!!    doubleprecision :: value
!!    call stuff('A',10.0)
!!    call stuff('PI',3.141592653589793238462643383279502884197169399375105820974944592307d0)
!!    value=dnum0('A*PI')
!!    write(*,*)value
!!    end program demo_stuff
!!
!!   Expected result:
!!
!!    31.415926535897931
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine stuff(varnam,value)

!character(len=*),parameter::ident_14="&
!&@(#)M_calculator::stuff(3fp): pass key value and integer|real|doubleprecision value to dictionary(3f) as doubleprecision"

character(len=*),intent(in)           :: varnam        ! variable name to add or replace value of
class(*),intent(in)                   :: value

real(kind=dp)                         :: val8          ! input value to store
character(len=:),allocatable          :: varnam_local  ! some trouble with variable length character strings on some machines
integer                               :: ierr
integer                               :: index
integer                               :: istart
!-----------------------------------------------------------------------------------------------------------------------------------
   varnam_local=adjustl(trim(varnam))//' '                      ! remove leading spaces but make sure at least one character long
   if(varnam_local(1:1).eq.'$')then                             ! add new variable to numeric value dictionary at specified location
      mssge='*stuff* numeric variable names must not start with a $'
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   call locate(keyr_q,varnam_local,index,ierr)
   istart=iabs(index)
   if(index.le.0)then   ! add entry to dictionary
      call insert(keyr_q,varnam_local,istart)
      call insert(values_d,0.0d0,istart)
   endif

   select type(value)
    type is (integer);         val8=dble(value)
    type is (real);            val8=dble(value)
    type is (doubleprecision); val8=value
   end select
   call replace(values_d,val8,istart)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine stuff
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     stuffa(3f) - [M_calculator] stuffa(3f): directly store a string into calculator variable name table
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine stuffa(varnam,string)
!!
!!    character(len=*),intent(in)          :: varnam
!!    character(len=*),intent(in)          :: string
!!
!!##DEFINITION
!!    Breaking the rule of only accessing the calculator thru calculator(3f):
!!
!!    a direct deposit of a value into the calculator assumed to be used
!!    only by friendly calls, for efficiency and to avoid problems with
!!    recursion if a routine called by the calculator in JUOWN1(3f) wants
!!    to store something back into the calculator
!!    variable table.
!!
!!##OPTIONS
!!    varnam    variable name to create or replace in calculator module
!!    string    string to associate with the calculator variable name varnam
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_stuffa
!!    use M_calculator, only : stuffa
!!    use M_calculator, only : snum0
!!    implicit none
!!       call stuffa('$A','')
!!       call stuffa('$mystring','this is the value of the string')
!!       write(*,*)snum0('$mystring')
!!       call stuffa('$mystring','this is the new value of the string')
!!       write(*,*)snum0('$mystring')
!!    end program demo_stuffa
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine stuffa(varnam,string)

!character(len=*),parameter::ident_15="@(#)M_calculator::stuffa(3f): directly store a string into calculator variable name table"

character(len=*),intent(in)           :: varnam    !  assuming friendly, not checking for null or too long varnam0
character(len=:),allocatable          :: varnam_local
character(len=*),intent(in)           :: string
integer                               :: indx
integer                               :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   varnam_local=adjustl(trim(varnam))
   ierr=0
!-----------------------------------------------------------------------------------------------------------------------------------
   call locate(keys_q,varnam_local,indx,ierr)
   if(indx.le.0)then                                        ! variable name not in dictionary
      indx=iabs(indx)
      call insert(keys_q,varnam_local,indx)                 ! adding the new variable name to the variable name array
      call insert(values,' '         ,indx)
      call insert(values_len,0       ,indx)
   endif
   ! found variable name in dictionary
   call replace(values,string,indx)
   call replace(values_len,len_trim(string),indx)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine stuffa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      inum0(3f) - [M_calculator] return integer value from calculator expression
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!   integer function inum0(inline,ierr)
!!
!!    character(len=*),intent(in)  :: inline
!!    integer,optional,intent(out) :: ierr
!!
!!##SYNOPSIS
!!
!!    INUM0() evaluates a CHARACTER argument as a FORTRAN-like
!!    calculator expression and returns an integer.
!!
!!     o INUM0() uses the calculator routine CALCULATOR(3f)
!!     o Remember that the calculator treats all values as DOUBLEPRECISION.
!!
!!    Values returned are assumed to be very close to being whole integer
!!    values. A small value (0.01) is added to the result before it is
!!    returned to reduce roundoff error problems. This could introduce
!!    errors if INUM0 is misused and is not being used to calculate
!!    essentially integer results.
!!##DESCRIPTION
!!
!!    inline  INLINE is a CHARACTER variable up to 255 characters long that is
!!            similar to a FORTRAN 77 numeric expression. Keep it less than 80
!!            characters when possible.
!!    ierr    zero (0) if no error occurs
!!
!!##DEPENDENCIES
!!         All programs that call the calculator routine can supply their
!!         own substitute_subroutine(3f) and substitute_C(3f) procedures. See
!!         the ../html/Example.html">example program for samples.
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_inum0
!!       use M_calculator, only : inum0
!!       i=inum0('20/3.4')
!!       j=inum0('CI = 13 * 3.1')
!!       k=inum0('CI')
!!       write(*,*)'Answers are ',I,J,K
!!       end program demo_inum0
!!
!!##SEE ALSO
!!       The syntax of an expression is as described in
!!       the main document of the Calculator Library.
!!   See
!!       CALCULATOR(),
!!       RNUM0(),
!!       DNUM0(),
!!       SNUM0(),
!!       EXPRESSION()
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!>
!! AUTHOR:  John S. Urban
!!##VERSION: 19971123
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
integer function inum0(inline,ierr)

!character(len=*),parameter::ident_16="@(#)M_calculator::inum0(3f):resolve a calculator string into a whole integer number"

!  The special string '*' returns -99999, otherwise return 0 on errors
character(len=*),intent(in)  :: inline
integer,optional,intent(out) :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
integer,parameter            :: IBIG=2147483647               ! overflow value (2**31-1)
integer                      :: iend
real,parameter               :: SMALL=0.0001                  ! and epsilon value
doubleprecision              :: dnum1
character(len=iclen_calc)    :: cdum20
integer                      :: ierr_local
integer                      :: ilen
!-----------------------------------------------------------------------------------------------------------------------------------
ierr_local=0
if(inline.eq.' ')then                                      ! return 0 for a blank string
   dnum1=0.0d0
elseif(inline.eq.'*')then                                  ! return -99999 on special string "*"
   dnum1=-99999d0
else                                                       ! parse string using calculator function
   iend=len(inline)
   call expression(inline(:iend),dnum1,cdum20,ierr_local,ilen)
   if(ierr_local.ne.0)then
      dnum1=0.0d0
   endif
endif
if(present(ierr))then
   ierr=ierr_local
endif
!-----------------------------------------------------------------------------------------------------------------------------------
! on most machines int() would catch the overflow, but this is safer
if(dnum1.gt.IBIG)then
   write(*,*)'*inum0* integer overflow 2**31-1 <',dnum1
   inum0=IBIG
elseif(dnum1.gt.0)then
   inum0=int(dnum1+SMALL)
else
   inum0=int(dnum1-SMALL)
endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function inum0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!       rnum0(3f) - [M_calculator] returns real number from string expression using CALCULATOR(3f)
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!    real function rnum0(inline)
!!
!!     character(len=*), intent=(in) :: inline
!!     integer,intent(out),optional  :: ierr
!!
!!##DESCRIPTION
!!     RNUM0() is used to return a REAL value from a CHARACTER string representing
!!     a numeric expression. It uses the M_calculator(3fp) module.
!!
!!     inline  INLINE is a CHARACTER variable up to (iclen_calc=512) characters long
!!             that is similar to a FORTRAN 77 numeric expression.
!!     ierr    error code. If zero, no error occurred
!!
!!##DEPENDENCIES
!!       o User-supplied routines:
!!         All programs that call the calculator routine can supply their
!!         own substitute_subroutine(3f) and substitute_C(3f) procedures. See
!!         the example program for samples.
!!##EXAMPLES
!!
!!   Sample program
!!
!!     program demo_rnum0
!!     use M_calculator, only : rnum0
!!     x=rnum0('20/3.4')
!!     y=rnum0('CI = 10 * sin(3.1416/4)')
!!     z=rnum0('CI')
!!     write(*,*)x,y,z
!!     end program demo_rnum0
!!
!!##SEE ALSO
!!
!!       o The syntax of an expression is as described in the main documentation
!!         of the Calculator Library.
!!       o See EXPRESSION(3f), CALCULATOR(3f), INUM0(3f), DNUM0(3f), SNUM0(3f).
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!>
!! AUTHOR    John S. Urban
!!##VERSION   1.0,19971123
!===================================================================================================================================
real function rnum0(inline,ierr)

!character(len=*),parameter::ident_17="@(#)M_calculator::rnum0(3f):resolve a calculator string into a real number"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: inline
integer,optional,intent(out) :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=iclen_calc)    :: cdum20
doubleprecision              :: d_answer
integer                      :: ierr_local
integer                      :: ilen
integer                      :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr_local=0
   if(inline.eq.' ')then
      d_answer=0.0d0
   elseif(inline.eq.'*')then                            !  the special string '*' returns -99999.0
      d_answer=-99999.0d0
   else
      iend=len(inline)
      call expression(inline(:iend),d_answer,cdum20,ierr_local,ilen)
      if(ierr_local.ne.0)then
         d_answer=0.0d0
      endif
   endif
   if(present(ierr))then
      ierr=ierr_local
   endif
   rnum0=real(d_answer)
!-----------------------------------------------------------------------------------------------------------------------------------
end function rnum0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      dnum0(3f) - [M_calculator] return double precision value from string expression using calculator(3f)
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!   doubleprecision function dnum0(inline,ierr)
!!
!!    character(len=*),intent(in) :: inline
!!    integer,optional,intent(out) :: ierr
!!
!!##DESCRIPTION
!!     DNUM0() is used to return a DOUBLEPRECISION value from a CHARACTER string
!!     representing a numeric expression.
!!       o If an error occurs in evaluating the expression INUM() returns zero(0).
!!       o DNUM0() ultimately uses the calculator routine CALCULATOR(3f)
!!
!!      inline  INLINE is a CHARACTER variable up to (iclen_calc=255) characters long
!!              that is similar to a FORTRAN 77 numeric expression.
!!      ierr    error code. If zero, no error occurred
!!
!!##EXAMPLES
!!
!!   Sample Program
!!
!!     program demo_dnum0
!!     use M_calculator, only : dnum0
!!     doubleprecision x,y,z
!!     X=DNUM0('20/3.4')
!!     Y=DNUM0('CI = 10 * sin(3.1416/4)')
!!     Z=DNUM0('CI')
!!     write(*,*)x,y,z
!!     end program demo_dnum0
!!
!!##SEE ALSO
!!
!!       o The syntax of an expression is as described in the main documentation of the Calculator Library.
!!       o See EXPRESSION(), CALCULATOR(), RNUM0(), SNUM0().
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!>
!! AUTHOR + John S. Urban
!!##VERSION 1.0, 19971123
!===================================================================================================================================
doubleprecision function dnum0(inline,ierr)

!character(len=*),parameter::ident_18="@(#)M_calculator::dnum0(3f):resolve a calculator string into a doubleprecision number"

character(len=*),intent(in) :: inline
integer,optional,intent(out) :: ierr
character(len=iclen_calc)           :: cdum20
doubleprecision             :: dnum1
integer                     :: iend
integer                     :: ierr_local
integer                     :: ilen
   ierr_local=0
   if(inline.eq.' ')then
      dnum1=0.0d0
   elseif(inline.eq.'*')then    !  the special string '*' returns -99999.0
      dnum1=-99999.0d0
   else
      iend=len(inline)
      call expression(inline(:iend),dnum1,cdum20,ierr_local,ilen)
      if(ierr_local.ne.0)then
         dnum1=0.0d0
      endif
   endif
   dnum0=dnum1
   if(present(ierr))then
      ierr=ierr_local
   endif
end function dnum0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     snum0(3f) - [M_calculator] resolve a calculator expression into a string(return blank on errors)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function snum0(inline0,ierr)
!!
!!    character(len=:),allocatable :: snum0(inline0)
!!    character(len=*),intent(in)  :: inline0                           ! input string
!!    integer,optional,intent(out) :: ierr
!!
!!##DESCRIPTION
!!     SNUM0() is used to return a string value up to (iclen_calc=512) characters
!!     long from a string expression.
!!     SNUM0() uses the calculator routine CALCULATOR(3f)
!!
!!     inline0  INLINE0 is a CHARACTER variable up to (iclen_calc=512) characters long that
!!              is similar to a FORTRAN 77 expression.
!!     ierr     error code. If zero, no error occurred
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_snum0
!!     use m_calculator, only: rnum0, snum0
!!     character(len=80)  :: ic,jc,kc
!!
!!     rdum=rnum0('A=83/2') ! set a variable in the calculator
!!     kc=snum0('$MYTITLE="This is my title variable"')
!!
!!     ic=snum0('$STR("VALUE IS [",A,"]")')
!!     jc=snum0('$MYTITLE')
!!
!!     write(*,*)'IC=',trim(ic)
!!     write(*,*)'JC=',trim(jc)
!!     write(*,*)'KC=',trim(kc)
!!
!!     end program demo_snum0
!!
!!    The output should look like
!!
!!      IC=VALUE IS [41.5]
!!      JC=This is my title variable
!!      KC=This is my title variable
!!
!!##DEPENDENCIES
!!       o User-supplied routines:
!!         All programs that call the calculator routine can supply their
!!         own substitute_subroutine(3f) and substitute_C(3f) procedures. See
!!         the example program for samples.
!!
!!##SEE ALSO
!!       o The syntax of an expression is described in the main document of the
!!         Calculator Library.
!!       o See CALCULATOR(), RNUM0(), SNUM0(), EXPRESSION().
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!>
!! AUTHOR    John S. Urban
!!##VERSION   1.0, 19971123
!===================================================================================================================================
!===================================================================================================================================
function snum0(inline0,ierr)

!character(len=*),parameter::ident_19="@(#)M_calculator::snum0(3f):resolve a calculator expression into a string"

!  a few odd things are done because some compilers did not work as expected
character(len=:),allocatable :: snum0
character(len=*),intent(in)  :: inline0                           ! input string
integer,optional,intent(out) :: ierr
character(len=iclen_calc)    :: lcopy                             ! working string
character(len=iclen_calc)    :: inline                            ! working copy of input string
integer                      :: ierr_local
integer                      :: iend                              ! size of input string
integer                      :: ilen
doubleprecision              :: dnum1

   inline=inline0                                                 ! some compilers need a copy of known length to work as expected
   ierr_local=0
   if(inline.eq.' ')then                                          ! what to do for a blank string
      snum0=' '                                                   ! return a blank string
   else                                                           ! non-blank input expression
      iend=len(inline)                                            ! size of working string
      lcopy=' '                                                   ! initialize trimmed string
      lcopy=adjustl(inline(:iend))                                ! trim leading spaces
      if(lcopy(1:1).eq.'$'.or.lcopy(1:1).eq.'"')then              ! this is a string that needs evaluated
         dnum1=0.0d0
         call expression(inline(:iend),dnum1,lcopy,ierr_local,ilen)
         if(ierr_local.ne.2)then                                  ! check if expression was evaluated to a string successfully
            snum0=' '                                             ! input string was not resolved to a string
         endif
         snum0=lcopy(:max(1,ilen))                                ! return whatever expression() returned
      else                                                        ! input was just a string, not an expression so just copy it
         snum0=inline(:iend)                                      ! copy input string to output
      endif
   endif
   if(present(ierr))then
      ierr=ierr_local
   endif
end function snum0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     expression(3f) - [M_calculator] return value from a string expression processing messages to simplify call to CALCULATOR(3f)
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine expression(inlin0,outval,outlin0,ierr,ilen)
!!
!!     character(len=*), intent=(in)  :: inlin0
!!     doubleprecision, intent=(out)  :: outval
!!     character(len=*), intent=(out) :: outlin0
!!     integer, intent=(out)          :: ierr
!!     integer, intent=(out)          :: ilen
!!
!!##DESCRIPTION
!!     expression() takes a string containing a FORTRAN-like expression and evaluates
!!     it and returns a numeric or string value as appropriate.
!!     The main purpose of expression() is to assume the burden of displaying the
!!     calculator messages for codes that make multiple calls to CALCULATOR(3f).
!!     CALCULATOR(3f) does not display error messages directly.
!!
!!       o EXPRESSION(3f) calls the calculator routine CALCULATOR(3f) to evaluate the
!!         expressions.
!!       o Messages beginning with a # are considered comments and are not passed
!!         on to the calculator.
!!
!!     inlin0  INLIN0 is a string containing a numeric expression. The expression can
!!             be up to (iclen_calc=512) characters long. The syntax of an expression
!!             is as described in the main document of the Calc library. For example:
!!
!!               'A=sin(3.1416/5)'
!!               '# this is a comment'
!!               '$STR("The value is ",40/3)'
!!
!!     outval  OUTVAL is a numeric value calculated from the expression in INLIN0
!!             (when IERR returns 0).
!!             When a string value is returned (IERR=2) then OUTVAL is the length of
!!             the output string.
!!     outlin0  OUTLIN0 contains a string representation of the number returned in
!!              OUTVAL up to 23 characters long when INLIN0 is a numeric expression. It
!!              contains a string up to (iclen_calc=512) characters long when INLIN0 is
!!              a string expression.
!!     ierr    IERR returns
!!
!!             o -1 if an error occurred
!!             o 0 if a numeric value is returned (value is in OUTVAL, string
!!               representation of the value is in OUTLIN2).
!!             o 1 if no value was returned but a message was displayed (If a 'dump'
!!               or 'funcs' command was passed to the calculator).
!!             o 2 if the expression evaluated to a string value instead of a
!!               numeric value (value is in OUTLIN0).
!!     ilen    ILEN returns the length of the input string minus trailing blanks.
!!
!!##DEPENDENCIES
!!       o User-supplied routines:
!!         All programs that call the calculator routine can supply their
!!         own substitute_subroutine(3f) and substitute_C(3f) procedures. See
!!         the example program for samples.
!!##EXAMPLES
!!
!!    Sample program:
!!
!!     program demo_expression
!!     use M_calculator,      only : iclen_calc
!!     use M_calculator, only : expression
!!     character(len=iclen_calc) ::  outlin0
!!     doubleprecision :: outval
!!     call expression('A=3.4**5    ',outval,outlin0,ierr,ilen)
!!     write(*,*)'value of expression is ',outval
!!     write(*,*)'string representation of value is ',trim(outlin0)
!!     write(*,*)'error flag value is ',ierr
!!     write(*,*)'length of expression is ',ilen
!!     end program demo_expression
!!
!!   Results:
!!
!!     value of expression is    454.35424000000000
!!     string representation of value is 454.35424
!!     error flag value is            0
!!     length of expression is            8
!!
!!##SEE ALSO
!!     See also: STRGAR(),RNUM0(),CALCULATOR(),INUM0(),SNUM0()
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!>
!! AUTHOR   John S. Urban
!!##VERSION  V1.0, 19971123
!===================================================================================================================================
recursive subroutine expression(inlin0,outval,outlin0,ierr,ilen)

!character(len=*),parameter::ident_20="@(#)M_calculator::expression(3f):call CALCULATOR(3f) calculator and display messages"

! evaluate a FORTRAN-like string expression and return a numeric
! value and its character equivalent or a string value as appropriate
character(len=*),intent(in) :: inlin0
doubleprecision             :: outval
character(len=*)            :: outlin0
integer,intent(out)         :: ierr
integer,intent(out)         :: ilen

character(len=iclen_calc)   :: line
character(len=iclen_calc)   :: outlin
doubleprecision,save        :: rvalue=0.0d0
intrinsic                   :: len
integer                     :: imaxi
character(len=iclen_calc)   :: event
!#----------------------------------------------------------------------------------------------------------------------------------
   ! copy INLIN0 to working copy LINE and find position of last non-blank character
   ! in the string
   line=''
   line=inlin0
   ! if the line is blank set imaxi to 1, else set it to the least of the length of the input string or (iclen_calc)
   ! NOTE: not checking if input expression is longer than (iclen_calc) characters!!
   imaxi=max(min(len(line),len(inlin0)),1)
   ilen=len_trim(line(1:imaxi))
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.eq.0)then                                            ! command was totally blank
      ierr=-1
      write(*,*)'*expression* warning===> blank expression'
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(line(:1).eq.'#')then                                  ! line was a comment
!-----------------------------------------------------------------------------------------------------------------------------------
   else
      ierr=0
      call calculator(line(:ilen),outlin,event,rvalue,ierr)         ! evaluate the expression
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(ierr)
      case(-1)                                    ! trapped error, display error message
        write(*,*)'*expression* error===>',trim(event)
        !call pdec(line(:ilen))                   ! echo input string as is and in ASCII decimal
      case(1)                                     ! general message, display message
        write(*,*)'*expression* message===>',trim(event)
      case(0)                                     ! numeric output
         outlin0=outlin
      case(2)                                     ! string output
         outlin0=event                            ! assumes outlin is long enough to return the string into
         ilen=int(rvalue)                         ! in special mode where a string is returned, rvalue is the length of the string
      case default
        write(*,*)'*expression* warning===> unexpected ierr value=',ierr
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   outval=rvalue                            ! return normal sized real value
end subroutine expression
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine juown1_placeholder(func,iflen,args,iargstp,n,fval,ctmp,ier)
      ! extend functions available to the calculator routine
!
!     if the function ownmode(1) is called this subroutine
!     will be accessed to do user-written functions.
!
!     func(iend-1)=procedure name.  func should not be changed.
!     iflen=length of procedure name.
!     args=array of 100 elements containing procedure arguments.
!     iargstp=type of argument(1=value,2=position of string value)
!     n=integer number of parameters
!     x=array of 55555 x values
!     y=array of 55555 y values
!     fval=value to replace function call
!     ctmp=string to return when returning a string value
!     ier=returned error flag value.
!         set to -1 if an error occurs.
!         set to  0 if a number is returned
!         set to  2 if a string is returned
!
!!use M_calculator, only : x, y, values, values_len
integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
character(len=*),intent(in)          :: func
integer,intent(in)                   :: iflen
real(kind=k_dbl),intent(in)          :: args(100)
integer,intent(in)                   :: iargstp(100)
integer,intent(in)                   :: n
real(kind=k_dbl)          :: fval
character(len=*)          :: ctmp
integer                   :: ier

integer                   :: i10
integer                   :: iwhich
integer                   :: ilen
!-----------------------------------------------------------------------
   fval=0.0d0
!-----------------------------------------------------------------------
   write(*,*)'*juown1_placeholder* unknown function ', func(1:iflen)
   write(*,*)'function name length is..',iflen
   write(*,*)'number of arguments .....',n
   do i10=1,n
      if(iargstp(i10).eq.0)then
         write(*,*)i10,' VALUE=',args(i10)
      elseif(iargstp(i10).eq.2)then
         iwhich=int(args(i10)+0.5d0)
         ilen=values_len(iwhich)
         write(*,*)i10,' STRING='//values(iwhich)(:ilen)
      else
         write(*,*)'unknown parameter type is ',iargstp(i10)
      endif
   enddo
end subroutine juown1_placeholder
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
real function c_placeholder(args,n)
! a built-in calculator function called c must be satisfied.
! write whatever you want here as a function
integer,intent(in)          :: n
real(kind=k_dbl),intent(in) :: args(n)
   c_placeholder=0.0_k_dbl
end function c_placeholder
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! copies
! use M_strings, only : upper, lower, value_to_string, delim
! use M_list,    only : locate, insert, replace
! use M_math,    only : round
!===================================================================================================================================
elemental pure function upper(str,begin,end) result (string)

!character(len=*),parameter::ident_21="@(#)M_strings::upper(3f): Changes a string to uppercase"

character(*), intent(In)      :: str                 ! inpout string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
   string = str                                      ! initialize output string to input string

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                ! located miniscule letter
          string(i:i) = char(iachar(str(i:i))-32)    ! change miniscule letter to uppercase
       end select
   end do

end function upper
!===================================================================================================================================
elemental pure function lower(str,begin,end) result (string)

!character(len=*),parameter::ident_22="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
   string = str

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
      case default
      end select
   end do

end function lower
!===================================================================================================================================
subroutine delim(line,array,n,icount,ibegin,iterm,ilen,dlim)

!character(len=*),parameter::ident_9="@(#)M_strings::delim(3f): parse a string and store tokens into an array"

!
!     given a line of structure " par1 par2 par3 ... parn "
!     store each par(n) into a separate variable in array.
!
!     IF ARRAY(1) == '#N#' do not store into string array  (KLUDGE))
!
!     also count number of elements of array initialized, and
!     return beginning and ending positions for each element.
!     also return position of last non-blank character (even if more
!     than n elements were found).
!
!     no quoting of delimiter is allowed
!     no checking for more than n parameters, if any more they are ignored
!
character(len=*),intent(in)    :: line
integer,intent(in)             :: n
character(len=*)               :: array(n)
integer,intent(out)            :: icount
integer,intent(out)            :: ibegin(n)
integer,intent(out)            :: iterm(n)
integer,intent(out)            :: ilen
character(len=*),intent(in)    :: dlim
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(line)):: line_local
logical             :: lstore
integer             :: i10
integer             :: iarray
integer             :: icol
integer             :: idlim
integer             :: iend
integer             :: ifound
integer             :: istart
!-----------------------------------------------------------------------------------------------------------------------------------
      icount=0
      ilen=len_trim(line)
      line_local=line

      idlim=len(dlim)
      if(idlim > 5)then
         idlim=len_trim(dlim)      ! dlim a lot of blanks on some machines if dlim is a big string
         if(idlim == 0)then
            idlim=1     ! blank string
         endif
      endif

      if(ilen == 0)then                                        ! command was totally blank
         return
      endif
!
!     there is at least one non-blank character in the command
!     ilen is the column position of the last non-blank character
!     find next non-delimiter
      icol=1

      if(array(1) == '#N#')then                                ! special flag to not store into character array
         lstore=.false.
      else
         lstore=.true.
      endif

      do iarray=1,n,1                                          ! store into each array element until done or too many words
         NOINCREMENT: do
            if(index(dlim(1:idlim),line_local(icol:icol)) == 0)then  ! if current character is not a delimiter
               istart=icol                                     ! start new token on the non-delimiter character
               ibegin(iarray)=icol
               iend=ilen-istart+1+1                            ! assume no delimiters so put past end of line
               do i10=1,idlim
                  ifound=index(line_local(istart:ilen),dlim(i10:i10))
                  if(ifound > 0)then
                     iend=min(iend,ifound)
                  endif
               enddo
               if(iend <= 0)then                               ! no remaining delimiters
                 iterm(iarray)=ilen
                 if(lstore)then
                    array(iarray)=line_local(istart:ilen)
                 endif
                 icount=iarray
                 return
               else
                 iend=iend+istart-2
                 iterm(iarray)=iend
                 if(lstore)then
                    array(iarray)=line_local(istart:iend)
                 endif
               endif
               icol=iend+2
               exit NOINCREMENT
            endif
            icol=icol+1
         enddo NOINCREMENT
!        last character in line was a delimiter, so no text left
!        (should not happen where blank=delimiter)
         if(icol > ilen)then
           icount=iarray
           if( (iterm(icount)-ibegin(icount)) < 0)then         ! last token was all delimiters
              icount=icount-1
           endif
           return
         endif
      enddo
      icount=n  ! more than n elements
end subroutine delim
!===================================================================================================================================
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

!character(len=*),parameter::ident_40="@(#)M_strings::value_to_string(3fp): subroutine returns a string from a value"

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,LOGICAL)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         write(*,*)'*value_to_string* UNKNOWN TYPE'
         chars=' '
      end select
      if(fmt.eq.'') then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   else                                                  ! no explicit format option present
      err_local=-1
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (real)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (logical)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      class default
         chars=''
      end select
      chars=adjustl(chars)
      if(index(chars,'.').ne.0) call trimzeros(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local.ne.0)then
      !! cannot currently do I/O from a function being called from I/O
      !!write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine locate_c(list,value,place,ier,errmsg)

!character(len=*),parameter::ident_5="&
!&@(#)M_list::locate_c(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
character(len=:),allocatable            :: list(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
integer                                 :: i
character(len=:),allocatable            :: message
integer                                 :: arraysize
integer                                 :: maxtry
integer                                 :: imin, imax
integer                                 :: error
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   arraysize=size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=nint(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',trim(value)//' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
end subroutine locate_c
!===================================================================================================================================
subroutine locate_d(list,value,place,ier,errmsg)

!character(len=*),parameter::ident_6="&
!&@(#)M_list::locate_d(3f): find PLACE in sorted doubleprecision array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

doubleprecision,allocatable            :: list(:)
doubleprecision,intent(in)             :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   arraysize=size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=nint(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
end subroutine locate_d
!===================================================================================================================================
subroutine locate_i(list,value,place,ier,errmsg)

!character(len=*),parameter::ident_8="&
!&@(#)M_list::locate_i(3f): find PLACE in sorted integer array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

integer,allocatable                    :: list(:)
integer,intent(in)                     :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   arraysize=size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=nint(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
end subroutine locate_i
!===================================================================================================================================
subroutine remove_i(list,place)

!character(len=*),parameter::ident_13="@(#)M_list::remove_i(3fp): remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_i
!===================================================================================================================================
subroutine remove_c(list,place)

!character(len=*),parameter::ident_9="@(#)M_list::remove_c(3fp): remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
end subroutine remove_c
subroutine remove_d(list,place)

!character(len=*),parameter::ident_10="&
!&@(#)M_list::remove_d(3fp): remove doubleprecision value from allocatable array at specified position"

doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_d
!===================================================================================================================================
subroutine replace_c(list,value,place)

!character(len=*),parameter::ident_14="@(#)M_list::replace_c(3fp): replace string in allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   tlen=len_trim(value)
   end=size(list)
   if(place.lt.0.or.place.gt.end)then
           write(stderr,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value).le.len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
end subroutine replace_c
!===================================================================================================================================
subroutine replace_d(list,value,place)

!character(len=*),parameter::ident_15="&
!&@(#)M_list::replace_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)   :: value
doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_d* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_d
!===================================================================================================================================
subroutine replace_i(list,value,place)

!character(len=*),parameter::ident_18="@(#)M_list::replace_i(3fp): place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_i
!===================================================================================================================================
subroutine insert_c(list,value,place)

!character(len=*),parameter::ident_19="@(#)M_list::insert_c(3fp): place string into allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end

   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif

   ii=max(len_trim(value),len(list),2)
   end=size(list)

   if(end.eq.0)then                                          ! empty array
      list=[character(len=ii) :: value ]
   elseif(place.eq.1)then                                    ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place.gt.end)then                                  ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(stderr,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_c
!===================================================================================================================================
subroutine insert_d(list,value,place)

!character(len=*),parameter::ident_21="&
!&@(#)M_list::insert_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)       :: value
doubleprecision,allocatable      :: list(:)
integer,intent(in)               :: place
integer                          :: end
   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_d* error: index out of range. end=',end,' index=',place,' value=',value
   endif
end subroutine insert_d
!===================================================================================================================================
subroutine insert_i(list,value,place)

!character(len=*),parameter::ident_23="@(#)M_list::insert_i(3fp): place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_i
!===================================================================================================================================
subroutine dict_delete(self,key)

!character(len=*),parameter::ident_24="@(#)M_list::dict_delete(3f): remove string from sorted allocatable string array if present"

class(dictionary),intent(inout) :: self
character(len=*),intent(in)     :: key
integer                         :: place

   call locate(self%key,key,place)
   if(place.ge.1)then
      call remove(self%key,place)
      call remove(self%value,place)
      call remove(self%count,place)
   endif

end subroutine dict_delete
!===================================================================================================================================
function dict_get(self,key) result(value)

!character(len=*),parameter::ident_25="@(#)M_list::dict_get(3f): get value of key-value pair in dictionary, given key"

!!class(dictionary),intent(inout) :: self
class(dictionary)               :: self
character(len=*),intent(in)     :: key
character(len=:),allocatable    :: value
integer                         :: place
   call locate(self%key,key,place)
   if(place.lt.1)then
      value=''
   else
      value=self%value(place)(:self%count(place))
   endif
end function dict_get
!===================================================================================================================================
subroutine dict_add(self,key,value)

!character(len=*),parameter::ident_26="@(#)M_list::dict_add(3f): place key-value pair into dictionary, adding the key if required"

class(dictionary),intent(inout) :: self
character(len=*),intent(in)     :: key
character(len=*),intent(in)     :: value
integer                         :: place
integer                         :: place2
   call locate(self%key,key,place)
   if(place.lt.1)then
      place2=iabs(place)
      call insert( self%key,   key,             place2 )
      call insert( self%value, value,           place2 )
      call insert( self%count, len_trim(value), place2 )
   elseif(place.gt.0)then  ! replace instead of insert
      call insert( self%value, value,           place )
      call insert( self%count, len_trim(value), place )
   endif
end subroutine dict_add
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function round(val,idigits0)
implicit none
character(len=*),parameter :: ident="@(#) M_math::round(3f): round val to specified number of significant digits"
integer,parameter          :: dp=kind(0.0d0)
real(kind=dp),intent(in)   :: val
integer,intent(in)         :: idigits0
   integer                 :: idigits,ipow
   real(kind=dp)           :: aval,rnormal
   real(kind=dp)           :: round
!  this does not work very well because of round-off errors.
!  Make a better one, probably have to use machine-dependent bit shifting
   ! make sure a reasonable number of digits has been requested
   idigits=max(1,idigits0)
   aval=abs(val)
!  select a power that will normalize the number
!  (put it in the range 1 > abs(val) <= 0)
   if(aval.ge.1)then
      ipow=int(log10(aval)+1)
   else
      ipow=int(log10(aval))
   endif
   rnormal=val/(10.0d0**ipow)
   if(rnormal.eq.1)then
      ipow=ipow+1
   endif
   !normalize, multiply by 10*idigits to an integer, and so on
   round=real(anint(val*10.d0**(idigits-ipow)))*10.d0**(ipow-idigits)
end function round
!===================================================================================================================================
subroutine trimzeros(string)

!character(len=*),parameter::ident_50="@(#)M_strings::trimzeros(3fp): Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: exp          ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      exp=str(ipos:)                         ! keep exponent string so it can be added back as a suffix
      str=str(1:ipos-1)                      ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if(index(str,'.').eq.0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i.le.1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   end do
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros
!===================================================================================================================================
subroutine init_random_seed(mine)

!character(len=*),parameter::ident_7="&
!&@(#)M_random::init_random_seed(3f): initialize random_number(3f) to return a single value with single integer seed like srand(3c)"

! to make this start with a single number like srand(3c) take the seed and
! use the value to fill the seed array, adding 37 to each subsequent value
! till the array is filled.
integer,intent(in) :: mine
   integer         :: i, n
   integer, dimension(:), allocatable :: seed
   call random_seed(size = n)
   allocate(seed(n))
   seed = mine + 37 * (/ (i - 1, i = 1, n) /)
  !write(*,*)seed
  !write(*,*)(/ (i - 1, i = 1, n) /)
   call random_seed(put = seed)
   deallocate(seed)
end subroutine init_random_seed
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_calculator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
 
 
!>>>>> build/dependencies/M_msg/src/M_msg.f90
module M_msg
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT,OUTPUT_UNIT    ! access computing environment
implicit none
private
!-----------------------------------------------------------------------------------------------------------------------------------
! USED SO FREQUENTLY IN OTHER MODULES PUT IN THIS ONE WITH NO DEPENDENCIES TO PREVENT CIRCULAR DEPENDENCY
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_1="@(#)M_msg::str(3f): {msg_scalar,msg_one}"

public str
public stderr
public wrt
public fmt
!!public :: a,i,f,g

interface str
   module procedure msg_scalar, msg_one
end interface str

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    str(3f) - [M_msg] converts any standard scalar type to a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Syntax:
!!
!!      function str(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,&
!!      & ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
!!      class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!      class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!      character(len=*),intent(in),optional :: sep
!!      character,len=(:),allocatable :: str
!!
!!##DESCRIPTION
!!    str(3f) builds a space-separated string from up to twenty scalar values.
!!
!!##OPTIONS
!!    g[0-9a-j]   optional value to print the value of after the message. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!
!!                Optionally, all the generic values can be
!!                single-dimensioned arrays. Currently, mixing scalar
!!                arguments and array arguments is not supported.
!!
!!    sep         separator string used between values. Defaults to a space.
!!
!!##RETURNS
!!    str     description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_msg
!!    use M_msg, only : str
!!    implicit none
!!    character(len=:),allocatable :: pr
!!    character(len=:),allocatable :: frmt
!!    integer                      :: biggest
!!
!!    pr=str('HUGE(3f) integers',huge(0),&
!!    &'and real',huge(0.0),'and double',huge(0.0d0))
!!    write(*,'(a)')pr
!!    pr=str('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    write(*,'(a)')pr
!!    pr=str('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    write(*,'(a)')pr
!!    pr=str('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!    write(*,'(a)')pr
!!
!!    ! create a format on the fly
!!    biggest=huge(0)
!!    frmt=str('(*(i',int(log10(real(biggest))),':,1x))',sep='')
!!    write(*,*)'format=',frmt
!!
!!    ! although it will often work, using str(3f)
!!    ! in an I/O statement is not recommended
!!    ! because if an error occurs str(3f) will try
!!    ! to write while part of an I/O statement
!!    ! which not all compilers can handle and is currently non-standard
!!    write(*,*)str('program will now stop')
!!
!!    end program demo_msg
!!
!!  Output
!!
!!    HUGE(3f) integers 2147483647 and real 3.40282347E+38 and double 1.7976931348623157E+308
!!    real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!    doubleprecision : 1.7976931348623157E+308 0.0000000000000000 12345.678900000001 2.2250738585072014E-308
!!    complex         : (3.40282347E+38,1.17549435E-38)
!!     format=(*(i9:,1x))
!!     program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function msg_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none

! ident_2="@(#)M_msg::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=:),allocatable  :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
   if(present(sep))then
      increment=len(sep)+1
      sep_local=sep
   else
      increment=2
      sep_local=' '
   endif

   istart=1
   line=''
   if(present(generic0))call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   msg_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function msg_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,&
               & generica,genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj,&
               & sep)
implicit none

! ident_3="@(#)M_msg::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
class(*),intent(in),optional  :: generica(:), genericb(:), genericc(:), genericd(:), generice(:)
class(*),intent(in),optional  :: genericf(:), genericg(:), generich(:), generici(:), genericj(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      increment=1+len(sep)
      sep_local=sep
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   msg_one=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      !type is (real(kind=real256));     write(error_unit,'(1pg0)',advance='no') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
      class default
         stop 'unknown type in *print_generic*'
   end select
   istart=len_trim(line)+increment+1
   line=trim(line)//']'//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmt(3f) - [M_msg] convert any intrinsic to a string using specified format
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function fmt(value,format) result(string)
!!
!!     class(*),intent(in),optional :: value
!!     character(len=*),intent(in),optional  :: format
!!     character(len=:),allocatable :: string
!!##DESCRIPTION
!!    FMT(3f) converts any standard intrinsic value to a string using the specified
!!    format.
!!##OPTIONS
!!    value    value to print the value of. May be of type INTEGER, LOGICAL,
!!             REAL, DOUBLEPRECISION, COMPLEX, or CHARACTER.
!!    format   format to use to print value. It is up to the user to use an
!!             appropriate format. The format does not require being
!!             surrounded by parenthesis. If not present a default is selected
!!             similar to what would be produced with free format.
!!##RETURNS
!!    string   A string value
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_fmt
!!     use :: M_msg, only : fmt
!!     implicit none
!!     character(len=:),allocatable :: output
!!
!!        output=fmt(10,"'[',i0,']'")
!!        write(*,*)'result is ',output
!!
!!        output=fmt(10.0/3.0,"'[',g0.5,']'")
!!        write(*,*)'result is ',output
!!
!!        output=fmt(.true.,"'The final answer is [',g0,']'")
!!        write(*,*)'result is ',output
!!
!!     end program demo_fmt
!!
!!   Results:
!!
!!     result is [10]
!!     result is [3.3333]
!!     result is The final answer is [T]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
recursive function fmt(generic,format) result (line)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128

! ident_4="@(#)M_msg::fmt(3f): convert any intrinsic to a string using specified format"

class(*),intent(in)          :: generic
character(len=*),intent(in),optional  :: format
character(len=:),allocatable :: line
character(len=:),allocatable :: fmt_local
integer                      :: ios
character(len=255)           :: msg
character(len=1),parameter   :: null=char(0)
integer                      :: ilen
   if(present(format))then
      fmt_local=format
   else
      fmt_local=''
   endif
   ! add ",a" and print null and use position of null to find length of output
   ! add cannot use SIZE= or POS= or ADVANCE='NO' on WRITE() on INTERNAL READ,
   ! and do not want to trim as trailing spaces can be significant
   if(fmt_local.eq.'')then
      select type(generic)
         type is (integer(kind=int8));     fmt_local='(i0,a)'
         type is (integer(kind=int16));    fmt_local='(i0,a)'
         type is (integer(kind=int32));    fmt_local='(i0,a)'
         type is (integer(kind=int64));    fmt_local='(i0,a)'
         type is (real(kind=real32));      fmt_local='(1pg0,a)'
         type is (real(kind=real64));      fmt_local='(1pg0,a)'
         type is (real(kind=real128));     fmt_local='(1pg0,a)'
         type is (logical);                fmt_local='(l1,a)'
         type is (character(len=*));       fmt_local='(a,a)'
         type is (complex);                fmt_local='("(",1pg0,",",1pg0,")",a)'
      end select
   else
      if(format(1:1).eq.'(')then
         fmt_local=format(:len_trim(format)-1)//',a)'
      else
         fmt_local='('//fmt_local//',a)'
      endif
   endif
   allocate(character(len=256) :: line) ! cannot currently write into allocatable variable
   ios=0
   select type(generic)
      type is (integer(kind=int8));     write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int16));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int32));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int64));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real32));      write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real64));      write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real128));     write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (logical);                write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (character(len=*));       write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (complex);                write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
   end select
   if(ios.ne.0)then
      line='<ERROR>'//trim(msg)
   else
      ilen=index(line,null,back=.true.)
      if(ilen.eq.0)ilen=len(line)
      line=line(:ilen-1)
   endif
end function fmt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    stderr(3f) - [M_msg] write message to stderr
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine stderr(msg,[generic])
!!
!!     class(*),intent(in),optional :: msg
!!     class(*),intent(in),optional :: generic0,generic1,generic2,generic3,generic4
!!     class(*),intent(in),optional :: generic5,generic6,generic7,generic8,generic9
!!##DESCRIPTION
!!    STDERR(3f) writes a message to standard error using a standard f2003 method.
!!    Up to ten generic options are available.
!!##OPTIONS
!!    msg           - description to print
!!    generic[0-9]  - optional value to print the value of after the message. May
!!                    be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!                    or CHARACTER.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_stderr
!!    use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
!!    use,intrinsic :: iso_fortran_env, only : real32, real64, real128
!!    use,intrinsic :: iso_fortran_env, only : real=> real32, integer=> int32
!!    use M_msg, only: stderr
!!    implicit none
!!
!!    call stderr('A simple message')
!!    call stderr('error: RVALUE=',3.0/4.0)
!!    call stderr('error: IVALUE=',123456789)
!!    call stderr('error: LVALUE=',.true.)
!!
!!    SEVERAL: block
!!    integer :: least=10, most=999, ival=-10
!!    call stderr('error: value',ival,'should be between',least,'and',most)
!!    endblock SEVERAL
!!
!!    call stderr('real32  :',huge(0.0_real32),0.0_real32,12345.6789_real32,tiny(0.0_real32))
!!    call stderr('real64  :',huge(0.0_real64),0.0_real64,12345.6789_real64,tiny(0.0_real64))
!!    call stderr('real128 :',huge(0.0_real128),0.0_real128,12345.6789_real128,tiny(0.0_real128))
!!    call stderr('complex :',cmplx(huge(0.0_real),tiny(0.0_real)))
!!
!!    call stderr('error: program will now stop')
!!    stop 1
!!
!!    end program demo_stderr
!!
!!   Results:
!!     A simple message
!!     error: RVALUE= 0.750000000
!!     error: IVALUE= 123456789
!!     error: LVALUE= T
!!     error: value -10 should be between 10 and 999
!!     real32  : 3.40282347E+38 ...
!!               0.00000000 ...
!!               12345.6787 ...
!!               1.17549435E-38
!!     real64  : 1.7976931348623157E+308 ...
!!               0.0000000000000000 ...
!!               12345.678900000001 ...
!!               2.2250738585072014E-308
!!     real128 : 1.18973149535723176508575932662800702E+4932 ...
!!               0.00000000000000000000000000000000000  ...
!!               12345.6789000000000000000000000000002 ...
!!               3.36210314311209350626267781732175260E-4932
!!     complex : (3.40282347E+38,1.17549435E-38)
!!     error: program will now stop
!!     STOP 1
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine stderr(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj)
implicit none

! ident_5="@(#)M_msg::stderr(3f): writes a message to standard error using a standard f2003 method"

class(*),intent(in),optional :: g0, g1, g2, g3, g4, g5, g6, g7, g8, g9
class(*),intent(in),optional :: ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
integer                      :: ios
   write(error_unit,'(a)',iostat=ios) str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj)
   flush(unit=output_unit,iostat=ios)
   flush(unit=error_unit,iostat=ios)
end subroutine stderr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    wrt(3f) - [M_msg] write multiple scalar values to any number of files
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine wrt(luns,generic(s),iostat)
!!
!!     integer,intent(in)           :: luns(:)
!!     class(*),intent(in),optional :: generic0,generic1,generic2,generic3,generic4
!!     class(*),intent(in),optional :: generic5,generic6,generic7,generic8,generic9
!!     class(*),intent(in),optional :: generica,genericb,genericc,genericd,generice
!!     class(*),intent(in),optional :: genericf,genericg,generich,generici,genericj
!!     integer,intent(out),optional :: ios
!!##DESCRIPTION
!!    WRT(3f) writes a list of scalar values  to the list of unit numbers in LUNS(:).
!!##OPTIONS
!!    LUNS            Unit numbers to write to. If of size zero no output is generated
!!    generic[1-20]   optional value to print the value of after the message. May
!!                    be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!                    or CHARACTER.
!!##RETURNS
!!    IOSTAT          The value of the last non-zero IOSTAT value. Returns zero if
!!                    no errors occurred.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_wrt
!!    use, intrinsic :: iso_fortran_env, only : &
!!     & stdin=>input_unit, &
!!     & stdout=>output_unit, &
!!     & stderr=>error_unit
!!    use M_msg, only: wrt, fmt
!!    implicit none
!!    integer,allocatable :: luns(:)
!!    integer :: iostat=0
!!    integer,parameter :: ints(3)=[1,2,3]
!!
!!    ! a null list allows for turning off verbose or debug mode output
!!    luns=[integer ::]
!!    call wrt(luns,'NULL LIST:',huge(0),'PI=',asin(1.0d0)*2.0d0,iostat=iostat)
!!    write(*,*)'IOSTAT=',iostat
!!
!!    ! multiple files can be used to create a log file, for example
!!    luns=[stderr,stdout]
!!    call wrt(luns,'TWO FILES:',huge(0),'PI=',asin(1.0d0)*2.0d0,iostat=iostat)
!!    write(*,*)'IOSTAT=',iostat
!!
!!    ! using fmt
!!    call wrt([stdout,stdout,stdout],'USING FMT :', &
!!     & huge(0),'PI=',asin(1.0d0)*2.0d0,fmt(ints(2),'i0.4'),iostat=iostat)
!!
!!
!!    end program demo_wrt
!!
!!##TWO FILES: 2147483647 PI= 3.1415926535897931
!!##TWO FILES: 2147483647 PI= 3.1415926535897931
!!  IOSTAT=           0
!!##USING FMT : 2147483647 PI= 3.1415926535897931 0002
!!##USING FMT : 2147483647 PI= 3.1415926535897931 0002
!!##USING FMT : 2147483647 PI= 3.1415926535897931 0002
!!  IOSTAT=           0
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine wrt(luns,g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj,iostat)
implicit none

! ident_6="@(#)M_msg::write(3f): writes a message to any number of open files with any scalar values"

integer,intent(in)           :: luns(:)
class(*),intent(in),optional :: g0, g1, g2, g3, g4, g5, g6, g7, g8, g9
class(*),intent(in),optional :: ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
integer,intent(out),optional :: iostat
integer                      :: i
character(len=256)           :: msg
   do i=1,size(luns)
      write(luns(i),'(a)',iostat=iostat,iomsg=msg)str(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj)
      if(iostat.ne.0)then
         call stderr('<ERROR>*write*:',msg)
      endif
   enddo
end subroutine wrt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
 
 
 
!>>>>> build/dependencies/M_msg/src/M_journal.f90
!>
!!##NAME
!!     M_journal(3fm) - [M_journal] write program messages to stdout and/or
!!     a log file
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!     use, M_journal , only : journal
!!##DESCRIPTION
!!
!!    For interactive programs in particular it is useful if all messages
!!    go thru the JOURNAL(3f) routine. This makes it easy to write messages
!!    to a log file as well as standard output; to toggle time prefixes
!!    on and off; to turn on and off debug-mode messages; control output
!!    paging and create replayable input journals.
!!
!!    The primary use of JOURNAL(3f) is to create journal files for
!!    interactive programs that can be replayed and/or be used to verify
!!    program executions. Typically, you would echo what the user typed to
!!    the trail file as-is, and write output you write to stdout as comments
!!    to the trail file so that the trail file can easily be read back in
!!    (by ignoring comments). So usually things that are read from user
!!    input are using output with WHERE='T' and output that usually goes
!!    to stdout is written with WHERE='SC' in the JOURNAL(3f) call.
!!
!!     >      :
!!     >      :
!!     > character(len=256) userline, output
!!     > call journal('O','my_trail_file')  ! open trail file
!!     >      :
!!     >      :
!!     > do
!!     >    read(*,'(a)',iostat=ios) userline  ! read user input
!!     >    if(ios.ne.0)exit
!!     >    ! echo user input to trail file
!!     >    call journal('T',userline)
!!     >    ! assume user input causes values i1, i2, and i3 to be calculated
!!     >    write(output,'(i0,1x,i0,1x)')i1,i2,i3 ! build an output line
!!     >    ! write output to stdout and as comment to trail file
!!     >    call journal(output)
!!     >    !or you can specify the WHERE parameter and up to ten scalar values
!!     >    call journal('SC','i1=',i1,'i2=',i2,'i3=',i3)
!!     > enddo
!!
!!    In this example an output line was built with an internal write; but calls
!!    to journal(3f) with numeric values with and without advancing I/O turned on
!!    are often used for simpler output:
!!
!!       I=10
!!       R=20.3
!!       ! write to stdout and trail file without advancing I/O
!!       call journal('+SC','I=',i)
!!       call journal('SC','AND R=',r)
!!
!!    writes to the trail file are ignored unless a trail file was opened with
!!
!!       CALL JOURNAL('O',filename)
!!
!!
!!    So that routines that do their output via JOURNAL(3f) can be used with and
!!    without programs generating trail files. That is, destinations 'T' and 'C'
!!    are ignored unless a trail file has been requested.
!!
!!    With no parameters, the trail file is flushed.
!!
!!##EXAMPLES
!!
!!
!!    The man(1) page for journal(3f) describes all the options for the WHERE field.
!!    In addition to being used to generate a journal, the routine can be used for
!!    producing optional debug messages and timing information.
!!
!!    Sample program for debug messages:
!!
!!      program demo_journal
!!      !! showing creating debug messages
!!      use M_journal, only : journal
!!      implicit none
!!      !! produces no output because trail is not on
!!      call journal('D','*demo* DEBUG MESSAGE 001 IGNORED')
!!      !! turn on debug messages
!!      call journal('>','debug on')
!!      !! produces output on stdout because debug mode
!!      !! is on but no named trail file
!!      call journal('D','*demo* DEBUG MESSAGE 002 ON STDOUT')
!!      !! open trail file
!!      call journal('O','mytrail.txt')
!!      !! debug messages now go to the trail file
!!      call journal('D','*demo* DEBUG MESSAGE 003 TO TRAIL')
!!      !! close trail file so messages go to stdout again
!!      call journal('O','')
!!      !! debug on stdout now
!!      call journal('D','*demo* DEBUG MESSAGE 004 TO STDOUT')
!!      call journal('<','debug off')
!!      !! back to no output from the next message
!!      call journal('D','*demo* DEBUG MESSAGE 005 IGNORED')
!!      end program demo_journal
!!
!!   Sample program for trail messages with optional timing information:
!!
!!      program testit
!!      use M_journal,only : journal
!!      implicit none
!!      call journal('a single string A -should be on S')
!!
!!      ! add time prefix to output
!!      call journal('%','%Y-%M-%DT%h:%m:%s.%x%u:%b')
!!      call journal('a single string B -should be on S with prefix')
!!      call journal('%','CPU_TIME: %c:CALLS: %C: %b')  ! change time prefix
!!      call journal('a single string B-1 -should be on S with prefix')
!!      call journal('a single string B-2 -should be on S with prefix')
!!      call journal('a single string B-3 -should be on S with prefix')
!!      !  Other useful time formats:
!!      !     %E -- Unix Epoch time
!!      !     %e -- integer value of Unix Epoch time
!!      !     %C -- number of times this format is used
!!      !     %c -- CPU_time(3f) output
!!      !     %S -- seconds since last use of this format
!!      !     %k -- CPU time in seconds from system_clock
!!      call journal('%','') ! turn off time prefix
!!      !
!!      call journal('a single string C -should be on S')
!!      !
!!      call journal('O','aaa.out') ! turn on trail file
!!      call journal('a single string D -should be on SC')
!!      call journal('a single string E -should be on SC')
!!      call journal('a single string F -should be on SC')
!!      call journal('O','') ! turn off trail file
!!      !
!!      call journal('a single string G -should be on S')
!!      call journal('a single string H -should be on S')
!!      call journal('a single string I -should be on S')
!!
!!      ! build one line of output with intrinsic scalar values added
!!      call journal('+sc','APPEND:')
!!      call journal('+sc',' integer',         1234)
!!      call journal('+sc',' and real',        1234.5678)
!!      call journal('+sc',' and double',1234567890.123456d0)
!!      call journal('+sc',' and logical',    .true.)
!!      call journal('sc','')
!!      !
!!      end program testit
!!
!!##AUTHOR
!!     John S. Urban
!!##LICENSE
!!     Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_journal
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT     ! access computing environment
use :: M_msg,                      only : str
implicit none
private

!>
!!##NAME
!!      journal(3f) - [M_journal] provides public message routine, no paging or graphic mode change
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!    subroutine journal([where,],[VALUE(s)])
!!
!!     character(len=*),intent(in) :: where
!!     character(len=*)|real|integer|doubleprecision|complex,optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!
!!   WRITE MESSAGES
!!    basic messages
!!
!!       call journal(where,[VALUE(S)])
!!       call journal(message) # a shortcut for "call journal('sc',message)":
!!   OPEN OR CLOSE TRAIL FILE
!!    trail file
!!
!!       call journal('O',trailfile_name) # open trail file
!!       call journal('O','')             # close trail file
!!   SET OUTPUT TIME PREFIX
!!    set the function display format for timestamps. See the NOW(3f)
!!    procedure for allowable timestamp macros
!!
!!       call journal('%',time_stamp_prefix_specification)
!!
!!   MODES
!!
!!    Turn on/off writing DEBUG messages to trail file
!!
!!       call journal('>','debug on') # turn on debug mode
!!       call journal('<','debug off') # turn off debug mode
!!
!!   ASSIGN STDOUT TO AN ALTERNATE FILE
!!    change stdout to iunit and open filename; or close unit and go back to stdout if filename=''
!!
!!       call journal(iunit,filename)
!!
!!    change stdout to iunit to use a file already open
!!
!!       call journal(iunit)
!!
!!##DESCRIPTION
!!
!!    If a user procedure is used for outputting messages instead of calling
!!    WRITE(3f) it is easy to provide control of when messages are printed
!!    (ie. a "verbose" mode, or "quite" mode), creating files to replay
!!    program execution, duplicating output, ...
!!
!!##OPTIONS
!!   WHERE  indicates where messages are written. A combination of the
!!          following characters can be used...
!!
!!      Usually one of these to write to the standard output files ...
!!
!!      S   write to stdout or iounit set with journal(unit) or
!!          journal(unit,filename).
!!      E   write to stderr
!!
!!      And one of these to write to trail file (ignore if no trail file
!!      defined) ...
!!
!!      C   write to trail file as a comment (if file is open)
!!          Writing output "as a comment" means it is preceded by a pound(#)
!!          character.
!!      T   write to trail file (if file is open)
!!
!!      Usually used by itself
!!
!!      D   write to trail file as a comment with "DEBUG:" prefix in front
!!          of message (if file is open) if debug mode is on. Write to stdout
!!          if no trail file and debug mode is on.
!!
!!      Modifier for S|E|C|T|D specifiers
!!
!!      +   subsequent files are written to with advance='no'. Position is
!!          important. '+sc' does an advance='no' on both files, 's+c'
!!          only does the advance='no' for the trail file.
!!
!!      Mode changing options used by themselves:
!!
!!      >   turn off debug messages
!!      <   turn on debug messages
!!      O   open trail file using value of "message" parameter or close
!!          trail file if no filename or a blank filename.
!!      A   Auxiliary programs that also want to write to the current log file
!!          (a2b, z2a, ...) call this routine to see if there is a trail file
!!          being generated and then add to it so that a program like ush(1f)
!!          can call the auxiliary programs and still just generate one log file,
!!          but if the auxiliary program is used as a stand-alone program no trail
!!          is generated.
!!
!!   VALUES(S)   message to write to stdout, stderr, and the trail file.
!!               a numeric or character value to optionally be appended
!!               to the message. Up to nine values are allowed. The WHERE
!!               field is required if values are added.
!!   FILENAME    when WHERE="O" to turn the trail file on or off, the "message"
!!               field becomes the trail filename to open. If blank, writing
!!               to the trail file is turned off.
!!   TFORMAT     when WHERE="%" the message is treated as a time format
!!               specification as described under now(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_journal
!!    use M_journal, only : journal
!!    !! BASIC USAGE
!!    call journal('write to standard output as-is, and trail file as a comment if open')
!!    ! since we have not opened a trail file yet, only stdout will display output
!!    call journal('c','ignored, as trail file is not open')
!!    ! now open trail file "trail"
!!    call journal('o','trail')
!!    call journal('sc','same thing except now trail file is open')
!!    ! only write to trail file if open
!!    call journal('c','not ignored, as trail file is open. Written with # suffix')
!!    call journal('t','not ignored, as trail file is open. Written as-is')
!!    ! turn off trail file
!!    call journal('o','')
!!    end program demo_journal
!!
!!   Adding intrinsic scalar values to the message:
!!
!!    program test_journal
!!    use M_journal, only: journal
!!    implicit none
!!       call journal('S','This is a test with no optional value')
!!       call journal('S','This is a test with a logical value',.true.)
!!       call journal('S','This is a test with a double value',1234567890.123456789d0)
!!       call journal('S','This is a test with a real value',1234567890.123456789)
!!       call journal('S','This is a test with an integer value',1234567890)
!!       call journal('STDC','This is a test using STDC',1234567890)
!!       call journal('stdc','This is a test using stdc',1234567890)
!!       call journal('o','journal.txt')                        ! open trail file
!!       call journal('S',1,12.34,56789.111111111d0,.false.,'a bunch of values')
!!       ! the combinations that make sense
!!       call journal('st','stdout and trail')
!!       call journal('s' ,'stdout only')
!!       call journal('t' ,'trail only')
!!       call journal('sc','stdout and trail_comment')
!!       call journal('c' ,'trail_comment only ')
!!       call journal('d' ,'debug only')
!!       call journal('e' ,'stderr only')
!!       call journal('o' ,' ') ! closing trail file
!!    end program test_journal
!!
!!    program testit
!!    ! this is a utility program that calls the module routines. It is typically built using ccall(1).
!!    use M_journal, only : journal
!!       character(len=:),allocatable :: time_stamp_prefix
!!       call journal('s','--------------------------------------------------------------------------------')
!!       call journal('s','SIMPLE WRITES')
!!       call one()
!!       call two()
!!       call journal('sc','called ONE() and TWO() but did not generate a log file')
!!       call journal('s','--------------------------------------------------------------------------------')
!!       call journal('s','SIMPLE WRITES WITH LOG FILE')
!!       call journal('o','journal.txt')                        ! open trail file
!!       call one()
!!       call two()
!!       call journal('sc','called ONE() and TWO() and generated log file journal.txt')
!!       call journal('','journal.txt')                         ! close trail file
!!       call journal('s','--------------------------------------------------------------------------------')
!!       call journal('s','SIMPLE WRITES WITH TIMING INFORMATION')
!!       time_stamp_prefix='CPU_TIME=%c:CALLS=%C:SINCE=%S:%b'  ! change time prefix
!!       call journal('%',time_stamp_prefix) ! set a time prefix in front of messages
!!       call journal('o','timed.txt')                          ! open trail file
!!       call one()
!!       call two()
!!       call journal('sc','called ONE() and TWO() and generate log file timed.txt')
!!       call journal('','timed.txt')                           ! close trail file
!!       call journal('%','')                                   ! turn off time prefix
!!       call journal('o','timed.txt')                          ! open trail file
!!       call journal('s','--------------------------------------------------------------------------------')
!!
!!    contains
!!
!!       subroutine two()
!!          call journal('Entered subroutine two')
!!          call journal('Exited subroutine two')
!!       end subroutine two
!!
!!       subroutine one()
!!          call journal('Entered subroutine one')
!!          sum=-HUGE(1.0)
!!          do i=1,10000000
!!            sum=sum+sqrt(real(i))
!!          enddo
!!          write(*,*)'SUM=',sum
!!          call journal('Exited subroutine one')
!!       end subroutine one
!!
!!    end program testit
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
public journal

interface journal
   module procedure flush_trail               ! journal()                ! no options
   module procedure write_message_only        ! journal(c)               ! must have one string
   module procedure where_write_message_all   ! journal(where,[g1-g9])   ! must have two strings
   module procedure set_stdout_lun            ! journal(i)               ! first is not a string
end interface journal

! ident_1="@(#)M_journal::journal(3fg): provides public message routine, no paging or graphic mode change"

! global variables

!integer,parameter,private  :: stdin=INPUT_UNIT
integer,save,private       :: my_stdout=OUTPUT_UNIT
logical,save               :: debug=.false.
integer,save               :: last_int=0

integer,parameter,private :: dp=kind(0.0d0)
real(kind=dp)             :: secday=86400.0d0              ! 24:00:00 hours as seconds

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine where_write_message(where,msg)

! ident_2="@(#)M_journal::where_write_message(3fp): basic message routine used for journal files"

character(len=*),intent(in)  :: where
character(len=*),intent(in)  :: msg
!
!  writes error messages and general information text to stdout and the trace file
!     where=*C* write to trail file as a comment (if file is open)
!     where=*D* write to trail file as a comment with DEBUG: prefix in front of message (if file is open and debug mode on)
!     where=*E* write to stderr
!     where=*S* write to stdout or iounit set with journal(unit) or journal(unit,filename)
!     where=*T* write to trail file (if file is open)
!     where=*+* subsequent writes for this call are written with advance='no'

!     where=> turn on debug messages (change mode), which are ones with WHERE='D'
!     where=< turn off debug messages  (change mode), which are ones with WHERE='D'

!     where=O open trail file "msg" or close trail file if blank filename is given
!     where=% set prefix to run thru now(3f) to generate time prefix strings, blank turns off time prefix
!     where=N open new file and assign stdout to the file unless file name is blank; then revert to my_stdout being original stdout.
!
!  the trail file messages are preceded by a pound character (#) by default so they can easily be interpreted as comments
!  if the trace file is subsequently used as input data for a program
!
logical,save                       :: trailopen=.false.
integer,save                       :: itrail
character,save                     :: comment='#'
integer                            :: i
integer                            :: ios
integer                            :: times             ! number of times written to stdout
character(len=3)                   :: adv               ! whether remaining writes from this call use advancing I/O

character(len=:),allocatable,save  :: prefix_template   ! string to run thru now_ex(3f) to make prefix
character(len=:),allocatable       :: prefix            ! the prefix string to add to output
logical,save                       :: prefix_it=.false. ! flag whether time prefix mode is on or not
character(len=4096)                :: mssge
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
   adv='yes'
!-----------------------------------------------------------------------------------------------------------------------------------
   if(prefix_it)then
      prefix=now_ex(prefix_template)
   else
      prefix=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   times=0
   do i=1,len_trim(where)
      select case(where(i:i))
      case('T','t')
         if(trailopen) then
            write(itrail,'(a)',advance=adv)prefix//trim(msg)
         !!elseif(times.eq.0)then
         !!   write(my_stdout,'(a)',advance=adv)prefix//trim(msg)
         !!   times=times+1
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('S','s')
         write(my_stdout,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      !-----------------------------------------------------------------------------------------------------------------------------
      case('E','e')
         write(stderr,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      !-----------------------------------------------------------------------------------------------------------------------------
      case('+'); adv='no'
      !-----------------------------------------------------------------------------------------------------------------------------
      case('>'); debug=.true.
      !-----------------------------------------------------------------------------------------------------------------------------
      case('<'); debug=.false.
      !-----------------------------------------------------------------------------------------------------------------------------
      case('%')                       ! setting timestamp prefix
         if(msg.eq.'')then            ! if message is blank turn off prefix
            prefix_it=.false.
         else                         ! store message as string to pass to now_ex() on subsequent calls to make prefix
            prefix_template=msg
            prefix_it=.true.
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('N')                                                   ! new name for my_stdout
         if(msg.ne.' '.and.msg.ne.'#N#'.and.msg.ne.'"#N#"')then   ! if filename not special or blank open new file
            close(unit=last_int,iostat=ios)
            open(unit=last_int,file=adjustl(trim(msg)),iostat=ios)
            if(ios.eq.0)then
               my_stdout=last_int
            else
               write(*,*)'*journal* error opening redirected output file, ioerr=',ios
               write(*,*)'*journal* msg='//trim(msg)
            endif
         elseif(msg.eq.' ')then
            close(unit=last_int,iostat=ios)
            my_stdout=6
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('C','c')
         if(trailopen)then
            write(itrail,'(3a)',advance=adv)prefix,comment,trim(msg)
         elseif(times.eq.0)then
            !! write(my_stdout,'(2a)',advance=adv)prefix,trim(msg)
            !! times=times+1
         endif
      case('D','d')
         if(debug)then
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'DEBUG: ',trim(msg)
            elseif(times.eq.0)then
               write(my_stdout,'(3a)',advance=adv)prefix,'DEBUG:',trim(msg)
               times=times+1
            endif
         endif
      case('F','f')
         flush(unit=itrail,iostat=ios,iomsg=mssge)
         if(ios.ne.0)then
            write(*,'(a)') trim(mssge)
         endif
      case('A','a')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential',file=adjustl(trim(msg)),&
            & form='formatted',iostat=ios,position='append')
            trailopen=.true.
         endif
      case('O','o')
         if(msg.ne.'')then
            open(newunit=itrail,status='unknown',access='sequential', file=adjustl(trim(msg)),form='formatted',iostat=ios)
            trailopen=.true.
         else
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'closing trail file:',trim(msg)
            endif
            close(unit=itrail,iostat=ios)
            trailopen=.false.
         endif
      case default
         write(my_stdout,'(a)',advance=adv)'*journal* bad WHERE value '//trim(where)//' when msg=['//trim(msg)//']'
      end select
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine where_write_message
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine flush_trail()

! ident_3="@(#)M_journal::flush_trail(3fp): flush trail file"

call where_write_message('F','IGNORE THIS STRING')
end subroutine flush_trail
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_stdout_lun(iounit)

! ident_4="@(#)M_journal::set_stdout_lun(3fp): change I/O logical unit value for standard writes"

integer,intent(in)                   :: iounit
   my_stdout=iounit
end subroutine set_stdout_lun
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    where_write_message_all(3f) - [M_journal] converts any standard scalar type to a string and calls journal(3f)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine where_write_message_all(where,g0,g1,g2g3,g4,g5,g6,g7,g8,g9,sep)
!!
!!     character(len=*),intent(in)   :: where
!!     class(*),intent(in)           :: g0
!!     class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     character,intent(in),optional :: sep
!!
!!##DESCRIPTION
!!    where_write_message_all(3f) builds and writes a space-separated string from up to nine scalar values.
!!
!!##OPTIONS
!!
!!    where    string designating where to write message, as with journal(3f)
!!    g0       value to print. May
!!             be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!             or CHARACTER.
!!    g[1-9]   optional additional values to print the value of after g0.
!!    sep      separator to add between values. Default is a space
!!##RETURNS
!!    where_write_message_all  description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_wm_all
!!    use M_journal, only : where_write_message_all
!!    implicit none
!!    end program program demo_wm_all
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine where_write_message_all(where, g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, sep)
implicit none

! ident_5="@(#)M_journal::where_write_message_all(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1, g2, g3, g4, g5, g6, g7, g8 ,g9
character,intent(in),optional :: sep
call where_write_message(where,str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9,sep))
end subroutine where_write_message_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_message_only(message)

! ident_6="@(#)M_journal::write_message_only(3fp): calls JOURNAL('sc',message)"

character(len=*),intent(in)          :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   call where_write_message('sc',trim(message))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine write_message_only
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine d2j(dat,julian,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
! * Author:    John S. Urban
! * Version:   1.0 2015-12-21
! * Reference: From Wikipedia, the free encyclopedia 2015-12-19
! * There is no year zero
! * Julian Day must be non-negative
! * Julian Day starts at noon; while Civil Calendar date starts at midnight
!-----------------------------------------------------------------------------------------------------------------------------------
! ident_7="@(#)d2j(3f): Converts proleptic Gregorian date array to Julian Day"
integer,intent(in)         :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
real(kind=dp),intent(out)  :: julian   ! Julian Day (non-negative, but may be non-integer)
integer,intent(out)        :: ierr     ! Error return, 0 for successful execution,-1=invalid year,-2=invalid month,-3=invalid day,
                                       ! -4=invalid date (29th Feb, non leap-year)
   integer                 :: year, month, day, utc, hour, minute
   real(kind=dp)           :: second
   integer                 :: A, Y, M, JDN
!-----------------------------------------------------------------------------------------------------------------------------------
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0d0    ! Second   ! correction for time zone and milliseconds
!-----------------------------------------------------------------------------------------------------------------------------------
   julian = -HUGE(99999)                  ! this is the date if an error occurs and IERR is < 0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(year==0 .or. year .lt. -4713) then
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or Febuary, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for Febuary
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + second/86400.0d0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(julian.lt.0.d0) then                  ! Julian Day must be non-negative
      ierr=1
   else
      ierr=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine d2j
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine j2d(dat,julian,ierr)
! ident_8="@(#)j2d(3f): Converts Julian Day to date array"
integer,intent(out)        :: dat(8)
integer                    :: timezone(8), tz
real(kind=dp),intent(in)   :: julian            ! Julian Day (non-negative)
integer,intent(out)        :: ierr              ! 0 for successful execution, otherwise 1
   real(kind=dp)           :: second
   integer                 :: year
   integer                 :: month
   integer                 :: day
   integer                 :: hour
   integer                 :: minute
   integer                 :: jalpha,ja,jb,jc,jd,je,ijul

   if(julian.lt.0.d0) then                      ! Negative Julian Day not allowed
      ierr=1
      return
   else
      ierr=0
   endif
   call date_and_time(values=timezone)
   tz=timezone(4)

   ijul=idint(julian)                           ! Integral Julian Day
   second=sngl((julian-dble(ijul))*secday)      ! Seconds from beginning of Jul. Day
   second=second+(tz*60)

   if(second.ge.(secday/2.0d0)) then            ! In next calendar day
      ijul=ijul+1
      second=second-(secday/2.0d0)              ! Adjust from noon to midnight
   else                                         ! In same calendar day
      second=second+(secday/2.0d0)              ! Adjust from noon to midnight
   endif

   if(second.ge.secday) then                    ! Final check to prevent time 24:00:00
      ijul=ijul+1
      second=second-secday
   endif

   minute=int(second/60.0)                      ! Integral minutes from beginning of day
   second=second-float(minute*60)               ! Seconds from beginning of minute
   hour=minute/60                               ! Integral hours from beginning of day
   minute=minute-hour*60                        ! Integral minutes from beginning of hour

   !---------------------------------------------
   jalpha=idint((dble(ijul-1867216)-0.25d0)/36524.25d0) ! Correction for Gregorian Calendar
   ja=ijul+1+jalpha-idint(0.25d0*dble(jalpha))
   !---------------------------------------------

   jb=ja+1524
   jc=idint(6680.d0+(dble(jb-2439870)-122.1d0)/365.25d0)
   jd=365*jc+idint(0.25d0*dble(jc))
   je=idint(dble(jb-jd)/30.6001d0)
   day=jb-jd-idint(30.6001d0*dble(je))
   month=je-1

   if(month.gt.12)then
      month=month-12
   endif

   year=jc-4715
   if(month.gt.2)then
      year=year-1
   endif

   if(year.le.0)then
      year=year-1
   endif

   dat(1)=year
   dat(2)=month
   dat(3)=day
   dat(4)=tz
   dat(5)=hour
   dat(6)=minute
   dat(7)=int(second)
   dat(8)=int((second-int(second))*1000.0)
   ierr=0

end subroutine j2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine d2u(dat,unixtime,ierr)
! ident_9="@(#)d2u(3f): Converts date array to Unix Time (UT starts at 0000 on 1 Jan. 1970)"
integer,intent(in)         :: dat(8)                  ! date time array similar to that returned by DATE_AND_TIME
real(kind=dp),intent(out)  :: unixtime                ! Unix time (seconds)
integer,intent(out)        :: ierr                    ! return 0 on successful, otherwise 1
   real(kind=dp)           :: julian
   real(kind=dp),save      :: julian_at_epoch
   logical,save            :: first=.true.
!-----------------------------------------------------------------------------------------------------------------------------------
if(first) then                                        ! Compute zero of Unix Time in Julian days and save
   call d2j([1970,1,1,0,0,0,0,0],julian_at_epoch,ierr)
   if(ierr.ne.0) return                               ! Error
   first=.false.
endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call d2j(dat,julian,ierr)
   if(ierr.ne.0) return                               ! Error
   unixtime=(julian-julian_at_epoch)*secday
end subroutine d2u
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine u2d(dat,unixtime,ierr)
! REF:JRH:1991-05-23
! REF:JSU:2015-12-12
!-----------------------------------------------------------------------------------------------------------------------------------
! ident_10="@(#)u2d(3f): Converts Unix Time to date array"
integer,intent(out)        :: dat(8)                           ! date and time array
real(kind=dp),intent(in)   :: unixtime                         ! Unix time (seconds)
integer,intent(out)        :: ierr                             ! 0 for successful execution, otherwise 1
real(kind=dp)              :: julian                           ! Unix time converted to a Julian date
real(kind=dp),save         :: Unix_Origin_as_Julian            ! start of Unix Time as Julian date
logical,save               :: first=.TRUE.
integer                    :: v(8)                             ! date and time array used to get time zone
!-----------------------------------------------------------------------------------------------------------------------------------
if(first)then                                                  ! Initialize calculated constants on first call
   call d2j([1970,1,1,0,0,0,0,0],Unix_Origin_as_Julian,ierr)   ! Compute start of Unix Time in Julian days
   if(ierr.ne.0) return                                        ! Error
   first=.FALSE.
endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call date_and_time(values=v)                                ! need to get time zone
   julian=(unixtime/secday)+Unix_Origin_as_Julian              ! convert seconds from Unix Epoch to Julian days
   call j2d(dat,julian,ierr)                                   ! calculate date array from Julian date
   dat(4)=v(4)
end subroutine u2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
FUNCTION d2o(dat) RESULT (ordinal)
! ident_11="@(#)d2o(3f): Converts date-time array to Ordinal day"
INTEGER,INTENT(IN)         :: dat(8)                  ! date time array similar to that returned by DATE_AND_TIME
INTEGER                    :: ordinal                 ! the returned number of days
   REAL(KIND=dp)           :: unixtime                ! Unix time (seconds)
   REAL(KIND=dp)           :: unix_first_day
   INTEGER                 :: ierr                    ! return 0 on successful, otherwise 1 from d2u(3f)
   CALL d2u(dat,unixtime,ierr)                        ! convert date to Unix Epoch Time
   IF(ierr.NE.0)THEN
      write(*,*)'*d2o* bad date array'
      ordinal=-1                                      ! initialize to bad value
   ELSE
      CALL d2u([dat(1),1,1,dat(4),0,0,0,0],unix_first_day,ierr)
      ordinal=int((unixtime-unix_first_day)/secday)+1
   ENDIF
END FUNCTION d2o
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
FUNCTION v2mo(imonth) RESULT(month_name)
! ident_12="@(#)v2mo(3f): returns the month name of a Common month"
CHARACTER(LEN=:),ALLOCATABLE :: month_name                                        ! string containing month name or abbreviation.
INTEGER,INTENT(IN)           :: imonth                                            ! the number of the month(1-12)
CHARACTER(LEN=*),PARAMETER   :: names(12)=[ character(len=9) ::  &
&'January  ', 'February ', 'March    ', 'April    ', 'May      ', 'June     ', &
&'July     ', 'August   ', 'September', 'October  ', 'November ', 'December ']
   SELECT CASE(imonth)
   CASE (1:12);  month_name=TRIM(names(imonth))
   CASE DEFAULT; month_name='UNKNOWN'
   END SELECT
END FUNCTION v2mo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
FUNCTION now(format)
! ident_13="@(#)now(3f): return string representing current time given format"
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: format
CHARACTER(LEN=:),ALLOCATABLE         :: now
   INTEGER                           :: values(8)
!-----------------------------------------------------------------------------------------------------------------------------------
   CALL DATE_AND_TIME(VALUES=values)
   IF(PRESENT(format))THEN
      IF(format.NE.' ')THEN
         now=fmtdate(values,format)
      ELSE
         now=fmtdate(values,'%Y-%M-%D %h:%m:%s %z')
      ENDIF
   ELSE
      NOW=fmtdate(values,'%Y-%M-%D %h:%m:%s %z Julian date is %J Epoch time is %E ')
   ENDIF
END FUNCTION now
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
FUNCTION fmtdate(values,format) RESULT (timestring)
! Read the FORMAT string and replace the "%" strings per the following rules:
!-----------------------------------------------------------------------------------------------------------------------------------
! ident_14="@(#)fmtdate(3f): given date array return date as string using format"
CHARACTER(LEN=*),INTENT(IN)     :: format    ! input format string
INTEGER,DIMENSION(8),INTENT(IN) :: values    ! numeric time values as DATE_AND_TIME(3f) intrinsic returns
CHARACTER(LEN=:),ALLOCATABLE    :: timestring
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   INTEGER              :: i10
   LOGICAL              :: keyword   ! flag that previous character was a % character
   CHARACTER(LEN=9)     :: day       ! day of week
   CHARACTER(LEN=1)     :: chara     ! character being looked at in format string
   CHARACTER(LEN=4096)  :: text      ! character array
   INTEGER              :: iout
   INTEGER              :: weekday
   INTEGER              :: ierr
   INTEGER,SAVE         :: called=0
   LOGICAL,SAVE         :: since=.FALSE.
   REAL(KIND=dp)        :: julian
   REAL(KIND=dp)        :: cputime
   INTEGER              :: ii
   REAL(KIND=dp)        :: unixtime
   REAL(KIND=dp),save   :: unixtime_last
   INTEGER              :: systemclock, countrate
   INTEGER              :: iso_year, iso_week, iso_weekday
   CHARACTER(LEN=10)    :: iso_name
   CHARACTER(LEN=2)     :: dayend

   text=' '
!  write string, when encounter a percent character do a substitution
   keyword=.FALSE.
   iout=1
   DO i10=1,LEN(format)
      chara=format(i10:i10)
      IF(chara.eq.'%'.and..not.keyword)THEN
            keyword=.TRUE.
            CYCLE
      ENDIF
      IF(keyword)THEN
         keyword=.FALSE.
         SELECT CASE(chara)
         !=====================================================================================
         CASE('%'); WRITE(text(iout:),'(A1)')chara                        ! literal percent character
         !=====================================================================================
         CASE('b'); WRITE(text(iout:),'(A1)')' '                          ! space character
         !=====================================================================================
         CASE('c'); CALL cpu_time(cputime)                                ! CPU_TIME()
                    WRITE(text(iout:),'(G0)')cputime
         !=====================================================================================
         CASE('C'); called = called + 1                                   ! number of times this routine called
                    WRITE(text(iout:),'(I0)')called
         !=====================================================================================
         CASE('d');                                                       ! the day of the month 1st..31st
                    dayend='  '
                    select case(values(3))
                    case(1,21,31); dayend='st'
                    case(2,22); dayend='nd'
                    case(3,23); dayend='rd'
                    case(4:20,24:30); dayend='th'
                    case default
                    end select
                    WRITE(text(iout:),'(I2,a)')values(3),dayend
         !=====================================================================================
         CASE('D'); WRITE(text(iout:),'(I2.2)')values(3)                  ! the day of the month 1..31
         !=====================================================================================
         CASE('e'); CALL d2u(values,unixtime,ierr)                        ! integer Unix Epoch time in seconds
                    WRITE(text(iout:),'(G0)')int(unixtime)
         !=====================================================================================
         CASE('E'); CALL d2u(values,unixtime,ierr)                        ! Unix Epoch time in seconds
                    WRITE(text(iout:),'(G0)')unixtime
         !=====================================================================================
         CASE('h'); WRITE(text(iout:),'(I2.2)')values(5)                  ! the hour of the day, in the range of 0 to 23
         !=====================================================================================
         CASE('H'); ii=mod(values(5),12)                                  ! hour of day in range 1..12
                    if(ii.eq.0)then
                       ii=12
                    endif
                    WRITE(text(iout:),'(I2.2)')ii
         !=====================================================================================
         CASE('i'); CALL woy(values,iso_year,iso_week,iso_weekday,iso_name) ! ISO week of year
                    WRITE(text(iout:),'(I0)')iso_week
         !=====================================================================================
         CASE('I'); CALL woy(values,iso_year,iso_week,iso_weekday,iso_name) ! iso-8601 Week-numbering year date
                    WRITE(text(iout:),'(a)')iso_name
         !=====================================================================================
         CASE('j'); CALL d2j(values,julian,ierr)                          ! integer Julian date (truncated to integer)
                    WRITE(text(iout:),'(I0)')int(julian)
         !=====================================================================================
         CASE('J'); CALL d2j(values,julian,ierr)                          ! Julian date to milliseconds
                    WRITE(text(iout:),'(I0,".",i3.3)')int(julian),int((julian-int(julian))*1000.0)
         !=====================================================================================
         CASE('k'); call system_clock(count=systemclock,count_rate=countrate)  ! systemclock/countrate
                    WRITE(text(iout:),'(G0)')real(systemclock)/countrate
         !=====================================================================================
         CASE('l'); WRITE(text(iout:),'(A3)')v2mo(values(2))              ! three characters of the name of the month of the year
         !=====================================================================================
         CASE('L'); WRITE(text(iout:),'(A)')v2mo(values(2))               ! name of the month of the year
         !=====================================================================================
         CASE('m'); WRITE(text(iout:),'(I2.2)')values(6)                  ! the minutes of the hour, in the range 0 to 59
         !=====================================================================================
         CASE('M'); WRITE(text(iout:),'(I2.2)')values(2)                  ! month of year (1..12)
         !=====================================================================================
         CASE('N'); if( values(5).ge.12)then                              ! AM||PM
                       WRITE(text(iout:),'("PM")')
                    else
                       WRITE(text(iout:),'("AM")')
                    endif
         !=====================================================================================
         CASE('O'); WRITE(text(iout:),'(I3.3)')d2o(values)                ! Ordinal day of year
         !=====================================================================================
         CASE('s'); WRITE(text(iout:),'(I2.2)')values(7)                  ! the seconds of the minute, in the range 0 to 60
         !=====================================================================================
         CASE('S'); IF(.NOT.since)THEN                                    ! seconds since last called
                       since=.TRUE.
                       CALL d2u(values,unixtime_last,ierr)
                    ENDIF
                    CALL d2u(values,unixtime,ierr)
                    WRITE(text(iout:),'(G0)')unixtime-unixtime_last
                    unixtime_last=unixtime
         !=====================================================================================
         CASE('t'); WRITE(text(iout:),'(A1)')CHAR(9)                      ! tab character
         !=====================================================================================
         CASE('U'); CALL dow(values,weekday,day,ierr)                     ! Return the day of the week, 1..7 Sunday=1
                    WRITE(text(iout:),'(I1)')weekday
         !=====================================================================================
         CASE('u'); CALL dow(values,weekday,day,ierr)                     ! Return the day of the week, 1..7 Monday=1
                    WRITE(text(iout:),'(I1)')mod(weekday+5,7)+1
         !=====================================================================================
         CASE('W'); CALL dow(values,weekday,day,ierr)                     ! Return the name of the day of the week
                    WRITE(text(iout:),'(a)')day
         !=====================================================================================
         CASE('w'); CALL dow(values,weekday,day,ierr)                     ! Return the first three characters of the day of the week
                    WRITE(text(iout:),'(A3)')day(1:3)
         !=====================================================================================
         CASE('x'); WRITE(text(iout:),'(I3.3)')values(8)                  ! the milliseconds of the second, in the range 0 to 999
         !=====================================================================================
         CASE('Y'); WRITE(text(iout:),'(I4.4)')values(1)                  ! the year, including the century (for example, 1990)
         !=====================================================================================
         CASE('Z'); WRITE(text(iout:),'(SP,I5.4)')values(4)               ! time difference with respect to UTC in minutes
         !=====================================================================================
         CASE('z'); WRITE(text(iout:),'(I3.2,":",I2.2)')int(values(4)/60),abs(mod(values(4),60)) ! time from UTC as +-hh:mm
         !=====================================================================================
         CASE DEFAULT
            WRITE(text(iout:),'(A1)')chara
         !=====================================================================================
         END SELECT
         !=====================================================================================
         iout=len_trim(text)+1
      ELSE
         WRITE(text(iout:),'(A1)')chara;iout=iout+1
      ENDIF
   ENDDO
   timestring=trim(text)
END FUNCTION fmtdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine fmtdate_usage(ii)
! ident_15="@(#)fmtdate_usage(3f): display macros recognized by fmtdate(3f)"
character(len=51),allocatable :: usage(:)
integer                       :: i,ii
character(len=ii)             :: blanks
usage=[ &                                               !date(1) COMMAND
&' Base time array:                                  ',&
&' (1) %Y -- year, yyyy                              ',&
&' (2) %M -- month of year, 01 to 12                 ',&
&' (3) %D -- day of month, 01 to 31                  ',&
&'     %d -- day of month, with suffix (1st, 2nd,...)',&
&' (4) %Z -- minutes from UTC                        ',&
&'     %z -- -+hh:mm from UTC                        ',&
&' (5) %h -- hours, 00 to 23                         ',&
&'     %H -- hour (1 to 12, or twelve-hour clock)    ',&
&'     %N -- AM (before noon) PM (>=after noon)      ',&
&' (6) %m -- minutes, 00 to 59                       ',&
&' (7) %s -- sec, 00 to 60                           ',&
&' (8) %x -- milliseconds 000 to 999                 ',&
&'Conversions                                        ',&
&'     %E -- Unix Epoch time                         ',&
&'     %e -- integer value of Unix Epoch time        ',&
&'     %J -- Julian  date                            ',&
&'     %j -- integer value of Julian date            ',&
&'     %O -- Ordinal day (day of year)               ',&
&'     %U -- day of week, 1..7 Sunday=1              ',&
&'     %u -- day of week, 1..7 Monday=1              ',&
&'     %i -- ISO week of year 1..53                  ',&
&'     %I -- iso-8601 week-numbering date(yyyy-Www-d)',&
&' Names                                             ',&
&'     %l -- abbreviated month name                  ',&
&'     %L -- full month name                         ',&
&'     %w -- first three characters of weekday       ',&
&'     %W -- weekday name                            ',&
&' Literals                                          ',&
&'     %% -- a literal %%                            ',&
&'     %t -- tab character                           ',&
&'     %b -- blank character                         ',&
&' Program timing:                                   ',&
&'     %c -- CPU_TIME(3f) output                     ',&
&'     %C -- number of times this routine is used    ',&
&'     %k -- time in seconds from SYSTEM_CLOCK(3f)   ',&
&'                                                   ']
   blanks=' '
   WRITE(*,'(a,a)')(blanks(:ii),usage(i),i=1,size(usage))
end subroutine fmtdate_usage
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dow(values, weekday, day, ierr)
! ident_16="@(#)dow(3f): Return the day of the week"
real(kind=dp)                      :: julian    ! the julian day for which the weekday is required,
integer,intent(in)                 :: values(8) ! date and time array used to get time zone
integer,intent(out),optional       :: weekday   ! The day of the week, 1 = Sunday
character*(*),intent(out),optional :: day       ! The name of the day of the week, e.g. 'Sunday'. Minimum length = 9
integer,intent(out)                :: ierr      ! Error return,0=correct,-1=invalid Julian day,-2=neither day nor weekday specified
   integer                         :: iweekday

   call d2j(values,julian,ierr)                 ! need julian date to calculate day of week for first day of month
   ierr = 0

   if(julian < 0) then
      ierr = -1
      return
   endif

   if(.not.present(day).and. .not.present(weekday)) then
      ierr=-2
      return
   endif

   ! julian day starts at noon so add 1/2 day
   ! add time zone
   iweekday = mod(int((julian+dble(values(4)/60.0d0/24.0d0)+0.5d0)+1.0d0), 7)
   iweekday = iweekday +1

   if(present(day)) then
      select case(iweekday)
      case(1)     ;day = 'Sunday'
      case(2)     ;day = 'Monday'
      case(3)     ;day = 'Tuesday'
      case(4)     ;day = 'Wednesday'
      case(5)     ;day = 'Thursday'
      case(6)     ;day = 'Friday'
      case(7)     ;day = 'Saturday'
      case default;day = 'E-R-R-O-R'
      end select
   endif

   if(present(weekday))then
      weekday=iweekday
   endif

end subroutine dow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine woy(dat,iso_year,iso_week,iso_weekday,iso_name)
!-----------------------------------------------------------------------------------------------------------------------------------
!  The ISO-8601 date and time standard was issued by the International Organization for Standardization (ISO).
!  It is used (mainly) in government and business for fiscal years, as well as in timekeeping.
!  The system specifies a week year atop the Gregorian calendar by defining a notation for ordinal weeks of the year.
!
!  An ISO week-numbering year (also called ISO year informally) has 52 or 53 full weeks.
!  That is 364 or 371 days instead of the usual 365 or 366 days.
!  The extra week is referred to here as a leap week, although ISO 8601 does not use this term.
!  Weeks start with Monday.
!  The first week of a year is the week that contains the first Thursday of the year (and, hence, always contains 4 January).
!  ISO week year numbering therefore slightly deviates from the Gregorian for some days close to 1 January.
!-----------------------------------------------------------------------------------------------------------------------------------
!CALCULATION:
!  The ISO-8601 week number of any date can be calculated, given its ordinal date (i.e. position within the year)
!  and its day of the week.

!METHOD:
!   Using ISO weekday numbers (running from 1 for Monday to 7 for Sunday),
!   subtract the weekday from the ordinal date, then add 10. Divide the result
!   by 7. Ignore the remainder; the quotient equals the week number. If
!   the week number thus obtained equals 0, it means that the given date
!   belongs to the preceding (week-based) year. If a week number of 53 is
!   obtained, one must check that the date is not actually in week 1 of the
!   following year.
! These two statements are assumed true when correcting the dates around January 1st ...
!   o  The number of weeks in a given year is equal to the corresponding week number of 28 December.
!   o  January 4th is always in the first week.
!
!ISO_NAME:
!  Week date representations are in the format YYYYWww-D.
!  o [YYYY] indicates the ISO week-numbering year which is slightly different from the traditional Gregorian calendar year.
!  o [Www] is the week number prefixed by the letter W, from W01 through W53.
!  o [D] is the weekday number, from 1 through 7, beginning with Monday and ending with Sunday.
!
!  For example, the Gregorian date 31 December 2006 corresponds to the Sunday of the 52nd week of 2006, and is written
!     2006-W52-7 (extended form)
!  or 2006W527 (compact form).
!
!REFERENCE:
!  From Wikipedia, the free encyclopedia 2015-12-19
!AUTHOR:
!  John S. Urban, 2015-12-19
!-----------------------------------------------------------------------------------------------------------------------------------
! ident_17="@(#)woy(3f): Calculate iso-8601 Week-numbering year date yyyy-Www-d"
integer,parameter               :: dp=kind(0.0d0)
integer,intent(in)              :: dat(8)     ! input date array
integer,intent(out)             :: iso_year, iso_week, iso_weekday
character(len=10),intent(out)   :: iso_name
integer                         :: shared_weekday
integer                         :: last_week_this_year
integer                         :: dec28_lastyear(8)   ! December 28th is always in last week
integer                         :: dec28_thisyear(8)   ! December 28th is always in last week
character(len=9)                :: day
integer                         :: ierr
   iso_year=dat(1)                                               ! initially assume the iso_year is the same as the data array year
   iso_week=uncorrected_week_of_year(dat)                        ! this is the week number unless around January 1st
   iso_weekday=shared_weekday                                    ! this is the number of the day of the week assuming Monday=1
   dec28_thisyear=[dat(1),12,28,dat(4),0,0,0,0]                  ! Dec 28th is always in last week; use this to get number of weeks
   last_week_this_year=uncorrected_week_of_year(dec28_thisyear)  ! get the number of the last week of the year (52 or 53)
   ! correct dates around January 1st
   if(iso_week  < 1)then                                         ! if week < 1 then week = lastWeek(year -1)
      dec28_lastyear=[dat(1)-1,12,28,dat(4),0,0,0,0]             ! Dec 28th is always in last week, we want its week number
      iso_week=uncorrected_week_of_year(dec28_lastyear)          ! got the week number for the last week of last year (52 or 53)
      iso_year=dat(1)-1                                          ! our date belongs to last year
   elseif(iso_week >last_week_this_year)then                     ! if week > lastweek(year) then week = 1
      iso_week=iso_week-last_week_this_year                      ! our date belongs to next year
      iso_year=dat(1)+1
   endif

   write(iso_name,'(i4.4,"-W",i2.2,"-",i1)')iso_year,iso_week,iso_weekday ! create ISO string designation for our date

contains
   function uncorrected_week_of_year(datin)
   implicit none
   integer            :: uncorrected_week_of_year
   integer,intent(in) :: datin(8)
      integer         :: ordinal
      call dow(datin,shared_weekday,day,ierr)                 ! formula needs day of week 1..7 where Monday=1
      shared_weekday=mod(shared_weekday+5,7)+1                ! change from Sunday=1 to Monday=1
      ordinal=d2o(datin)                                      ! formula needs ordinal day of year where Jan 1st=1
      uncorrected_week_of_year=(ordinal-shared_weekday+10)/7
   end function uncorrected_week_of_year

end subroutine woy
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function dj(dat) result (julian)
! ident_18="@(#)dj(3f): Given date array returns Julian Day"
real(kind=dp)              :: julian
integer,intent(in)         :: dat(8)
   integer                 :: ierr
call d2j(dat,julian,ierr)
end function dj

function jd(julian) result (dat)
! ident_19="@(#)jd(3f): Given Julian Day returns date array"
real(kind=dp),intent(in)   :: julian
integer                    :: dat(8)
   integer                 :: ierr
call j2d(dat,julian,ierr)
end function jd

function du(dat) result (unixtime)
! ident_20="@(#)du(3f): Given date array returns Unix Epoch time"
real(kind=dp)              :: unixtime
integer,intent(in)         :: dat(8)
   integer                 :: ierr
call d2u(dat,unixtime,ierr)
end function du

function ud(unixtime) result (dat)
! ident_21="@(#)ud(3f): Given Unix Epoch Time returns date array"
real(kind=dp),intent(in)   :: unixtime
integer                    :: dat(8)
   integer                 :: ierr
call u2d(dat,unixtime,ierr)
end function ud
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!
!   XXXX
!  X    X
! X
! X
! X
! X
! X
!  X    X
!   XXXX
!
subroutine sys_sleep(wait_seconds)
use, intrinsic  :: iso_c_binding, only: c_int

! ident_22="@(#)sys_sleep(3f): call sleep(3c)"

integer (c_int) :: wait_seconds, how_long
interface
      function c_sleep (seconds)  bind ( C, name="sleep" )
          import
          integer (c_int) :: c_sleep !  should be unsigned int (not available in Fortran).  OK until highest bit gets set.
          integer (c_int), intent (in), VALUE :: seconds
      end function c_sleep
end interface
   if(wait_seconds.gt.0)then
      how_long = c_sleep(wait_seconds)
   endif
end subroutine sys_sleep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function now_ex(format)

! ident_23="@(#)M_time::now_ex(3f): use of now(3f) outside of a module"

character(len=*),intent(in),optional :: format
character(len=:),allocatable         :: now_ex
   now_ex=now(format)
end function now_ex
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
 
 
!>>>>> build/dependencies/M_msg/src/M_help.f90
module M_help
use,intrinsic :: iso_fortran_env, only : stderr=>error_unit, stdin=>input_unit, stdout=>output_unit
use M_journal, only : journal
implicit none
private
public help_command
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    help_command(3f) - [M_help] uses a specially formatted text array to
!!    provide a HELP interface
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Syntax:
!!
!!      function help_command(text_array,topic,position)
!!
!!##DESCRIPTION
!!    This routine, when given a CHARACTER array displays the text
!!    interactively. The special topics "manual","topics", and "search"
!!    are reserved. "manual" causes the entire array to be displayed.
!!    "topics" displays all lines not beginning with a space or three or
!!    more equal signs, and "search" must be followed by a string to search
!!    for in the manual.
!!
!!    A line beginning with a non-blank character in column one is a topic
!!
!!    A topic with the preceding line beginning with "===" is a special
!!    topic that will be displayed up to the next line beginning with "==="
!!
!!    The special topic "manual" displays the entire help text
!!
!!    The help text is paged based on the values in the position() array. The
!!    first value is the current line count on the current page, and the
!!    second value is how many lines should be displayed as a page before
!!    a paging prompt is produced. POSITION(2) is typically set to 23.
!!    POSITION(1) can be set to zero, especially if the calling page is
!!    not tracking paging itself.
!!
!!    Entering a "q" at the prompt exits the help text. To see other options
!!    enter an "h" at the prompt.
!!
!!       h
!!       #----------------------------------------------------# PAGING
!!       | f|SPACE b  forward or backward one page            |
!!       | u d        redraw up or down one-half page         |
!!       | r          refresh page                            |
!!       | e y | j k  refresh page moving up or down one line |
!!       #----------------------------------------------------# JUMPING
!!       | g          go to top of manual                     |
!!       | NNN        go to line number NNN. Use a sign (+-)  |
!!       |            for a relative move.                    |
!!       | .          toggle line numbering                   |
!!       #----------------------------------------------------# SEARCHING
!!       | /STRING    advance to next line containing string  |
!!       | ?STRING    search for string above current line    |
!!       | n N        find next occurrence up or down in file |
!!       | \STRING    show all lines with specified string.   |
!!       | t          displays topic lines.                   |
!!       #----------------------------------------------------#
!!       | w FILENAME write entire user guide to local file   |
!!       | h          display this help                       |
!!       | q          quit                                    |
!!       #----------------------------------------------------#
!!       Anything else is ignored.
!!       Line count is 25 out of 54 . Page size is 23 (see "lines")
!!       continue ..
!!
!!
!!    A normal topic is displayed until another topic line (line beginning
!!    with a non-blank) is encountered
!!
!!    The help text must begin with a line starting with "==="
!!
!!    If a line is preceded by an "===" line it is considered a section
!!    instead of a topic, and all lines between that line and the next line
!!    beginning with "===" are displayed.
!!##OPTIONS
!!    help_text  The block of text to treat as the input document
!!
!!    topic      What topic or section to search for (case sensitive). A blank
!!               is equivalent to "SUMMARY". There are several reserved names.
!!               "manual" means the entire help text, and "topics" shows only
!!               lines not beginning with a blank, and "search" does a
!!               case-insensitive search for a string.
!!
!!    position   A small array with two values. The second value is the size
!!               of the page to be used between pauses. The first one indicates
!!               how many lines on the current page have been displayed.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_help_command
!!    use M_help, only : help_command
!!    character(len=:),allocatable :: help_text(:)
!!    integer                      :: position(2)
!!    position=[0,23]
!!    help_text=[character(len=80) :: &
!!    '==============================================',&
!!    '   A sample help text file.                   ',&
!!    '   Note the first line MUST start with "==="  ',&
!!    '==============================================',&
!!    'SUMMARY                                       ',&
!!    '  This is usually a crib sheet                ',&
!!    '==============================================',&
!!    'SECTION1                                      ',&
!!    'topic1                                        ',&
!!    '   A description of topic 1                   ',&
!!    '                                              ',&
!!    '   and any general text you want              ',&
!!    '                                              ',&
!!    'topic2  A description of topic 2              ',&
!!    'topic3                                        ',&
!!    '   A description of topic 3                   ',&
!!    '   more  description of topic 3               ',&
!!    '   and more description of topic 3 a          ',&
!!    '   and more description of topic 3 b          ',&
!!    '   and more description of topic 3 c          ',&
!!    '   and more description of topic 3 d          ',&
!!    '   and more description of topic 3 e          ',&
!!    '   and more description of topic 3 f          ',&
!!    '   and more description of topic 3 g          ',&
!!    '   and more description of topic 3 h          ',&
!!    '   and more description of topic 3 i          ',&
!!    '   and more description of topic 3 j          ',&
!!    '   and more description of topic 3 k          ',&
!!    '   and more description of topic 3 l          ',&
!!    '   and more description of topic 3 m          ',&
!!    '   and more description of topic 3 n          ',&
!!    '   and more description of topic 3 o          ',&
!!    '   and more description of topic 3 p          ',&
!!    '   and more description of topic 3 q          ',&
!!    '   and more description of topic 3 r          ',&
!!    '   and more description of topic 3 s          ',&
!!    '   and more description of topic 3 t          ',&
!!    '   and more description of topic 3 u          ',&
!!    '   and more description of topic 3 v          ',&
!!    '   and more description of topic 3 w          ',&
!!    '   and more description of topic 3 x          ',&
!!    '   and more description of topic 3 y          ',&
!!    '   and more description of topic 3 z          ',&
!!    '==============================================',&
!!    'SECTION2                                      ',&
!!    'topic4  A description of topic 4              ',&
!!    '   this is the last part of SECTION1          ',&
!!    'topic5                                        ',&
!!    '  This is all about the fifth topic and is    ',&
!!    '  just displayed as-is. The text cannot start ',&
!!    '  in column one or it will be seen as the     ',&
!!    '  beginning of a topic.                       ',&
!!    '==============================================',&
!!    '                                              ' ]
!!
!!    write(*,*)'>>>>>'
!!    call help_command(help_text,'',position)
!!    write(*,*)'>>>>>topic1'
!!    call help_command(help_text,'topic1',position)
!!    write(*,*)'>>>>>topics'
!!    call help_command(help_text,'topics',position)
!!    write(*,*)'>>>>>manual'
!!    call help_command(help_text,'manual',position)
!!    end program demo_help_command
subroutine help_command(help_text,topic_name,position)

! ident_1="@(#)M_help::help_command(3f): interactively display help text"

character(len=*),intent(in)            :: help_text(:)
character(len=*),intent(in)            :: topic_name
integer                                :: position(2)
integer                                :: end_of_first_word
integer                                :: start_of_topic
integer                                :: ios
character(len=:),allocatable           :: topic, old_topic, string
logical                                :: block_topic
integer                                :: i, j, k, jj, ii
logical                                :: numbered
character(len=len(help_text))          :: last_response
integer                                :: toomany
integer,parameter                      :: max_toomany=2000
integer                                :: howbig
integer                                :: old_position

   howbig=size(help_text)
   toomany=1
   last_response=' '
   numbered=.false.
   topic=trim(topic_name)
   old_topic=''
   old_position=0
   if(index(topic,'search ').eq.1)then
      topic='search'
   endif
   INFINITE: do

      if (topic.eq.' ') then                                           ! if no topic
         call journal('Type "help" followed by a case-sensitive topic name ...')
         topic='SUMMARY'
      endif
      select case(topic)
      case('manual')                                  ! show all the help text
         i=0
         do
            i=i+1
            if(i.gt.howbig)exit
            if(help_text(i)(1:3).eq.'===')then
               if(numbered)then
                  call journal('sc',i,' ')
               else
                  call journal(' ')
               endif
            else
               if(numbered)then
                  call journal('sc',i,help_text(i))
               else
                  call journal('sc',help_text(i))
               endif
            endif
            if(want_to_stop())exit INFINITE
            if(old_topic.ne.'')cycle INFINITE
            if(i.ge.howbig) then
               do j=1,max_toomany
                  call journal('sc','[end-of-file] (line',i,')')
                  position(1)=position(2)+1
                  if(want_to_stop())exit INFINITE
                  if(old_topic.ne.'')cycle INFINITE
                  if(i.lt.howbig)exit
               enddo
               if(i.ge.howbig)exit
            endif
         enddo
         exit INFINITE
      case('topics')                         ! go through all the text showing lines not starting with a a space or equal
         i=1                                 ! display topic starting at start_of_topic
         do
            i=i+1
            if(i.gt.howbig) exit
            if(help_text(i)(1:1).eq.'   ')cycle
            if(help_text(i)(1:3).eq.'===')cycle
            jj=merge(0,3,help_text(i-1)(1:3).eq.'===')
            if(numbered)then
               call journal('sc',i,'>',repeat(' ',jj)//help_text(i))
            else
               call journal('sc','>',repeat(' ',jj)//help_text(i))
            endif
            if(want_to_stop())then
               if(old_topic.ne.'')then
                  topic=old_topic
                  old_topic=''
                  i=old_position
                  call pageback(1)
                  i=max(1,i)
                  position(1)=position(2)+1
                  cycle INFINITE
               endif
               exit INFINITE
            endif
         enddo
         if(old_topic.ne.'')then
            topic=old_topic
            old_topic=''
            i=old_position
            call pageback(1)
            i=max(1,i)
            position(1)=position(2)+1
            cycle INFINITE
         endif
         exit INFINITE
      case('search')                         ! go through all the text showing lines matching string
         position(1) = 0
         string=topic_name//'        '
         string=trim(lower(adjustl(string(8:))))
         i=0
         do
            i=i+1
            if(i.gt.howbig)exit
            if(help_text(i)(1:1).ne.' '.and.help_text(i)(1:3).ne.'===')then
               old_topic=help_text(i)//' '
               ii=index(old_topic,' ')
               old_topic=old_topic(:ii)
            endif
            if(index(lower(help_text(i)),string).ne.0)then
               if(numbered)then
                  call journal('sc',i,help_text(i))
               else
                  call journal('sc',old_topic,'>',help_text(i))
               endif
               if(want_to_stop())exit INFINITE
               if(i.ge.howbig) then
                  do j=1,max_toomany
                     call journal('sc','[end-of-file] (line',i,')')
                     position(1)=position(2)+1
                     if(want_to_stop())exit INFINITE
                     if(i.lt.howbig)exit
                  enddo
                  if(i.ge.howbig)exit
               endif
            endif
         enddo
         exit INFINITE
      case default ! find the line that starts with the topic
         start_of_topic=0
         ! find the line to start with by finding a line that starts with the given topic ( ASSUMING FIRST LINE is ===)
         FINDIT: do j=1,len(help_text)
            do i=2, howbig                                          ! get first word of lines not starting with a blank
               if(help_text(i)(1:1).ne.' ')then                              ! only topic lines start in column one so skip these
                  end_of_first_word=index(help_text(i),' ')-1
                  if(end_of_first_word.eq.0)end_of_first_word=len(help_text) ! if line is filled and does not have a blank
                  end_of_first_word=end_of_first_word-j+1
                  if(end_of_first_word.le.0)cycle
                  !x!write(*,*)'['//topic(:end_of_first_word)//']['//help_text(i)(:end_of_first_word)//']'
                  if(topic.eq.help_text(i)(:end_of_first_word))then      ! find a line that matches topic
                     exit FINDIT
                  endif
               endif
            enddo
         enddo FINDIT
         start_of_topic=i

         if(i.eq.0)then
            call journal('<ERROR> internal error. First line of text must start with "==="')
            !!help_text=[character(len=len(help_text)) :: repeat("=",80),help_text]
            start_of_topic=start_of_topic+1
         endif

         if(help_text(i-1)(1:3).eq.'===')then  ! if the line above the start started with "===" it is a block comment
            block_topic=.true.
         else
            block_topic=.false.
         endif

         if(start_of_topic.gt.howbig.or.start_of_topic.eq.0)then
            call journal('sc','SORRY, No help on ',topic)
         else
            position(1) = 0
            if(numbered)then
               call journal('sc',i,help_text(start_of_topic))                       ! show the start line
            else
               call journal('sc',help_text(start_of_topic))                       ! show the start line
            endif

            i=start_of_topic+1                                              ! display topic starting at start_of_topic
            do
               if(help_text(i)(1:1).ne.' '.and. .not.block_topic )then       ! stop at next topic if not a block of help
                  exit
               elseif(block_topic .and. help_text(i)(1:3).eq.'===')then
                  exit
               endif
               if(numbered)then
                  call journal('sc',i,help_text(i))
               else
                  call journal('sc',help_text(i))
               endif
               if(want_to_stop())exit INFINITE
               if(old_topic.ne.'')cycle INFINITE
               toomany=toomany+1
               if(toomany.ge.max_toomany)exit INFINITE    ! to prevent infinite loops in batch mode
               i=max(start_of_topic-1,i)
               i=i+1
               if(i.gt.howbig) exit
            enddo
         endif
         exit INFINITE
      end select
      if(want_to_stop())exit INFINITE
   enddo INFINITE
contains

function want_to_stop()
character(len=len(help_text))      :: response
character(len=1)                     :: letter
logical                              :: want_to_stop
integer                              :: j
integer                              :: jj
doubleprecision                      :: val
integer                              :: ierr
   position(1) = position(1) + 1
   want_to_stop=.false.
   PROMPT: do
      if(position(1) .gt. position(2)) then
         call journal('sc','continue ..')
         read(stdin,'(a)',iostat=ios) response         ! read letter to pause from standard input
         response=adjustl(response)
         letter=response(1:1)
         select case(letter)
         case(' ','f')                                        ! next page
            position(1) = 0                                ! start new page
         case('b')                                            ! back one page
            call pageback(2)
            position(1) = 0
         case('0':'9')                                        ! assumed to be a number
            call a2d(response,val,ierr)
            i=nint(val)-1
            i=max(i,1)
            i=min(i,howbig-1)
            position(1) = 0
         case('-','+')                                        ! assumed to be a number
            call pageback(1)
            call a2d(response,val,ierr)
            i=i+nint(val)
            i=max(i,1)
            i=min(i,howbig-1)
            position(1) = 0
         case('t')                                            ! topics
            old_topic=topic
            old_position=i
            topic='topics'
            position(1)=0
            exit PROMPT
            !do j=2,howbig
            !   if(help_text(j)(1:1).eq.'   ')cycle
            !   if(help_text(j)(1:3).eq.'===')cycle
            !   jj=merge(0,3,help_text(j-1)(1:3).eq.'===')
            !   if(numbered)then
            !      call journal('sc',j,'>',repeat(' ',jj)//help_text(j))
            !   else
            !      call journal('sc','>',repeat(' ',jj)//help_text(j))
            !   endif
            !enddo
            !call pageback(1)
            !i=max(1,i)
            !position(1)=position(2)+1
            !cycle PROMPT
         case('u')                                            ! back one-half page
            call pageback(1)
            i=max(1,i-position(2)/2-1)
            position(1) = 0
         case('e','k')                                        ! back one line page
            call pageback(1)
            i=max(1,i-1)
            position(1) = 0
         case('y','j')                                        ! down one line page
            call pageback(1)
            i=max(1,i+1)
            position(1) = 0
         case('w')
            WRITEFILE: block
            character(len=1000) :: errmsg
            integer :: temp_lun
               response=adjustl(response(2:))
               if(response.eq.'')response='userguide.txt'
               open(newunit=temp_lun,file=response,status='new',iostat=ios,iomsg=errmsg) ! open help file
               if(ios.eq.0)then
                  write(temp_lun,'(a)',iostat=ios)( trim(help_text(k)),k=1,howbig )
                  call journal('sc','<INFO> user guide is on file',trim(response) )
                  close(unit=temp_lun,iostat=ios)
               else
                  call journal(trim(errmsg))
               endif
            endblock WRITEFILE
            i=max(1,i-1)
         case('d')                                            ! down one-half page
            i=min(howbig-1,i-position(2)/2-1)
            position(1) = 0
         case('r')                                            ! repaint page
            call pageback(1)
            position(1) = 0
         case('/','n')                                        ! find string below
            j=i ! hold
            if(letter.eq.'n')response=last_response
            if(response(2:).eq.'')response=last_response
            i=i+1
            do
               if(index(lower(help_text(i)),trim(response(2:))).ne.0)then
                  i=max(1,i-1)

                  exit
               else
                  i=i+1
               endif
               if(i.gt.howbig) exit
            enddo
            if(i.gt.howbig)i=j
            position(1) = 0
            last_response=response
         case('\') ! find string
            response=lower(adjustl(response(2:)))
            if(response.eq.' ')response=last_response
            jj=len_trim(response)
            do j=1,howbig
               if(index(lower(help_text(j)),response(:jj)).ne.0)then
                  call journal('sc',j,help_text(j))
               endif
            enddo
            i=i-1
            call pageback(1)
            last_response='/'//response
         case('?','N','p')                                            ! find string above
            j=i ! hold
            if(letter.eq.'N'.or.letter.eq.' ')response=last_response
            if(response(2:).eq.'')response=last_response
            i=i-1
            do
               if(index(lower(help_text(i)),trim(response(2:))).ne.0)then
                  exit
               else
                  i=i-1
               endif
               if(i.le.1) then
                  i=j
                  exit
               endif
            enddo
            call pageback(1)
            position(1) = 0
            last_response=response
         case('g')                                            ! repaint page
            i=1
            position(1) = 0
         case('.')                                            ! help
            position(1) = 0
            numbered=.not.numbered
         case('h')                                            ! help
            call journal('sc','#----------------------------------------------------# PAGING')
            call journal('sc','| f|SPACE b  forward or backward one page            |')
            call journal('sc','| u d        redraw up or down one-half page         |')
            call journal('sc','| r          refresh page                            |')
            call journal('sc','| e y | j k  refresh page moving up or down one line |')
            call journal('sc','#----------------------------------------------------# JUMPING')
            call journal('sc','| g          go to top of manual                     |')
            call journal('sc','| NNN        go to line number NNN. Use a sign (+-)  |')
            call journal('sc','|            for a relative move.                    |')
            call journal('sc','| .          toggle line numbering                   |')
            call journal('sc','#----------------------------------------------------# SEARCHING')
            call journal('sc','| /STRING    advance to next line containing string  |')
            call journal('sc','| ?STRING    search for string above current line    |')
            call journal('sc','| n N        find next occurrence up or down in file |')
            call journal('sc','| \STRING    show all lines with specified string.   |')
            call journal('sc','| t          displays topic lines.                   |')
            call journal('sc','#----------------------------------------------------#')
            call journal('sc','| w FILENAME write entire user guide to local file   |')
            call journal('sc','| h          display this help                       |')
            call journal('sc','| q          quit                                    |')
            call journal('sc','#----------------------------------------------------#')
            call journal('sc','Anything else is ignored.')
            call journal('sc','Line count is ',i,'out of',howbig,'. Page size is',position(2),'(see "lines")')
            cycle
         case('q')
            position(1) = -1
            want_to_stop=.true.
         case default
            call pageback(2)
            call journal('sc','unknown option -- enter "h" for assistance or "q" to quit')
         end select
      endif
      exit
   enddo PROMPT
end function want_to_stop

subroutine pageback(loops)
integer,intent(in) :: loops
integer            :: j
   do j=1,loops
      i=max(1,i-position(2)-1)
   enddo
end subroutine pageback

end subroutine help_command

subroutine a2d(chars,valu,ierr)

! ident_2="@(#)M_strings::a2d(3fp): subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o works with any g-format input, including integer, real, and exponential.
!  o if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!    IERR will still be non-zero in this case.

character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)

character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
character(len=3),save        :: nan_string='NaN'

   ierr=0                                                    ! initialize error flag to zero
   local_chars=chars
   msg=''
   if(len(local_chars).eq.0)local_chars=' '
   write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
   read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
   if(ierr.ne.0)then                                         ! if an error occurred ierr will be non-zero.
      read(nan_string,'(g3.3)')valu
      call journal('sc','*a2d* - cannot produce number from string ['//trim(chars)//']')
      if(msg.ne.'')then
         call journal('sc','*a2d* - ['//trim(msg)//']')
      endif
   endif
end subroutine a2d

elemental pure function lower(str,begin,end) result (string)

! ident_3="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(in)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(1,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                   ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))-diff)   ! change letter to miniscule
      case default
      end select
   enddo

end function lower

end module M_help
 
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>>>>> app/compute.f90
program demo_calculator
!compute(1f): line mode calculator program (that calls calculator(3f))
use M_calculator, only: calculator,iclen_calc
! iclen_calc : max length of expression or variable value as a string
implicit none
integer,parameter         :: dp=kind(0.0d0)
character(len=iclen_calc) :: line
character(len=iclen_calc) :: outlin
character(len=iclen_calc) :: event
real(kind=dp)             :: rvalue
integer                   :: ierr
   ierr=0
   write(*,*)'Enter expressions or "funcs" or "dump"'
   call calculator('ownmode(1)',outlin,event,rvalue,ierr)
   ! activate user-defined function interface
   INFINITE: do
      read(*,'(a)',end=999)line
      if(line.eq.'.')stop
      call calculator(line,outlin,event,rvalue,ierr)
      select case (ierr)
         ! several different meanings to the error flag returned by calculator
       case(0)
         ! a numeric value was returned without error
         write(*,'(a,a,a)')trim(outlin),' = ',trim(line)
       case(2)
         ! a string value was returned without error
         write(*,'(a)')trim(event(:int(rvalue)))
       case(1)
         ! a request for a message has been returned (from DUMP or FUNC)
         write(*,'(a,a)')'message===>',trim(event(:len_trim(event)))
       case(-1)
         ! an error has occurred
         write(*,'(a,a)')'error===>',trim(event(:len_trim(event)))
       case default
         ! this should not occur
         WRITE(6,'(A,i10)')'*CALCULATOR* UNEXPECTED IERR VALUE ',IERR
      end select
   enddo INFINITE
999 continue
end program demo_calculator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
