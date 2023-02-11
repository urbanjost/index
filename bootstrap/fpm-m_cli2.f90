!>>>>> ././src/M_CLI2_docs.f90
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_CLI2_docs
implicit none
private
public help_intrinsics
!interface help_intrinsics
!   module procedure help_intrinsics_all
!   module procedure help_intrinsics_one
!end interface help_intrinsics
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics(name,prefix,topic,m_help) result (textblock)
character(len=*),intent(in)                       :: name
logical,intent(in),optional                       :: prefix
logical,intent(in),optional                       :: topic
logical,intent(in),optional                       :: m_help
character(len=256),allocatable                    :: textblock(:)
character(len=:),allocatable                      :: a, b, c
integer                                           :: i, p, pg
   select case(name)
   case('','manual','M_CLI2')
      textblock=help_intrinsics_all(prefix,topic,m_help)
   case('fortran','toc')
      textblock=help_intrinsics_section()
      do i=1,size(textblock)
         p = index(textblock(i), '[')
         pg = index(textblock(i), ']')
         if(p.gt.0.and.pg.gt.p)then
          a=textblock(i)(:p-1)
          b=textblock(i)(p:pg)
          c=textblock(i)(pg+1:)
          textblock(i)=b//' '//a//c
         endif
      enddo
      call sort_name(textblock)
   case default
      textblock=help_intrinsics_one(name,prefix,topic,m_help)
   end select
end function help_intrinsics
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_section() result (textblock)

!@(#) grab lines in NAME section and append them to generate an index of manpages

character(len=256),allocatable  :: textblock(:)
character(len=256),allocatable  :: add(:)
character(len=256),allocatable  :: label
character(len=10)               :: cnum
integer                         :: i
integer                         :: icount
logical                         :: is_label
logical                         :: grab
   allocate(textblock(0))
   icount=1
   do
      write(cnum,'(i0)') icount
      add=help_intrinsics_one(cnum)
      if( size(add) .eq. 0 ) exit
      label=''
      grab=.false.
      is_label=.false.
      do i=1,size(add)
         if(add(i).ne.'')then
            is_label=verify(add(i)(1:1),'ABCDEFGHIJKLMNOPQRSTUVWXYZ ') == 0 &
            .and. verify(trim(add(i)),'ABCDEFGHIJKLMNOPQRSTUVWXYZ ') == 0
         endif
         if(add(i).eq.'')then
            ! skip
         elseif(is_label.and.add(i).eq.'NAME')then
            grab=.true.
         elseif(is_label)then
            exit
         elseif(grab)then
            label=adjustl(trim(label))//' '//adjustl(compact(trim(add(i))))
         endif
      enddo
      textblock=[character(len=256) :: textblock,compact(label)]
      icount=icount + 1
   enddo
end function help_intrinsics_section
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_all(prefix,topic,m_help) result (textblock)
logical,intent(in),optional     :: prefix
logical,intent(in),optional     :: topic
logical,intent(in),optional     :: m_help
character(len=256),allocatable  :: textblock(:)
character(len=256),allocatable  :: header(:)
character(len=256),allocatable  :: add(:)
character(len=10)               :: cnum
integer                         :: icount
   allocate(textblock(0))
   icount=1
   do
      write(cnum,'(i0)') icount
      add=help_intrinsics_one(cnum,prefix,topic,m_help)
      if( size(add) .eq. 0 ) exit
      textblock=[character(len=256) :: textblock,add]
      icount=icount + 1
   enddo
   if(present(m_help))then
      if(m_help)then
         header=[ character(len=256) :: &
         '================================================================================',    &
         'SUMMARY',    &
         ' The primary Fortran topics are',    &
         ' tan                   tanh                      this_image',    &
         '']
         textblock=[header,textblock]
      endif
   endif
end function help_intrinsics_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_one(name,prefix,topic,m_help) result (textblock)
character(len=*),intent(in)      :: name
logical,intent(in),optional      :: prefix
logical,intent(in),optional      :: m_help
logical,intent(in),optional      :: topic
character(len=256),allocatable   :: textblock(:)
character(len=:),allocatable     :: shortname
integer                          :: i
select case(name)

case('1','get_args')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   get_args(3f) - [ARGUMENTS:M_CLI2] return keyword values when parsing', &
'   command line arguments', &
'   (LICENSE:PD)', &
'', &
'SYNOPSIS', &
'  get_args(3f) and its convenience functions:', &
'', &
'    use M_CLI2, only : get_args', &
'    ! convenience functions', &
'    use M_CLI2, only : dget, iget, lget, rget, sget, cget', &
'    use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets', &
'', &
'    subroutine get_args(name,value,delimiters)', &
'', &
'     character(len=*),intent(in) :: name', &
'', &
'     type(${TYPE}),allocatable,intent(out) :: value(:)', &
'     ! or', &
'     type(${TYPE}),allocatable,intent(out) :: value', &
'', &
'     character(len=*),intent(in),optional :: delimiters', &
'', &
'     where ${TYPE} may be from the set', &
'             {real,doubleprecision,integer,logical,complex,character(len=:)}', &
'DESCRIPTION', &
'', &
'   GET_ARGS(3f) returns the value of keywords after SET_ARGS(3f) has', &
'   been called to parse the command line. For fixed-length CHARACTER', &
'   variables see GET_ARGS_FIXED_LENGTH(3f). For fixed-size arrays see', &
'   GET_ARGS_FIXED_SIZE(3f).', &
'', &
'   As a convenience multiple pairs of keywords and variables may be', &
'   specified if and only if all the values are scalars and the CHARACTER', &
'   variables are fixed-length or pre-allocated.', &
'', &
'OPTIONS', &
'', &
'    NAME        name of commandline argument to obtain the value of', &
'    VALUE       variable to hold returned value. The kind of the value', &
'                is used to determine the type of returned value. May', &
'                be a scalar or allocatable array. If type is CHARACTER', &
'                the scalar must have an allocatable length.', &
'    DELIMITERS  By default the delimiter for array values are comma,', &
'                colon, and whitespace. A string containing an alternate', &
'                list of delimiter characters may be supplied.', &
'', &
'CONVENIENCE FUNCTIONS', &
'   There are convenience functions that are replacements for calls to', &
'   get_args(3f) for each supported default intrinsic type', &
'', &
'     o scalars -- dget(3f), iget(3f), lget(3f), rget(3f), sget(3f),', &
'                  cget(3f)', &
'     o vectors -- dgets(3f), igets(3f), lgets(3f), rgets(3f),', &
'                  sgets(3f), cgets(3f)', &
'', &
'   D is for DOUBLEPRECISION, I for INTEGER, L for LOGICAL, R for REAL,', &
'   S for string (CHARACTER), and C for COMPLEX.', &
'', &
'   If the functions are called with no argument they will return the', &
'   UNNAMED array converted to the specified type.', &
'', &
'EXAMPLE', &
'', &
'Sample program:', &
'', &
'    program demo_get_args', &
'    use M_CLI2,  only : filenames=>unnamed, set_args, get_args', &
'    implicit none', &
'    integer                      :: i', &
'     ! Define ARGS', &
'    real                         :: x, y, z', &
'    real,allocatable             :: p(:)', &
'    character(len=:),allocatable :: title', &
'    logical                      :: l, lbig', &
'     ! Define and parse (to set initial values) command line', &
'     !   o only quote strings and use double-quotes', &
'     !   o set all logical values to F or T.', &
'    call set_args(''         &', &
'       & -x 1 -y 2 -z 3     &', &
'       & -p -1,-2,-3        &', &
'       & --title "my title" &', &
'       & -l F -L F          &', &
'       & --label " "        &', &
'       & '')', &
'     ! Assign values to elements', &
'     ! Scalars', &
'    call get_args(''x'',x,''y'',y,''z'',z,''l'',l,''L'',lbig)', &
'     ! Allocatable string', &
'    call get_args(''title'',title)', &
'     ! Allocatable arrays', &
'    call get_args(''p'',p)', &
'     ! Use values', &
'    write(*,''(1x,g0,"=",g0)'')''x'',x, ''y'',y, ''z'',z', &
'    write(*,*)''p='',p', &
'    write(*,*)''title='',title', &
'    write(*,*)''l='',l', &
'    write(*,*)''L='',lbig', &
'    if(size(filenames) > 0)then', &
'       write(*,''(i6.6,3a)'')(i,''['',filenames(i),'']'',i=1,size(filenames))', &
'    endif', &
'    end program demo_get_args', &
'AUTHOR', &
'     John S. Urban, 2019', &
'LICENSE', &
'     Public Domain', &
'']

shortname="get_args"

call select()


case('2','get_args_fixed_length')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   get_args_fixed_length(3f) - [ARGUMENTS:M_CLI2] return keyword values', &
'   for fixed-length string when parsing command line', &
'   (LICENSE:PD)', &
'', &
'SYNOPSIS', &
'   subroutine get_args_fixed_length(name,value)', &
'', &
'    character(len=*),intent(in)  :: name', &
'    character(len=:),allocatable :: value', &
'    character(len=*),intent(in),optional :: delimiters', &
'', &
'DESCRIPTION', &
'', &
'   get_args_fixed_length(3f) returns the value of a string', &
'   keyword when the string value is a fixed-length CHARACTER', &
'   variable.', &
'', &
'OPTIONS', &
'', &
'   NAME   name of commandline argument to obtain the value of', &
'', &
'   VALUE  variable to hold returned value.', &
'          Must be a fixed-length CHARACTER variable.', &
'', &
'   DELIMITERS  By default the delimiter for array values are comma,', &
'               colon, and whitespace. A string containing an alternate', &
'               list of delimiter characters may be supplied.', &
'', &
'EXAMPLE', &
'Sample program:', &
'', &
'    program demo_get_args_fixed_length', &
'    use M_CLI2,  only : set_args, get_args_fixed_length', &
'    implicit none', &
'', &
'     ! Define args', &
'    character(len=80)   :: title', &
'     ! Parse command line', &
'    call set_args('' --title "my title" '')', &
'     ! Assign values to variables', &
'    call get_args_fixed_length(''title'',title)', &
'     ! Use values', &
'    write(*,*)''title='',title', &
'', &
'    end program demo_get_args_fixed_length', &
'', &
'AUTHOR', &
'     John S. Urban, 2019', &
'LICENSE', &
'     Public Domain', &
'']

shortname="get_args_fixed_length"

call select()


case('3','get_args_fixed_size')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   get_args_fixed_size(3f) - [ARGUMENTS:M_CLI2] return keyword values', &
'   for fixed-size array when parsing command line arguments', &
'   (LICENSE:PD)', &
'', &
'SYNOPSIS', &
'   subroutine get_args_fixed_size(name,value)', &
'', &
'    character(len=*),intent(in) :: name', &
'    [real|doubleprecision|integer|logical|complex] :: value(NNN)', &
'       or', &
'    character(len=MMM) :: value(NNN)', &
'', &
'    character(len=*),intent(in),optional :: delimiters', &
'', &
'DESCRIPTION', &
'', &
'   get_args_fixed_size(3f) returns the value of keywords for fixed-size', &
'   arrays after set_args(3f) has been called. On input on the command', &
'   line all values of the array must be specified.', &
'', &
'OPTIONS', &
'   NAME        name of commandline argument to obtain the value of', &
'', &
'   VALUE       variable to hold returned values. The kind of the value', &
'               is used to determine the type of returned value. Must be', &
'               a fixed-size array. If type is CHARACTER the length must', &
'               also be fixed.', &
'', &
'   DELIMITERS  By default the delimiter for array values are comma,', &
'               colon, and whitespace. A string containing an alternate', &
'               list of delimiter characters may be supplied.', &
'', &
'EXAMPLE', &
'Sample program:', &
'', &
'    program demo_get_args_fixed_size', &
'    use M_CLI2,  only : set_args, get_args_fixed_size', &
'    implicit none', &
'    integer,parameter   :: dp=kind(0.0d0)', &
'    ! DEFINE ARGS', &
'    real                :: x(2)', &
'    real(kind=dp)       :: y(2)', &
'    integer             :: p(3)', &
'    character(len=80)   :: title(1)', &
'    logical             :: l(4), lbig(4)', &
'    complex             :: cmp(2)', &
'    ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE', &
'    !   o only quote strings', &
'    !   o set all logical values to F or T.', &
'    call set_args('' &', &
'       & -x 10.0,20.0 &', &
'       & -y 11.0,22.0 &', &
'       & -p -1,-2,-3 &', &
'       & --title "my title" &', &
'       & -l F,T,F,T -L T,F,T,F  &', &
'       & --cmp 111,222.0,333.0e0,4444 &', &
'       & '')', &
'    ! ASSIGN VALUES TO ELEMENTS', &
'       call get_args_fixed_size(''x'',x)', &
'       call get_args_fixed_size(''y'',y)', &
'       call get_args_fixed_size(''p'',p)', &
'       call get_args_fixed_size(''title'',title)', &
'       call get_args_fixed_size(''l'',l)', &
'       call get_args_fixed_size(''L'',lbig)', &
'       call get_args_fixed_size(''cmp'',cmp)', &
'    ! USE VALUES', &
'       write(*,*)''x='',x', &
'       write(*,*)''p='',p', &
'       write(*,*)''title='',title', &
'       write(*,*)''l='',l', &
'       write(*,*)''L='',lbig', &
'       write(*,*)''cmp='',cmp', &
'    end program demo_get_args_fixed_size', &
'  Results:', &
'', &
'AUTHOR', &
'     John S. Urban, 2019', &
'LICENSE', &
'     Public Domain', &
'']

shortname="get_args_fixed_size"

call select()


case('4','get_subcommand')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   get_subcommand(3f) - [ARGUMENTS:M_CLI2] special-case routine for', &
'   handling subcommands on a command line', &
'   (LICENSE:PD)', &
'', &
'SYNOPSIS', &
'   function get_subcommand()', &
'', &
'    character(len=:),allocatable :: get_subcommand', &
'', &
'DESCRIPTION', &
'   In the special case when creating a program with subcommands it', &
'   is assumed the first word on the command line is the subcommand. A', &
'   routine is required to handle response file processing, therefore', &
'   this routine (optionally processing response files) returns that', &
'   first word as the subcommand name.', &
'', &
'   It should not be used by programs not building a more elaborate', &
'   command with subcommands.', &
'', &
'RETURNS', &
'   NAME   name of subcommand', &
'', &
'EXAMPLE', &
'Sample program:', &
'', &
'   program demo_get_subcommand', &
'   !x! SUBCOMMANDS', &
'   !x! For a command with subcommands like git(1)', &
'   !x! you can make separate namelists for each subcommand.', &
'   !x! You can call this program which has two subcommands (run, test),', &
'   !x! like this:', &
'   !x!    demo_get_subcommand --help', &
'   !x!    demo_get_subcommand run -x -y -z --title -l -L', &
'   !x!    demo_get_subcommand test --title -l -L --testname', &
'   !x!    demo_get_subcommand run --help', &
'      implicit none', &
'   !x! DEFINE VALUES TO USE AS ARGUMENTS WITH INITIAL VALUES', &
'      real               :: x=-999.0,y=-999.0,z=-999.0', &
'      character(len=80)  :: title="not set"', &
'      logical            :: l=.false.', &
'      logical            :: l_=.false.', &
'      character(len=80)  :: testname="not set"', &
'      character(len=20)  :: name', &
'      call parse(name) !x! DEFINE AND PARSE COMMAND LINE', &
'      !x! ALL DONE CRACKING THE COMMAND LINE.', &
'      !x! USE THE VALUES IN YOUR PROGRAM.', &
'      write(*,*)''command was '',name', &
'      write(*,*)''x,y,z .... '',x,y,z', &
'      write(*,*)''title .... '',title', &
'      write(*,*)''l,l_ ..... '',l,l_', &
'      write(*,*)''testname . '',testname', &
'   contains', &
'   subroutine parse(name)', &
'   !x! PUT EVERYTHING TO DO WITH COMMAND PARSING HERE FOR CLARITY', &
'   use M_CLI2, only : set_args, get_args, get_args_fixed_length', &
'   use M_CLI2, only : get_subcommand, set_mode', &
'   character(len=*)              :: name    ! the subcommand name', &
'   character(len=:),allocatable  :: help_text(:), version_text(:)', &
'      call set_mode(''response_file'')', &
'   ! define version text', &
'      version_text=[character(len=80) :: &', &
'         ''@(#)PROGRAM:     demo_get_subcommand            >'', &', &
'         ''@(#)DESCRIPTION: My demo program  >'', &', &
'         ''@(#)VERSION:     1.0 20200715     >'', &', &
'         ''@(#)AUTHOR:      me, myself, and I>'', &', &
'         ''@(#)LICENSE:     Public Domain    >'', &', &
'         '''' ]', &
'       ! general help for "demo_get_subcommand --help"', &
'       help_text=[character(len=80) :: &', &
'        '' allowed subcommands are          '', &', &
'        ''   * run  -l -L --title -x -y -z  '', &', &
'        ''   * test -l -L --title           '', &', &
'        '''' ]', &
'      ! find the subcommand name by looking for first word on command', &
'      ! not starting with dash', &
'      name = get_subcommand()', &
'      select case(name)', &
'      case(''run'')', &
'       help_text=[character(len=80) :: &', &
'        ''                                  '', &', &
'        '' Help for subcommand "run"        '', &', &
'        ''                                  '', &', &
'        '''' ]', &
'       call set_args( &', &
'       & ''-x 1 -y 2 -z 3 --title "my title" -l F -L F'',&', &
'       & help_text,version_text)', &
'       call get_args(''x'',x)', &
'       call get_args(''y'',y)', &
'       call get_args(''z'',z)', &
'       call get_args_fixed_length(''title'',title)', &
'       call get_args(''l'',l)', &
'       call get_args(''L'',l_)', &
'      case(''test'')', &
'       help_text=[character(len=80) :: &', &
'        ''                                  '', &', &
'        '' Help for subcommand "test"       '', &', &
'        ''                                  '', &', &
'        '''' ]', &
'       call set_args(&', &
'       & ''--title "my title" -l F -L F --testname "Test"'',&', &
'       & help_text,version_text)', &
'       call get_args_fixed_length(''title'',title)', &
'       call get_args(''l'',l)', &
'       call get_args(''L'',l_)', &
'       call get_args_fixed_length(''testname'',testname)', &
'      case default', &
'       ! process help and version', &
'       call set_args('' '',help_text,version_text)', &
'       write(*,''(*(a))'')''unknown or missing subcommand ['',trim(name),'']''', &
'       write(*,''(a)'')[character(len=80) ::  &', &
'       '' allowed subcommands are          '', &', &
'       ''   * run  -l -L -title -x -y -z   '', &', &
'       ''   * test -l -L -title            '', &', &
'       '''' ]', &
'       stop', &
'      end select', &
'   end subroutine parse', &
'   end program demo_get_subcommand', &
'', &
'AUTHOR', &
'     John S. Urban, 2019', &
'', &
'LICENSE', &
'     Public Domain', &
'']

shortname="get_subcommand"

call select()


case('5','M_CLI2')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   M_CLI2(3fm) - [ARGUMENTS::M_CLI2::INTRO] command line argument', &
'   parsing using a prototype command', &
'   (LICENSE:PD)', &
'SYNOPSIS', &
'  Available procedures and variables:', &
'', &
'     ! basic procedures', &
'     use M_CLI2, only : set_args, get_args, specified, set_mode', &
'     ! convenience functions', &
'     use M_CLI2, only : dget, iget, lget, rget, sget, cget', &
'     use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets', &
'     ! variables', &
'     use M_CLI2, only : unnamed, remaining, args', &
'     ! working with non-allocatable strings and arrays', &
'     use M_CLI2, only : get_args_fixed_length, get_args_fixed_size', &
'     ! special function for creating subcommands', &
'     use M_CLI2, only : get_subcommand(3f)', &
'', &
'DESCRIPTION', &
'   The M_CLI2 module cracks a Unix-style command line.', &
'', &
'   Typically one call to SET_ARGS(3f) is made to define the command', &
'   arguments, set default values and parse the command line. Then a call', &
'   is made to the convenience procedures or GET_ARGS(3f) proper for each', &
'   command keyword to obtain the argument values.', &
'', &
'   Detailed descriptions of each procedure and example programs are', &
'   included.', &
'', &
'EXAMPLE', &
'', &
'Sample minimal program which may be called in various ways:', &
'', &
'    mimimal -x 100.3 -y 3.0e4', &
'    mimimal --xvalue=300 --debug', &
'    mimimal --yvalue 400', &
'    mimimal -x 10 file1 file2 file3', &
'', &
'Program example:', &
'', &
'    program minimal', &
'    use M_CLI2,  only : set_args, lget, rget, sgets', &
'    implicit none', &
'    real    :: x, y', &
'    integer :: i', &
'    character(len=:),allocatable :: filenames(:)', &
'       ! define and crack command line', &
'       call set_args('' --yvalue:y 0.0 --xvalue:x 0.0 --debug F'')', &
'       ! get values', &
'       x=rget(''xvalue'')', &
'       y=rget(''yvalue'')', &
'       if(lget(''debug''))then', &
'          write(*,*)''X='',x', &
'          write(*,*)''Y='',y', &
'          write(*,*)''ATAN2(Y,X)='',atan2(x=x,y=y)', &
'       else', &
'          write(*,*)atan2(x=x,y=y)', &
'       endif', &
'       filenames=sgets() ! sget with no name gets "unnamed" values', &
'       if(size(filenames) > 0)then', &
'          write(*,''(g0)'')''filenames:''', &
'          write(*,''(i6.6,3a)'')(i,''['',filenames(i),'']'',i=1,size(filenames))', &
'       endif', &
'    end program minimal', &
'', &
'Sample program using get_args() and variants', &
'', &
'    program demo_M_CLI2', &
'    use M_CLI2,  only : set_args, get_args', &
'    use M_CLI2,  only : filenames=>unnamed', &
'    use M_CLI2,  only : get_args_fixed_length, get_args_fixed_size', &
'    implicit none', &
'    integer                      :: i', &
'    integer,parameter            :: dp=kind(0.0d0)', &
'     !', &
'     ! Define ARGS', &
'    real                         :: x, y, z', &
'    logical                      :: l, lbig', &
'    character(len=40)            :: label    ! FIXED LENGTH', &
'    real(kind=dp),allocatable    :: point(:)', &
'    logical,allocatable          :: logicals(:)', &
'    character(len=:),allocatable :: title    ! VARIABLE LENGTH', &
'    real                         :: p(3)     ! FIXED SIZE', &
'    logical                      :: logi(3)  ! FIXED SIZE', &
'     !', &
'     ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE', &
'     !   o set a value for all keywords.', &
'     !   o double-quote strings, strings must be at least one space', &
'     !     because adjacent double-quotes designate a double-quote', &
'     !     in the value.', &
'     !   o set all logical values to F', &
'     !   o numeric values support an "e" or "E" exponent', &
'     !   o for lists delimit with a comma, colon, or space', &
'    call set_args(''                         &', &
'            & -x 1 -y 2 -z 3                &', &
'            & -p -1 -2 -3                   &', &
'            & --point 11.11, 22.22, 33.33e0 &', &
'            & --title "my title" -l F -L F  &', &
'            & --logicals  F F F F F         &', &
'            & --logi F T F                  &', &
'            & --label " " &', &
'            ! note space between quotes is required', &
'            & '')', &
'     ! Assign values to elements using G_ARGS(3f).', &
'     ! non-allocatable scalars can be done up to twenty per call', &
'    call get_args(''x'',x, ''y'',y, ''z'',z, ''l'',l, ''L'',lbig)', &
'     ! As a convenience multiple pairs of keywords and variables may be', &
'     ! specified if and only if all the values are scalars and the CHARACTER', &
'     ! variables are fixed-length or pre-allocated.', &
'     !', &
'     ! After SET_ARGS(3f) has parsed the command line', &
'     ! GET_ARGS(3f) retrieves the value of keywords accept for', &
'     ! two special cases. For fixed-length CHARACTER variables', &
'     ! see GET_ARGS_FIXED_LENGTH(3f). For fixed-size arrays see', &
'     ! GET_ARGS_FIXED_SIZE(3f).', &
'     !', &
'     ! allocatables should be done one at a time', &
'    call get_args(''title'',title) ! allocatable string', &
'    call get_args(''point'',point) ! allocatable arrays', &
'    call get_args(''logicals'',logicals)', &
'     !', &
'     ! less commonly ...', &
'', &
'     ! for fixed-length strings', &
'    call get_args_fixed_length(''label'',label)', &
'', &
'     ! for non-allocatable arrays', &
'    call get_args_fixed_size(''p'',p)', &
'    call get_args_fixed_size(''logi'',logi)', &
'     !', &
'     ! all done parsing, use values', &
'    write(*,*)''x='',x, ''y='',y, ''z='',z, x+y+z', &
'    write(*,*)''p='',p', &
'    write(*,*)''point='',point', &
'    write(*,*)''title='',title', &
'    write(*,*)''label='',label', &
'    write(*,*)''l='',l', &
'    write(*,*)''L='',lbig', &
'    write(*,*)''logicals='',logicals', &
'    write(*,*)''logi='',logi', &
'     !', &
'     ! unnamed strings', &
'     !', &
'    if(size(filenames) > 0)then', &
'       write(*,''(i6.6,3a)'')(i,''['',filenames(i),'']'',i=1,size(filenames))', &
'    endif', &
'     !', &
'    end program demo_M_CLI2', &
'', &
'AUTHOR', &
'    John S. Urban, 2019', &
'LICENSE', &
'    Public Domain', &
'SEE ALSO', &
'    + get_args(3f)', &
'    + get_args_fixed_size(3f)', &
'    + get_args_fixed_length(3f)', &
'    + get_subcommand(3f)', &
'    + set_mode(3f)', &
'    + specified(3f)', &
'', &
'Note that the convenience routines are described under get_args(3f):', &
'dget(3f), iget(3f), lget(3f), rget(3f), sget(3f), cget(3f) dgets(3f),', &
'igets(3f), lgets(3f), rgets(3f), sgets(3f), cgets(3f)', &
'']

shortname="M_CLI2"

call select()


case('6','set_args')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   set_args(3f) - [ARGUMENTS:M_CLI2] command line argument parsing', &
'   (LICENSE:PD)', &
'', &
'SYNOPSIS', &
'    subroutine set_args(prototype,help_text,version_text,ierr,errmsg)', &
'', &
'     character(len=*),intent(in),optional              :: prototype', &
'     character(len=*),intent(in),optional              :: help_text(:)', &
'     character(len=*),intent(in),optional              :: version_text(:)', &
'     integer,intent(out),optional                      :: ierr', &
'     character(len=:),intent(out),allocatable,optional :: errmsg', &
'DESCRIPTION', &
'', &
'   SET_ARGS(3f) requires a unix-like command prototype which defines', &
'   the command-line options and their default values. When the program', &
'   is executed this and the command-line options are applied and the', &
'   resulting values are placed in an internal table for retrieval via', &
'   GET_ARGS(3f).', &
'', &
'   The built-in --help and --version options require optional help_text', &
'   and version_text values to be provided to be particularly useful.', &
'', &
'OPTIONS', &
'', &
'   PROTOTYPE   composed of all command arguments concatenated', &
'               into a Unix-like command prototype string. For', &
'               example:', &
'', &
'                call set_args(''-L F --ints 1,2,3 --title "my title" -R 10.3'')', &
'', &
'               The following options are predefined for all commands:', &
'               ''--verbose F --usage F --help F --version F''.', &
'', &
'               see "DEFINING THE PROTOTYPE" in the next section for', &
'               further details.', &
'', &
'   HELP_TEXT   if present, will be displayed when the program is called with', &
'               a --help switch, and then the program will terminate. If', &
'               help text is not supplied the command line initialization', &
'               string will be echoed.', &
'', &
'   VERSION_TEXT  if present, any version text defined will be displayed', &
'                 when the program is called with a --version switch,', &
'                 and then the program will terminate.', &
'   IERR          if present a non-zero option is returned when an', &
'                 error occurs instead of the program terminating.', &
'   ERRMSG        a description of the error if ierr is present.', &
'', &
'DEFINING THE PROTOTYPE', &
'', &
'   o Keywords start with a single dash for short single-character', &
'     keywords, and with two dashes for longer keywords.', &
'', &
'   o all keywords on the prototype MUST get a value.', &
'', &
'      * logicals must be set to an unquoted F.', &
'', &
'      * strings must be delimited with double-quotes.', &
'        Since internal double-quotes are represented with two', &
'        double-quotes the string must be at least one space.', &
'', &
'   o numeric keywords are not allowed; but this allows', &
'     negative numbers to be used as values.', &
'', &
'   o lists of values should be comma-delimited unless a', &
'     user-specified delimiter is used. The prototype', &
'     must use the same array delimiters as the call to', &
'     get the value.', &
'', &
'   o to define a zero-length allocatable array make the', &
'     value a delimiter (usually a comma) or an empty set', &
'     of braces ("[]").', &
'', &
'   LONG AND SHORT NAMES', &
'', &
'   Long keywords start with two dashes followed by more than one letter.', &
'   Short keywords are a dash followed by a single letter.', &
'', &
'   o It is recommended long names (--keyword) should be all lowercase', &
'     but are case-sensitive by default, unless "set_mode(''ignorecase'')"', &
'     is in effect.', &
'', &
'   o Long names should always be more than one character.', &
'', &
'   o The recommended way to have short names is to suffix the long', &
'     name with :LETTER in the definition.', &
'', &
'     If this syntax is used then logical shorts may be combined on the', &
'     command line when "set_mode(''strict'')" is in effect.', &
'', &
'   SPECIAL BEHAVIORS', &
'', &
'   o A special behavior occurs if a keyword name ends in ::.', &
'     When the program is called the next parameter is taken as', &
'     a value even if it starts with -. This is not generally', &
'     recommended but is useful in rare cases where non-numeric', &
'     values starting with a dash are desired.', &
'', &
'   o If the prototype ends with "--" a special mode is turned', &
'     on where anything after "--" on input goes into the variable', &
'     REMAINING with values double-quoted and also into the array ARGS', &
'     instead of becoming elements in the UNNAMED array. This is not', &
'     needed for normal processing, but was needed for a program that', &
'     needed this behavior for its subcommands.', &
'', &
'     That is, for a normal call all unnamed values go into UNNAMED', &
'     and ARGS and REMAINING are ignored. So for', &
'', &
'         call set_args(''-x 10 -y 20 '')', &
'', &
'     A program invocation such as', &
'', &
'         xx a b c -- A B C " dd "', &
'', &
'     results in', &
'', &
'      UNNAMED= [''a'',''b'',''c'',''A'',''B'',''C'','' dd'']', &
'      REMAINING= ''''', &
'      ARGS= [character(len=0) :: ] ! ie, an empty character array', &
'', &
'     Whereas', &
'', &
'      call set_args(''-x 10 -y 20 --'')', &
'', &
'     generates the following output from the same program execution:', &
'', &
'      UNNAMED= [''a'',''b'',''c'']', &
'      REMAINING= ''"A" "B" "C" " dd "''', &
'      ARGS= [''A'',''B'',''C,'' dd'']', &
'', &
'USAGE NOTES', &
'     When invoking the program line note the (subject to change)', &
'     following restrictions (which often differ between various', &
'     command-line parsers):', &
'', &
'     o values for duplicate keywords are appended together with a space', &
'       separator when a command line is executed by default.', &
'', &
'     o shuffling is not supported. Values immediately follow their', &
'       keywords.', &
'', &
'     o Only short Boolean keywords can be bundled together.', &
'       If allowing bundling is desired call "set_mode(''strict'')".', &
'       This will require prefixing long names with "--" and short', &
'       names with "-". Otherwise M_CLI2 relaxes that requirement', &
'       and mostly does not care what prefix is used for a keyword.', &
'       But this would make it unclear what was meant by "-ox" if', &
'       allowed options were "-o F -x F --ox F " for example, so', &
'       "strict" mode is required to remove the ambiguity.', &
'', &
'     o if a parameter value of just "-" is supplied it is', &
'       converted to the string "stdin".', &
'', &
'     o values not needed for a keyword value go into the character', &
'       array "UNNAMED".', &
'', &
'       In addition if the keyword "--" is encountered on the command', &
'       line the rest of the command line goes into the character array', &
'       "UNNAMED".', &
'', &
'EXAMPLE', &
'', &
'Sample program:', &
'', &
'    program demo_set_args', &
'    use M_CLI2,  only : filenames=>unnamed, set_args, get_args', &
'    use M_CLI2,  only : get_args_fixed_size', &
'    implicit none', &
'    integer                      :: i', &
'    ! DEFINE ARGS', &
'    real                         :: x, y, z', &
'    real                         :: p(3)', &
'    character(len=:),allocatable :: title', &
'    logical                      :: l, lbig', &
'    integer,allocatable          :: ints(:)', &
'    !', &
'    !  DEFINE COMMAND (TO SET INITIAL VALUES AND ALLOWED KEYWORDS)', &
'    !  AND READ COMMAND LINE', &
'    call set_args('' &', &
'       ! reals', &
'       & -x 1 -y 2.3 -z 3.4e2 &', &
'       ! integer array', &
'       & -p -1,-2,-3 &', &
'       ! always double-quote strings', &
'       & --title "my title" &', &
'       ! string should be a single character at a minimum', &
'       & --label " ", &', &
'       ! set all logical values to F', &
'       & -l F -L F &', &
'       ! set allocatable size to zero if you like by using a delimiter', &
'       & --ints , &', &
'       & '')', &
'    ! ASSIGN VALUES TO ELEMENTS', &
'    !     SCALARS', &
'    call get_args(''x'',x)', &
'    call get_args(''y'',y)', &
'    call get_args(''z'',z)', &
'    call get_args(''l'',l)', &
'    call get_args(''L'',lbig)', &
'    call get_args(''ints'',ints)      ! ALLOCATABLE ARRAY', &
'    call get_args(''title'',title)    ! ALLOCATABLE STRING', &
'    call get_args_fixed_size(''p'',p) ! NON-ALLOCATABLE ARRAY', &
'    ! USE VALUES', &
'    write(*,*)''x='',x', &
'    write(*,*)''y='',y', &
'    write(*,*)''z='',z', &
'    write(*,*)''p='',p', &
'    write(*,*)''title='',title', &
'    write(*,*)''ints='',ints', &
'    write(*,*)''l='',l', &
'    write(*,*)''L='',lbig', &
'    ! UNNAMED VALUES', &
'    if(size(filenames) > 0)then', &
'       write(*,''(i6.6,3a)'')(i,''['',filenames(i),'']'',i=1,size(filenames))', &
'    endif', &
'    end program demo_set_args', &
'', &
'RESPONSE FILES', &
'', &
' If you have no interest in using external files as abbreviations', &
' you can ignore this section. Otherwise, before calling set_args(3f)', &
' add:', &
'', &
'    use M_CLI2, only : set_mode', &
'    call set_mode(''response_file'')', &
'', &
' M_CLI2 Response files are small files containing CLI (Command Line', &
' Interface) arguments that end with ".rsp" that can be used when command', &
' lines are so long that they would exceed line length limits or so complex', &
' that it is useful to have a platform-independent method of creating', &
' an abbreviation.', &
'', &
' Shell aliases and scripts are often used for similar purposes (and', &
' allow for much more complex conditional execution, of course), but', &
' they generally cannot be used to overcome line length limits and are', &
' typically platform-specific.', &
'', &
' Examples of commands that support similar response files are the Clang', &
' and Intel compilers, although there is no standard format for the files.', &
'', &
' They are read if you add options of the syntax "@NAME" as the FIRST', &
' parameters on your program command line calls. They are not recursive --', &
' that is, an option in a response file cannot be given the value "@NAME2"', &
' to call another response file.', &
'', &
' More than one response name may appear on a command line.', &
'', &
' They are case-sensitive names.', &
'', &
' Note "@" s a special character in Powershell, and requires being escaped', &
' with a grave character.', &
'', &
'  LOCATING RESPONSE FILES', &
'', &
' A search for the response file always starts with the current directory.', &
' The search then proceeds to look in any additional directories specified', &
' with the colon-delimited environment variable CLI_RESPONSE_PATH.', &
'', &
' The first resource file found that results in lines being processed', &
' will be used and processing stops after that first match is found. If', &
' no match is found an error occurs and the program is stopped.', &
'', &
'  RESPONSE FILE SECTIONS', &
'', &
' A simple response file just has options for calling the program in it', &
' prefixed with the word "options".', &
' But they can also contain section headers to denote selections that are', &
' only executed when a specific OS is being used, print messages, and', &
' execute system commands.', &
'', &
'  SEARCHING FOR OSTYPE IN REGULAR FILES', &
'', &
' So assuming the name @NAME was specified on the command line a file', &
' named NAME.rsp will be searched for in all the search directories', &
' and then in that file a string that starts with the string @OSTYPE', &
' (if the environment variables $OS and $OSTYPE are not blank. $OSTYPE', &
' takes precedence over $OS).', &
'', &
'  SEARCHING FOR UNLABELED DIRECTIVES IN REGULAR FILES', &
'', &
' Then, the same files will be searched for lines above any line starting', &
' with "@". That is, if there is no special section for the current OS', &
' it just looks at the top of the file for unlabeled options.', &
'', &
'  SEARCHING FOR OSTYPE AND NAME IN THE COMPOUND FILE', &
'', &
' In addition or instead of files with the same name as the @NAME option', &
' on the command line, you can have one file named after the executable', &
' name that contains multiple abbreviation names.', &
'', &
' So if your program executable is named EXEC you create a single file', &
' called EXEC.rsp and can append all the simple files described above', &
' separating them with lines of the form @OSTYPE@NAME or just @NAME.', &
'', &
' So if no specific file for the abbreviation is found a file called', &
' "EXEC.rsp" is searched for where "EXEC" is the name of the executable.', &
' This file is always a "compound" response file that uses the following format:', &
'', &
' Any compound EXEC.rsp file found in the current or searched directories', &
' will be searched for the string @OSTYPE@NAME first.', &
'', &
' Then if nothing is found, the less specific line @NAME is searched for.', &
'', &
'  THE SEARCH IS OVER', &
'', &
' Sounds complicated but actually works quite intuitively. Make a file in', &
' the current directory and put options in it and it will be used. If that', &
' file ends up needing different cases for different platforms add a line', &
' like "@Linux" to the file and some more lines and that will only be', &
' executed if the environment variable OSTYPE or OS is "Linux". If no match', &
' is found for named sections the lines at the top before any "@" lines', &
' will be used as a default if no match is found.', &
'', &
' If you end up using a lot of files like this you can combine them all', &
' together and put them into a file called "program_name".rsp and just', &
' put lines like @NAME or @OSTYPE@NAME at that top of each selection.', &
'', &
' Now, back to the details on just what you can put in the files.', &
'', &
'SPECIFICATION FOR RESPONSE FILES', &
'', &
'  SIMPLE RESPONSE FILES', &
'', &
' The first word of a line is special and has the following meanings:', &
'', &
'   options|-  Command options following the rules of the SET_ARGS(3f)', &
'              prototype. So', &
'               o It is preferred to specify a value for all options.', &
'               o double-quote strings.', &
'               o give a blank string value as " ".', &
'               o use F|T for lists of logicals,', &
'               o lists of numbers should be comma-delimited.', &
'               o --usage, --help, --version, --verbose, and unknown', &
'                 options are ignored.', &
'', &
'   comment|#  Line is a comment line', &
'   system|!   System command.', &
'              System commands are executed as a simple call to', &
'              system (so a cd(1) or setting a shell variable', &
'              would not effect subsequent lines, for example)', &
'              BEFORE the command being processed.', &
'   print|>    Message to screen', &
'   stop       display message and stop program.', &
'', &
' NOTE: system commands are executed when encountered, but options are', &
' gathered from multiple option lines and passed together at the end of', &
' processing of the block; so all commands will be executed BEFORE the', &
' command for which options are being supplied no matter where they occur.', &
'', &
' So if a program that does nothing but echos its parameters', &
'', &
'   program testit', &
'   use M_CLI2, only : set_args, rget, sget, lget, set_mode', &
'   implicit none', &
'      real :: x,y                           ; namelist/args/ x,y', &
'      character(len=:),allocatable :: title ; namelist/args/ title', &
'      logical :: big                        ; namelist/args/ big', &
'      call set_mode(''response_file'')', &
'      call set_args(''-x 10.0 -y 20.0 --title "my title" --big F'')', &
'      x=rget(''x'')', &
'      y=rget(''y'')', &
'      title=sget(''title'')', &
'      big=lget(''big'')', &
'      write(*,nml=args)', &
'   end program testit', &
'', &
' And a file in the current directory called "a.rsp" contains', &
'', &
'    # defaults for project A', &
'    options -x 1000 -y 9999', &
'    options --title " "', &
'    options --big T', &
'', &
' The program could be called with', &
'', &
'    $myprog     # normal call', &
'     X=10.0 Y=20.0 TITLE="my title"', &
'', &
'    $myprog @a  # change defaults as specified in "a.rsp"', &
'    X=1000.0 Y=9999.0 TITLE=" "', &
'', &
'    # change defaults but use any option as normal to override defaults', &
'    $myprog @a -y 1234', &
'     X=1000.0 Y=1234.0 TITLE=" "', &
'', &
'  COMPOUND RESPONSE FILES', &
'', &
' A compound response file has the same basename as the executable with a', &
' ".rsp" suffix added. So if your program is named "myprg" the filename', &
' must be "myprg.rsp".', &
'', &
'   Note that here `basename` means the last leaf of the', &
'   name of the program as returned by the Fortran intrinsic', &
'   GET_COMMAND_ARGUMENT(0,...) trimmed of anything after a period ("."),', &
'   so it is a good idea not to use hidden files.', &
'', &
' Unlike simple response files compound response files can contain multiple', &
' setting names.', &
'', &
' Specifically in a compound file', &
' if the environment variable $OSTYPE (first) or $OS is set the first search', &
' will be for a line of the form (no leading spaces should be used):', &
'', &
'   @OSTYPE@alias_name', &
'', &
' If no match or if the environment variables $OSTYPE and $OS were not', &
' set or a match is not found then a line of the form', &
'', &
'   @alias_name', &
'', &
' is searched for in simple or compound files. If found subsequent lines', &
' will be ignored that start with "@" until a line not starting with', &
' "@" is encountered. Lines will then be processed until another line', &
' starting with "@" is found or end-of-file is encountered.', &
'', &
'  COMPOUND RESPONSE FILE EXAMPLE', &
' An example compound file', &
'', &
'   #################', &
'   @if', &
'   > RUNNING TESTS USING RELEASE VERSION AND ifort', &
'   options test --release --compiler ifort', &
'   #################', &
'   @gf', &
'   > RUNNING TESTS USING RELEASE VERSION AND gfortran', &
'   options test --release --compiler gfortran', &
'   #################', &
'   @nv', &
'   > RUNNING TESTS USING RELEASE VERSION AND nvfortran', &
'   options test --release --compiler nvfortran', &
'   #################', &
'   @nag', &
'   > RUNNING TESTS USING RELEASE VERSION AND nagfor', &
'   options test --release --compiler nagfor', &
'   #', &
'   #################', &
'   # OS-specific example:', &
'   @Linux@install', &
'   #', &
'   # install executables in directory (assuming install(1) exists)', &
'   #', &
'   system mkdir -p ~/.local/bin', &
'   options run --release T --runner "install -vbp -m 0711 -t ~/.local/bin"', &
'   @install', &
'   STOP INSTALL NOT SUPPORTED ON THIS PLATFORM OR $OSTYPE NOT SET', &
'   #', &
'   #################', &
'   @fpm@testall', &
'   #', &
'   !fpm test --compiler nvfortran', &
'   !fpm test --compiler ifort', &
'   !fpm test --compiler gfortran', &
'   !fpm test --compiler nagfor', &
'   STOP tests complete. Any additional parameters were ignored', &
'   #################', &
'', &
' Would be used like', &
'', &
'   fpm @install', &
'   fpm @nag --', &
'   fpm @testall', &
'', &
'  NOTES', &
'', &
'   The intel Fortran compiler now calls the response files "indirect', &
'   files" and does not add the implied suffix ".rsp" to the files', &
'   anymore. It also allows the @NAME syntax anywhere on the command line,', &
'   not just at the beginning. -- 20201212', &
'', &
'AUTHOR', &
'     John S. Urban, 2019', &
'', &
'LICENSE', &
'     Public Domain', &
'']

shortname="set_args"

call select()


case('7','set_mode')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   set_mode(3f) - [ARGUMENTS:M_CLI2] turn on optional modes', &
'   (LICENSE:PD)', &
'', &
'SYNOPSIS', &
'   subroutine set_mode(key,mode)', &
'', &
'    character(len=*),intent(in) :: key', &
'    logical,intent(in),optional :: mode', &
'', &
'DESCRIPTION', &
'    Allow optional behaviors.', &
'', &
'OPTIONS', &
'   KEY    name of option', &
'', &
'   The following values are allowed:', &
'', &
'   o  response_file - enable use of response file', &
'', &
'   o  ignorecase - ignore case in long key names. So the user', &
'      does not have to remember if the option is --IgnoreCase', &
'      or --ignorecase or --ignoreCase', &
'', &
'   o  underdash  - treat dash in keyword as an underscore.', &
'      So the user should not have to remember if the option is', &
'      --ignore_case or --ignore-case.', &
'', &
'   o  strict - allow Boolean keys to be bundled, but requires', &
'      a single dash prefix be used for short key names and', &
'      long names must be prefixed with two dashes.', &
'', &
'   o  lastonly  - when multiple keywords occur keep the rightmost', &
'      value specified instead of appending the values together.', &
'', &
'   MODE   set to .true. to activate the optional mode.', &
'          Set to .false. to deactivate the mode.', &
'          It is .true. by default.', &
'', &
'EXAMPLE', &
'Sample program:', &
'', &
'   program demo_set_mode', &
'   use M_CLI2,  only : set_args, lget, set_mode', &
'   implicit none', &
'   character(len=*),parameter :: all=''(*(g0))''', &
'      !', &
'      ! enable use of response files', &
'      call set_mode(''response_file'')', &
'      !', &
'      ! Any dash in a keyword is treated as an underscore', &
'      call set_mode(''underdash'')', &
'      !', &
'      ! The case of long keywords are ignored.', &
'      ! Values and short names remain case-sensitive', &
'      call set_mode(''ignorecase'')', &
'      !', &
'      ! short single-character boolean keys may be bundled', &
'      ! but it is required that a single dash is used for', &
'      ! short keys and a double dash for long keywords.', &
'      call set_mode(''strict'')', &
'      !', &
'      call set_args('' --switch_X:X F --switch-Y:Y F --ox:O F -t F -x F -o F'')', &
'      !', &
'      print all,''--switch_X or -X ... '',lget(''switch_X'')', &
'      print all,''--switch_Y or -Y ... '',lget(''switch_Y'')', &
'      print all,''--ox or -O       ... '',lget(''ox'')', &
'      print all,''-o               ... '',lget(''o'')', &
'      print all,''-x               ... '',lget(''x'')', &
'      print all,''-t               ... '',lget(''t'')', &
'   end program demo_set_mode', &
'', &
'AUTHOR', &
'     John S. Urban, 2019', &
'LICENSE', &
'     Public Domain', &
'']

shortname="set_mode"

call select()


case('8','specified')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   specified(3f) - [ARGUMENTS:M_CLI2] return true if keyword was present', &
'   on command line', &
'   (LICENSE:PD)', &
'', &
'SYNOPSIS', &
'   elemental impure function specified(name)', &
'', &
'    character(len=*),intent(in) :: name', &
'    logical :: specified', &
'', &
'DESCRIPTION', &
'', &
'   specified(3f) returns .true. if the specified keyword was present on', &
'   the command line.', &
'', &
'   M_CLI2 intentionally does not have validators except for SPECIFIED(3f)', &
'   and of course a check whether the input conforms to the type when', &
'   requesting a value (with get_args(3f) or the convenience functions', &
'   like inum(3f)).', &
'', &
'   Fortran already has powerful validation capabilities. Logical', &
'   expressions ANY(3f) and ALL(3f) are standard Fortran features which', &
'   easily allow performing the common validations for command line', &
'   arguments without having to learn any additional syntax or methods.', &
'', &
'OPTIONS', &
'', &
'   NAME   name of commandline argument to query the presence of. Long', &
'          names should always be used.', &
'', &
'RETURNS', &
'   SPECIFIED  returns .TRUE. if specified NAME was present on the command', &
'              line when the program was invoked.', &
'', &
'EXAMPLE', &
'Sample program:', &
'', &
'   program demo_specified', &
'   use, intrinsic :: iso_fortran_env, only : &', &
'   & stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT', &
'   use M_CLI2,  only : set_args, igets, rgets, specified, sget, lget', &
'   implicit none', &
'', &
'   ! Define args', &
'   integer,allocatable  :: ints(:)', &
'   real,allocatable     :: floats(:)', &
'   logical              :: flag', &
'   character(len=:),allocatable :: color', &
'   character(len=:),allocatable :: list(:)', &
'   integer :: i', &
'', &
'    call set_args(''&', &
'       & --color:c "red"       &', &
'       & --flag:f F            &', &
'       & --ints:i 1,10,11      &', &
'       & --floats:T 12.3, 4.56 &', &
'       & '')', &
'    ints=igets(''ints'')', &
'    floats=rgets(''floats'')', &
'    flag=lget(''flag'')', &
'    color=sget(''color'')', &
'', &
'    write(*,*)''color='',color', &
'    write(*,*)''flag='',flag', &
'    write(*,*)''ints='',ints', &
'    write(*,*)''floats='',floats', &
'', &
'    write(*,*)''was -flag specified?'',specified(''flag'')', &
'', &
'    ! elemental', &
'    write(*,*)specified([''floats'',''ints  ''])', &
'', &
'    ! If you want to know if groups of parameters were specified use', &
'    ! ANY(3f) and ALL(3f)', &
'    write(*,*)''ANY:'',any(specified([''floats'',''ints  '']))', &
'    write(*,*)''ALL:'',all(specified([''floats'',''ints  '']))', &
'', &
'    ! For mutually exclusive', &
'    if (all(specified([''floats'',''ints  ''])))then', &
'        write(*,*)''You specified both names --ints and --floats''', &
'    endif', &
'', &
'    ! For required parameter', &
'    if (.not.any(specified([''floats'',''ints  ''])))then', &
'        write(*,*)''You must specify --ints or --floats''', &
'    endif', &
'', &
'   ! check if all values are in range from 10 to 30 and even', &
'   write(*,*)''are all numbers good?'',all([ints>=10,ints<= 30,(ints/2)*2==ints])',&
'', &
'   ! perhaps you want to check one value at a time', &
'   do i=1,size(ints)', &
'      write(*,*)ints(i),[ints(i) >= 10,ints(i) <= 30,(ints(i)/2)*2 == ints(i)]', &
'      if(all([ints(i) >= 10,ints(i) <= 30,(ints(i)/2)*2 == ints(i)]) )then', &
'         write(*,*)ints(i),''is an even number from 10 to 30 inclusive''', &
'      else', &
'         write(*,*)ints(i),''is not an even number from 10 to 30 inclusive''', &
'      endif', &
'   enddo', &
'', &
'   list = [character(len=10) :: ''red'',''white'',''blue'']', &
'   if( any(color == list) )then', &
'      write(*,*)color,''matches a value in the list''', &
'   else', &
'      write(*,*)color,''not in the list''', &
'   endif', &
'', &
'   if(size(ints).eq.3)then', &
'      write(*,*)''ints(:) has expected number of values''', &
'   else', &
'      write(*,*)''ints(:) does not have expected number of values''', &
'   endif', &
'', &
'   end program demo_specified', &
'', &
'Default output', &
'', &
' > color=red', &
' > flag= F', &
' > ints=           1          10          11', &
' > floats=   12.3000002       4.55999994', &
' > was -flag specified? F', &
' > F F', &
' > ANY: F', &
' > ALL: F', &
' > You must specify --ints or --floats', &
' >           1 F T F', &
' >           1  is not an even number from 10 to 30 inclusive', &
' >          10 T T T', &
' >          10  is an even number from 10 to 30 inclusive', &
' >          11 T T F', &
' >          11  is not an even number from 10 to 30 inclusive', &
' > red matches a value in the list', &
' > ints(:) has expected number of values', &
'', &
'AUTHOR', &
'     John S. Urban, 2019', &
'LICENSE', &
'     Public Domain', &
'']

shortname="specified"

call select()

case default
   allocate (character(len=256) :: textblock(0))
end select
contains
subroutine select()
if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif
end subroutine select
end function help_intrinsics_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_name(lines)
!@(#) sort_name(3fp):sort strings(a-z) over specified field using shell sort starting with [ character
character(len = *)                :: lines(:)
   character(len = :),allocatable :: ihold
   integer                        :: n, igap, i, j, k, jg
   n = size(lines)
   if(n.gt.0)then
      allocate(character(len = len(lines(1))) :: ihold)
   else
      ihold = ''
   endif
   igap = n
   INFINITE: do
      igap = igap/2
      if(igap.eq.0) exit INFINITE
      k = n-igap
      i = 1
      INNER: do
         j = i
         INSIDE: do
            jg = j+igap
            if( lle( lower(lines(j)), lower(lines(jg)) ) )exit INSIDE
            ihold = lines(j)
            lines(j) = lines(jg)
            lines(jg) = ihold
            j = j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i = i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
elemental pure function lower(str) result (string)
!@(#) ${PACKAGE}::lower(3f): Changes a string to lowercase over specified range
character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   do i = 1, len_trim(str)     ! step thru each letter in the string
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32) ! change letter to miniscule
      case default
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!elemental pure function compact(str,char) result (outstr)
function compact(str,char) result (outstr)

!$@(#) ${PACKAGE}::compact(3f): Converts white-space to single spaces; removes leading spaces

character(len=*),intent(in)          :: str
character(len=*),intent(in),optional :: char
character(len=len(str))              :: outstr
character(len=1)                     :: ch
integer                              :: i
integer                              :: position_in_output
logical                              :: last_was_space
character(len=1)                     :: char_p
logical                              :: nospace
if(present(char))then
   char_p=char
   if(len(char).eq.0)then
      nospace=.true.
   else
      nospace=.false.
   endif
else
   char_p=' '
   nospace=.false.
endif
   outstr=' '
   last_was_space=.false.
   position_in_output=0

   IFSPACE: do i=1,len_trim(str)
     ch=str(i:i)
     select case(iachar(ch))
       case(0:32,127)                                         ! space or tab character or control character
         if(position_in_output.eq.0)then                      ! still at beginning so ignore leading whitespace
            cycle IFSPACE
         elseif(.not.last_was_space) then                     ! if have not already put out a space output one
           if(.not.nospace)then
              position_in_output=position_in_output+1
              outstr(position_in_output:position_in_output)=char_p
           endif
         endif
         last_was_space=.true.
       case(:-1,33:126,128:)                                  ! not a space, quote, or control character so copy it
         position_in_output=position_in_output+1
         outstr(position_in_output:position_in_output)=ch
         last_was_space=.false.
     end select
   enddo IFSPACE

end function compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_CLI2_docs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================


!>>>>> ././src/M_CLI2.F90
!VERSION 1.0 20200115
!VERSION 2.0 20200802
!VERSION 3.0 20201021  LONG:SHORT syntax
!VERSION 3.1 20201115  LONG:SHORT:: syntax
!VERSION 3.2 20230205  set_mode()
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    M_CLI2(3fm) - [ARGUMENTS::M_CLI2::INTRO] command line argument
!!    parsing using a prototype command
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   Available procedures and variables:
!!
!!      ! basic procedures
!!      use M_CLI2, only : set_args, get_args, specified, set_mode
!!      ! convenience functions
!!      use M_CLI2, only : dget, iget, lget, rget, sget, cget
!!      use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets
!!      ! variables
!!      use M_CLI2, only : unnamed, remaining, args
!!      ! working with non-allocatable strings and arrays
!!      use M_CLI2, only : get_args_fixed_length, get_args_fixed_size
!!      ! special function for creating subcommands
!!      use M_CLI2, only : get_subcommand(3f)
!!
!!##DESCRIPTION
!!    The M_CLI2 module cracks a Unix-style command line.
!!
!!    Typically one call to SET_ARGS(3f) is made to define the command
!!    arguments, set default values and parse the command line. Then a call
!!    is made to the convenience procedures or GET_ARGS(3f) proper for each
!!    command keyword to obtain the argument values.
!!
!!    Detailed descriptions of each procedure and example programs are
!!    included.
!!
!!##EXAMPLE
!!
!!
!! Sample minimal program which may be called in various ways:
!!
!!     mimimal -x 100.3 -y 3.0e4
!!     mimimal --xvalue=300 --debug
!!     mimimal --yvalue 400
!!     mimimal -x 10 file1 file2 file3
!!
!! Program example:
!!
!!     program minimal
!!     use M_CLI2,  only : set_args, lget, rget, sgets
!!     implicit none
!!     real    :: x, y
!!     integer :: i
!!     character(len=:),allocatable :: filenames(:)
!!        ! define and crack command line
!!        call set_args(' --yvalue:y 0.0 --xvalue:x 0.0 --debug F')
!!        ! get values
!!        x=rget('xvalue')
!!        y=rget('yvalue')
!!        if(lget('debug'))then
!!           write(*,*)'X=',x
!!           write(*,*)'Y=',y
!!           write(*,*)'ATAN2(Y,X)=',atan2(x=x,y=y)
!!        else
!!           write(*,*)atan2(x=x,y=y)
!!        endif
!!        filenames=sgets() ! sget with no name gets "unnamed" values
!!        if(size(filenames) > 0)then
!!           write(*,'(g0)')'filenames:'
!!           write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!        endif
!!     end program minimal
!!
!! Sample program using get_args() and variants
!!
!!     program demo_M_CLI2
!!     use M_CLI2,  only : set_args, get_args
!!     use M_CLI2,  only : filenames=>unnamed
!!     use M_CLI2,  only : get_args_fixed_length, get_args_fixed_size
!!     implicit none
!!     integer                      :: i
!!     integer,parameter            :: dp=kind(0.0d0)
!!      !
!!      ! Define ARGS
!!     real                         :: x, y, z
!!     logical                      :: l, lbig
!!     character(len=40)            :: label    ! FIXED LENGTH
!!     real(kind=dp),allocatable    :: point(:)
!!     logical,allocatable          :: logicals(:)
!!     character(len=:),allocatable :: title    ! VARIABLE LENGTH
!!     real                         :: p(3)     ! FIXED SIZE
!!     logical                      :: logi(3)  ! FIXED SIZE
!!      !
!!      ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!      !   o set a value for all keywords.
!!      !   o double-quote strings, strings must be at least one space
!!      !     because adjacent double-quotes designate a double-quote
!!      !     in the value.
!!      !   o set all logical values to F
!!      !   o numeric values support an "e" or "E" exponent
!!      !   o for lists delimit with a comma, colon, or space
!!     call set_args('                         &
!!             & -x 1 -y 2 -z 3                &
!!             & -p -1 -2 -3                   &
!!             & --point 11.11, 22.22, 33.33e0 &
!!             & --title "my title" -l F -L F  &
!!             & --logicals  F F F F F         &
!!             & --logi F T F                  &
!!             & --label " " &
!!             ! note space between quotes is required
!!             & ')
!!      ! Assign values to elements using G_ARGS(3f).
!!      ! non-allocatable scalars can be done up to twenty per call
!!     call get_args('x',x, 'y',y, 'z',z, 'l',l, 'L',lbig)
!!      ! As a convenience multiple pairs of keywords and variables may be
!!      ! specified if and only if all the values are scalars and the CHARACTER
!!      ! variables are fixed-length or pre-allocated.
!!      !
!!      ! After SET_ARGS(3f) has parsed the command line
!!      ! GET_ARGS(3f) retrieves the value of keywords accept for
!!      ! two special cases. For fixed-length CHARACTER variables
!!      ! see GET_ARGS_FIXED_LENGTH(3f). For fixed-size arrays see
!!      ! GET_ARGS_FIXED_SIZE(3f).
!!      !
!!      ! allocatables should be done one at a time
!!     call get_args('title',title) ! allocatable string
!!     call get_args('point',point) ! allocatable arrays
!!     call get_args('logicals',logicals)
!!      !
!!      ! less commonly ...
!!
!!      ! for fixed-length strings
!!     call get_args_fixed_length('label',label)
!!
!!      ! for non-allocatable arrays
!!     call get_args_fixed_size('p',p)
!!     call get_args_fixed_size('logi',logi)
!!      !
!!      ! all done parsing, use values
!!     write(*,*)'x=',x, 'y=',y, 'z=',z, x+y+z
!!     write(*,*)'p=',p
!!     write(*,*)'point=',point
!!     write(*,*)'title=',title
!!     write(*,*)'label=',label
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     write(*,*)'logicals=',logicals
!!     write(*,*)'logi=',logi
!!      !
!!      ! unnamed strings
!!      !
!!     if(size(filenames) > 0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!      !
!!     end program demo_M_CLI2
!!
!!##AUTHOR
!!     John S. Urban, 2019
!!##LICENSE
!!     Public Domain
!!##SEE ALSO
!!     + get_args(3f)
!!     + get_args_fixed_size(3f)
!!     + get_args_fixed_length(3f)
!!     + get_subcommand(3f)
!!     + set_mode(3f)
!!     + specified(3f)
!!
!! Note that the convenience routines are described under get_args(3f):
!! dget(3f), iget(3f), lget(3f), rget(3f), sget(3f), cget(3f) dgets(3f),
!! igets(3f), lgets(3f), rgets(3f), sgets(3f), cgets(3f)
!===================================================================================================================================
module M_CLI2
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT, warn=>OUTPUT_UNIT
implicit none
private

integer,parameter,private :: dp=kind(0.0d0)
integer,parameter,private :: sp=kind(0.0)

character(len=*),parameter          :: gen='(*(g0))'
character(len=:),allocatable,public :: unnamed(:)
character(len=:),allocatable,public :: args(:)
character(len=:),allocatable,public :: remaining
public                              :: set_mode
public                              :: set_args
public                              :: get_subcommand
public                              :: get_args
public                              :: get_args_fixed_size
public                              :: get_args_fixed_length
public                              :: specified
public                              :: print_dictionary

public                              :: dget, iget, lget, rget, sget, cget
public                              :: dgets, igets, lgets, rgets, sgets, cgets

type option
   character(:),allocatable :: shortname
   character(:),allocatable :: longname
   character(:),allocatable :: value
   integer                  :: length
   logical                  :: present_in
   logical                  :: mandatory
end type option

character(len=:),allocatable,save :: keywords(:)
character(len=:),allocatable,save :: shorts(:)
character(len=:),allocatable,save :: values(:)
integer,allocatable,save          :: counts(:)
logical,allocatable,save          :: present_in(:)
logical,allocatable,save          :: mandatory(:)

logical,save                      :: G_DEBUG=.false.
logical,save                      :: G_UNDERDASH=.false.
logical,save                      :: G_NOSEPARATOR=.false.
logical,save                      :: G_IGNORECASE=.false.      ! ignore case of long keywords
logical,save                      :: G_STRICT=.false.          ! strict short and long rules or allow -longname and --shortname
logical,save                      :: G_APPEND=.true.           ! whether to append or replace when duplicate keywords found

logical,save                      :: G_keyword_single_letter=.true.
character(len=:),allocatable,save :: G_passed_in
logical,save                      :: G_remaining_on, G_remaining_option_allowed
character(len=:),allocatable,save :: G_remaining
character(len=:),allocatable,save :: G_subcommand              ! possible candidate for a subcommand
character(len=:),allocatable,save :: G_STOP_MESSAGE
integer,save                      :: G_STOP
logical,save                      :: G_QUIET
character(len=:),allocatable,save :: G_PREFIX

! try out response files
! CLI_RESPONSE_FILE is left public for backward compatibility, but should be set via "set_mode('response_file')
logical,save,public               :: CLI_RESPONSE_FILE=.false. ! allow @name abbreviations
logical,save                      :: G_OPTIONS_ONLY            ! process response file only looking for options for get_subcommand()
logical,save                      :: G_RESPONSE                ! allow @name abbreviations
character(len=:),allocatable,save :: G_RESPONSE_IGNORED

! return allocatable arrays
interface  get_args;  module  procedure  get_anyarray_d;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_i;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_r;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_x;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_c;  end interface  ! any size array and any length
interface  get_args;  module  procedure  get_anyarray_l;  end interface  ! any size array

! return scalars
interface  get_args;  module  procedure  get_scalar_d;               end interface
interface  get_args;  module  procedure  get_scalar_i;               end interface
interface  get_args;  module  procedure  get_scalar_real;            end interface
interface  get_args;  module  procedure  get_scalar_complex;         end interface
interface  get_args;  module  procedure  get_scalar_logical;         end interface
interface  get_args;  module  procedure  get_scalar_anylength_c;     end interface  ! any length

! multiple scalars
interface  get_args;  module  procedure  many_args;               end  interface

! return non-allocatable arrays
! said in conflict with get_args_*. Using class to get around that.
! that did not work either. Adding size parameter as optional parameter works; but using a different name
interface  get_args_fixed_size;  module procedure get_fixedarray_class;            end interface ! any length, fixed size array
!interface   get_args;           module procedure get_fixedarray_d;                end interface
!interface   get_args;           module procedure get_fixedarray_i;                end interface
!interface   get_args;           module procedure get_fixedarray_r;                end interface
!interface   get_args;           module procedure get_fixedarray_l;                end interface
!interface   get_args;           module procedure get_fixedarray_fixed_length_c;   end interface

interface   get_args_fixed_length;  module  procedure  get_args_fixed_length_a_array; end interface  ! fixed length any size array
interface   get_args_fixed_length;  module  procedure  get_args_fixed_length_scalar_c;  end interface       ! fixed length

! Generic subroutine inserts element into allocatable array at specified position

! find PLACE in sorted character array where value can be found or should be placed
interface  locate_;  module procedure locate_c                            ; end interface

! insert entry into a sorted allocatable array at specified position
interface  insert_;  module procedure insert_c,      insert_i,  insert_l  ; end interface

! replace entry by index from a sorted allocatable array if it is present
interface  replace_; module procedure replace_c,     replace_i, replace_l ; end interface

! delete entry by index from a sorted allocatable array if it is present
interface  remove_;  module procedure remove_c,      remove_i,  remove_l  ; end interface

! convenience functions
interface cgets;module procedure cgs, cg;end interface
interface dgets;module procedure dgs, dg;end interface
interface igets;module procedure igs, ig;end interface
interface lgets;module procedure lgs, lg;end interface
interface rgets;module procedure rgs, rg;end interface
interface sgets;module procedure sgs, sg;end interface

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    check_commandline(3f) - [ARGUMENTS:M_CLI2]check command and process
!!    pre-defined options
!!
!!##SYNOPSIS
!!
!!      subroutine check_commandline(help_text,version_text,ierr,errmsg)
!!
!!       character(len=*),intent(in),optional :: help_text(:)
!!       character(len=*),intent(in),optional :: version_text(:)
!!
!!##DESCRIPTION
!!     Checks the commandline  and processes the implicit --help, --version,
!!     --verbose, and --usage parameters.
!!
!!     If the optional text values are supplied they will be displayed by
!!     --help and --version command-line options, respectively.
!!
!!##OPTIONS
!!
!!     HELP_TEXT     if present, will be displayed if program is called with
!!                   --help switch, and then the program will terminate. If
!!                   not supplied, the command line initialized string will be
!!                   shown when --help is used on the commandline.
!!
!!     VERSION_TEXT  if present, will be displayed if program is called with
!!                   --version switch, and then the program will terminate.
!!
!!        If the first four characters of each line are "@(#)" this prefix
!!        will not be displayed and the last non-blank letter will be
!!        removed from each line. This if for support of the SCCS what(1)
!!        command. If you do not have the what(1) command on GNU/Linux and
!!        Unix platforms you can probably see how it can be used to place
!!        metadata in a binary by entering:
!!
!!         strings demo_commandline|grep '@(#)'|tr '>' '\n'|sed -e 's/  */ /g'
!!
!!##EXAMPLE
!!
!!
!! Typical usage:
!!
!!      program check_commandline
!!      use M_CLI2,  only : unnamed, set_args, get_args
!!      implicit none
!!      integer                      :: i
!!      character(len=:),allocatable :: version_text(:), help_text(:)
!!      real               :: x, y, z
!!      character(len=*),parameter :: cmd='-x 1 -y 2 -z 3'
!!         version_text=[character(len=80) :: "version 1.0","author: me"]
!!         help_text=[character(len=80) :: &
!!                 & "wish I put instructions","here","I suppose?"]
!!         call set_args(cmd,help_text,version_text)
!!         call get_args('x',x,'y',y,'z',z)
!!         ! All done cracking the command line. Use the values in your program.
!!         write (*,*)x,y,z
!!         ! the optional unnamed values on the command line are
!!         ! accumulated in the character array "UNNAMED"
!!         if(size(unnamed) > 0)then
!!            write (*,'(a)')'files:'
!!            write (*,'(i6.6,3a)') (i,'[',unnamed(i),']',i=1,size(unnamed))
!!         endif
!!      end program check_commandline
!===================================================================================================================================
subroutine check_commandline(help_text,version_text)
character(len=*),intent(in),optional :: help_text(:)
character(len=*),intent(in),optional :: version_text(:)
character(len=:),allocatable         :: line
integer                              :: i
integer                              :: istart
integer                              :: iback
   if(get('usage') == 'T')then
      call print_dictionary('USAGE:')
      call mystop(32)
      return
   endif
   if(present(help_text))then
      if(get('help') == 'T')then
         do i=1,size(help_text)
            call journal(help_text(i))
         enddo
         call mystop(1,'displayed help text')
         return
      endif
   elseif(get('help') == 'T')then
      call default_help()
      call mystop(2,'displayed default help text')
      return
   endif
   if(present(version_text))then
      if(get('version') == 'T')then
         istart=1
         iback=0
         if(size(version_text) > 0)then
            if(index(version_text(1),'@'//'(#)') == 1)then ! allow for what(1) syntax
               istart=5
               iback=1
            endif
         endif
         do i=1,size(version_text)
            !xINTEL BUG*!call journal(version_text(i)(istart:len_trim(version_text(i))-iback))
            line=version_text(i)(istart:len_trim(version_text(i))-iback)
            call journal(line)
         enddo
         call mystop(3,'displayed version text')
         return
      endif
   elseif(get('version') == 'T')then

      if(G_QUIET)then
         G_STOP_MESSAGE = 'no version text'
      else
         call journal('*check_commandline* no version text')
      endif
      call mystop(4,'displayed default version text')
      return
   endif
contains
subroutine default_help()
character(len=:),allocatable :: cmd_name
integer :: ilength
   call get_command_argument(number=0,length=ilength)
   if(allocated(cmd_name))deallocate(cmd_name)
   allocate(character(len=ilength) :: cmd_name)
   call get_command_argument(number=0,value=cmd_name)
   G_passed_in=G_passed_in//repeat(' ',len(G_passed_in))
   G_passed_in=replace_str(G_passed_in, ' --', NEW_LINE('A')//' --')
   if(.not.G_QUIET)then
      call journal(cmd_name,G_passed_in) ! no help text, echo command and default options
   endif
   deallocate(cmd_name)
end subroutine default_help
end subroutine check_commandline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_args(3f) - [ARGUMENTS:M_CLI2] command line argument parsing
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine set_args(prototype,help_text,version_text,ierr,errmsg)
!!
!!      character(len=*),intent(in),optional              :: prototype
!!      character(len=*),intent(in),optional              :: help_text(:)
!!      character(len=*),intent(in),optional              :: version_text(:)
!!      integer,intent(out),optional                      :: ierr
!!      character(len=:),intent(out),allocatable,optional :: errmsg
!!##DESCRIPTION
!!
!!    SET_ARGS(3f) requires a unix-like command prototype which defines
!!    the command-line options and their default values. When the program
!!    is executed this and the command-line options are applied and the
!!    resulting values are placed in an internal table for retrieval via
!!    GET_ARGS(3f).
!!
!!    The built-in --help and --version options require optional help_text
!!    and version_text values to be provided to be particularly useful.
!!
!!##OPTIONS
!!
!!    PROTOTYPE   composed of all command arguments concatenated
!!                into a Unix-like command prototype string. For
!!                example:
!!
!!                 call set_args('-L F --ints 1,2,3 --title "my title" -R 10.3')
!!
!!                The following options are predefined for all commands:
!!                '--verbose F --usage F --help F --version F'.
!!
!!                see "DEFINING THE PROTOTYPE" in the next section for
!!                further details.
!!
!!    HELP_TEXT   if present, will be displayed when the program is called with
!!                a --help switch, and then the program will terminate. If
!!                help text is not supplied the command line initialization
!!                string will be echoed.
!!
!!    VERSION_TEXT  if present, any version text defined will be displayed
!!                  when the program is called with a --version switch,
!!                  and then the program will terminate.
!!    IERR          if present a non-zero option is returned when an
!!                  error occurs instead of the program terminating.
!!    ERRMSG        a description of the error if ierr is present.
!!
!!##DEFINING THE PROTOTYPE
!!
!!    o Keywords start with a single dash for short single-character
!!      keywords, and with two dashes for longer keywords.
!!
!!    o all keywords on the prototype MUST get a value.
!!
!!       * logicals must be set to an unquoted F.
!!
!!       * strings must be delimited with double-quotes.
!!         Since internal double-quotes are represented with two
!!         double-quotes the string must be at least one space.
!!
!!    o numeric keywords are not allowed; but this allows
!!      negative numbers to be used as values.
!!
!!    o lists of values should be comma-delimited unless a
!!      user-specified delimiter is used. The prototype
!!      must use the same array delimiters as the call to
!!      get the value.
!!
!!    o to define a zero-length allocatable array make the
!!      value a delimiter (usually a comma) or an empty set
!!      of braces ("[]").
!!
!!    LONG AND SHORT NAMES
!!
!!    Long keywords start with two dashes followed by more than one letter.
!!    Short keywords are a dash followed by a single letter.
!!
!!    o It is recommended long names (--keyword) should be all lowercase
!!      but are case-sensitive by default, unless "set_mode('ignorecase')"
!!      is in effect.
!!
!!    o Long names should always be more than one character.
!!
!!    o The recommended way to have short names is to suffix the long
!!      name with :LETTER in the definition.
!!
!!      If this syntax is used then logical shorts may be combined on the
!!      command line when "set_mode('strict')" is in effect.
!!
!!    SPECIAL BEHAVIORS
!!
!!    o A special behavior occurs if a keyword name ends in ::.
!!      When the program is called the next parameter is taken as
!!      a value even if it starts with -. This is not generally
!!      recommended but is useful in rare cases where non-numeric
!!      values starting with a dash are desired.
!!
!!    o If the prototype ends with "--" a special mode is turned
!!      on where anything after "--" on input goes into the variable
!!      REMAINING with values double-quoted and also into the array ARGS
!!      instead of becoming elements in the UNNAMED array. This is not
!!      needed for normal processing, but was needed for a program that
!!      needed this behavior for its subcommands.
!!
!!      That is, for a normal call all unnamed values go into UNNAMED
!!      and ARGS and REMAINING are ignored. So for
!!
!!          call set_args('-x 10 -y 20 ')
!!
!!      A program invocation such as
!!
!!          xx a b c -- A B C " dd "
!!
!!      results in
!!
!!       UNNAMED= ['a','b','c','A','B','C',' dd']
!!       REMAINING= ''
!!       ARGS= [character(len=0) :: ] ! ie, an empty character array
!!
!!      Whereas
!!
!!       call set_args('-x 10 -y 20 --')
!!
!!      generates the following output from the same program execution:
!!
!!       UNNAMED= ['a','b','c']
!!       REMAINING= '"A" "B" "C" " dd "'
!!       ARGS= ['A','B','C,' dd']
!!
!!##USAGE NOTES
!!      When invoking the program line note the (subject to change)
!!      following restrictions (which often differ between various
!!      command-line parsers):
!!
!!      o values for duplicate keywords are appended together with a space
!!        separator when a command line is executed by default.
!!
!!      o shuffling is not supported. Values immediately follow their
!!        keywords.
!!
!!      o Only short Boolean keywords can be bundled together.
!!        If allowing bundling is desired call "set_mode('strict')".
!!        This will require prefixing long names with "--" and short
!!        names with "-". Otherwise M_CLI2 relaxes that requirement
!!        and mostly does not care what prefix is used for a keyword.
!!        But this would make it unclear what was meant by "-ox" if
!!        allowed options were "-o F -x F --ox F " for example, so
!!        "strict" mode is required to remove the ambiguity.
!!
!!      o if a parameter value of just "-" is supplied it is
!!        converted to the string "stdin".
!!
!!      o values not needed for a keyword value go into the character
!!        array "UNNAMED".
!!
!!        In addition if the keyword "--" is encountered on the command
!!        line the rest of the command line goes into the character array
!!        "UNNAMED".
!!
!!##EXAMPLE
!!
!!
!! Sample program:
!!
!!     program demo_set_args
!!     use M_CLI2,  only : filenames=>unnamed, set_args, get_args
!!     use M_CLI2,  only : get_args_fixed_size
!!     implicit none
!!     integer                      :: i
!!     ! DEFINE ARGS
!!     real                         :: x, y, z
!!     real                         :: p(3)
!!     character(len=:),allocatable :: title
!!     logical                      :: l, lbig
!!     integer,allocatable          :: ints(:)
!!     !
!!     !  DEFINE COMMAND (TO SET INITIAL VALUES AND ALLOWED KEYWORDS)
!!     !  AND READ COMMAND LINE
!!     call set_args(' &
!!        ! reals
!!        & -x 1 -y 2.3 -z 3.4e2 &
!!        ! integer array
!!        & -p -1,-2,-3 &
!!        ! always double-quote strings
!!        & --title "my title" &
!!        ! string should be a single character at a minimum
!!        & --label " ", &
!!        ! set all logical values to F
!!        & -l F -L F &
!!        ! set allocatable size to zero if you like by using a delimiter
!!        & --ints , &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!     !     SCALARS
!!     call get_args('x',x)
!!     call get_args('y',y)
!!     call get_args('z',z)
!!     call get_args('l',l)
!!     call get_args('L',lbig)
!!     call get_args('ints',ints)      ! ALLOCATABLE ARRAY
!!     call get_args('title',title)    ! ALLOCATABLE STRING
!!     call get_args_fixed_size('p',p) ! NON-ALLOCATABLE ARRAY
!!     ! USE VALUES
!!     write(*,*)'x=',x
!!     write(*,*)'y=',y
!!     write(*,*)'z=',z
!!     write(*,*)'p=',p
!!     write(*,*)'title=',title
!!     write(*,*)'ints=',ints
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     ! UNNAMED VALUES
!!     if(size(filenames) > 0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!     end program demo_set_args
!!
!!##RESPONSE FILES
!!
!!  If you have no interest in using external files as abbreviations
!!  you can ignore this section. Otherwise, before calling set_args(3f)
!!  add:
!!
!!     use M_CLI2, only : set_mode
!!     call set_mode('response_file')
!!
!!  M_CLI2 Response files are small files containing CLI (Command Line
!!  Interface) arguments that end with ".rsp" that can be used when command
!!  lines are so long that they would exceed line length limits or so complex
!!  that it is useful to have a platform-independent method of creating
!!  an abbreviation.
!!
!!  Shell aliases and scripts are often used for similar purposes (and
!!  allow for much more complex conditional execution, of course), but
!!  they generally cannot be used to overcome line length limits and are
!!  typically platform-specific.
!!
!!  Examples of commands that support similar response files are the Clang
!!  and Intel compilers, although there is no standard format for the files.
!!
!!  They are read if you add options of the syntax "@NAME" as the FIRST
!!  parameters on your program command line calls. They are not recursive --
!!  that is, an option in a response file cannot be given the value "@NAME2"
!!  to call another response file.
!!
!!  More than one response name may appear on a command line.
!!
!!  They are case-sensitive names.
!!
!!  Note "@" s a special character in Powershell, and requires being escaped
!!  with a grave character.
!!
!!   LOCATING RESPONSE FILES
!!
!!  A search for the response file always starts with the current directory.
!!  The search then proceeds to look in any additional directories specified
!!  with the colon-delimited environment variable CLI_RESPONSE_PATH.
!!
!!  The first resource file found that results in lines being processed
!!  will be used and processing stops after that first match is found. If
!!  no match is found an error occurs and the program is stopped.
!!
!!   RESPONSE FILE SECTIONS
!!
!!  A simple response file just has options for calling the program in it
!!  prefixed with the word "options".
!!  But they can also contain section headers to denote selections that are
!!  only executed when a specific OS is being used, print messages, and
!!  execute system commands.
!!
!!   SEARCHING FOR OSTYPE IN REGULAR FILES
!!
!!  So assuming the name @NAME was specified on the command line a file
!!  named NAME.rsp will be searched for in all the search directories
!!  and then in that file a string that starts with the string @OSTYPE
!!  (if the environment variables $OS and $OSTYPE are not blank. $OSTYPE
!!  takes precedence over $OS).
!!
!!   SEARCHING FOR UNLABELED DIRECTIVES IN REGULAR FILES
!!
!!  Then, the same files will be searched for lines above any line starting
!!  with "@". That is, if there is no special section for the current OS
!!  it just looks at the top of the file for unlabeled options.
!!
!!   SEARCHING FOR OSTYPE AND NAME IN THE COMPOUND FILE
!!
!!  In addition or instead of files with the same name as the @NAME option
!!  on the command line, you can have one file named after the executable
!!  name that contains multiple abbreviation names.
!!
!!  So if your program executable is named EXEC you create a single file
!!  called EXEC.rsp and can append all the simple files described above
!!  separating them with lines of the form @OSTYPE@NAME or just @NAME.
!!
!!  So if no specific file for the abbreviation is found a file called
!!  "EXEC.rsp" is searched for where "EXEC" is the name of the executable.
!!  This file is always a "compound" response file that uses the following format:
!!
!!  Any compound EXEC.rsp file found in the current or searched directories
!!  will be searched for the string @OSTYPE@NAME first.
!!
!!  Then if nothing is found, the less specific line @NAME is searched for.
!!
!!   THE SEARCH IS OVER
!!
!!  Sounds complicated but actually works quite intuitively. Make a file in
!!  the current directory and put options in it and it will be used. If that
!!  file ends up needing different cases for different platforms add a line
!!  like "@Linux" to the file and some more lines and that will only be
!!  executed if the environment variable OSTYPE or OS is "Linux". If no match
!!  is found for named sections the lines at the top before any "@" lines
!!  will be used as a default if no match is found.
!!
!!  If you end up using a lot of files like this you can combine them all
!!  together and put them into a file called "program_name".rsp and just
!!  put lines like @NAME or @OSTYPE@NAME at that top of each selection.
!!
!!  Now, back to the details on just what you can put in the files.
!!
!!##SPECIFICATION FOR RESPONSE FILES
!!
!!   SIMPLE RESPONSE FILES
!!
!!  The first word of a line is special and has the following meanings:
!!
!!    options|-  Command options following the rules of the SET_ARGS(3f)
!!               prototype. So
!!                o It is preferred to specify a value for all options.
!!                o double-quote strings.
!!                o give a blank string value as " ".
!!                o use F|T for lists of logicals,
!!                o lists of numbers should be comma-delimited.
!!                o --usage, --help, --version, --verbose, and unknown
!!                  options are ignored.
!!
!!    comment|#  Line is a comment line
!!    system|!   System command.
!!               System commands are executed as a simple call to
!!               system (so a cd(1) or setting a shell variable
!!               would not effect subsequent lines, for example)
!!               BEFORE the command being processed.
!!    print|>    Message to screen
!!    stop       display message and stop program.
!!
!!  NOTE: system commands are executed when encountered, but options are
!!  gathered from multiple option lines and passed together at the end of
!!  processing of the block; so all commands will be executed BEFORE the
!!  command for which options are being supplied no matter where they occur.
!!
!!  So if a program that does nothing but echos its parameters
!!
!!    program testit
!!    use M_CLI2, only : set_args, rget, sget, lget, set_mode
!!    implicit none
!!       real :: x,y                           ; namelist/args/ x,y
!!       character(len=:),allocatable :: title ; namelist/args/ title
!!       logical :: big                        ; namelist/args/ big
!!       call set_mode('response_file')
!!       call set_args('-x 10.0 -y 20.0 --title "my title" --big F')
!!       x=rget('x')
!!       y=rget('y')
!!       title=sget('title')
!!       big=lget('big')
!!       write(*,nml=args)
!!    end program testit
!!
!!  And a file in the current directory called "a.rsp" contains
!!
!!     # defaults for project A
!!     options -x 1000 -y 9999
!!     options --title " "
!!     options --big T
!!
!!  The program could be called with
!!
!!     $myprog     # normal call
!!      X=10.0 Y=20.0 TITLE="my title"
!!
!!     $myprog @a  # change defaults as specified in "a.rsp"
!!     X=1000.0 Y=9999.0 TITLE=" "
!!
!!     # change defaults but use any option as normal to override defaults
!!     $myprog @a -y 1234
!!      X=1000.0 Y=1234.0 TITLE=" "
!!
!!   COMPOUND RESPONSE FILES
!!
!!  A compound response file has the same basename as the executable with a
!!  ".rsp" suffix added. So if your program is named "myprg" the filename
!!  must be "myprg.rsp".
!!
!!    Note that here `basename` means the last leaf of the
!!    name of the program as returned by the Fortran intrinsic
!!    GET_COMMAND_ARGUMENT(0,...) trimmed of anything after a period ("."),
!!    so it is a good idea not to use hidden files.
!!
!!  Unlike simple response files compound response files can contain multiple
!!  setting names.
!!
!!  Specifically in a compound file
!!  if the environment variable $OSTYPE (first) or $OS is set the first search
!!  will be for a line of the form (no leading spaces should be used):
!!
!!    @OSTYPE@alias_name
!!
!!  If no match or if the environment variables $OSTYPE and $OS were not
!!  set or a match is not found then a line of the form
!!
!!    @alias_name
!!
!!  is searched for in simple or compound files. If found subsequent lines
!!  will be ignored that start with "@" until a line not starting with
!!  "@" is encountered. Lines will then be processed until another line
!!  starting with "@" is found or end-of-file is encountered.
!!
!!   COMPOUND RESPONSE FILE EXAMPLE
!!  An example compound file
!!
!!    #################
!!    @if
!!    > RUNNING TESTS USING RELEASE VERSION AND ifort
!!    options test --release --compiler ifort
!!    #################
!!    @gf
!!    > RUNNING TESTS USING RELEASE VERSION AND gfortran
!!    options test --release --compiler gfortran
!!    #################
!!    @nv
!!    > RUNNING TESTS USING RELEASE VERSION AND nvfortran
!!    options test --release --compiler nvfortran
!!    #################
!!    @nag
!!    > RUNNING TESTS USING RELEASE VERSION AND nagfor
!!    options test --release --compiler nagfor
!!    #
!!    #################
!!    # OS-specific example:
!!    @Linux@install
!!    #
!!    # install executables in directory (assuming install(1) exists)
!!    #
!!    system mkdir -p ~/.local/bin
!!    options run --release T --runner "install -vbp -m 0711 -t ~/.local/bin"
!!    @install
!!    STOP INSTALL NOT SUPPORTED ON THIS PLATFORM OR $OSTYPE NOT SET
!!    #
!!    #################
!!    @fpm@testall
!!    #
!!    !fpm test --compiler nvfortran
!!    !fpm test --compiler ifort
!!    !fpm test --compiler gfortran
!!    !fpm test --compiler nagfor
!!    STOP tests complete. Any additional parameters were ignored
!!    #################
!!
!!  Would be used like
!!
!!    fpm @install
!!    fpm @nag --
!!    fpm @testall
!!
!!   NOTES
!!
!!    The intel Fortran compiler now calls the response files "indirect
!!    files" and does not add the implied suffix ".rsp" to the files
!!    anymore. It also allows the @NAME syntax anywhere on the command line,
!!    not just at the beginning. -- 20201212
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!
!!##LICENSE
!!      Public Domain

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_args(prototype,help_text,version_text,string,prefix,ierr,errmsg)

! ident_1="@(#) M_CLI2 set_args(3f) parse prototype string"

character(len=*),intent(in)                       :: prototype
character(len=*),intent(in),optional              :: help_text(:)
character(len=*),intent(in),optional              :: version_text(:)
character(len=*),intent(in),optional              :: string
character(len=*),intent(in),optional              :: prefix
integer,intent(out),optional                      :: ierr
character(len=:),intent(out),allocatable,optional :: errmsg
character(len=:),allocatable                      :: hold               ! stores command line argument
integer                                           :: ibig
character(len=:),allocatable                      :: debug_mode

   debug_mode= upper(get_env('CLI_DEBUG_MODE','FALSE'))//' '
   select case(debug_mode(1:1))
   case('Y','T')
      G_DEBUG=.true.
   end select

   G_response=CLI_RESPONSE_FILE
   G_options_only=.false.
   G_passed_in=''
   G_STOP=0
   G_STOP_MESSAGE=''
   if(present(prefix))then
      G_PREFIX=prefix
   else
      G_PREFIX=''
   endif
   if(present(ierr))then
      G_QUIET=.true.
   else
      G_QUIET=.false.
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   IF(ALLOCATED(UNNAMED)) DEALLOCATE(UNNAMED)
   ALLOCATE(CHARACTER(LEN=IBIG) :: UNNAMED(0))
   if(allocated(args)) deallocate(args)
   allocate(character(len=ibig) :: args(0))

   call wipe_dictionary()
   hold='--version F --usage F --help F --version F '//adjustl(prototype)
   call prototype_and_cmd_args_to_nlist(hold,string)
   if(allocated(G_RESPONSE_IGNORED))then
      if(G_DEBUG)write(*,gen)'<DEBUG>SET_ARGS:G_RESPONSE_IGNORED:',G_RESPONSE_IGNORED
      if(size(unnamed) /= 0)write(*,*)'LOGIC ERROR'
      call split(G_RESPONSE_IGNORED,unnamed)
   endif

   if(.not.allocated(unnamed))then
       allocate(character(len=0) :: unnamed(0))
   endif
   if(.not.allocated(args))then
       allocate(character(len=0) :: args(0))
   endif
   call check_commandline(help_text,version_text) ! process --help, --version, --usage
   if(present(ierr))then
      ierr=G_STOP
   endif
   if(present(errmsg))then
      errmsg=G_STOP_MESSAGE
   endif
end subroutine set_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_subcommand(3f) - [ARGUMENTS:M_CLI2] special-case routine for
!!    handling subcommands on a command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function get_subcommand()
!!
!!     character(len=:),allocatable :: get_subcommand
!!
!!##DESCRIPTION
!!    In the special case when creating a program with subcommands it
!!    is assumed the first word on the command line is the subcommand. A
!!    routine is required to handle response file processing, therefore
!!    this routine (optionally processing response files) returns that
!!    first word as the subcommand name.
!!
!!    It should not be used by programs not building a more elaborate
!!    command with subcommands.
!!
!!##RETURNS
!!    NAME   name of subcommand
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!    program demo_get_subcommand
!!    !x! SUBCOMMANDS
!!    !x! For a command with subcommands like git(1)
!!    !x! you can make separate namelists for each subcommand.
!!    !x! You can call this program which has two subcommands (run, test),
!!    !x! like this:
!!    !x!    demo_get_subcommand --help
!!    !x!    demo_get_subcommand run -x -y -z --title -l -L
!!    !x!    demo_get_subcommand test --title -l -L --testname
!!    !x!    demo_get_subcommand run --help
!!       implicit none
!!    !x! DEFINE VALUES TO USE AS ARGUMENTS WITH INITIAL VALUES
!!       real               :: x=-999.0,y=-999.0,z=-999.0
!!       character(len=80)  :: title="not set"
!!       logical            :: l=.false.
!!       logical            :: l_=.false.
!!       character(len=80)  :: testname="not set"
!!       character(len=20)  :: name
!!       call parse(name) !x! DEFINE AND PARSE COMMAND LINE
!!       !x! ALL DONE CRACKING THE COMMAND LINE.
!!       !x! USE THE VALUES IN YOUR PROGRAM.
!!       write(*,*)'command was ',name
!!       write(*,*)'x,y,z .... ',x,y,z
!!       write(*,*)'title .... ',title
!!       write(*,*)'l,l_ ..... ',l,l_
!!       write(*,*)'testname . ',testname
!!    contains
!!    subroutine parse(name)
!!    !x! PUT EVERYTHING TO DO WITH COMMAND PARSING HERE FOR CLARITY
!!    use M_CLI2, only : set_args, get_args, get_args_fixed_length
!!    use M_CLI2, only : get_subcommand, set_mode
!!    character(len=*)              :: name    ! the subcommand name
!!    character(len=:),allocatable  :: help_text(:), version_text(:)
!!       call set_mode('response_file')
!!    ! define version text
!!       version_text=[character(len=80) :: &
!!          '@(#)PROGRAM:     demo_get_subcommand            >', &
!!          '@(#)DESCRIPTION: My demo program  >', &
!!          '@(#)VERSION:     1.0 20200715     >', &
!!          '@(#)AUTHOR:      me, myself, and I>', &
!!          '@(#)LICENSE:     Public Domain    >', &
!!          '' ]
!!        ! general help for "demo_get_subcommand --help"
!!        help_text=[character(len=80) :: &
!!         ' allowed subcommands are          ', &
!!         '   * run  -l -L --title -x -y -z  ', &
!!         '   * test -l -L --title           ', &
!!         '' ]
!!       ! find the subcommand name by looking for first word on command
!!       ! not starting with dash
!!       name = get_subcommand()
!!       select case(name)
!!       case('run')
!!        help_text=[character(len=80) :: &
!!         '                                  ', &
!!         ' Help for subcommand "run"        ', &
!!         '                                  ', &
!!         '' ]
!!        call set_args( &
!!        & '-x 1 -y 2 -z 3 --title "my title" -l F -L F',&
!!        & help_text,version_text)
!!        call get_args('x',x)
!!        call get_args('y',y)
!!        call get_args('z',z)
!!        call get_args_fixed_length('title',title)
!!        call get_args('l',l)
!!        call get_args('L',l_)
!!       case('test')
!!        help_text=[character(len=80) :: &
!!         '                                  ', &
!!         ' Help for subcommand "test"       ', &
!!         '                                  ', &
!!         '' ]
!!        call set_args(&
!!        & '--title "my title" -l F -L F --testname "Test"',&
!!        & help_text,version_text)
!!        call get_args_fixed_length('title',title)
!!        call get_args('l',l)
!!        call get_args('L',l_)
!!        call get_args_fixed_length('testname',testname)
!!       case default
!!        ! process help and version
!!        call set_args(' ',help_text,version_text)
!!        write(*,'(*(a))')'unknown or missing subcommand [',trim(name),']'
!!        write(*,'(a)')[character(len=80) ::  &
!!        ' allowed subcommands are          ', &
!!        '   * run  -l -L -title -x -y -z   ', &
!!        '   * test -l -L -title            ', &
!!        '' ]
!!        stop
!!       end select
!!    end subroutine parse
!!    end program demo_get_subcommand
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
function get_subcommand() result(sub)

! ident_2="@(#) M_CLI2 get_subcommand(3f) parse prototype string to get subcommand allowing for response files"

character(len=:),allocatable  :: sub
character(len=:),allocatable  :: cmdarg
character(len=:),allocatable  :: array(:)
character(len=:),allocatable  :: prototype
integer                       :: ilongest
integer                       :: i
integer                       :: j
   G_subcommand=''
   G_options_only=.true.
   sub=''

   if(.not.allocated(unnamed))then
      allocate(character(len=0) :: unnamed(0))
   endif

   ilongest=longest_command_argument()
   allocate(character(len=max(63,ilongest)):: cmdarg)
   cmdarg(:) = ''
   ! look for @NAME if CLI_RESPONSE_FILE=.TRUE. AND LOAD THEM
   do i = 1, command_argument_count()
      call get_command_argument(i, cmdarg)
      if(scan(adjustl(cmdarg(1:1)),'@')  ==  1)then
         call get_prototype(cmdarg,prototype)
         call split(prototype,array)
         ! assume that if using subcommands first word not starting with dash is the subcommand
         do j=1,size(array)
            if(adjustl(array(j)(1:1))  /=  '-')then
            G_subcommand=trim(array(j))
            sub=G_subcommand
            exit
         endif
         enddo
      endif
   enddo

   if(G_subcommand /= '')then
      sub=G_subcommand
   elseif(size(unnamed) /= 0)then
      sub=unnamed(1)
   else
      cmdarg(:) = ''
      do i = 1, command_argument_count()
         call get_command_argument(i, cmdarg)
         if(adjustl(cmdarg(1:1))  /=  '-')then
            sub=trim(cmdarg)
           exit
        endif
      enddo
   endif
   G_options_only=.false.
end function get_subcommand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_usage(3f) - [ARGUMENTS:M_CLI2] allow setting a short description
!!    for keywords for the --usage switch
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine set_usage(keyword,description)
!!
!!      character(len=*),intent(in)     ::  keyword
!!      character(len=*),intent(in)     ::  description
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!     KEYWORD      the name of a command keyword
!!     DESCRIPTION  a brief one-line description of the keyword
!!
!!
!!##EXAMPLE
!!
!! sample program:
!!
!!     Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine set_usage(keyword,description,value)
character(len=*),intent(in) :: keyword
character(len=*),intent(in) :: description
character(len=*),intent(in) :: value
write(*,*)keyword
write(*,*)description
write(*,*)value
! store the descriptions in an array and then apply them when set_args(3f) is called.
! alternatively, could allow for a value as well in lieu of the prototype
end subroutine set_usage
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    prototype_to_dictionary(3f) - [ARGUMENTS:M_CLI2] parse user command
!!    and store tokens into dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     recursive subroutine prototype_to_dictionary(string)
!!
!!      character(len=*),intent(in)     ::  string
!!
!!##DESCRIPTION
!!      given a string of form
!!
!!        -var value -var value
!!
!!      define dictionary of form
!!
!!        keyword(i), value(i)
!!
!!      o  string values
!!
!!          o must be delimited with double quotes.
!!          o adjacent double quotes put one double quote into value
!!          o must not be null. A blank is specified as " ", not "".
!!
!!      o  logical values
!!
!!          o logical values must have a value. Use F.
!!
!!      o  leading and trailing blanks are removed from unquoted values
!!
!!
!!##OPTIONS
!!      STRING   string is character input string to define command
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!! sample program:
!!
!!     call prototype_to_dictionary(' -l F --ignorecase F --title "my title string" -x 10.20')
!!     call prototype_to_dictionary(' --ints 1,2,3,4')
!!
!! Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
recursive subroutine prototype_to_dictionary(string)

! ident_3="@(#) M_CLI2 prototype_to_dictionary(3f) parse user command and store tokens into dictionary"

character(len=*),intent(in)       :: string ! string is character input string of options and values

character(len=:),allocatable      :: dummy   ! working copy of string
character(len=:),allocatable      :: value
character(len=:),allocatable      :: keyword
character(len=3)                  :: delmt   ! flag if in a delimited string or not
character(len=1)                  :: currnt  ! current character being processed
character(len=1)                  :: prev    ! character to left of CURRNT
character(len=1)                  :: forwrd  ! character to right of CURRNT
integer,dimension(2)              :: ipnt
integer                           :: islen   ! number of characters in input string
integer                           :: ipoint
integer                           :: itype
integer,parameter                 :: VAL=1, KEYW=2
integer                           :: ifwd
integer                           :: ibegin
integer                           :: iend
integer                           :: place

   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   dummy=adjustl(string)//'  '

   keyword=""          ! initial variable name
   value=""            ! initial value of a string
   ipoint=0            ! ipoint is the current character pointer for (dummy)
   ipnt(2)=2           ! pointer to position in keyword
   ipnt(1)=1           ! pointer to position in value
   itype=VAL           ! itype=1 for value, itype=2 for variable

   delmt="off"
   prev=" "

   G_keyword_single_letter=.true.
   do
      ipoint=ipoint+1               ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)   ! store current character into currnt
      ifwd=min(ipoint+1,islen)      ! ensure not past end of string
      forwrd=dummy(ifwd:ifwd)       ! next character (or duplicate if last)

      if((currnt=="-" .and. prev==" " .and. delmt == "off" .and. index("0123456789.",forwrd) == 0).or.ipoint > islen)then
         ! beginning of a keyword
         if(forwrd == '-')then                      ! change --var to -var so "long" syntax is supported
            !x!dummy(ifwd:ifwd)='_'
            ipoint=ipoint+1                         ! ignore second - instead (was changing it to _)
            G_keyword_single_letter=.false.         ! flag this is a long keyword
         else
            G_keyword_single_letter=.true.          ! flag this is a short (single letter) keyword
         endif
         if(ipnt(1)-1 >= 1)then                     ! position in value
            ibegin=1
            iend=len_trim(value(:ipnt(1)-1))
            TESTIT: do
               if(iend  ==  0)then                  ! len_trim returned 0, value is blank
                  iend=ibegin
                  exit TESTIT
               elseif(value(ibegin:ibegin) == " ")then
                  ibegin=ibegin+1
               else
                  exit TESTIT
               endif
            enddo TESTIT
            if(keyword /= ' ')then
               if(value=='[]')value=','
               call update(keyword,value)            ! store name and its value
            elseif( G_remaining_option_allowed)then  ! meaning "--" has been encountered
               if(value=='[]')value=','
               call update('_args_',trim(value))
            else
               !x!write(warn,'(*(g0))')'*prototype_to_dictionary* warning: ignoring string [',trim(value),'] for ',trim(keyword)
               G_RESPONSE_IGNORED=TRIM(VALUE)
               if(G_DEBUG)write(*,gen)'<DEBUG>PROTOTYPE_TO_DICTIONARY:G_RESPONSE_IGNORED:',G_RESPONSE_IGNORED
            endif
         else
            call locate_key(keyword,place)
            if(keyword /= ' '.and.place < 0)then
               call update(keyword,'F')           ! store name and null value (first pass)
            elseif(keyword /= ' ')then
               call update(keyword,' ')           ! store name and null value (second pass)
            elseif(.not.G_keyword_single_letter.and.ipoint-2 == islen) then ! -- at end of line
               G_remaining_option_allowed=.true.  ! meaning for "--" is that everything on commandline goes into G_remaining
            endif
         endif
         itype=KEYW                            ! change to expecting a keyword
         value=""                              ! clear value for this variable
         keyword=""                            ! clear variable name
         ipnt(1)=1                             ! restart variable value
         ipnt(2)=1                             ! restart variable name

      else       ! currnt is not one of the special characters
         ! the space after a keyword before the value
         if(currnt == " " .and. itype  ==  KEYW)then
            ! switch from building a keyword string to building a value string
            itype=VAL
            ! beginning of a delimited value
         elseif(currnt  ==  """".and.itype  ==  VAL)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
               if(itype == VAL)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
               delmt="on"
            elseif(delmt  ==  "on")then     ! first quote of a delimited string
               delmt="off"
            else
               delmt="on"
            endif
            if(prev /= """")then  ! leave quotes where found them
               if(itype == VAL)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
            endif
         else     ! add character to current keyword or value
            if(itype == VAL)then
               value=value//currnt
            else
               keyword=keyword//currnt
            endif
            ipnt(itype)=ipnt(itype)+1
         endif

      endif

      prev=currnt
      if(ipoint <= islen)then
         cycle
      else
         exit
      endif
   enddo

end subroutine prototype_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    specified(3f) - [ARGUMENTS:M_CLI2] return true if keyword was present
!!    on command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental impure function specified(name)
!!
!!     character(len=*),intent(in) :: name
!!     logical :: specified
!!
!!##DESCRIPTION
!!
!!    specified(3f) returns .true. if the specified keyword was present on
!!    the command line.
!!
!!    M_CLI2 intentionally does not have validators except for SPECIFIED(3f)
!!    and of course a check whether the input conforms to the type when
!!    requesting a value (with get_args(3f) or the convenience functions
!!    like inum(3f)).
!!
!!    Fortran already has powerful validation capabilities. Logical
!!    expressions ANY(3f) and ALL(3f) are standard Fortran features which
!!    easily allow performing the common validations for command line
!!    arguments without having to learn any additional syntax or methods.
!!
!!##OPTIONS
!!
!!    NAME   name of commandline argument to query the presence of. Long
!!           names should always be used.
!!
!!##RETURNS
!!    SPECIFIED  returns .TRUE. if specified NAME was present on the command
!!               line when the program was invoked.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!    program demo_specified
!!    use, intrinsic :: iso_fortran_env, only : &
!!    & stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
!!    use M_CLI2,  only : set_args, igets, rgets, specified, sget, lget
!!    implicit none
!!
!!    ! Define args
!!    integer,allocatable  :: ints(:)
!!    real,allocatable     :: floats(:)
!!    logical              :: flag
!!    character(len=:),allocatable :: color
!!    character(len=:),allocatable :: list(:)
!!    integer :: i
!!
!!     call set_args('&
!!        & --color:c "red"       &
!!        & --flag:f F            &
!!        & --ints:i 1,10,11      &
!!        & --floats:T 12.3, 4.56 &
!!        & ')
!!     ints=igets('ints')
!!     floats=rgets('floats')
!!     flag=lget('flag')
!!     color=sget('color')
!!
!!     write(*,*)'color=',color
!!     write(*,*)'flag=',flag
!!     write(*,*)'ints=',ints
!!     write(*,*)'floats=',floats
!!
!!     write(*,*)'was -flag specified?',specified('flag')
!!
!!     ! elemental
!!     write(*,*)specified(['floats','ints  '])
!!
!!     ! If you want to know if groups of parameters were specified use
!!     ! ANY(3f) and ALL(3f)
!!     write(*,*)'ANY:',any(specified(['floats','ints  ']))
!!     write(*,*)'ALL:',all(specified(['floats','ints  ']))
!!
!!     ! For mutually exclusive
!!     if (all(specified(['floats','ints  '])))then
!!         write(*,*)'You specified both names --ints and --floats'
!!     endif
!!
!!     ! For required parameter
!!     if (.not.any(specified(['floats','ints  '])))then
!!         write(*,*)'You must specify --ints or --floats'
!!     endif
!!
!!    ! check if all values are in range from 10 to 30 and even
!!    write(*,*)'are all numbers good?',all([ints >= 10,ints <= 30,(ints/2)*2 == ints])
!!
!!    ! perhaps you want to check one value at a time
!!    do i=1,size(ints)
!!       write(*,*)ints(i),[ints(i) >= 10,ints(i) <= 30,(ints(i)/2)*2 == ints(i)]
!!       if(all([ints(i) >= 10,ints(i) <= 30,(ints(i)/2)*2 == ints(i)]) )then
!!          write(*,*)ints(i),'is an even number from 10 to 30 inclusive'
!!       else
!!          write(*,*)ints(i),'is not an even number from 10 to 30 inclusive'
!!       endif
!!    enddo
!!
!!    list = [character(len=10) :: 'red','white','blue']
!!    if( any(color == list) )then
!!       write(*,*)color,'matches a value in the list'
!!    else
!!       write(*,*)color,'not in the list'
!!    endif
!!
!!    if(size(ints).eq.3)then
!!       write(*,*)'ints(:) has expected number of values'
!!    else
!!       write(*,*)'ints(:) does not have expected number of values'
!!    endif
!!
!!    end program demo_specified
!!
!! Default output
!!
!!  > color=red
!!  > flag= F
!!  > ints=           1          10          11
!!  > floats=   12.3000002       4.55999994
!!  > was -flag specified? F
!!  > F F
!!  > ANY: F
!!  > ALL: F
!!  > You must specify --ints or --floats
!!  >           1 F T F
!!  >           1  is not an even number from 10 to 30 inclusive
!!  >          10 T T T
!!  >          10  is an even number from 10 to 30 inclusive
!!  >          11 T T F
!!  >          11  is not an even number from 10 to 30 inclusive
!!  > red matches a value in the list
!!  > ints(:) has expected number of values
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
elemental impure function specified(key)
character(len=*),intent(in) :: key
logical                     :: specified
integer                     :: place
   call locate_key(key,place)                   ! find where string is or should be
   if(place < 1)then
      specified=.false.
   else
      specified=present_in(place)
   endif
end function specified
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    update(3f) - [ARGUMENTS:M_CLI2] update internal dictionary given
!!    keyword and value
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine update(key,val)
!!
!!      character(len=*),intent(in)           :: key
!!      character(len=*),intent(in),optional  :: val
!!##DESCRIPTION
!!      Update internal dictionary in M_CLI2(3fm) module.
!!##OPTIONS
!!      key  name of keyword to add, replace, or delete from dictionary
!!      val  if present add or replace value associated with keyword. If not
!!           present remove keyword entry from dictionary.
!!
!!           If "present" is true, a value will be appended
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine update(key,val)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: val
integer                               :: place, ii
integer                               :: iilen
character(len=:),allocatable          :: val_local
character(len=:),allocatable          :: short
character(len=:),allocatable          :: long
character(len=:),allocatable          :: long_short(:)
integer                               :: isize
logical                               :: set_mandatory
   set_mandatory=.false.
   call split(trim(key),long_short,':',nulls='return') ! split long:short keyword or long:short:: or long:: or short::
   ! check for :: on end
   isize=size(long_short)

   if(isize > 0)then                     ! very special-purpose syntax where if ends in :: next field is a value even
      if(long_short(isize) == '')then    ! if it starts with a dash, for --flags option on fpm(1).
         set_mandatory=.true.
         long_short=long_short(:isize-1)
      endif
   endif

   select case(size(long_short))
   case(0)
      long=''
      short=''
   case(1)
      long=trim(long_short(1))
      if(len_trim(long) == 1)then
         !x!ii= findloc (shorts, long, dim=1) ! if parsing arguments on line and a short keyword look up long value
         ii=maxloc([0,merge(1, 0, shorts == long)],dim=1)
         if(ii > 1)then
            long=keywords(ii-1)
         endif
         short=long
      else
         short=''
      endif
   case(2)
      long=trim(long_short(1))
      short=trim(long_short(2))
   case default
      write(warn,*)'WARNING: incorrect syntax for key: ',trim(key)
      long=trim(long_short(1))
      short=trim(long_short(2))
   end select
   if(G_UNDERDASH) long=replace_str(long,'-','_')
   if(G_NOSEPARATOR)then
      long=replace_str(long,'-','')
      long=replace_str(long,'_','')
   endif
   if(G_IGNORECASE.and.len_trim(long) > 1)long=lower(long)
   if(present(val))then
      val_local=val
      iilen=len_trim(val_local)
      call locate_key(long,place)                  ! find where string is or should be
      if(place < 1)then                                ! if string was not found insert it
         call insert_(keywords,long,iabs(place))
         call insert_(values,val_local,iabs(place))
         call insert_(counts,iilen,iabs(place))
         call insert_(shorts,short,iabs(place))
         call insert_(present_in,.true.,iabs(place))
         call insert_(mandatory,set_mandatory,iabs(place))
      else
         if(present_in(place))then                      ! if multiple keywords append values with space between them
            if(G_append)then
               if(values(place)(1:1) == '"')then
               ! UNDESIRABLE: will ignore previous blank entries
                  val_local='"'//trim(unquote(values(place)))//' '//trim(unquote(val_local))//'"'
               else
                  val_local=values(place)//' '//val_local
               endif
            endif
            iilen=len_trim(val_local)
         endif
         call replace_(values,val_local,place)
         call replace_(counts,iilen,place)
         call replace_(present_in,.true.,place)
      endif
   else                                                 ! if no value is present remove the keyword and related values
      call locate_key(long,place)                       ! check name as long and short
      if(place > 0)then
         call remove_(keywords,place)
         call remove_(values,place)
         call remove_(counts,place)
         call remove_(shorts,place)
         call remove_(present_in,place)
         call remove_(mandatory,place)
      endif
   endif
end subroutine update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    wipe_dictionary(3fp) - [ARGUMENTS:M_CLI2] reset private M_CLI2(3fm)
!!    dictionary to empty
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!      subroutine wipe_dictionary()
!!##DESCRIPTION
!!      reset private M_CLI2(3fm) dictionary to empty
!!##EXAMPLE
!!
!! Sample program:
!!
!!      program demo_wipe_dictionary
!!      use M_CLI2, only : dictionary
!!         call wipe_dictionary()
!!      end program demo_wipe_dictionary
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine wipe_dictionary()
   if(allocated(keywords))deallocate(keywords)
   allocate(character(len=0) :: keywords(0))
   if(allocated(values))deallocate(values)
   allocate(character(len=0) :: values(0))
   if(allocated(counts))deallocate(counts)
   allocate(counts(0))
   if(allocated(shorts))deallocate(shorts)
   allocate(character(len=0) :: shorts(0))
   if(allocated(present_in))deallocate(present_in)
   allocate(present_in(0))
   if(allocated(mandatory))deallocate(mandatory)
   allocate(mandatory(0))
end subroutine wipe_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get(3f) - [ARGUMENTS:M_CLI2] get dictionary value associated with
!!    key name in private M_CLI2(3fm) dictionary
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!    Get dictionary value associated with key name in private M_CLI2(3fm)
!!    dictionary.
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate_key(key,place)
   if(place < 1)then
      valout=''
   else
      valout=values(place)(:counts(place))
   endif
end function get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    prototype_and_cmd_args_to_nlist(3f) - [ARGUMENTS:M_CLI2] convert
!!    Unix-like command arguments to table
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine prototype_and_cmd_args_to_nlist(prototype)
!!
!!      character(len=*) :: prototype
!!##DESCRIPTION
!!    create dictionary with character keywords, values, and value lengths
!!    using the routines for maintaining a list from command line arguments.
!!##OPTIONS
!!      prototype
!!##EXAMPLE
!!
!! Sample program
!!
!!      program demo_prototype_and_cmd_args_to_nlist
!!      use M_CLI2,  only : prototype_and_cmd_args_to_nlist, unnamed
!!      implicit none
!!      character(len=:),allocatable :: readme
!!      character(len=256)           :: message
!!      integer                      :: ios
!!      integer                      :: i
!!      doubleprecision              :: something
!!
!!      ! define arguments
!!      logical            :: l,h,v
!!      real               :: p(2)
!!      complex            :: c
!!      doubleprecision    :: x,y,z
!!
!!      ! uppercase keywords get an underscore to make it easier to remember
!!      logical            :: l_,h_,v_
!!      ! character variables must be long enough to hold returned value
!!      character(len=256) :: a_,b_
!!      integer            :: c_(3)
!!
!!         ! give command template with default values
!!         ! all values except logicals get a value.
!!         ! strings must be delimited with double quotes
!!         ! A string has to have at least one character as for -A
!!         ! lists of numbers should be comma-delimited.
!!         ! No spaces are allowed in lists of numbers
!!         call prototype_and_cmd_args_to_nlist('&
!!         & -l -v -h -LVH -x 0 -y 0.0 -z 0.0d0 -p 0,0 &
!!         & -A " " -B "Value B" -C 10,20,30 -c (-123,-456)',readme)
!!
!!         call get_args('x',x,'y',y,'z',z)
!!            something=sqrt(x**2+y**2+z**2)
!!            write (*,*)something,x,y,z
!!            if(size(unnamed) > 0)then
!!               write (*,'(a)')'files:'
!!               write (*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!            endif
!!      end program demo_prototype_and_cmd_args_to_nlist
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine prototype_and_cmd_args_to_nlist(prototype,string)

! ident_4="@(#) M_CLI2 prototype_and_cmd_args_to_nlist create dictionary from prototype if not null and update from command line"

character(len=*),intent(in)           :: prototype
character(len=*),intent(in),optional  :: string
integer                               :: ibig
integer                               :: itrim
integer                               :: iused

   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:START'
   G_passed_in=prototype                            ! make global copy for printing
   ibig=longest_command_argument()                  ! bug in gfortran. len=0 should be fine
   ibig=max(ibig,1)
   IF(ALLOCATED(UNNAMED))DEALLOCATE(UNNAMED)
   ALLOCATE(CHARACTER(LEN=IBIG) :: UNNAMED(0))
   if(allocated(args))deallocate(args)
   allocate(character(len=ibig) :: args(0))

   G_remaining_option_allowed=.false.
   G_remaining_on=.false.
   G_remaining=''
   if(prototype /= '')then
      call prototype_to_dictionary(prototype)       ! build dictionary from prototype

      ! if short keywords not used by user allow them for standard options

      call locate_key('h',iused)
      if(iused <= 0)then
         call update('help')
         call update('help:h','F')
      endif

      call locate_key('v',iused)
      if(iused <= 0)then
         call update('version')
         call update('version:v','F')
      endif

      call locate_key('V',iused)
      if(iused <= 0)then
         call update('verbose')
         call update('verbose:V','F')
      endif

      call locate_key('u',iused)
      if(iused <= 0)then
         call update('usage')
         call update('usage:u','F')
      endif

      present_in=.false.                            ! reset all values to false so everything gets written
   endif

   if(present(string))then                          ! instead of command line arguments use another prototype string
      if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:CALL PROTOTYPE_TO_DICTIONARY:STRING=',STRING
      call prototype_to_dictionary(string)          ! build dictionary from prototype
   else
      if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:CALL CMD_ARGS_TO_DICTIONARY:CHECK=',.true.
      call cmd_args_to_dictionary()
   endif

   if( len(G_remaining) > 1)then                    ! if -- was in prototype then after -- on input return rest in this string
      itrim=len(G_remaining)
      if(G_remaining(itrim:itrim) == ' ')then       ! was adding a space at end as building it, but do not want to remove blanks
         G_remaining=G_remaining(:itrim-1)
      endif
      remaining=G_remaining
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:NORMAL END'
end subroutine prototype_and_cmd_args_to_nlist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine expand_response(name)
character(len=*),intent(in)  :: name
character(len=:),allocatable :: prototype
logical                      :: hold

   if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:START:NAME=',name

   call get_prototype(name,prototype)

   if(prototype /= '')then
      hold=G_append
      G_append=.false.
      if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:CALL PROTOTYPE_TO_DICTIONARY:PROTOTYPE=',prototype
      call prototype_to_dictionary(prototype)       ! build dictionary from prototype
      G_append=hold
   endif

   if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:END'

end subroutine expand_response
!===================================================================================================================================
subroutine get_prototype(name,prototype) ! process @name abbreviations
character(len=*),intent(in) :: name
character(len=:),allocatable,intent(out) :: prototype
character(len=:),allocatable             :: filename
character(len=:),allocatable             :: os
character(len=:),allocatable             :: plain_name
character(len=:),allocatable             :: search_for
integer                                  :: lun
integer                                  :: ios
integer                                  :: itrim
character(len=4096)                      :: line !x! assuming input never this long
character(len=256)                       :: message
character(len=:),allocatable             :: array(:) ! output array of tokens
integer                                  :: lines_processed

   lines_processed=0
   plain_name=name//'  '
   plain_name=trim(name(2:))
   os= '@' // get_env('OSTYPE',get_env('OS'))
   if(G_DEBUG)write(*,gen)'<DEBUG>GET_PROTOTYPE:OS=',OS

   search_for=''
   ! look for NAME.rsp and see if there is an @OS  section in it and position to it and read
   if(os /= '@')then
      search_for=os
      call find_and_read_response_file(plain_name)
      if(lines_processed /= 0)return
   endif

   ! look for NAME.rsp and see if there is anything before an OS-specific section
   search_for=''
   call find_and_read_response_file(plain_name)
   if(lines_processed /= 0)return

   ! look for ARG0.rsp  with @OS@NAME  section in it and position to it
   if(os /= '@')then
      search_for=os//name
      call find_and_read_response_file(basename(get_name(),suffix=.false.))
      if(lines_processed /= 0)return
   endif

   ! look for ARG0.rsp  with a section called @NAME in it and position to it
   search_for=name
   call find_and_read_response_file(basename(get_name(),suffix=.false.))
   if(lines_processed /= 0)return

   write(*,gen)'<ERROR> response name ['//trim(name)//'] not found'
   stop 1
contains
!===================================================================================================================================
subroutine find_and_read_response_file(rname)
! search for a simple file named the same as the @NAME field with one entry assumed in it
character(len=*),intent(in)  :: rname
character(len=:),allocatable :: paths(:)
character(len=:),allocatable :: testpath
character(len=256)           :: message
integer                      :: i
integer                      :: ios
   prototype=''
   ! look for NAME.rsp
   ! assume if have / or \ a full filename was supplied to support ifort(1)
   if((index(rname,'/') /= 0.or.index(rname,'\') /= 0) .and. len(rname) > 1 )then
      filename=rname
      lun=fileopen(filename,message)
      if(lun /= -1)then
         call process_response()
         close(unit=lun,iostat=ios)
      endif
      return
   else
      filename=rname//'.rsp'
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:FILENAME=',filename

   ! look for name.rsp in directories from environment variable assumed to be a colon-separated list of directories
   call split(get_env('CLI_RESPONSE_PATH','~/.local/share/rsp'),paths)
   paths=[character(len=len(paths)) :: ' ',paths]
   if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:PATHS=',paths

   do i=1,size(paths)
      testpath=join_path(paths(i),filename)
      lun=fileopen(testpath,message)
      if(lun /= -1)then
         if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:SEARCH_FOR=',search_for
         if(search_for /= '') call position_response() ! set to end of file or where string was found
         call process_response()
         if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:LINES_PROCESSED=',LINES_PROCESSED
         close(unit=lun,iostat=ios)
         if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:CLOSE:LUN=',LUN,' IOSTAT=',IOS
         if(lines_processed /= 0)exit
      endif
   enddo

end subroutine find_and_read_response_file
!===================================================================================================================================
subroutine position_response()
integer :: ios
   line=''
   INFINITE: do
      read(unit=lun,fmt='(a)',iostat=ios,iomsg=message)line
      if(is_iostat_end(ios))then
         if(G_DEBUG)write(*,gen)'<DEBUG>POSITION_RESPONSE:EOF'
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios /= 0)then
         write(*,gen)'<ERROR>*position_response*:'//trim(message)
         exit INFINITE
      endif
      line=adjustl(line)
      if(line == search_for)return
   enddo INFINITE
end subroutine position_response
!===================================================================================================================================
subroutine process_response()
character(len=:),allocatable :: padded
character(len=:),allocatable :: temp
   line=''
   lines_processed=0
      INFINITE: do
      read(unit=lun,fmt='(a)',iostat=ios,iomsg=message)line
      if(is_iostat_end(ios))then
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios /= 0)then
         write(*,gen)'<ERROR>*process_response*:'//trim(message)
         exit INFINITE
      endif
      line=trim(adjustl(line))
      temp=line
      if(index(temp//' ','#') == 1)cycle
      if(temp /= '')then

         if(index(temp,'@') == 1.and.lines_processed /= 0)exit INFINITE

         call split(temp,array) ! get first word
         itrim=len_trim(array(1))+2
         temp=temp(itrim:)

         PROCESS: select case(lower(array(1)))
         case('comment','#','')
         case('system','!','$')
            if(G_options_only)exit PROCESS
            lines_processed= lines_processed+1
            call execute_command_line(temp)
         case('options','option','-')
            lines_processed= lines_processed+1
            prototype=prototype//' '//trim(temp)
         case('print','>','echo')
            if(G_options_only)exit PROCESS
            lines_processed= lines_processed+1
            write(*,'(a)')trim(temp)
         case('stop')
            if(G_options_only)exit PROCESS
            write(*,'(a)')trim(temp)
            stop
         case default
            if(array(1)(1:1) == '-')then
               ! assume these are simply options to support ifort(1)
               ! if starts with a single dash must assume a single argument
               ! and rest is value to support -Dname and -Ifile option
               ! which currently is not supported, so multiple short keywords
               ! does not work. Just a ifort(1) test at this point, so do not document
               if(G_options_only)exit PROCESS
               padded=trim(line)//'  '
               if(padded(2:2) == '-')then
                  prototype=prototype//' '//trim(line)
               else
                  prototype=prototype//' '//padded(1:2)//' '//trim(padded(3:))
               endif
               lines_processed= lines_processed+1
            else
               if(array(1)(1:1) == '@')cycle INFINITE !skip adjacent @ lines from first
               lines_processed= lines_processed+1
               write(*,'(*(g0))')'unknown response keyword [',array(1),'] with options of [',trim(temp),']'
            endif
         end select PROCESS

      endif
      enddo INFINITE
end subroutine process_response

end subroutine get_prototype
!===================================================================================================================================
function fileopen(filename,message) result(lun)
character(len=*),intent(in)              :: filename
character(len=*),intent(out),optional    :: message
integer                                  :: lun
integer                                  :: ios
character(len=256)                       :: message_local

   ios=0
   message_local=''
   open(file=filename,newunit=lun,&
    & form='formatted',access='sequential',action='read',&
    & position='rewind',status='old',iostat=ios,iomsg=message_local)

   if(ios /= 0)then
      lun=-1
      if(present(message))then
         message=trim(message_local)
      else
         write(*,gen)trim(message_local)
      endif
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>FILEOPEN:FILENAME=',filename,' LUN=',lun,' IOS=',IOS,' MESSAGE=',trim(message_local)

end function fileopen
!===================================================================================================================================
function get_env(NAME,DEFAULT) result(VALUE)
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
   ! get length required to hold value
   length=0
   if(NAME /= '')then
      call get_environment_variable(NAME, length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
          !x!print *, NAME, " is not defined in the environment. Strange..."
          VALUE=''
      case (2)
          !x!print *, "This processor doesn't support environment variables. Boooh!"
          VALUE=''
      case default
          ! make string to hold value of sufficient size
          if(allocated(value))deallocate(value)
          allocate(character(len=max(howbig,1)) :: VALUE)
          ! get value
         call get_environment_variable(NAME,VALUE,status=stat,trim_name=.true.)
          if(stat /= 0)VALUE=''
      end select
   else
      VALUE=''
   endif
   if(VALUE == ''.and.present(DEFAULT))VALUE=DEFAULT
end function get_env
!===================================================================================================================================
function join_path(a1,a2,a3,a4,a5) result(path)
   ! Construct path by joining strings with os file separator
   !
   character(len=*), intent(in)           :: a1, a2
   character(len=*), intent(in), optional :: a3, a4, a5
   character(len=:), allocatable          :: path
   character(len=1)                       :: filesep

   filesep = separator()
   if(a1 /= '')then
      path = trim(a1) // filesep // trim(a2)
   else
      path = trim(a2)
   endif
   if (present(a3)) path = path // filesep // trim(a3)
   if (present(a4)) path = path // filesep // trim(a4)
   if (present(a5)) path = path // filesep // trim(a5)
   path=adjustl(path//'  ')
   path=path(1:1)//replace_str(path,filesep//filesep,'') ! some systems allow names starting with '//' or '\\'
   path=trim(path)
end function join_path
!===================================================================================================================================
function get_name() result(name)
! get the pathname of arg0
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
character(len=4096)          :: long_name
character(len=:),allocatable :: name
   arg0_length=0
   name=''
   long_name=''
   call get_command_argument(0,length=arg0_length,status=istat)
   if(istat == 0)then
      if(allocated(arg0))deallocate(arg0)
      allocate(character(len=arg0_length) :: arg0)
      call get_command_argument(0,arg0,status=istat)
      if(istat == 0)then
         inquire(file=arg0,iostat=istat,name=long_name)
         name=trim(long_name)
      else
         name=arg0
      endif
   endif
end function get_name
!===================================================================================================================================
function basename(path,suffix) result (base)
    ! Extract filename from path with/without suffix
    !
character(*), intent(In) :: path
logical, intent(in), optional :: suffix
character(:), allocatable :: base

character(:), allocatable :: file_parts(:)
logical :: with_suffix

   if (.not.present(suffix)) then
      with_suffix = .true.
   else
      with_suffix = suffix
   endif

   if (with_suffix) then
      call split(path,file_parts,delimiters='\/')
      if(size(file_parts) > 0)then
         base = trim(file_parts(size(file_parts)))
      else
         base = ''
      endif
   else
      call split(path,file_parts,delimiters='\/.')
      if(size(file_parts) >= 2)then
         base = trim(file_parts(size(file_parts)-1))
      elseif(size(file_parts) == 1)then
         base = trim(file_parts(1))
      else
         base = ''
      endif
   endif
end function basename
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function separator() result(sep)
!>
!!##NAME
!!    separator(3f) - [M_io:ENVIRONMENT] try to determine pathname directory separator character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function separator() result(sep)
!!
!!     character(len=1) :: sep
!!
!!##DESCRIPTION
!!   First testing for the existence of "/.",  then if that fails a list
!!   of variable names assumed to contain directory paths {PATH|HOME} are
!!   examined first for a backslash, then a slash. Assuming basically the
!!   choice is a ULS or MSWindows system, and users can do weird things like
!!   put a backslash in a ULS path and break it.
!!
!!   Therefore can be very system dependent. If the queries fail the
!!   default returned is "/".
!!
!!##EXAMPLE
!!
!!   sample usage
!!
!!    program demo_separator
!!    use M_io, only : separator
!!    implicit none
!!       write(*,*)'separator=',separator()
!!    end program demo_separator

! use the pathname returned as arg0 to determine pathname separator
integer                      :: ios
integer                      :: i
logical                      :: existing=.false.
character(len=1)             :: sep
!x!IFORT BUG:character(len=1),save        :: sep_cache=' '
integer,save                 :: isep=-1
character(len=4096)          :: name
character(len=:),allocatable :: envnames(:)

    ! NOTE:  A parallel code might theoretically use multiple OS
    !x!FORT BUG:if(sep_cache /= ' ')then  ! use cached value.
    !x!FORT BUG:    sep=sep_cache
    !x!FORT BUG:    return
    !x!FORT BUG:endif
    if(isep /= -1)then  ! use cached value.
        sep=char(isep)
        return
    endif
    FOUND: block
    ! simple, but does not work with ifort
    ! most MSWindows environments see to work with backslash even when
    ! using POSIX filenames to do not rely on '\.'.
    inquire(file='/.',exist=existing,iostat=ios,name=name)
    if(existing.and.ios == 0)then
        sep='/'
        exit FOUND
    endif
    ! check variables names common to many platforms that usually have a
    ! directory path in them although a ULS file can contain a backslash
    ! and vice-versa (eg. "touch A\\B\\C"). Removed HOMEPATH because it
    ! returned a name with backslash on CygWin, Mingw, WLS even when using
    ! POSIX filenames in the environment.
    envnames=[character(len=10) :: 'PATH', 'HOME']
    do i=1,size(envnames)
       if(index(get_env(envnames(i)),'\') /= 0)then
          sep='\'
          exit FOUND
       elseif(index(get_env(envnames(i)),'/') /= 0)then
          sep='/'
          exit FOUND
       endif
    enddo

    write(*,*)'<WARNING>unknown system directory path separator'
    sep='\'
    endblock FOUND
    !x!IFORT BUG:sep_cache=sep
    isep=ichar(sep)
end function separator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_args_to_dictionary()
! convert command line arguments to dictionary entries
!x!logical                      :: guess_if_value
integer                      :: pointer
character(len=:),allocatable :: lastkeyword
integer                      :: i, jj, kk
integer                      :: ilength, istatus, imax
character(len=1)             :: letter
character(len=:),allocatable :: current_argument
character(len=:),allocatable :: current_argument_padded
character(len=:),allocatable :: dummy
character(len=:),allocatable :: oldvalue
logical                      :: nomore
logical                      :: next_mandatory
   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START'
   next_mandatory=.false.
   nomore=.false.
   pointer=0
   lastkeyword=' '
   G_keyword_single_letter=.true.
   i=1
   current_argument=''
   GET_ARGS: do while (get_next_argument()) ! insert and replace entries
      if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:WHILE:CURRENT_ARGUMENT=',current_argument

      if( current_argument  ==  '-' .and. nomore .eqv. .true. )then   ! sort of
      elseif( current_argument  ==  '-')then                          ! sort of
         current_argument='"stdin"'
      endif
      if( current_argument  ==  '--' .and. nomore .eqv. .true. )then  ! -- was already encountered
      elseif( current_argument  ==  '--' )then                        ! everything after this goes into the unnamed array
         nomore=.true.
         pointer=0
         if(G_remaining_option_allowed)then
            G_remaining_on=.true.
         endif
         cycle GET_ARGS
      endif

      dummy=current_argument//'   '
      current_argument_padded=current_argument//'   '

      if(.not.next_mandatory.and..not.nomore.and.current_argument_padded(1:2) == '--')then    ! beginning of long word
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START_LONG:'
         G_keyword_single_letter=.false.
         if(lastkeyword /= '')then
            call ifnull()
         endif
         call locate_key(current_argument_padded(3:),pointer)
         if(pointer <= 0)then
            if(G_QUIET)then
               lastkeyword="UNKNOWN"
               pointer=0
               cycle GET_ARGS
            endif
            call print_dictionary('UNKNOWN LONG KEYWORD: '//current_argument)
            call mystop(1)
            return
         endif
         lastkeyword=trim(current_argument_padded(3:))
         next_mandatory=mandatory(pointer)
      elseif(.not.next_mandatory &
      & .and..not.nomore &
      & .and.current_argument_padded(1:1) == '-' &
      & .and.index("0123456789.",dummy(2:2)) == 0)then
      ! short word
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START_SHORT'
         G_keyword_single_letter=.true.
         if(lastkeyword /= '')then
            call ifnull()
         endif
         call locate_key(current_argument_padded(2:),pointer)
         jj=len(current_argument)
         if( (pointer <= 0.or.jj.ge.3).and.(G_STRICT) )then  ! name not found
            if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:SHORT NOT FOUND:',current_argument_padded(2:)
            ! in strict mode this might be multiple single-character values
            do kk=2,jj
               letter=current_argument_padded(kk:kk)
               call locate_key(letter,pointer)
               if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:LETTER:',letter,pointer
               if(pointer > 0)then
                  call update(keywords(pointer),'T')
               else
                  if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:UNKNOWN SHORT:',letter
                  call print_dictionary('UNKNOWN SHORT KEYWORD:'//letter) ! //' in '//current_argument)
                  if(G_QUIET)then
                     lastkeyword="UNKNOWN"
                     pointer=0
                     cycle GET_ARGS
                  endif
                  call mystop(2)
                  return
               endif
               current_argument='-'//current_argument_padded(jj:jj)
            enddo
            !--------------
            lastkeyword=""
            pointer=0
            if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:SHORT_END:2:'
            cycle GET_ARGS
            !--------------
         elseif(pointer<0)then
            if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:UNKNOWN SHORT_CONFIRMED:',letter
            call print_dictionary('UNKNOWN SHORT KEYWORD:'//current_argument_padded(2:))
            if(G_QUIET)then
               lastkeyword="UNKNOWN"
               pointer=0
               cycle GET_ARGS
            endif
            call mystop(2)
            return
         endif
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:SHORT_END:1:'
         lastkeyword=trim(current_argument_padded(2:))
         next_mandatory=mandatory(pointer)
      elseif(pointer == 0)then                                       ! unnamed arguments
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:UNNAMED ARGUMENT:',current_argument
         if(G_remaining_on)then
            if(len(current_argument) < 1)then
               G_remaining=G_remaining//'"" '
            elseif(current_argument(1:1) == '-')then
               !get fancier to handle spaces and =!G_remaining=G_remaining//current_argument//' '
               G_remaining=G_remaining//'"'//current_argument//'" '
            else
               G_remaining=G_remaining//'"'//current_argument//'" '
            endif
            imax=max(len(args),len(current_argument))
            args=[character(len=imax) :: args,current_argument]
         else
            imax=max(len(unnamed),len(current_argument))
            if(scan(current_argument//' ','@') == 1.and.G_response)then
               if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:1:CALL EXPAND_RESPONSE:CURRENT_ARGUMENT=',current_argument
               call expand_response(current_argument)
            else
               unnamed=[character(len=imax) :: unnamed,current_argument]
            endif
         endif
      else
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:FOUND:',current_argument
         oldvalue=get(keywords(pointer))//' '
         if(oldvalue(1:1) == '"')then
            current_argument=quote(current_argument(:ilength))
         endif
         if(upper(oldvalue) == 'F'.or.upper(oldvalue) == 'T')then  ! assume boolean parameter
            if(current_argument /= ' ')then
               if(G_remaining_on)then
                  if(len(current_argument) < 1)then
                        G_remaining=G_remaining//'"" '
                  elseif(current_argument(1:1) == '-')then
                       !get fancier to handle spaces and =!G_remaining=G_remaining//current_argument//' '
                        G_remaining=G_remaining//'"'//current_argument//'" '
                  else
                        G_remaining=G_remaining//'"'//current_argument//'" '
                  endif
                  imax=max(len(args),len(current_argument))
                  args=[character(len=imax) :: args,current_argument]
               else
                  imax=max(len(unnamed),len(current_argument))
                  if(scan(current_argument//' ','@') == 1.and.G_response)then
                    if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:2:CALL EXPAND_RESPONSE:CURRENT_ARGUMENT=',current_argument
                    call expand_response(current_argument)
                  else
                    unnamed=[character(len=imax) :: unnamed,current_argument]
                  endif
               endif
            endif
            current_argument='T'
         endif
         call update(keywords(pointer),current_argument)
         pointer=0
         lastkeyword=''
         next_mandatory=.false.
      endif
   enddo GET_ARGS
   if(lastkeyword /= '')then
      call ifnull()
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:NORMAL END'

contains

subroutine ifnull()
   oldvalue=get(lastkeyword)//' '
   if(upper(oldvalue) == 'F'.or.upper(oldvalue) == 'T')then
      call update(lastkeyword,'T')
   elseif(oldvalue(1:1) == '"')then
      call update(lastkeyword,'" "')
   else
      call update(lastkeyword,' ')
   endif
end subroutine ifnull

function get_next_argument()
!
! get next argument from command line into allocated variable current_argument
!
logical,save :: hadequal=.false.
character(len=:),allocatable,save :: right_hand_side
logical :: get_next_argument
integer :: iright
integer :: iequal

   if(hadequal)then  ! use left-over value from previous -NAME=VALUE syntax
      current_argument=right_hand_side
      right_hand_side=''
      hadequal=.false.
      get_next_argument=.true.
      ilength=len(current_argument)
      return
   endif

   if(i>command_argument_count())then
      get_next_argument=.false.
      return
   else
      get_next_argument=.true.
   endif

   call get_command_argument(number=i,length=ilength,status=istatus)                              ! get next argument
   if(istatus /= 0) then                                                                          ! on error
      write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
         &'status=',istatus,&
         &'length=',ilength
      get_next_argument=.false.
   else
      ilength=max(ilength,1)
      if(allocated(current_argument))deallocate(current_argument)
      allocate(character(len=ilength) :: current_argument)
      call get_command_argument(number=i,value=current_argument,length=ilength,status=istatus)    ! get next argument
      if(istatus /= 0) then                                                                       ! on error
         write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
            &'status=',istatus,&
            &'length=',ilength,&
            &'target length=',len(current_argument)
         get_next_argument=.false.
       endif

       ! if an argument keyword and an equal before a space split on equal and save right hand side for next call
       if(nomore)then
       elseif( len(current_argument) == 0)then
       else
          iright=index(current_argument,' ')
          if(iright == 0)iright=len(current_argument)
          iequal=index(current_argument(:iright),'=')
          if(next_mandatory)then
          elseif(iequal /= 0.and.current_argument(1:1) == '-')then
             if(iequal /= len(current_argument))then
                right_hand_side=current_argument(iequal+1:)
             else
                right_hand_side=''
             endif
             hadequal=.true.
             current_argument=current_argument(:iequal-1)
          endif
       endif
   endif
   i=i+1
end function get_next_argument

end subroutine cmd_args_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    print_dictionary(3f) - [ARGUMENTS:M_CLI2] print internal dictionary
!!    created by calls to set_args(3f)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine print_dictionary(header,stop)
!!
!!      character(len=*),intent(in),optional :: header
!!      logical,intent(in),optional          :: stop
!!##DESCRIPTION
!!    Print the internal dictionary created by calls to set_args(3f).
!!    This routine is intended to print the state of the argument list
!!    if an error occurs in using the set_args(3f) procedure.
!!##OPTIONS
!!     HEADER  label to print before printing the state of the command
!!             argument list.
!!     STOP    logical value that if true stops the program after displaying
!!             the dictionary.
!!##EXAMPLE
!!
!!
!!
!! Typical usage:
!!
!!       program demo_print_dictionary
!!       use M_CLI2,  only : set_args, get_args
!!       implicit none
!!       real :: x, y, z
!!          call set_args('-x 10 -y 20 -z 30')
!!          call get_args('x',x,'y',y,'z',z)
!!          ! all done cracking the command line; use the values in your program.
!!          write(*,*)x,y,z
!!       end program demo_print_dictionary
!!
!!      Sample output
!!
!!      Calling the sample program with an unknown parameter or the --usage
!!      switch produces the following:
!!
!!         $ ./demo_print_dictionary -A
!!         UNKNOWN SHORT KEYWORD: -A
!!         KEYWORD             PRESENT  VALUE
!!         z                   F        [3]
!!         y                   F        [2]
!!         x                   F        [1]
!!         help                F        [F]
!!         version             F        [F]
!!         usage               F        [F]
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine print_dictionary(header,stop)
character(len=*),intent(in),optional :: header
logical,intent(in),optional          :: stop
integer          :: i
   if(G_QUIET)return
   if(present(header))then
      if(header /= '')then
         write(warn,'(a)')header
      endif
   endif
   if(allocated(keywords))then
      if(size(keywords) > 0)then
         write(warn,'(a,1x,a,1x,a,1x,a)')atleast('KEYWORD',max(len(keywords),8)),'SHORT','PRESENT','VALUE'
         write(warn,'(*(a,1x,a5,1x,l1,8x,"[",a,"]",/))') &
         & (atleast(keywords(i),max(len(keywords),8)),shorts(i),present_in(i),values(i)(:counts(i)),i=size(keywords),1,-1)
      endif
   endif
   if(allocated(unnamed))then
      if(size(unnamed) > 0)then
         write(warn,'(a)')'UNNAMED'
         write(warn,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
      endif
   endif
   if(allocated(args))then
      if(size(args) > 0)then
         write(warn,'(a)')'ARGS'
         write(warn,'(i6.6,3a)')(i,'[',args(i),']',i=1,size(args))
      endif
   endif
   if(G_remaining /= '')then
      write(warn,'(a)')'REMAINING'
      write(warn,'(a)')G_remaining
   endif
   if(present(stop))then
      if(stop) call mystop(5)
   endif
end subroutine print_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_args(3f) - [ARGUMENTS:M_CLI2] return keyword values when parsing
!!    command line arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   get_args(3f) and its convenience functions:
!!
!!     use M_CLI2, only : get_args
!!     ! convenience functions
!!     use M_CLI2, only : dget, iget, lget, rget, sget, cget
!!     use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets
!!
!!     subroutine get_args(name,value,delimiters)
!!
!!      character(len=*),intent(in) :: name
!!
!!      type(${TYPE}),allocatable,intent(out) :: value(:)
!!      ! or
!!      type(${TYPE}),allocatable,intent(out) :: value
!!
!!      character(len=*),intent(in),optional :: delimiters
!!
!!      where ${TYPE} may be from the set
!!              {real,doubleprecision,integer,logical,complex,character(len=:)}
!!##DESCRIPTION
!!
!!    GET_ARGS(3f) returns the value of keywords after SET_ARGS(3f) has
!!    been called to parse the command line. For fixed-length CHARACTER
!!    variables see GET_ARGS_FIXED_LENGTH(3f). For fixed-size arrays see
!!    GET_ARGS_FIXED_SIZE(3f).
!!
!!    As a convenience multiple pairs of keywords and variables may be
!!    specified if and only if all the values are scalars and the CHARACTER
!!    variables are fixed-length or pre-allocated.
!!
!!##OPTIONS
!!
!!     NAME        name of commandline argument to obtain the value of
!!     VALUE       variable to hold returned value. The kind of the value
!!                 is used to determine the type of returned value. May
!!                 be a scalar or allocatable array. If type is CHARACTER
!!                 the scalar must have an allocatable length.
!!     DELIMITERS  By default the delimiter for array values are comma,
!!                 colon, and whitespace. A string containing an alternate
!!                 list of delimiter characters may be supplied.
!!
!!##CONVENIENCE FUNCTIONS
!!    There are convenience functions that are replacements for calls to
!!    get_args(3f) for each supported default intrinsic type
!!
!!      o scalars -- dget(3f), iget(3f), lget(3f), rget(3f), sget(3f),
!!                   cget(3f)
!!      o vectors -- dgets(3f), igets(3f), lgets(3f), rgets(3f),
!!                   sgets(3f), cgets(3f)
!!
!!    D is for DOUBLEPRECISION, I for INTEGER, L for LOGICAL, R for REAL,
!!    S for string (CHARACTER), and C for COMPLEX.
!!
!!    If the functions are called with no argument they will return the
!!    UNNAMED array converted to the specified type.
!!
!!##EXAMPLE
!!
!!
!! Sample program:
!!
!!     program demo_get_args
!!     use M_CLI2,  only : filenames=>unnamed, set_args, get_args
!!     implicit none
!!     integer                      :: i
!!      ! Define ARGS
!!     real                         :: x, y, z
!!     real,allocatable             :: p(:)
!!     character(len=:),allocatable :: title
!!     logical                      :: l, lbig
!!      ! Define and parse (to set initial values) command line
!!      !   o only quote strings and use double-quotes
!!      !   o set all logical values to F or T.
!!     call set_args('         &
!!        & -x 1 -y 2 -z 3     &
!!        & -p -1,-2,-3        &
!!        & --title "my title" &
!!        & -l F -L F          &
!!        & --label " "        &
!!        & ')
!!      ! Assign values to elements
!!      ! Scalars
!!     call get_args('x',x,'y',y,'z',z,'l',l,'L',lbig)
!!      ! Allocatable string
!!     call get_args('title',title)
!!      ! Allocatable arrays
!!     call get_args('p',p)
!!      ! Use values
!!     write(*,'(1x,g0,"=",g0)')'x',x, 'y',y, 'z',z
!!     write(*,*)'p=',p
!!     write(*,*)'title=',title
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     if(size(filenames) > 0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!     end program demo_get_args
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!>
!!##NAME
!!    get_args_fixed_length(3f) - [ARGUMENTS:M_CLI2] return keyword values
!!    for fixed-length string when parsing command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine get_args_fixed_length(name,value)
!!
!!     character(len=*),intent(in)  :: name
!!     character(len=:),allocatable :: value
!!     character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    get_args_fixed_length(3f) returns the value of a string
!!    keyword when the string value is a fixed-length CHARACTER
!!    variable.
!!
!!##OPTIONS
!!
!!    NAME   name of commandline argument to obtain the value of
!!
!!    VALUE  variable to hold returned value.
!!           Must be a fixed-length CHARACTER variable.
!!
!!    DELIMITERS  By default the delimiter for array values are comma,
!!                colon, and whitespace. A string containing an alternate
!!                list of delimiter characters may be supplied.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!     program demo_get_args_fixed_length
!!     use M_CLI2,  only : set_args, get_args_fixed_length
!!     implicit none
!!
!!      ! Define args
!!     character(len=80)   :: title
!!      ! Parse command line
!!     call set_args(' --title "my title" ')
!!      ! Assign values to variables
!!     call get_args_fixed_length('title',title)
!!      ! Use values
!!     write(*,*)'title=',title
!!
!!     end program demo_get_args_fixed_length
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!>
!!##NAME
!!    get_args_fixed_size(3f) - [ARGUMENTS:M_CLI2] return keyword values
!!    for fixed-size array when parsing command line arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine get_args_fixed_size(name,value)
!!
!!     character(len=*),intent(in) :: name
!!     [real|doubleprecision|integer|logical|complex] :: value(NNN)
!!        or
!!     character(len=MMM) :: value(NNN)
!!
!!     character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    get_args_fixed_size(3f) returns the value of keywords for fixed-size
!!    arrays after set_args(3f) has been called. On input on the command
!!    line all values of the array must be specified.
!!
!!##OPTIONS
!!    NAME        name of commandline argument to obtain the value of
!!
!!    VALUE       variable to hold returned values. The kind of the value
!!                is used to determine the type of returned value. Must be
!!                a fixed-size array. If type is CHARACTER the length must
!!                also be fixed.
!!
!!    DELIMITERS  By default the delimiter for array values are comma,
!!                colon, and whitespace. A string containing an alternate
!!                list of delimiter characters may be supplied.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!     program demo_get_args_fixed_size
!!     use M_CLI2,  only : set_args, get_args_fixed_size
!!     implicit none
!!     integer,parameter   :: dp=kind(0.0d0)
!!     ! DEFINE ARGS
!!     real                :: x(2)
!!     real(kind=dp)       :: y(2)
!!     integer             :: p(3)
!!     character(len=80)   :: title(1)
!!     logical             :: l(4), lbig(4)
!!     complex             :: cmp(2)
!!     ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!     !   o only quote strings
!!     !   o set all logical values to F or T.
!!     call set_args(' &
!!        & -x 10.0,20.0 &
!!        & -y 11.0,22.0 &
!!        & -p -1,-2,-3 &
!!        & --title "my title" &
!!        & -l F,T,F,T -L T,F,T,F  &
!!        & --cmp 111,222.0,333.0e0,4444 &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!        call get_args_fixed_size('x',x)
!!        call get_args_fixed_size('y',y)
!!        call get_args_fixed_size('p',p)
!!        call get_args_fixed_size('title',title)
!!        call get_args_fixed_size('l',l)
!!        call get_args_fixed_size('L',lbig)
!!        call get_args_fixed_size('cmp',cmp)
!!     ! USE VALUES
!!        write(*,*)'x=',x
!!        write(*,*)'p=',p
!!        write(*,*)'title=',title
!!        write(*,*)'l=',l
!!        write(*,*)'L=',lbig
!!        write(*,*)'cmp=',cmp
!!     end program demo_get_args_fixed_size
!!   Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine get_fixedarray_class(keyword,generic,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
class(*)                             :: generic(:)
character(len=*),intent(in),optional :: delimiters
   select type(generic)
    type is (character(len=*));  call get_fixedarray_fixed_length_c(keyword,generic,delimiters)
    type is (integer);           call get_fixedarray_i(keyword,generic,delimiters)
    type is (real);              call get_fixedarray_r(keyword,generic,delimiters)
    type is (complex);           call get_fixed_size_complex(keyword,generic,delimiters)
    type is (real(kind=dp));     call get_fixedarray_d(keyword,generic,delimiters)
    type is (logical);           call get_fixedarray_l(keyword,generic,delimiters)
    class default
      call mystop(-7,'*get_fixedarray_class* crud -- procedure does not know about this type')
   end select
end subroutine get_fixedarray_class
!===================================================================================================================================
! return allocatable arrays
!===================================================================================================================================
subroutine get_anyarray_l(keyword,larray,delimiters)

! ident_5="@(#) M_CLI2 get_anyarray_l(3f) given keyword fetch logical array from string in dictionary(F on err)"

character(len=*),intent(in)  :: keyword                    ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
logical,allocatable          :: larray(:)                  ! convert value to an array
character(len=*),intent(in),optional   :: delimiters
character(len=:),allocatable :: carray(:)                  ! convert value to an array
character(len=:),allocatable :: val
integer                      :: i
integer                      :: place
integer                      :: iichar                     ! point to first character of word unless first character is "."
   call locate_key(keyword,place)                          ! find where string is or should be
   if(place > 0)then                                      ! if string was found
      val=values(place)(:counts(place))
      call split(adjustl(upper(val)),carray,delimiters=delimiters)  ! convert value to uppercase, trimmed; then parse into array
   else
      call journal('*get_anyarray_l* unknown keyword',keyword)
      call mystop(8 ,'*get_anyarray_l* unknown keyword '//keyword)
      if(allocated(larray))deallocate(larray)
      allocate(larray(0))
      return
   endif
   if(size(carray) > 0)then                                  ! if not a null string
      if(allocated(larray))deallocate(larray)
      allocate(larray(size(carray)))                          ! allocate output array
      do i=1,size(carray)
         larray(i)=.false.                                    ! initialize return value to .false.
         if(carray(i)(1:1) == '.')then                        ! looking for fortran logical syntax .STRING.
            iichar=2
         else
            iichar=1
         endif
         select case(carray(i)(iichar:iichar))             ! check word to see if true or false
         case('T','Y',' '); larray(i)=.true.               ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
         case('F','N');     larray(i)=.false.              ! assume this is false or no
         case default
            call journal("*get_anyarray_l* bad logical expression for ",(keyword),'=',carray(i))
         end select
      enddo
   else                                                       ! for a blank string return one T
      if(allocated(larray))deallocate(larray)
      allocate(larray(1))                                     ! allocate output array
      larray(1)=.true.
   endif
end subroutine get_anyarray_l
!===================================================================================================================================
subroutine get_anyarray_d(keyword,darray,delimiters)

! ident_6="@(#) M_CLI2 get_anyarray_d(3f) given keyword fetch dble value array from Language Dictionary (0 on err)"

character(len=*),intent(in)           :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp),allocatable,intent(out) :: darray(:)    ! function type
character(len=*),intent(in),optional  :: delimiters

character(len=:),allocatable          :: carray(:)    ! convert value to an array using split(3f)
integer                               :: i
integer                               :: place
integer                               :: ierr
character(len=:),allocatable          :: val
!-----------------------------------------------------------------------------------------------------------------------------------
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                 ! if string was found
      val=values(place)(:counts(place))
      val=replace_str(val,'(','')
      val=replace_str(val,')','')
      call split(val,carray,delimiters=delimiters)    ! find value associated with keyword and split it into an array
   else
      call journal('*get_anyarray_d* unknown keyword '//keyword)
      call mystop(9 ,'*get_anyarray_d* unknown keyword '//keyword)
      if(allocated(darray))deallocate(darray)
      allocate(darray(0))
      return
   endif
   if(allocated(darray))deallocate(darray)
   allocate(darray(size(carray)))                     ! create the output array
   do i=1,size(carray)
      call a2d(carray(i), darray(i),ierr) ! convert the string to a numeric value
      if(ierr /= 0)then
         call mystop(10 ,'*get_anyarray_d* unreadable value '//carray(i)//' for keyword '//keyword)
      endif
   enddo
end subroutine get_anyarray_d
!===================================================================================================================================
subroutine get_anyarray_i(keyword,iarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
integer,allocatable                  :: iarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
   call get_anyarray_d(keyword,darray,delimiters)
   iarray=nint(darray)
end subroutine get_anyarray_i
!===================================================================================================================================
subroutine get_anyarray_r(keyword,rarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real,allocatable                     :: rarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
   call get_anyarray_d(keyword,darray,delimiters)
   rarray=real(darray)
end subroutine get_anyarray_r
!===================================================================================================================================
subroutine get_anyarray_x(keyword,xarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
complex(kind=sp),allocatable         :: xarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: half,sz,i
   call get_anyarray_d(keyword,darray,delimiters)
   sz=size(darray)
   half=sz/2
   if(sz /= half+half)then
      call journal('*get_anyarray_x* uneven number of values defining complex value '//keyword)
      call mystop(11,'*get_anyarray_x* uneven number of values defining complex value '//keyword)
      if(allocated(xarray))deallocate(xarray)
      allocate(xarray(0))
   endif

   !x!================================================================================================
   !x!IFORT,GFORTRAN OK, NVIDIA RETURNS NULL ARRAY: xarray=cmplx(real(darray(1::2)),real(darray(2::2)))
   if(allocated(xarray))deallocate(xarray)
   allocate(xarray(half))
   do i=1,sz,2
      xarray((i+1)/2)=cmplx( darray(i),darray(i+1),kind=sp )
   enddo
   !x!================================================================================================

end subroutine get_anyarray_x
!===================================================================================================================================
subroutine get_anyarray_c(keyword,strings,delimiters)

! ident_8="@(#)M_CLI2::get_anyarray_c(3f): Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)          :: keyword       ! name to look up in dictionary
character(len=:),allocatable         :: strings(:)
character(len=*),intent(in),optional :: delimiters
integer                              :: place
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,strings,delimiters=delimiters)   ! find value associated with keyword and split it into an array
   else
      call journal('*get_anyarray_c* unknown keyword '//keyword)
      call mystop(12,'*get_anyarray_c* unknown keyword '//keyword)
      if(allocated(strings))deallocate(strings)
      allocate(character(len=0)::strings(0))
   endif
end subroutine get_anyarray_c
!===================================================================================================================================
!===================================================================================================================================
subroutine get_args_fixed_length_a_array(keyword,strings,delimiters)

! ident_7="@(#) M_CLI2 get_args_fixed_length_a_array(3f) Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)          :: keyword       ! name to look up in dictionary
character(len=*),allocatable         :: strings(:)
character(len=*),intent(in),optional :: delimiters
character(len=:),allocatable         :: strings_a(:)
integer                              :: place
character(len=:),allocatable         :: val
integer                              :: ibug
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,strings_a,delimiters=delimiters)   ! find value associated with keyword and split it into an array
      if( len(strings_a) <= len(strings) )then
         strings=strings_a
      else
         ibug=len(strings)
         call journal('*get_args_fixed_length_a_array* values too long. Longest is',len(strings_a),'allowed is',ibug)
         write(*,'("strings=",3x,*(a,1x))')strings
         call journal('*get_args_fixed_length_a_array* keyword='//keyword)
         call mystop(13,'*get_args_fixed_length_a_array* keyword='//keyword)
         strings=[character(len=len(strings)) ::]
      endif
   else
      call journal('*get_args_fixed_length_a_array* unknown keyword '//keyword)
      call mystop(14,'*get_args_fixed_length_a_array* unknown keyword '//keyword)
      strings=[character(len=len(strings)) ::]
   endif
end subroutine get_args_fixed_length_a_array
!===================================================================================================================================
! return non-allocatable arrays
!===================================================================================================================================
subroutine get_fixedarray_i(keyword,iarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
integer                              :: iarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_d(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(iarray,dim=1) == dsize)then
      iarray=nint(darray)
   else
      ibug=size(iarray)
      call journal('*get_fixedarray_i* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary('USAGE:')
      call mystop(33)
      iarray=0
   endif
end subroutine get_fixedarray_i
!===================================================================================================================================
subroutine get_fixedarray_r(keyword,rarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real                                 :: rarray(:)
character(len=*),intent(in),optional :: delimiters
real,allocatable                     :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_r(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(rarray,dim=1) == dsize)then
      rarray=darray
   else
      ibug=size(rarray)
      call journal('*get_fixedarray_r* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary('USAGE:')
      call mystop(33)
      rarray=0.0
   endif
end subroutine get_fixedarray_r
!===================================================================================================================================
subroutine get_fixed_size_complex(keyword,xarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
complex                              :: xarray(:)
character(len=*),intent(in),optional :: delimiters
complex,allocatable                  :: darray(:)    ! function type
integer                              :: half, sz
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_x(keyword,darray,delimiters)
   dsize=size(darray)
   sz=dsize*2
   half=sz/2
   if(sz /= half+half)then
      call journal('*get_fixed_size_complex* uneven number of values defining complex value '//keyword)
      call mystop(15,'*get_fixed_size_complex* uneven number of values defining complex value '//keyword)
      xarray=0
      return
   endif
   if(ubound(xarray,dim=1) == dsize)then
      xarray=darray
   else
      ibug=size(xarray)
      call journal('*get_fixed_size_complex* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary('USAGE:')
      call mystop(34)
      xarray=cmplx(0.0,0.0)
   endif
end subroutine get_fixed_size_complex
!===================================================================================================================================
subroutine get_fixedarray_d(keyword,darr,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp)                        :: darr(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_d(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(darr,dim=1) == dsize)then
      darr=darray
   else
      ibug=size(darr)
      call journal('*get_fixedarray_d* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary('USAGE:')
      call mystop(35)
      darr=0.0d0
   endif
end subroutine get_fixedarray_d
!===================================================================================================================================
subroutine get_fixedarray_l(keyword,larray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
logical                              :: larray(:)
character(len=*),intent(in),optional :: delimiters
logical,allocatable                  :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_l(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(larray,dim=1) == dsize)then
      larray=darray
   else
      ibug=size(larray)
      call journal('*get_fixedarray_l* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary('USAGE:')
      call mystop(36)
      larray=.false.
   endif
end subroutine get_fixedarray_l
!===================================================================================================================================
subroutine get_fixedarray_fixed_length_c(keyword,strings,delimiters)

! ident_8="@(#) M_CLI2 get_fixedarray_fixed_length_c(3f) Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*)                     :: strings(:)
character(len=*),intent(in),optional :: delimiters
character(len=:),allocatable         :: str(:)
character(len=*),intent(in)          :: keyword   ! name to look up in dictionary
integer                              :: place
integer                              :: ssize
integer                              :: ibug
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                 ! find where string is or should be
   if(place > 0)then                              ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,str,delimiters=delimiters)   ! find value associated with keyword and split it into an array
      ssize=size(str)
      if(ssize==size(strings))then
         strings(:ssize)=str
      else
         ibug=size(strings)
         call journal('*get_fixedarray_fixed_length_c* wrong number of values for keyword',&
            & keyword,'got',ssize,'expected ',ibug) !,ubound(strings,dim=1)
         call print_dictionary('USAGE:')
         call mystop(30,'*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
         strings=''
      endif
   else
      call journal('*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
      call mystop(16,'*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
      strings=''
   endif
end subroutine get_fixedarray_fixed_length_c
!===================================================================================================================================
! return scalars
!===================================================================================================================================
subroutine get_scalar_d(keyword,d)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp)                 :: d
real(kind=dp),allocatable     :: darray(:)    ! function type
integer                       :: ibug
   call get_anyarray_d(keyword,darray)
   if(size(darray) == 1)then
      d=darray(1)
   else
      ibug=size(darray)
      call journal('*get_anyarray_d* incorrect number of values for keyword "',keyword,'" expected one found',ibug)
      call print_dictionary('USAGE:')
      call mystop(31,'*get_anyarray_d* incorrect number of values for keyword "'//keyword//'" expected one')
   endif
end subroutine get_scalar_d
!===================================================================================================================================
subroutine get_scalar_real(keyword,r)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
real,intent(out)              :: r
real(kind=dp)                 :: d
   call get_scalar_d(keyword,d)
   r=real(d)
end subroutine get_scalar_real
!===================================================================================================================================
subroutine get_scalar_i(keyword,i)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
integer,intent(out)           :: i
real(kind=dp)                 :: d
   call get_scalar_d(keyword,d)
   i=nint(d)
end subroutine get_scalar_i
!===================================================================================================================================
subroutine get_scalar_anylength_c(keyword,string)

! ident_9="@(#) M_CLI2 get_scalar_anylength_c(3f) Fetch string value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)   :: keyword              ! name to look up in dictionary
character(len=:),allocatable,intent(out)  :: string
integer                       :: place
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return string
      string=unquote(values(place)(:counts(place)))
   else
      call mystop(17,'*get_anyarray_c* unknown keyword '//keyword)
      call journal('*get_anyarray_c* unknown keyword '//keyword)
      string=''
   endif
end subroutine get_scalar_anylength_c
!===================================================================================================================================
elemental impure subroutine get_args_fixed_length_scalar_c(keyword,string)

! ident_10="@(#) M_CLI2 get_args_fixed_length_scalar_c(3f) Fetch string value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)   :: keyword              ! name to look up in dictionary
character(len=*),intent(out)  :: string
integer                       :: place
integer                       :: unlen
integer                       :: ibug
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return string
      string=unquote(values(place)(:counts(place)))
   else
      call mystop(18,'*get_args_fixed_length_scalar_c* unknown keyword '//keyword)
      string=''
   endif
   unlen=len_trim(unquote(values(place)(:counts(place))))
   if(unlen>len(string))then
      ibug=len(string)
      call journal('*get_args_fixed_length_scalar_c* value too long for',keyword,'allowed is',ibug,&
      & 'input string [',values(place),'] is',unlen)
      call mystop(19,'*get_args_fixed_length_scalar_c* value too long')
      string=''
   endif
end subroutine get_args_fixed_length_scalar_c
!===================================================================================================================================
subroutine get_scalar_complex(keyword,x)
character(len=*),intent(in) :: keyword      ! keyword to retrieve value from dictionary
complex,intent(out)         :: x
real(kind=dp)               :: d(2)
   call get_fixedarray_d(keyword,d)
   x=cmplx(d(1),d(2),kind=sp)
end subroutine get_scalar_complex
!===================================================================================================================================
subroutine get_scalar_logical(keyword,l)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
logical                       :: l
logical,allocatable           :: larray(:)    ! function type
integer                       :: ibug
   l=.false.
   call get_anyarray_l(keyword,larray)
   if(.not.allocated(larray) )then
      call journal('*get_scalar_logical* expected one value found not allocated')
      call mystop(37,'*get_scalar_logical* incorrect number of values for keyword "'//keyword//'"')
   elseif(size(larray) == 1)then
      l=larray(1)
   else
      ibug=size(larray)
      call journal('*get_scalar_logical* expected one value found',ibug)
      call mystop(21,'*get_scalar_logical* incorrect number of values for keyword "'//keyword//'"')
   endif
end subroutine get_scalar_logical
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! THE REMAINDER SHOULD BE ROUTINES EXTRACTED FROM OTHER MODULES TO MAKE THIS MODULE STANDALONE BY POPULAR REQUEST
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!use M_strings,                     only : UPPER, LOWER, QUOTE, REPLACE_STR=>REPLACE, UNQUOTE, SPLIT, STRING_TO_VALUE
!use M_list,                        only : insert, locate, remove, replace
!use M_journal,                     only : JOURNAL

!use M_args,                        only : LONGEST_COMMAND_ARGUMENT
! routines extracted from other modules
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    longest_command_argument(3f) - [ARGUMENTS:M_args] length of longest
!!    argument on command line
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function longest_command_argument() result(ilongest)
!!
!!     integer :: ilongest
!!
!!##DESCRIPTION
!!    length of longest argument on command line. Useful when allocating
!!    storage for holding arguments.
!!##RESULT
!!    longest_command_argument  length of longest command argument
!!##EXAMPLE
!!
!! Sample program
!!
!!      program demo_longest_command_argument
!!      use M_args, only : longest_command_argument
!!         write(*,*)'longest argument is ',longest_command_argument()
!!      end program demo_longest_command_argument
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
function longest_command_argument() result(ilongest)
integer :: i
integer :: ilength
integer :: istatus
integer :: ilongest
   ilength=0
   ilongest=0
   GET_LONGEST: do i=1,command_argument_count()                             ! loop throughout command line arguments to find longest
      call get_command_argument(number=i,length=ilength,status=istatus)     ! get next argument
      if(istatus /= 0) then                                                 ! on error
         write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining length for argument ',i
         exit GET_LONGEST
      elseif(ilength > 0)then
         ilongest=max(ilongest,ilength)
      endif
   enddo GET_LONGEST
end function longest_command_argument
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    journal(3f) - [M_CLI2] converts a list of standard scalar types to a string and writes message
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine journal(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
!!
!!     class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!     character(len=*),intent(in),optional :: sep
!!
!!##DESCRIPTION
!!    journal(3f) builds and prints a space-separated string from up to twenty scalar values.
!!
!!##OPTIONS
!!    g[0-9a-j]   optional value to print the value of after the message. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!
!!    sep         separator to place between values. Defaults to a space.
!!##RETURNS
!!    journal     description to print
!!##EXAMPLES
!!
!! Sample program:
!!
!!     program demo_journal
!!     use M_CLI2, only : journal
!!     implicit none
!!     character(len=:),allocatable :: frmt
!!     integer                      :: biggest
!!
!!     call journal('HUGE(3f) integers',huge(0),'and real',&
!!               & huge(0.0),'and double',huge(0.0d0))
!!     call journal('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!     call journal('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!     call journal('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!
!!     end program demo_journal
!!
!!  Output
!!
!!     HUGE(3f) integers 2147483647 and real 3.40282347E+38 and
!!     double 1.7976931348623157E+308
!!     real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!     doubleprecision : 1.7976931348623157E+308 0.0000000000000000
!!     12345.678900000001 2.2250738585072014E-308
!!     complex         : (3.40282347E+38,1.17549435E-38)
!!      format=(*(i9:,1x))
!!      program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine journal(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep)

! ident_11="@(#) M_CLI2 journal(3fp) writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional         :: g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
character(len=*),intent(in),optional :: sep
character(len=:),allocatable         :: sep_local
character(len=4096)                  :: line
integer                              :: istart
integer                              :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep_local)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=''
   if(present(g0))call print_generic(g0)
   if(present(g1))call print_generic(g1)
   if(present(g2))call print_generic(g2)
   if(present(g3))call print_generic(g3)
   if(present(g4))call print_generic(g4)
   if(present(g5))call print_generic(g5)
   if(present(g6))call print_generic(g6)
   if(present(g7))call print_generic(g7)
   if(present(g8))call print_generic(g8)
   if(present(g9))call print_generic(g9)
   if(present(ga))call print_generic(ga)
   if(present(gb))call print_generic(gb)
   if(present(gc))call print_generic(gc)
   if(present(gd))call print_generic(gd)
   if(present(ge))call print_generic(ge)
   if(present(gf))call print_generic(gf)
   if(present(gg))call print_generic(gg)
   if(present(gh))call print_generic(gh)
   if(present(gi))call print_generic(gi)
   if(present(gj))call print_generic(gj)
   write(*,'(a)')trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64))
         write(line(istart:),'(1pg0)') generic
      !x! DOES NOT WORK WITH NVFORTRAN: type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical)
         write(line(istart:),'(l1)') generic
      type is (character(len=*))
         write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end subroutine journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function upper(str) result (string)

! ident_12="@(#) M_CLI2 upper(3f) Changes a string to uppercase"

character(*), intent(in)      :: str
character(:),allocatable      :: string
integer                       :: i
   string = str
   do i = 1, len_trim(str)
       select case (str(i:i))
       case ('a':'z')
          string(i:i) = char(iachar(str(i:i))-32)
       end select
   end do
end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lower(str) result (string)

! ident_13="@(#) M_CLI2 lower(3f) Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(:),allocatable     :: string
integer                      :: i
   string = str
   do i = 1, len_trim(str)
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine a2i(chars,valu,ierr)

! ident_14="@(#) M_CLI2 a2i(3fp) subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
integer,parameter           :: ihuge=huge(0)
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8 <= huge(valu))then
      if(valu8 <= huge(valu))then
         valu=int(valu8)
      else
         call journal('*a2i*','- value too large',valu8,'>',ihuge)
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

! ident_15="@(#) M_CLI2 a2d(3fp) subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o  works with any g-format input, including integer, real, and exponential.
!  o  if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o  if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!     IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=unquote(chars)
   msg=''
   if(len(local_chars) == 0)local_chars=' '
   local_chars=replace_str(local_chars,',','')                  ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd /= 0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         write(frmt,"('(Z',i0,')')")len(local_chars)
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         write(frmt,"('(B',i0,')')")len(local_chars)
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         write(frmt,"('(O',i0,')')")len(local_chars)
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr /= 0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(f3.3)')valu
      endif
      if(local_chars /= 'eod')then                           ! print warning message except for special value "eod"
         call journal('*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg /= '')then
            call journal('*a2d* - ['//trim(msg)//']')
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split(3f) - [M_CLI2:TOKENS] parse string into an array using specified
!!    delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine split(input_line,array,delimiters,order,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=:),allocatable,intent(out) :: array(:)
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: order
!!     character(len=*),optional,intent(in)     :: nulls
!!##DESCRIPTION
!!    SPLIT(3f) parses a string using specified delimiter characters and
!!    store tokens into an allocatable array
!!
!!##OPTIONS
!!
!!    INPUT_LINE  Input string to tokenize
!!
!!    ARRAY       Output array of tokens
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    ORDER SEQUENTIAL|REVERSE|RIGHT  Order of output array.
!!                By default ARRAY contains the tokens having parsed
!!                the INPUT_LINE from left to right. If ORDER='RIGHT'
!!                or ORDER='REVERSE' the parsing goes from right to left.
!!
!!    NULLS IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!     program demo_split
!!     use M_CLI2, only: split
!!     character(len=*),parameter     :: &
!!     & line='  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!     character(len=:),allocatable :: array(:) ! output array of tokens
!!        write(*,*)'INPUT LINE:['//LINE//']'
!!        write(*,'(80("="))')
!!        write(*,*)'typical call:'
!!        CALL split(line,array)
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)'custom list of delimiters (colon and vertical line):'
!!        CALL split(line,array,delimiters=':|',order='sequential',nulls='ignore')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)&
!!      &'custom list of delimiters, reverse array order and count null fields:'
!!        CALL split(line,array,delimiters=':|',order='reverse',nulls='return')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)'INPUT LINE:['//LINE//']'
!!        write(*,*)&
!!        &'default delimiters and reverse array order and return null fields:'
!!        CALL split(line,array,delimiters='',order='reverse',nulls='return')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!     end program demo_split
!!
!!   Output
!!
!!    > INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    > ===========================================================================
!!    >  typical call:
!!    > 1 ==> aBcdef
!!    > 2 ==> ghijklmnop
!!    > 3 ==> qrstuvwxyz
!!    > 4 ==> 1:|:2
!!    > 5 ==> 333|333
!!    > 6 ==> a
!!    > 7 ==> B
!!    > 8 ==> cc
!!    >  SIZE:           8
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters (colon and vertical line):
!!    > 1 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    > 2 ==> 2     333
!!    > 3 ==> 333 a B cc
!!    >  SIZE:           3
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters, reverse array order and return null fields:
!!    > 1 ==> 333 a B cc
!!    > 2 ==> 2     333
!!    > 3 ==>
!!    > 4 ==>
!!    > 5 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    >  SIZE:           5
!!    > --------------------------------------------------------------------------
!!    >  INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    >  default delimiters and reverse array order and count null fields:
!!    > 1 ==>
!!    > 2 ==>
!!    > 3 ==>
!!    > 4 ==> cc
!!    > 5 ==> B
!!    > 6 ==> a
!!    > 7 ==> 333|333
!!    > 8 ==>
!!    > 9 ==>
!!    > 10 ==>
!!    > 11 ==>
!!    > 12 ==> 1:|:2
!!    > 13 ==>
!!    > 14 ==> qrstuvwxyz
!!    > 15 ==> ghijklmnop
!!    > 16 ==>
!!    > 17 ==>
!!    > 18 ==> aBcdef
!!    > 19 ==>
!!    > 20 ==>
!!    >  SIZE:          20
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_16="@(#) M_CLI2 split(3f) parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: iilen                  ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters /= '')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)//',:' ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)//',:'    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)    !x! intel compiler says allocated already ???
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   if(allocated(iterm))deallocate(iterm)      !x! intel compiler says allocated already ???
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   iilen=len(input_line)                                          ! IILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
   if(iilen > 0)then                                              ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,iilen,1                                  ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)) == 0)then  ! if current character is not a delimiter
            iterm(i30)=iilen                                      ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):iilen),dlim(i10:i10))
               IF(ifound > 0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol > iilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   if(allocated(array))deallocate(array)
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to turn
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20) < ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    replace_str(3f) - [M_CLI2:EDITING] function globally replaces one
!!    substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function replace_str(targetline,old,new,range,ierr) result (newline)
!!
!!     character(len=*)               :: targetline
!!     character(len=*),intent(in)    :: old
!!     character(len=*),intent(in)    :: new
!!     integer,intent(in),optional    :: range(2)
!!     integer,intent(out),optional   :: ierr
!!     logical,intent(in),optional    :: clip
!!     character(len=:),allocatable   :: newline
!!##DESCRIPTION
!!    Globally replace one substring for another in string.
!!    Either CMD or OLD and NEW must be specified.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     range       if present, only change range(1) to range(2) of
!!                 occurrences of old string
!!     ierr        error code. If ier = -1 bad directive, >= 0 then
!!                 count of changes made
!!     clip        whether to return trailing spaces or not. Defaults to .false.
!!##RETURNS
!!     newline     allocatable string returned
!!
!!##EXAMPLES
!!
!! Sample Program:
!!
!!       program demo_replace_str
!!       use M_CLI2, only : replace_str
!!       implicit none
!!       character(len=:),allocatable :: targetline
!!
!!       targetline='this is the input string'
!!
!!       call testit('th','TH','THis is THe input string')
!!
!!       ! a null old substring means "at beginning of line"
!!       call testit('','BEFORE:', 'BEFORE:THis is THe input string')
!!
!!       ! a null new string deletes occurrences of the old substring
!!       call testit('i','', 'BEFORE:THs s THe nput strng')
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','A')
!!       write(*,*)'replace a with A ['//targetline//']'
!!
!!       write(*,*)'Examples of the use of RANGE='
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','A',range=[3,5])
!!       write(*,*)'replace a with A instances 3 to 5 ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','',range=[3,5])
!!       write(*,*)'replace a with null instances 3 to 5 ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa aa aa a a a aa aaaaaa',&
!!        & 'aa','CCCC',range=[3,5])
!!       write(*,*)'replace aa with CCCC instances 3 to 5 ['//targetline//']'
!!
!!       contains
!!       subroutine testit(old,new,expected)
!!       character(len=*),intent(in) :: old,new,expected
!!       write(*,*)repeat('=',79)
!!       write(*,*)':STARTED ['//targetline//']'
!!       write(*,*)':OLD['//old//']', ' NEW['//new//']'
!!       targetline=replace_str(targetline,old,new)
!!       write(*,*)':GOT     ['//targetline//']'
!!       write(*,*)':EXPECTED['//expected//']'
!!       write(*,*)':TEST    [',targetline == expected,']'
!!       end subroutine testit
!!
!!       end program demo_replace_str
!!
!!   Expected output
!!
!!     ===============================================================================
!!     STARTED [this is the input string]
!!     OLD[th] NEW[TH]
!!     GOT     [THis is THe input string]
!!     EXPECTED[THis is THe input string]
!!     TEST    [ T ]
!!     ===============================================================================
!!     STARTED [THis is THe input string]
!!     OLD[] NEW[BEFORE:]
!!     GOT     [BEFORE:THis is THe input string]
!!     EXPECTED[BEFORE:THis is THe input string]
!!     TEST    [ T ]
!!     ===============================================================================
!!     STARTED [BEFORE:THis is THe input string]
!!     OLD[i] NEW[]
!!     GOT     [BEFORE:THs s THe nput strng]
!!     EXPECTED[BEFORE:THs s THe nput strng]
!!     TEST    [ T ]
!!     replace a with A [A b Ab bAAA AAAA]
!!     Examples of the use of RANGE=
!!     replace a with A instances 3 to 5 [a b ab bAAA aaaa]
!!     replace a with null instances 3 to 5 [a b ab b aaaa]
!!     replace aa with CCCC instances 3 to 5 [a b ab baaa aaCCCC CCCC CCCC
!!     a a a aa aaaaaa]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function replace_str(targetline,old,new,ierr,range) result (newline)

! ident_17="@(#) M_CLI2 replace_str(3f) Globally replace one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
! parameters
character(len=*),intent(in)            :: targetline   ! input line to be changed
character(len=*),intent(in)            :: old          ! old substring to replace
character(len=*),intent(in)            :: new          ! new substring
integer,intent(out),optional           :: ierr         ! error code. If ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional            :: range(2)     ! start and end of which changes to make
!-----------------------------------------------------------------------------------------------------------------------------------
! returns
character(len=:),allocatable  :: newline               ! output string buffer
!-----------------------------------------------------------------------------------------------------------------------------------
! local
integer  :: icount,ichange
integer  :: original_input_length
integer  :: len_old, len_new
integer  :: ladd
integer  :: left_margin, right_margin
integer  :: ind
integer  :: ic
integer  :: iichar
integer  :: range_local(2)
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   left_margin=1                                       ! left_margin is left margin of window to change
   right_margin=len(targetline)                        ! right_margin is right margin of window to change
   newline=''                                          ! begin with a blank line as output string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(range))then
      range_local=range
   else
      range_local=[1,original_input_length]
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old == 0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      iichar=len_new + original_input_length
      if(len_new > 0)then
         newline=new(:len_new)//targetline(left_margin:original_input_length)
      else
         newline=targetline(left_margin:original_input_length)
      endif
      ichange=1                                        ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ichange
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   iichar=left_margin                                  ! place to put characters into output string
   ic=left_margin                                      ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1 ! try finding start of OLD in remaining part of input in change window
      if(ind == ic-1.or.ind > right_margin)then           ! did not find old string or found old string past edit window
         exit loop                                        ! no more changes left to make
      endif
      icount=icount+1                                  ! found an old string to change, so increment count of change candidates
      if(ind > ic)then                                 ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         newline=newline(:iichar-1)//targetline(ic:ind-1)
         iichar=iichar+ladd
      endif
      if(icount >= range_local(1).and.icount <= range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new /= 0)then                                          ! put in new string
            newline=newline(:iichar-1)//new(:len_new)
            iichar=iichar+len_new
         endif
      else
         if(len_old /= 0)then                                          ! put in copy of old string
            newline=newline(:iichar-1)//old(:len_old)
            iichar=iichar+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ichange)
   case (0)                                            ! there were no changes made to the window
      newline=targetline                               ! if no changes made output should be input
   case default
      if(ic <= len(targetline))then                    ! if there is more after last change on original line add it
         newline=newline(:iichar-1)//targetline(ic:max(ic,original_input_length))
      endif
   end select
   if(present(ierr))ierr=ichange
!-----------------------------------------------------------------------------------------------------------------------------------
end function replace_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    quote(3f) - [M_CLI2:QUOTES] add quotes to string as if written with
!!    list-directed input
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function quote(str,mode,clip) result (quoted_str)
!!
!!    character(len=*),intent(in)          :: str
!!    character(len=*),optional,intent(in) :: mode
!!    logical,optional,intent(in)          :: clip
!!    character(len=:),allocatable         :: quoted_str
!!##DESCRIPTION
!!    Add quotes to a CHARACTER variable as if it was written using
!!    list-directed input. This is particularly useful for processing
!!    strings to add to CSV files.
!!
!!##OPTIONS
!!    str         input string to add quotes to, using the rules of
!!                list-directed input (single quotes are replaced by two
!!                adjacent quotes)
!!    mode        alternate quoting methods are supported:
!!
!!                   DOUBLE   default. replace quote with double quotes
!!                   ESCAPE   replace quotes with backslash-quote instead
!!                            of double quotes
!!
!!    clip        default is to trim leading and trailing spaces from the
!!                string. If CLIP
!!                is .FALSE. spaces are not trimmed
!!
!!##RESULT
!!    quoted_str  The output string, which is based on adding quotes to STR.
!!##EXAMPLE
!!
!! Sample program:
!!
!!     program demo_quote
!!     use M_CLI2, only : quote
!!     implicit none
!!     character(len=:),allocatable :: str
!!     character(len=1024)          :: msg
!!     integer                      :: ios
!!     character(len=80)            :: inline
!!        do
!!           write(*,'(a)',advance='no')'Enter test string:'
!!           read(*,'(a)',iostat=ios,iomsg=msg)inline
!!           if(ios /= 0)then
!!              write(*,*)trim(inline)
!!              exit
!!           endif
!!
!!           ! the original string
!!           write(*,'(a)')'ORIGINAL     ['//trim(inline)//']'
!!
!!           ! the string processed by quote(3f)
!!           str=quote(inline)
!!           write(*,'(a)')'QUOTED     ['//str//']'
!!
!!           ! write the string list-directed to compare the results
!!           write(*,'(a)',iostat=ios,iomsg=msg) 'LIST DIRECTED:'
!!           write(*,*,iostat=ios,iomsg=msg,delim='none') inline
!!           write(*,*,iostat=ios,iomsg=msg,delim='quote') inline
!!           write(*,*,iostat=ios,iomsg=msg,delim='apostrophe') inline
!!        enddo
!!     end program demo_quote
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function quote(str,mode,clip) result (quoted_str)
character(len=*),intent(in)          :: str                ! the string to be quoted
character(len=*),optional,intent(in) :: mode
logical,optional,intent(in)          :: clip
logical                              :: clip_local
character(len=:),allocatable         :: quoted_str

character(len=1),parameter           :: double_quote = '"'
character(len=20)                    :: local_mode

   if(present(mode))then
      local_mode=mode
   else
      local_mode='DOUBLE'
   endif

   if(present(clip))then
      clip_local=clip
   else
      clip_local=.false.
   endif

   if(clip_local)then
      quoted_str=adjustl(str)
   else
      quoted_str=str
   endif

   select case(lower(local_mode))
   case('double')
      quoted_str=double_quote//trim(replace_str(quoted_str,'"','""'))//double_quote
   case('escape')
      quoted_str=double_quote//trim(replace_str(quoted_str,'"','\"'))//double_quote
   case default
      call journal('*quote* ERROR: unknown quote mode ',local_mode)
      quoted_str=str
   end select

end function quote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    unquote(3f) - [M_CLI2:QUOTES] remove quotes from string as if read
!!    with list-directed input
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   pure function unquote(quoted_str,esc) result (unquoted_str)
!!
!!    character(len=*),intent(in)          :: quoted_str
!!    character(len=1),optional,intent(in) :: esc
!!    character(len=:),allocatable         :: unquoted_str
!!##DESCRIPTION
!!    Remove quotes from a CHARACTER variable as if it was read using
!!    list-directed input. This is particularly useful for processing
!!    tokens read from input such as CSV files.
!!
!!    Fortran can now read using list-directed input from an internal file,
!!    which should handle quoted strings, but list-directed input does not
!!    support escape characters, which UNQUOTE(3f) does.
!!##OPTIONS
!!    quoted_str  input string to remove quotes from, using the rules of
!!                list-directed input (two adjacent quotes inside a quoted
!!                region are replaced by a single quote, a single quote or
!!                double quote is selected as the delimiter based on which
!!                is encountered first going from left to right, ...)
!!    esc         optional character used to protect the next quote
!!                character from being processed as a quote, but simply as
!!                a plain character.
!!##RESULT
!!    unquoted_str  The output string, which is based on removing quotes
!!                  from quoted_str.
!!##EXAMPLE
!!
!! Sample program:
!!
!!       program demo_unquote
!!       use M_CLI2, only : unquote
!!       implicit none
!!       character(len=128)           :: quoted_str
!!       character(len=:),allocatable :: unquoted_str
!!       character(len=1),parameter   :: esc='\'
!!       character(len=1024)          :: msg
!!       integer                      :: ios
!!       character(len=1024)          :: dummy
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)quoted_str
!!          if(ios /= 0)then
!!             write(*,*)trim(msg)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'QUOTED       ['//trim(quoted_str)//']'
!!
!!          ! the string processed by unquote(3f)
!!          unquoted_str=unquote(trim(quoted_str),esc)
!!          write(*,'(a)')'UNQUOTED     ['//unquoted_str//']'
!!
!!          ! read the string list-directed to compare the results
!!          read(quoted_str,*,iostat=ios,iomsg=msg)dummy
!!          if(ios /= 0)then
!!             write(*,*)trim(msg)
!!          else
!!             write(*,'(a)')'LIST DIRECTED['//trim(dummy)//']'
!!          endif
!!       enddo
!!       end program demo_unquote
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure function unquote(quoted_str,esc) result (unquoted_str)
character(len=*),intent(in)          :: quoted_str              ! the string to be unquoted
character(len=1),optional,intent(in) :: esc                     ! escape character
character(len=:),allocatable         :: unquoted_str
integer                              :: inlen
character(len=1),parameter           :: single_quote = "'"
character(len=1),parameter           :: double_quote = '"'
integer                              :: quote                   ! whichever quote is to be used
integer                              :: before
integer                              :: current
integer                              :: iesc
integer                              :: iput
integer                              :: i
logical                              :: inside
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(esc))then                           ! select escape character as specified character or special value meaning not set
      iesc=ichar(esc)                             ! allow for an escape character
   else
      iesc=-1                                     ! set to value that matches no character
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   inlen=len(quoted_str)                          ! find length of input string
   if(allocated(unquoted_str))deallocate(unquoted_str)
   allocate(character(len=inlen) :: unquoted_str) ! initially make output string length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(inlen >= 1)then                             ! double_quote is the default quote unless the first character is single_quote
      if(quoted_str(1:1) == single_quote)then
         quote=ichar(single_quote)
      else
         quote=ichar(double_quote)
      endif
   else
      quote=ichar(double_quote)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   before=-2                                      ! initially set previous character to impossible value
   unquoted_str(:)=''                             ! initialize output string to null string
   iput=1
   inside=.false.
   STEPTHROUGH: do i=1,inlen
      current=ichar(quoted_str(i:i))
      if(before == iesc)then                      ! if previous character was escape use current character unconditionally
           iput=iput-1                            ! backup
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2                              ! this could be second esc or quote
      elseif(current == quote)then                ! if current is a quote it depends on whether previous character was a quote
         if(before == quote)then
           unquoted_str(iput:iput)=char(quote)    ! this is second quote so retain it
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before /= iesc)then
            inside=.true.
         else                                     ! this is first quote so ignore it except remember it in case next is a quote
            before=current
         endif
      else
         unquoted_str(iput:iput)=char(current)
         iput=iput+1
         before=current
      endif
   enddo STEPTHROUGH
!-----------------------------------------------------------------------------------------------------------------------------------
   unquoted_str=unquoted_str(:iput-1)
!-----------------------------------------------------------------------------------------------------------------------------------
end function unquote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    decodebase(3f) - [M_CLI2:BASE] convert whole number string in base
!!                     [2-36] to base 10 number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function decodebase(string,basein,out10)
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in)           :: basein
!!    integer,intent(out)          :: out10
!!##DESCRIPTION
!!
!!    Convert a numeric string representing a whole number in base BASEIN
!!    to base 10. The function returns FALSE if BASEIN is not in the range
!!    [2..36] or if string STRING contains invalid characters in base BASEIN
!!    or if result OUT10 is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    string   input string. It represents a whole number in
!!             the base specified by BASEIN unless BASEIN is set
!!             to zero. When BASEIN is zero STRING is assumed to
!!             be of the form BASE#VALUE where BASE represents
!!             the function normally provided by BASEIN.
!!    basein   base of input string; either 0 or from 2 to 36.
!!    out10    output value in base 10
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!      program demo_decodebase
!!      use M_CLI2, only : codebase, decodebase
!!      implicit none
!!      integer           :: ba,bd
!!      character(len=40) :: x,y
!!      integer           :: r
!!
!!      print *,' BASE CONVERSION'
!!      write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!      write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!      INFINITE: do
!!         print *,''
!!         write(*,'("Enter number in start base: ")',advance='no'); read *, x
!!         if(x == '0') exit INFINITE
!!         if(decodebase(x,bd,r)) then
!!            if(codebase(r,ba,y)) then
!!              write(*,'("In base ",I2,": ",A20)')  ba, y
!!            else
!!              print *,'Error in coding number.'
!!            endif
!!         else
!!            print *,'Error in decoding number.'
!!         endif
!!      enddo INFINITE
!!
!!      end program demo_decodebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!       Ref.: "Math matiques en Turbo-Pascal by
!!              M. Ducamp and A. Reverchon (2),
!!              Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function decodebase(string,basein,out_baseten)

! ident_18="@(#) M_CLI2 decodebase(3f) convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein == 0.and.ipound > 1)then                                  ! split string into two values
     call a2i(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local >= 0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        IF(CH == '-'.AND.K == 1)THEN
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    locate_(3f) - [M_CLI2] finds the index where a string is found or
!!                  should be in a sorted array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine locate_(list,value,place,ier,errmsg)
!!
!!    character(len=:)|doubleprecision|real|integer,allocatable :: list(:)
!!    character(len=*)|doubleprecision|real|integer,intent(in)  :: value
!!    integer, intent(out)                  :: PLACE
!!
!!    integer, intent(out),optional         :: IER
!!    character(len=*),intent(out),optional :: ERRMSG
!!
!!##DESCRIPTION
!!
!!    LOCATE_(3f) finds the index where the VALUE is found or should
!!    be found in an array. The array must be sorted in descending
!!    order (highest at top). If VALUE is not found it returns the index
!!    where the name should be placed at with a negative sign.
!!
!!    The array and list must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL,INTEGER)
!!
!!##OPTIONS
!!
!!    VALUE         the value to locate in the list.
!!    LIST          is the list array.
!!
!!##RETURNS
!!    PLACE         is the subscript that the entry was found at if it is
!!                  greater than zero(0).
!!
!!                  If PLACE is negative, the absolute value of
!!                  PLACE indicates the subscript value where the
!!                  new entry should be placed in order to keep the
!!                  list alphabetized.
!!
!!    IER           is zero(0) if no error occurs.
!!                  If an error occurs and IER is not
!!                  present, the program is stopped.
!!
!!    ERRMSG        description of any error
!!
!!##EXAMPLES
!!
!!
!! Find if a string is in a sorted array, and insert the string into
!! the list if it is not present ...
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate_
!!     implicit none
!!     character(len=:),allocatable  :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     call update_dic(arr,'b')
!!     call update_dic(arr,'[')
!!     call update_dic(arr,'c')
!!     call update_dic(arr,'ZZ')
!!     call update_dic(arr,'ZZZZ')
!!     call update_dic(arr,'z')
!!
!!     contains
!!     subroutine update_dic(arr,string)
!!     character(len=:),intent(in),allocatable :: arr(:)
!!     character(len=*),intent(in)  :: string
!!     integer                      :: place, plus, ii, end
!!     ! find where string is or should be
!!     call locate_(arr,string,place)
!!     write(*,*)'for "'//string//'" index is ',place, size(arr)
!!     ! if string was not found insert it
!!     if(place < 1)then
!!        plus=abs(place)
!!        ii=len(arr)
!!        end=size(arr)
!!        ! empty array
!!        if(end == 0)then
!!           arr=[character(len=ii) :: string ]
!!        ! put in front of array
!!        elseif(plus == 1)then
!!           arr=[character(len=ii) :: string, arr]
!!        ! put at end of array
!!        elseif(plus == end)then
!!           arr=[character(len=ii) :: arr, string ]
!!        ! put in middle of array
!!        else
!!           arr=[character(len=ii) :: arr(:plus-1), string,arr(plus:) ]
!!        endif
!!        ! show array
!!        write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     endif
!!     end subroutine update_dic
!!     end program demo_locate
!!
!!   Results:
!!
!!     for "b" index is            2           5
!!     for "[" index is           -4           5
!!    SIZE=5 xxx,b,aaa,[,ZZZ,
!!     for "c" index is           -2           6
!!    SIZE=6 xxx,c,b,aaa,[,ZZZ,
!!     for "ZZ" index is           -7           7
!!    SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     for "ZZZZ" index is           -6           8
!!    SIZE=8 xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!     for "z" index is           -1           9
!!    SIZE=9 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine locate_c(list,value,place,ier,errmsg)

! ident_19="@(#) M_CLI2 locate_c(3f) find PLACE in sorted character array LIST where VALUE can be found or should be placed"

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
   if(arraysize == 0)then
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

      if(value == list(PLACE))then
         exit LOOP
      elseif(value > list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin > imax)then
         place=-imin
         if(iabs(place) > arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place > arraysize.or.place <= 0)then
         message='*locate_* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate_* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   elseif(error /= 0)then
      write(warn,*)message//' VALUE=',trim(value)//' PLACE=',place
      call mystop(-24,'(*locate_c* '//message)
   endif
   if(present(errmsg))then
      errmsg=message
   endif
end subroutine locate_c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    remove_(3f) - [M_CLI2] remove entry from an allocatable array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine remove_(list,place)
!!
!!    character(len=:)|doubleprecision|real|integer,intent(inout) :: list(:)
!!    integer, intent(out) :: PLACE
!!
!!##DESCRIPTION
!!
!!    Remove a value from an allocatable array at the specified index.
!!    The array is assumed to be sorted in descending order. It may be of
!!    type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER.
!!
!!##OPTIONS
!!
!!    list    is the list array.
!!    PLACE   is the subscript for the entry that should be removed
!!
!!##EXAMPLES
!!
!!
!! Sample program
!!
!!     program demo_remove
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate_, remove_
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!     integer                       :: end
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'b', 'ab', 'bb', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove_(arr,1)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove_(arr,4)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end program demo_remove
!!
!!   Results:
!!
!!    Expected output
!!
!!     SIZE=9 xxx,bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=8 bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=7 bb,b,b,aaa,ZZZ,Z,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine remove_c(list,place)

! ident_20="@(#) M_CLI2 remove_c(3fp) remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place <= 0.or.place > end)then                       ! index out of bounds of array
   elseif(place == end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
end subroutine remove_c
subroutine remove_l(list,place)

! ident_21="@(#) M_CLI2 remove_l(3fp) remove value from allocatable array at specified position"

logical,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(place <= 0.or.place > end)then                       ! index out of bounds of array
   elseif(place == end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_l
subroutine remove_i(list,place)

! ident_22="@(#) M_CLI2 remove_i(3fp) remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(place <= 0.or.place > end)then                       ! index out of bounds of array
   elseif(place == end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    replace_(3f) - [M_CLI2] replace entry in a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine replace_(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer, intent(out) :: place
!!
!!##DESCRIPTION
!!
!!    replace a value in an allocatable array at the specified index. Unless the
!!    array needs the string length to increase this is merely an assign of a value
!!    to an array element.
!!
!!    The array may be of type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER>
!!    It is assumed to be sorted in descending order without duplicate values.
!!
!!    The value and list must be of the same type.
!!
!!##OPTIONS
!!
!!    VALUE         the value to place in the array
!!    LIST          is the array.
!!    PLACE         is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!! Replace key-value pairs in a dictionary
!!
!!     program demo_replace
!!     use M_CLI2, only  : insert_, locate_, replace_
!!     ! Find if a key is in a list and insert it
!!     ! into the key list and value list if it is not present
!!     ! or replace the associated value if the key existed
!!     implicit none
!!     character(len=20)            :: key
!!     character(len=100)           :: val
!!     character(len=:),allocatable :: keywords(:)
!!     character(len=:),allocatable :: values(:)
!!     integer                      :: i
!!     integer                      :: place
!!     call update_dic('b','value of b')
!!     call update_dic('a','value of a')
!!     call update_dic('c','value of c')
!!     call update_dic('c','value of c again')
!!     call update_dic('d','value of d')
!!     call update_dic('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>",a,/))')(trim(keywords(i)),trim(values(i)),i=1,size(keywords))
!!
!!     call locate_key('a',place)
!!     if(place > 0)then
!!        write(*,*)'The value of "a" is',trim(values(place))
!!     else
!!        write(*,*)'"a" not found'
!!     endif
!!
!!     contains
!!     subroutine update_dic(key,val)
!!     character(len=*),intent(in)  :: key
!!     character(len=*),intent(in)  :: val
!!     integer                      :: place
!!
!!     ! find where string is or should be
!!     call locate_key(key,place)
!!     ! if string was not found insert it
!!     if(place < 1)then
!!        call insert_(keywords,key,abs(place))
!!        call insert_(values,val,abs(place))
!!     else ! replace
!!        call replace_(values,val,place)
!!     endif
!!
!!     end subroutine update_dic
!!    end program demo_replace
!!
!!   Expected output
!!
!!    d==>value of d
!!    c==>value of c again
!!    b==>value of b
!!    a==>value of a again
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine replace_c(list,value,place)

! ident_23="@(#) M_CLI2 replace_c(3fp) replace string in allocatable string array at specified position"

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
   if(place < 0.or.place > end)then
           write(warn,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value) <= len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
end subroutine replace_c
subroutine replace_l(list,value,place)

! ident_24="@(#) M_CLI2 replace_l(3fp) place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place > 0.and.place <= end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(warn,*)'*replace_l* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_l
subroutine replace_i(list,value,place)

! ident_25="@(#) M_CLI2 replace_i(3fp) place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place > 0.and.place <= end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(warn,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    insert_(3f) - [M_CLI2] insert entry into a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine insert_(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer,intent(in)    :: place
!!
!!##DESCRIPTION
!!
!!    Insert a value into an allocatable array at the specified index.
!!    The list and value must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL, or INTEGER)
!!
!!##OPTIONS
!!
!!    list    is the list array. Must be sorted in descending order.
!!    value   the value to place in the array
!!    PLACE   is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!! Find if a string is in a sorted array, and insert the string into
!! the list if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate_, insert_
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!     ! add or replace values
!!     call update_dic(arr,'b')
!!     call update_dic(arr,'[')
!!     call update_dic(arr,'c')
!!     call update_dic(arr,'ZZ')
!!     call update_dic(arr,'ZZZ')
!!     call update_dic(arr,'ZZZZ')
!!     call update_dic(arr,'')
!!     call update_dic(arr,'z')
!!
!!     contains
!!     subroutine update_dic(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, end
!!
!!     end=size(arr)
!!     ! find where string is or should be
!!     call locate_(arr,string,place)
!!     ! if string was not found insert it
!!     if(place < 1)then
!!        call insert_(arr,string,abs(place))
!!     endif
!!     ! show array
!!     end=size(arr)
!!     write(*,'("array is now SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end subroutine update_dic
!!     end program demo_insert
!!
!!   Results:
!!
!!     array is now SIZE=5 xxx,b,aaa,ZZZ,,
!!     array is now SIZE=6 xxx,b,aaa,[,ZZZ,,
!!     array is now SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     array is now SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!     array is now SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!     array is now SIZE=10 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine insert_c(list,value,place)

! ident_26="@(#) M_CLI2 insert_c(3fp) place string into allocatable string array at specified position"

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

   if(end == 0)then                                        ! empty array
      list=[character(len=ii) :: value ]
   elseif(place == 1)then                                  ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place > end)then                                 ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place >= 2.and.place <= end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(warn,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_c
subroutine insert_l(list,value,place)

! ident_27="@(#) M_CLI2 insert_l(3fp) place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place == 1)then                                    ! put in front of array
      list=[value, list]
   elseif(place > end)then                                   ! put at end of array
      list=[list, value ]
   elseif(place >= 2.and.place <= end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(warn,*)'*insert_l* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_l
subroutine insert_i(list,value,place)

! ident_28="@(#) M_CLI2 insert_i(3fp) place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place == 1)then                                    ! put in front of array
      list=[value, list]
   elseif(place > end)then                                   ! put at end of array
      list=[list, value ]
   elseif(place >= 2.and.place <= end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(warn,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine many_args(n0,g0, n1,g1, n2,g2, n3,g3, n4,g4, n5,g5, n6,g6, n7,g7, n8,g8, n9,g9, &
                   & na,ga, nb,gb, nc,gc, nd,gd, ne,ge, nf,gf, ng,gg, nh,gh, ni,gi, nj,gj )

! ident_29="@(#) M_CLI2 many_args(3fp) allow for multiple calls to get_args(3f)"

character(len=*),intent(in)          :: n0, n1
character(len=*),intent(in),optional :: n2, n3, n4, n5, n6, n7, n8, n9, na, nb, nc, nd, ne, nf, ng, nh, ni, nj
class(*),intent(out)           :: g0, g1
class(*),intent(out),optional  :: g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
   call get_generic(n0,g0)
   call get_generic(n1,g1)
   if( present(n2) .and. present(g2) )call get_generic(n2,g2)
   if( present(n3) .and. present(g3) )call get_generic(n3,g3)
   if( present(n4) .and. present(g4) )call get_generic(n4,g4)
   if( present(n5) .and. present(g5) )call get_generic(n5,g5)
   if( present(n6) .and. present(g6) )call get_generic(n6,g6)
   if( present(n7) .and. present(g7) )call get_generic(n7,g7)
   if( present(n8) .and. present(g8) )call get_generic(n8,g8)
   if( present(n9) .and. present(g9) )call get_generic(n9,g9)
   if( present(na) .and. present(ga) )call get_generic(na,ga)
   if( present(nb) .and. present(gb) )call get_generic(nb,gb)
   if( present(nc) .and. present(gc) )call get_generic(nc,gc)
   if( present(nd) .and. present(gd) )call get_generic(nd,gd)
   if( present(ne) .and. present(ge) )call get_generic(ne,ge)
   if( present(nf) .and. present(gf) )call get_generic(nf,gf)
   if( present(ng) .and. present(gg) )call get_generic(ng,gg)
   if( present(nh) .and. present(gh) )call get_generic(nh,gh)
   if( present(ni) .and. present(gi) )call get_generic(ni,gi)
   if( present(nj) .and. present(gj) )call get_generic(nj,gj)
contains
!===================================================================================================================================
function c(generic)
class(*),intent(in) :: generic
character(len=:),allocatable :: c
   select type(generic)
      type is (character(len=*)); c=trim(generic)
      class default
         c='unknown'
         stop 'get_many:: parameter name is not character'
   end select
end function c
!===================================================================================================================================
subroutine get_generic(name,generic)
use,intrinsic :: iso_fortran_env, only : real64
character(len=*),intent(in)  :: name
class(*),intent(out)         :: generic
   select type(generic)
      type is (integer);                        call get_args(name,generic)
      type is (real);                           call get_args(name,generic)
      type is (real(kind=real64));              call get_args(name,generic)
      type is (logical);                        call get_args(name,generic)
      !x!type is (character(len=:),allocatable ::);   call get_args(name,generic)
      type is (character(len=*));
      call get_args_fixed_length(name,generic)
      type is (complex);                        call get_args(name,generic)
      class default
         stop 'unknown type in *get_generic*'
   end select
end subroutine get_generic
!===================================================================================================================================
end subroutine many_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function iget(n); integer                      :: iget; character(len=*),intent(in) :: n; call get_args(n,iget); end function iget
function rget(n); real                         :: rget; character(len=*),intent(in) :: n; call get_args(n,rget); end function rget
function dget(n); real(kind=dp)                :: dget; character(len=*),intent(in) :: n; call get_args(n,dget); end function dget
function sget(n); character(len=:),allocatable :: sget; character(len=*),intent(in) :: n; call get_args(n,sget); end function sget
function cget(n); complex                      :: cget; character(len=*),intent(in) :: n; call get_args(n,cget); end function cget
function lget(n); logical                      :: lget; character(len=*),intent(in) :: n; call get_args(n,lget); end function lget

function igs(n); integer,allocatable          :: igs(:); character(len=*),intent(in) :: n; call get_args(n,igs); end function igs
function rgs(n); real,allocatable             :: rgs(:); character(len=*),intent(in) :: n; call get_args(n,rgs); end function rgs
function dgs(n); real(kind=dp),allocatable    :: dgs(:); character(len=*),intent(in) :: n; call get_args(n,dgs); end function dgs
function sgs(n,delims)
character(len=:),allocatable         :: sgs(:)
character(len=*),optional,intent(in) :: delims
character(len=*),intent(in)          :: n
   call get_args(n,sgs,delims)
end function sgs
function cgs(n); complex,allocatable          :: cgs(:); character(len=*),intent(in) :: n; call get_args(n,cgs); end function cgs
function lgs(n); logical,allocatable          :: lgs(:); character(len=*),intent(in) :: n; call get_args(n,lgs); end function lgs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function ig()
integer,allocatable :: ig(:)
integer             :: i, ierr
   if(allocated(ig))deallocate(ig)
   allocate(ig(size(unnamed)))
   do i=1,size(ig)
      call a2i(unnamed(i),ig(i),ierr)
   enddo
end function ig
!===================================================================================================================================
function rg()
real,allocatable :: rg(:)
   rg=real(dg())
end function rg
!===================================================================================================================================
function dg()
real(kind=dp),allocatable :: dg(:)
integer                   :: i
integer                   :: ierr
   if(allocated(dg))deallocate(dg)
   allocate(dg(size(unnamed)))
   do i=1,size(dg)
      call a2d(unnamed(i),dg(i),ierr)
   enddo
end function dg
!===================================================================================================================================
function lg()
logical,allocatable   :: lg(:)
integer               :: i
integer               :: iichar
character,allocatable :: hold
   if(allocated(lg))deallocate(lg)
   allocate(lg(size(unnamed)))
   do i=1,size(lg)
      hold=trim(upper(adjustl(unnamed(i))))
      if(hold(1:1) == '.')then                 ! looking for fortran logical syntax .STRING.
         iichar=2
      else
         iichar=1
      endif
      select case(hold(iichar:iichar))         ! check word to see if true or false
      case('T','Y',' '); lg(i)=.true.          ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
      case('F','N');     lg(i)=.false.         ! assume this is false or no
      case default
         call journal("*lg* bad logical expression for element",i,'=',hold)
      end select
   enddo
end function lg
!===================================================================================================================================
function cg()
complex,allocatable :: cg(:)
integer             :: i, ierr
real(kind=dp)       :: rc, ic
   if(allocated(cg))deallocate(cg)
   allocate(cg(size(unnamed)))
   do i=1,size(cg),2
      call a2d(unnamed(i),rc,ierr)
      call a2d(unnamed(i+1),ic,ierr)
      cg(i)=cmplx(rc,ic,kind=sp)
   enddo
end function cg
!===================================================================================================================================
! Does not work with gcc 5.3
!function sg()
!character(len=:),allocatable :: sg(:)
!   sg=unnamed
!end function sg

!===================================================================================================================================
function sg()
character(len=:),allocatable :: sg(:)
   if(allocated(sg))deallocate(sg)
   allocate(sg,source=unnamed)
end function sg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine mystop(sig,msg)
! negative signal means always stop program
! else do not stop and set G_STOP_MESSAGE if G_QUIET is true
! or
! print message and stop if G_QUIET is false
! the MSG is NOT for displaying except for internal errors when the program will be stopped.
! It is for returning a value when the stop is being ignored
!
integer,intent(in) :: sig
character(len=*),intent(in),optional :: msg
   if(sig < 0)then
      if(present(msg))call journal(msg)
      stop 1
   elseif(.not.G_QUIET)then
      stop
   else
      if(present(msg)) then
         G_STOP_MESSAGE=msg
      else
         G_STOP_MESSAGE=''
      endif
      G_STOP=sig
   endif
end subroutine mystop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function atleast(line,length,pattern) result(strout)

! ident_30="@(#) M_strings atleast(3f) return string padded to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=*),intent(in),optional       :: pattern
character(len=max(length,len(trim(line)))) :: strout
if(present(pattern))then
   strout=line//repeat(pattern,len(strout)/len(pattern)+1)
else
   strout=line
endif
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine locate_key(value,place)

! ident_31="@(#) M_CLI2 locate_key(3f) find PLACE in sorted character array where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
integer                                 :: ii
character(len=:),allocatable            :: value_local

   if(G_UNDERDASH)then
      value_local=trim(replace_str(value,'-','_'))
   else
      value_local=trim(value)
   endif
   if(G_NOSEPARATOR)then
      value_local=replace_str(value_local,'-','')
      value_local=replace_str(value_local,'_','')
   endif

   if(G_IGNORECASE.and.len_trim(value_local) > 1)value_local=lower(value_local)

   if(len(value_local) == 1)then
      !x!ii=findloc(shorts,value_local,dim=1)
      ii=maxloc([0,merge(1, 0, shorts == value_local)],dim=1)
      if(ii > 1)then
         place=ii-1
      else
         call locate_(keywords,value_local,place)
      endif
   else
      call locate_(keywords,value_local,place)
   endif
end subroutine locate_key
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_mode(3f) - [ARGUMENTS:M_CLI2] turn on optional modes
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine set_mode(key,mode)
!!
!!     character(len=*),intent(in) :: key
!!     logical,intent(in),optional :: mode
!!
!!##DESCRIPTION
!!     Allow optional behaviors.
!!
!!##OPTIONS
!!    KEY    name of option
!!
!!    The following values are allowed:
!!
!!    o  response_file - enable use of response file
!!
!!    o  ignorecase - ignore case in long key names. So the user
!!       does not have to remember if the option is --IgnoreCase
!!       or --ignorecase or --ignoreCase
!!
!!    o  underdash  - treat dash in keyword as an underscore.
!!       So the user should not have to remember if the option is
!!       --ignore_case or --ignore-case.
!!
!!    o  strict - allow Boolean keys to be bundled, but requires
!!       a single dash prefix be used for short key names and
!!       long names must be prefixed with two dashes.
!!
!!    o  lastonly  - when multiple keywords occur keep the rightmost
!!       value specified instead of appending the values together.
!!
!!    MODE   set to .true. to activate the optional mode.
!!           Set to .false. to deactivate the mode.
!!           It is .true. by default.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!    program demo_set_mode
!!    use M_CLI2,  only : set_args, lget, set_mode
!!    implicit none
!!    character(len=*),parameter :: all='(*(g0))'
!!       !
!!       ! enable use of response files
!!       call set_mode('response_file')
!!       !
!!       ! Any dash in a keyword is treated as an underscore
!!       call set_mode('underdash')
!!       !
!!       ! The case of long keywords are ignored.
!!       ! Values and short names remain case-sensitive
!!       call set_mode('ignorecase')
!!       !
!!       ! short single-character boolean keys may be bundled
!!       ! but it is required that a single dash is used for
!!       ! short keys and a double dash for long keywords.
!!       call set_mode('strict')
!!       !
!!       call set_args(' --switch_X:X F --switch-Y:Y F --ox:O F -t F -x F -o F')
!!       !
!!       print all,'--switch_X or -X ... ',lget('switch_X')
!!       print all,'--switch_Y or -Y ... ',lget('switch_Y')
!!       print all,'--ox or -O       ... ',lget('ox')
!!       print all,'-o               ... ',lget('o')
!!       print all,'-x               ... ',lget('x')
!!       print all,'-t               ... ',lget('t')
!!    end program demo_set_mode
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
elemental impure subroutine set_mode(key,mode)
character(len=*),intent(in) :: key
logical,intent(in),optional :: mode
logical :: local_mode

   if(present(mode))then
      local_mode=mode
   else
      local_mode=.true.
   endif

   select case(lower(key))
   case('response_file','response file'); CLI_RESPONSE_FILE=local_mode
   case('debug');                         G_DEBUG=local_mode
   case('ignorecase');                    G_IGNORECASE=local_mode
   case('underdash');                     G_UNDERDASH=local_mode
   case('noseparator');                   G_NOSEPARATOR=local_mode
   case('strict');                        G_STRICT=local_mode
   case('lastonly');                      G_APPEND=.not.local_mode
   case default
      call journal('*set_mode* unknown key name ',key)
   end select

   if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:END'

end subroutine set_mode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_CLI2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================


!>>>>> build/dependencies/M_match/src/M_match.f90
!09/22/1980  15:38:34
!04/19/2020  11:05:06
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    M_match(3fp) - [M_MATCH] Basic Regular Expressions
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    use M_match, only: match, amatch, getpat, makpat
!!    use M_match, only: YES, MAXPAT, MAXARG, MAXLINE, EOS, NEWLINE, ERR
!!
!!##DESCRIPTION
!!    Find a string matching a regular expression.
!!
!!       *   zero or more occurrences of the previous character
!!       .   any character
!!       ^   beginning of line
!!       $   end of line
!!       []  class of characters. Inside the braces
!!
!!            ^  at the beginning of the class means to
!!               negate the class.
!!            -  if not the first or last character in
!!               the class, denotes a range of characters
!!       Escape characters:
!!        \\n  newline
!!        \\r  carriage return
!!        \\t  tab
!!        \\b  backspace
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
module M_match
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
private
public :: getpat  !....... encode regular expression for pattern matching
public :: match   !....... match pattern anywhere on line
public :: amatch  !....... look for pattern matching regular expression
public :: makpat  !....... encode regular expression for pattern matching
public :: regex_pattern
public :: bpos, epos
private :: omatch
private :: error
private :: addset
private :: dodash
private :: locate
private :: patsiz
private :: stclos
private :: getccl
private :: filset
private :: esc

integer,parameter,public :: MAXTAGS=10

interface getpat;     module procedure getpat_, getpat__;           end interface
interface makpat;     module procedure makpat_          ;           end interface
interface amatch;     module procedure amatch_, amatch__;           end interface
interface match;      module procedure match_,  match__ ;           end interface
interface omatch;     module procedure omatch_          ;           end interface

!========== STANDARD RATFOR DEFINITIONS ==========
!x!integer,parameter :: CHARACTER=INTEGER
integer,parameter :: chr=kind(1)
integer,parameter :: byte=kind(1)
integer,parameter :: def=kind(1)
!x!integer,parameter :: ANDIF=IF

integer(kind=byte),parameter :: EOF=10003_byte
integer(kind=byte),parameter,public :: EOS=10002_byte
integer(kind=byte),parameter,public :: ERR=10001_byte
integer(kind=byte),parameter,public :: YES=1_byte
!x!integer(kind=byte),parameter :: ARB=100_byte

integer(kind=byte),parameter :: ACCENT=96_byte
integer(kind=byte),parameter :: AND=38_byte
integer(kind=byte),parameter :: ATSIGN=64_byte
integer(kind=byte),parameter :: BACKSLASH=92_byte
integer(kind=byte),parameter :: BACKSPACE=8_byte
integer(kind=byte),parameter :: CR=13_byte
integer(kind=byte),parameter :: BANG=33_byte
integer(kind=byte),parameter :: BAR=124_byte
integer(kind=byte),parameter :: BIGA=65_byte
integer(kind=byte),parameter :: BIGB=66_byte
integer(kind=byte),parameter :: BIGC=67_byte
integer(kind=byte),parameter :: BIGD=68_byte
integer(kind=byte),parameter :: BIGE=69_byte
integer(kind=byte),parameter :: BIGF=70_byte
integer(kind=byte),parameter :: BIGG=71_byte
integer(kind=byte),parameter :: BIGH=72_byte
integer(kind=byte),parameter :: BIGI=73_byte
integer(kind=byte),parameter :: BIGJ=74_byte
integer(kind=byte),parameter :: BIGK=75_byte
integer(kind=byte),parameter :: BIGL=76_byte
integer(kind=byte),parameter :: BIGM=77_byte
integer(kind=byte),parameter :: BIGN=78_byte
integer(kind=byte),parameter :: BIGO=79_byte
integer(kind=byte),parameter :: BIGP=80_byte
integer(kind=byte),parameter :: BIGQ=81_byte
integer(kind=byte),parameter :: BIGR=82_byte
integer(kind=byte),parameter :: BIGS=83_byte
integer(kind=byte),parameter :: BIGT=84_byte
integer(kind=byte),parameter :: BIGU=85_byte
integer(kind=byte),parameter :: BIGV=86_byte
integer(kind=byte),parameter :: BIGW=87_byte
integer(kind=byte),parameter :: BIGX=88_byte
integer(kind=byte),parameter :: BIGY=89_byte
integer(kind=byte),parameter :: BIGZ=90_byte
integer(kind=byte),parameter,public :: BLANK=32_byte
integer(kind=byte),parameter :: CARET=94_byte
integer(kind=byte),parameter :: COLON=58_byte
integer(kind=byte),parameter :: COMMA=44_byte
integer(kind=byte),parameter :: DIG0=48_byte
integer(kind=byte),parameter :: DIG1=49_byte
integer(kind=byte),parameter :: DIG2=50_byte
integer(kind=byte),parameter :: DIG3=51_byte
integer(kind=byte),parameter :: DIG4=52_byte
integer(kind=byte),parameter :: DIG5=53_byte
integer(kind=byte),parameter :: DIG6=54_byte
integer(kind=byte),parameter :: DIG7=55_byte
integer(kind=byte),parameter :: DIG8=56_byte
integer(kind=byte),parameter :: DIG9=57_byte
integer(kind=byte),parameter :: DIGIT=2_byte
integer(kind=byte),parameter :: DOLLAR=36_byte
integer(kind=byte),parameter :: DQUOTE=34_byte
integer(kind=byte),parameter :: EQUALS=61_byte
integer(kind=byte),parameter :: ERROUT=2_byte
integer(kind=byte),parameter :: GREATER=62_byte
integer(kind=byte),parameter :: LBRACE=123_byte
integer(kind=byte),parameter :: LBRACK=91_byte
integer(kind=byte),parameter :: LESS=60_byte
integer(kind=byte),parameter :: LETA=97_byte
integer(kind=byte),parameter :: LETB=98_byte
integer(kind=byte),parameter :: LETC=99_byte
integer(kind=byte),parameter :: LETD=100_byte
integer(kind=byte),parameter :: LETE=101_byte
integer(kind=byte),parameter :: LETF=102_byte
integer(kind=byte),parameter :: LETG=103_byte
integer(kind=byte),parameter :: LETH=104_byte
integer(kind=byte),parameter :: LETI=105_byte
integer(kind=byte),parameter :: LETJ=106_byte
integer(kind=byte),parameter :: LETK=107_byte
integer(kind=byte),parameter :: LETL=108_byte
integer(kind=byte),parameter :: LETM=109_byte
integer(kind=byte),parameter :: LETN=110_byte
integer(kind=byte),parameter :: LETO=111_byte
integer(kind=byte),parameter :: LETP=112_byte
integer(kind=byte),parameter :: LETQ=113_byte
integer(kind=byte),parameter :: LETR=114_byte
integer(kind=byte),parameter :: LETS=115_byte
integer(kind=byte),parameter :: LETT=116_byte
integer(kind=byte),parameter :: LETTER=1_byte
integer(kind=byte),parameter :: LETU=117_byte
integer(kind=byte),parameter :: LETV=118_byte
integer(kind=byte),parameter :: LETW=119_byte
integer(kind=byte),parameter :: LETX=120_byte
integer(kind=byte),parameter :: LETY=121_byte
integer(kind=byte),parameter :: LETZ=122_byte
integer(kind=byte),parameter :: LPAREN=40_byte
!x!integer(kind=byte),parameter :: MAXCHARS=20_byte
integer(kind=byte),parameter,public :: MAXLINE=1024_byte       ! TYPICAL LINE LENGTH
!x!integer(kind=byte),parameter :: MAXNAME=30_byte        ! TYPICAL FILE NAME SIZE
integer(kind=byte),parameter :: MINUS=45_byte
integer(kind=byte),parameter :: NEWLINE=10_byte
integer(kind=byte),parameter,public :: NO=0_byte
integer(kind=byte),parameter :: NOERR=0_byte
integer(kind=byte),parameter :: NOT=126_byte           ! SAME AS TILDE
integer(kind=byte),parameter :: OK=-2_byte
integer(kind=byte),parameter :: OR=BAR             ! SAME AS BAR
integer(kind=byte),parameter :: PERCENT=37_byte
integer(kind=byte),parameter :: PERIOD=46_byte
integer(kind=byte),parameter :: PLUS=43_byte
integer(kind=byte),parameter :: QMARK=63_byte
integer(kind=byte),parameter :: RBRACE=125_byte
integer(kind=byte),parameter :: RBRACK=93_byte
integer(kind=byte),parameter :: READ=0_byte
integer(kind=byte),parameter :: READWRITE=2_byte
integer(kind=byte),parameter :: RPAREN=41_byte
integer(kind=byte),parameter :: SEMICOL=59_byte
integer(kind=byte),parameter :: SHARP=35_byte
integer(kind=byte),parameter :: SLASH=47_byte
integer(kind=byte),parameter :: SQUOTE=39_byte
integer(kind=byte),parameter :: STAR=42_byte
integer(kind=byte),parameter :: TAB=9_byte
integer(kind=byte),parameter :: TILDE=126_byte
integer(kind=byte),parameter :: UNDERLINE=95_byte
integer(kind=byte),parameter :: WRITE=1_byte

! HANDY MACHINE-DEPENDENT PARAMETERS, CHANGE FOR A NEW MACHINE
integer(kind=byte),parameter,public  :: MAXPAT=512
integer(kind=byte),parameter,public  :: MAXARG=512
integer(kind=byte),parameter :: MAXSUBS=10

integer(kind=byte),parameter :: COUNT=1
integer(kind=byte),parameter :: PREVCL=2
integer(kind=byte),parameter :: START=3
integer(kind=byte),parameter :: CLOSIZE=4

!x!integer(kind=byte),parameter  :: ESCAPE=ATSIGN
!x!integer(kind=byte),parameter  :: ANY=QMARK
!x!integer(kind=byte),parameter  :: BOL=PERCENT

integer(kind=byte),parameter   :: EOL=DOLLAR
integer(kind=byte),parameter   :: CLOSURE=STAR
integer(kind=byte),parameter   :: DASH=MINUS
integer(kind=byte),parameter   :: ESCAPE=BACKSLASH
integer(kind=byte),parameter   :: ANY=PERIOD
integer(kind=byte),parameter   :: BOL=CARET

integer(kind=byte),parameter :: CCL=LBRACK
integer(kind=byte),parameter :: CCLEND=RBRACK

integer(kind=byte),parameter :: NCCL=LETN
integer(kind=byte),parameter :: CHAR=LETA
integer(kind=byte),parameter :: BOSS=LBRACE        ! <
integer(kind=byte),parameter :: EOSS=RBRACE        ! >

!x!COMMON /CSUBS/ BPOS(MAXSUBS), EPOS(MAXSUBS)
integer(kind=byte) ::  bpos(maxsubs)           ! beginning of partial match
integer(kind=byte) ::  epos(maxsubs)           ! end of corresponding partial match

type :: regex_pattern
   integer :: pat(MAXPAT)
end type regex_pattern

contains
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
function f2r(string,isize)

character(len=*),parameter::ident_1="&
&@(#)M_match::f2r(3f): convert Fortran character variable to Ratfor integer array with Ratfor terminator"

character(len=*),intent(in) :: string
integer,intent(in)          :: isize
!!integer                     :: f2r(len(string)+1)
integer                     :: f2r(isize)
integer                     :: i
f2r=blank
   do i=1,len_trim(string)
      f2r(i)=ichar(string(i:i))
   enddo
   f2r(i)=eos
end function f2r
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
function r2f(ints)

character(len=*),parameter::ident_2="@(#)M_match::r2f(3f): convert Ratfor integer array to Fortran character variable"

integer,intent(in)          :: ints(:)
character(len=size(ints)-1) :: r2f
integer                     :: i
intrinsic char
   r2f=' '
   do i=1,size(ints)-1
      if(ints(i).eq.eos)then
         exit
      endif
      r2f(i:i)=char(ints(i))
   enddo
end function r2f
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getpat(3f) - [M_MATCH] convert str into pattern
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function getpat(str, pat)
!!##DESCRIPTION
!!    convert str into pattern
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function getpat_(arg, pat)

! ident_1="@(#)M_match::getpat_ convert argument into pattern"

integer(kind=def) :: getpat_
integer(kind=def) :: arg(maxarg)
integer(kind=def) :: pat(maxpat)
   getpat_ = makpat_(arg, 1, EOS, pat)
end function getpat_
!===================================================================================================================================
function getpat__(arg_str, pat)

character(len=*),intent(in)   :: arg_str
integer(kind=def),intent(out) :: pat(maxpat)
integer(kind=def)             :: getpat__
integer(kind=def)             :: arg(maxarg)
integer                       :: len_arg_str

   len_arg_str=len(arg_str)
   if(len_arg_str.gt.MAXARG-1)then
      write(*,*)'*getpat* error: input arg_str too long,',len_arg_str,' > ',MAXARG-1
      stop
   endif
   arg=f2r(arg_str,size(arg))
   getpat__ = makpat_(arg, 1, eos, pat)
end function getpat__
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    addset(3f) - [M_MATCH] put c in string(J) if it fits, increment J
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function addset(c, str, j, maxsiz)
!!##DESCRIPTION
!!   put c in string(j) if it fits, increment
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function addset(c, set, j, maxsiz)

! ident_2="@(#)M_match::addset put C in SET(J) if it fits, increment J"

integer(kind=byte)            :: addset
integer(kind=chr),intent(in)  :: c
integer(kind=chr)             :: set(:)
integer(kind=byte)            :: j
integer(kind=byte),intent(in) :: maxsiz
   if (j > maxsiz)then
      addset = NO
   else
      set(j) = c
      j = j + 1
      addset = YES
   endif
end function addset
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    dodash(3f) - [M_MATCH] expand array(i-1)-array(i+1) into set(j)... from valid
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! subroutine dodash(valid, array, i, set, j, maxset)
!!##DESCRIPTION
!!    expand array(i-1)-array(i+1) into set(j)... from valid
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
! dodash - expand array(i-1)-array(i+1) into set(j)... from valid
subroutine dodash(valid, array, i, set, j, maxset)
integer(kind=def) ::  i, j, junk, k, limit, maxset
character(len=*),intent(in) :: valid
integer(kind=chr) :: array(:)
integer(kind=chr) :: set(:)
intrinsic char
   i = i + 1
   j = j - 1
   limit = index(valid, char(esc(array, i)))
   k=index(valid,char(set(j)))
   do
      if(.not. (k.le.limit)) exit
      junk = addset(ichar(valid(k:k)), set, j, maxset)
      k=k+1
   enddo
end subroutine dodash
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    locate(3f) - [M_MATCH] look for c in char class at pat(offset)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     pure integer function locate(c, pat, offset)
!!##DESCRIPTION
!!    look for c in char class at pat(offset)
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
pure function locate(c, pat, offset)

! ident_3="@(#)M_match::locate look for c in char class at pat(offset)"

integer(kind=def)            :: locate
integer(kind=chr),intent(in) :: c
integer(kind=chr),intent(in) :: pat(maxpat)
integer(kind=def),intent(in) :: offset
integer(kind=def)            :: i
   ! size of class is at pat(offset), characters follow

   !x!for (i = offset + pat(offset); i > offset; i = i - 1)

   locate = NO
   LOC: do i = offset + pat(offset), offset+1,  -1
      if (c == pat(i)) then
         locate = YES
         exit LOC
      endif
   enddo LOC
end function locate
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    match(3f) - [M_MATCH] find match to a basic regular expression anywhere on input string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer function match(line, pattern)
!!
!!     character(len=*),intent(in) :: line
!!     integer,intent(in)          :: pattern(MAXPAT)
!!
!!##DESCRIPTION
!!  Given a BRE(Basic Regular Expression) converted to a pattern
!!  return whether an input string matches it.
!!
!!##OPTIONS
!!    LIN  string to search for a match to the pattern
!!    PAT  pattern generated from a BRE using getpat(3f) or makpat(3f).
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_match
!!     use :: M_match, only : getpat, match
!!     use :: M_match, only : MAXPAT, MAXARG, MAXLINE, YES, ERR
!!     implicit none
!!     ! find _ find patterns in text
!!     integer                      :: pat(MAXPAT)
!!     character(len=MAXARG-1)      :: argument
!!     integer                      :: stat
!!     integer                      :: ios
!!     integer                      :: len_arg
!!     character(len=MAXLINE-2)     :: line
!!     call get_command_argument(1, argument,status=stat,length=len_arg)
!!     if(stat.ne.0.or.argument.eq.'')then
!!        write(*,*)"usage: find pattern."
!!     elseif(getpat(argument(:len_arg), pat) .eq. ERR) then
!!        write(*,*)"illegal pattern."
!!     else
!!        INFINITE: do
!!           read(*,'(a)',iostat=ios)line
!!           if(ios.ne.0)exit
!!           if(match(trim(line), pat) .eq. YES) then
!!              write(*,'(*(a))')trim(line)
!!           endif
!!        enddo INFINITE
!!     endif
!!     end program demo_match
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!
!!##LICENSE
!!   Public Domain
function match_(lin, pat)

! ident_4="@(#)M_match::match find match anywhere on line"

integer(kind=def) :: match_
integer(kind=chr) :: lin(maxline), pat(maxpat)
integer(kind=def) :: i

   if (pat(1) == bol) then            ! anchored match
      if (amatch_(lin, 1, pat) > 0) then
         match_ = yes
         return
      endif
   else               ! unanchored
      !- for (i = 1; lin(i) /= eos; i = i + 1)
      i=1
      do while (lin(i) /= eos)
         if (amatch_(lin, i, pat) > 0) then
            match_ = yes
            return
         endif
         i=i+1
      enddo
   endif
   match_ = no
end function match_
!==================================================================================================================================!
function match__(lin_str, pat)

! ident_5="@(#)M_match::match find match anywhere on line"

character(len=*),intent(in) :: lin_str
integer(kind=def) :: match__
integer(kind=chr) :: lin(maxline), pat(maxpat)
   lin=f2r(lin_str,size(lin))
   match__=match_(lin,pat)
end function match__
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    patsiz(3f) - [M_MATCH] returns size of pattern entry at pat(n)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function patsiz(pat, n)
!!##DESCRIPTION
!!  returns size of pattern entry at pat(n)
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function patsiz(pat, n)

! ident_6="@(#)M_match::patsiz returns size of pattern entry at pat(n)"

integer(kind=def) :: patsiz
integer(kind=chr) :: pat(MAXPAT)
integer(kind=def) :: n

   select case(pat(n))
    case(CHAR,BOSS,EOSS)
      patsiz = 2
    case(BOL,EOL,ANY)
      patsiz = 1
    case(CCL,NCCL)
      patsiz = pat(n + 1) + 2
    case(CLOSURE)                  ! optional
      patsiz = CLOSIZE
    case default
      call error("in patsiz: cannot happen.")
   end select

end function patsiz
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    stclos(3f) - [M_MATCH] insert CLOSURE entry at pat(j)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function stclos(pat, j, lastj, lastcl)
!!##DESCRIPTION
!!  insert CLOSURE entry at pat(j)
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function stclos(pat, j, lastj, lastcl)

! ident_7="@(#)M_match::stclos insert closure entry at pat(j)"

integer(kind=def) :: stclos
integer(kind=chr) :: pat(maxpat)
integer(kind=def) :: j, jp, jt, junk, lastcl, lastj

   !- for (jp = j - 1; jp >= lastj; jp = jp - 1) <   ! make a hole
   do jp = j - 1, lastj,  - 1   ! make a hole
      jt = jp + closize
      junk = addset(pat(jp), pat, jt, maxpat)
   enddo
   j = j + closize
   stclos = lastj
   junk = addset(closure, pat, lastj, maxpat)   ! put closure in it
   junk = addset(0, pat, lastj, maxpat)      ! count
   junk = addset(lastcl, pat, lastj, maxpat)   ! prevcl
   junk = addset(0, pat, lastj, maxpat)      ! start
end function stclos
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getccl(3f) - [M_MATCH] expand char class at arg(i) into pat(j)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function getccl(arg, i, pat, j)
!!##DESCRIPTION
!!    expand char class at arg(i) into pat(j)
!!##OPTIONS
!!    ARG  ADE string array
!!    I    index into ARG
!!    PAT  encoded regular expression
!!    J    .
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function getccl(arg, i, pat, j)

! ident_8="@(#)M_match::getccl expand char class at arg(i) into pat(j)"

integer(kind=def) :: getccl
integer(kind=chr)  :: arg(maxarg), pat(maxpat)
integer(kind=def) :: i, j, jstart, junk

   i = i + 1      ! skip over [
   if (arg(i) == tilde .or. arg(i) == caret) then
      junk = addset(nccl, pat, j, maxpat)
      i = i + 1
   else
      junk = addset(ccl, pat, j, maxpat)
   endif
   jstart = j
   junk = addset(0, pat, j, maxpat)      ! leave room for count
   call filset(cclend, arg, i, pat, j, maxpat)
   pat(jstart) = j - jstart - 1
   if (arg(i) == cclend)then
      getccl = ok
   else
      getccl = err
   endif
end function getccl
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    filset(3f) - [M_MATCH] expand set at array(i) into set(j), stop at delim
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! subroutine filset(delim, array, i, set, j, maxset)
!!##DESCRIPTION
!!   expand set at array(i) into set(j), stop at delim
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
subroutine filset(delim, array, i, set, j, maxset)

! ident_9="@(#)M_match::filset expand set at  array(i)  into  set(j),  stop at  delim"

integer(kind=def) :: i, j, junk, maxset
integer(kind=chr) :: array(:), delim, set(:)

character(len=*),parameter :: digits= "0123456789"
character(len=*),parameter :: lowalf= "abcdefghijklmnopqrstuvwxyz"
character(len=*),parameter :: upalf= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
intrinsic char

   !-  for ( ; array(i) /= delim .and. array(i) /= eos; i = i + 1)
   do while( array(i) /= delim .and. array(i) /= eos)
      if (array(i) == escape) then
         junk = addset(esc(array, i), set, j, maxset)
      elseif (array(i) /= dash) then
         junk = addset(array(i), set, j, maxset)
      elseif (j <= 1 .or. array(i+1) == eos) then   ! literal -
         junk = addset(dash, set, j, maxset)
      elseif (index(digits, char(set (j - 1))) > 0) then
         call dodash(digits, array, i, set, j, maxset)
     elseif (index(lowalf, char(set (j - 1))) > 0) then
         call dodash(lowalf, array, i, set, j, maxset)
      elseif (index(upalf, char(set (j - 1))) > 0) then
         call dodash(upalf, array, i, set, j, maxset)
      else
         junk = addset (DASH, set, j, maxset)
      endif
      i=i+1
   enddo
end subroutine filset
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    esc(3f) - [M_MATCH] map array(i) into escaped character if appropriate
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer function esc(array, i)
!!    integer,intent(in) :: array(*)
!!    integer            :: i
!!
!!##DESCRIPTION
!!    To support commonly used non-printable characters escaped strings are
!!    supported. When the ESCAPE character is encountered the following
!!    character is examined. If one of the special characters ESC(3f) will
!!    increment I and return the designated non-printable character. Otherwise
!!    it will return the character as-is.
!!
!!    o convert \n to newline
!!    o convert \t to horizontal tab
!!    o convert \r to carriage return
!!    o convert \b to backspace
!!    o convert \nnn to character represented by octal value
!!
!!##OPTIONS
!!    ARRAY  array of ADE (ASCII Decimal Equivalent) values terminated by
!!           an EOS (End-Of-String) character representing a string to scan
!!           for escaped characters.
!!    I      pointer into ARRAY. It is incremented to the position of the
!!           next character in ARRAY on return.
!!
!!##RETURNS
!!    ESC    The ADE for the substituted character
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!
!!##LICENSE
!!   Public Domain
function esc(array, i)

! ident_10="@(#)M_match::esc map  array(i)  into escaped character if appropriate"

integer(kind=chr) :: esc
integer(kind=chr) :: array(:)
integer(kind=def) :: i

   if (array(i) /= escape)then                         ! if not an escape character, return the character as-is
      esc = array(i)
   elseif (array(i+1) == eos)then                      ! if ESCAP is the last character it is left as-is and is not special
      esc = escape
   else
      i = i + 1                                        ! increment I to look at character after ESCAP
      select case(array(i))                            ! make substitution to special character for designated characters
      case(ichar('n'),ichar('N'));  esc = newline
      case(ichar('t'),ichar('T')); esc = tab
      case(ichar('b'),ichar('B')); esc = backspace
      case(ichar('r'),ichar('R')); esc = cr
      case(dig0:dig7)
         !- for (esc = 0; array(i) >= dig0 .and. array(i) <= dig7; i = i + 1)
         esc=0
         do while (array(i) >= dig0 .and.  array(i) <= dig7)
            i = i + 1
            esc = 8*esc + array(i) - dig0
            i = i - 1                                  ! so like other cases
         enddo
      case default
         esc = array(i)                                ! otherwise just copy character
      end select
   endif
end function esc
!----------------------------------------------------------------------------------------------------------------------------------!
! Conventional C Constants
!   Oct  Dec  Hex  Char
!   -----------------------
!   000  0    00   NUL '\0'   Null
!   007  7    07   BEL '\a'   Bell
!  *010  8    08   BS  '\b'   Backspace
!  *011  9    09   HT  '\t'   Horizontal Tab
!  *012  10   0A   LF  '\n'   Line Feed
!   013  11   0B   VT  '\v'   Vertical Tab
!   014  12   0C   FF  '\f'   Form Feed
!  *015  13   0D   CR  '\r'   Carriage Return
!   134  92   5C   \   '\\'   Backslash
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    omatch(3f) - [M_MATCH] try to match a single pattern at pat(j)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function omatch(lin, i, pat, j)
!!##DESCRIPTION
!!    try to match a single pattern at pat(j)
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function omatch_(lin, i, pat, j)

! ident_11="@(#)M_match::omatch_ try to match a single pattern at pat(j)"

integer(kind=def) ::  omatch_
integer(kind=chr)  :: lin(maxline), pat(maxpat)
integer(kind=def) :: bump, i, j, k

   omatch_ = no
   if (lin(i) == eos)then
      return
   endif
   bump = -1
   if (pat(j) == char) then
      if (lin(i) == pat(j + 1))then
         bump = 1
      endif
   elseif (pat(j) == bol) then
      if (i == 1)then
         bump = 0
      endif
   elseif (pat(j) == any) then
      if (lin(i) /= newline)then
         bump = 1
      endif
   elseif (pat(j) == eol) then
      if (lin(i) == newline .or. lin(i) == eos)then
         bump = 0
      endif
   elseif (pat(j) == ccl) then
      if (locate(lin(i), pat, j + 1) == yes)then
         bump = 1
      endif
   elseif (pat(j) == nccl) then
      if (lin(i) /= newline .and. locate(lin(i), pat, j + 1) == no)then
         bump = 1
      endif
   elseif (pat(j) == boss) then
      k = pat(j+1)
      bpos(k+1) = i
      bump = 0
   elseif (pat(j) == eoss) then
      k = pat(j+1)
      epos(k+1) = i
      bump = 0
   else
      call error("in omatch_: can't happen.")
   endif
   if (bump >= 0) then
      i = i + bump
      omatch_ = yes
   endif
end function omatch_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    amatch(3f) - [M_MATCH] look for pattern matching regular expression; returns its location
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    loc = amatch(line, from, pat, tagbeg, tagend)
!!
!!     character(len=*),intent(in) :: line
!!     integer,intent(in)          :: from
!!     character                   :: pat(MAXPAT)
!!     integer                     :: tagbeg(MAXTAGS), tagend(MAXTAGS)
!!     integer                     :: loc
!!##DESCRIPTION
!!    AMATCH scans LINE starting at location FROM, looking
!!    for a pattern which matches the regular expression coded
!!    in PAT. If the pattern is found, its starting location
!!    in LINE is returned. If the pattern is not found, AMATCH
!!    returns 0.
!!
!!    The regular expression in PAT must have been previously
!!    encoded by GETPAT(3f) or MAKPAT(3f). (For a complete description
!!    of regular expressions, see the manpage for M_match.)
!!
!!    AMATCH(3f) is a special-purpose version of MATCH(3f), which should
!!    be used in most cases.
!!##OPTIONS
!!    LINE           input line to scan
!!    FROM           beginning location to start scan from
!!    PAT            coded regular expression encoded by GETPAT(3f) or MAKPAT(3f)
!!    TAGBEG,TAGEND  element "i + 1" returns start or end, respectively, of "i"th tagged subpattern
!!##RETURNS
!!    LOC   returns location match was found or zero (0) if no match remains
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_amatch
!!     use :: M_match, only : getpat, amatch
!!     use :: M_match, only : MAXPAT, MAXARG, MAXLINE, MAXTAGS, YES, ERR
!!     implicit none
!!     ! find _ find patterns in text
!!     integer                      :: pat(MAXPAT)
!!     character(len=MAXARG-1)      :: argument
!!     integer                      :: stat
!!     integer                      :: ios
!!     integer                      :: len_arg
!!     integer                      :: loc
!!     integer                      :: ii
!!     character(len=MAXLINE-2)     :: line
!!     integer                      :: tagbeg(MAXTAGS),tagend(MAXTAGS)
!!     call get_command_argument(1, argument,status=stat,length=len_arg)
!!     if(stat.ne.0.or.argument.eq.'')then
!!        write(*,*)"usage: find pattern."
!!     elseif(getpat(argument(:len_arg), pat) .eq. ERR) then
!!        write(*,*)"illegal pattern."
!!     else
!!        INFINITE: do
!!           read(*,'(a)',iostat=ios)line
!!           tagbeg=-9999;tagend=-9999
!!           if(ios.ne.0)exit
!!           loc = amatch(trim(line), 1, pat, tagbeg, tagend) ! returns location/0
!!           if(loc.gt.0)then ! matched; if no match, loc is returned as 0
!!              write(*,'(*(a))')trim(line)
!!              ! (element "i + 1" returns start or end, respectively, of "i"th tagged subpattern)
!!              write(*,'(*(i0,1x,i0,1x,i0,/))')(ii,tagbeg(ii),tagend(ii),ii=1,size(tagbeg))
!!           endif
!!        enddo INFINITE
!!     endif
!!     end program demo_amatch
!!
!!##SEE ALSO
!!    match, getpat, makpat
!!##DIAGNOSTICS
!!    None
!!##AUTHOR
!!    John S. Urban
!!##REFERENCE
!!    "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!    Public Domain
function amatch_(lin, from, pat)

! ident_12="@(#)M_match::amatch_  (non-recursive) look for match starting at lin(from)"

integer(kind=def) :: amatch_
integer(kind=chr)  :: lin(maxline), pat(maxpat)
integer(kind=def) :: from, i, j, offset, stack
   stack = 0
   offset = from      ! next unexamined input character
!-   for (j = 1; j <= maxsubs; j = j + 1) <     ! clear partial match results
   do j = 1, maxsubs     ! clear partial match results
      bpos(j) = offset
      epos(j) = offset
   enddo
!-   for (j = 1; pat(j) /= eos; j = j + patsiz(pat, j))
   j=1
   do while (pat(j) /= eos)
      if (pat(j) == closure) then   ! a closure entry
         stack = j
         j = j + closize      ! step over closure
         !- for (i = offset; lin(i) /= eos; )   ! match as many as possible
         i = offset
         do while ( lin(i) /= eos )                 ! match as many as
            if (omatch_(lin, i, pat, j) == no)then   ! possible
               exit
            endif
         enddo
         pat(stack+count) = i - offset
         pat(stack+start) = offset
         offset = i      ! character that made us fail
      elseif (omatch_(lin, offset, pat, j) == no) then   ! non-closure
!-         for ( ; stack > 0; stack = pat(stack+prevcl))
         do while (stack >0)
            if (pat(stack+count) > 0)then
               exit
            endif
            stack = pat(stack+prevcl)
         enddo
         if (stack <= 0) then      ! stack is empty
            amatch_ = 0            ! return failure
            return
         endif
         pat(stack+count) = pat(stack+count) - 1
         j = stack + closize
         offset = pat(stack+start) + pat(stack+count)
      endif
      j = j + patsiz(pat, j)
   enddo ! else omatch_ succeeded
   epos(1) = offset
   amatch_ = offset
   ! success
end function amatch_
!==================================================================================================================================!
function amatch__(lin_str, from, pat)

! ident_13="@(#)M_match::amatch"

character(len=*),intent(in) :: lin_str
integer,intent(in) :: from
integer(kind=def)  :: amatch__
integer(kind=chr)  :: pat(maxpat)
integer(kind=chr)  :: lin(maxline)
   lin=f2r(lin_str,size(lin))
   amatch__=amatch_(lin,from,pat)
end function amatch__
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    makpat(3f) - [M_MATCH] make pattern from arg(from), terminate on delim
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function makpat(arg, from, delim, pat)
!!##DESCRIPTION
!!  make pattern from arg(from), terminate on delim
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function makpat_(arg, from, delim, pat)

! ident_14="@(#)M_match::makpat_ make pattern from arg(from), terminate at delim"

integer(kind=def) :: makpat_
integer(kind=chr)  :: arg(maxarg), delim, pat(maxpat)
integer(kind=def) :: from, i, j, junk, lastcl, lastj, lj, nsubs, sp, substk(maxsubs)

   j = 1      ! pat index
   lastj = 1
   lastcl = 0
   nsubs = 0  ! counts number of @(@) pairs
   sp = 0     ! stack pointer for substk
   !-   for (i = from; arg(i) /= delim .and. arg(i) /= eos; i = i + 1) <
   i=from
   do while ( arg(i) /= delim .and. arg(i) /= eos )
      lj = j
      if (arg(i) == any)then
         junk = addset(any, pat, j, maxpat)
      elseif (arg(i) == bol .and. i == from)then
         junk = addset(bol, pat, j, maxpat)
      elseif (arg(i) == eol .and. arg(i + 1) == delim)then
         junk = addset(eol, pat, j, maxpat)
      elseif (arg(i) == ccl) then
         if (getccl(arg, i, pat, j) == err)then
            exit
         endif
      elseif (arg(i) == closure .and. i > from) then
         lj = lastj
         !x!if(pat(lj)==bol .or. pat(lj)==eol .or. pat(lj)==closure .or. pat(lj-1) == boss .or. pat(lj-1) == eoss) then
         if(pat(lj)==bol .or. pat(lj)==eol .or. pat(lj)==closure .or. pat(lj) == boss .or. pat(lj) == eoss) then
            exit
         endif
         lastcl = stclos(pat, j, lastj, lastcl)
      elseif (arg(i) == escape .and. arg(i+1) == lparen) then
         nsubs = nsubs + 1
         if (nsubs >= maxsubs)then
            exit
         endif
         junk = addset(boss, pat, j, maxpat)
         junk = addset(nsubs, pat, j, maxpat)
         sp = sp + 1
         substk(sp) = nsubs
         i = i + 1
      elseif (arg(i) == escape .and. arg(i+1) == rparen) then
         if (sp <= 0)then
            exit
         endif
         junk = addset(eoss, pat, j, maxpat)
         junk = addset(substk(sp), pat, j, maxpat)
         sp = sp - 1
         i = i + 1
      else
         junk = addset(char, pat, j, maxpat)
         junk = addset(esc(arg, i), pat, j, maxpat)
      endif
      lastj = lj
      i=i+1
   enddo
   if (arg(i) /= delim .or. sp /= 0)then   ! terminated early
      makpat_ = err
   elseif (addset(eos, pat, j, maxpat) == no)then   ! no room
      makpat_ = err
   else
      makpat_ = i
   endif
end function makpat_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    error(3f) - [M_MATCH] print message and stop program execution
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine error(line)
!!##DESCRIPTION
!!    print message and stop program execution
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##REFERENCE
!!    "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!    Public Domain
subroutine error(message)

! ident_15="@(#)M_match::error(3f): print message and stop program execution"

character(len=*),intent(in) :: message
   write(stderr,'(a)')trim(message)
   stop
end subroutine error
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
end module M_match
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!


!>>>>> build/dependencies/M_strings/src/M_strings.F90
!>
!!##NAME
!!    M_strings(3f) - [M_strings::INTRO] Fortran string module
!!
!!##DESCRIPTION
!!    The M_strings(3fm) module is a collection of Fortran procedures
!!    that supplement the built-in intrinsic string routines. Routines
!!    for parsing, tokenizing, changing case, substituting new strings for
!!    substrings, locating strings with simple wildcard expressions, removing
!!    tabs and line terminators and other string manipulations are included.
!!
!!    M_strings_oop(3fm) is a companion module that provides an OOP interface
!!    to the M_strings module.
!!
!!##SYNOPSIS
!!
!!  public entities:
!!
!!      use M_strings,only : split, sep, delim, chomp, strtok
!!      use M_strings,only : split2020, find_field
!!      use M_strings,only : substitute, change, modif, transliterate, &
!!              & reverse, squeeze
!!      use M_strings,only : replace, join
!!      use M_strings,only : upper, lower, upper_quoted
!!      use M_strings,only : rotate13
!!      use M_strings,only : adjustc, compact, nospace, indent
!!      use M_strings,only : crop, clip, unquote, quote, matching_delimiter
!!      use M_strings,only : len_white, pad, lpad, cpad, rpad, zpad, &
!!              & stretch, lenset, merge_str
!!      use M_strings,only : switch, s2c, c2s
!!      use M_strings,only : noesc, notabs, dilate, expand, visible
!!      use M_strings,only : longest_common_substring
!!      use M_strings,only : string_to_value, string_to_values, s2v, s2vs
!!      use M_strings,only : int, real, dble, nint
!!      use M_strings,only : atoi, atol, aton
!!      use M_strings,only : value_to_string, v2s, msg
!!      use M_strings,only : listout, getvals
!!      use M_strings,only : glob, ends_with
!!      use M_strings,only : paragraph
!!      use M_strings,only : base, decodebase, codebase, base2
!!      use M_strings,only : isalnum, isalpha, iscntrl, isdigit
!!      use M_strings,only : isgraph, islower, isprint, ispunct
!!      use M_strings,only : isspace, isupper, isascii, isblank, isxdigit
!!      use M_strings,only : isnumber
!!      use M_strings,only : fortran_name
!!      use M_strings,only : describe
!!      use M_strings,only : edit_distance
!!      use M_strings,only : bundle
!!
!!   TOKENS
!!
!!       split  subroutine parses string using specified delimiter characters
!!              and stores tokens into an array
!!       sep    function interface to split(3f)
!!       delim  subroutine parses string using specified delimiter characters
!!              and store tokens into an array
!!       chomp  function consumes input line as it returns next token in a
!!              string using specified delimiters
!!       paragraph    convert a string into a paragraph
!!       strtok tokenize a string like C strtok(3c) routine
!!
!!       CONTRIBUTIONS
!!
!!       split2020   split a string using prototype of proposed standard
!!                   procedure
!!       find_field  token a string
!!
!!   EDITING
!!
!!       substitute     subroutine non-recursively globally replaces old
!!                      substring with new substring
!!       replace        function non-recursively globally replaces old
!!                      substring with new substring using allocatable string
!!                      (version of substitute(3f) without limitation on
!!                      length of output string)
!!       change         subroutine non-recursively globally replaces old
!!                      substring with new substring with a directive like
!!                      line editor
!!       modif          subroutine modifies a string with a directive like the
!!                      XEDIT line editor MODIFY command
!!       transliterate  replace characters found in set one with characters
!!                      from set two
!!       reverse        reverse character order in a string
!!       join           join an array of CHARACTER variables with specified
!!                      separator
!!       rotate13       apply trivial encryption algorithm ROT13 to a string
!!       squeeze        delete adjacent duplicate characters from a string
!!
!!   CASE
!!
!!       upper          function converts string to uppercase
!!       lower          function converts string to miniscule
!!       upper_quoted   function converts string to uppercase skipping strings
!!                      quoted per Fortran rules
!!
!!   STRING LENGTH AND PADDING
!!
!!       len_white  find location of last non-whitespace character
!!       lenset     return a string of specified length
!!       pad        return a string of at least specified length
!!       zpad       pad integer or string to length with zero characters
!!                  on left
!!       lpad       convert scalar intrinsic to a string padded on left to
!!                  specified length
!!       cpad       convert scalar intrinsic to a centered string of the
!!                  specified length
!!       rpad       convert scalar intrinsic to a string padded on right to
!!                  specified length
!!       stretch    return a string of at least specified length with suffix
!!       merge_str  make strings of equal length and then call MERGE(3f)
!!                  intrinsic
!!   WHITE SPACE
!!
!!       adjustc  elemental function centers text within the length of the
!!                input string
!!       compact  left justify string and replace duplicate whitespace with
!!                single characters or nothing
!!       nospace  function replaces whitespace with nothing
!!       indent   find number of leading spaces
!!       crop     function trims leading and trailing spaces and control
!!                characters
!!       clip     function trims leading and trailing spaces
!!
!!       See Also: squeeze
!!
!!   QUOTES
!!
!!       matching_delimiter  find position of matching delimiter
!!       unquote  remove quotes from string as if read with list-directed input
!!       quote    add quotes to string as if written with list-directed input
!!
!!
!!   CHARACTER ARRAY VERSUS STRING
!!
!!       switch  switch between a string and an array of single characters
!!       s2c     convert string to array of single characters and add null
!!               terminator for passing to C
!!       c2s     convert null-terminated array of single characters to
!!               string for converting strings returned from C
!!
!!   NONALPHA
!!
!!       noesc    convert non-printable ASCII8 characters to a space
!!       notabs   convert tabs to spaces while maintaining columns,
!!                assuming tabs are set every 8 characters
!!       dilate   function to convert tabs to spaces assuming tabs are set
!!                every 8 characters
!!       expand   expand escape sequences in a string
!!       visible  expand escape sequences in a string to "control" and
!!                meta-control representations
!!
!!   NUMERIC STRINGS
!!
!!       string_to_value   generic subroutine returns numeric value (REAL,
!!                         DOUBLEPRECISION, INTEGER) from string
!!       string_to_values  subroutine reads an array of numbers from a string
!!       getvals           subroutine reads a relatively arbitrary number
!!                         of values from a string using list-directed read
!!       s2v               function returns DOUBLEPRECISION numeric value
!!                         from string
!!       s2vs              function returns a DOUBLEPRECISION array of numbers
!!                         from a string
!!       s2vs              function returns a DOUBLEPRECISION array of numbers
!!                         from a string
!!       atoi              function returns INTEGER(kind=int32)  from a string
!!       atol              function returns INTEGER(kind=int64)  from a string
!!       aton              changes string to numeric value
!!       msg               append the values of up to nine values into a string
!!
!!       value_to_string   generic subroutine returns string given numeric value
!!                         (REAL, DOUBLEPRECISION, INTEGER, LOGICAL )
!!       v2s               generic function returns string from numeric value
!!                         (REAL, DOUBLEPRECISION, INTEGER )
!!       listout           expand a list of numbers where negative numbers
!!                         denote range ends (1 -10 means 1 thru 10)
!!       isnumber          determine if string represents a number
!!
!!   CHARACTER TESTS
!!
!!       glob        compares given string for match to pattern which may
!!                   contain wildcard characters
!!       ends_with   test whether strings ends with one of the specified suffixes
!!
!!       o isalnum   returns .true. if character is a letter or digit
!!       o isalpha   returns .true. if character is a letter and
!!                   .false. otherwise
!!       o iscntrl   returns .true. if character is a delete character or
!!                   ordinary control character
!!       o isdigit   returns .true. if character is a digit (0,1,...,9)
!!                   and .false. otherwise
!!       o isgraph   returns .true. if character is a printable character
!!                   except a space is considered non-printable
!!       o islower   returns .true. if character is a miniscule letter (a-z)
!!       o isprint   returns .true. if character is an ASCII printable
!!                   character
!!       o ispunct   returns .true. if character is a printable punctuation
!!                   character
!!       o isspace   returns .true. if character is a null, space, tab,
!!                   carriage return, new line, vertical tab, or formfeed
!!       o isupper   returns .true. if character is an uppercase letter (A-Z)
!!       o isascii   returns .true. if the character is in the range char(0)
!!                   to char(127)
!!       o isblank   returns .true. if character is a blank character
!!                   (space or horizontal tab.
!!       o isxdigit  returns .true. if character is a hexadecimal digit
!!                   (0-9, a-f, or A-F).
!!
!!       fortran_name   returns .true. if input string is a valid Fortran name
!!
!!   BASE CONVERSION
!!
!!       base       convert whole number string in base [2-36] to string
!!                  in alternate base [2-36]
!!       base2      convert INTEGER to a string representing a binary value
!!       codebase   convert whole number string in base [2-36] to base
!!                  10 number
!!       decodebase convert whole number in base 10 to string in base [2-36]
!!
!!   MISCELLANEOUS
!!
!!       bundle     return up to twenty strings of arbitrary length as an array
!!       describe   returns a string describing the name of a single character
!!       edit_distance  returns a naive edit distance using the Levenshtein
!!                      distance algorithm
!!       longest_common_substring  function that returns the longest common
!!                                 substring of two strings.
!!
!!   INTRINSICS
!!
!!    The M_strings(3fm) module supplements and works in combination with
!!    the Fortran built-in intrinsics. Stand-alone Fortran lets you access
!!    the characters in a string using ranges much like they are character
!!    arrays, assignment, comparisons with standard operators, supports
!!    dynamically allocatable strings and supports concatenation using the //
!!    operator, as well as a number of intrinsic string routines:
!!
!!        adjustl             Left adjust a string
!!        adjustr             Right adjust a string
!!        index               Position of a substring within a string
!!        repeat              Repeated string concatenation
!!        scan                Scan a string for the presence of a set
!!                            of characters
!!        trim                Remove trailing blank characters of a string
!!        verify              Scan a string for the absence of a set of
!!                            characters
!!        len                 It returns the length of a character string
!!        achar               converts an integer into a character
!!        iachar              converts a character into an integer
!!        len_trim            finds length of string with trailing spaces
!!                            ignored
!!        new_line            Newline character
!!        selected_char_kind  Choose character kind
!!        lge                 Lexical greater than or equal
!!        lgt                 Lexical greater than
!!        lle                 Lexical less than or equal
!!        llt                 Lexical less than
!!
!!   OOPS INTERFACE
!!
!!    The M_strings_oop(3fm) module (included with the M_strings(3fm)
!!    module) provides an OOP (Object-Oriented Programming) interface to
!!    the M_strings(3fm) module.
!!
!!##SEE ALSO
!!    There are additional routines in other GPF modules for working with
!!    expressions (M_calculator), time strings (M_time), random strings
!!    (M_random, M_uuid), lists (M_list), and interfacing with the C regular
!!    expression library (M_regex).
!!
!!##EXAMPLES
!!
!!    Each of the procedural functions includes an example program in the
!!    corresponding man(1) page for the function. The object-oriented
!!    interface does not have individual man(1) pages, but is instead
!!    demonstrated using the following example program:
!!
!!     program demo_M_strings
!!     use M_strings,only : split, sep, delim, chomp, strtok
!!     use M_strings,only : split2020, find_field
!!     use M_strings,only : substitute, change, modif, transliterate, &
!!             & reverse, squeeze
!!     use M_strings,only : replace, join
!!     use M_strings,only : upper, lower, upper_quoted
!!     use M_strings,only : rotate13
!!     use M_strings,only : adjustc, compact, nospace, indent
!!     use M_strings,only : crop, clip, unquote, quote, matching_delimiter
!!     use M_strings,only : len_white, pad, lpad, cpad, rpad, zpad, &
!!             & stretch, lenset, merge_str
!!     use M_strings,only : switch, s2c, c2s
!!     use M_strings,only : noesc, notabs, dilate, expand, visible
!!     use M_strings,only : longest_common_substring
!!     use M_strings,only : string_to_value, string_to_values, s2v, s2vs
!!     use M_strings,only : int, real, dble, nint
!!     use M_strings,only : atoi, atol, aton
!!     use M_strings,only : value_to_string, v2s, msg
!!     use M_strings,only : listout, getvals
!!     use M_strings,only : glob, ends_with
!!     use M_strings,only : paragraph
!!     use M_strings,only : base, decodebase, codebase, base2
!!     use M_strings,only : isalnum, isalpha, iscntrl, isdigit
!!     use M_strings,only : isgraph, islower, isprint, ispunct
!!     use M_strings,only : isspace, isupper, isascii, isblank, isxdigit
!!     use M_strings,only : isnumber
!!     use M_strings,only : fortran_name
!!     use M_strings,only : describe
!!     use M_strings,only : edit_distance
!!     use M_strings,only : bundle
!!     end program demo_M_strings
!!
!!   Expected output
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
MODULE M_strings !
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT        ! access computing environment
use, intrinsic :: iso_fortran_env, only : output_unit, stderr=>error_unit
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none

! ident_1="@(#) M_strings(3f) Fortran module containing routines that deal with character strings"

!-----------------------------------------------------------------------------------------------------------------------------------
private

!----------------------# TOKENS
public split           !  subroutine parses a string using specified delimiter characters and store tokens into an allocatable array
public sep             !  function interface to split
public chomp           !  function consumes input line as it returns next token in a string using specified delimiters
public delim           !  subroutine parses a string using specified delimiter characters and store tokens into an array
public strtok          !  gets next token. Used by change(3f)
public paragraph       !  convert a long string into a paragraph
!----------------------# EDITING
public substitute      !  subroutine non-recursively globally replaces old substring with new substring in string
public replace         !  function non-recursively globally replaces old substring with new substring in string
public change          !  replaces old substring with new substring in string with a directive like a line editor
public modif           !  change string using a directive using rules similar to XEDIT line editor MODIFY command
public transliterate   !  when characters in set one are found replace them with characters from set two
public reverse         !  elemental function reverses character order in a string
public join            !  append an array of character variables with specified separator into a single CHARACTER variable
public squeeze         !  delete adjacent duplicate characters from a string
public rotate13        !  apply trivial encryption algorithm ROT13 to string
!----------------------# CHARACTER ARRAY VERSUS STRING
public switch          !  generic switch between a string and an array of single characters (a2s,s2a)
private a2s            !  function to copy char array to string
private s2a            !  function to copy string(1:Clen(string)) to char array
public s2c             !  convert character variable to array of character(len=1) with null terminator for C compatibility
public c2s             !  convert null-terminated array of character(len=1) to string for strings returned by C
!----------------------# CASE
public upper           !  elemental function converts string to uppercase
public lower           !  elemental function converts string to miniscule
public upper_quoted    !  elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules
!----------------------# WHITE SPACE
public adjustc         !  elemental function centers string within the length of the input string
public compact         !  left justify string and replace duplicate whitespace with single characters or nothing
public nospace         !  function replaces whitespace with nothing
public indent          !  count number of leading spaces
public crop            !  function trims leading and trailing spaces and control characters
public clip            !  function trims leading and trailing spaces
!----------------------# QUOTES
public matching_delimiter !  find position of matching delimiter
public unquote         !  remove quotes from string as if read with list-directed input
public quote           !  add quotes to string as if written with list-directed input
!----------------------# STRING LENGTH
public lenset          !  return a string as specified length
public pad             !  return a string of at least specified length
public zpad            !  return a string of at least specified length padded on left with zeros
interface zpad;    module procedure zpad_scalar, zpad_vector;  end interface
public lpad            !  convert value to a string of at least specified length padded on left with zeros
interface lpad;    module procedure lpad_scalar, lpad_vector;  end interface
public cpad            !  convert value to a centered string of at least specified length
interface cpad;    module procedure cpad_scalar, cpad_vector;  end interface
public rpad            !  convert value to a string of at least specified length padded on right with zeros
interface rpad;    module procedure rpad_scalar, rpad_vector;  end interface
public stretch         !  return a string of at least specified length with suffix
public merge_str       !  make strings of equal length and then call MERGE(3f) intrinsic
public len_white       !  find location of last non-whitespace character
!----------------------# NONALPHA
public noesc           !  elemental function converts non-printable ASCII8 characters to a space
public notabs          !  convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters
public dilate          !  convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters
public expand          !  expand escape sequences in a string
public visible         !  expand escape sequences in a string to control and meta-control representations
!----------------------# NUMERIC STRINGS
public string_to_value !  generic subroutine returns REAL|DOUBLEPRECISION|INTEGER value from string (a2d,a2r,a2i)
 private a2d           !  subroutine returns double value from string
 private a2r           !  subroutine returns real value from string
 private a2i           !  subroutine returns integer value from string
public string_to_values!  subroutine returns values from a string
public getvals         !  subroutine returns values from a string
public s2v             !  function returns doubleprecision value from string
public s2vs            !  function returns a doubleprecision array of numbers from a string
                       !  NOT USING INTERNAL READ FOR CONVERSION
public atoi            !   function returns an INTEGER(kind=int32) value from a string
public atol            !   function returns an INTEGER(kind=int64) value from a string
public aton            !   function returns true or false as to whether string converts to numeric value, and numeric value
                       !------------------------------------------------------------------------------------------------------------
public msg             !  function returns a string representing up to nine scalar intrinsic values
public value_to_string !  generic subroutine returns string given numeric REAL|DOUBLEPRECISION|INTEGER|LOGICAL value
public v2s             !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER|LOGICAL value
 private d2s           !  function returns string from doubleprecision value
 private r2s           !  function returns string from real value
 private i2s           !  function returns string from integer value
 private l2s           !  function returns string from logical value
public isnumber        !  determine if string represents a number
 private trimzeros_    !  Delete trailing zeros from numeric decimal string
public listout         !  expand a list of numbers where  negative numbers denote range ends (1 -10 means 1 thru 10)
!-----------------------------------------------------------------------------------------------------------------------------------
!
! extend intrinsics to accept CHARACTER values
!
public int, real, dble, nint

interface int;     module procedure atoi;              end interface
interface real;    module procedure real_s2v;          end interface
interface dble;    module procedure dble_s2v;          end interface
interface nint;    module procedure nint_s2v;          end interface

interface aton
module procedure ator_real32
module procedure ator_real64
module procedure atoi_int8
module procedure atoi_int16
module procedure atoi_int32
module procedure atoi_int64
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
!----------------------# BIT ROUTINES
public setbits8        !  use a string representing a positive binary value to fill the bits of an INTEGER value
public setbits16       !  use a string representing a positive binary value to fill the bits of an INTEGER value
public setbits32       !  use a string representing a positive binary value to fill the bits of an INTEGER value
public setbits64       !  use a string representing a positive binary value to fill the bits of an INTEGER value
!----------------------# BASE CONVERSION
public base            !  convert whole number string in base [2-36] to string in alternate base [2-36]
public codebase        !  convert whole number string in base [2-36] to base 10 number
public decodebase      !  convert whole number in base 10 to string in base [2-36]
public base2           !  convert INTEGER to a string representing a binary value
!----------------------# LOGICAL TESTS
public glob            !  compares given string for match to pattern which may contain wildcard characters
public ends_with       !  test whether strings ends with one of the specified suffix
public isalnum         !  elemental function returns .true. if CHR is a letter or digit
public isalpha         !  elemental function returns .true. if CHR is a letter and .false. otherwise
public isascii         !  elemental function returns .true. if the low order byte of c is in the range char(0) to char(127)
public isblank         !  elemental function returns .true. if CHR is a blank character (space or horizontal tab.
public iscntrl         !  elemental function returns .true. if CHR is a delete character or ordinary control character
public isdigit         !  elemental function returns .true. if CHR is a digit (0,1,...,9) and .false. otherwise
public isgraph         !  elemental function true if CHR is an ASCII printable character except considers a space non-printable
public islower         !  elemental function returns .true. if CHR is a miniscule letter (a-z)
public isprint         !  elemental function determines if CHR is an ASCII printable character
public ispunct         !  elemental function returns .true. if CHR is a printable punctuation character
public isspace         !  elemental function true if CHR is a null, space, tab, carriage return, new line, vertical tab, or formfeed
public isupper         !  elemental function returns .true. if CHR is an uppercase letter (A-Z)
public isxdigit        !  elemental function returns .true. if CHR is a hexadecimal digit (0-9, a-f, or A-F).
!----------------------#
!-------------------------------#
public fortran_name             !  elemental function returns .true. if LINE is a valid Fortran name
public describe                 !  returns a string describing character
public edit_distance            !  returns a naive edit distance using the Levenshtein distance algorithm
public bundle                   !  return up to twenty strings of arbitrary length as an array
public longest_common_substring !  function that returns the longest common substring of two strings.
!-------------------------------#

!-----------------------------------------------------------------------------------------------------------------------------------

! ident_2="@(#) M_strings switch(3f) toggle between string and array of characters; generic{a2s s2a}"

interface switch
   module procedure a2s, s2a
end interface switch
! note how returned result is "created" by the function
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_3="@(#) M_strings string_to_value(3f) Generic subroutine converts numeric string to a number (a2d a2r a2i)"

interface string_to_value
   module procedure a2d, a2r, a2i
end interface
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_4="@(#) M_strings v2s(3f) Generic function returns string given REAL|INTEGER|DOUBLEPRECISION value(d2s r2s i2s)"

interface v2s
   module procedure d2s, r2s, i2s, l2s
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
!-!interface setbits ! boz
!-!        module procedure setbits8, setbits16, setbits32, setbits64
!-!end interface
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_5="@(#) M_strings msg(3f) convert up to nine scalar values to a string. Alternatively can also handle one-dimensional arrays"

interface msg
   module procedure msg_scalar, msg_one
end interface msg
!-----------------------------------------------------------------------------------------------------------------------------------
! ASCII character constants
character, public, parameter :: ascii_nul = char(0)   ! null
character, public, parameter :: ascii_bel = char(7)   ! bell
character, public, parameter :: ascii_bs  = char(8)   ! backspace
character, public, parameter :: ascii_ht  = char(9)   ! horizontal tab
character, public, parameter :: ascii_lf  = char(10)  ! line feed or newline
character, public, parameter :: ascii_ff  = char(12)  ! form feed or newpage
character, public, parameter :: ascii_cr  = char(13)  ! carriage return
character, public, parameter :: ascii_esc = char(27)  ! escape
!-----------------------------------------------------------------------------------------------------------------------------------
interface ends_with
    procedure :: ends_with_str
    procedure :: ends_with_any
end interface ends_with
!-----------------------------------------------------------------------------------------------------------------------------------
public :: split2020, string_tokens
public :: find_field

interface split2020
   module procedure :: split_tokens, split_first_last, split_pos
end interface split2020
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
!This contains a conditionally built mini-version of M_journal which allows the M_strings.f90 module
!to be built using make as a stand-alone distribution but still have make.shell built a true version
!
!This is so when built with make.shell(1) or fpm(1) it will use the
!real M_journal.f90 file but that fpm(1) will not auto-discover the mini
!M_journal.f90 file and built it and cause duplicates.

interface journal
   module procedure flush_trail               ! journal()                ! no options
   module procedure write_message_only        ! journal(c)               ! must have one string
   module procedure where_write_message_all   ! journal(where,[g1-g9])   ! must have two strings
   module procedure set_stdout_lun            ! journal(i)               ! first is not a string
end interface journal

interface str
   module procedure str_scalar, str_one
end interface str

!$@(#) M_journal::journal(3fg): provides public message routine, no paging or graphic mode change

! global variables

integer,save,private       :: stdout=OUTPUT_UNIT
logical,save               :: debug=.false.
integer,save               :: last_int=0
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
! for compatibility allow old name for renamed procedures
interface matchw;  module procedure glob;    end interface
interface atleast; module procedure pad;     end interface
interface cc;      module procedure bundle;  end interface
public matchw          !  clone of glob -- for backward compatibiity
public atleast         !  clone of pad -- for backward compatibiity
public cc              !  clone of pad -- for backward compatibiity
!-----------------------------------------------------------------------------------------------------------------------------------
CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    glob(3f) - [M_strings:COMPARE] compare given string for match to
!!    a pattern which may contain globbing wildcard characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    logical function glob(string, pattern )
!!
!!     character(len=*),intent(in) :: string
!!     character(len=*),intent(in) :: pattern
!!
!!##DESCRIPTION
!!    glob(3f) compares given (entire) STRING for a match to PATTERN which may
!!    contain basic wildcard "globbing" characters.
!!
!!    In this version to get a match the entire string must be described
!!    by PATTERN. Trailing whitespace is significant, so trim the input
!!    string to have trailing whitespace ignored.
!!
!!    Patterns like "b*ba" fail on a string like "babababa" because the
!!    algorithm finds an early match. To skip over the early matches insert
!!    an extra character at the end of the string and pattern that does
!!    not occur in the pattern. Typically a NULL is used (char(0)).
!!
!!##OPTIONS
!!    string   the input string to test to see if it contains the pattern.
!!    pattern  the following simple globbing options are available
!!
!!             o "?" matching any one character
!!             o "*" matching zero or more characters.
!!               Do NOT use adjacent asterisks.
!!             o spaces are significant and must be matched or pretrimmed
!!             o There is no escape character, so matching strings with
!!               literal question mark and asterisk is problematic.
!!
!!##EXAMPLES
!!
!!   Example program
!!
!!    program demo_glob
!!    implicit none
!!    ! This main() routine passes a bunch of test strings
!!    ! into the above code.  In performance comparison mode,
!!    ! it does that over and over. Otherwise, it does it just
!!    ! once. Either way, it outputs a passed/failed result.
!!    !
!!    integer :: nReps
!!    logical :: allpassed
!!    integer :: i
!!    allpassed = .true.
!!
!!    nReps = 10000
!!    ! Can choose as many repetitions as you're expecting
!!    ! in the real world.
!!    nReps = 1
!!
!!    do i=1,nReps
!!       ! Cases with repeating character sequences.
!!       allpassed= test("a*abab",      "a*b",   .true.)  .and. allpassed
!!       allpassed= test("ab",          "*?",    .true.)  .and. allpassed
!!       allpassed= test("abc",         "*?",    .true.)  .and. allpassed
!!       allpassed= test("abcccd",      "*ccd",  .true.)  .and. allpassed
!!       allpassed= test("bLah",        "bLaH",  .false.) .and. allpassed
!!       allpassed= test("mississippi", "*sip*", .true.)  .and. allpassed
!!       allpassed= &
!!        & test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.) .and. allpassed
!!       allpassed= &
!!        & test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.) .and. allpassed
!!       allpassed= &
!!        & test("mississipissippi", "*issip*ss*", .true.) .and. allpassed
!!       allpassed= &
!!        & test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.) .and. allpassed
!!       allpassed= &
!!        & test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.) .and. allpassed
!!       allpassed= test("xyxyxyzyxyz", "xy*z*xyz", .true.)  .and. allpassed
!!       allpassed= test("xyxyxyxyz",   "xy*xyz",   .true.)  .and. allpassed
!!       allpassed= test("mississippi", "mi*sip*",  .true.)  .and. allpassed
!!       allpassed= test("ababac",      "*abac*",   .true.)  .and. allpassed
!!       allpassed= test("aaazz",       "a*zz*",    .true.)  .and. allpassed
!!       allpassed= test("a12b12",      "*12*23",   .false.) .and. allpassed
!!       allpassed= test("a12b12",      "a12b",     .false.) .and. allpassed
!!       allpassed= test("a12b12",      "*12*12*",  .true.)  .and. allpassed
!!
!!       ! Additional cases where the '*' char appears in the tame string.
!!       allpassed= test("*",     "*",      .true.)  .and. allpassed
!!       allpassed= test("a*r",   "a*",     .true.)  .and. allpassed
!!       allpassed= test("a*ar",  "a*aar",  .false.) .and. allpassed
!!
!!       ! More double wildcard scenarios.
!!       allpassed= test("XYXYXYZYXYz", "XY*Z*XYz",  .true.)  .and. allpassed
!!       allpassed= test("missisSIPpi", "*SIP*",     .true.)  .and. allpassed
!!       allpassed= test("mississipPI", "*issip*PI", .true.)  .and. allpassed
!!       allpassed= test("xyxyxyxyz",   "xy*xyz",    .true.)  .and. allpassed
!!       allpassed= test("miSsissippi", "mi*sip*",   .true.)  .and. allpassed
!!       allpassed= test("miSsissippi", "mi*Sip*",   .false.) .and. allpassed
!!       allpassed= test("abAbac",      "*Abac*",    .true.)  .and. allpassed
!!       allpassed= test("aAazz",       "a*zz*",     .true.)  .and. allpassed
!!       allpassed= test("A12b12",      "*12*23",    .false.) .and. allpassed
!!       allpassed= test("a12B12",      "*12*12*",   .true.)  .and. allpassed
!!       allpassed= test("oWn",         "*oWn*",     .true.)  .and. allpassed
!!
!!       ! Completely tame (no wildcards) cases.
!!       allpassed= test("bLah", "bLah", .true.) .and. allpassed
!!
!!       ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
!!       allpassed= test("a", "*?", .true.) .and. allpassed
!!
!!       ! More mixed wildcard tests including coverage for false positives.
!!       allpassed= test("a",      "??",         .false.) .and. allpassed
!!       allpassed= test("ab",     "?*?",        .true.)  .and. allpassed
!!       allpassed= test("ab",     "*?*?*",      .true.)  .and. allpassed
!!       allpassed= test("abc",    "?**?*?",     .true.)  .and. allpassed
!!       allpassed= test("abc",    "?**?*&?",    .false.) .and. allpassed
!!       allpassed= test("abcd",   "?b*??",      .true.)  .and. allpassed
!!       allpassed= test("abcd",   "?a*??",      .false.) .and. allpassed
!!       allpassed= test("abcd",   "?**?c?",     .true.)  .and. allpassed
!!       allpassed= test("abcd",   "?**?d?",     .false.) .and. allpassed
!!       allpassed= test("abcde",  "?*b*?*d*?",  .true.)  .and. allpassed
!!
!!       ! Single-character-match cases.
!!       allpassed= test("bLah",   "bL?h",  .true.)  .and. allpassed
!!       allpassed= test("bLaaa",  "bLa?",  .false.) .and. allpassed
!!       allpassed= test("bLah",   "bLa?",  .true.)  .and. allpassed
!!       allpassed= test("bLaH",   "?Lah",  .false.) .and. allpassed
!!       allpassed= test("bLaH",   "?LaH",  .true.)  .and. allpassed
!!
!!       allpassed= test('abcdefghijk' ,  '?b*',     .true.)  .and. allpassed
!!       allpassed= test('abcdefghijk' ,  '*c*',     .true.)  .and. allpassed
!!       allpassed= test('abcdefghijk' ,  '*c',      .false.) .and.  allpassed
!!       allpassed= test('abcdefghijk' ,  '*c*k',    .true.)  .and. allpassed
!!       allpassed= test('LS'          ,  '?OW',     .false.) .and.  allpassed
!!       allpassed= test('teztit'      ,  'tez*t*t', .true.)  .and. allpassed
!!         ! Two pattern match problems that might pose difficulties
!!       allpassed= test('e '           , '*e* ',      .true.) .and. allpassed
!!       allpassed= test('abcde       ' , '*e      *', .true.) .and. allpassed
!!       allpassed= test('bababa'       , 'b*ba',      .true.) .and. allpassed
!!       allpassed= test('baaaaax'      , 'b*ax',      .true.) .and. allpassed
!!       allpassed= test('baaaaa'       , 'b*ax',      .false.) .and. allpassed
!!       allpassed= test('baaaaax'      , 'b*a',       .false.) .and. allpassed
!!       allpassed= test(''             , 'b*',        .false.) .and. allpassed
!!       allpassed= test(''             , '*',         .true.) .and.  allpassed
!!       allpassed= test('b'            , '',          .false.) .and. allpassed
!!       allpassed= test('3'            , '??',        .false.) .and. allpassed
!!       ! known flaws
!!       allpassed= test(''             , '',          .true.) .and. allpassed
!!       allpassed= test('baaaaa'       , 'b*a',       .true.) .and. allpassed
!!       ! add unused character to work around
!!       allpassed= test(''//char(0),      ''//char(0),   .true.).and.allpassed
!!       allpassed= test('baaaaa'//char(0),'b*a'//char(0),.true.).and.allpassed
!!
!!       ! Many-wildcard scenarios.
!!       allpassed= test(&
!!       &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!!       &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
!!       &"a*a*a*a*a*a*aa*aaa*a*a*b",&
!!       &.true.) .and. allpassed
!!       allpassed= test(&
!!       &"abababababababababababababababababababaacacacacacacac&
!!       &adaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!       &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
!!       &.true.) .and. allpassed
!!       allpassed= test(&
!!       &"abababababababababababababababababababaacacacacacaca&
!!       &cadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!       &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
!!       &.false.) .and. allpassed
!!       allpassed= test(&
!!       &"abababababababababababababababababababaacacacacacacacad&
!!       &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!       &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
!!       &.false.) .and. allpassed
!!       allpassed= test(&
!!       &"abababababababababababababababababababaacacacacacacacad&
!!       &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!       &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
!!       &.true.) .and. allpassed
!!       allpassed= test("aaabbaabbaab","*aabbaa*a*",.true.).and.allpassed
!!       allpassed= &
!!       test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*",&
!!       &"a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.) .and. allpassed
!!       allpassed= test("aaaaaaaaaaaaaaaaa",&
!!       &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.) .and. allpassed
!!       allpassed= test("aaaaaaaaaaaaaaaa",&
!!       &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.) .and. allpassed
!!       allpassed= test(&
!!       &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!       &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!       & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc&
!!       &*abc*abc*abc*",&
!!       &.false.) .and. allpassed
!!       allpassed= test(&
!!       &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!       &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!       &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
!!       &.true.) .and. allpassed
!!       allpassed= test("abc*abcd*abcd*abc*abcd",&
!!       &"abc*abc*abc*abc*abc", .false.) .and. allpassed
!!       allpassed= test( "abc*abcd*abcd*abc*abcd*abcd&
!!       &*abc*abcd*abc*abc*abcd", &
!!       &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
!!       &.true.) .and. allpassed
!!       allpassed= test("abc",&
!!       &"********a********b********c********", .true.) .and. allpassed
!!       allpassed=&
!!       &test("********a********b********c********", "abc",.false.)&
!!       & .and.allpassed
!!       allpassed= &
!!       &test("abc", "********a********b********b********",.false.)&
!!       & .and.allpassed
!!       allpassed= test("*abc*", "***a*b*c***", .true.) .and. allpassed
!!
!!       ! A case-insensitive algorithm test.
!!       ! allpassed=test("mississippi", "*issip*PI", .true.) .and. allpassed
!!     enddo
!!
!!     if (allpassed)then
!!        write(*,'(*(g0,1x))')"Passed",nReps
!!     else
!!        write(*,'(a)')"Failed"
!!     endif
!!    contains
!!    ! This is a test program for wildcard matching routines.
!!    ! It can be used either to test a single routine for correctness,
!!    ! or to compare the timings of two (or more) different wildcard
!!    ! matching routines.
!!    !
!!    function test(tame, wild, bExpectedResult) result(bPassed)
!!    use M_strings, only : glob
!!       character(len=*) :: tame
!!       character(len=*) :: wild
!!       logical          :: bExpectedResult
!!       logical          :: bResult
!!       logical          :: bPassed
!!       bResult = .true.    ! We'll do "&=" cumulative checking.
!!       bPassed = .false.   ! Assume the worst.
!!       write(*,*)repeat('=',79)
!!       bResult = glob(tame, wild) ! Call a wildcard matching routine.
!!
!!       ! To assist correctness checking, output the two strings in any
!!       ! failing scenarios.
!!       if (bExpectedResult .eqv. bResult) then
!!          bPassed = .true.
!!          if(nReps == 1) write(*,*)"Passed match on ",tame," vs. ", wild
!!       else
!!          if(nReps == 1) write(*,*)"Failed match on ",tame," vs. ", wild
!!       endif
!!
!!    end function test
!!    end program demo_glob
!!
!!   Expected output
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   The article "Matching Wildcards: An Empirical Way to Tame an Algorithm"
!!   in Dr Dobb's Journal, By Kirk J. Krauss, October 07, 2014
!!
!!##LICENSE
!!   Public Domain
function glob(tame,wild)

! ident_6="@(#) M_strings glob(3f) function compares text strings one of which can have wildcards ('*' or '?')."

logical                    :: glob
character(len=*)           :: tame       ! A string without wildcards
character(len=*)           :: wild       ! A (potentially) corresponding string with wildcards
character(len=len(tame)+1) :: tametext
character(len=len(wild)+1) :: wildtext
character(len=1),parameter :: NULL=char(0)
integer                    :: wlen
integer                    :: ti, wi
integer                    :: i
character(len=:),allocatable :: tbookmark, wbookmark
! These two values are set when we observe a wildcard character. They
! represent the locations, in the two strings, from which we start once we have observed it.
   tametext=tame//NULL
   wildtext=wild//NULL
   tbookmark = NULL
   wbookmark = NULL
   wlen=len(wild)
   wi=1
   ti=1
   do                                            ! Walk the text strings one character at a time.
      if(wildtext(wi:wi) == '*')then             ! How do you match a unique text string?
         do i=wi,wlen                            ! Easy: unique up on it!
            if(wildtext(wi:wi) == '*')then
               wi=wi+1
            else
               exit
            endif
         enddo
         if(wildtext(wi:wi) == NULL) then        ! "x" matches "*"
            glob=.true.
            return
         endif
         if(wildtext(wi:wi)  /=  '?') then
            ! Fast-forward to next possible match.
            do while (tametext(ti:ti)  /=  wildtext(wi:wi))
               ti=ti+1
               if (tametext(ti:ti) == NULL)then
                  glob=.false.
                  return                         ! "x" doesn't match "*y*"
               endif
            enddo
         endif
         wbookmark = wildtext(wi:)
         tbookmark = tametext(ti:)
      elseif(tametext(ti:ti)  /=  wildtext(wi:wi) .and. wildtext(wi:wi)  /=  '?') then
         ! Got a non-match. If we've set our bookmarks, back up to one or both of them and retry.
         if(wbookmark /= NULL) then
            if(wildtext(wi:) /=  wbookmark) then
               wildtext = wbookmark
               wlen=len_trim(wbookmark)
               wi=1
               ! Don't go this far back again.
               if (tametext(ti:ti)  /=  wildtext(wi:wi)) then
                  tbookmark=tbookmark(2:)
                  tametext = tbookmark
                  ti=1
                  cycle                          ! "xy" matches "*y"
               else
                  wi=wi+1
               endif
            endif
            if (tametext(ti:ti) /= NULL) then
               ti=ti+1
               cycle                             ! "mississippi" matches "*sip*"
            endif
         endif
         glob=.false.
         return                                  ! "xy" doesn't match "x"
      endif
      ti=ti+1
      wi=wi+1
      if (ti > len(tametext)) then
         glob=.false.
         return
      elseif (tametext(ti:ti) == NULL) then          ! How do you match a tame text string?
         if(wildtext(wi:wi) /= NULL)then
            do while (wildtext(wi:wi) == '*')    ! The tame way: unique up on it!
               wi=wi+1                           ! "x" matches "x*"
               if(wildtext(wi:wi) == NULL)exit
            enddo
         endif
         if (wildtext(wi:wi) == NULL)then
            glob=.true.
            return                               ! "x" matches "x"
         endif
         glob=.false.
         return                                  ! "x" doesn't match "xy"
      endif
   enddo
end function glob
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    ends_with(3f) - [M_strings:COMPARE] test if string ends with specified
!!                    suffix(es)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function ends_with(source_string,suffix)
!!
!!     or
!!
!!    function ends_with(source_string,[suffix])
!!
!!     character(len=*),intent(in)          :: source_string
!!     character(len=*),intent(in)          :: suffix(..)
!!     logical                              :: ends_with
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!     SOURCE_STRING  string to tokenize
!!     SUFFIX         list of separator strings. May be scalar or an array.
!!                    Trailing spaces are ignored.
!!
!!##RETURNS
!!     ENDS_WITH      returns .TRUE. if one of the suffix match the end
!!                    of SOURCE_STRING.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_ends_with
!!    use M_strings, only : ends_with
!!    use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
!!    implicit none
!!       write(stdout,*)ends_with('prog.a',['.o','.i','.s'])
!!       write(stdout,*)ends_with('prog.f90',['.F90','.f90','.f  ','.F  '])
!!       write(stdout,*)ends_with('prog.pdf','.pdf')
!!       write(stdout,*)ends_with('prog.doc','.txt')
!!    end program demo_ends_with
!!
!!   Results:
!!
!!     F
!!     T
!!     T
!!     F
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function ends_with_str(string, ending) result(matched)
character(*), intent(in) :: string, ending
integer                  :: n1, n2
logical                  :: matched
   n1 = len(string) - len(ending) + 1
   n2 = len(string)
   if (n1 < 1) then
       matched = .false.
   else
       matched = (string(n1:n2) == ending)
   endif
end function ends_with_str
!-----------------------------------------------------------------------------------------------------------------------------------
pure function ends_with_any(string, endings) result(matched)
character(*), intent(in) :: string
character(*), intent(in) :: endings(:)
logical                  :: matched
integer                  :: i
   matched = .true.
   FINDIT: block
   do i=1, size(endings)
       if(ends_with_str(string,trim(endings(i)))) exit FINDIT
   enddo
   matched = .false.
   endblock FINDIT
end function ends_with_any
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sep(3f) - [M_strings:TOKENS] function to parse string into an array using
!!    specified delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function sep(input_line,delimiters,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: nulls
!!     character(len=:),allocatable             :: sep(:)
!!
!!##DESCRIPTION
!!     sep(3f) parses a string using specified delimiter characters and
!!     store tokens into an allocatable array
!!
!!##OPTIONS
!!    INPUT_LINE  Input string to tokenize
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    NULLS=IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!    ORDER='ASCENDING'|'DESCENDING'  by default the tokens are returned from
!!                                    last to first; order='ASCENDING' returns
!!                                    them from first to last (left to right).
!!##RETURNS
!!    SEP       Output array of tokens
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_sep
!!    use M_strings, only: sep
!!    character(len=*),parameter :: fo='(/,a,*(/,"[",g0,"]":,","))'
!!    character(len=*),parameter :: line=&
!!    '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!       write(*,'(a)') 'INPUT LINE:['//LINE//']'
!!       write(*,fo) 'typical call:',sep(line)
!!       write(*,fo) 'delimiters ":|":',sep(line,':|')
!!       write(*,fo) 'count null fields ":|":',sep(line,':|','return')
!!    end program demo_sep
!!
!!  Output
!!
!!    INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!
!!    typical call:
!!    [cc        ],
!!    [B         ],
!!    [a         ],
!!    [333|333   ],
!!    [1:|:2     ],
!!    [qrstuvwxyz],
!!    [ghijklmnop],
!!    [aBcdef    ]
!!
!!    delimiters ":|":
!!    [333 a B cc                         ],
!!    [2     333                          ],
!!    [  aBcdef   ghijklmnop qrstuvwxyz  1]
!!
!!    count null fields ":|":
!!    [333 a B cc                         ],
!!    [2     333                          ],
!!    [                                   ],
!!    [                                   ],
!!    [  aBcdef   ghijklmnop qrstuvwxyz  1]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function sep(input_line,delimiters,nulls,order)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_7="@(#) M_strings sep(3f) parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=*),optional,intent(in)     :: order       ! return strings composed of delimiters or not ignore|return|ignoreend

character(len=:),allocatable             :: sep(:)      ! output array of tokens
integer                                  :: isize
   call split(input_line,sep,delimiters,'right',nulls)
   if(present(order))then
   select case(order)
   case('ascending','ASCENDING')
    isize=size(sep)
    if(isize > 1)then
       sep=sep(isize:1:-1)
    endif
   end select
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sep
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split(3f) - [M_strings:TOKENS] parse string into an array using
!!    specified delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine split(input_line,array,delimiters,order,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=:),allocatable,intent(out) :: array(:)
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: order
!!     character(len=*),optional,intent(in)     :: nulls
!!
!!##DESCRIPTION
!!     SPLIT(3f) parses a string using specified delimiter characters and
!!     store tokens into an allocatable array
!!
!!##OPTIONS
!!    INPUT_LINE  Input string to tokenize
!!
!!    ARRAY       Output array of tokens
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    ORDER SEQUENTIAL|REVERSE|RIGHT  Order of output array.
!!                By default ARRAY contains the tokens having parsed
!!                the INPUT_LINE from left to right. If ORDER='RIGHT'
!!                or ORDER='REVERSE' the parsing goes from right to left.
!!                (This can be accomplished with array syntax in modern
!!                Fortran, but was more useful pre-fortran90).
!!
!!    NULLS=IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_split
!!    use M_strings, only: split
!!    implicit none
!!    integer :: i
!!    character(len=*),parameter     :: line=&
!!    '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!    character(len=:),allocatable :: array(:) ! output array of tokens
!!       write(*,*)'INPUT LINE:['//line//']'
!!       write(*,'(70("="))')
!!       write(*,*)'typical call:'
!!       call split(line,array)
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',size(array)
!!       write(*,'(70("-"))')
!!       write(*,*)'custom list of delimiters (colon and vertical line):'
!!       call split(line,array,delimiters=':|',&
!!       & order='sequential',nulls='ignore')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',size(array)
!!       write(*,'(70("-"))')
!!       write(*,*) 'custom list of delimiters, &
!!       &reverse array order and count null fields:'
!!       call split(line,array,delimiters=':|',&
!!       &order='reverse',nulls='return')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',size(array)
!!       write(*,'(70("-"))')
!!       write(*,*)'INPUT LINE:['//line//']'
!!       write(*,*) 'default delimiters and reverse array order &
!!       &and return null fields:'
!!       call split(line,array,delimiters='',order='reverse',nulls='return')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',size(array)
!!    end program demo_split
!!
!!  Output
!!
!!   >INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|
!!   333 a B cc    ]
!!   >=================================================================
!!   > typical call:
!!   >1 ==> aBcdef
!!   >2 ==> ghijklmnop
!!   >3 ==> qrstuvwxyz
!!   >4 ==> 1:|:2
!!   >5 ==> 333|333
!!   >6 ==> a
!!   >7 ==> B
!!   >8 ==> cc
!!   > SIZE:           8
!!   >----------------------------------------------------------------
!!   > custom list of delimiters (colon and vertical line):
!!   >1 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!   >2 ==> 2     333
!!   >3 ==> 333 a B cc
!!   > SIZE:           3
!!   >----------------------------------------------------------------
!!   > custom list of delimiters, reverse array order and
!!   return null fields:
!!   >1 ==> 333 a B cc
!!   >2 ==> 2     333
!!   >3 ==>
!!   >4 ==>
!!   >5 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!   > SIZE:           5
!!   >----------------------------------------------------------------
!!   > INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|
!!   333 a B cc    ]
!!   > default delimiters and reverse array order and count null fields:
!!   >1 ==>
!!   >2 ==>
!!   >3 ==>
!!   >4 ==> cc
!!   >5 ==> B
!!   >6 ==> a
!!   >7 ==> 333|333
!!   >8 ==>
!!   >9 ==>
!!   >10 ==>
!!   >11 ==>
!!   >12 ==> 1:|:2
!!   >13 ==>
!!   >14 ==> qrstuvwxyz
!!   >15 ==> ghijklmnop
!!   >16 ==>
!!   >17 ==>
!!   >18 ==> aBcdef
!!   >19 ==>
!!   >20 ==>
!!   > SIZE:          20
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_8="@(#) M_strings split(3f) parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: lgth                   ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters /= '')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)    !x! intel compiler says allocated already ?
   if(allocated(iterm))deallocate(iterm)      !x! intel compiler says allocated already ?
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   lgth=len(input_line)                                           ! lgth is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lgth > 0)then                                              ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,lgth,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)) == 0)then  ! if current character is not a delimiter
            iterm(i30)=lgth                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):lgth),dlim(i10:i10))
               IF(ifound > 0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol > lgth)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to return
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20) < ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    chomp(3f) - [M_strings:TOKENS] Tokenize a string, consuming it one
!!    token per call
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function chomp(source_string,token[,delimiters])
!!
!!     character(len=*)                     :: source_string
!!     character(len=:),intent(out)         :: token
!!     character(len=:),intent(in),optional :: delimiters
!!     integer                              :: chomp
!!
!!##DESCRIPTION
!!    The CHOMP(3f) function is used to isolate sequential tokens in a
!!    string, SOURCE_STRING. These tokens are delimited in the string by at
!!    least one of the characters in DELIMITERS. This routine consumes the
!!    source_string one token per call. It returns -1 when complete. The
!!    default delimiter list is "space,tab,carriage return,newline".
!!
!!##OPTIONS
!!     SOURCE_STRING  string to tokenize
!!     DELIMITERS     list of separator characters
!!
!!##RETURNS
!!     TOKEN          returned token
!!     CHOMP          status flag. 0 = success, -1 = no tokens remain
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_chomp
!!
!!    use M_strings, only : chomp
!!    implicit none
!!    character(len=100)            :: inline
!!    character(len=:),allocatable  :: token
!!    character(len=*),parameter    :: delimiters=' ;,'
!!    integer                       :: ios
!!    integer                       :: icount
!!    integer                       :: itoken
!!       icount=0
!!       do        ! read lines from stdin until end-of-file or error
!!          read (unit=*,fmt="(a)",iostat=ios) inline
!!          if(ios /= 0)stop
!!          icount=icount+1
!!          itoken=0
!!          write(*,*)'INLINE ',trim(inline)
!!          do while ( chomp(inline,token,delimiters) >=  0)
!!             itoken=itoken+1
!!             print *, itoken,'TOKEN=['//trim(token)//']'
!!          enddo
!!       enddo
!!
!!    end program demo_chomp
!!
!!   sample input file
!!
!!     this is a test of chomp; A:B :;,C;;
!!
!!   sample output file
!!
!!     > INLINE     this is a test of chomp; A:B :;,C;;
!!     >           1 TOKEN=[this]
!!     >           2 TOKEN=[is]
!!     >           3 TOKEN=[a]
!!     >           4 TOKEN=[test]
!!     >           5 TOKEN=[of]
!!     >           6 TOKEN=[chomp]
!!     >           7 TOKEN=[A:B]
!!     >           8 TOKEN=[:]
!!     >           9 TOKEN=[C]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
FUNCTION chomp(source_string,token,delimiters)

! ident_9="@(#) M_strings chomp(3f) Tokenize a string JSU- 20151030"

character(len=*)                         :: source_string    ! string to tokenize
character(len=:),allocatable,intent(out) :: token            ! returned token
character(len=*),intent(in),optional     :: delimiters       ! list of separator characters
integer                                  :: chomp            ! returns copy of shifted source_string
character(len=:),allocatable             :: delimiters_local
integer                                  :: token_start      ! beginning of token found if function result is .true.
integer                                  :: token_end        ! end of token found if function result is .true.
integer                                  :: isource_len
!-----------------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(present(delimiters))then
      delimiters_local=delimiters
   else                                          ! increment start to previous end + 1
      delimiters_local=char(32)//char(09)//char(10)//char(13) ! space,horizontal tab, newline, carriage return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   token_start=1
   do while (token_start  <=  isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_start:token_start))  /=  0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end  <=  isource_len-1)                         ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_end+1:token_end+1))  /=  0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
   !write(*,*)'TOKEN_START ',token_start
   !write(*,*)'TOKEN_END   ',token_end
   chomp=isource_len-token_end
   if(chomp >= 0)then
      token=source_string(token_start:token_end)
      source_string=source_string(token_end+1:)
   else
      token=''
      source_string=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function chomp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      delim(3f) - [M_strings:TOKENS] parse a string and store tokens into
!!      an array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine delim(line,array,n,icount,ibegin,iterm,lgth,dlim)
!!
!!     character(len=*),intent(in)  :: line
!!     integer,integer(in)          :: n
!!     integer,intent(out)          :: icount
!!     character(len=*)             :: array(n)
!!     integer,intent(out)          :: ibegin(n)
!!     integer,intent(out)          :: iterm(n)
!!     integer,intent(out)          :: lgth
!!     character(len=*)             :: dlim
!!
!!##DESCRIPTION
!!      Given a LINE of structure " par1 par2 par3 ... parn "
!!      store each par(n) into a separate variable in ARRAY (UNLESS
!!      ARRAY(1) == '#N#')
!!
!!      Also set ICOUNT to number of elements of array initialized, and
!!      return beginning and ending positions for each element in IBEGIN(N)
!!      and ITERM(N).
!!
!!      Return position of last non-blank character (even if more
!!      than N elements were found) in lgth
!!
!!      No quoting or escaping of delimiter is allowed, so the delimiter
!!      character can not be placed in a token.
!!
!!      No checking for more than N parameters; If any more they are ignored.
!!
!!##OPTIONS
!!    LINE      input string to parse into tokens
!!    ARRAY(N)  array that receives tokens
!!    N         size of arrays ARRAY, IBEGIN, ITERM
!!    ICOUNT    number of tokens found
!!    IBEGIN(N) starting columns of tokens found
!!    ITERM(N)  ending columns of tokens found
!!    LGTH      position of last non-blank character in input string LINE
!!    DLIM      delimiter characters
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!     program demo_delim
!!
!!     use M_strings, only: delim
!!     implicit none
!!     character(len=80) :: line
!!     character(len=80) :: dlm
!!     integer,parameter :: n=10
!!     character(len=20) :: array(n)=' '
!!     integer           :: ibegin(n),iterm(n)
!!     integer           :: i20, icount, lgth, i10
!!     line=' first  second 10.3 words_of_stuff  '
!!     do i20=1,4
!!        ! change delimiter list and what is calculated or parsed
!!        if(i20 == 1)dlm=' '
!!        if(i20 == 2)dlm='o'
!!        if(i20 == 3)dlm=' aeiou'    ! NOTE SPACE IS FIRST
!!        if(i20 == 3)ARRAY(1)='#N#'  ! QUIT RETURNING STRING ARRAY
!!        if(i20 == 4)line='AAAaBBBBBBbIIIIIi  J K L'
!!
!!        ! write out a break line composed of =========== ..
!!        write(*,'(57("="))')
!!        ! show line being parsed
!!        write(*,'(a)')'PARSING=['//trim(line)//'] on '//trim(dlm)
!!        ! call parsing procedure
!!        call delim(line,array,n,icount,ibegin,iterm,lgth,dlm)
!!        write(*,*)'number of tokens found=',icount
!!        write(*,*)'last character in column ',lgth
!!        if(icount > 0)then
!!           if(lgth /= iterm(icount))then
!!              write(*,*)'ignored from column ',iterm(icount)+1,' to ',lgth
!!           endif
!!           do i10=1,icount
!!              ! check flag to see if ARRAY() was set
!!              if(array(1) /= '#N#')then
!!                 ! from returned array
!!                 write(*,'(a,a,a)',advance='no')&
!!                 &'[',array(i10)(:iterm(i10)-ibegin(i10)+1),']'
!!              endif
!!           enddo
!!           ! using start and end positions in IBEGIN() and ITERM()
!!           write(*,*)
!!           do i10=1,icount
!!              ! from positions in original line
!!              write(*,'(a,a,a)',advance='no')&
!!              &'[',line(ibegin(i10):iterm(i10)),']'
!!           enddo
!!           write(*,*)
!!        endif
!!     enddo
!!     end program demo_delim
!!
!!   Results:
!!
!!    =========================================================
!!    PARSING=[ first  second 10.3 words_of_stuff] on
!!     number of tokens found=           4
!!     last character in column           34
!!    [first][second][10.3][words_of_stuff]
!!    [first][second][10.3][words_of_stuff]
!!    =========================================================
!!    PARSING=[ first  second 10.3 words_of_stuff] on o
!!     number of tokens found=           4
!!     last character in column           34
!!    [ first  sec][nd 10.3 w][rds_][f_stuff]
!!    [ first  sec][nd 10.3 w][rds_][f_stuff]
!!    =========================================================
!!    PARSING=[ first  second 10.3 words_of_stuff] on  aeiou
!!     number of tokens found=          10
!!     last character in column           34
!!
!!    [f][rst][s][c][nd][10.3][w][rds_][f_st][ff]
!!    =========================================================
!!    PARSING=[AAAaBBBBBBbIIIIIi  J K L] on  aeiou
!!     number of tokens found=           5
!!     last character in column           24
!!
!!    [AAA][BBBBBBbIIIII][J][K][L]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine delim(line,array,n,icount,ibegin,iterm,lgth,dlim)

! ident_10="@(#) M_strings delim(3f) parse a string and store tokens into an array"

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
integer,intent(out)            :: lgth
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
      lgth=len_trim(line)
      line_local=line

      idlim=len(dlim)
      if(idlim > 5)then
         idlim=len_trim(dlim)      ! dlim a lot of blanks on some machines if dlim is a big string
         if(idlim == 0)then
            idlim=1     ! blank string
         endif
      endif

      if(lgth == 0)then                                        ! command was totally blank
         return
      endif
!
!     there is at least one non-blank character in the command
!     lgth is the column position of the last non-blank character
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
               iend=lgth-istart+1+1                            ! assume no delimiters so put past end of line
               do i10=1,idlim
                  ifound=index(line_local(istart:lgth),dlim(i10:i10))
                  if(ifound > 0)then
                     iend=min(iend,ifound)
                  endif
               enddo
               if(iend <= 0)then                               ! no remaining delimiters
                 iterm(iarray)=lgth
                 if(lstore)then
                    array(iarray)=line_local(istart:lgth)
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
         if(icol > lgth)then
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
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_strings:EDITING] function replaces one
!!    substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!! syntax:
!!
!!      function replace(targetline,old,new,cmd,&
!!       & occurrence, &
!!       & repeat, &
!!       & ignorecase, &
!!       & ierr) result (newline)
!!      character(len=*)                       :: targetline
!!      character(len=*),intent(in),optional   :: old
!!      character(len=*),intent(in),optional   :: new
!!      character(len=*),intent(in),optional   :: cmd
!!      integer,intent(in),optional            :: occurrence
!!      integer,intent(in),optional            :: repeat
!!      logical,intent(in),optional            :: ignorecase
!!      integer,intent(out),optional           :: ierr
!!      character(len=:),allocatable           :: newline
!!
!!##DESCRIPTION
!!    Replace one substring for another in string.
!!    Either CMD or OLD and NEW must be specified.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     cmd         alternate way to specify old and new string, in
!!                 the form c/old/new/; where "/" can be any character
!!                 not in "old" or "new".
!!     occurrence  if present, start changing at the Nth occurrence of the
!!                 OLD string. If negative start replacing from the left
!!                 end of the string.
!!     repeat      number of replacements to perform. Defaults to a global
!!                 replacement.
!!     ignorecase  whether to ignore ASCII case or not. Defaults
!!                 to .false. .
!!##RETURNS
!!     newline     allocatable string returned
!!     ierr        error code. iF ier = -1 bad directive, >= 0 then
!!                 count of changes made.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_replace
!!    use M_strings, only : replace
!!    implicit none
!!    character(len=:),allocatable :: line
!!
!!    write(*,*)replace('Xis is Xe string','X','th')
!!    write(*,*)replace('Xis is xe string','x','th',ignorecase=.true.)
!!    write(*,*)replace('Xis is xe string','X','th',ignorecase=.false.)
!!
!!    ! a null old substring means "at beginning of line"
!!    write(*,*) replace('my line of text','','BEFORE:')
!!
!!    ! a null new string deletes occurrences of the old substring
!!    write(*,*) replace('I wonder i ii iii','i','')
!!
!!    ! Examples of the use of RANGE
!!
!!    line=replace('aaaaaaaaa','a','A',occurrence=1,repeat=1)
!!    write(*,*)'replace first a with A ['//line//']'
!!
!!    line=replace('aaaaaaaaa','a','A',occurrence=3,repeat=3)
!!    write(*,*)'replace a with A for 3rd to 5th occurrence ['//line//']'
!!
!!    line=replace('ababababa','a','',occurrence=3,repeat=3)
!!    write(*,*)'replace a with null instances 3 to 5 ['//line//']'
!!
!!    line=replace( &
!!     & 'a b ab baaa aaaa aa aa a a a aa aaaaaa',&
!!     & 'aa','CCCC',occurrence=-1,repeat=1)
!!    write(*,*)'replace lastaa with CCCC ['//line//']'
!!
!!    write(*,*)replace('myf90stuff.f90.f90','f90','for',occurrence=-1,repeat=1)
!!    write(*,*)replace('myf90stuff.f90.f90','f90','for',occurrence=-2,repeat=2)
!!
!!    end program demo_replace
!!
!!   Results:
!!
!!     this is the string
!!     this is the string
!!     this is xe string
!!     BEFORE:my line of text
!!     I wonder
!!     replace first a with A [Aaaaaaaaa]
!!     replace a with A for 3rd to 5th occurrence [aaAAAaaaa]
!!     replace a with null instances 3 to 5 [ababbb]
!!     replace lastaa with CCCC [a b ab baaa aaaa aa aa a a a aa aaaaCCCC]
!!     myf90stuff.f90.for
!!     myforstuff.for.f90
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine crack_cmd(cmd,old,new,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)              :: cmd
character(len=:),allocatable,intent(out) :: old,new                ! scratch string buffers
integer                                  :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=1)                         :: delimiters
integer                                  :: itoken
integer,parameter                        :: id=2                   ! expected location of delimiter
logical                                  :: ifok
integer                                  :: lmax                   ! length of target string
integer                                  :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   old=''
   new=''
   lmax=len_trim(cmd)                       ! significant length of change directive

   if(lmax >= 4)then                      ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)               ! find delimiter in expected location
      itoken=0                            ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id) == cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token  ==  (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*crack_cmd* incorrect change directive -too short')
   endif

end subroutine crack_cmd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function replace(targetline,old,new,cmd,occurrence,repeat,ignorecase,ierr) result (newline)

! ident_11="@(#) M_strings replace(3f) replace one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
! parameters
character(len=*),intent(in)            :: targetline   ! input line to be changed
character(len=*),intent(in),optional   :: old          ! old substring to replace
character(len=*),intent(in),optional   :: new          ! new substring
character(len=*),intent(in),optional   :: cmd          ! contains the instructions changing the string
integer,intent(in),optional            :: occurrence   ! Nth occurrence of OLD string to start replacement at
integer,intent(in),optional            :: repeat       ! how many replacements
logical,intent(in),optional            :: ignorecase
integer,intent(out),optional           :: ierr         ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
!-----------------------------------------------------------------------------------------------------------------------------------
! returns
character(len=:),allocatable  :: newline               ! output string buffer
!-----------------------------------------------------------------------------------------------------------------------------------
! local
character(len=:),allocatable  :: new_local, old_local, old_local_for_comparison
integer                       :: icount,ichange,ier2
integer                       :: original_input_length
integer                       :: len_old, len_new
integer                       :: ladd
integer                       :: left_margin, right_margin
integer                       :: ind
integer                       :: ic
integer                       :: ichr
integer                       :: range_local(2)
character(len=:),allocatable  :: targetline_for_comparison   ! input line to be changed
logical                       :: ignorecase_local
logical                       :: flip
character(len=:),allocatable  :: targetline_local   ! input line to be changed
!-----------------------------------------------------------------------------------------------------------------------------------
   flip=.false.
   ignorecase_local=.false.
   original_input_length=len_trim(targetline)          ! get non-blank length of input line

!  get old_local and new_local from cmd or old and new
   if(present(cmd))then
      call crack_cmd(cmd,old_local,new_local,ier2)
      if(ier2 /= 0)then
         newline=targetline  ! if no changes are made return original string on error
         if(present(ierr))ierr=ier2
         return
      endif
   elseif(present(old).and.present(new))then
      old_local=old
      new_local=new
   else
      newline=targetline  ! if no changes are made return original string on error
      call journal('sc','*replace* must specify OLD and NEW or CMD')
      return
   endif
   if(present(ignorecase))then
      ignorecase_local=ignorecase
   else
      ignorecase_local=.false.
   endif
   if(present(occurrence))then
      range_local(1)=abs(occurrence)
   else
      range_local(1)=1
   endif
   if(present(repeat))then
      range_local(2)=range_local(1)+repeat-1
   else
      range_local(2)=original_input_length
   endif
   if(ignorecase_local)then
      targetline_for_comparison=lower(targetline)
      old_local_for_comparison=lower(old_local)
   else
      targetline_for_comparison=targetline
      old_local_for_comparison=old_local
   endif
   if(present(occurrence))then
      if(occurrence < 0)then
         flip=.true.
         targetline_for_comparison=reverse(targetline_for_comparison)
         targetline_local=reverse(targetline)
         old_local_for_comparison=reverse(old_local_for_comparison)
         old_local=reverse(old_local)
         new_local=reverse(new_local)
      else
         targetline_local=targetline
      endif
   else
      targetline_local=targetline
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   len_old=len(old_local)                              ! length of old substring to be replaced
   len_new=len(new_local)                              ! length of new substring to replace old substring
   left_margin=1                                       ! left_margin is left margin of window to change
   right_margin=len(targetline)                        ! right_margin is right margin of window to change
   newline=''                                          ! begin with a blank line as output string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old == 0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichr=len_new + original_input_length
      if(len_new > 0)then
         newline=new_local(:len_new)//targetline_local(left_margin:original_input_length)
      else
         newline=targetline_local(left_margin:original_input_length)
      endif
      ichange=1                                        ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ichange
      if(flip) newline=reverse(newline)
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichr=left_margin                                    ! place to put characters into output string
   ic=left_margin                                      ! place looking at in input string
   loop: do
                                                       ! try finding start of OLD in remaining part of input in change window
      ind=index(targetline_for_comparison(ic:),old_local_for_comparison(:len_old))+ic-1
      if(ind == ic-1.or.ind > right_margin)then       ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      icount=icount+1                                  ! found an old string to change, so increment count of change candidates
      if(ind > ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         newline=newline(:ichr-1)//targetline_local(ic:ind-1)
         ichr=ichr+ladd
      endif
      if(icount >= range_local(1).and.icount <= range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new /= 0)then                                          ! put in new string
            newline=newline(:ichr-1)//new_local(:len_new)
            ichr=ichr+len_new
         endif
      else
         if(len_old /= 0)then                                          ! put in copy of old string
            newline=newline(:ichr-1)//old_local(:len_old)
            ichr=ichr+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ichange)
   case (0)                                            ! there were no changes made to the window
      newline=targetline_local                         ! if no changes made output should be input
   case default
      if(ic <= len(targetline))then                    ! if there is more after last change on original line add it
         newline=newline(:ichr-1)//targetline_local(ic:max(ic,original_input_length))
      endif
   end select
   if(present(ierr))ierr=ichange
   if(flip) newline=reverse(newline)
!-----------------------------------------------------------------------------------------------------------------------------------
end function replace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    substitute(3f) - [M_strings:EDITING] subroutine globally substitutes
!!    one substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine substitute(targetline,old,new,ierr,start,end)
!!
!!     character(len=*)              :: targetline
!!     character(len=*),intent(in)   :: old
!!     character(len=*),intent(in)   :: new
!!     integer,intent(out),optional  :: ierr
!!     integer,intent(in),optional   :: start
!!     integer,intent(in),optional   :: end
!!
!!##DESCRIPTION
!!    Globally substitute one substring for another in string.
!!
!!##OPTIONS
!!     TARGETLINE  input line to be changed. Must be long enough to
!!                 hold altered output.
!!     OLD         substring to find and replace
!!     NEW         replacement for OLD substring
!!     IERR        error code. If IER = -1 bad directive, >= 0 then
!!                 count of changes made.
!!     START       sets the left margin to be scanned for OLD in
!!                 TARGETLINE.
!!     END         sets the right margin to be scanned for OLD in
!!                 TARGETLINE.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_substitute
!!    use M_strings, only : substitute
!!    implicit none
!!    ! must be long enough to hold changed line
!!    character(len=80) :: targetline
!!
!!    targetline='this is the input string'
!!    write(*,*)'ORIGINAL    : '//trim(targetline)
!!
!!    ! changes the input to 'THis is THe input string'
!!    call substitute(targetline,'th','TH')
!!    write(*,*)'th => TH    : '//trim(targetline)
!!
!!    ! a null old substring means "at beginning of line"
!!    ! changes the input to 'BEFORE:this is the input string'
!!    call substitute(targetline,'','BEFORE:')
!!    write(*,*)'"" => BEFORE: '//trim(targetline)
!!
!!    ! a null new string deletes occurrences of the old substring
!!    ! changes the input to 'ths s the nput strng'
!!    call substitute(targetline,'i','')
!!    write(*,*)'i => ""     : '//trim(targetline)
!!
!!    end program demo_substitute
!!
!!   Expected output
!!
!!     ORIGINAL    : this is the input string
!!     th => TH    : THis is THe input string
!!     "" => BEFORE: BEFORE:THis is THe input string
!!     i => ""     : BEFORE:THs s THe nput strng
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine substitute(targetline,old,new,ierr,start,end)

! ident_12="@(#) M_strings substitute(3f) Globally substitute one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*)               :: targetline         ! input line to be changed
character(len=*),intent(in)    :: old                ! old substring to replace
character(len=*),intent(in)    :: new                ! new substring
integer,intent(out),optional   :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional    :: start              ! start sets the left margin
integer,intent(in),optional    :: end                ! end sets the right margin
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(targetline)) :: dum1               ! scratch string buffers
integer                        :: ml, mr, ier1
integer                        :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: ichr
!-----------------------------------------------------------------------------------------------------------------------------------
   if (present(start)) then                            ! optional starting column
      ml=start
   else
      ml=1
   endif
   if (present(end)) then                              ! optional ending column
      mr=end
   else
      mr=len(targetline)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier1=0                                              ! initialize error flag/change count
   maxlengthout=len(targetline)                        ! max length of output string
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   dum1(:)=' '                                         ! initialize string to build output in
   id=mr-ml                                            ! check for window option ! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id <= 0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old == 0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichr=len_new + original_input_length
      if(ichr > maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new > 0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1                                           ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ier1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichr=il                                            ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind == ic-1.or.ind > ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind > ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(ichr-1+ladd > maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichr:)=targetline(ic:ind-1)
         ichr=ichr+ladd
      endif
      if(ichr-1+len_new > maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new /= 0)then
         dum1(ichr:)=new(:len_new)
         ichr=ichr+len_new
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ier1)
   case (:-1)
      call journal('sc','*substitute* new line will be too long')
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(ichr+ladd > maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic < len(targetline))then
         dum1(ichr:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    change(3f) - [M_strings:EDITING] change old string to new string with
!!    a directive like a line editor
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine change(target_string,cmd,ierr)
!!
!!     character(len=*),intent(inout) :: target_string
!!     character(len=*),intent(in)    :: cmd
!!     integer                        :: ierr
!!
!!##DESCRIPTION
!!    change an old substring into a new substring in a character variable
!!    like a line editor. Primarily used to create interactive utilities
!!    such as input history editors for interactive line-mode programs. The
!!    output string is assumed long enough to accommodate the change.
!!    a directive resembles a line editor directive of the form
!!
!!       C/old_string/new_string/
!!
!!    where / may be any character which is not included in old_string
!!    or new_string.
!!
!!    a null old_string implies "beginning of string".
!!
!!##OPTIONS
!!    target_string  line to be changed
!!    cmd            contains instructions to change the string
!!    ierr           error code.
!!
!!       o =-1 bad directive
!!       o =0 no changes made
!!       o >0 count of changes made
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_change
!!
!!     use M_strings, only : change
!!     implicit none
!!     character(len=132) :: line='This is a test string to change'
!!     integer            :: ierr
!!        write(*,*)trim(line)
!!        ! change miniscule a to uppercase A
!!        call change(line,'c/a/A/',ierr)
!!        write(*,*)trim(line)
!!        ! put string at beginning of line
!!        call change(line,'c//prefix: /',ierr)
!!        write(*,*)trim(line)
!!        ! remove blanks
!!        call change(line,'c/ //',ierr)
!!        write(*,*)trim(line)
!!    end program demo_change
!!
!!   Expected output
!!
!!     This is a test string to change
!!     This is A test string to chAnge
!!     prefix: This is A test string to chAnge
!!     prefix:ThisisAteststringtochAnge
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine change(target_string,cmd,ierr)
! Change a string assumed long enough to accommodate the change, with a directive that resembles a line editor directive of the form
!    C/old_string/new_string/
! where / may be any character which is not included in old_string or new_string.
! a null old_string implies "beginning of string"
!===================================================================================================================================

! ident_13="@(#) M_strings change(3f) change a character string like a line editor"

character(len=*),intent(inout)   :: target_string          ! line to be changed
character(len=*),intent(in)      :: cmd                    ! contains the instructions changing the string
character(len=1)                 :: delimiters
integer                          :: ierr                   ! error code. ier=-1 bad directive;=0 no changes made;>0 ier changes made
integer                          :: itoken
integer,parameter                :: id=2                   ! expected location of delimiter
character(len=:),allocatable     :: old,new                ! scratch string buffers
logical                          :: ifok
integer                          :: lmax                   ! length of target string
integer                          :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   lmax=len_trim(cmd)                                                          ! significant length of change directive
   if(lmax >= 4)then                         ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)                                                    ! find delimiter in expected location
      itoken=0                                                                 ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id) == cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token  ==  (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif

      call substitute(target_string,old,new,ierr,1,len_trim(target_string))    ! change old substrings to new substrings
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*change* incorrect change directive -too short')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine change
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     strtok(3f) - [M_strings:TOKENS] Tokenize a string
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  function strtok(source_string,itoken,token_start,token_end,delimiters)
!!  result(strtok_status)
!!
!!   ! returned value
!!   logical                      :: strtok_status
!!   ! string to tokenize
!!   character(len=*),intent(in)  :: source_string
!!   ! token count since started
!!   integer,intent(inout)        :: itoken
!!   ! beginning of token
!!   integer,intent(out)          :: token_start
!!   ! end of token
!!   integer,intent(inout)        :: token_end
!!   ! list of separator characters
!!   character(len=*),intent(in)  :: delimiters
!!
!!##DESCRIPTION
!!     The STRTOK(3f) function is used to isolate sequential tokens in a
!!     string, SOURCE_STRING. These tokens are delimited in the string by
!!     at least one of the characters in DELIMITERS. The first time that
!!     STRTOK(3f) is called, ITOKEN should be specified as zero. Subsequent
!!     calls, wishing to obtain further tokens from the same string,
!!     should pass back in TOKEN_END  and ITOKEN until the function result
!!     returns .false.
!!
!!     This routine assumes no other calls are made to it using any other
!!     input string while it is processing an input line.
!!
!!##OPTIONS
!!     source_string  input string to parse
!!     itoken         token count should be set to zero for a new string
!!     delimiters     characters used to determine the end of tokens
!!
!!##RETURN
!!     token_start    beginning position in SOURCE_STRING where token was found
!!     token_end      ending position in SOURCE_STRING where token was found
!!     strtok_status
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_strtok
!!     use M_strings, only : strtok
!!     implicit none
!!     character(len=264)          :: inline
!!     character(len=*),parameter  :: delimiters=' ;,'
!!     integer                     :: ios, itoken, istart, iend
!!        do ! read lines from stdin until end-of-file or error
!!           read (unit=*,fmt="(a)",iostat=ios) inline
!!           if(ios /= 0)stop
!!           ! must set ITOKEN=0 before looping on strtok(3f)
!!           ! on a new string.
!!           itoken=0
!!           do while &
!!           &( strtok(inline,itoken,istart,iend,delimiters) )
!!              print *, itoken,&
!!              & 'TOKEN=['//(inline(istart:iend))//']',istart,iend
!!           enddo
!!        enddo
!!     end program demo_strtok
!!
!!     sample input file
!!
!!      this is a test of strtok; A:B :;,C;;
!!
!!     sample output file
!!
!!     1  TOKEN=[this]    2   5
!!     2  TOKEN=[is]      7   8
!!     3  TOKEN=[a]       10  10
!!     4  TOKEN=[test]    12  15
!!     5  TOKEN=[of]      17  18
!!     6  TOKEN=[strtok]  20  25
!!     7  TOKEN=[A:B]     28  30
!!     8  TOKEN=[:]       32  32
!!     9  TOKEN=[C]       35  35
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
FUNCTION strtok(source_string,itoken,token_start,token_end,delimiters) result(strtok_status)
! JSU- 20151030

! ident_14="@(#) M_strings strtok(3f) Tokenize a string"

character(len=*),intent(in)  :: source_string    ! Source string to tokenize.
character(len=*),intent(in)  :: delimiters       ! list of separator characters. May change between calls
integer,intent(inout)        :: itoken           ! token count since started
logical                      :: strtok_status    ! returned value
integer,intent(out)          :: token_start      ! beginning of token found if function result is .true.
integer,intent(inout)        :: token_end        ! end of token found if function result is .true.
integer,save                 :: isource_len
!----------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(itoken <= 0)then                           ! this is assumed to be the first call
      token_start=1
   else                                          ! increment start to previous end + 1
      token_start=token_end+1
   endif
!----------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!----------------------------------------------------------------------------------------------------------------------------
   if(token_start > isource_len)then            ! user input error or at end of string
      token_end=isource_len                      ! assume end of token is end of string until proven otherwise so it is set
      strtok_status=.false.
      return
   endif
!----------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   do while (token_start  <=  isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_start:token_start))  /=  0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end  <=  isource_len-1)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_end+1:token_end+1))  /=  0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   if (token_start  >  isource_len) then        ! determine if finished
      strtok_status=.false.                      ! flag that input string has been completely processed
   else
      itoken=itoken+1                            ! increment count of tokens found
      strtok_status=.true.                       ! flag more tokens may remain
   endif
!----------------------------------------------------------------------------------------------------------------------------
end function strtok
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    modif(3f) - [M_strings:EDITING] emulate the MODIFY command from the
!!    line editor XEDIT
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine modif(cline,cmod)
!!
!!     character(len=*) :: cline ! input string to change
!!     ! directive provides directions on changing string
!!     character(len=*) :: cmod
!!
!!##DESCRIPTION
!!   MODIF(3f) Modifies the line currently pointed at using a directive
!!   that acts much like a line editor directive.
!!   Primarily used to create interactive utilities such as input history
!!   editors for interactive line-mode programs.
!!
!!   the modify directives are as follows-
!!
!!    DIRECTIVE EXPLANATION
!!
!!    ^STRING#   Causes the string of characters between the ^ and the
!!               next # to be inserted before the characters pointed to
!!               by the ^. an ^ or & within the string is treated as a
!!               regular character. If the closing # is not specified,
!!               MODIF(3f) inserts the remainder of the line as if a # was
!!               specified after the last nonblank character.
!!
!!               There are two exceptions. the combination ^# causes a #
!!               to be inserted before the character pointed to by the
!!               ^, and an ^ as the last character of the directives
!!               causes a blank to be inserted.
!!
!!    #          (When not the first # after an ^) causes the character
!!               above it to be deleted.
!!
!!    &          Replaces the character above it with a space.
!!
!!    (SPACE)    A space below a character leaves it unchanged.
!!
!!    Any other character replaces the character above it.
!!
!!##EXAMPLES
!!
!!   Example input/output:
!!
!!    THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
!!    THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
!!    ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!!
!!   Sample program:
!!
!!    program demo_modif
!!    use M_strings, only : modif
!!    implicit none
!!    character(len=256)           :: line
!!    integer                      :: ios
!!    integer                      :: count
!!    integer                      :: COMMAND_LINE_LENGTH
!!    character(len=:),allocatable :: COMMAND_LINE
!!       ! get command name length
!!       call get_command_argument(0,length=count)
!!       ! get command line length
!!       call get_command(length=COMMAND_LINE_LENGTH)
!!       ! allocate string big enough to hold command line
!!       allocate(character(len=COMMAND_LINE_LENGTH+200) :: COMMAND_LINE)
!!       ! get command line as a string
!!       call get_command(command=COMMAND_LINE)
!!       ! trim leading spaces just in case
!!       COMMAND_LINE=adjustl(COMMAND_LINE)
!!       ! remove command name
!!       COMMAND_LINE=adjustl(COMMAND_LINE(COUNT+2:))
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios)line
!!          if(ios /= 0)exit
!!          call modif(line,COMMAND_LINE)
!!          write(*,'(a)')trim(line)
!!       enddo INFINITE
!!    end program demo_modif
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine modif(cline,mod)

!$@(#) M_strings::modif(3f): Emulate the MODIFY command from the line editor XEDIT

!
! MODIF
! =====
! ACTION- MODIFIES THE LINE CURRENTLY POINTED AT. THE INPUT STRING CLINE IS ASSUMED TO BE LONG ENOUGH TO ACCOMMODATE THE CHANGES
!         THE MODIFY DIRECTIVES ARE AS FOLLOWS-
!
!   DIRECTIVE                       EXPLANATION
!   ---------                       ------------
!   ^STRING#   CAUSES THE STRING OF CHARACTERS BETWEEN THE ^ AND THE
!              NEXT  # TO BE INSERTED BEFORE THE CHARACTERS POINTED TO
!              BY THE ^. AN ^ OR & WITHIN THE STRING IS TREATED AS A
!              REGULAR CHARACTER. IF THE CLOSING # IS NOT SPECIFIED,
!              MODIF(3f) INSERTS THE REMAINDER OFTHELINE AS IF A # WAS
!              SPECIFIED AFTER THE LAST NONBLANK CHARACTER.
!
!              THERE ARE TWO EXCEPTIONS. THE COMBINATION ^# CAUSES A #
!              TO BE INSERTED BEFORE THE CHARACTER POINTED TO BY THE
!              ^,  AND AN ^ AS THE LAST CHARACTER OF THE DIRECTIVES
!              CAUSES A BLANK TO BE INSERTED.
!
!   #          (WHEN NOT THE FIRST # AFTER AN ^) CAUSES THE CHARACTER
!              ABOVE IT TO BE DELETED.
!
!   &          REPLACES THE CHARACTER ABOVE IT WITH A SPACE.
!
!   (SPACE)    A SPACE BELOW A CHARACTER LEAVES IT UNCHANGED.
!
!   ANY OTHER CHARACTER REPLACES THE CHARACTER ABOVE IT.
!
! EXAMPLE-
! THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
! THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
! ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
character(len=*)            :: cline        !STRING TO BE MODIFIED
character(len=*),intent(in) :: mod          !STRING TO DIRECT MODIFICATION
character(len=len(cline))   :: cmod
character(len=3),parameter  :: c='#&^'      !ASSIGN DEFAULT EDIT CHARACTERS
integer                     :: maxscra      !LENGTH OF SCRATCH BUFFER
character(len=len(cline))   :: dum2         !SCRATCH CHARACTER BUFFER
logical                     :: linsrt       !FLAG FOR INSERTING DATA ON LINE
integer :: i, j, ic, ichr, iend, lmax, lmx1
maxscra=len(cline)
   cmod=trim(mod)
   lmax=min0(len(cline),maxscra)         !DETERMINE MAXIMUM LINE LENGTH
   lmx1=lmax-1                           !MAX LINE LENGTH -1
   dum2=' '                              !INITIALIZE NEW LINE
   linsrt=.false.                        !INITIALIZE INSERT MODE
   iend=len_trim(cmod)                   !DETERMINE END OF MODS
   i=0                                   !CHAR COUNTER FOR MOD LINE CMOD
   ic=0                                  !CHAR COUNTER FOR CURRENT LINE CLINE
   ichr=0                                !CHAR COUNTER NEW LINE DUM2
11 continue
   i=i+1                                 !NEXT CHAR IN MOD LINE
   if(ichr > lmx1)goto 999              !IF TOO MANY CHARS IN NEW LINE
   if(linsrt) then                       !IF INSERTING NEW CHARS
      if(i > iend) cmod(i:i)=c(1:1)     !FORCE END OF INSERT MODE
      if(cmod(i:i) == c(1:1))then        !IF END OF INSERT MODE
         linsrt=.false.                  !RESET INSERT MODE FLAG
         if(ic+1 == i)then               !NULL INSERT STRING
            ichr=ichr+1                  !INCREMENT COUNTER FOR NEW LINE
            dum2(ichr:ichr)=c(1:1)       !INSERT INSERT MODE TERMINATOR
         endif
         do j=ic,i                       !LOOP OF NUMBER OF CHARS INSERTED
            ichr=ichr+1                  !INCREMENT COUNTER FOR NEW LINE
            if(ichr > lmax)goto 999     !IF AT BUFFER LIMIT, QUIT
            dum2(ichr:ichr)=cline(j:j)   !APPEND CHARS FROM ORIG LINE
         enddo                           !...WHICH ALIGN WITH INSERTED CHARS
         ic=i                            !RESET CHAR COUNT TO END OF INSERT
         goto 1                          !CHECK NEW LINE LENGTH AND CYCLE
      endif                              !END OF TERMINATED INSERT LOGIC
      ichr=ichr+1                        !INCREMENT NEW LINE COUNT
      dum2(ichr:ichr)=cmod(i:i)          !SET NEWLINE CHAR TO INSERTED CHAR
   else                                  !IF NOT INSERTING CHARACTERS
      ic=ic+1                            !INCREMENT ORIGINAL LINE COUNTER
      if(cmod(i:i) == c(1:1))goto 1      !IF DELETE CHAR. NO COPY AND CYCLE
      if(cmod(i:i) == c(3:3))then        !IF BEGIN INSERT MODE
         linsrt=.true.                   !SET INSERT FLAG TRUE
         goto 1                          !CHECK LINE LENGTH AND CONTINUE
      endif                              !IF NOT BEGINNING INSERT MODE
      ichr=ichr+1                        !INCREMENT NEW LINE COUNTER
      if(cmod(i:i) == c(2:2))then        !IF REPLACE WITH BLANK
         dum2(ichr:ichr)=' '             !SET NEWLINE CHAR TO BLANK
         goto 1                          !CHECK LINE LENGTH AND CYCLE
      endif                              !IF NOT REPLACE WITH BLANK
      if(cmod(i:i) == ' ')then           !IF BLANK, KEEP ORIGINAL CHARACTER
         dum2(ichr:ichr)=cline(ic:ic)    !SET NEW CHAR TO ORIGINAL CHAR
      else                               !IF NOT KEEPING OLD CHAR
         dum2(ichr:ichr)=cmod(i:i)       !REPLACE ORIGINAL CHAR WITH NEW
      endif                              !END CHAR KEEP OR REPLACE
   endif                                 !END INSERT OR NO-INSERT
1  continue
   if(i < lmax)goto 11                  !CHECK FOR END OF LINE REACHED
                                         !AND CYCLE IF OK
999   continue
   cline=dum2                            !SET ORIGINAL CHARS TO NEW CHARS
end subroutine modif                     !RETURN
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      len_white(3f) - [M_strings:LENGTH] get length of string trimmed
!!      of whitespace.
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer function len_white(string)
!!
!!     character(len=*) :: string
!!
!!##DESCRIPTION
!!      len_white(3f) returns the position of the last character in
!!      string that is not a whitespace character. The Fortran90 intrinsic
!!      LEN_TRIM() should be used when trailing whitespace can be assumed
!!      to always be spaces.
!!
!!      This procedure was heavily used in the past because ANSI FORTRAN
!!      77 character objects are fixed length and blank padded and the
!!      LEN_TRIM() intrinsic did not exist. It should now be used only when
!!      whitespace characters other than blanks are likely.
!!
!!##OPTIONS
!!      string     input string whose trimmed length is being calculated
!!                 ignoring all trailing whitespace characters.
!!##RETURNS
!!      len_white  the number of characters in the trimmed string
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_len_white
!!
!!      use M_strings, only : len_white
!!      implicit none
!!      character(len=80) ::  s
!!      integer           :: lgth, lastnb
!!      intrinsic len
!!
!!      s=' ABCDEFG abcdefg '
!!      lgth = len(s)
!!      lastnb = len_white(s)
!!
!!      write(*,*) 'total length of variable is ',lgth
!!      write(*,*) 'trimmed length of variable is ',lastnb
!!      write(*,*) 'trimmed string=[',s(:lastnb),']'
!!
!!     end program demo_len_white
!!
!!   Results:
!!
!!     total length of variable is           80
!!     trimmed length of variable is           16
!!     trimmed string=[ ABCDEFG abcdefg]
!!
!!##NOTES
!!
!! o len_white
!!
!!      is a resource-intensive routine. Once the end of
!!      the string is found, it is probably best to keep track of it in
!!      order to avoid repeated calls to len_white. Because they
!!      might be more efficient, consider looking for vendor-supplied or
!!      system-optimized equivalents. For example:
!!
!!         o lnblnk - Solaris f77
!!         o len_trim - FORTRAN 90
!!
!! o Some compilers seem to have trouble passing a string of variable
!!   length properly. To be safe, use something like this:
!!
!!       subroutine message(s)
!!        character(len=*) :: s ! s is of variable length
!!           lgth=len(s)        ! get total length of variable
!!           ! explicitly specify a substring instead of just variable name
!!           lastnb = len_white(s(:lgth))
!!           write(*,*)'error:[',s(:lastnb),']'
!!       end subroutine messages
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental integer function len_white(string)
!  DEPRECATED. Use len_trim(3f),trim(3f) unless you might have trailing nulls (common when interacting with C procedures)"
!  John S. Urban, 1984, 1997-12-31
!  Note that if the string is blank, a length of 0 is returned; which is not a legal string length in Fortran77.
!  this routine used to return one instead of zero.
!   - mod 1:     1994
!                added null (char(0)) because HP and some Suns not padding
!                strings with blank, but with null characters; 1994 JSU
!   - mod 2:     1999
!                update syntax with INTENT(), ENDDO, no RETURN
!                still need instead of LEN_TRIM() because some systems stil pad CHARACTER with NULL
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_15="@(#) M_strings len_white(3f) return position of last non-blank/non-null character in string"

character(len=*),intent(in):: string ! input string to determine length of
integer                    :: i10
intrinsic len
   len_white=0
   do i10=len(string),1,-1
      select case(string(i10:i10))
      case(' ')                 ! space(32)
      case(char(0))             ! null(0)
      case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13)
      case default
         len_white=i10
         exit
      end select
   enddo
end function len_white
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    crop(3f) - [M_strings:WHITESPACE] trim leading and trailing blanks
!!               and control characters from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function crop(strin) result (strout)
!!
!!     character(len=*),intent(in)  :: strin
!!     character(len=:),allocatable :: strout
!!
!!##DESCRIPTION
!!    All control characters throughout the string are replaced with spaces
!!    and leading and trailing spaces are trimmed from the resulting string.
!!    Tabs are expanded assuming a stop every eight characters.
!!
!!##OPTIONS
!!    strin   input string to trim leading and trailing space and control
!!            characters from
!!
!!##RETURNS
!!    strout  cropped version of input string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_crop
!!    use M_strings, only: crop
!!    implicit none
!!    character(len=20) ::  untrimmed = '   ABCDEFG abcdefg  '
!!       write(*,*) 'untrimmed string=[',untrimmed,']'
!!       write(*,*) 'cropped string=[',crop(untrimmed),']'
!!    end program demo_crop
!!
!!   Expected output
!!
!!      untrimmed string=[   ABCDEFG abcdefg                      ]
!!      cropped string=[ABCDEFG abcdefg]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function crop(strin) result (strout)

! ident_16="@(#) M_strings crop(3f) replace control characters with whitespace and trim leading and trailings spaces from resulting string"

character(len=*),intent(in)  :: strin
character(len=:),allocatable :: strout
   strout=trim(adjustl(noesc(dilate(strin))))
end function crop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    clip(3f) - [M_strings:WHITESPACE] trim leading and trailing blanks from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function clip(strin) result (strout)
!!
!!     character(len=*),intent(in)  :: strin
!!     character(len=:),allocatable :: strout
!!
!!##DESCRIPTION
!!    leading and trailing spaces are trimmed from the resulting string.
!!
!!##OPTIONS
!!    strin   input string to trim leading and trailing space characters from
!!
!!##RETURNS
!!    strout  clipped version of input string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_clip
!!    use M_strings, only: clip
!!    implicit none
!!    character(len=20) ::  untrimmed = '   ABCDEFG abcdefg  '
!!       write(*,*) 'untrimmed string=[',untrimmed,']'
!!       write(*,*) 'clipped string=[',clip(untrimmed),']'
!!    end program demo_clip
!!
!!   Expected output
!!
!!      untrimmed string=[   ABCDEFG abcdefg                      ]
!!      clipped string=[ABCDEFG abcdefg]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function clip(string) result(lopped)

! ident_17="@(#) M_strings clip(3f) trim leading and trailings spaces from resulting string"

logical,parameter            :: T=.true.,F=.false.
character(len=*),intent(in)  :: string
character(len=:),allocatable :: lopped
integer                      :: ends(2)
   ends=verify( string, " ", [F,T] )
   if(ends(1) == 0)then
      lopped=""
   else
      lopped=string(ends(1):ends(2))
   endif
end function clip
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    transliterate(3f) - [M_strings:EDITING] replace characters from old
!!                        set with new set
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function transliterate(instr,old_set,new_set) result(outstr)
!!
!!     character(len=*),intent(in)  :: instr
!!     character(len=*),intent(in)  :: old_set
!!     character(len=*),intent(in)  :: new_set
!!     character(len=len(instr))    :: outstr
!!
!!##DESCRIPTION
!!    Translate, squeeze, and/or delete characters from the input string.
!!
!!##OPTIONS
!!    instr    input string to change
!!    old_set  list of letters to change in INSTR if found
!!
!!             Each character in the input string that matches a character
!!             in the old set is replaced.
!!
!!    new_set  list of letters to replace letters in OLD_SET with.
!!
!!             If the new_set is the empty set the matched characters
!!             are deleted.
!!
!!             If the new_set is shorter than the old set the last character
!!             in the new set is used to replace the remaining characters
!!             in the new set.
!!
!!##RETURNS
!!    outstr   instr with substitutions applied
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_transliterate
!!
!!     use M_strings, only : transliterate
!!     implicit none
!!     character(len=80)   :: STRING
!!
!!     STRING='aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ'
!!     write(*,'(a)') STRING
!!
!!     ! convert a string to uppercase:
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')
!!
!!     ! change all miniscule letters to a colon (":"):
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz',':')
!!
!!     ! delete all miniscule letters
!!     write(*,*) TRANSLITERATE(STRING, &
!!     & 'abcdefghijklmnopqrstuvwxyz','')
!!
!!    end program demo_transliterate
!!
!!    Expected output
!!
!!     > aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ
!!     > AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ
!!     > :A:B:C:D:E:F:G:H:I:J:K:L:M:N:O:P:Q:R:S:T:U:V:W:X:Y:Z
!!     > ABCDEFGHIJKLMNOPQRSTUVWXYZ
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
PURE FUNCTION transliterate(instr,old_set,new_set) RESULT(outstr)

! ident_18="@(#) M_strings transliterate(3f) replace characters from old set with new set"

!-----------------------------------------------------------------------------------------------------------------------------------
CHARACTER(LEN=*),INTENT(IN)  :: instr                             ! input string to change
CHARACTER(LEN=*),intent(in)  :: old_set
CHARACTER(LEN=*),intent(in)  :: new_set
!-----------------------------------------------------------------------------------------------------------------------------------
CHARACTER(LEN=LEN(instr))    :: outstr                            ! output string to generate
!-----------------------------------------------------------------------------------------------------------------------------------
INTEGER                      :: i10                               ! loop counter for stepping thru string
INTEGER                      :: ii,jj
!-----------------------------------------------------------------------------------------------------------------------------------
   jj=LEN(new_set)
   IF(jj /= 0)THEN
      outstr=instr                                                ! initially assume output string equals input string
      stepthru: DO i10 = 1, LEN(instr)
         ii=iNDEX(old_set,instr(i10:i10))                         ! see if current character is in old_set
         IF (ii /= 0)THEN
            if(ii <= jj)then                                      ! use corresponding character in new_set
               outstr(i10:i10) = new_set(ii:ii)
            else
               outstr(i10:i10) = new_set(jj:jj)                   ! new_set not as long as old_set; use last character in new_set
            endif
         ENDIF
      ENDDO stepthru
   else                                                           ! new_set is null string so delete characters in old_set
      outstr=' '
      hopthru: DO i10 = 1, LEN(instr)
         ii=iNDEX(old_set,instr(i10:i10))                         ! see if current character is in old_set
         IF (ii == 0)THEN                                         ! only keep characters not in old_set
            jj=jj+1
            outstr(jj:jj) = instr(i10:i10)
         ENDIF
      ENDDO hopthru
   endif
END FUNCTION transliterate
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    rotate13(3f) - [M_strings] apply trivial ROT13 encryption to a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    rotate13(input) result(output)
!!
!!     character(len=*),intent(in) :: input
!!     character(len=len(input))   :: output
!!
!!##DESCRIPTION
!!    ROT13 ("rotate by 13 places", sometimes hyphenated ROT-13) is a simple
!!    letter substitution cipher that replaces a letter with the 13th letter
!!    after it in the alphabet; wrapping around if necessary.
!!
!!    The transformation can be done using a lookup table, such as the
!!    following:
!!
!!       Input  ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
!!       Output NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm
!!
!!    ROT13 is used in online forums as a means of hiding spoilers,
!!    punchlines, puzzle solutions, and offensive materials from the casual
!!    glance. ROT13 has inspired a variety of letter and word games on-line,
!!    and is frequently mentioned in newsgroup conversations.
!!
!!    The algorithm provides virtually no cryptographic security, and is
!!    often cited as a canonical example of weak encryption.
!!
!!    ROT13 is a special case of the Caesar cipher which was developed in
!!    ancient Rome.
!!
!!    ALGORITHM
!!
!!    Applying ROT13 to a piece of text merely requires examining its
!!    alphabetic characters and replacing each one by the letter 13 places
!!    further along in the alphabet, wrapping back to the beginning if
!!    necessary. A becomes N, B becomes O, and so on up to M, which becomes
!!    Z, then the sequence continues at the beginning of the alphabet: N
!!    becomes A, O becomes B, and so on to Z, which becomes M. Only those
!!    letters which occur in the English alphabet are affected; numbers,
!!    symbols, whitespace, and all other characters are left unchanged.
!!
!!    SAME ALGORITHM FOR ENCODING AND DECODING
!!
!!    Because there are 26 letters in the English alphabet and 26 = 2 x 13,
!!    the ROT13 function is its own inverse: so the same action can be used
!!    for encoding and decoding. In other words, two successive applications
!!    of ROT13 restore the original text (in mathematics, this is sometimes
!!    called an involution; in cryptography, a reciprocal cipher).
!!
!!    TRIVIAL SECURITY
!!
!!    The use of a constant shift means that the encryption effectively
!!    has no key, and decryption requires no more knowledge than the fact
!!    that ROT13 is in use. Even without this knowledge, the algorithm is
!!    easily broken through frequency analysis.
!!
!!    In encrypted normal English-language text of any significant size,
!!    ROT13 is recognizable from some letter/word patterns. The words "n",
!!    "V" (capitalized only), and "gur" (ROT13 for "a", "I", and "the"),
!!    and words ending in "yl" ("ly") are examples.
!!
!!##REFERENCES
!!    Wikipedia, the free encyclopedia
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_rotate13
!!    use M_strings, only : rotate13
!!    implicit none
!!    character(len=256) :: line
!!    integer            :: ios
!!    do
!!       read(*,'(a)',iostat=ios)line
!!       if(ios /= 0)exit
!!       write(*,'(a)')rotate13(line)
!!    enddo
!!    end program demo_rotate13
!!
!!  Sample usage:
!!
!!    demo_rotate13
!!    United we stand, divided we fall.
!!    Havgrq jr fgnaq, qvivqrq jr snyy.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function rotate13 (input)

! ident_19="@(#) M_strings rotate13(3f) converts a character to its ROT13 equivalent which is a trivial encryption."

character(len=*),intent(in) :: input
character(len=len(input))   :: rotate13
integer                     :: itemp
integer                     :: i
   rotate13=' '
   do i=1,len_trim(input)
      itemp = iachar(input(i:i))
      select case(itemp)
       case(65:77,97:109)
         itemp = itemp + 13
       case(78:90,110:122)
         itemp = itemp - 13
      end select
      rotate13(i:i) = char ( itemp )
   enddo

end function rotate13
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    join(3f) - [M_strings:EDITING] append CHARACTER variable array into
!!    a single CHARACTER variable with specified separator
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function join(str,sep,trm,left,right,start,end) result (string)
!!
!!     character(len=*),intent(in)          :: str(:)
!!     character(len=*),intent(in),optional :: sep
!!     logical,intent(in),optional          :: trm
!!     character(len=*),intent(in),optional :: right
!!     character(len=*),intent(in),optional :: left
!!     character(len=*),intent(in),optional :: start
!!     character(len=*),intent(in),optional :: end
!!     character(len=:),allocatable         :: string
!!
!!##DESCRIPTION
!!   JOIN(3f) appends the elements of a CHARACTER array into a single
!!   CHARACTER variable, with elements 1 to N joined from left to right.
!!   By default each element is trimmed of trailing spaces and the
!!   default separator is a null string.
!!
!!##OPTIONS
!!      STR(:)  array of CHARACTER variables to be joined
!!      SEP     separator string to place between each variable. defaults
!!              to a null string.
!!      LEFT    string to place at left of each element
!!      RIGHT   string to place at right of each element
!!      START   prefix string
!!      END     suffix string
!!      TRM     option to trim each element of STR of trailing
!!              spaces. Defaults to .TRUE.
!!
!!##RESULT
!!      STRING  CHARACTER variable composed of all of the elements of STR()
!!              appended together with the optional separator SEP placed
!!              between the elements.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!   program demo_join
!!   use M_strings, only: join
!!   implicit none
!!   character(len=:),allocatable  :: s(:)
!!   character(len=:),allocatable  :: out
!!   integer                       :: i
!!     s=[character(len=10) :: 'United',' we',' stand,', &
!!     & ' divided',' we fall.']
!!     out=join(s)
!!     write(*,'(a)') out
!!     write(*,'(a)') join(s,trm=.false.)
!!     write(*,'(a)') (join(s,trm=.false.,sep='|'),i=1,3)
!!     write(*,'(a)') join(s,sep='<>')
!!     write(*,'(a)') join(s,sep=';',left='[',right=']')
!!     write(*,'(a)') join(s,left='[',right=']')
!!     write(*,'(a)') join(s,left='>>')
!!   end program demo_join
!!
!!  Expected output:
!!
!!   United we stand, divided we fall.
!!   United     we        stand,    divided   we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United<> we<> stand,<> divided<> we fall.
!!   [United];[ we];[ stand,];[ divided];[ we fall.]
!!   [United][ we][ stand,][ divided][ we fall.]
!!   >>United>> we>> stand,>> divided>> we fall.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function join(str,sep,trm,left,right,start,end) result (string)

! ident_20="@(#) M_strings join(3f) merge string array into a single CHARACTER value adding specified separators caps prefix and suffix"

character(len=*),intent(in)          :: str(:)
character(len=*),intent(in),optional :: sep, right, left, start, end
logical,intent(in),optional          :: trm
character(len=:),allocatable         :: sep_local, left_local, right_local
character(len=:),allocatable         :: string
logical                              :: trm_local
integer                              :: i
   if(present(sep))then   ; sep_local=sep     ; else ; sep_local=''     ; endif
   if(present(trm))then   ; trm_local=trm     ; else ; trm_local=.true. ; endif
   if(present(left))then  ; left_local=left   ; else ; left_local=''    ; endif
   if(present(right))then ; right_local=right ; else ; right_local=''   ; endif
   string=''
   if(size(str) == 0)then
      string=string//left_local//right_local
   else
      do i = 1,size(str)-1
         if(trm_local)then
            string=string//left_local//trim(str(i))//right_local//sep_local
         else
            string=string//left_local//str(i)//right_local//sep_local
         endif
      enddo
      if(trm_local)then
         string=string//left_local//trim(str(i))//right_local
      else
         string=string//left_local//str(i)//right_local
      endif
   endif
   if(present(start))string=start//string
   if(present(end))string=string//end
end function join
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      reverse(3f) - [M_strings:EDITING] Return a string reversed
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function reverse(str) result (string)
!!
!!     character(*), intent(in) :: str
!!     character(len(str))      :: string
!!
!!##DESCRIPTION
!!      reverse(string) returns a copy of the input string with
!!      all characters reversed from right to left.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_reverse
!!       use M_strings, only: reverse
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          write(*,*)'REVERSE STRINGS:',reverse('Madam, I''m Adam')
!!          s='abcdefghijklmnopqrstuvwxyz'
!!          write(*,*) 'original input string is ....',s
!!          write(*,*) 'reversed output string is ...',reverse(s)
!!       end program demo_reverse
!!
!!  Results:
!!
!!      >  REVERSE STRINGS:madA m'I ,madaM
!!      >  original input string is ....abcdefghijklmnopqrstuvwxyz
!!      >  reversed output string is ...zyxwvutsrqponmlkjihgfedcba
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function reverse(string) result (rev)

! ident_21="@(#) M_strings reverse(3f) Return a string reversed"

character(len=*),intent(in)    :: string   ! string to reverse
character(len=len(string))     :: rev      ! return value (reversed string)
integer                        :: length
integer                        :: i
   length = len(string)

   do i = 1,length
      rev(i:i)=string(length-i+1:length-i+1)
   enddo
end function reverse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! upper_quoted(3f) - [M_strings:CASE] elemental function converts string to
!!                miniscule skipping strings quoted per Fortran syntax rules
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function upper_quoted(str) result (string)
!!
!!     character(*), intent(in)    :: str
!!     character(len(str))         :: string  ! output string
!!
!!##DESCRIPTION
!!    upper_quoted(string) returns a copy of the input string with all not-quoted
!!    characters converted to uppercase, assuming ASCII character sets
!!    are being used. The quoting rules are the same as for Fortran source.
!!    Either a single or double quote starts a quoted string, and a quote
!!    character of the same type is doubled when it appears internally in
!!    the quoted string. If a double quote quotes the string single quotes
!!    may appear in the quoted string as single characters, and vice-versa
!!    for single quotes.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all unquoted characters converted
!!           to uppercase
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_upper_quoted
!!     use M_strings, only: upper_quoted
!!     implicit none
!!     character(len=:),allocatable  :: s
!!     s=' ABCDEFG abcdefg "Double-Quoted" ''Single-Quoted'' "with ""&
!!        & Quote" everything else'
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper_quoted(s)
!!        write(*,'(1x,a,*(a:,"+"))') 'upper_quoted(3f) is elemental ==>', &
!!        & upper_quoted(["abc","def","ghi"])
!!     end program demo_upper_quoted
!!
!!    Expected output:
!!
!!     mixed-case input string is .... ABCDEFG abcdefg "Double-Quoted"
!!     'Single-Quoted' "with "" Quote" everything else
!!     upper-case output string is ... ABCDEFG ABCDEFG "Double-Quoted"
!!     'Single-Quoted' "with "" Quote" EVERYTHING ELSE
!!     upper_quoted(3f) is elemental ==>ABC+DEF+GHI
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental pure function upper_quoted(str) result (string)

! ident_22="@(#) M_strings upper_quoted(3f) elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules"

character(len=*), intent(in)   :: str     ! The input string
character(len=len(str))        :: string  ! The output string
logical                        :: toggle
character(len=1)               :: togglechar
integer                        :: irnk
integer                        :: i
character(len=26), parameter   :: large="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
character(len=26), parameter   :: small="abcdefghijklmnopqrstuvwxyz"

   string=str
   toggle = .TRUE.
   do i = 1, len_trim(string)
      if(toggle) then
         if(string(i:i) == '"' .or. string(i:i) == "'") then
            toggle = .not. toggle
            togglechar = string(i:i)
         endif
         irnk = index(small, string(i:i))
         if(irnk > 0) then
            string(i:i) = large(irnk:irnk)
         endif
      else
         if(string(i:i) == togglechar) toggle = .not. toggle
      endif
   enddo
end function upper_quoted
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! upper(3f) - [M_strings:CASE] changes a string to uppercase
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function upper(str,begin,end) result (string)
!!
!!     character(*), intent(in)    :: str
!!     integer,optional,intent(in) :: begin,end
!!     character(len(str))         :: string  ! output string
!!
!!##DESCRIPTION
!!      upper(string) returns a copy of the input string with all characters
!!      converted in the optionally specified range to uppercase, assuming
!!      ASCII character sets are being used. If no range is specified the
!!      entire string is converted to uppercase.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!    begin  optional starting position in "str" to begin converting to
!!           uppercase
!!    end    optional ending position in "str" to stop converting to
!!           uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all characters converted to
!!           uppercase over optionally specified range.
!!
!!##TRIVIA
!!    The terms "uppercase" and "lowercase" date back to the early days of
!!    the mechanical printing press. Individual metal alloy casts of each
!!    needed letter, or punctuation symbol, were meticulously added to a
!!    press block, by hand, before rolling out copies of a page. These
!!    metal casts were stored and organized in wooden cases. The more
!!    often needed miniscule letters were placed closer to hand, in the
!!    lower cases of the work bench. The less often needed, capitalized,
!!    majuscule letters, ended up in the harder to reach upper cases.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_upper
!!     use M_strings, only: upper
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s=' ABCDEFG abcdefg '
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper(s)
!!        write(*,*) 'make first character uppercase  ... ',&
!!        & upper('this is a sentence.',1,1)
!!        write(*,'(1x,a,*(a:,"+"))') 'UPPER(3f) is elemental ==>',&
!!        & upper(["abc","def","ghi"])
!!     end program demo_upper
!!
!!    Expected output
!!
!!     mixed-case input string is .... ABCDEFG abcdefg
!!     upper-case output string is ... ABCDEFG ABCDEFG
!!     make first character uppercase  ... This is a sentence.
!!     UPPER(3f) is elemental ==>ABC+DEF+GHI
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
! Timing
!
!    Several different methods have been proposed for changing case.
!    A simple program that copies a large file and converts it to
!    uppercase was timed and compared to a simple copy. This was used
!    to select the default function.
!
! NULL:    83.41user  9.25system 1:37.94elapsed 94%CPU
! upper:  101.44user 10.89system 1:58.36elapsed 94%CPU
! upper2: 105.04user 10.69system 2:04.17elapsed 93%CPU
! upper3: 267.21user 11.69system 4:49.21elapsed 96%CPU
elemental pure function upper(str,begin,end) result (string)

! ident_23="@(#) M_strings upper(3f) returns a trimmed uppercase string"

character(*), intent(in)      :: str                 ! input string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str                                      ! initialize output string to input string
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(ibegin,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                    ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                 ! located miniscule letter
          string(i:i) = achar(iachar(str(i:i))+diff)  ! change miniscule letter to majascule
       end select
   enddo

end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lower(3f) - [M_strings:CASE] changes a string to lowercase over
!!    specified range
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function lower(str,begin,end) result (string)
!!
!!     character(*), intent(in) :: str
!!     integer,optional         :: begin, end
!!     character(len(str))      :: string  ! output string
!!
!!##DESCRIPTION
!!      lower(string) returns a copy of the input string with all characters
!!      converted to miniscule over the specified range, assuming ASCII
!!      character sets are being used. If no range is specified the entire
!!      string is converted to miniscule.
!!
!!##OPTIONS
!!    str    string to convert to miniscule
!!    begin  optional starting position in "str" to begin converting to
!!           miniscule
!!    end    optional ending position in "str" to stop converting to
!!           miniscule
!!
!!##RESULTS
!!    lower  copy of the input string with all characters converted to
!!           miniscule over optionally specified range.
!!
!!##TRIVIA
!!    The terms "uppercase" and "lowercase" date back to the early days of
!!    the mechanical printing press. Individual metal alloy casts of each
!!    needed letter, or punctuation symbol, were meticulously added to a
!!    press block, by hand, before rolling out copies of a page. These
!!    metal casts were stored and organized in wooden cases. The more
!!    often needed miniscule letters were placed closer to hand, in the
!!    lower cases of the work bench. The less often needed, capitalized,
!!    majuscule letters, ended up in the harder to reach upper cases.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_lower
!!       use M_strings, only: lower
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          s=' ABCDEFG abcdefg '
!!          write(*,*) 'mixed-case input string is ....',s
!!          write(*,*) 'lower-case output string is ...',lower(s)
!!       end program demo_lower
!!
!!    Expected output
!!
!!       mixed-case input string is .... ABCDEFG abcdefg
!!       lower-case output string is ... abcdefg abcdefg
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental pure function lower(str,begin,end) result (string)

! ident_24="@(#) M_strings lower(3f) Changes a string to lowercase over specified range"

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
         string(i:i) = achar(iachar(str(i:i))-diff)   ! change letter to miniscule
      case default
      end select
   enddo

end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    switch(3f) - [M_strings:ARRAY] converts between CHARACTER scalar and
!!    array of single characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function switch(array) result (string)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!      or
!!
!!    pure function switch(string) result (array)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!##DESCRIPTION
!!    SWITCH(3f): generic function that switches CHARACTER string to an array
!!    of single characters or an array of single characters to a CHARACTER
!!    string. Useful in passing strings to C. New Fortran features may
!!    supersede these routines.
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_switch
!!    use M_strings, only : switch, isalpha, islower, nospace
!!    character(len=*),parameter :: &
!!    & dashes='-----------------------------------'
!!    character(len=*),parameter :: string='This is a string'
!!    character(len=1024)        :: line
!!
!!    ! First, examples of standard Fortran features
!!    ! returns array [F,T,T,T,T,T]
!!    write(*,*)['A','=','=','=','=','='] == '='
!!    ! this would return T
!!    write(*,*)all(['=','=','=','=','=','='] == '=')
!!    ! this would return F
!!    write(*,*)all(['A','=','=','=','=','='] == '=')
!!
!!    ! so to test if the string DASHES is all dashes
!!    ! using SWITCH(3f) is
!!    if(all(switch(dashes) == '-'))then
!!       write(*,*)'DASHES is all dashes'
!!    endif
!!
!!    ! so to test is a string is all letters
!!    ! isalpha(3f) returns .true. only if character is a letter
!!    ! false because dashes are not a letter
!!    write(*,*) all(isalpha(switch(dashes)))
!!    ! false because of spaces
!!    write(*,*) all(isalpha(switch(string)))
!!    ! true because removed whitespace
!!    write(*,*) all(isalpha(switch(nospace(string))))
!!
!!    ! to see if a string is all uppercase
!!    ! show the string
!!    write(*,*) string
!!    ! converted to character array
!!    write(*,'(1x,*("[",a,"]":))') switch(string)
!!    write(*,'(*(l3))') islower(switch(string))
!!
!!    ! we need a string that is all letters
!!    line=nospace(string)
!!    write(*,*)'LINE=',trim(line)
!!    ! all true except first character
!!    write(*,*) islower(switch(nospace(string)))
!!    ! should be false
!!    write(*,*) all(islower(switch(nospace(string))))
!!    ! should be true
!!    write(*,*) all(islower(switch(nospace(string(2:)))))
!!
!!    end program demo_switch
!!
!!  Expected output
!!
!!     F T T T T T
!!     T
!!     F
!!     DASHES is all dashes
!!     F
!!     F
!!     T
!!     This is a string
!!     [T][h][i][s][ ][i][s][ ][a][ ][s][t][r][i][n][g]
!!      F  T  T  T  F  T  T  F  T  F  T  T  T  T  T  T
!!     LINE=Thisisastring
!!     F T T T T T T T T T T T T
!!     F
!!     T
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function a2s(array)  result (string)

! ident_25="@(#) M_strings a2s(3fp) function to copy char array to string"

character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall( i = 1:size(array)) string(i:i) = array(i)
! ----------------------------------------------------------------------------------------------------------------------------------
!  string=transfer(array,string)
end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

! ident_26="@(#) M_strings s2a(3fp) function to copy string(1 Clen(string)) to char array"

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall(i=1:len(string)) array(i) = string(i:i)
! ----------------------------------------------------------------------------------------------------------------------------------
!  array=transfer(string,array)
end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2c(3f) - [M_strings:ARRAY] convert character variable to array of
!!      characters with last element set to null
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function s2c(string)
!!
!!     character(len=*),intent=(in)  :: string
!!     character(len=1),allocatable  :: s2c(:)
!!
!!##DESCRIPTION
!!    Given a character variable convert it to an array of single-character
!!    character variables with the last element set to a null character.
!!    This is generally used to pass character variables to C procedures.
!!
!!##EXAMPLES
!!
!!    Sample Program:
!!
!!     program demo_s2c
!!     use M_strings, only : s2c
!!     implicit none
!!     character(len=*),parameter   :: string="single string"
!!     character(len=3),allocatable :: array(:)
!!        write(*,*)'INPUT STRING ',trim(string)
!!        ! put one character into each 3-character element of array
!!        array=s2c(string)
!!        ! write array with ASCII Decimal Equivalent below it except show
!!        ! unprintable characters like NULL as "XXX"
!!        write(*,'(1x,*("[",a3,"]":))')&
!!             & merge('XXX',array,iachar(array(:)(1:1)) < 32)
!!        write(*,'(1x,*("[",i3,"]":))')&
!!             & iachar(array(:)(1:1))
!!     end program demo_s2c
!!
!!   Expected output:
!!
!!    INPUT STRING single string
!!    [s  ][i  ][n  ][g  ][l  ][e  ][   ][s  ][t  ][r  ][i  ][n  ][g  ][XXX]
!!    [115][105][110][103][108][101][ 32][115][116][114][105][110][103][  0]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function s2c(string)  RESULT (array)
use,intrinsic :: ISO_C_BINDING, only : C_CHAR

! ident_27="@(#) M_strings s2c(3f) copy string(1 Clen(string)) to char array with null terminator"

character(len=*),intent(in)     :: string

! This is changing, but currently the most portable way to pass a CHARACTER variable to C is to convert it to an array of
! character variables with length one and add a null character to the end of the array. The s2c(3f) function helps do this.
character(kind=C_CHAR,len=1)    :: array(len_trim(string)+1)
integer                         :: i
   do i = 1,size(array)-1
      array(i) = string(i:i)
   enddo
   array(size(array):)=achar(0)
end function s2c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      c2s(3f) - [M_strings:ARRAY] convert C string pointer to Fortran
!!      character string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function c2s(c_string_pointer) result(f_string)
!!
!!     type(c_ptr), intent(in)       :: c_string_pointer
!!     character(len=:), allocatable :: f_string
!!
!!##DESCRIPTION
!!    Given a C pointer to a character string return a Fortran character
!!    string.
!!
!!##OPTIONS
!!    c_string_pointer  C pointer to convert
!!
!!##RETURNS
!!    f_string          Fortran character variable to return
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function c2s(c_string_pointer) result(f_string)
! gets a C string (pointer), and returns the corresponding Fortran string;
! If the C string is null, it returns "NULL", similar to C's "(null)" printed in similar cases:
use, intrinsic :: iso_c_binding, only: c_ptr,c_f_pointer,c_char,c_null_char

! ident_28="@(#) M_strings c2s(3f) copy pointer to C char array till a null is encountered to a Fortran string up to 4096 characters"

integer,parameter                             :: max_length=4096
type(c_ptr), intent(in)                       :: c_string_pointer
character(len=:), allocatable                 :: f_string
character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
character(len=max_length)                            :: aux_string
integer                                       :: i,length=0

   call c_f_pointer(c_string_pointer,char_array_pointer,[max_length])
   if (.not.associated(char_array_pointer)) then
     allocate(character(len=4)::f_string)
     f_string="NULL"
     return
   endif
   aux_string=" "
   do i=1,max_length
     if (char_array_pointer(i)==c_null_char) then
       length=i-1
       exit
     endif
     aux_string(i:i)=char_array_pointer(i)
   enddo
   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)

end function c2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      indent(3f) - [M_strings:WHITESPACE] count number of leading spaces
!!      in a string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function indent(line)
!!
!!     integer                        :: indent
!!     character(len=*),intent(in)    :: line
!!
!!##DESCRIPTION
!!    Count number of leading spaces in a CHARACTER variable.
!!
!!##EXAMPLES
!!
!!  Sample Program:
!!
!!    program demo_indent
!!    !  test filter to count leading spaces in a character variable
!!    !  might want to call notabs(3f) to expand tab characters
!!    use M_strings, only : indent
!!    implicit none
!!    character(len=1024) :: in
!!    integer             :: ios
!!       READFILE: do
!!          read(*,'(A)',iostat=ios)in
!!          if(ios /= 0) exit READFILE
!!          write(*,'(i3,"",a)')indent(in),trim(in)
!!       enddo READFILE
!!    end program demo_indent
!!
!! Results:
!!
!!      3   a b c
!!      0a b c
!!      6      a b c
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function indent(line)

! ident_29="@(#) M_strings indent(3f) find number of leading spaces in a string"

integer                        :: indent
character(len=*),intent(in)    :: line
integer                        :: i
   indent=0
   NOTSPACE: block
      SCAN: do i=1,len(line)
         if(line(i:i) /= ' ')then
            indent=i-1
            exit NOTSPACE
         endif
      enddo SCAN
      indent=len(line)
   endblock NOTSPACE
end function indent
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    visible(3f) - [M_strings:NONALPHA] expand a string to control and
!!    meta-control representations
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function visible(input) result(output)
!!
!!     character(len=*),intent(in)           :: input
!!     character(len=:),allocatable          :: output
!!
!!##DESCRIPTION
!!     visible(3f) expands characters to commonly used sequences used
!!     to represent the characters as control sequences or meta-control
!!     sequences.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_visible
!!     use M_strings, only : visible
!!     integer :: i
!!        do i=0,255
!!           write(*,'(i0,1x,a)')i,visible(char(i))
!!        enddo
!!     end program demo_visible
!!##BUGS
!!     The expansion is not reversible, as input sequences such as "M-" or
!!     "^a" will look like expanded sequences.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function visible(input) result(output)
character(len=*),intent(in)  :: input
character(len=:),allocatable :: output

! ident_30="@(#) M_strings visible(3f) expand escape sequences in a string to control and meta-control representations"

integer                      :: i
character(len=1)             :: c

character(len=*),parameter :: chars(0:255)= [ &
'^@  ', '^A  ', '^B  ', '^C  ', '^D  ', '^E  ', '^F  ', '^G  ', '^H  ', '^I  ', &
'^J  ', '^K  ', '^L  ', '^M  ', '^N  ', '^O  ', '^P  ', '^Q  ', '^R  ', '^S  ', &
'^T  ', '^U  ', '^V  ', '^W  ', '^X  ', '^Y  ', '^Z  ', '^[  ', '^\  ', '^]  ', &
'^^  ', '^_  ', '    ', '!   ', '"   ', '#   ', '$   ', '%   ', '&   ', '''   ', &
'(   ', ')   ', '*   ', '+   ', ',   ', '-   ', '.   ', '/   ', '0   ', '1   ', &
'2   ', '3   ', '4   ', '5   ', '6   ', '7   ', '8   ', '9   ', ':   ', ';   ', &
'<   ', '=   ', '>   ', '?   ', '@   ', 'A   ', 'B   ', 'C   ', 'D   ', 'E   ', &
'F   ', 'G   ', 'H   ', 'I   ', 'J   ', 'K   ', 'L   ', 'M   ', 'N   ', 'O   ', &
'P   ', 'Q   ', 'R   ', 'S   ', 'T   ', 'U   ', 'V   ', 'W   ', 'X   ', 'Y   ', &
'Z   ', '[   ', '\   ', ']   ', '^   ', '_   ', '`   ', 'a   ', 'b   ', 'c   ', &
'd   ', 'e   ', 'f   ', 'g   ', 'h   ', 'i   ', 'j   ', 'k   ', 'l   ', 'm   ', &
'n   ', 'o   ', 'p   ', 'q   ', 'r   ', 's   ', 't   ', 'u   ', 'v   ', 'w   ', &
'x   ', 'y   ', 'z   ', '{   ', '|   ', '}   ', '~   ', '^?  ', 'M-^@', 'M-^A', &
'M-^B', 'M-^C', 'M-^D', 'M-^E', 'M-^F', 'M-^G', 'M-^H', 'M-^I', 'M-^J', 'M-^K', &
'M-^L', 'M-^M', 'M-^N', 'M-^O', 'M-^P', 'M-^Q', 'M-^R', 'M-^S', 'M-^T', 'M-^U', &
'M-^V', 'M-^W', 'M-^X', 'M-^Y', 'M-^Z', 'M-^[', 'M-^\', 'M-^]', 'M-^^', 'M-^_', &
'M-  ', 'M-! ', 'M-" ', 'M-# ', 'M-$ ', 'M-% ', 'M-& ', 'M-'' ', 'M-( ', 'M-) ', &
'M-* ', 'M-+ ', 'M-, ', 'M-- ', 'M-. ', 'M-/ ', 'M-0 ', 'M-1 ', 'M-2 ', 'M-3 ', &
'M-4 ', 'M-5 ', 'M-6 ', 'M-7 ', 'M-8 ', 'M-9 ', 'M-: ', 'M-; ', 'M-< ', 'M-= ', &
'M-> ', 'M-? ', 'M-@ ', 'M-A ', 'M-B ', 'M-C ', 'M-D ', 'M-E ', 'M-F ', 'M-G ', &
'M-H ', 'M-I ', 'M-J ', 'M-K ', 'M-L ', 'M-M ', 'M-N ', 'M-O ', 'M-P ', 'M-Q ', &
'M-R ', 'M-S ', 'M-T ', 'M-U ', 'M-V ', 'M-W ', 'M-X ', 'M-Y ', 'M-Z ', 'M-[ ', &
'M-\ ', 'M-] ', 'M-^ ', 'M-_ ', 'M-` ', 'M-a ', 'M-b ', 'M-c ', 'M-d ', 'M-e ', &
'M-f ', 'M-g ', 'M-h ', 'M-i ', 'M-j ', 'M-k ', 'M-l ', 'M-m ', 'M-n ', 'M-o ', &
'M-p ', 'M-q ', 'M-r ', 'M-s ', 'M-t ', 'M-u ', 'M-v ', 'M-w ', 'M-x ', 'M-y ', &
'M-z ', 'M-{ ', 'M-| ', 'M-} ', 'M-~ ', 'M-^?']
output=''
do i=1,len(input)
   c=input(i:i)
   if(c == ' ')then
      output=output//' '
   else
      output=output//trim(chars(iachar(c)))
   endif
enddo
end function visible
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    expand(3f) - [M_strings:NONALPHA] expand C-like escape sequences
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function expand(line,escape) result(lineout)
!!
!!    character(len=*)                      :: line
!!    character(len=1),intent(in),optional  :: escape
!!    character(len=:),allocatable          :: lineout
!!
!!##DESCRIPTION
!!     EXPAND() expands sequences used to represent commonly used escape
!!     sequences or control characters. By default ...
!!
!!     Escape sequences
!!       \      backslash
!!       a      alert (BEL) -- g is an alias for a
!!       b      backspace
!!       c      suppress further output
!!       e      escape
!!       f      form feed
!!       n      new line
!!       r      carriage return
!!       t      horizontal tab
!!       v      vertical tab
!!       oNNN   byte with octal value NNN (3 digits)
!!       dNNN   byte with decimal value NNN (3 digits)
!!       xHH    byte with hexadecimal value HH (2 digits) -- h is an alias for x
!!
!!     The default escape character is the backslash, but this may be
!!     changed using the optional parameter ESCAPE.
!!
!!##EXAMPLES
!!
!!    Sample Program:
!!
!!     program demo_expand
!!     !  test filter to expand escape sequences in input lines
!!     use M_strings, only : expand
!!     character(len=1024) :: line
!!     integer             :: ios
!!        READFILE: block
!!           do
!!              read(*,'(A)',iostat=ios)line
!!              if(ios /= 0) exit READFILE
!!              write(*,'(a)')trim(expand(line))
!!           enddo
!!        endblock READFILE
!!     end program demo_expand
!!
!!    Sample input:
!!
!!      \e[2J
!!      \tABC\tabc
!!      \tA\a
!!      \nONE\nTWO\nTHREE
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function expand(line,escape) result(lineout)
!x!USE ISO_C_BINDING ,ONLY: c_horizontal_tab

! ident_31="@(#) M_strings expand(3f) return string with escape sequences expanded"

character(len=*),parameter            :: c_horizontal_tab=char(9)
character(len=*),intent(in)           :: line
character(len=1),intent(in),optional  :: escape ! escape character. Default is backslash
! expand escape sequences found in input string
! Escape sequences
!    %%      escape character           %a     alert (BEL) -- gi is an alias for a
!    %b      backspace                  %c     suppress further output
!    %e      escape                     %E     escape
!    %f      form feed                  %n     new line
!    %r      carriage return            %t     horizontal tab
!    %v      vertical tab
!    %oNNN   byte with octal value NNN (3 digits)
!    %dNNN   byte with decimal value NNN (3 digits)
!    %xHH    byte with hexadecimal value HH (2 digits) -- h is an alias for x
character(len=1)                      :: esc    ! escape character. Default is %
character(len=:),allocatable          :: lineout
integer                               :: i
integer                               :: lgth
character(len=3)                      :: thr
integer                               :: xxx
integer                               :: ios
   i=0 ! pointer into input

   lgth=len_trim(line)
   lineout=''

   if(lgth == 0)return

   if (present(escape))then
      esc=escape
   else
      esc=char(92)
   endif

   EXP: do
      i=i+1
      if(i > lgth)exit
      if(line(i:i) == esc)then
         i=i+1
         if(i > lgth)exit
         if(line(i:i) /= esc)then
            BACKSLASH: select case(line(i:i))
            case('a','A','g','G');lineout=lineout//char(  7) ! %a     alert (BEL)
            case('b','B');lineout=lineout//char(  8)         ! %b     backspace
            case('c','C');exit EXP                           ! %c     suppress further output
            case('d','D')                                    ! %d     Dnnn decimal value
                      thr=line(i+1:)
                   read(thr,'(i3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('e','E');lineout=lineout//char( 27)         ! %e     escape
            case('f','F');lineout=lineout//char( 12)         ! %f     form feed
            case('n','N');lineout=lineout//char( 10)         ! %n     new line
           !case('n','N');lineout=lineout//new_line('A')     ! %n     new line
            case('o','O')
                      thr=line(i+1:)
                   read(thr,'(o3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('r','R');lineout=lineout//char( 13)         ! %r     carriage return
            case('t','T');lineout=lineout//c_horizontal_tab  ! %t     horizontal tab
            case('v','V');lineout=lineout//char( 11)         ! %v     vertical tab
            case('x','X','h','H')                            ! %x     xHH  byte with hexadecimal value HH (1 to 2 digits)
                      thr=line(i+1:)
                   read(thr,'(z2)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+2
            end select BACKSLASH
         else
            lineout=lineout//esc                             ! escape character, defaults to backslash
         endif
      else
         lineout=lineout//line(i:i)
      endif
      if(i >= lgth)exit EXP
   enddo EXP

end function expand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notabs(3f) - [M_strings:NONALPHA] expand tab characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine notabs(INSTR,OUTSTR,lgth)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=*),intent=(out) :: OUTSTR
!!     integer,intent=(out)          :: lgth
!!
!!##DESCRIPTION
!!     NOTABS() converts tabs in INSTR to spaces in OUTSTR while maintaining
!!     columns. It assumes a tab is set every 8 characters. Trailing spaces
!!     are removed.
!!
!!     In addition, trailing carriage returns and line feeds are removed
!!     (they are usually a problem created by going to and from MSWindows).
!!
!!     What are some reasons for removing tab characters from an input line?
!!     Some Fortran compilers have problems with tabs, as tabs are not
!!     part of the Fortran character set. Some editors and printers will
!!     have problems with tabs. It is often useful to expand tabs in input
!!     files to simplify further processing such as tokenizing an input line.
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded. Assumed to be of sufficient
!!               length
!!     lgth      Significant length of returned string
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_notabs
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use M_strings, only : notabs
!!    character(len=1024) :: in,out
!!    integer             :: ios,iout
!!       do
!!          read(*,'(A)',iostat=ios)in
!!          if(ios /= 0) exit
!!          call notabs(in,out,iout)
!!          write(*,'(a)')out(:iout)
!!       enddo
!!    end program demo_notabs
!!
!!##SEE ALSO
!!     GNU/Unix commands expand(1) and unexpand(1)
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental impure subroutine notabs(instr,outstr,lgth)

! ident_32="@(#) M_strings notabs(3f) convert tabs to spaces while maintaining columns remove CRLF chars"

character(len=*),intent(in)   :: instr        ! input line to scan for tab characters
character(len=*),intent(out)  :: outstr       ! tab-expanded version of INSTR produced
integer,intent(out)           :: lgth         ! column position of last character put into output string
                                              ! that is, lgth holds the position of the last non-blank character in OUTSTR
!===================================================================================================================================
integer,parameter             :: tabsize=8    ! assume a tab stop is set every 8th column
integer                       :: ipos         ! position in OUTSTR to put next character of INSTR
integer                       :: lenin        ! length of input string trimmed of trailing spaces
integer                       :: lenout       ! number of characters output string can hold
integer                       :: istep        ! counter that advances thru input string INSTR one character at a time
character(len=1)              :: c            ! character in input line being processed
integer                       :: iade         ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   ipos=1                                     ! where to put next character in output string OUTSTR
   lenin=len_trim(instr( 1:len(instr) ))      ! length of INSTR trimmed of trailing spaces
   lenout=len(outstr)                         ! number of characters output string OUTSTR can hold
   outstr=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: do istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=iachar(c)                       ! get ADE of the character
         EXPAND_TABS : select case (iade)     ! take different actions depending on which character was found
         case(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (mod(ipos-1,tabsize)))
         case(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         case default                         ! c is anything else other than a tab,newline,or return  insert it in output string
            if(ipos > lenout)then
               call journal("*notabs* output string overflow")
               exit
            else
               outstr(ipos:ipos)=c
               ipos=ipos+1
            endif
         end select EXPAND_TABS
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=min(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      lgth=len_trim(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
end subroutine notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dilate(3f) - [M_strings:NONALPHA] expand tab characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dilate(INSTR) result(OUTSTR)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=:),allocatable  :: OUTSTR
!!
!!##DESCRIPTION
!!     dilate() converts tabs in INSTR to spaces in OUTSTR.  It assumes a
!!     tab is set every 8 characters. Trailing spaces are removed.
!!
!!     In addition, trailing carriage returns and line feeds are removed
!!     (they are usually a problem created by going to and from MSWindows).
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_dilate
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use M_strings, only : dilate
!!    implicit none
!!    character(len=:),allocatable :: in
!!    integer                      :: i
!!       in='  this is my string  '
!!       ! change spaces to tabs to make a sample input
!!       do i=1,len(in)
!!          if(in(i:i) == ' ')in(i:i)=char(9)
!!       enddo
!!       write(*,'(a)')in,dilate(in)
!!    end program demo_dilate
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!    Public Domain
function dilate(INSTR) result(OUTSTR)

! ident_33="@(#) M_strings dilate(3f) convert tabs to spaces and trims line removing CRLF chars"

CHARACTER(LEN=*),INTENT(IN)   :: instr        ! input line to scan for tab characters
CHARACTER(LEN=:),allocatable  :: outstr       ! tab-expanded version of INSTR produced
integer                       :: i
integer                       :: icount
integer                       :: lgth
   icount=0
   do i=1,len(instr)
      if(instr(i:i) == char(9))icount=icount+1
   enddo
   allocate(character(len=(len(instr)+8*icount)) :: outstr)
   call notabs(instr,outstr,lgth)
   outstr=outstr(:lgth)
!===================================================================================================================================
END function dilate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    adjustc(3f) - [M_strings:WHITESPACE] center text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   pure function adjustc(string[,length])
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in),optional  :: length
!!    character(len=:),allocatable :: adjustc
!!
!!##DESCRIPTION
!!     Centers input text in a string of the length specified. Returns a
!!     string of length LENGTH if LENGTH is present. Otherwise returns a
!!     string of the length of the input string.
!!
!!##OPTIONS
!!     string  input string to trim and center
!!     length  line length to center text in, optional.
!!
!!##RETURNS
!!     adjustc  centered output string
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_adjustc
!!    use M_strings, only : adjustc
!!    !  using length of the input string
!!       write(*,'(a)')       '================================'
!!       write(*,'(a)')adjustc('centered string                 ')
!!       write(*,'(a)')adjustc('                 centered string')
!!       write(*,'(a)')adjustc('  centered string               ')
!!    !  using explicit output string length
!!       write(*,'(a)')repeat('=',50)
!!       write(*,'(a)')adjustc('this is a centered string',50)
!!       write(*,'(a)')repeat('=',50)
!!    end program demo_adjustc
!!
!!   Expected output
!!
!!    ================================
!!            centered string
!!            centered string
!!            centered string
!!    ==================================================
!!                this is a centered string
!!    ==================================================
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
pure function adjustc(string,length)

! ident_34="@(#) M_strings adjustc(3f) center text"

!>
!! PROCEDURE   adjustc(3f)
!! DESCRIPTION center text using implicit or explicit length
!!##VERSION     2.0, 20160711
!! AUTHOR      John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: string         ! input string to trim and center
integer,intent(in),optional  :: length         ! line length to center text in
character(len=:),allocatable :: adjustc        ! output string
integer                      :: inlen
integer                      :: ileft          ! left edge of string if it is centered
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(length))then                     ! optional length
      inlen=length                             ! length will be requested length
      if(inlen <= 0)then                       ! bad input length
         inlen=len(string)                     ! could not use input value, fall back to length of input string
      endif
   else                                        ! output length was not explicitly specified, use input string length
      inlen=len(string)
   endif
   allocate(character(len=inlen):: adjustc)    ! create output at requested length
   adjustc(1:inlen)=' '                        ! initialize output string to all blanks
!-----------------------------------------------------------------------------------------------------------------------------------
   ileft =(inlen-len_trim(adjustl(string)))/2  ! find starting point to start input string to center it
   if(ileft > 0)then                          ! if string will fit centered in output
      adjustc(ileft+1:inlen)=adjustl(string)   ! center the input text in the output string
   else                                        ! input string will not fit centered in output string
      adjustc(1:inlen)=adjustl(string)         ! copy as much of input to output as can
   endif
end function adjustc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    nospace(3f) - [M_strings:WHITESPACE] remove all whitespace from
!!    input string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function nospace(str) - remove all whitespace from input string
!!
!!     character(len=*),intent(in)          :: str
!!     character(len=:),allocatable         :: nospace
!!
!!##DESCRIPTION
!!    nospace(3f) removes space, tab, carriage return, new line, vertical
!!    tab, formfeed and null characters (called "whitespace"). The output
!!    is returned trimmed.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_nospace
!!     use M_strings, only: nospace
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s='  This     is      a     test  '
!!        write(*,*) 'original input string is ....',s
!!        write(*,*) 'processed output string is ...',nospace(s)
!!        if(nospace(s) == 'Thisisatest')then
!!           write(*,*)'nospace test passed'
!!        else
!!           write(*,*)'nospace test error'
!!        endif
!!     end program demo_nospace
!!
!!   Expected output
!!
!!     original input string is ....  This     is      a     test
!!     processed output string is ...Thisisatest
!!     nospace test passed
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function nospace(line)

! ident_35="@(#) M_strings nospace(3f) remove all whitespace from input string"

character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=:),allocatable   ::  nospace          ! returned string
integer                        ::  ipos             ! position to place next output character at
integer                        ::  i                ! counter to increment from beginning to end of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(nospace,mold=line)                      ! initially make output line length of input line
   nospace(:len_trim(nospace))=' '
   ipos=0
   do i=1,len_trim(line)                            ! increment from first to last character of the input line
      if ( isspace( line(i:i) ) ) cycle             ! if a blank is encountered skip it
      ipos=ipos+1                                   ! increment count of non-blank characters found
      nospace(ipos:ipos)=line(i:i)                  ! store non-blank character in output
   enddo
   nospace=trim(nospace)                            ! blank out unpacked part of line
end function nospace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    stretch(3f) - [M_strings:LENGTH] return string padded to at least
!!    specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function stretch(str,length,pattern,suffix) result(strout)
!!
!!     character(len=*),intent(in)         :: str
!!     integer,intent(in)                  :: length
!!     character(len=*)intent(in),optional :: pattern
!!     character(len=*)intent(in),optional :: suffix
!!     character(len=:),allocatable        :: strout
!!
!!##DESCRIPTION
!!    stretch(3f) pads a string with spaces to at least the specified
!!    length. If the trimmed input string is longer than the requested
!!    length the original string is returned trimmed of trailing spaces.
!!
!!##OPTIONS
!!    str      the input string to return trimmed, but then padded to
!!             the specified length if shorter than length
!!    length   The minimum string length to return
!!    pattern  optional string to use as padding. Defaults to a space.
!!    suffix   optional string to append to output string
!!
!!##RETURNS
!!    strout  The input string padded to the requested length or
!!            the trimmed input string if the input string is
!!            longer than the requested length.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!   program demo_stretch
!!    use M_strings, only : stretch
!!    implicit none
!!    character(len=10)            :: string='abcdefghij'
!!    character(len=:),allocatable :: answer
!!    integer                      :: i
!!       answer=stretch(string,5)
!!       write(*,'("[",a,"]")') answer
!!       answer=stretch(string,20)
!!       write(*,'("[",a,"]")') answer
!!       i=30
!!       write(*,*)
!!       write(*,'(1x,a,i0)') &
!!        & stretch('CHAPTER 1 : The beginning ',i,'.'), 1    ,&
!!        & stretch('CHAPTER 2 : The end ',i,'.'),       1234 ,&
!!        & stretch('APPENDIX ',i,'.'),                  1235
!!       write(*,*)
!!       write(*,'(1x,a,i7)') &
!!        & stretch('CHAPTER 1 : The beginning ',i,'.'), 1    ,&
!!        & stretch('CHAPTER 2 : The end ',i,'.'),       1234 ,&
!!        & stretch('APPENDIX ',i,'.'),                  1235
!!       write(*,*)
!!       write(*,*) &
!!        & stretch('CHAPTER 1 : The beginning ',i,suffix=': '), 1
!!       write(*,*) &
!!        & stretch('CHAPTER 2 : The end ',i,suffix=': '),1234
!!       write(*,*) &
!!        & stretch('APPENDIX ',i,suffix=': '),           1235
!!   end program demo_stretch
!!
!!   Results:
!!
!!    [abcdefghij]
!!    [abcdefghij          ]
!!
!!     CHAPTER 1 : The beginning ....1
!!     CHAPTER 2 : The end ..........1234
!!     APPENDIX .....................1235
!!
!!     CHAPTER 1 : The beginning ....      1
!!     CHAPTER 2 : The end ..........   1234
!!     APPENDIX .....................   1235
!!
!!     CHAPTER 1 : The beginning     :            1
!!     CHAPTER 2 : The end           :         1234
!!     APPENDIX                      :         1235
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function stretch(line,length,pattern,suffix) result(strout)

! ident_36="@(#) M_strings stretch(3f) return string padded to at least specified length"

character(len=*),intent(in)                  :: line
integer,intent(in)                           :: length
character(len=*),intent(in),optional         :: pattern
character(len=*),intent(in),optional         :: suffix
!-!character(len=max(length,len(trim(line)))) :: strout
character(len=:),allocatable                 :: strout
   if(present(pattern))then
      strout=pad(line,length,pattern)
   else
      strout=pad(line,length)
   endif
   if(present(suffix))then
      strout=strout//suffix
   endif
end function stretch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    rpad(3f) - [M_strings:LENGTH] convert to a string and pad on the right
!!    to requested length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function rpad(valuein,length) result(strout)
!!
!!     class*,intent(in)       :: valuein(..)
!!     integer,intent(in)      :: length
!!
!!##DESCRIPTION
!!    rpad(3f) converts a scalar intrinsic value to a string and then pads
!!    it on the right with spaces to at least the specified length. If the
!!    trimmed input string is longer than the requested length the string
!!    is returned trimmed of leading and trailing spaces.
!!
!!##OPTIONS
!!    str      The input may be scalar or a vector.
!!             the input value to return as a string, padded on the left to
!!             the specified length if shorter than length. The input may be
!!             any intrinsic scalar which is converted to a cropped string
!!             much as if written with list-directed output.
!!    length   The minimum string length to return
!!
!!##RETURNS
!!    strout  The input string padded to the requested length
!!            on the right with spaces.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!      program demo_rpad
!!       use M_strings, only : rpad
!!       implicit none
!!          write(*,'("[",a,"]")') rpad( 'my string', 20)
!!          write(*,'("[",a,"]")') rpad( 'my string   ', 20)
!!          write(*,'("[",a,"]")') rpad( '   my string', 20)
!!          write(*,'("[",a,"]")') rpad( '   my string   ', 20)
!!          write(*,'("[",a,"]")') rpad( valuein=42 , length=7)
!!          write(*,'("[",a,"]")') rpad( valuein=1.0/9.0 , length=20)
!!      end program demo_rpad
!!
!!  Results:
!!
!!      > [my string           ]
!!      > [my string           ]
!!      > [my string           ]
!!      > [my string           ]
!!      > [42     ]
!!      > [0.111111112         ]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function rpad_scalar(valuein,length) result(strout)

! ident_37="@(#) M_strings rpad_scalar(3f) return value padded to at least specified length"

class(*),intent(in)              :: valuein
integer,intent(in),optional      :: length
character(len=:),allocatable     :: strout

character(len=96)                :: line
integer                          :: local_length

   select type(valuein)
      type is (integer(kind=int8));    write(line,'(i0)') valuein
      type is (integer(kind=int16));   write(line,'(i0)') valuein
      type is (integer(kind=int32));   write(line,'(i0)') valuein
      type is (integer(kind=int64));   write(line,'(i0)') valuein
      type is (real(kind=real32));     write(line,'(1pg0)') valuein
      type is (real(kind=real64));     write(line,'(1pg0)') valuein
      type is (logical);               write(line,'(l1)') valuein
      type is (complex);               write(line,'("(",1pg0,",",1pg0,")")') valuein
      type is (character(len=*))
         if(present(length))then
            local_length = length
         else
            local_length = len(valuein)
         endif
         strout = pad(valuein,local_length,' ',clip=.true.)
         return
      class default
         stop '<ERROR>*rpad_scalar* unknown type'
   end select

   if(present(length))then
      strout = pad( line, length, ' ', clip=.true. )
   else
      strout = crop( line )
   endif

end function rpad_scalar
!===================================================================================================================================
function rpad_vector(valuein,length) result(strout)

! ident_38="@(#) M_strings rpad_vector(3f) return strings or arguments converted to string right-padded to at least specified length"

class(*),intent(in)              :: valuein(:)
integer,intent(in),optional      :: length
character(len=:),allocatable     :: strout(:)
integer                          :: i
integer                          :: mxlen
   if(present(length))then
      allocate(character(len=length) :: strout(size(valuein) ))
      do i=1,size(valuein)
         strout(i)=rpad_scalar(valuein(i),length)
      enddo
   else  ! doing this twice is a lot of overhead
      mxlen=0
      do i=1,size(valuein)
         mxlen=max(mxlen, len_trim(rpad_scalar(valuein(i))) )
      enddo
      allocate(character(len=mxlen) :: strout(size(valuein) ))
      do i=1,size(valuein)
         strout(i)=rpad_scalar(valuein(i),mxlen)
      enddo
   endif
end function rpad_vector
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    cpad(3f) - [M_strings:LENGTH] convert to a cropped string and then
!!    centers the string to specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function cpad(valuein,length) result(strout)
!!
!!     class*,intent(in)       :: valuein(..)
!!     integer,intent(in)      :: length
!!
!!##DESCRIPTION
!!    cpad(3f) converts a scalar value to a cropped string and then pads
!!    it with spaces to center it to at least the specified length. If
!!    the trimmed input is longer than the requested length the string is
!!    returned trimmed of leading and trailing spaces.
!!
!!##OPTIONS
!!    str      The input may be scalar or a vector.
!!             the input value to return as a string, padded with spaces to
!!             center it at the the specified length if shorter than
!!             length. The input may be any intrinsic scalar which is
!!             converted to a cropped string much as if written with
!!             list-directed output.
!!    length   The minimum string length to return
!!
!!##RETURNS
!!    strout  The input string center-padded to the requested length
!!            with spaces.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!      program demo_cpad
!!       use M_strings, only : cpad
!!       implicit none
!!          write(*,'("[",a,"]")') cpad( 'my string', 20)
!!          write(*,'("[",a,"]")') cpad( 'my string   ', 20)
!!          write(*,'("[",a,"]")') cpad( '   my string', 20)
!!          write(*,'("[",a,"]")') cpad( '   my string   ', 20)
!!          write(*,'("[",a,"]")') cpad( valuein=42 , length=7)
!!          write(*,'("[",a,"]")') cpad( valuein=1.0/9.0 , length=20)
!!      end program demo_cpad
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function cpad_scalar(valuein,length) result(strout)

! ident_39="@(#) M_strings cpad_scalar(3f) convert value to string center-padded to at least specified length"

class(*),intent(in)              :: valuein
integer,intent(in),optional      :: length
character(len=:),allocatable     :: strout

character(len=96)                :: line
integer                          :: local_length

   select type(valuein)
      type is (integer(kind=int8));    write( line, '(i0)' ) valuein
      type is (integer(kind=int16));   write( line, '(i0)' ) valuein
      type is (integer(kind=int32));   write( line, '(i0)' ) valuein
      type is (integer(kind=int64));   write( line, '(i0)' ) valuein
      type is (real(kind=real32));     write( line, '(1pg0)' ) valuein
      type is (real(kind=real64));     write( line, '(1pg0)' ) valuein
      type is (logical);               write( line, '(l1)' ) valuein
      type is (complex);               write( line, '("(",1pg0,",",1pg0,")")' ) valuein
      type is (character(len = *))
         if(present( length ) )then
            local_length = length
         else
            local_length = len(valuein)
         endif
         strout = adjustc( crop(valuein), local_length )
         return
      class default
         stop '<ERROR>*cpad_scalar* unknown type'
   end select

   if(present(length))then
      strout = adjustc( crop(line), length )
   else
      strout = crop( line )
   endif

end function cpad_scalar
!===================================================================================================================================
function cpad_vector(valuein,length) result(strout)

! ident_40="@(#) M_strings cpad_vector(3f) return strings or arguments converted to string center-padded to at least specified length"

class(*),intent(in)              :: valuein(:)
integer,intent(in),optional      :: length
character(len=:),allocatable     :: strout(:)
integer                          :: i
integer                          :: mxlen
   if(present(length))then
      allocate(character(len=length) :: strout(size(valuein) ))
      do i=1,size(valuein)
         strout(i)=cpad_scalar(valuein(i),length)
      enddo
   else  ! doing this twice is a lot of overhead
      mxlen=0
      do i=1,size(valuein)
         mxlen=max(mxlen, len_trim(cpad_scalar(valuein(i))) )
      enddo
      allocate(character(len=mxlen) :: strout(size(valuein) ))
      do i=1,size(valuein)
         strout(i)=cpad_scalar(valuein(i),mxlen)
      enddo
   endif
end function cpad_vector
!===================================================================================================================================
!>
!!
!!##NAME
!!    lpad(3f) - [M_strings:LENGTH] convert to a cropped string and then
!!    blank-pad on the left to requested length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lpad(valuein,length) result(strout)
!!
!!     class*,intent(in)       :: valuein(..)
!!     integer,intent(in)      :: length
!!
!!##DESCRIPTION
!!    lpad(3f) converts a scalar value to a cropped string and then pads
!!    it on the left with spaces to at least the specified length. If
!!    the trimmed input is longer than the requested length the string is
!!    returned trimmed of leading and trailing spaces.
!!
!!##OPTIONS
!!    str      The input may be scalar or a vector.
!!             the input value to return as a string, padded on the left to
!!             the specified length if shorter than length. The input may be
!!             any intrinsic scalar which is converted to a cropped string
!!             much as if written with list-directed output.
!!    length   The minimum string length to return
!!
!!##RETURNS
!!    strout  The input string padded to the requested length
!!            on the left with spaces.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!      program demo_lpad
!!       use M_strings, only : lpad
!!       implicit none
!!          write(*,'("[",a,"]")') lpad( 'my string', 20)
!!          write(*,'("[",a,"]")') lpad( 'my string   ', 20)
!!          write(*,'("[",a,"]")') lpad( '   my string', 20)
!!          write(*,'("[",a,"]")') lpad( '   my string   ', 20)
!!          write(*,'("[",a,"]")') lpad( valuein=42 , length=7)
!!          write(*,'("[",a,"]")') lpad( valuein=1.0/9.0 , length=20)
!!      end program demo_lpad
!!
!! Results:
!!
!!     > [           my string]
!!     > [           my string]
!!     > [           my string]
!!     > [           my string]
!!     > [     42]
!!     > [         0.111111112]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function lpad_scalar(valuein,length) result(strout)

! ident_41="@(#) M_strings lpad_scalar(3f) convert value to string padded on left to at least specified length"

class(*),intent(in)              :: valuein
integer,intent(in),optional      :: length
character(len=:),allocatable     :: strout

character(len=96)                :: line
integer                          :: local_length

   select type(valuein)
      type is (integer(kind=int8));    write(line,'(i0)') valuein
      type is (integer(kind=int16));   write(line,'(i0)') valuein
      type is (integer(kind=int32));   write(line,'(i0)') valuein
      type is (integer(kind=int64));   write(line,'(i0)') valuein
      type is (real(kind=real32));     write(line,'(1pg0)') valuein
      type is (real(kind=real64));     write(line,'(1pg0)') valuein
      type is (logical);               write(line,'(l1)') valuein
      type is (complex);               write(line,'("(",1pg0,",",1pg0,")")') valuein
      type is (character(len=*))
         if(present( length ))then
            local_length=length
         else
            local_length=len( valuein )
         endif
         strout = pad( valuein, local_length, ' ', right=.false., clip=.true. )
         return
      class default
         stop '<ERROR>*lpad_scalar* unknown type'
   end select

   if(present(length))then
      strout = pad( line, length, ' ', clip=.true., right=.false. )
   else
      strout = crop( line )
   endif

end function lpad_scalar
!===================================================================================================================================
function lpad_vector(valuein,length) result(strout)

! ident_42="@(#) M_strings lpad_vector(3f) return vector of strings or arguments converted to string left-padded to at least specified length"

class(*),intent(in)              :: valuein(:)
integer,intent(in),optional      :: length
character(len=:),allocatable     :: strout(:)
integer                          :: i
integer                          :: mxlen
   if(present(length))then
      allocate(character(len=length) :: strout(size(valuein) ))
      do i=1,size(valuein)
         strout(i)=lpad_scalar(valuein(i),length)
      enddo
   else  ! doing this twice is a lot of overhead
      mxlen=0
      do i=1,size(valuein)
         mxlen=max(mxlen, len_trim(lpad_scalar(valuein(i))) )
      enddo
      allocate(character(len=mxlen) :: strout(size(valuein) ))
      do i=1,size(valuein)
         strout(i)=lpad_scalar(valuein(i),mxlen)
      enddo
   endif
end function lpad_vector
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    zpad(3f) - [M_strings:LENGTH] pad a string on the left with zeros to
!!    specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function zpad(valuein,length) result(strout)
!!
!!     class*,intent(in)           :: valuein(..)
!!     integer,intent(in),optional :: length
!!
!!##DESCRIPTION
!!    zpad(3f) crops the input string or integer (which will be converted
!!    to a string) and then pads it on the left with zeros to at least
!!    the specified length. If the trimmed input string is longer than the
!!    requested length the original string is returned trimmed of leading
!!    and trailing spaces.
!!
!!    For strings representing unsigned numbers this is basically an alias for
!!
!!        strout=pad(str,length,'0',clip=.true.,right=.false.)
!!
!!    For integers the same is often done with internal WRITE(3f) statements
!!    such as
!!
!!        write(strout,'(i5.5)')ivalue
!!
!!    but unlike internal I/O the function call can be used in expressions
!!    or passed as a procedure argument. If the requested length is exceeded
!!    the returned string is untruncated but cropped of leading and trailing
!!    spaces.
!!
!!##OPTIONS
!!    str      May be a scalor or vector string or integer. The input string
!!             to return trimmed, but then padded to the specified length
!!             if shorter than length. If an integer is input it is first
!!             converted to a string. If the leftmost non-blank character
!!             is a sign character it is moved to the left-most position
!!             of the output.
!!    length   The minimum string length to return. If not present, the
!!             length of the input parameter STR is used. If the input value
!!             STR is not a string no zero padding occurs if LENGTH is not
!!             supplied.
!!
!!##RETURNS
!!    strout  The input string padded to the requested length or the trimmed
!!            input string if the input string is longer than the requested
!!            length.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!      program demo_zpad
!!       use M_strings, only : zpad
!!       implicit none
!!       integer :: lun, i
!!          write(*,'("[",a,"]")') zpad( '111', 5)
!!          write(*,'("[",a,"]")') zpad( '123456789', 5)
!!          write(*,'("[",a,"]")') zpad( '  34567  ', 7)
!!          write(*,'("[",a,"]")') zpad( valuein=42 , length=7)
!!          write(*,'("[",a,"]")') zpad( '  +34567  ', 7)
!!          write(*,'("[",a,"]")') zpad( '  -34567  ', 7)
!!          write(*,'("[",a,"]")') zpad(1234)
!!          write(*,'("[",a,"]")') zpad(-1234)
!!          write(*,'("[",a,"]")') zpad(1234,8)
!!          write(*,'("[",a,"]")') zpad(-1234,8)
!!          write(*,'("[",a,"]")') zpad('')
!!          write(*,'("[",a,"]")') zpad('0')
!!          write(*,'("[",a,"]")') zpad('0    ')
!!          write(*,'("[",a,"]")') zpad('     ')
!!          write(*,'("[",a,"]")') zpad([1,10,100,1000,10000,100000],8)
!!
!!          ! open output_00085.dat
!!          i=85
!!          open(newunit=lun,file='output_'//zpad(i,5)//'.dat')
!!          close(unit=lun,status='delete')
!!
!!      end program demo_zpad
!!
!!  Results:
!!
!!       [00111]
!!       [123456789]
!!       [0034567]
!!       [0000042]
!!       [+0034567]
!!       [-0034567]
!!       [1234]
!!       [-1234]
!!       [00001234]
!!       [-00001234]
!!       []
!!       [0]
!!       [00000]
!!       [00000]
!!       [00000001]
!!       [00000010]
!!       [00000100]
!!       [00001000]
!!       [00010000]
!!       [00100000]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function zpad_scalar(valuein,length) result(strout)

! ident_43="@(#) M_strings zpad_vector(3f) return string or argument converted to string zero-padded to at least specified length"

class(*),intent(in)              :: valuein
integer,intent(in),optional      :: length
character(len=:),allocatable     :: strout
character(len=4096)              :: line
integer                          :: local_length
   if(present(length))then
      local_length=length
   else
      local_length=-1
   endif
   select type(valuein)
      type is (integer(kind=int8));     write(line,'(i0)') valuein
      type is (integer(kind=int16));    write(line,'(i0)') valuein
      type is (integer(kind=int32));    write(line,'(i0)') valuein
      type is (integer(kind=int64));    write(line,'(i0)') valuein
      type is (real(kind=real32));      write(line,'(1pg0)') valuein
      type is (real(kind=real64));      write(line,'(1pg0)') valuein
      type is (logical);                write(line,'(l1)') valuein
      type is (character(len=*));       line=valuein
         if(local_length==-1)local_length=len(valuein)
      type is (complex);                write(line,'("(",1pg0,",",1pg0,")")') valuein
   end select
   if(local_length == -1)then
     strout=clip(line)
   else
      line=clip(line)//'  '
      if(scan(line(1:1),'+-') == 1)then
         strout= line(1:1)//pad(line(2:),local_length,'0',clip=.true.,right=.false.)
      else
         strout= pad(line,local_length,'0',clip=.true.,right=.false.)
      endif
   endif
end function zpad_scalar
!===================================================================================================================================
function zpad_vector(valuein,length) result(strout)

! ident_44="@(#) M_strings zpad_vector(3f) return vector of strings or arguments converted to string zero-padded to at least specified length"

class(*),intent(in)              :: valuein(:)
integer,intent(in),optional      :: length
character(len=:),allocatable     :: strout(:)
integer                          :: i
integer                          :: mxlen
   if(present(length))then
      allocate(character(len=length) :: strout(size(valuein) ))
      do i=1,size(valuein)
         strout(i)=zpad_scalar(valuein(i),length)
      enddo
   else  ! doing this twice is a lot of overhead
      mxlen=0
      do i=1,size(valuein)
         mxlen=max(mxlen, len_trim(zpad_scalar(valuein(i))) )
      enddo
      allocate(character(len=mxlen) :: strout(size(valuein) ))
      do i=1,size(valuein)
         strout(i)=zpad_scalar(valuein(i),mxlen)
      enddo
   endif
end function zpad_vector
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   pad(3f) - [M_strings:LENGTH] return string padded to at least
!!   specified length
!!   (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!   function pad(str,length,pattern,right,clip) result(strout)
!!
!!    character(len=*)                           :: str
!!    integer,intent(in)                         :: length
!!    character(len=max(length,len(trim(line)))) :: strout
!!    character(len=*),intent(in),optional       :: pattern
!!    logical,intent(in),optional                :: right
!!    logical,intent(in),optional                :: clip
!!
!!##DESCRIPTION
!!   pad(3f) pads a string with a pattern to at least the specified
!!   length. If the trimmed input string is longer than the requested
!!   length the trimmed string is returned.
!!
!!##OPTIONS
!!   str      the input string to return trimmed, but then padded to
!!            the specified length if shorter than length
!!   length   The minimum string length to return
!!   pattern  optional string to use as padding. Defaults to a space.
!!   right    if true pads string on the right, else on the left
!!   clip     trim spaces from input string but otherwise retain length.
!!            Except for simple cases you typically would trim the input
!!            yourself.
!!
!!##RETURNS
!!   strout  The input string padded to the requested length or
!!           the trimmed input string if the input string is
!!           longer than the requested length.
!!
!!##EXAMPLE
!!
!!  Sample Program:
!!
!!    program demo_pad
!!     use M_strings, only : pad
!!     implicit none
!!     character(len=10)            :: string='abcdefghij'
!!     character(len=:),allocatable :: answer
!!     integer                      :: i
!!     character(len=*),parameter   :: g='(*(g0))'
!!        answer=pad(string,5)
!!        write(*,'("[",a,"]")') answer
!!        answer=pad(string,20)
!!        write(*,'("[",a,"]")') answer
!!        i=30
!!        write(*,g)
!!        write(*,'(1x,a,1x,i0)') &
!!         & pad('CHAPTER 1 : The beginning ',i,'.'), 1   , &
!!         & pad('CHAPTER 2 : The end ',i,'.'),       1234, &
!!         & pad('APPENDIX ',i,'.'),                  1235
!!        write(*,*)
!!        write(*,'(1x,a,i7)') &
!!         & pad('CHAPTER 1 : The beginning ',i,'.'), 1   , &
!!         & pad('CHAPTER 2 : The end ',i,'.'),       1234, &
!!         & pad('APPENDIX ',i,'.'),                  1235
!!
!!         write(*,g)pad('12',5,'0',right=.false.)
!!
!!         write(*,g)pad('12345 ',30,'_',right=.false.)
!!         write(*,g)pad('12345 ',30,'_',right=.false.,clip=.true.)
!!         write(*,g)pad('12345 ',7,'_',right=.false.)
!!         write(*,g)pad('12345 ',7,'_',right=.false.,clip=.true.)
!!         write(*,g)pad('12345 ',6,'_',right=.false.)
!!         write(*,g)pad('12345 ',6,'_',right=.false.,clip=.true.)
!!         write(*,g)pad('12345 ',5,'_',right=.false.)
!!         write(*,g)pad('12345 ',5,'_',right=.false.,clip=.true.)
!!         write(*,g)pad('12345 ',4,'_',right=.false.)
!!         write(*,g)pad('12345 ',4,'_',right=.false.,clip=.true.)
!!    end program demo_pad
!!
!!  Results:
!!
!!  > [abcdefghij]
!!  > [abcdefghij          ]
!!  >
!!  >  CHAPTER 1 : The beginning .... 1
!!  >  CHAPTER 2 : The end .......... 1234
!!  >  APPENDIX ..................... 1235
!!  >
!!  >  CHAPTER 1 : The beginning ....      1
!!  >  CHAPTER 2 : The end ..........   1234
!!  >  APPENDIX .....................   1235
!!  > 00012
!!  > ________________________12345
!!  > _________________________12345
!!  > _12345
!!  > __12345
!!  > 12345
!!  > _12345
!!  > 12345
!!  > 12345
!!  > 12345
!!  > 12345
!!
!!##SEE ALSO
!!     adjustl(3f), adjustr(3f), repeat(3f), trim(3f), len_trim(3f), len(3f)
!!
!!     adjustc(3f), stretch(3f), lpad(3f), rpad(3f), cpad(3f), zpad(3f), lenset(3f)
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function pad(line,length,pattern,right,clip) result(strout)

!$@(#) M_strings::pad(3f): return string padded to at least specified length

character(len=*),intent(in)          :: line
integer,intent(in)                   :: length
character(len=*),intent(in),optional :: pattern
logical,optional,intent(in)          :: right
logical,optional,intent(in)          :: clip
character(len=:),allocatable         :: strout
logical                              :: local_right
logical                              :: local_clip
character(len=:),allocatable         :: local_pattern
character(len=:),allocatable         :: local_line

if(  present(right)    )then;  local_right=right;      else;  local_right=.true.;  endif
if(  present(clip)     )then;  local_clip=clip;        else;  local_clip=.false.;  endif
if(  present(pattern)  )then;  local_pattern=pattern;  else;  local_pattern=' ';   endif

if(len(local_pattern) == 0)then
   strout=line
else

   if(local_clip)then
      local_line=trim(adjustl(line))
      allocate(character(len=max(length,len(local_line))) :: strout)
   else
      local_line=line
      allocate(character(len=max(length,len(line))) :: strout)
   endif

   if(local_right)then
      strout(:)=local_line//repeat(local_pattern,len(strout)/len(local_pattern)+1)
   else
      strout(:)=repeat(local_pattern, ceiling(real(len(strout))/len(local_pattern)))
      strout(max(0,len(strout)-len(local_line))+1:)=local_line
   endif

endif
end function pad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lenset(3f) - [M_strings:LENGTH] return string trimmed or padded to
!!                 specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lenset(str,length) result(strout)
!!
!!     character(len=*)                     :: str
!!     character(len=length)                :: strout
!!     integer,intent(in)                   :: length
!!
!!##DESCRIPTION
!!    lenset(3f) truncates a string or pads it with spaces to the specified
!!    length.
!!
!!##OPTIONS
!!    str     input string
!!    length  output string length
!!
!!##RESULTS
!!    strout  output string
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_lenset
!!      use M_strings, only : lenset
!!      implicit none
!!      character(len=10)            :: string='abcdefghij'
!!      character(len=:),allocatable :: answer
!!         answer=lenset(string,5)
!!         write(*,'("[",a,"]")') answer
!!         answer=lenset(string,20)
!!         write(*,'("[",a,"]")') answer
!!     end program demo_lenset
!!
!!    Expected output:
!!
!!     [abcde]
!!     [abcdefghij          ]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function lenset(line,length) result(strout)

! ident_45="@(#) M_strings lenset(3f) return string trimmed or padded to specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=length)        ::  strout
   strout=line
end function lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    merge_str(3f) - [M_strings:LENGTH] pads strings to same length and
!!    then calls MERGE(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function merge_str(str1,str2,expr) result(strout)
!!
!!     character(len=*),intent(in),optional :: str1
!!     character(len=*),intent(in),optional :: str2
!!     logical,intent(in)              :: expr
!!     character(len=:),allocatable    :: strout
!!
!!##DESCRIPTION
!!    merge_str(3f) pads the shorter of str1 and str2 to the longest length
!!    of str1 and str2 and then calls MERGE(padded_str1,padded_str2,expr).
!!    It trims trailing spaces off the result and returns the trimmed
!!    string. This makes it easier to call MERGE(3f) with strings, as
!!    MERGE(3f) requires the strings to be the same length.
!!
!!    NOTE: STR1 and STR2 are always required even though declared optional.
!!          this is so the call "STR_MERGE(A,B,present(A))" is a valid call.
!!          The parameters STR1 and STR2 when they are optional parameters
!!          can be passed to a procedure if the options are optional on the
!!          called procedure.
!!
!!##OPTIONS
!!    STR1    string to return if the logical expression EXPR is true
!!    STR2    string to return if the logical expression EXPR is false
!!    EXPR    logical expression to evaluate to determine whether to return
!!            STR1 when true, and STR2 when false.
!!##RESULT
!!     MERGE_STR  a trimmed string is returned that is otherwise the value
!!                of STR1 or STR2, depending on the logical expression EXPR.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_merge_str
!!     use M_strings, only : merge_str
!!     implicit none
!!     character(len=:), allocatable :: answer
!!        answer=merge_str('first string', &
!!         & 'second string is longer',10 == 10)
!!        write(*,'("[",a,"]")') answer
!!        answer=merge_str('first string', &
!!         & 'second string is longer',10 /= 10)
!!        write(*,'("[",a,"]")') answer
!!     end program demo_merge_str
!!
!!   Expected output
!!
!!     [first string]
!!     [second string is longer]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces

! ident_46="@(#) M_strings merge_str(3f) pads first and second arguments to MERGE(3f) to same length"

character(len=*),intent(in),optional :: str1
character(len=*),intent(in),optional :: str2
character(len=:),allocatable         :: str1_local
character(len=:),allocatable         :: str2_local
logical,intent(in)                   :: expr
character(len=:),allocatable         :: strout
integer                              :: big
   if(present(str2))then
      str2_local=str2
   else
      str2_local=''
   endif
   if(present(str1))then
      str1_local=str1
   else
      str1_local=''
   endif
   big=max(len(str1_local),len(str2_local))
   ! note: perhaps it would be better to warn or fail if an optional value that is not present is returned, instead of returning ''
   strout=trim(merge(lenset(str1_local,big),lenset(str2_local,big),expr))
end function merge_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    squeeze(3f) - [M_strings:EDITING] delete adjacent duplicate occurrences
!!    of a character from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function squeeze(STR,CHAR) result (OUTSTR)
!!
!!     character(len=*),intent(in)          :: STR
!!     character(len=*),intent(in),optional :: CHAR
!!     character(len=len(str))              :: OUTSTR
!!
!!##DESCRIPTION
!!    squeeze(3f) reduces adjacent duplicates of the specified character
!!    to a single character
!!
!!##OPTIONS
!!    STR     input string in which to reduce adjacent duplicate characters
!!            to a single character
!!    CHAR    The character to remove adjacent duplicates of
!!
!!##RETURNS
!!    OUTSTR  string with all contiguous adjacent occurrences of CHAR removed
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_squeeze
!!    use M_strings, only : squeeze
!!    implicit none
!!    character(len=:),allocatable :: strings(:)
!!
!!    strings=[ character(len=72) :: &
!!    &'', &
!!    &'"If I were two-faced,&
!!    &would I be wearing this one?" --- Abraham Lincoln',  &
!!    &'..1111111111111111111&
!!    &111111111111111111111111111111111111111111117777888', &
!!    &'I never give ''em hell,&
!!    &I just tell the truth, and they think it''s hell.',&
!!    &'                                                  &
!!    & --- Harry S Truman'    &
!!    &]
!!       call printme( trim(strings(1)), ' ' )
!!       call printme( strings(2:4),     ['-','7','.'] )
!!       call printme( strings(5),       [' ','-','r'] )
!!    contains
!!    impure elemental subroutine printme(str,chr)
!!    character(len=*),intent(in) :: str
!!    character(len=1),intent(in) :: chr
!!    character(len=:),allocatable :: answer
!!       write(*,'(a)')repeat('=',11)
!!       write(*,'("IN:   <<<",g0,">>>")')str
!!       answer=squeeze(str,chr)
!!       write(*,'("OUT:  <<<",g0,">>>")')answer
!!       write(*,'("LENS: ",*(g0,1x))')"from",len(str),"to",len(answer), &
!!               & "for a change of",len(str)-len(answer)
!!       write(*,'("CHAR: ",g0)')chr
!!    end subroutine printme
!!    end program demo_squeeze
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function squeeze(str,charp) result (outstr)

character(len=*),intent(in)  :: str
character(len=1),intent(in)  :: charp
character(len=:),allocatable :: outstr
character(len=1)             :: ch, last_one
integer                      :: i, pio ! position in output

   outstr=repeat(' ',len(str))      ! start with a string big enough to hold any output
   if(len(outstr)==0)return         ! handle edge condition
   last_one=str(1:1)                ! since at least this long start output with first character
   outstr(1:1)=last_one
   pio=1

   do i=2,len(str)
      ch=str(i:i)
      pio=pio+merge(0,1, ch == last_one.and.ch == charp) ! decide whether to advance before saving
      outstr(pio:pio)=ch  ! store new one or overlay the duplcation
      last_one=ch
   enddo

   outstr=outstr(:pio)              ! trim the output string to just what was set
end function squeeze
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    compact(3f) - [M_strings:WHITESPACE] converts contiguous whitespace
!!    to a single character (or nothing)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function compact(STR,CHAR) result (OUTSTR)
!!
!!     character(len=*),intent(in)          :: STR
!!     character(len=*),intent(in),optional :: CHAR
!!     character(len=len(str))              :: OUTSTR
!!
!!##DESCRIPTION
!!    COMPACT(3f) converts multiple spaces, tabs and control characters
!!    (called "whitespace") to a single character or nothing. Leading
!!    whitespace is removed.
!!
!!##OPTIONS
!!    STR     input string to reduce or remove whitespace from
!!    CHAR    By default the character that replaces adjacent
!!            whitespace is a space. If the optional CHAR parameter is supplied
!!            it will be used to replace the whitespace. If a null character is
!!            supplied for CHAR whitespace is removed.
!!
!!##RETURNS
!!    OUTSTR  string of same length as input string but with all contiguous
!!            whitespace reduced to a single space and leading whitespace
!!            removed
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_compact
!!     use M_strings, only : compact
!!     implicit none
!!     ! produces 'This is a test               '
!!     write(*,*)compact('  This     is      a     test  ')
!!     ! produces 'Thisisatest                  '
!!     write(*,*)compact('  This     is      a     test  ',char='')
!!     ! produces 'This:is:a:test               '
!!     write(*,*)compact('  This     is      a     test  ',char=':')
!!     ! note CHAR is used to replace the whitespace, but if CHAR is
!!     ! in the original string it is just copied
!!     write(*,*)compact('A  AA    A   AAAAA',char='A')
!!     ! produces (original A characters are left as-is) 'AAAAAAAAAAAA'
!!     ! not 'A'
!!    end program demo_compact
!!
!!    Expected output
!!
!!     >This is a test
!!     >Thisisatest
!!     >This:is:a:test
!!     >AAAAAAAAAAAA
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!elemental pure function compact(str,char) result (outstr)
function compact(str,char) result (outstr)

! ident_47="@(#) M_strings compact(3f) Converts white-space to single spaces; removes leading spaces"

character(len=*),intent(in)          :: str
character(len=*),intent(in),optional :: char
character(len=len(str))              :: outstr
character(len=1)                     :: ch
integer                              :: i
integer                              :: position_in_output
logical                              :: last_was_space
character(len=1)                     :: char_p
logical                              :: nospace
if(present(char))then
   char_p=char
   if(len(char) == 0)then
      nospace=.true.
   else
      nospace=.false.
   endif
else
   char_p=' '
   nospace=.false.
endif
   outstr=' '
   last_was_space=.false.
   position_in_output=0

   IFSPACE: do i=1,len_trim(str)
     ch=str(i:i)
     select case(iachar(ch))
       case(0:32,127)                                         ! space or tab character or control character
         if(position_in_output == 0)then                      ! still at beginning so ignore leading whitespace
            cycle IFSPACE
         elseif(.not.last_was_space) then                     ! if have not already put out a space output one
           if(.not.nospace)then
              position_in_output=position_in_output+1
              outstr(position_in_output:position_in_output)=char_p
           endif
         endif
         last_was_space=.true.
       case(:-1,33:126,128:)                                  ! not a space, quote, or control character so copy it
         position_in_output=position_in_output+1
         outstr(position_in_output:position_in_output)=ch
         last_was_space=.false.
     end select
   enddo IFSPACE

end function compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     noesc(3f) - [M_strings:NONALPHA] convert non-printable characters
!!     to a space
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental function noesc(INSTR)
!!
!!     character(len=*),intent(in) :: INSTR
!!     character(len=len(instr))   :: noesc
!!
!!##DESCRIPTION
!!      Convert non-printable characters to a space.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_noesc
!!
!!     use M_strings, only : noesc
!!     implicit none
!!     character(len=128) :: ascii
!!     character(len=128) :: cleared
!!     integer            :: i
!!     ! fill variable with base ASCII character set
!!     do i=1,128
!!        ascii(i:i)=char(i-1)
!!     enddo
!!     cleared=noesc(ascii)
!!     write(*,*)'characters and their ADE (ASCII Decimal Equivalent)'
!!     call ade(ascii)
!!     write(*,*)'Cleared of non-printable characters'
!!     call ade(cleared)
!!     write(*,*)'Cleared string:'
!!     write(*,*)cleared
!!     contains
!!       subroutine ade(string)
!!       implicit none
!!       ! the string to print
!!       character(len=*),intent(in) :: string
!!       ! number of characters in string to print
!!       integer :: lgth
!!       ! counter used to step thru string
!!       integer :: i
!!          ! get trimmed length of input string
!!          lgth=len_trim(string(:len(string)))
!!
!!          ! replace lower unprintable characters with spaces
!!          write(*,101)(merge(string(i:i),' ',&
!!          & iachar(string(i:i)) >= 32        &
!!          & .and.                            &
!!          & iachar(string(i:i)) <= 126)      &
!!          & ,i=1,lgth)
!!
!!          ! print ADE value of character underneath it
!!          write(*,202)     (iachar(string(i:i))/100,    i=1,lgth)
!!          write(*,202)(mod( iachar(string(i:i)),100)/10,i=1,lgth)
!!          write(*,202)(mod((iachar(string(i:i))),10),   i=1,lgth)
!!       ! format for printing string characters
!!       101   format(*(a1:))
!!       ! format for printing ADE values
!!       202   format(*(i1:))
!!       end subroutine ade
!!     end program demo_noesc
!!
!!    Expected output
!!
!!    The string is printed with the ADE value vertically beneath.
!!    The original string has all the ADEs from 000 to 127. After
!!    NOESC(3f) is called on the string all the "non-printable"
!!    characters are replaced with a space (ADE of 032).
!!
!!   characters and their ADE (ASCII Decimal Equivalent)
!!
!!    >                                 !"#$%&'()*+,-./0123456789
!!    :;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >0000000000000000000000000000000000000000000000000000000000
!!    0000000000000000000000000000000000000000001111111111111111111111111111
!!    >00000000001111111111222222222233333333334444444444555555555566666666
!!    667777777777888888888899999999990000000000111111111122222222
!!    >012345678901234567890123456789012345678901234567890123456789012345678
!!    90123456789012345678901234567890123456789012345678901234567
!!
!!   Cleared of non-printable characters
!!
!!    >                                 !"#$%&'()*+,-./0123456789
!!    :;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >0000000000000000000000000000000000000000000000000000000000
!!    000000000000000000000000000000000000000000111111111111111111111111111
!!    >3333333333333333333333333333333333333333444444444455555555
!!    556666666666777777777788888888889999999999000000000011111111112222222
!!    >2222222222222222222222222222222223456789012345678901234567
!!    890123456789012345678901234567890123456789012345678901234567890123456
!!
!!   Cleared string:
!!
!!    >                                  !"#$%&'()*+,-./0123456789:;<=>?@
!!    ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function noesc(INSTR)

! ident_48="@(#) M_strings noesc(3f) convert non-printable characters to a space"

character(len=*),intent(in) :: INSTR      ! string that might contain nonprintable characters
character(len=len(instr))   :: noesc
integer                     :: ic,i10
!-----------------------------------------------------------------------------------------------------------------------------------
   noesc=''                               ! initialize output string
   do i10=1,len_trim(INSTR(1:len(INSTR)))
      ic=iachar(INSTR(i10:i10))
      if(ic <= 31.or.ic == 127)then       ! find characters with ADE of 0-31, 127
         noesc(I10:I10)=' '               ! replace non-printable characters with a space
      else
         noesc(I10:I10)=INSTR(i10:i10)    ! copy other characters as-is from input string to output string
      endif
   enddo
end function noesc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_value(3f) - [M_strings:TYPE] subroutine returns numeric
!!      value from string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine string_to_value(chars,valu,ierr)
!!
!!     character(len=*),intent(in)              :: chars   ! input string
!!     integer|real|doubleprecision,intent(out) :: valu
!!     integer,intent(out)                      :: ierr
!!
!!##DESCRIPTION
!!    Returns a numeric value from a numeric character string.
!!
!!    Works with any g-format input, including integer, real, and
!!    exponential. If the input string begins with "B", "Z", or "O"
!!    and otherwise represents a positive whole number it is assumed to
!!    be a binary, hexadecimal, or octal value. If the string contains
!!    commas they are removed. If the string is of the form NN:MMM... or
!!    NN#MMM then NN is assumed to be the base of the whole number.
!!
!!    If an error occurs in the READ, IOSTAT is returned in IERR and
!!    value is set to zero. if no error occurs, IERR=0.
!!
!!##OPTIONS
!!       CHARS  input string to read numeric value from
!!
!!##RETURNS
!!    VALU   numeric value returned. May be INTEGER, REAL, or
!!              DOUBLEPRECISION.
!!    IERR   error flag (0 == no error)
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_string_to_value
!!     use M_strings, only: string_to_value
!!     implicit none
!!     real :: value
!!     integer :: ierr
!!     character(len=80) :: string
!!        string=' -40.5e-2 '
!!        call string_to_value(string,value,ierr)
!!        write(*,*) 'value of string ['//trim(string)//'] is ',value
!!    end program demo_string_to_value
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine a2r(chars,valu,ierr)

! ident_49="@(#) M_strings a2r(3fp) subroutine returns real value from string"

character(len=*),intent(in) :: chars                      ! input string
real,intent(out)            :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(ierr == 0)then
      if(valu8 <= huge(valu))then
         valu=real(valu8)
      else
         call journal('sc','*a2r*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2r
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2i(chars,valu,ierr)

! ident_50="@(#) M_strings a2i(3fp) subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8 <= huge(valu))then
      if(valu8 <= huge(valu))then
         valu=int(valu8)
      else
         call journal('sc','*a2i*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

! ident_51="@(#) M_strings a2d(3fp) subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o works with any g-format input, including integer, real, and exponential.
!  o if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!    IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=unquote(chars)
   msg=''
   if(len(local_chars) == 0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd /= 0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         frmt='(Z'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         frmt='(B'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         frmt='(O'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr /= 0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(g3.3)')valu
      endif
      if(local_chars /= 'eod')then                           ! print warning message except for special value "eod"
         call journal('sc','*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg /= '')then
            call journal('sc','*a2d* - ['//trim(msg)//']')
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    s2v(3f) - [M_strings:TYPE] function returns doubleprecision
!!    numeric value from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function s2v(string[,ierr][,onerr])
!!
!!     character(len=*)             :: string
!!     doubleprecision              :: s2v
!!     integer,intent(out),optional :: ierr
!!     class(*),intent(in),optional :: onerr
!!
!!##DESCRIPTION
!!    This function converts a string to a DOUBLEPRECISION numeric value.
!!
!!    The intrinsics INT(3f), REAL(3f), and DBLE(3f) are also extended
!!    to take CHARACTER variables. The KIND= keyword is not supported
!!    on the extensions.
!!
!!##OPTIONS
!!
!!     string   holds string assumed to represent a numeric value
!!     ierr     If an error occurs the program is stopped if the optional
!!              parameter IERR is not present. If IERR returns a non-zero
!!              value an error occurred.
!!     onerr    The value to return on error. A value of NaN is
!!              returned on error by default.
!!
!!##RETURNS
!!     s2v      numeric value read from string
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_s2v
!!
!!     use M_strings, only: s2v, int, real, dble
!!     implicit none
!!     character(len=8)              :: s=' 10.345 '
!!     integer                       :: i
!!     character(len=14),allocatable :: strings(:)
!!     doubleprecision               :: dv
!!     integer                       :: errnum
!!
!!     ! different strings representing INTEGER, REAL, and DOUBLEPRECISION
!!     strings=[&
!!     &' 10.345       ',&
!!     &'+10           ',&
!!     &'    -3        ',&
!!     &'    -4.94e-2  ',&
!!     &'0.1           ',&
!!     &'12345.678910d0',&
!!     &'              ',& ! Note: will return zero without an error message
!!     &'1 2 1 2 1 . 0 ',& ! Note: spaces will be ignored
!!     &'WHAT?         ']  ! Note: error messages will appear, zero returned
!!
!!     ! a numeric value is returned,
!!     ! so it can be used in numeric expression
!!     write(*,*) '1/2 value of string is ',s2v(s)/2.0d0
!!     write(*,*)
!!     write(*,*)' STRING            VALUE                    ERROR_NUMBER'
!!     do i=1,size(strings)
!!        ! Note: not a good idea to use s2v(3f) in a WRITE(3f) statement,
!!        ! as it does I/O when errors occur, so called on a separate line
!!        dv=s2v(strings(i),errnum)
!!        write(*,*) strings(i)//'=',dv,errnum
!!     enddo
!!     write(*,*)"Extended intrinsics"
!!     write(*,*)'given inputs:',s,strings(:8)
!!     write(*,*)'INT(3f):',int(s),int(strings(:8))
!!     write(*,*)'REAL(3f):',real(s),real(strings(:8))
!!     write(*,*)'DBLE(3f):',dble(s),dble(strings(:8))
!!     write(*,*)"That's all folks!"
!!
!!     end program demo_s2v
!!
!!    Expected output
!!
!!     >1/2 value of string is    5.1725000000000003
!!     >
!!     > STRING            VALUE                    ERROR_NUMBER
!!     > 10.345       =   10.345000000000001                0
!!     >+10           =   10.000000000000000                0
!!     >    -3        =  -3.0000000000000000                0
!!     >    -4.94e-2  =  -4.9399999999999999E-002           0
!!     >0.1           =  0.10000000000000001                0
!!     >12345.678910d0=   12345.678910000001                0
!!     >              =   0.0000000000000000                0
!!     >1 2 1 2 1 . 0 =   12121.000000000000                0
!!     >*a2d* - cannot produce number from string [WHAT?]
!!     >*a2d* - [Bad value during floating point read]
!!     >WHAT?         =   0.0000000000000000             5010
!!     >Extended intrinsics
!!     >given inputs: 10.345 10.345 +10 -3 -4.94e-2 0.1
!!     12345.678910d0 1 2 1 2 1 . 0
!!     >INT(3f): 10 10 10 -3 0 0 12345 0 12121
!!     >REAL(3f): 10.3450003 10.3450003 10.0000000 -3.00000000
!!     -4.94000018E-02
!!     >          0.100000001 12345.6787 0.00000000 12121.0000
!!     >DBLE(3f): 10.345000000000001 10.345000000000001
!!     10.000000000000000
!!     >          -3.0000000000000000 -4.9399999999999999E-002
!!     0.10000000000000001
!!     >          12345.678910000001 0.0000000000000000
!!     12121.000000000000
!!     >That's all folks!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!>
!!##PROCEDURE:
!! DESCRIPTION: s2v(3f): function returns doubleprecision number from string;zero if error occurs
!!##VERSION:     2.0, 20160704
!! AUTHOR:      John S. Urban
doubleprecision function s2v(chars,ierr,onerr)
!  1989 John S. Urban

! ident_52="@(#) M_strings s2v(3f) returns doubleprecision number from string;zero if error occurs"

character(len=*),intent(in)  :: chars
integer,optional             :: ierr
doubleprecision              :: valu
integer                      :: ierr_local
class(*),intent(in),optional :: onerr

   ierr_local=0
   if(present(onerr))then
      call a2d(chars,valu,ierr_local,onerr)
   else
      call a2d(chars,valu,ierr_local)
   endif
   if(present(ierr))then ! if error is not returned stop program on error
      ierr=ierr_local
      s2v=valu
   elseif(ierr_local /= 0)then
      write(*,*)'*s2v* stopped while reading '//trim(chars)
      stop 1
   else
      s2v=valu
   endif
end function s2v
!===================================================================================================================================
! calls to s2v(3f) for extending intrinsics int(3f), real(3f), dble(3f)
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      dble(3f) - [M_strings:TYPE] overloads DBLE(3f) so it can handle character arguments
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    impure elemental function dble(string)
!!
!!     character(len=*) :: string
!!     integer          :: dble
!!
!!##DESCRIPTION
!!    dble(3f) returns a DOUBLE value when given a numeric representation of a
!!    numeric value. This overloads the DBLE(3f) intrinsic so that CHARACTER
!!    arguments assumed to represent a numeric value may be input.
!!
!!##OPTIONS
!!       STRING  input string to be converted to a dble value
!!
!!##RETURNS
!!    DBLE  double precision value represented by input string
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_dble
!!      use M_strings, only: dble
!!      implicit none
!!      write(*,*)dble('100'),dble('20.4')
!!      write(*,*)'dble still works',dble(20),dble(20.4)
!!      write(*,*)'elemental',&
!!      & dble([character(len=23) :: '10','20.3','20.5','20.6'])
!!      end program demo_dble
!!
!! Results:
!!
!!      >    100.00000000000000        20.399999999999999
!!      >  dble still works   20.000000000000000 20.399999618530273
!!      >  elemental   10.00000000000000  20.30000000000000
!!      >  20.50000000000000 20.60000000000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
impure elemental doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      real(3f) - [M_strings:TYPE] overloads REAL(3f) so it can handle character arguments
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    impure elemental function real(string)
!!
!!     character(len=*) :: string
!!     integer          :: real
!!
!!##DESCRIPTION
!!    real(3f) returns a REAL value when given a numeric representation of a
!!    numeric value. This overloads the REAL(3f) intrinsic so that CHARACTER
!!    arguments assumed to represent a numeric value may be input.
!!
!!##OPTIONS
!!       STRING  input string to be converted to a real value
!!
!!##RETURNS
!!       REAL  real value represented by input string
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_real
!!      use M_strings, only: real
!!      implicit none
!!      write(*,*)real('100'),real('20.4')
!!      write(*,*)'real still works',real(20)
!!      write(*,*)'elemental',&
!!      & real([character(len=23) :: '10','20.3','20.5','20.6'])
!!      end program demo_real
!!
!! Results:
!!
!!      >    100.000000       20.3999996
!!      >  real still works   20.0000000
!!      >  elemental   10.0000000  20.2999992  20.5000000  20.6000004
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
impure elemental real function real_s2v(chars)
character(len=*),intent(in) :: chars
   real_s2v=real(s2v(chars))
end function real_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      int(3f) - [M_strings:TYPE] overloads INT(3f) so it can handle character arguments
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    impure elemental function int(string)
!!
!!     character(len=*) :: string
!!     integer(kind=int32) :: int
!!
!!##DESCRIPTION
!!    int(3f) returns an integer when given a numeric representation of a
!!    numeric value. This overloads the INT(3f) intrinsic so that CHARACTER
!!    arguments assumed to represent a numeric value may be input.
!!
!!##OPTIONS
!!       STRING  input string to be converted to an INT32 integer
!!
!!##RETURNS
!!       INT  integer represented by input string
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_int
!!      use M_strings, only: int
!!      implicit none
!!      write(*,*)int('100'),int('20.4')
!!      write(*,*)'int still works',int(20.4)
!!      write(*,*)'elemental',&
!!      & int([character(len=23) :: '10','20.3','20.5','20.6'])
!!      end program demo_int
!!
!! Results:
!!
!!      >          100          20
!!      >  int still works          20
!!      >  elemental          10          20          20          20
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      nint(3f) - [M_strings:TYPE] overloads NINT(3f) so it can handle character arguments
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    impure elemental function nint(string)
!!
!!     character(len=*) :: string
!!     integer          :: nint
!!
!!##DESCRIPTION
!!    nint(3f) returns an integer when given a numeric representation of a
!!    numeric value. This overloads the NINT(3f) intrinsic so that CHARACTER
!!    arguments assumed to represent a numeric value may be input.
!!
!!##OPTIONS
!!       STRING  input string to be converted to an integer
!!
!!##RETURNS
!!       NINT  integer represented by input string
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_nint
!!      use M_strings, only: nint
!!      implicit none
!!      write(*,*)nint('100'),nint('20.4')
!!      write(*,*)'nint still works',nint(20.4)
!!      write(*,*)'elemental',&
!!      & nint([character(len=23) :: '10','20.3','20.5','20.6'])
!!      end program demo_nint
!!
!! Results:
!!
!!      >          100          20
!!      >  nint still works          20
!!      >  elemental          10          20          21          21
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
impure elemental integer function nint_s2v(chars)
character(len=*),intent(in) :: chars
   nint_s2v=nint(s2v(chars))
end function nint_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      value_to_string(3f) - [M_strings:TYPE] return numeric string
!!      from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine value_to_string(value,chars[,lgth,ierr,fmt,trimz])
!!
!!     character(len=*) :: chars  ! minimum of 23 characters required
!!     !--------
!!     ! VALUE may be any <em>one</em> of the following types:
!!     doubleprecision,intent(in)               :: value
!!     real,intent(in)                          :: value
!!     integer,intent(in)                       :: value
!!     logical,intent(in)                       :: value
!!     !--------
!!     character(len=*),intent(out)             :: chars
!!     integer,intent(out),optional             :: lgth
!!     integer,optional                         :: ierr
!!     character(len=*),intent(in),optional     :: fmt
!!     logical,intent(in)                       :: trimz
!!
!!##DESCRIPTION
!!    value_to_string(3f) returns a numeric representation of a numeric
!!    value in a string given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the string using internal writes. It
!!    then removes trailing zeros from non-zero values, and left-justifies
!!    the string.
!!
!!##OPTIONS
!!       VALUE   input value to be converted to a string
!!       FMT     You may specify a specific format that produces a string
!!               up to the length of CHARS; optional.
!!       TRIMZ   If a format is supplied the default is not to try to trim
!!               trailing zeros. Set TRIMZ to .true. to trim zeros from a
!!               string assumed to represent a simple numeric value.
!!
!!##RETURNS
!!       CHARS   returned string representing input value, must be at least
!!               23 characters long; or what is required by optional FMT
!!               if longer.
!!       LGTH    position of last non-blank character in returned string;
!!               optional.
!!       IERR    If not zero, error occurred; optional.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_value_to_string
!!      use M_strings, only: value_to_string
!!      implicit none
!!      character(len=80) :: string
!!      integer           :: lgth
!!         call value_to_string(3.0/4.0,string,lgth)
!!         write(*,*) 'The value is [',string(:lgth),']'
!!
!!         call value_to_string(3.0/4.0,string,lgth,fmt='')
!!         write(*,*) 'The value is [',string(:lgth),']'
!!
!!         call value_to_string&
!!         &(3.0/4.0,string,lgth,fmt='("THE VALUE IS ",g0)')
!!         write(*,*) 'The value is [',string(:lgth),']'
!!
!!         call value_to_string(1234,string,lgth)
!!         write(*,*) 'The value is [',string(:lgth),']'
!!
!!         call value_to_string(1.0d0/3.0d0,string,lgth)
!!         write(*,*) 'The value is [',string(:lgth),']'
!!
!!      end program demo_value_to_string
!!
!!    Expected output
!!
!!     The value is [0.75]
!!     The value is [      0.7500000000]
!!     The value is [THE VALUE IS .750000000]
!!     The value is [1234]
!!     The value is [0.33333333333333331]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

! ident_53="@(#) M_strings value_to_string(3fp) subroutine returns a string from a value"

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
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         call journal('*value_to_string* UNKNOWN TYPE')
         chars=' '
      end select
      if(fmt == '') then
         chars=adjustl(chars)
         call trimzeros_(chars)
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
      if(index(chars,'.') /= 0) call trimzeros_(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local /= 0)then
       ! cannot currently do I/O from a function being called from I/O
       !write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      v2s(3f) - [M_strings:TYPE] return numeric string from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function v2s(value) result(outstr)
!!
!!        integer|real|doubleprecision|logical,intent(in ) :: value
!!        character(len=:),allocatable :: outstr
!!        character(len=*),optional,intent(in) :: fmt
!!
!!##DESCRIPTION
!!    v2s(3f) returns a representation of a numeric value as a
!!    string when given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the strings using internal WRITE()
!!    statements. Trailing zeros are removed from non-zero values, and the
!!    string is left-justified.
!!
!!##OPTIONS
!!    VALUE   input value to be converted to a string
!!    FMT     format can be explicitly given, but is limited to
!!            generating a string of eighty or less characters.
!!
!!##RETURNS
!!    OUTSTR  returned string representing input value,
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_v2s
!!    use M_strings, only: v2s
!!    write(*,*) 'The value of 3.0/4.0 is ['//v2s(3.0/4.0)//']'
!!    write(*,*) 'The value of 1234    is ['//v2s(1234)//']'
!!    write(*,*) 'The value of 0d0     is ['//v2s(0d0)//']'
!!    write(*,*) 'The value of .false. is ['//v2s(.false.)//']'
!!    write(*,*) 'The value of .true. is  ['//v2s(.true.)//']'
!!    end program demo_v2s
!!
!!   Expected output
!!
!!     The value of 3.0/4.0 is [0.75]
!!     The value of 1234    is [1234]
!!     The value of 0d0     is [0]
!!     The value of .false. is [F]
!!     The value of .true. is  [T]
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function d2s(dvalue,fmt) result(outstr)

! ident_54="@(#) M_strings d2s(3fp) private function returns string given doubleprecision value"

doubleprecision,intent(in)   :: dvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(dvalue,string,fmt=fmt)
   else
      call value_to_string(dvalue,string)
   endif
   outstr=trim(string)
end function d2s
!===================================================================================================================================
function r2s(rvalue,fmt) result(outstr)

! ident_55="@(#) M_strings r2s(3fp) private function returns string given real value"

real,intent(in)              :: rvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(rvalue,string,fmt=fmt)
   else
      call value_to_string(rvalue,string)
   endif
   outstr=trim(string)
end function r2s
!===================================================================================================================================
function i2s(ivalue,fmt) result(outstr)

! ident_56="@(#) M_strings i2s(3fp) private function returns string given integer value"

integer,intent(in)           :: ivalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(ivalue,string,fmt=fmt)
   else
      call value_to_string(ivalue,string)
   endif
   outstr=trim(string)
end function i2s
!===================================================================================================================================
function l2s(lvalue,fmt) result(outstr)

! ident_57="@(#) M_strings l2s(3fp) private function returns string given logical value"

logical,intent(in)           :: lvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)             :: string
   if(present(fmt))then
      call value_to_string(lvalue,string,fmt=fmt)
   else
      call value_to_string(lvalue,string)
   endif
   outstr=trim(string)
end function l2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isnumber(3f) - [M_strings:TYPE] determine if a string represents a number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function isnumber(str,msg)
!!
!!     character(len=*),intent(in)  :: str
!!     character(len=:),intent(out),allocatable,optional  :: msg
!!
!!##DESCRIPTION
!!     ISNUMBER(3f) returns a value greater than zero if the string represents
!!     a number, and a number less than or equal to zero if it is a bad number.
!!     Blank characters are ignored.
!!
!!##OPTIONS
!!     str  the string to evaluate as to whether it represents a numeric value
!!          or not
!!     msg  An optional message describing the string
!!
!!##RETURNS
!!     isnumber  the following values are returned
!!
!!                1 for an integer             [-+]NNNNN
!!                2 for a whole number         [-+]NNNNN.
!!                3 for a real value           [-+]NNNNN.MMMM
!!                4 for a exponential value    [-+]NNNNN.MMMM[-+]LLLL
!!                                             [-+]NNNNN.MMMM[ed][-+]LLLL
!!
!!               values less than 1 represent an error
!!
!!##EXAMPLES
!!
!!   As the example shows, you can use an internal READ(3f) along with the
!!   IOSTAT= parameter to check (and read) a string as well.
!!
!!     program demo_isnumber
!!     use M_strings, only : isnumber
!!     implicit none
!!     character(len=256) :: line
!!     real               :: value
!!     integer            :: ios1, ios2
!!     integer            :: answer
!!     character(len=256) :: message
!!     character(len=:),allocatable :: description
!!        write(*,*)'Begin entering values, one per line'
!!        do
!!           read(*,'(a)',iostat=ios1)line
!!           !
!!           ! try string as number using list-directed input
!!           line=''
!!           read(line,*,iostat=ios2,iomsg=message) value
!!           if(ios2 == 0)then
!!              write(*,*)'VALUE=',value
!!           elseif( is_iostat_end(ios1) ) then
!!              stop 'end of file'
!!           else
!!              write(*,*)'ERROR:',ios2,trim(message)
!!           endif
!!           !
!!           ! try string using isnumber(3f)
!!           answer=isnumber(line,msg=description)
!!           if(answer > 0)then
!!              write(*,*) &
!!              & ' for ',trim(line),' ',answer,':',description
!!           else
!!              write(*,*) &
!!              & ' ERROR for ',trim(line),' ',answer,':',description
!!           endif
!!           !
!!        enddo
!!     end program demo_isnumber
!!
!!  Example run
!!
!!    > Begin entering values
!!    > ERROR:          -1 End of file
!!    >  ERROR for            -1 :null string
!!    >10
!!    > VALUE=   10.0000000
!!    >  for 10            1 :integer
!!    >20
!!    > VALUE=   20.0000000
!!    >  for 20            1 :integer
!!    >20.
!!    > VALUE=   20.0000000
!!    >  for 20.            2 :whole number
!!    >30.1
!!    > VALUE=   30.1000004
!!    >  for 30.1            3 :real number
!!    >3e1
!!    > VALUE=   30.0000000
!!    >  for 3e1            4 :value with exponent
!!    >1-2
!!    > VALUE=   9.99999978E-03
!!    >  for 1-2            4 :value with exponent
!!    >100.22d-4
!!    > VALUE=   1.00220004E-02
!!    >  for 100.22d-4            4 :value with exponent
!!    >1--2
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1--2           -5 :bad number
!!    >e
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for e           -6 :missing leading value before exponent
!!    >e1
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for e1           -6 :missing leading value before exponent
!!    >1e
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e           -3 :missing exponent
!!    >1e+
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e+           -4 :missing exponent after sign
!!    >1e+2.0
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e+2.0           -5 :bad number
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function isNumber(string,msg,verbose)

! ident_58="@(#) M_strings isnumber(3f) Determines if a string is a number of not."

character(len=*),intent(in)    :: string
character(len=:),intent(out),allocatable,optional :: msg
logical,intent(in),optional                      :: verbose
integer                      :: isnumber

integer             :: i,iend
character(len=1),allocatable :: z(:)
character(len=:),allocatable :: message
logical                      :: founddigit
logical                      :: verbose_local

   i=1
   founddigit=.false.
   isnumber=0
   z=switch(trim(nospace(string)))
   iend=size(z)
   message='not a number'
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
   DONE : block
      if(iend == 0)then
         isnumber=-1                   ! string is null
         message='null string'
         exit DONE
      endif

      if(index('+-',z(i)) /= 0) i=i+1  ! skip optional leading sign
      if(i > iend)then
         isnumber=-2                   ! string was just a sign
         message='just a sign'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1

      if(i > iend)then
         isnumber=1                    ! [+-]NNNNNN
         message='integer'
         exit DONE
      endif
      if(z(i) == '.')then              ! a period would be OK at this point
         i=i+1
      endif

      if(i > iend)then                ! [+-]NNNNNN.
         isnumber=2
         message='whole number'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1
      if(i > iend)then
         isnumber=3                    ! [+-]NNNNNN.MMMM
         message='real number'
         exit DONE
      endif

      if(index('eEdD',z(i)) /= 0)then
         i=i+1
         if(i == 2)then
            isnumber=-6                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
            message='missing leading value before exponent'
            exit DONE
         endif
      endif
      if(i > iend)then
         isnumber=-3                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
         message='missing exponent'
         exit DONE
      endif
      if(.not.founddigit)then
         isnumber=-7
         message='missing value before exponent'
         exit DONE
      endif
      if(index('+-',z(i)) /= 0) i=i+1
      if(i > iend)then
         isnumber=-4                   ! [+-]NNNNNN[.[MMMM]]e[+-] but a value must follow
         message='missing exponent after sign'
         exit DONE
      endif
      call next()                      ! position I to next non-digit or end of string+1
      if(i > iend)then
         isnumber=4                    ! [+-]NNNNNN.MMMMe[+-]LL
         message='value with exponent'
         exit DONE
      endif
      isnumber=-5
      message='bad number'
   endblock DONE
   if(verbose_local)then
      write(*,*)trim(string)//' is '//message
   endif
   if(present(msg))then
      msg=message
   endif

contains
   subroutine next() ! move to next non-digit or end of string+1
      integer :: j
      do j=i,iend
         if(.not.isdigit(z(j)))then
            exit
         endif
         founddigit=.true.
         if(verbose_local) write(*,*)'I=',i,' J=',j,' Z(j)=',z(j)
      enddo
      i=j
      if(verbose_local)then
         write(*,*)'I and J=',i
         if(i <= iend) then
            write(*,*)'Z(I)=',z(i)
         else
            write(*,*)'====>'
         endif
      endif
   end subroutine next
end function isNumber
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    trimzeros_(3fp) - [M_strings:TYPE] Delete trailing zeros from
!!    numeric decimal string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine trimzeros_(str)
!!
!!     character(len=*)  :: str
!!
!!##DESCRIPTION
!!    TRIMZEROS_(3f) deletes trailing zeros from a string representing a
!!    number. If the resulting string would end in a decimal point, one
!!    trailing zero is added.
!!
!!##OPTIONS
!!    str   input string will be assumed to be a numeric value and have
!!          trailing zeros removed
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_trimzeros_
!!       use M_strings, only : trimzeros_
!!       character(len=:),allocatable :: string
!!          write(*,*)trimzeros_('123.450000000000')
!!          write(*,*)trimzeros_('12345')
!!          write(*,*)trimzeros_('12345.')
!!          write(*,*)trimzeros_('12345.00e3')
!!       end program demo_trimzeros_
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine trimzeros_(string)

! ident_59="@(#) M_strings trimzeros_(3fp) Delete trailing zeros from numeric decimal string"

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
   if(index(str,'.') == 0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i <= 1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   enddo
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    listout(3f) - [M_strings:NUMERIC] expand a list of numbers where negative
!!    numbers denote range ends (1 -10 means 1 thru 10)
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine listout(icurve_lists,icurve_expanded,inums,ierr)
!!
!!    integer,intent(in)    :: icurve_lists(:)
!!    integer,intent(out)   :: icurve_expanded(:)
!!    integer,intent(out)   :: inums
!!    integer,intent(out)   :: ierr
!!
!!##DESCRIPTION
!!    expand a list of whole numbers where negative numbers indicate a range.
!!    So [10,-20] would be expanded to [10,11,12,13,14,15,16,17,18,19,20].
!!
!!##OPTIONS
!!    icurve_lists(:)      input array
!!
!!##RETURNS
!!    icurve_expanded(:)   output array; assumed large enough to hold
!!                         returned list
!!    inums                number of icurve_expanded numbers on output
!!    ierr                 zero if no error occurred
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_listout
!!     use M_strings, only : listout
!!     implicit none
!!     integer,allocatable :: icurve_lists(:)
!!     integer :: icurve_expanded(1000)
!!     ! icurve_lists is input array
!!     integer :: inums
!!     ! icurve_expanded is output array
!!     integer :: i
!!     ! number of icurve_lists values on input,
!!     ! number of icurve_expanded numbers on output
!!     integer :: ierr
!!        icurve_lists=[1, 20, -30, 101, 100, 99, 100, -120, 222, -200]
!!        inums=size(icurve_lists)
!!        call listout(icurve_lists,icurve_expanded,inums,ierr)
!!        if(ierr == 0)then
!!           write(*,'(i0)')(icurve_expanded(i),i=1,inums)
!!        else
!!           write(*,'(a,i0)')'error occurred in *listout* ',ierr
!!           write(*,'(i0)')(icurve_expanded(i),i=1,inums)
!!        endif
!!     end program demo_listout
!!
!! Results:
!!
!!     > 1 20 21 22 23
!!     > 24 25 26 27 28
!!     > 29 30 101 100 99
!!     > 100 101 102 103 104
!!     > 105 106 107 108 109
!!     > 110 111 112 113 114
!!     > 115 116 117 118 119
!!     > 120 222 221 220 219
!!     > 218 217 216 215 214
!!     > 213 212 211 210 209
!!     > 208 207 206 205 204
!!     > 203 202 201 200
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine listout(icurve_lists,icurve_expanded,inums_out,ierr)

! ident_60="@(#) M_strings listout(3f) copy icurve_lists to icurve_expanded expanding negative numbers to ranges (1 -10 means 1 thru 10)"

!   Created: 19971231
integer,intent(in)    :: icurve_lists(:)             ! input array
integer,intent(out)   :: icurve_expanded(:)          ! output array
integer,intent(out)   :: inums_out                   ! number of icurve_expanded numbers on output
integer,intent(out)   :: ierr                        ! status variable

character(len=80)     :: temp1
integer               :: i80, i90
integer               :: imin, imax
integer               :: idirection, icount
integer               :: iin
integer               :: inums_max

   ierr=0
   icurve_expanded=0                          ! initialize output array
   inums_out=0                                ! initialize number of significant values in output array

   inums_max=size(icurve_expanded)
   if(inums_max == 0)then
      ierr=-2
      return
   endif

   iin=size(icurve_lists)
   if(iin > 0)then
      icurve_expanded(1)=icurve_lists(1)
   endif

   icount=2
      do i90=2,iin
         if(icurve_lists(i90) < 0)then
            imax=abs(icurve_lists(i90))
            imin=abs(icurve_lists(i90-1))
            if(imin > imax)then
               idirection=-1
               imin=imin-1
            elseif(imax > imin)then
               idirection=1
               imin=imin+1
            else
               idirection=1
            endif
            do i80=imin,imax,idirection
               if(icount > inums_max) then
                  write(temp1,'(a,i5,a)')'*listout* only ',inums_max,' values allowed'
                  ierr=-1
                  call journal(temp1)
                  inums_out=icount-1
                  exit
               endif
               icurve_expanded(icount)=i80
               icount=icount+1
            enddo
         else
            icurve_expanded(icount)=icurve_lists(i90)
            icount=icount+1
         endif
      enddo
   inums_out=icount-1

end subroutine listout
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     quote(3f) - [M_strings:QUOTES] add quotes to string as if written
!!     with list-directed input
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function quote(str,mode,clip) result (quoted_str)
!!
!!    character(len=*),intent(in)          :: str
!!    character(len=*),optional,intent(in) :: mode
!!    logical,optional,intent(in)          :: clip
!!    character(len=:),allocatable         :: quoted_str
!!
!!##DESCRIPTION
!!    Add quotes to a CHARACTER variable as if it was written using
!!    list-directed input. This is particularly useful for processing
!!    strings to add to CSV files.
!!
!!##OPTIONS
!!    str    input string to add quotes to, using the rules of
!!           list-directed input (single quotes are replaced by two
!!           adjacent quotes)
!!    mode   alternate quoting methods are supported:
!!
!!             DOUBLE   default. replace quote with double quotes
!!             ESCAPE   replace quotes with backslash-quote instead of
!!                      double quotes
!!
!!    clip   default is to trim leading and trailing spaces from the
!!           string. If CLIP is .FALSE. spaces are not trimmed
!!
!!##RESULT
!!    quoted_str  The output string, which is based on adding quotes to STR.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_quote
!!    use M_strings, only : quote
!!    implicit none
!!    character(len=:),allocatable :: str
!!    character(len=1024)          :: msg
!!    integer                      :: ios
!!    character(len=80)            :: inline
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)inline
!!          if(ios /= 0)then
!!             write(*,*)trim(inline)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'ORIGINAL     ['//trim(inline)//']'
!!
!!          ! the string processed by quote(3f)
!!          str=quote(inline)
!!          write(*,'(a)')'QUOTED     ['//str//']'
!!
!!          ! write the string list-directed to compare the results
!!          write(*,'(a)',iostat=ios,iomsg=msg) 'LIST DIRECTED:'
!!          write(*,*,iostat=ios,iomsg=msg,delim='none') inline
!!          write(*,*,iostat=ios,iomsg=msg,delim='quote') inline
!!          write(*,*,iostat=ios,iomsg=msg,delim='apostrophe') inline
!!       enddo
!!    end program demo_quote
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function quote(str,mode,clip) result (quoted_str)
character(len=*),intent(in)          :: str                ! the string to be quoted
character(len=*),optional,intent(in) :: mode
logical,optional,intent(in)          :: clip
character(len=:),allocatable         :: quoted_str

character(len=1),parameter           :: double_quote = '"'
character(len=20)                    :: local_mode

   if(present(clip))then
      if(clip)then
         quoted_str=adjustl(str)
      else
         quoted_str=str
      endif
   else
      quoted_str=str
   endif

   local_mode=merge_str(mode,'DOUBLE',present(mode))

   select case(lower(local_mode))
   case('double')
      quoted_str=double_quote//trim(replace(quoted_str,'"','""'))//double_quote
   case('escape')
      quoted_str=double_quote//trim(replace(quoted_str,'"','\"'))//double_quote
   case default
      call journal('sc','*quote* ERROR: unknown quote mode ',local_mode)
      quoted_str=str
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
end function quote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     unquote(3f) - [M_strings:QUOTES] remove quotes from string as if
!!     read with list-directed input
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function unquote(quoted_str,esc) result (unquoted_str)
!!
!!    character(len=*),intent(in)          :: quoted_str
!!    character(len=1),optional,intent(in) :: esc
!!    character(len=:),allocatable         :: unquoted_str
!!
!!##DESCRIPTION
!!    Remove quotes from a CHARACTER variable as if it was read using
!!    list-directed input. This is particularly useful for processing
!!    tokens read from input such as CSV files.
!!
!!    Fortran can now read using list-directed input from an internal file,
!!    which should handle quoted strings, but list-directed input does not
!!    support escape characters, which UNQUOTE(3f) does.
!!
!!##OPTIONS
!!    quoted_str  input string to remove quotes from, using the rules of
!!                list-directed input (two adjacent quotes inside a quoted
!!                region are replaced by a single quote, a single quote or
!!                double quote is selected as the delimiter based on which
!!                is encountered first going from left to right, ...)
!!    esc         optional character used to protect the next quote
!!                character from being processed as a quote, but simply as
!!                a plain character.
!!
!!##RESULT
!!    unquoted_str  The output string, which is based on removing quotes
!!                  from quoted_str.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_unquote
!!       use M_strings, only : unquote
!!       implicit none
!!       character(len=128)           :: quoted_str
!!       character(len=:),allocatable :: unquoted_str
!!       character(len=1),parameter   :: esc='\'
!!       character(len=1024)          :: msg
!!       integer                      :: ios
!!       character(len=1024)          :: dummy
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)quoted_str
!!          if(ios /= 0)then
!!             write(*,*)trim(msg)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'QUOTED       ['//trim(quoted_str)//']'
!!
!!          ! the string processed by unquote(3f)
!!          unquoted_str=unquote(trim(quoted_str),esc)
!!          write(*,'(a)')'UNQUOTED     ['//unquoted_str//']'
!!
!!          ! read the string list-directed to compare the results
!!          read(quoted_str,*,iostat=ios,iomsg=msg)dummy
!!          if(ios /= 0)then
!!             write(*,*)trim(msg)
!!          else
!!             write(*,'(a)')'LIST DIRECTED['//trim(dummy)//']'
!!          endif
!!       enddo
!!    end program demo_unquote
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function unquote(quoted_str,esc) result (unquoted_str)
character(len=*),intent(in)          :: quoted_str              ! the string to be unquoted
character(len=1),optional,intent(in) :: esc                     ! escape character
character(len=:),allocatable         :: unquoted_str
integer                              :: inlen
character(len=1),parameter           :: single_quote = "'"
character(len=1),parameter           :: double_quote = '"'
integer                              :: quote                   ! whichever quote is to be used
integer                              :: before
integer                              :: current
integer                              :: iesc
integer                              :: iput
integer                              :: i
logical                              :: inside
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(esc))then                           ! select escape character as specified character or special value meaning not set
      iesc=iachar(esc)                            ! allow for an escape character
   else
      iesc=-1                                     ! set to value that matches no character
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   inlen=len(quoted_str)                          ! find length of input string
   allocate(character(len=inlen) :: unquoted_str) ! initially make output string length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(inlen >= 1)then                             ! double_quote is the default quote unless the first character is single_quote
      if(quoted_str(1:1) == single_quote)then
         quote=iachar(single_quote)
      else
         quote=iachar(double_quote)
      endif
   else
      quote=iachar(double_quote)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   before=-2                                      ! initially set previous character to impossible value
   unquoted_str(:)=''                             ! initialize output string to null string
   iput=1
   inside=.false.
   STEPTHROUGH: do i=1,inlen
      current=iachar(quoted_str(i:i))
      if(before == iesc)then                      ! if previous character was escape use current character unconditionally
           iput=iput-1                            ! backup
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2                              ! this could be second esc or quote
      elseif(current == quote)then                ! if current is a quote it depends on whether previous character was a quote
         if(before == quote)then
           unquoted_str(iput:iput)=char(quote)    ! this is second quote so retain it
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before /= iesc)then
            inside=.true.
         else                                     ! this is first quote so ignore it except remember it in case next is a quote
            before=current
         endif
      else
         unquoted_str(iput:iput)=char(current)
         iput=iput+1
         before=current
      endif
   enddo STEPTHROUGH
!-----------------------------------------------------------------------------------------------------------------------------------
   unquoted_str=unquoted_str(:iput-1)
!-----------------------------------------------------------------------------------------------------------------------------------
end function unquote
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    edit_distance(3f) - [M_strings:DESCRIBE] returns a naive edit distance using
!!    the Levenshtein distance algorithm
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure elemental function edit_distance(str1,str2) result (distance)
!!
!!     character(len=*),intent(in)   :: str1, str2
!!     integer :: distance
!!
!!##DESCRIPTION
!!
!!   The Levenshtein distance function returns how many edits (deletions,
!!   insertions, transposition) are required to turn one string into another.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_edit_distance
!!    use M_strings, only : edit_distance
!!       write(*,*)edit_distance('kittens','sitting')==3
!!       write(*,*)edit_distance('geek','gesek')==1
!!       write(*,*)edit_distance('Saturday','Sunday')==3
!!    end program demo_edit_distance
!!
!!   Expected output
!!
!!     T
!!     T
!!     T
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
! The Levenshtein distance function returns how many edits (deletions,
! insertions, transposition) are required to turn one string into another.

pure elemental integer function edit_distance (a,b)
character(len=*), intent(in) :: a, b
integer                      :: len_a, len_b, i, j, cost
! matrix for calculating Levenshtein distance
!integer                      :: matrix(0:len_trim(a), 0:len_trim(b)) ! not supported by all compilers yet
integer,allocatable          :: matrix(:,:)
   len_a = len_trim(a)
   len_b = len_trim(b)
   !-------------------------------------- ! required by older compilers instead of above declaration
   if(allocated(matrix))deallocate(matrix)
   allocate(matrix(0:len_a,0:len_b))
   !--------------------------------------
   matrix(:,0) = [(i,i=0,len_a)]
   matrix(0,:) = [(j,j=0,len_b)]
   do i = 1, len_a
      do j = 1, len_b
         cost=merge(0,1,a(i:i)==b(j:j))
         matrix(i,j) = min(matrix(i-1,j)+1, matrix(i,j-1)+1, matrix(i-1,j-1)+cost)
      enddo
   enddo
   edit_distance = matrix(len_a,len_b)
end function edit_distance
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    bundle(3f) - [M_strings:ARRAY] return up to twenty strings of arbitrary length
!!                 as an array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function bundle(str1,str2,...str20,len) result (vec)
!!
!!     character(len=*),intent(in),optional   :: str1, str2 ... str20
!!     integer,intent(in),optional            :: len
!!
!!##DESCRIPTION
!!    Given a list of up to twenty strings create a string array. The
!!    length of the variables with be the same as the maximum length
!!    of the input strings unless explicitly specified via LEN.
!!
!!    This is an alternative to the syntax
!!
!!      [ CHARACTER(LEN=NN) :: str1, str2, ... ]
!!
!!    that by default additionally calculates the minimum length required
!!    to prevent truncation.
!!
!!##OPTIONS
!!    str1,str2, ... str20  input strings to combine into a vector
!!    len   length of returned array variables
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_bundle
!!    use M_strings, only: bundle
!!    implicit none
!!       print "(*('""',a,'""':,',',1x))", bundle("one")
!!       print "(*('""',a,'""':,',',1x))", bundle("one","two")
!!       print "(*('""',a,'""':,',',1x))", bundle("one","two","three")
!!       print "(*('""',a,'""':,',',1x))", bundle("one","two","three",&
!!               & "four","five","six","seven")
!!    end program demo_bundle
!!
!!   Expected output
!!
!!    "one"
!!    "one", "two"
!!    "one  ", "two  ", "three"
!!    "one  ", "two  ", "three", "four ", "five ", "six  ", "seven"
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function bundle(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,len) result(vec)
! return character array containing present arguments
character(len=*),intent(in),optional  :: x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
character(len=*),intent(in),optional  :: x11,x12,x13,x14,x15,x16,x17,x18,x19,x20
integer,intent(in),optional           :: len
character(len=:),allocatable          :: vec(:)
integer                               :: ilen, icount, iset
   ilen=0
   icount=0
   iset=0
   call increment(x1)
   call increment(x2)
   call increment(x3)
   call increment(x4)
   call increment(x5)
   call increment(x6)
   call increment(x7)
   call increment(x8)
   call increment(x9)
   call increment(x10)
   call increment(x11)
   call increment(x12)
   call increment(x13)
   call increment(x14)
   call increment(x15)
   call increment(x16)
   call increment(x17)
   call increment(x18)
   call increment(x19)
   call increment(x20)

   if(present(len)) ilen=len
   allocate (character(len=ilen) ::vec(icount))

   call set(x1)
   call set(x2)
   call set(x3)
   call set(x4)
   call set(x5)
   call set(x6)
   call set(x7)
   call set(x8)
   call set(x9)
   call set(x10)
   call set(x11)
   call set(x12)
   call set(x13)
   call set(x14)
   call set(x15)
   call set(x16)
   call set(x17)
   call set(x18)
   call set(x19)
   call set(x20)

contains

subroutine increment(str)
character(len=*),intent(in),optional :: str
   if(present(str))then
      ilen=max(ilen,len_trim(str))
      icount=icount+1
   endif
end subroutine increment

subroutine set(str)
character(len=*),intent(in),optional :: str
   if(present(str))then
      iset=iset+1
      vec(iset)=str
   endif
end subroutine set

end function bundle
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    describe(3f) - [M_strings:DESCRIBE] returns a string describing the name of
!!    a single character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function describe(ch) result (string)
!!
!!     character(len=1),intent(in)   :: ch
!!     character(len=:),allocatable  :: string
!!
!!##DESCRIPTION
!!    describe(3f) returns a string describing long name of a single
!!    character
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_describe
!!     use M_strings, only : describe
!!     implicit none
!!     integer :: i
!!        do i=1,128  ! fill variable with base ASCII character set
!!           write(*,*)describe(char(i-1))
!!        enddo
!!    end program demo_describe
!!
!!   Expected output
!!
!!     ctrl-@ or ctrl-? (NUL) null
!!     ctrl-A (SOH) start of heading
!!     ctrl-B (STX) start of text
!!     ctrl-C (ETX) end of text
!!     ctrl-D (EOT) end of transmission
!!     ctrl-E (ENQ) enquiry
!!     ctrl-F (ACK) acknowledge
!!     ctrl-G (BEL) bell
!!     ctrl-H (BS) backspace
!!     ctrl-I (HT) horizontal tabulation
!!     ctrl-J (LF) line feed
!!     ctrl-K (VT) vertical tabulation
!!     ctrl-L (FF) form feed
!!     ctrl-M (CR) carriage return
!!     ctrl-N (SO) shift out
!!     ctrl-O (SI) shift in
!!     ctrl-P (DLE) data link escape
!!     ctrl-Q (DC1) device control 1
!!     ctrl-R (DC2) device control 2
!!     ctrl-S (DC3) device control 3
!!     ctrl-T (DC4) device control 4
!!     ctrl-U (NAK) negative acknowledge
!!     ctrl-V (SYN) synchronous idle
!!     ctrl-W (ETB) end of transmission block
!!     ctrl-X (CAN) cancel
!!     ctrl-Y (EM) end of medium
!!     ctrl-Z (SUB) substitute
!!     ctrl-[ (ESC) escape
!!     ctrl-\ or ctrl-@ (FS) file separator
!!     ctrl-] (GS) group separator
!!     ctrl-^ or ctrl-= (RS) record separator
!!     ctrl-_ (US) unit separator
!!     space
!!     ! exclamation point
!!     " quotation marks
!!     # number sign
!!     $ currency symbol
!!     % percent
!!     & ampersand
!!     ' apostrophe
!!     ( left parenthesis
!!     ) right parenthesis
!!     * asterisk
!!     + plus
!!     , comma
!!     - minus
!!     . period
!!     / slash
!!     0 zero
!!     1 one
!!     2 two
!!     3 three
!!     4 four
!!     5 five
!!     6 six
!!     7 seven
!!     8 eight
!!     9 nine
!!     : colon
!!     ; semicolon
!!     < less than
!!     = equals
!!     > greater than
!!     ? question mark
!!     @ at sign
!!     majuscule A
!!     majuscule B
!!     majuscule C
!!     majuscule D
!!     majuscule E
!!     majuscule F
!!     majuscule G
!!     majuscule H
!!     majuscule I
!!     majuscule J
!!     majuscule K
!!     majuscule L
!!     majuscule M
!!     majuscule N
!!     majuscule O
!!     majuscule P
!!     majuscule Q
!!     majuscule R
!!     majuscule S
!!     majuscule T
!!     majuscule U
!!     majuscule V
!!     majuscule W
!!     majuscule X
!!     majuscule Y
!!     majuscule Z
!!     [ left bracket
!!     \ backslash
!!     ] right bracket
!!     ^ caret
!!     _ underscore
!!     ` grave accent
!!     miniscule a
!!     miniscule b
!!     miniscule c
!!     miniscule d
!!     miniscule e
!!     miniscule f
!!     miniscule g
!!     miniscule h
!!     miniscule i
!!     miniscule j
!!     miniscule k
!!     miniscule l
!!     miniscule m
!!     miniscule n
!!     miniscule o
!!     miniscule p
!!     miniscule q
!!     miniscule r
!!     miniscule s
!!     miniscule t
!!     miniscule u
!!     miniscule v
!!     miniscule w
!!     miniscule x
!!     miniscule y
!!     miniscule z
!!     { left brace
!!     | vertical line
!!     } right brace
!!     ~ tilde
!!     ctrl-? (DEL) delete
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function describe(ch) result (string)

! ident_61="@(#) M_strings describe(3f) return string describing long name of a single character"

character(len=1),intent(in)   :: ch
character(len=:),allocatable  :: string
! LATER: add hex, octal, decimal, key-press description, alternate names
!  ASCII character codes
   select case (iachar(ch))
   case(     0  ); STRING="ctrl-@ or ctrl-? (NUL) null"
   case(     1  ); STRING="ctrl-A (SOH) start of heading"
   case(     2  ); STRING="ctrl-B (STX) start of text"
   case(     3  ); STRING="ctrl-C (ETX) end of text"
   case(     4  ); STRING="ctrl-D (EOT) end of transmission"
   case(     5  ); STRING="ctrl-E (ENQ) enquiry"
   case(     6  ); STRING="ctrl-F (ACK) acknowledge"
   case(     7  ); STRING="ctrl-G (BEL) bell"
   case(     8  ); STRING="ctrl-H (BS) backspace"
   case(     9  ); STRING="ctrl-I (HT) horizontal tabulation"
   case(    10  ); STRING="ctrl-J (LF) line feed"
   case(    11  ); STRING="ctrl-K (VT) vertical tabulation"
   case(    12  ); STRING="ctrl-L (FF) form feed"
   case(    13  ); STRING="ctrl-M (CR) carriage return"
   case(    14  ); STRING="ctrl-N (SO) shift out"
   case(    15  ); STRING="ctrl-O (SI) shift in"
   case(    16  ); STRING="ctrl-P (DLE) data link escape"
   case(    17  ); STRING="ctrl-Q (DC1) device control 1"
   case(    18  ); STRING="ctrl-R (DC2) device control 2"
   case(    19  ); STRING="ctrl-S (DC3) device control 3"
   case(    20  ); STRING="ctrl-T (DC4) device control 4"
   case(    21  ); STRING="ctrl-U (NAK) negative acknowledge"
   case(    22  ); STRING="ctrl-V (SYN) synchronous idle"
   case(    23  ); STRING="ctrl-W (ETB) end of transmission block"
   case(    24  ); STRING="ctrl-X (CAN) cancel"
   case(    25  ); STRING="ctrl-Y (EM) end of medium"
   case(    26  ); STRING="ctrl-Z (SUB) substitute"
   case(    27  ); STRING="ctrl-[ (ESC) escape"
   case(    28  ); STRING="ctrl-\ or ctrl-@ (FS) file separator"
   case(    29  ); STRING="ctrl-] (GS) group separator"
   case(    30  ); STRING="ctrl-^ or ctrl-= (RS) record separator"
   case(    31  ); STRING="ctrl-_ (US) unit separator"
   case(    32  ); STRING="space"
   case(    33  ); STRING="! exclamation point (screamer, gasper, slammer, startler, bang, shriek, pling)"
   case(    34  ); STRING=""" quotation marks"
   case(    35  ); STRING="# number sign (hash, pound sign, hashtag)"
   case(    36  ); STRING="$ currency symbol"
   case(    37  ); STRING="% percent"
   case(    38  ); STRING="& ampersand"
   case(    39  ); STRING="' apostrophe"
   case(    40  ); STRING="( left parenthesis"
   case(    41  ); STRING=") right parenthesis"
   case(    42  ); STRING="* asterisk"
   case(    43  ); STRING="+ plus"
   case(    44  ); STRING=", comma"
   case(    45  ); STRING="- minus"
   case(    46  ); STRING=". period"
   case(    47  ); STRING="/ slash"
   case(    48  ); STRING="0 zero"
   case(    49  ); STRING="1 one"
   case(    50  ); STRING="2 two"
   case(    51  ); STRING="3 three"
   case(    52  ); STRING="4 four"
   case(    53  ); STRING="5 five"
   case(    54  ); STRING="6 six"
   case(    55  ); STRING="7 seven"
   case(    56  ); STRING="8 eight"
   case(    57  ); STRING="9 nine"
   case(    58  ); STRING=": colon"
   case(    59  ); STRING="; semicolon"
   case(    60  ); STRING="< less than"
   case(    61  ); STRING="= equals"
   case(    62  ); STRING="> greater than"
   case(    63  ); STRING="? question mark"
   case(    64  ); STRING="@ at sign"
   case(    65  ); STRING="A majuscule A"
   case(    66  ); STRING="B majuscule B"
   case(    67  ); STRING="C majuscule C"
   case(    68  ); STRING="D majuscule D"
   case(    69  ); STRING="E majuscule E"
   case(    70  ); STRING="F majuscule F"
   case(    71  ); STRING="G majuscule G"
   case(    72  ); STRING="H majuscule H"
   case(    73  ); STRING="I majuscule I"
   case(    74  ); STRING="J majuscule J"
   case(    75  ); STRING="K majuscule K"
   case(    76  ); STRING="L majuscule L"
   case(    77  ); STRING="M majuscule M"
   case(    78  ); STRING="N majuscule N"
   case(    79  ); STRING="O majuscule O"
   case(    80  ); STRING="P majuscule P"
   case(    81  ); STRING="Q majuscule Q"
   case(    82  ); STRING="R majuscule R"
   case(    83  ); STRING="S majuscule S"
   case(    84  ); STRING="T majuscule T"
   case(    85  ); STRING="U majuscule U"
   case(    86  ); STRING="V majuscule V"
   case(    87  ); STRING="W majuscule W"
   case(    88  ); STRING="X majuscule X"
   case(    89  ); STRING="Y majuscule Y"
   case(    90  ); STRING="Z majuscule Z"
   case(    91  ); STRING="[ left bracket"
   case(    92  ); STRING="\ backslash"
   case(    93  ); STRING="] right bracket"
   case(    94  ); STRING="^ caret"
   case(    95  ); STRING="_ underscore"
   case(    96  ); STRING="` grave accent"
   case(    97  ); STRING="a miniscule a"
   case(    98  ); STRING="b miniscule b"
   case(    99  ); STRING="c miniscule c"
   case(   100  ); STRING="d miniscule d"
   case(   101  ); STRING="e miniscule e"
   case(   102  ); STRING="f miniscule f"
   case(   103  ); STRING="g miniscule g"
   case(   104  ); STRING="h miniscule h"
   case(   105  ); STRING="i miniscule i"
   case(   106  ); STRING="j miniscule j"
   case(   107  ); STRING="k miniscule k"
   case(   108  ); STRING="l miniscule l"
   case(   109  ); STRING="m miniscule m"
   case(   110  ); STRING="n miniscule n"
   case(   111  ); STRING="o miniscule o"
   case(   112  ); STRING="p miniscule p"
   case(   113  ); STRING="q miniscule q"
   case(   114  ); STRING="r miniscule r"
   case(   115  ); STRING="s miniscule s"
   case(   116  ); STRING="t miniscule t"
   case(   117  ); STRING="u miniscule u"
   case(   118  ); STRING="v miniscule v"
   case(   119  ); STRING="w miniscule w"
   case(   120  ); STRING="x miniscule x"
   case(   121  ); STRING="y miniscule y"
   case(   122  ); STRING="z miniscule z"
   case(   123  ); STRING="{ left brace"
   case(   124  ); STRING="| vertical line"
   case(   125  ); STRING="} right brace"
   case(   126  ); STRING="~ tilde"
   case(   127  ); STRING="ctrl-? (DEL) delete"
   case default
         STRING='UNKNOWN'//v2s(IACHAR(ch))
   end select
end function describe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getvals(3f) - [M_strings:TYPE] read arbitrary number of REAL values
!!    from a character variable up to size of VALUES() array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine getvals(line,values,icount,ierr)
!!
!!     character(len=*),intent(in)  :: line
!!     class(*),intent(out)         :: values(:)
!!     integer,intent(out)          :: icount
!!     integer,intent(out),optional :: ierr
!!
!!##DESCRIPTION
!!   GETVALS(3f) reads a relatively arbitrary number of numeric values from
!!   a character variable into a REAL array using list-directed input.
!!
!!   NOTE: In this version null values are skipped instead of meaning to leave
!!         that value unchanged
!!
!!        1,,,,,,,2 / reads VALUES=[1.0,2.0]
!!
!!   Per list-directed rules when reading values, allowed delimiters are
!!   comma, semi-colon and space.
!!
!!   the slash separator can be used to add inline comments.
!!
!!        10.1, 20.43e-1 ; 11 / THIS IS TREATED AS A COMMENT
!!
!!   Repeat syntax can be used up to the size of the output array. These are
!!   equivalent input lines:
!!
!!        4*10.0
!!        10.0, 10.0, 10.0, 10.0
!!
!!##OPTIONS
!!   LINE      A character variable containing the characters representing
!!             a list of numbers
!!
!!##RETURNS
!!   VALUES()  array holding numbers read from string. May be of type
!!             INTEGER, REAL, DOUBLEPRECISION, or CHARACTER. If CHARACTER the
!!             strings are returned as simple words instead of numeric values.
!!   ICOUNT    number of defined numbers in VALUES(). If ICOUNT reaches
!!             the size of the VALUES() array parsing stops.
!!   IERR      zero if no error occurred in reading numbers. Optional.
!!             If not present and an error occurs the program is terminated.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!       program demo_getvals
!!       use M_strings, only: getvals
!!       implicit none
!!       integer,parameter  :: longest_line=256
!!       character(len=longest_line) :: line
!!       real               :: values(longest_line/2+1)
!!       integer            :: ios,icount,ierr
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios) line
!!          if(ios /= 0)exit INFINITE
!!          call getvals(line,values,icount,ierr)
!!          write(*,'(4(g0,1x))')'VALUES=',values(:icount)
!!       enddo INFINITE
!!       end program demo_getvals
!!
!!  Sample input lines
!!
!!        10,20 30.4
!!        1 2 3
!!        1
!!
!!        3 4*2.5 8
!!        32.3333 / comment 1
!!        30e3;300,    30.0, 3
!!        even 1 like this! 10
!!        11,,,,22,,,,33
!!
!!  Expected output:
!!
!!     VALUES=   10.0000000       20.0000000       30.3999996
!!     VALUES=   1.00000000       2.00000000       3.00000000
!!     VALUES=   1.00000000
!!     VALUES=
!!     VALUES=   3.00000000       2.50000000       2.50000000
!!     2.50000000       2.50000000       8.00000000
!!     VALUES=   32.3333015
!!     VALUES=   30000.0000       300.000000       30.0000000
!!     3.00000000
!!     *getvals* WARNING:[even] is not a number
!!     *getvals* WARNING:[like] is not a number
!!     *getvals* WARNING:[this!] is not a number
!!     VALUES=   1.00000000       10.0000000
!!     VALUES=   11.0000000       22.0000000       33.0000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine getvals(line,values,icount,ierr)

! ident_62="@(#) M_strings getvals(3f) read arbitrary number of values from a character variable"

! JSU 20170831

character(len=*),intent(in)  :: line
class(*),intent(out)         :: values(:)
integer,intent(out)          :: icount
integer,intent(out),optional :: ierr

character(len=:),allocatable :: buffer
character(len=len(line))     :: words(size(values))
integer                      :: ios, i, ierr_local,isize

   isize=0
   select type(values)
   type is (integer);          isize=size(values)
   type is (real);             isize=size(values)
   type is (doubleprecision);  isize=size(values)
   type is (character(len=*)); isize=size(values)
   end select

   ierr_local=0

   words=' '                            ! make sure words() is initialized to null+blanks
   buffer=trim(unquote(line))//"/"      ! add a slash to the end so how the read behaves with missing values is clearly defined
   read(buffer,*,iostat=ios) words      ! undelimited strings are read into an array
   icount=0
   do i=1,isize                         ! loop thru array and convert non-blank words to numbers
      if(words(i) == ' ')cycle

      select type(values)
      type is (integer);          read(words(i),*,iostat=ios)values(icount+1)
      type is (real);             read(words(i),*,iostat=ios)values(icount+1)
      type is (doubleprecision);  read(words(i),*,iostat=ios)values(icount+1)
      type is (character(len=*)); values(icount+1)=words(i)
      end select

      if(ios == 0)then
         icount=icount+1
      else
         ierr_local=ios
         write(ERROR_UNIT,*)'*getvals* WARNING:['//trim(words(i))//'] is not a number of specified type'
      endif
   enddo

   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local /= 0)then        ! error occurred and not returning error to main program to print message and stop program
      write(ERROR_UNIT,*)'*getval* error reading line ['//trim(line)//']'
      stop 2
   endif

end subroutine getvals
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_values(3f) - [M_strings:TYPE] read a string representing
!!      numbers into a numeric array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine string_to_values(line,iread,values,inums,delims,ierr)
!!
!!        character(len=*) :: line
!!        integer          :: iread
!!        real             :: values(*)
!!        integer          :: inums
!!        character(len=*) :: delims
!!        integer          :: ierr
!!
!!##DESCRIPTION
!!    This routine can take a string representing a series of numbers and
!!    convert it to a numeric array and return how many numbers were found.
!!
!!##OPTIONS
!!       LINE     Input string containing numbers
!!       IREAD    maximum number of values to try to read from input string
!!
!!##RESULTS
!!       VALUES   real array to be filled with numbers
!!       INUMS    number of values successfully read (before error occurs
!!                if one does)
!!       DELIMS   delimiter character(s), usually a space. must not be a
!!                null string. If more than one character, a space must
!!                not be the last character or it will be ignored.
!!       IERR     error flag (0=no error, else column number string starts
!!                at that error occurred on).
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!      program demo_string_to_values
!!       use M_strings, only : string_to_values
!!       implicit none
!!       character(len=80)  :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!       integer,parameter  :: isz=10
!!       real               :: array(isz)
!!       integer            :: inums, ierr, ii
!!
!!       call string_to_values(s,10,array,inums,' ;',ierr)
!!       call reportit()
!!
!!       call string_to_values('10;2.3;3.1416',isz,array,inums,' ;',ierr)
!!       call reportit()
!!
!!       contains
!!          subroutine reportit()
!!             write(*,*)'string_to_values:'
!!             write(*,*)'input string.............',trim(s)
!!             write(*,*)'number of values found...',inums
!!             write(*,*)'values...................',(array(ii),ii=1,inums)
!!          end subroutine reportit
!!      end program demo_string_to_values
!!
!!    Expected output
!!
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           6
!!     values...................   10.0000000  20000.0000  3.45000005
!!     -4.00299978  1234.00000  5678.00000
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           3
!!     values...................   10.0000000  2.29999995  3.14159989
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine string_to_values(line,iread,values,inums,delims,ierr)
!----------------------------------------------------------------------------------------------------------------------------------
!   1989,1997-12-31,2014 John S. Urban

!   given a line of structure , string , string , string process each
!   string as a numeric value and store into an array.
!   DELIMS contain the legal delimiters. If a space is an allowed delimiter, it must not appear last in DELIMS.
!   There is no direct checking for more values than can fit in VALUES.
!   Quits if encounters any errors in read.
!----------------------------------------------------------------------------------------------------------------------------------

! ident_63="@(#) M_strings string_to_values(3f) reads an array of numbers from a numeric string"

character(len=*),intent(in)  :: line          ! input string
integer,intent(in)           :: iread         ! maximum number of values to try to read into values
real,intent(inout)           :: values(iread) ! real array to be filled with values
integer,intent(out)          :: inums         ! number of values successfully read from string
character(len=*),intent(in)  :: delims        ! allowed delimiters
integer,intent(out)          :: ierr          ! 0 if no error, else column number undecipherable string starts at
!----------------------------------------------------------------------------------------------------------------------------------
character(len=256)           :: delims_local        ! mutable copy of allowed delimiters
integer                      :: istart,iend,lgth,icol
integer                      :: i10,i20,i40
real                         :: rval
integer                      :: ier
integer                      :: delimiters_length
!----------------------------------------------------------------------------------------------------------------------------------
      delims_local=delims                                 ! need a mutable copy of the delimiter list
      if(delims_local == '')then                          ! if delimiter list is null or all spaces make it a space
         delims_local=' '                                 ! delimiter is a single space
         delimiters_length=1                        ! length of delimiter list
      else
         delimiters_length=len_trim(delims)         ! length of variable WITH TRAILING WHITESPACE TRIMMED
      endif
!----------------------------------------------------------------------------------------------------------------------------------
      ierr=0                                        ! initialize error code returned
      inums=0                                       ! initialize count of values successfully returned
      istart=0
!----------------------------------------------------------------------------------------------------------------------------------
      lgth=0                                        ! lgth will be the position of the right-most non-delimiter in the input line
      do i20=len(line),1,-1                         ! loop from end of string to beginning to find right-most non-delimiter
         if(index(delims_local(:delimiters_length),line(i20:i20)) == 0)then   ! found a non-delimiter
            lgth=i20
            exit
         endif
      enddo
      if(lgth == 0)then                             ! command was totally composed of delimiters
         call journal('*string_to_values* blank line passed as a list of numbers')
         return
      endif
!----------------------------------------------------------------------------------------------------------------------------------
!     there is at least one non-delimiter sub-string
!     lgth is the column position of the last non-delimiter character
!     now, starting at beginning of string find next non-delimiter
      icol=1                                                     ! pointer to beginning of unprocessed part of LINE
      LOOP: dO i10=1,iread,1                                     ! each pass should find a value
         if(icol > lgth) EXIT LOOP                              ! everything is done
         INFINITE: do
            if(index(delims_local(:delimiters_length),line(icol:icol)) == 0)then           ! found non-delimiter
               istart=icol
               iend=0                                            ! FIND END OF SUBSTRING
               do i40=istart,lgth                                ! look at each character starting at left
                  if(index(delims_local(:delimiters_length),line(i40:i40)) /= 0)then       ! determine if character is a delimiter
                     iend=i40                                    ! found a delimiter. record where it was found
                     EXIT                                        ! found end of substring so leave loop
                  endif
               enddo
              if(iend == 0)iend=lgth+1                           ! no delimiters found, so this substring goes to end of line
               iend=iend-1                                       ! do not want to pass delimiter to be converted
               rval=0.0
               call string_to_value(line(istart:iend),rval,ier)  ! call procedure to convert string to a numeric value
               if(ier == 0)then                                  ! a substring was successfully converted to a numeric value
                  values(i10)=rval                               ! store numeric value in return array
                  inums=inums+1                                  ! increment number of values converted to a numeric value
               else                                              ! an error occurred converting string to value
                  ierr=istart                                    ! return starting position of substring that could not be converted
                  return
               endif
               icol=iend+2                                       ! set to next character to look at
               CYCLE LOOP                                        ! start looking for next value
            else                                                 ! this is a delimiter so keep looking for start of next string
               icol=icol+1                                       ! increment pointer into LINE
               CYCLE INFINITE
            endif
         enddo INFINITE
      enddo LOOP
!     error >>>>> more than iread numbers were in the line.
end subroutine string_to_values
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2vs(3f) - [M_strings:TYPE] given a string representing numbers
!!      return a numeric array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function s2vs(line[,delim])
!!
!!        character(len=*) :: line
!!        doubleprecision,allocatable :: s2vs(:)
!!
!!##DESCRIPTION
!!    The function S2VS(3f) takes a string representing a series of numbers
!!    and converts it to a numeric doubleprecision array. The string values
!!    may be delimited by spaces, semi-colons, and commas by default.
!!
!!##OPTIONS
!!       LINE   Input string containing numbers
!!       DELIM  optional list of delimiter characters. If a space is
!!              included, it should appear as the left-most character
!!              in the list. The default is " ;," (spaces, semi-colons,
!!              and commas).
!!
!!##RESULTS
!!       S2VS   doubleprecision array
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!      program demo_s2vs
!!      use M_strings, only : s2vs
!!      implicit none
!!      character(len=80) :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!      real,allocatable :: values(:)
!!      integer,allocatable :: ivalues(:)
!!      integer :: ii
!!
!!      values=s2vs(s)
!!      ivalues=int(s2vs(s))
!!      call reportit()
!!
!!      contains
!!        subroutine reportit()
!!          write(*,*)'S2VS:'
!!          write(*,*)'input string.............',&
!!           & trim(s)
!!          write(*,*)'number of values found...',&
!!           & size(values)
!!          write(*,*)'values...................',&
!!           & (values(ii),ii=1,size(values))
!!          write(*,'(*(g0,1x))')'ivalues..................',&
!!           & (ivalues(ii),ii=1,size(values))
!!        end subroutine reportit
!!      end program demo_s2vs
!!
!!   Expected output
!!
!!     S2VS:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           6
!!     values...................   10.0000000 20000.0000 3.45000005
!!     -4.00299978 1234.00000 5678.00000
!!    ivalues.................. 10 20000 3 -4 1234 5678
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function s2vs(string,delim) result(darray)

! ident_64="@(#) M_strings s2vs(3f) function returns array of values from a string"

character(len=*),intent(in)        :: string                       ! keyword to retrieve value for from dictionary
character(len=*),optional          :: delim                        ! delimiter characters
character(len=:),allocatable       :: delim_local
doubleprecision,allocatable        :: darray(:)                    ! function type

character(len=:),allocatable       :: carray(:)                    ! convert value to an array using split(3f)
integer                            :: i
integer                            :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(delim))then
      delim_local=delim
   else
      delim_local=' ;,'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(string,carray,delimiters=delim_local)         ! split string into an array
   allocate(darray(size(carray)))                           ! create the output array
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier)       ! convert the string to a numeric value
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function s2vs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isprint(3f) - [M_strings:COMPARE] returns .true. if character is an
!!     ASCII printable character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isprint(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isprint
!!
!!##DESCRIPTION
!!     isprint(3f) returns .true. if character is an ASCII printable character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isprint  logical value returns true if character is a
!!             printable ASCII character else false.
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_isprint
!!    use M_strings, only : isprint
!!    implicit none
!!    integer                    :: i
!!    character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!       write(*,'(40(a))')'ISPRINT: ',pack( string, isprint(string) )
!!    end program demo_isprint
!!
!!   Results:
!!
!!    ISPRINT:  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEF
!!    GHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmn
!!    opqrstuvwxyz{|}~
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isprint(onechar)

! ident_65="@(#) M_strings isprint(3f) indicates if input character is a printable ASCII character"

character,intent(in) :: onechar
logical              :: isprint
   select case (onechar)
      case (' ':'~')   ; isprint=.TRUE.
      case default     ; isprint=.FALSE.
   end select
end function isprint
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isgraph(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     printable character except a space is considered non-printable
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isgraph(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isgraph
!!
!!##DESCRIPTION
!!    isgraph(3f) returns .true. if character is a printable character
!!    except a space is considered non-printable
!!
!!##OPTIONS
!!    onechar   character to test
!!
!!##RETURNS
!!    isgraph   logical value returns true if character is a printable
!!              non-space character
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_isgraph
!!    use M_strings, only : isgraph
!!    implicit none
!!    integer                    :: i
!!    character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!       write(*,'(40(a))')'ISGRAPH: ',pack( string, isgraph(string) )
!!    end program demo_isgraph
!!
!!   Results:
!!
!!    ISGRAPH: !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFG
!!    HIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmno
!!    pqrstuvwxyz{|}~
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isgraph(onechar)

! ident_66="@(#) M_strings isgraph(3f) indicates if character is printable ASCII character excluding space"

character,intent(in) :: onechar
logical              :: isgraph
   select case (iachar(onechar))
   case (33:126)
     isgraph=.TRUE.
   case default
     isgraph=.FALSE.
   end select
end function isgraph
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isalpha(3f) - [M_strings:COMPARE] returns .true. if character is a
!!    letter and .false. otherwise
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!   elemental function isalpha(onechar)
!!
!!    character,intent(in) :: onechar
!!    logical              :: isalpha
!!
!!##DESCRIPTION
!!    isalpha(3f) returns .true. if character is a letter and
!!    .false. otherwise
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isalpha  logical value returns .true. if character is a ASCII letter
!!             or false otherwise.
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!     program demo_isalpha
!!     use M_strings, only : isalpha
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(40(a))')'ISGRAPH: ',pack( string, isalpha(string) )
!!     end program demo_isalpha
!!
!!   Results:
!!
!!    ISGRAPH: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklm
!!    nopqrstuvwxyz
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
elemental function isalpha(ch) result(res)

! ident_67="@(#) M_strings isalpha(3f) Return .true. if character is a letter and .false. otherwise"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'Z','a':'z')
     res=.true.
   case default
     res=.false.
   end select
end function isalpha
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isxdigit(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     hexadecimal digit (0-9, a-f, or A-F).
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isxdigit(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isxdigit
!!
!!##DESCRIPTION
!!     isxdigit(3f) returns .true. if character is a hexadecimal digit (0-9,
!!     a-f, or A-F).
!!
!!##OPTIONS
!!    onechar   character to test
!!
!!##RETURNS
!!    isxdigit  logical value returns true if character is a hexadecimal digit
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program demo_isxdigit
!!     use M_strings, only : isxdigit
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(40(a))')'ISXDIGIT: ',pack( string, isxdigit(string) )
!!     end program demo_isxdigit
!!
!!   Results:
!!
!!    ISXDIGIT: 0123456789ABCDEFabcdef
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isxdigit(ch) result(res)

! ident_68="@(#) M_strings isxdigit(3f) returns .true. if c is a hexadecimal digit (0-9 a-f or A-F)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'F','a':'f','0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isxdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isdigit(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     digit (0,1,...,9) and .false. otherwise
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isdigit(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isdigit
!!
!!##DESCRIPTION
!!     isdigit(3f) returns .true. if character is a digit (0,1,...,9)
!!     and .false. otherwise
!!
!!##EXAMPLES
!!
!!
!!  Sample Program:
!!
!!     program demo_isdigit
!!     use M_strings, only : isdigit, isspace, switch
!!     implicit none
!!     character(len=10),allocatable :: string(:)
!!     integer                       :: i
!!        string=[&
!!        & '1 2 3 4 5 ' ,&
!!        & 'letters   ' ,&
!!        & '1234567890' ,&
!!        & 'both 8787 ' ]
!!        ! if string is nothing but digits and whitespace return .true.
!!        do i=1,size(string)
!!           write(*,'(a)',advance='no')'For string['//string(i)//']'
!!           write(*,*) &
!!            & all(isdigit(switch(string(i))).or.&
!!            & isspace(switch(string(i))))
!!        enddo
!!     end program demo_isdigit
!!
!!  Expected output:
!!
!!        For string[1 2 3 4 5 ] T
!!        For string[letters   ] F
!!        For string[1234567890] T
!!        For string[both 8787 ] F
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isdigit(ch) result(res)

! ident_69="@(#) M_strings isdigit(3f) Returns .true. if ch is a digit (0-9) and .false. otherwise"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isblank(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     blank character (space or horizontal tab).
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isblank(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isblank
!!
!!##DESCRIPTION
!!     isblank(3f) returns .true. if character is a blank character (space
!!     or horizontal tab).
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isblank  logical value returns true if character is a "blank"
!!             ( an ASCII  space or horizontal tab character).
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_isblank
!!     use M_strings, only : isblank
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(*(g0,1x))')'ISXBLANK: ',&
!!        & iachar(pack( string, isblank(string) ))
!!     end program demo_isblank
!!
!!   Results:
!!
!!    ISXBLANK:  9 32
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isblank(ch) result(res)

! ident_70="@(#) M_strings isblank(3f) returns .true. if character is a blank (space or horizontal tab)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ',char(9))
     res=.true.
   case default
     res=.false.
   end select
end function isblank
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isascii(3f) - [M_strings:COMPARE] returns .true. if the character is
!!     in the range char(0) to char(256)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isascii(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isascii
!!
!!##DESCRIPTION
!!     isascii(3f) returns .true. if the character is in the range char(0)
!!     to char(127)
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isupper  logical value returns true if character is an ASCII
!!             character.
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_isascii
!!     use M_strings, only : isascii
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,255)]
!!        write(*,'(10(g0,1x))')'ISASCII: ', &
!!        & iachar(pack( string, isascii(string) ))
!!     end program demo_isascii
!!
!!  Results:
!!
!!    ISASCII:  0 1 2 3 4 5 6 7 8
!!    9 10 11 12 13 14 15 16 17 18
!!    19 20 21 22 23 24 25 26 27 28
!!    29 30 31 32 33 34 35 36 37 38
!!    39 40 41 42 43 44 45 46 47 48
!!    49 50 51 52 53 54 55 56 57 58
!!    59 60 61 62 63 64 65 66 67 68
!!    69 70 71 72 73 74 75 76 77 78
!!    79 80 81 82 83 84 85 86 87 88
!!    89 90 91 92 93 94 95 96 97 98
!!    99 100 101 102 103 104 105 106 107 108
!!    109 110 111 112 113 114 115 116 117 118
!!    119 120 121 122 123 124 125 126 127
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isascii(ch) result(res)

! ident_71="@(#) M_strings isascii(3f) returns .true. if character is in the range char(0) to char(127)"

character,intent(in) :: ch
logical              :: res
   select case(iachar(ch))
   case(0:127)
     res=.true.
   case default
     res=.false.
   end select
end function isascii
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isspace(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     null, space, tab, carriage return, new line, vertical tab, or formfeed
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isspace(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isspace
!!
!!##DESCRIPTION
!!     isspace(3f) returns .true. if character is a null, space, tab,
!!     carriage return, new line, vertical tab, or formfeed
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    isspace  returns true if character is ASCII white space
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_isspace
!!     use M_strings, only : isspace
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISSPACE: ', &
!!        & iachar(pack( string, isspace(string) ))
!!     end program demo_isspace
!!
!!   Results:
!!
!!    ISSPACE:  0 9 10 11 12 13 32
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function isspace(ch) result(res)

! ident_72="@(#) M_strings isspace(3f) true if null space tab return new line vertical tab or formfeed"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ')                 ! space(32)
     res=.true.
   case(char(0))             ! null(0)
     res=.true.
   case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13),
     res=.true.
   case default
     res=.false.
   end select
end function isspace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     iscntrl(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     delete character or ordinary control character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function iscntrl(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: iscntrl
!!
!!##DESCRIPTION
!!     iscntrl(3f) returns .true. if character is a delete character or
!!     ordinary control character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    iscntrl  logical value returns true if character is a control character
!!
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_iscntrl
!!     use M_strings, only : iscntrl
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISCNTRL: ', &
!!        & iachar(pack( string, iscntrl(string) ))
!!     end program demo_iscntrl
!!
!!   Results:
!!
!!    ISCNTRL:  0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
!!    20 21 22 23 24 25 26 27 28 29 30 31 127
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function iscntrl(ch) result(res)

! ident_73="@(#) M_strings iscntrl(3f) true if a delete or ordinary control character(0x7F or 0x00-0x1F)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(char(127),char(0):char(31))
     res=.true.
   case default
     res=.false.
   end select
end function iscntrl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     ispunct(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     printable punctuation character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function ispunct(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: ispunct
!!
!!##DESCRIPTION
!!     ispunct(3f) returns .true. if character is a printable punctuation
!!     character
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    ispunct  logical value returns true if character is a printable
!!             punctuation character.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_ispunct
!!     use M_strings, only : ispunct
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(20(g0,1x))')'ISPUNCT: ', &
!!        & iachar(pack( string, ispunct(string) ))
!!        write(*,'(20(g0,1x))')'ISPUNCT: ', &
!!        & pack( string, ispunct(string) )
!!     end program demo_ispunct
!!   Results:
!!
!!    ISPUNCT:  33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 58 59 60 61
!!    62 63 64 91 92 93 94 95 96 123 124 125 126
!!    ISPUNCT:  ! " # $ % & ' ( ) * + , - . / : ; < =
!!    > ? @ [ \ ] ^ _ ` { | } ~
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function ispunct(ch) result(res)

! ident_74="@(#) M_strings ispunct(3f) true if a printable punctuation character (isgraph(c)&&!isalnum(c))"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case (char(33):char(47), char(58):char(64), char(91):char(96), char(123):char(126))
     res=.true.
!  case(' ','0':'9','A':'Z','a':'z',char(128):)
!    res=.true.
!  case(char(0):char(31),char(127))
!    res=.true.
   case default
     res=.false.
   end select
end function ispunct
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     fortran_name(3f) - [M_strings:COMPARE] test if string meets criteria
!!     for being a fortran name
!!
!!##SYNOPSIS
!!
!!
!!     elemental function fortran_name(line) result (lout)
!!
!!      character(len=*),intent(in)  :: line
!!      logical                      :: lout
!!
!!##DESCRIPTION
!!     Determines if a string is an allowed Fortran name. To pass the input
!!     string must be composed of 1 to 63 ASCII characters and start with a
!!     letter and be composed entirely of alphanumeric characters [a-zA-Z0-9]
!!     and underscores.
!!
!!##OPTIONS
!!     LINE   input string to test. Leading spaces are significant but
!!            trailing spaces are ignored.
!!
!!##RETURNS
!!     LOUT   a logical value indicating if the input string passed or failed
!!            the test to see if it is a valid Fortran name or not.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!      program demo_fortran_name
!!      use M_strings, only : fortran_name
!!      implicit none
!!      character(len=20),parameter :: names(*)=[character(len=20) ::  &
!!       & '_name',         'long_variable_name', 'name_',         &
!!       & '12L',           'a__b__c  ',          'PropertyOfGas', &
!!       & '3%3',           '$NAME',              ' ',             &
!!       & 'Variable-name', 'A',                  'x@x' ]
!!      integer :: i
!!         write(*,'(i3,1x,a20,1x,l1)')&
!!         & (i,names(i),fortran_name(names(i)),i=1,size(names))
!!      end program demo_fortran_name
!!
!!    Results:
!!
!!      >  1 _name                F
!!      >  2 long_variable_name   T
!!      >  3 name_                T
!!      >  4 12L                  F
!!      >  5 a__b__c              T
!!      >  6 PropertyOfGas        T
!!      >  7 3%3                  F
!!      >  8 $NAME                F
!!      >  9                      F
!!      > 10 Variable-name        F
!!      > 11 A                    T
!!      > 12 x@x                  F
elemental function fortran_name(line) result (lout)

! ident_75="@(#) M_strings fortran_name(3f) Return .true. if name is a valid Fortran name"

! determine if a string is a valid Fortran name ignoring trailing spaces (but not leading spaces)
character(len=*),parameter   :: int='0123456789'
character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*),parameter   :: allowed=upper//lower//int//'_'

character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   name=trim(line)
   if(len(name) /= 0)then
      lout = verify(name(1:1), lower//upper) == 0  &
       & .and. verify(name,allowed) == 0           &
       & .and. len(name) <= 63
   else
      lout = .false.
   endif
end function fortran_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     isupper(3f) - [M_strings:COMPARE] returns .true. if character is an
!!     uppercase letter (A-Z)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function isupper(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: isupper
!!
!!##DESCRIPTION
!!     isupper(3f) returns .true. if character is an uppercase letter (A-Z)
!!
!!##OPTIONS
!!    onechar  character to test
!!##RETURNS
!!    isupper  logical value returns true if character is an uppercase
!!             ASCII character else false.
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_isupper
!!     use M_strings, only : isupper
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(10(g0,1x))')'ISUPPER: ', &
!!        & iachar(pack( string, isupper(string) ))
!!        write(*,'(10(g0,1x))')'ISUPPER: ', &
!!        & pack( string, isupper(string) )
!!     end program demo_isupper
!!
!!  Results:
!!
!!    ISUPPER:  65 66 67 68 69 70 71 72 73
!!    74 75 76 77 78 79 80 81 82 83
!!    84 85 86 87 88 89 90
!!    ISUPPER:  A B C D E F G H I
!!    J K L M N O P Q R S
!!    T U V W X Y Z
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
pure elemental function isupper(ch) result(res)

! ident_76="@(#) M_strings isupper(3f) returns true if character is an uppercase letter (A-Z)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'Z'); res=.true.
   case default;  res=.false.
   end select
end function isupper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     islower(3f) - [M_strings:COMPARE] returns .true. if character is a
!!     miniscule letter (a-z)
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    elemental function islower(onechar)
!!
!!     character,intent(in) :: onechar
!!     logical              :: islower
!!
!!##DESCRIPTION
!!     islower(3f) returns .true. if character is a miniscule letter (a-z)
!!
!!##OPTIONS
!!    onechar  character to test
!!
!!##RETURNS
!!    islower  logical value returns true if character is a lowercase
!!             ASCII character else false.
!!##EXAMPLE
!!
!!  Sample program
!!
!!     program demo_islower
!!     use M_strings, only : islower
!!     implicit none
!!     integer                    :: i
!!     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]
!!        write(*,'(15(g0,1x))')'ISLOWER: ', &
!!        & iachar(pack( string, islower(string) ))
!!        write(*,'(15(g0,1x))')'ISLOWER: ', &
!!        & pack( string, islower(string) )
!!     end program demo_islower
!!   Results:
!!
!!    ISLOWER:  97 98 99 100 101 102 103 104 105 106 107 108 109 110
!!    111 112 113 114 115 116 117 118 119 120 121 122
!!    ISLOWER:  a b c d e f g h i j k l m n
!!    o p q r s t u v w x y z
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     Public Domain
elemental function islower(ch) result(res)

! ident_77="@(#) M_strings islower(3f) returns true if character is a miniscule letter (a-z)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('a':'z'); res=.true.
   case default;  res=.false.
   end select
end function islower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isalnum,isalpha,iscntrl,isdigit,isgraph,islower,
!!    isprint,ispunct,isspace,isupper,
!!    isascii,isblank,isxdigit(3f) - [M_strings:COMPARE] test membership in
!!    subsets of ASCII set
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Where "FUNCNAME" is one of the function names in the group, the
!!    functions are defined by
!!
!!     elemental function FUNCNAME(onechar)
!!     character,intent(in) :: onechar
!!     logical              :: FUNC_NAME
!!##DESCRIPTION
!!
!!       These elemental functions test if a character belongs to various
!!       subsets of the ASCII character set.
!!
!!       isalnum    returns .true. if character is a letter (a-z,A-Z)
!!                  or digit (0-9)
!!       isalpha    returns .true. if character is a letter and
!!                  .false. otherwise
!!       isascii    returns .true. if character is in the range char(0)
!!                  to char(127)
!!       isblank    returns .true. if character is a blank (space or
!!                  horizontal tab).
!!       iscntrl    returns .true. if character is a delete character or
!!                  ordinary control character (0x7F or 0x00-0x1F).
!!       isdigit    returns .true. if character is a digit (0,1,...,9)
!!                  and .false. otherwise
!!       isgraph    returns .true. if character is a printable ASCII
!!                  character excluding space
!!       islower    returns .true. if character is a miniscule letter (a-z)
!!       isprint    returns .true. if character is a printable ASCII character
!!       ispunct    returns .true. if character is a printable punctuation
!!                  character (isgraph(c) && !isalnum(c)).
!!       isspace    returns .true. if character is a null, space, tab,
!!                  carriage return, new line, vertical tab, or formfeed
!!       isupper    returns .true. if character is an uppercase letter (A-Z)
!!       isxdigit   returns .true. if character is a hexadecimal digit
!!                  (0-9, a-f, or A-F).
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_isdigit
!!
!!     use M_strings, only : isdigit, isspace, switch
!!     implicit none
!!     character(len=10),allocatable :: string(:)
!!     integer                       :: i
!!        string=[&
!!        & '1 2 3 4 5 ' ,&
!!        & 'letters   ' ,&
!!        & '1234567890' ,&
!!        & 'both 8787 ' ]
!!        ! if string is nothing but digits and whitespace return .true.
!!        do i=1,size(string)
!!           write(*,'(a)',advance='no')'For string['//string(i)//']'
!!           write(*,*) &
!!           all(isdigit(switch(string(i))) .or. &
!!           & isspace(switch(string(i))))
!!        enddo
!!
!!     end program demo_isdigit
!!
!!   Expected output:
!!
!!    For string[1 2 3 4 5 ] T
!!    For string[letters   ] F
!!    For string[1234567890] T
!!    For string[both 8787 ] F
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function isalnum(ch) result(res)

! ident_78="@(#) M_strings isalnum(3f) returns true if character is a letter (a-z A-Z) or digit(0-9)"

character,intent(in)       :: ch
logical                    :: res
   select case(ch)
   case('a':'z','A':'Z','0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isalnum
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    base(3f) - [M_strings:BASE] convert whole number string in base [2-36]
!!    to string in alternate base [2-36]
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function base(x,b,y,a)
!!
!!    character(len=*),intent(in)  :: x
!!    character(len=*),intent(out) :: y
!!    integer,intent(in)           :: b,a
!!##DESCRIPTION
!!
!!    Convert a numeric string from base B to base A. The function returns
!!    FALSE if B is not in the range [2..36] or if string X contains invalid
!!    characters in base B or if result Y is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in a base > 10.
!!
!!##OPTIONS
!!    x   input string representing numeric whole value
!!    b   assumed base of input string
!!    y   output string
!!    a   base specified for output string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_base
!!    use M_strings, only : base
!!    implicit none
!!    integer           :: ba,bd
!!    character(len=40) :: x,y
!!
!!    print *,' BASE CONVERSION'
!!    write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!    write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!    INFINITE: do
!!       write(*,'("Enter number in start base (0 to quit): ")',advance='no')
!!       read *, x
!!       if(x == '0') exit INFINITE
!!       if(base(x,bd,y,ba))then
!!            write(*,'("In base ",I2,": ",A20)')  ba, y
!!        else
!!          print *,'Error in decoding/encoding number.'
!!        endif
!!     enddo INFINITE
!!
!!     end program demo_base
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
logical function base(x,b,y,a)
character(len=*),intent(in)  :: x
character(len=*),intent(out) :: y
integer,intent(in)           :: b,a
integer                      :: temp

! ident_79="@(#) M_strings base(3f) convert whole number string in base [2-36] to string in alternate base [2-36]"

base=.true.
if(decodebase(x,b,temp)) then
   if(codebase(temp,a,y)) then
   else
      print *,'Error in coding number.'
      base=.false.
   endif
else
   print *,'Error in decoding number.'
   base=.false.
endif

end function base
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    base2(3f) - [M_strings:BASE] convert whole number to string in base 2
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function base2(int)
!!
!!     integer,intent(in)           :: int
!!     character(len=:),allocatable :: base2
!!
!!##DESCRIPTION
!!
!!    Convert a whole number to a string in base 2.
!!
!!    This is often done with the B edit descriptor and
!!    an internal WRITE() statement, but is done without
!!    calling the I/O routines, and as a function.
!!
!!##OPTIONS
!!    int   input string representing numeric whole value
!!##RETURNS
!!    base2   string representing input value in base 2
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_base2
!!      use M_strings, only : base2
!!      implicit none
!!         write(*,'(a)') base2(huge(0))
!!         write(*,'(a)') base2(0)
!!         write(*,'(a)') base2(64)
!!         write(*,'(a)') base2(-64)
!!         write(*,'(a)') base2(-huge(0)-1)
!!      end program demo_base2
!! Results:
!!
!!     > 1111111111111111111111111111111
!!     > 0
!!     > 1000000
!!     > 11111111111111111111111111000000
!!     > 10000000000000000000000000000000
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
! 0 in binary: 0
! 42 in binary: 101010
! huge(int) in binary: 1111111111111111111111111111111
! 032 in binary is 100000
! itimes=10000000
!      G_TRICK=base2_f(32)   <BASE2_F  >Processor Time =  0.766 seconds.
!      G_TRICK=base2_fdo(32) <BASE2_FDO>Processor Time =  0.958 seconds.
!      G_TRICK=base2_a(32)   <BASE2_A  >Processor Time =  1.022 seconds.
!      G_TRICK=base2_c(32)   <BASE2_C  >Processor Time =  7.208 seconds.
!      G_TRICK=empty(32)     <EMPTY    >Processor Time =  0.132 seconds.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2(x) result(str)
!  return string representing number as a binary number.  Fixed-length string:
integer, intent(in)                        :: x
integer                                    :: i
character(len=max(1,bit_size(x)-leadz(x))) :: str
    associate(n => len(str))
      str = repeat('0',n)
      do i = 0,n-1
        if (btest(x,i)) str(n-i:n-i) = '1'
      end do
    end associate
end function base2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_fdo(x) result(str)
!  return string representing number as a binary number.  Fixed-length string: do concurrent
integer, intent(in) :: x
character(len=max(1,bit_size(x)-leadz(x))) :: str

integer :: n, i

    if (x == 0) then
      str(1:1) = '0'
      return
    endif
    n = len(str)
    str = repeat('0',n)
    do concurrent (i = 0:n-1, btest(x,i))
      str(n-i:n-i) = '1'
    end do
end function base2_fdo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_a(x) result(str)
!  return string representing number as a binary number. Allocatable-length string:
integer, intent(in) :: x
character(len=:), allocatable :: str

integer :: n, i

    n = max(1,bit_size(x)-leadz(x))
    allocate(character(len=n) :: str)
    if (x == 0) then
      str(1:1) = '0'
      return
    endif

    str = repeat('0',n)
    do concurrent (i = 0:n-1, btest(x,i))
      str(n-i:n-i) = '1'
    end do
end function base2_a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function base2_c(x) result(str)
! internal write
integer, intent(in) :: x
character(len=max(1,bit_size(x)-leadz(x))) :: str
    write( str, fmt="(b0)" ) x
end function base2_c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    decodebase(3f) - [M_strings:BASE] convert whole number string in base
!!    [2-36] to base 10 number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function decodebase(string,basein,out10)
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in)           :: basein
!!    integer,intent(out)          :: out10
!!##DESCRIPTION
!!
!!    Convert a numeric string representing a whole number in base BASEIN
!!    to base 10. The function returns FALSE if BASEIN is not in the range
!!    [2..36] or if string STRING contains invalid characters in base BASEIN
!!    or if result OUT10 is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    string   input string. It represents a whole number in
!!             the base specified by BASEIN unless BASEIN is set
!!             to zero. When BASEIN is zero STRING is assumed to
!!             be of the form BASE#VALUE where BASE represents
!!             the function normally provided by BASEIN.
!!    basein   base of input string; either 0 or from 2 to 36.
!!    out10    output value in base 10
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_decodebase
!!    use M_strings, only : codebase, decodebase
!!    implicit none
!!    integer           :: ba,bd
!!    character(len=40) :: x,y
!!    integer           :: r
!!
!!    print *,' BASE CONVERSION'
!!    write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!    write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!    INFINITE: do
!!       print *,''
!!       write(*,'("Enter number in start base: ")',advance='no'); read *, x
!!       if(x == '0') exit INFINITE
!!       if(decodebase(x,bd,r)) then
!!          if(codebase(r,ba,y)) then
!!            write(*,'("In base ",I2,": ",A20)')  ba, y
!!          else
!!            print *,'Error in coding number.'
!!          endif
!!       else
!!          print *,'Error in decoding number.'
!!       endif
!!    enddo INFINITE
!!
!!    end program demo_decodebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!       Ref.: "Math matiques en Turbo-Pascal by
!!              M. Ducamp and A. Reverchon (2),
!!              Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function decodebase(string,basein,out_baseten)

! ident_80="@(#) M_strings decodebase(3f) convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein == 0.and.ipound > 1)then                                  ! split string into two values
     call string_to_value(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local >= 0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        if(ch == '-'.and.k == 1)then
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    codebase(3f) - [M_strings:BASE] convert whole number in base 10 to
!!    string in base [2-36]
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function codebase(in_base10,out_base,answer)
!!
!!    integer,intent(in)           :: in_base10
!!    integer,intent(in)           :: out_base
!!    character(len=*),intent(out) :: answer
!!
!!##DESCRIPTION
!!    Convert a number from base 10 to base OUT_BASE. The function returns
!!    .FALSE. if OUT_BASE is not in [2..36] or if number IN_BASE10 is
!!    too big.
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_codebase
!!    use M_strings, only : codebase
!!    implicit none
!!    character(len=20) :: answer
!!    integer           :: i, j
!!    logical           :: ierr
!!    do j=1,100
!!       do i=2,36
!!          ierr=codebase(j,i,answer)
!!          write(*,*)'VALUE=',j,' BASE=',i,' ANSWER=',answer
!!       enddo
!!    enddo
!!    end program demo_codebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!     Ref.: "Math matiques en Turbo-Pascal by
!!            M. Ducamp and A. Reverchon (2),
!!            Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function codebase(inval10,outbase,answer)

! ident_81="@(#) M_strings codebase(3f) convert whole number in base 10 to string in base [2-36]"

integer,intent(in)           :: inval10
integer,intent(in)           :: outbase
character(len=*),intent(out) :: answer
integer                      :: n
real                         :: inval10_local
integer                      :: outbase_local
integer                      :: in_sign
  answer=''
  in_sign=sign(1,inval10)*sign(1,outbase)
  inval10_local=abs(inval10)
  outbase_local=abs(outbase)
  if(outbase_local<2.or.outbase_local>36) then
    print *,'*codebase* ERROR: base must be between 2 and 36. base was',outbase_local
    codebase=.false.
  else
     do while(inval10_local>0.0 )
        n=INT(inval10_local-outbase_local*INT(inval10_local/outbase_local))
        if(n<10) then
           answer=ACHAR(IACHAR('0')+n)//answer
        else
           answer=ACHAR(IACHAR('A')+n-10)//answer
        endif
        inval10_local=INT(inval10_local/outbase_local)
     enddo
     codebase=.true.
  endif
  if(in_sign == -1)then
     answer='-'//trim(answer)
  endif
  if(answer == '')then
     answer='0'
  endif
end function codebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function todecimal(base, instr)

! ident_82="@(#) M_strings todecimal(3f) given string and base return decimal integer"

! based on an example at rosetta code.
character(len=36),parameter  :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
integer,intent(in)           :: base
character(*),intent(in)      :: instr
character(len=:),allocatable :: instr_local
integer                      :: todecimal
integer                      :: length, i, n

   instr_local=trim(lower(instr))
   todecimal = 0
   length = len(instr_local)
   do i = 1, length
      n = index(alphanum, instr_local(i:i)) - 1
      n = n * base**(length-i)
      todecimal = todecimal + n
   enddo
end function todecimal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function tobase(base, number)

! ident_83="@(#) M_strings tobase(3f) given integer and base return string"

! based on an example at rosetta code.
character(len=36),parameter  :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
integer,intent(in)           :: base
integer,intent(in)           :: number
character(len=:),allocatable :: tobase
character(len=31)            :: holdit
integer                      :: number_local, i, rem
   number_local=number

   holdit = "                               "
   do i = 31, 1, -1
      if(number_local < base) then
         holdit(i:i) = alphanum(number_local+1:number_local+1)
         exit
      endif
      rem = mod(number_local, base)
      holdit(i:i) = alphanum(rem+1:rem+1)
      number_local = number_local / base
   enddo
   tobase = adjustl(holdit)
end function tobase

!SUBROUTINE DectoBase(decimal, string, base)
! CHARACTER string
!    string = '0'
!    temp = decimal
!    length = CEILING( LOG(decimal+1, base) )   !<<<<<<<< INTERESTING
!    DO i = length, 1, -1
!      n = MOD( temp, base )
!      string(i) = "0123456789abcdefghijklmnopqrstuvwxyz"(n+1)
!      temp = INT(temp / base)
!    ENDDO
! END
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    paragraph(3f) - [M_strings:TOKENS] break a long line into a paragraph
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function paragraph(source_string,length)
!!
!!    character(len=*),intent(in)       :: source_string
!!    integer,intent(in)                :: length
!!    character(allocatable(len=length)    :: paragraph(:)
!!
!!##DESCRIPTION
!!    paragraph(3f) breaks a long line into a simple paragraph of specified
!!    line length.
!!
!!    Given a long string break it on spaces into an array such that no
!!    variable is longer than the specified length. Individual words longer
!!    than LENGTH will be placed in variables by themselves.
!!
!!##OPTIONS
!!     SOURCE_STRING  input string to break into an array of shorter strings
!!                    on blank delimiters
!!     LENGTH         length of lines to break the string into.
!!
!!##RETURNS
!!     PARAGRAPH  character array filled with data from source_string
!!                broken at spaces into variables of length LENGTH.
!!
!!##EXAMPLE
!!
!!  sample program
!!
!!    program demo_paragraph
!!    use M_strings, only : paragraph
!!    implicit none
!!    character(len=:),allocatable :: paragrph(:)
!!    character(len=*),parameter    :: string= '&
!!     &one two three four five &
!!     &six seven eight &
!!     &nine ten eleven twelve &
!!     &thirteen fourteen fifteen sixteen &
!!     &seventeen'
!!
!!    write(*,*)'LEN=',len(string)
!!    write(*,*)'INPUT:'
!!    write(*,*)string
!!
!!    paragrph=paragraph(string,40)
!!    write(*,*)'LEN=',len(paragrph),' SIZE=',size(paragrph)
!!    write(*,*)'OUTPUT:'
!!    write(*,'(a)')paragrph
!!
!!    write(*,'(a)')paragraph(string,0)
!!    write(*,'(3x,a)')paragraph(string,47)
!!
!!    end program demo_paragraph
!!
!!   Results:
!!
!!     LEN=         106
!!     INPUT:
!!     one two three four five six seven eight nine ten eleven twelve
!!     thirteen fourteen fifteen sixteen seventeen
!!     LEN=          40  SIZE=           3
!!     OUTPUT:
!!    one two three four five six seven eight
!!    nine ten eleven twelve thirteen fourteen
!!    fifteen sixteen seventeen
!!    one
!!    two
!!    three
!!    four
!!    five
!!    six
!!    seven
!!    eight
!!    nine
!!    ten
!!    eleven
!!    twelve
!!    thirteen
!!    fourteen
!!    fifteen
!!    sixteen
!!    seventeen
!!       one two three four five six seven eight nine
!!       ten eleven twelve thirteen fourteen fifteen
!!       sixteen seventeen
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function paragraph(source_string,length)

! ident_84="@(#) M_strings paragraph(3f) wrap a long string into a paragraph"

character(len=*),intent(in)       :: source_string
integer,intent(in)                :: length
integer                           :: itoken
integer                           :: istart
integer                           :: iend
character(len=*),parameter        :: delimiters=' '
character(len=:),allocatable      :: paragraph(:)
integer                           :: ilines
integer                           :: ilength
integer                           :: iword, iword_max
integer                           :: i
!-----------------------------------------------------------------------------------------------------------------------------------
!  parse string once to find out how big to make the returned array, then redo everything but store the data
!  could store array of endpoints and leave original whitespace alone or many other options
   do i=1,2
      iword_max=0                                  ! length of longest token
      ilines=1                                     ! number of output line output will go on
      ilength=0                                    ! length of output line so far
      itoken=0                                     ! must set ITOKEN=0 before looping on strtok(3f) on a new string.
      do while ( strtok(source_string,itoken,istart,iend,delimiters) )
         iword=iend-istart+1
         iword_max=max(iword_max,iword)
         if(iword > length)then                   ! this token is longer than the desired line length so put it on a line by itself
            if(ilength /= 0)then
               ilines=ilines+1
            endif
            if(i == 2)then     ! if paragraph has been allocated store data, else just gathering data to determine size of paragraph
               paragraph(ilines)=source_string(istart:iend)//' '
            endif
            ilength=iword+1
         elseif(ilength+iword <= length)then       ! this word will fit on current line
            if(i == 2)then
               paragraph(ilines)=paragraph(ilines)(:ilength)//source_string(istart:iend)
            endif
            ilength=ilength+iword+1
         else                                      ! adding this word would make line too long so start new line
            ilines=ilines+1
            ilength=0
            if(i == 2)then
               paragraph(ilines)=paragraph(ilines)(:ilength)//source_string(istart:iend)
            endif
            ilength=iword+1
         endif
      enddo
      if(i==1)then                                 ! determined number of lines needed so allocate output array
         allocate(character(len=max(length,iword_max)) :: paragraph(ilines))
         paragraph=' '
      endif
   enddo
   paragraph=paragraph(:ilines)
end function paragraph
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function setbits8(string) result(answer)
integer(kind=int8)          :: answer
character(len=8),intent(in) :: string
integer                     :: pos
integer                     :: lgth
   answer=0_int8
   lgth=len(string)
   if(lgth /= bit_size(answer))then
      write(stderr,*)'*setbits8* wrong string length =',lgth
      lgth=min(lgth,int(bit_size(answer)))
   endif
   do pos=1,lgth
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits8* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits8
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits16(string) result(answer)
integer(kind=int16)          :: answer
character(len=16),intent(in) :: string
integer                      :: pos
integer                      :: lgth
   answer=0_int16
   lgth=len(string)
   if(lgth /= bit_size(answer))then
      write(stderr,*)'*setbits16* wrong string length =',lgth
      lgth=min(lgth,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits16* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits16
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits32(string) result(answer)
integer(kind=int32)          :: answer
character(len=32),intent(in) :: string
integer                      :: pos
integer                      :: lgth
   answer=0_int32
   lgth=len(string)
   if(lgth /= bit_size(answer))then
      write(stderr,*)'*setbits32* wrong string length =',lgth
      lgth=min(lgth,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits32* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits32
!-----------------------------------------------------------------------------------------------------------------------------------
function setbits64(string) result(answer)
integer(kind=int64)          :: answer
character(len=64),intent(in) :: string
integer                      :: pos
integer                      :: lgth
   answer=0_int64
   lgth=len(string)
   if(lgth /= bit_size(answer))then
      write(stderr,*)'*setbits64* wrong string length =',lgth
      lgth=min(lgth,int(bit_size(answer)))
   endif
   do pos=1,len(string)
      select case(string(pos:pos))
       case('1')
         answer = ibset(answer, pos-1)
       case('0')
         answer = ibclr(answer, pos-1)
       case default
         write(stderr,*)'*setbits64* unknown value. must be 0 or 1. found [',string(pos:pos),'] at position ',pos,' in ',string
      end select
   enddo
end function setbits64
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     msg(3f) - [M_strings:TYPE] converts any standard scalar type to a string
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!     function msg(g1,g2g3,g4,g5,g6,g7,g8,g9,sep)
!!
!!      class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!      character(len=*),intent(in),optional :: sep
!!      character(len=:),allocatable :: msg
!!
!!##DESCRIPTION
!!     msg(3f) builds a space-separated string from up to nine scalar values.
!!
!!##OPTIONS
!!     g[1-9]  optional value to print the value of after the message. May
!!             be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!             or CHARACTER.
!!     sep     separator between values. Defaults to a space
!!
!!##RETURNS
!!     msg     description to print
!!
!!##EXAMPLES
!!
!!
!!   Sample program:
!!
!!        program demo_msg
!!        use M_strings, only : msg
!!        implicit none
!!        character(len=:),allocatable :: pr
!!        character(len=:),allocatable :: frmt
!!        integer                      :: biggest
!!
!!        pr=msg('HUGE(3f) integers',huge(0),&
!!        & 'and real',huge(0.0),'and double',huge(0.0d0))
!!        write(*,'(a)')pr
!!        pr=msg('real            :',&
!!         & huge(0.0),0.0,12345.6789,tiny(0.0) )
!!        write(*,'(a)')pr
!!        pr=msg('doubleprecision :',&
!!         & huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!        write(*,'(a)')pr
!!        pr=msg('complex         :',&
!!         & cmplx(huge(0.0),tiny(0.0)) )
!!        write(*,'(a)')pr
!!
!!        ! create a format on the fly
!!        biggest=huge(0)
!!        frmt=msg('(*(i',int(log10(real(biggest))),':,1x))',sep='')
!!        write(*,*)'format=',frmt
!!
!!        ! although it will often work, using msg(3f) in an I/O statement
!!        ! is not recommended
!!        write(*,*)msg('program will now stop')
!!
!!        end program demo_msg
!!
!!   Output
!!
!!       HUGE(3f) integers 2147483647 and real 3.40282347E+38
!!       and double 1.7976931348623157E+308
!!       real            : 3.40282347E+38 0.00000000
!!       12345.6787 1.17549435E-38
!!       doubleprecision : 1.7976931348623157E+308 0.0000000000000000
!!       12345.678900000001 2.2250738585072014E-308
!!       complex         : (3.40282347E+38,1.17549435E-38)
!!        format=(*(i9:,1x))
!!        program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function msg_scalar(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)

! ident_85="@(#) M_strings msg_scalar(3fp) writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic1 ,generic2 ,generic3 ,generic4 ,generic5
class(*),intent(in),optional  :: generic6 ,generic7 ,generic8 ,generic9
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      !x!type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      !x!type is (real(kind=real256));     write(line(istart:),'(1pg0)') generic
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
function msg_one(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)

! ident_86="@(#) M_strings msg_one(3fp) writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic1(:)
class(*),intent(in),optional  :: generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable   :: sep_local
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_one=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      !x!type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      !x!type is (real(kind=real256));     write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//"]"//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    find_field(3f) - [M_strings:TOKENS] parse a string into tokens
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine find_field (string, field, position, delims, delim, found)
!!
!!     character*(*),intent(in)           :: string
!!     character*(*),intent(out)          :: field
!!     integer,optional,intent(inout)     :: position
!!     character*(*),optional,intent(in)  :: delims
!!     character*(*),optional,intent(out) :: delim
!!     logical,optional,intent(out)       :: found
!!
!!##DESCRIPTION
!!
!!    Find a delimited field in a string.
!!
!!    Here's my equivalent, which I've used for nearly 2 decades, as you can
!!    see from the date. This doesn't try to mimic the C strtok (and doesn't
!!    have its limitations either). It is in a much more native Fortran style.
!!
!!    It is a little more complicated than some because it does some things
!!    that I regularly find useful. For example, it can tell the caller what
!!    trailing delimiter it found. This can be useful, for example, to
!!    distinguish between
!!
!!        somefield, someotherfield
!!
!!    versus
!!
!!        somefield=somevalue, someotherfield
!!
!!    Also, I have a bit of special handling for blanks. All the usage
!!    information is in the argument descriptions. Note that most of the
!!    arguments are optional.
!!
!!        from comp.lang.fortran @ Richard Maine
!!
!!##OPTIONS
!!    STRING     The string input.
!!
!!    FIELD      The returned field. Blank if no field found.
!!
!!    POSITION   On entry, the starting position for searching for the field.
!!               Default is 1 if the argument is not present.
!!               On exit, the starting position of the next field or
!!               len(string)+1 if there is no following field.
!!
!!    DELIMS     String containing the characters to be accepted as delimiters.
!!               If this includes a blank character, then leading blanks are
!!               removed from the returned field and the end delimiter may
!!               optionally be preceded by blanks. If this argument is
!!               not present, the default delimiter set is a blank.
!!
!!    DELIM      Returns the actual delimiter that terminated the field.
!!               Returns char(0) if the field was terminated by the end of
!!               the string or if no field was found.
!!               If blank is in delimiters and the field was terminated
!!               by one or more blanks, followed by a non-blank delimiter,
!!               the non-blank delimiter is returned.
!!
!!    FOUND      True if a field was found.
!!
!!##EXAMPLES
!!
!! Sample of uses
!!
!!        program demo_find_field
!!        use M_strings, only : find_field
!!        implicit none
!!        character(len=256)           :: string
!!        character(len=256)           :: field
!!        integer                      :: position
!!        character(len=:),allocatable :: delims
!!        character(len=1)             :: delim
!!        logical                      :: found
!!
!!        delims='[,]'
!!        position=1
!!        found=.true.
!!        string='[a,b,[ccc,ddd],and more]'
!!        write(*,'(a)')trim(string)
!!        do
!!           call find_field(string,field,position,delims,delim,found=found)
!!           if(.not.found)exit
!!           write(*,'("<",a,">")')trim(field)
!!        enddo
!!        write(*,'(*(g0))')repeat('=',70)
!!
!!        position=1
!!        found=.true.
!!        write(*,'(a)')trim(string)
!!        do
!!           call find_field(string,field,position,'[], ',delim,found=found)
!!           if(.not.found)exit
!!           write(*,'("<",a,">",i0,1x,a)')trim(field),position,delim
!!        enddo
!!        write(*,'(*(g0))')repeat('=',70)
!!
!!        end program demo_find_field
!! ```
!! Results:
!! ```text
!!  > [a,b,[ccc,ddd],and more]
!!  > <>
!!  > <a>
!!  > <b>
!!  > <>
!!  > <ccc>
!!  > <ddd>
!!  > <>
!!  > <and more>
!!  > <>
!!  > ======================================================================
!!  > [a,b,[ccc,ddd],and more]
!!  > <>2 [
!!  > <a>4 ,
!!  > <b>6 ,
!!  > <>7 [
!!  > <ccc>11 ,
!!  > <ddd>15 ]
!!  > <>16 ,
!!  > <and>20
!!  > <more>257 ]
!!  > ======================================================================
!!
!!##AUTHOR
!!    Richard Maine
!!
!!##LICENSE
!!    MIT
!!
!!##VERSION
!!    version 0.1.0, copyright Nov 15 1990, Richard Maine
!!
!!    Minor editing to conform to inclusion in the string procedure module
subroutine find_field (string, field, position, delims, delim, found)

!-- Find a delimited field in a string.
!-- 15 Nov 90, Richard Maine.

!-------------------- interface.
character*(*),intent(in)           :: string
character*(*),intent(out)          :: field
integer,optional,intent(inout)     :: position
character*(*),optional,intent(in)  :: delims
character*(*),optional,intent(out) :: delim
logical,optional,intent(out)       :: found
!-------------------- local.
character  :: delimiter*1
integer    :: pos, field_start, field_end, i
logical    :: trim_blanks
!-------------------- executable code.
   field = ''
   delimiter = char(0)
   pos = 1
   if (present(found)) found = .false.
   if (present(position)) pos = position
   if (pos > len(string)) goto 9000
   !if (pos < 1) error stop 'Illegal position in find_field'
   if (pos < 1) stop 'Illegal position in find_field'

   !-- Skip leading blanks if blank is a delimiter.
   field_start = pos
   trim_blanks = .true.
   if (present(delims)) trim_blanks = index(delims,' ') /= 0
   if (trim_blanks) then
      i = verify(string(pos:),' ')
      if (i == 0) then
         pos = len(string) + 1
         goto 9000
      end if
      field_start = pos + i - 1
   end if
   if (present(found)) found = .true.

   !-- Find the end of the field.
   if (present(delims)) then
      i = scan(string(field_start:), delims)
   else
      i = scan(string(field_start:), ' ')
   end if
   if (i == 0) then
      field_end = len(string)
      delimiter = char(0)
      pos = field_end + 1
   else
      field_end = field_start + i - 2
      delimiter = string(field_end+1:field_end+1)
      pos = field_end + 2
   end if

   !-- Return the field.
   field = string(field_start:field_end)

   !-- Skip trailing blanks if blank is a delimiter.
   if (trim_blanks) then
      i = verify(string(field_end+1:), ' ')
      if (i == 0) then
         pos = len(string) + 1
         goto 9000
      end if
      pos = field_end + i

      !-- If the first non-blank character is a delimiter,
      !-- skip blanks after it.
      i = 0
      if (present(delims)) i = index(delims, string(pos:pos))
      if (i /= 0) then
         delimiter = string(pos:pos)
         pos = pos + 1
         i = verify(string(pos:), ' ')
         if (i == 0) then
            pos = len(string) + 1
         else
            pos = pos + i - 1
         end if
      end if
   end if
   !---------- Normal exit.
   9000 continue
   if (present(delim)) delim = delimiter
   if (present(position)) position = pos
end subroutine find_field
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split2020(3f) - [M_strings:TOKENS] parse a string into tokens using
!!    proposed f2023 method
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   TOKEN form
!!
!!    subroutine split2020 (string, set, tokens, separator)
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    character(len=:),allocatable,intent(out) :: tokens(:)
!!    character(len=1),allocatable,intent(out),optional :: separator(:)
!!
!!   BOUNDS ARRAY form
!!
!!    subroutine split2020 (string, set, first, last)
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    integer,allocatable,intent(out) :: first(:)
!!    integer,allocatable,intent(out) :: last(:)
!!
!!   STEP THROUGH BY POSITION form
!!
!!    subroutine split2020 (string, set, pos [, back])
!!    character(len=*),intent(in) :: string
!!    character(len=*),intent(in) :: set
!!    integer,intent(inout)       :: pos
!!    logical,intent(in),optional :: back
!!
!!##DESCRIPTION
!!    Parse a string into tokens. STRING, SET, TOKENS and SEPARATOR must
!!    all be of the same CHARACTER kind type parameter.
!!
!!##OPTIONS
!!    STRING      string to break into tokens
!!
!!    SET         Each character in SET is a token delimiter. A
!!                sequence of zero or more characters in STRING delimited by
!!                any token delimiter, or the beginning or end of STRING,
!!                comprise a token. Thus, two consecutive token delimiters
!!                in STRING, or a token delimiter in the first or last
!!                character of STRING, indicate a token with zero length.
!!
!!                ??? how about if null defaults to all whitespace characters
!!
!!    TOKENS      It is allocated with the lower bound equal to
!!                one and the upper bound equal to the number of tokens in
!!                STRING, and with character length equal to the length of
!!                the longest token. The tokens in STRING are assigned by
!!                intrinsic assignment, in the order found, to the elements
!!                of TOKENS, in array element order.
!!
!!                ???If input is null it still must be of size 1?
!!
!!    SEPARATOR   Each element in SEPARATOR(i) is assigned the value of
!!                the ith token delimiter in STRING.
!!                It is allocated with the lower bound equal to
!!                one and the upper bound equal to one less than the number
!!                of tokens in STRING, and with character length equal to
!!                one.
!!
!!                ???one less than? '' ' '
!!
!!    FIRST     It is allocated with the lower bound equal to one and the
!!              upper bound equal to the number of tokens in STRING. Each
!!              element is assigned, in array element order, the starting
!!              position of each token in STRING, in the order found. If a
!!              token has zero length, the starting position is equal to one
!!              if the token is at the beginning of STRING, and one greater
!!              than the position of the preceding delimiter otherwise.
!!
!!    LAST      It is allocated with the lower bound equal to one and the
!!              upper bound equal to the number of tokens in STRING. Each
!!              element is assigned, in array element order, the ending
!!              position of each token in STRING, in the order found. If
!!              a token has zero length, the ending position is one less
!!              than the starting position.
!!
!!    POS       If BACK is present with the value .TRUE., the value
!!              of POS shall be in the range 0 < POS     LEN (STRING)+1;
!!              otherwise it shall be in the range 0     POS LEN (STRING).
!!
!!              If BACK is absent or is present with the value .FALSE., POS
!!              is assigned the position of the leftmost token delimiter in
!!              STRING whose position is greater than POS, or if there is
!!              no such character, it is assigned a value one greater than
!!              the length of STRING. This identifies a token with starting
!!              position one greater than the value of POS on invocation,
!!              and ending position one less than the value of POS on return.
!!
!!              If BACK is present with the value true, POS is assigned the
!!              position of the rightmost token delimiter in STRING whose
!!              position is less than POS, or if there is no such character,
!!              it is assigned the value zero. This identifies a token with
!!              ending position one less than the value of POS on invocation,
!!              and starting position one greater than the value of POS
!!              on return.
!!
!!              When SPLIT is invoked with a value for POS of
!!              1 <= POS <= LEN(STRING) and STRING(POS:POS) is not a
!!              token delimiter present in SET, the token identified by
!!              SPLIT does not comprise a complete token as described in the
!!              description of the SET argument, but rather a partial token.
!!
!!    BACK      shall be a logical scalar. It is an INTENT (IN) argument. If
!!              POS does not appear and BACK is present with the value true,
!!              STRING is scanned backwards for tokens starting from the
!!              end. If POS does not appear and BACK is absent or present
!!              with the value false, STRING is scanned forwards for tokens
!!              starting from the beginning.
!!
!!##EXAMPLES
!!
!! Sample of uses
!!
!!    program demo_sort2020
!!    use M_strings, only : split2020
!!    implicit none
!!    character(len=*),parameter :: gen='(*("[",g0,"]":,","))'
!!
!!     ! Execution of TOKEN form
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=:), allocatable :: tokens(:)
!!       character (len=*),parameter :: set = " ,"
!!       string = 'first,second,third'
!!       call split2020(string, set, tokens )
!!       write(*,gen)tokens
!!
!!     ! assigns the value ['first ','second','third ' ]
!!     ! to TOKENS.
!!     endblock
!!
!!     ! Execution of BOUNDS form
!!
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=*),parameter :: set = " ,"
!!       integer, allocatable        :: first(:), last(:)
!!       string =    'first,second,,forth'
!!       call split2020 (string, set, first, last)
!!       write(*,gen)first
!!       write(*,gen)last
!!
!!     ! will assign the value [ 1, 7, 14, 15 ] to FIRST,
!!     ! and the value [ 5, 12, 13, 19 ] to LAST.
!!     endblock
!!
!!     ! Execution of STEP form
!!     block
!!       character (len=:), allocatable :: string
!!       character (len=*),parameter :: set = " ,"
!!       integer :: p, istart, iend
!!       string = " one,   last  example  "
!!       do while (p < len(string))
!!         istart = p + 1
!!         call split2020 (string, set, p)
!!         iend=p-1
!!         if(iend > istart)then
!!            print '(t3,a,1x,i0,1x,i0)', string (istart:iend),istart,iend
!!         endif
!!       enddo
!!     endblock
!!    end program demo_sort2020
!!
!!   Results:
!!
!!    [first ],[second],[third ]
!!    [1],[7],[14],[15]
!!    [5],[12],[13],[19]
!!      one 2 4
!!      last 9 12
!!      example 15 21
!!
!!      > ??? option to skip adjacent delimiters (not return null tokens)
!!      >     common with whitespace
!!      > ??? quoted strings, especially CSV both " and ', Fortran adjacent
!!      >     is insert versus other rules
!!      > ??? escape character like \\ .
!!      > ??? multi-character delimiters like \\n, \\t,
!!      > ??? regular expression separator
!!
!!##AUTHOR
!!    Milan Curcic, "milancurcic@hey.com"
!!
!!##LICENSE
!!    MIT
!!
!!##VERSION
!!    version 0.1.0, copyright 2020, Milan Curcic
  pure subroutine split_tokens(string, set, tokens, separator)
     ! Splits a string into tokens using characters in set as token delimiters.
     ! If present, separator contains the array of token delimiters.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    character(:), allocatable, intent(out) :: tokens(:)
    character, allocatable, intent(out), optional :: separator(:)

    integer, allocatable :: first(:), last(:)
    integer :: n

    call split2020(string, set, first, last)
    allocate(character(len=maxval(last - first) + 1) :: tokens(size(first)))

    do concurrent (n = 1:size(tokens))
      tokens(n) = string(first(n):last(n))
    enddo

    if (present(separator)) then
      allocate(separator(size(tokens) - 1))
      do concurrent (n = 1:size(tokens) - 1)
        separator(n) = string(first(n+1)-1:first(n+1)-1)
      enddo
    endif

  end subroutine split_tokens
!===================================================================================================================================
  pure subroutine split_first_last(string, set, first, last)
     ! Computes the first and last indices of tokens in input string, delimited
     ! by the characters in set, and stores them into first and last output
     ! arrays.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, allocatable, intent(out) :: first(:)
    integer, allocatable, intent(out) :: last(:)

    character :: set_array(len(set))
    logical, dimension(len(string)) :: is_first, is_last, is_separator
    integer :: n, slen

    slen = len(string)

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    enddo

    do concurrent (n = 1:slen)
      is_separator(n) = any(string(n:n) == set_array)
    enddo

    is_first = .false.
    is_last = .false.

    if (.not. is_separator(1)) is_first(1) = .true.

    do concurrent (n = 2:slen-1)
      if (.not. is_separator(n)) then
        if (is_separator(n - 1)) is_first(n) = .true.
        if (is_separator(n + 1)) is_last(n) = .true.
      else
        if (is_separator(n - 1)) then
          is_first(n) = .true.
          is_last(n-1) = .true.
        endif
      endif
    enddo

    if (.not. is_separator(slen)) is_last(slen) = .true.

    first = pack([(n, n = 1, slen)], is_first)
    last = pack([(n, n = 1, slen)], is_last)

  end subroutine split_first_last
!===================================================================================================================================
  pure subroutine split_pos(string, set, pos, back)
     ! If back is absent, computes the leftmost token delimiter in string whose
     ! position is > pos. If back is present and true, computes the rightmost
     ! token delimiter in string whose position is < pos. The result is stored
     ! in pos.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    integer, intent(in out) :: pos
    logical, intent(in), optional :: back

    logical :: backward
    character :: set_array(len(set))
    integer :: n, result_pos

    !TODO use optval when implemented in stdlib
    !backward = optval(back, .false.)
    backward = .false.
    if (present(back)) backward = back

    do concurrent (n = 1:len(set))
      set_array(n) = set(n:n)
    enddo

    if (backward) then
      result_pos = 0
      do n = pos - 1, 1, -1
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        endif
      enddo
    else
      result_pos = len(string) + 1
      do n = pos + 1, len(string)
        if (any(string(n:n) == set_array)) then
          result_pos = n
          exit
        endif
      enddo
    endif

    pos = result_pos

  end subroutine split_pos
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
  pure function string_tokens(string, set) result(tokens)
     ! Splits a string into tokens using characters in set as token delimiters.
    character(*), intent(in) :: string
    character(*), intent(in) :: set
    character(:), allocatable :: tokens(:)
    call split_tokens(string, set, tokens)
  end function string_tokens
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Duplicate the M_journal module in condensed form for now so can be stand-alone on GITHUB
!                                                                     ll
!                                                                      l
!    j                                                                 l
!                                                                      l
!    j                                                                 l
!    j        oooooo    u      u   r rrrrrr   n nnnnn      aaaa        l
!    j       o      o   u      u   rr         nn     n         a       l
!    j       o      o   u      u   r          n      n    aaaaaa       l
! j  j       o      o   u      u   r          n      n   a     a       l
!  jj         oooooo     uuuuuu u  r          n      n    aaaaa a      l
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! @(#) place-holder for journal module
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine where_write_message(where,msg)

!@(#) M_journal::where_write_message(3fp): basic message routine used for journal files

character(len=*),intent(in)  :: where
character(len=*),intent(in)  :: msg
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
   adv='yes'
!-----------------------------------------------------------------------------------------------------------------------------------
   prefix=''
!-----------------------------------------------------------------------------------------------------------------------------------
   times=0
   do i=1,len_trim(where)
      select case(where(i:i))
      case('T','t')
         if(trailopen) then
            write(itrail,'(a)',advance=adv)prefix//trim(msg)
          !elseif(times == 0)then
          !   write(stdout,'(a)',advance=adv)prefix//trim(msg)
          !   times=times+1
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('S','s')
         write(stdout,'(a)',advance=adv)prefix//trim(msg)
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
         if(msg == '')then            ! if message is blank turn off prefix
            prefix_it=.false.
         else                         ! store message as string to pass to now_ex() on subsequent calls to make prefix
            prefix_template=msg
            prefix_it=.true.
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('N')                                                   ! new name for stdout
         if(msg /= ' '.and.msg /= '#N#'.and.msg /= '"#N#"')then   ! if filename not special or blank open new file
            close(unit=last_int,iostat=ios)
            open(unit=last_int,file=adjustl(trim(msg)),iostat=ios)
            if(ios == 0)then
               stdout=last_int
            else
               write(*,*)'*journal* error opening redirected output file, ioerr=',ios
               write(*,*)'*journal* msg='//trim(msg)
            endif
         elseif(msg == ' ')then
            close(unit=last_int,iostat=ios)
            stdout=6
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('C','c')
         if(trailopen)then
            write(itrail,'(3a)',advance=adv)prefix,comment,trim(msg)
         elseif(times == 0)then
             ! write(stdout,'(2a)',advance=adv)prefix,trim(msg)
             ! times=times+1
         endif
      case('D','d')
         if(debug)then
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'DEBUG: ',trim(msg)
            elseif(times == 0)then
               write(stdout,'(3a)',advance=adv)prefix,'DEBUG:',trim(msg)
               times=times+1
            endif
         endif
      case('F','f')
         flush(unit=itrail,iostat=ios,iomsg=mssge)
         if(ios /= 0)then
            write(*,'(a)') trim(mssge)
         endif
      case('A','a')
         if(msg /= '')then
            open(newunit=itrail,status='unknown',access='sequential',file=adjustl(trim(msg)),&
            & form='formatted',iostat=ios,position='append')
            trailopen=.true.
         endif
      case('O','o')
         if(msg /= '')then
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
         write(stdout,'(a)',advance=adv)'*journal* bad WHERE value '//trim(where)//' when msg=['//trim(msg)//']'
      end select
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine where_write_message
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine flush_trail()
call where_write_message('F','IGNORE THIS STRING')
end subroutine flush_trail
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_stdout_lun(iounit)
integer,intent(in)                   :: iounit
   stdout=iounit
end subroutine set_stdout_lun
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine where_write_message_all(where, g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,nospace)

!$(#) M_journal::where_write_message_all(3f): writes a message to a string composed of any standard scalar types

character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
logical,intent(in),optional   :: nospace
 !call where_write_message(where,str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9,nospace))
end subroutine where_write_message_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_message_only(message)

!$(#) M_journal::write_message_only(3fp): calls JOURNAL('sc',message)

character(len=*),intent(in)          :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   call where_write_message('sc',trim(message))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine write_message_only
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function str_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=*),intent(in),optional :: sep
character(len=:), allocatable :: str_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
character(len=:),allocatable  :: sep_local
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
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
   str_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      !x!type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      !x!type is (real(kind=real256));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic

end function str_scalar
!===================================================================================================================================
function str_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: str_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
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
   str_one=trim(line)
contains

subroutine print_generic(generic)
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      !x!type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      !x!type is (real(kind=real256));     write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
      class default
         stop 'unknown type in *print_generic*'
   end select
   line=trim(line)//"]"//sep_local
   istart=len_trim(line)+increment
end subroutine print_generic

end function str_one
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lowercase(str) result(lcstr)

! convert string to lower case leaving quoted strings as is

character (len=*):: str
character (len=len_trim(str)):: lcstr
integer :: lgth
integer :: ioffset
integer :: iquote
integer :: i
integer :: iav
integer :: iqc

lgth=len_trim(str)
ioffset=iachar('A')-iachar('a')
iquote=0
lcstr=str
do i=1,lgth
  iav=iachar(str(i:i))
  if(iquote==0 .and. (iav==34 .or.iav==39)) then
    iquote=1
    iqc=iav
    cycle
  endif
  if(iquote==1 .and. iav==iqc) then
    iquote=0
    cycle
  endif
  if (iquote==1) cycle
  if(iav >= iachar('A') .and. iav <= iachar('Z')) then
    lcstr(i:i)=achar(iav-ioffset)
  else
    lcstr(i:i)=str(i:i)
  endif
enddo

end function lowercase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function uppercase(str) result(ucstr)

! convert string to upper case leaving quoted strings as is

character (len=*):: str
character (len=len_trim(str)):: ucstr
integer :: lgth
integer :: ioffset
integer :: iquote
integer :: i
integer :: iav
integer :: iqc

lgth=len_trim(str)
ioffset=iachar('A')-iachar('a')
iquote=0
ucstr=str
do i=1,lgth
  iav=iachar(str(i:i))
  if(iquote==0 .and. (iav==34 .or.iav==39)) then
    iquote=1
    iqc=iav
    cycle
  endif
  if(iquote==1 .and. iav==iqc) then
    iquote=0
    cycle
  endif
  if (iquote==1) cycle
  if(iav >= iachar('a') .and. iav <= iachar('z')) then
    ucstr(i:i)=achar(iav+ioffset)
  else
    ucstr(i:i)=str(i:i)
  endif
enddo

end function uppercase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     matching_delimiter(3f) - [M_strings:QUOTES] find position of matching delimiter
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   impure elemental subroutine matching_delimiter(str,ipos,imatch)
!!
!!    character(len=*),intent(in)  :: str
!!    integer,intent(in)           :: ipos
!!    integer,intent(out)          :: imatch
!!
!!##DESCRIPTION
!!    Sets imatch to the position in string of the delimiter matching the
!!    delimiter in position ipos. Allowable delimiters are (), [], {}, <>.
!!
!!##OPTIONS
!!    str     input string to locate delimiter position in
!!    ipos    position of delimiter to find match for
!!    imatch  location of matching delimiter. If no match is found, zero (0)
!!            is returned.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_matching_delimiter
!!       use M_strings, only : matching_delimiter
!!       implicit none
!!       character(len=128)  :: str
!!       integer             :: imatch
!!
!!       str=' a [[[[b] and ] then ] finally ]'
!!       write(*,*)'string=',str
!!       call matching_delimiter(str,1,imatch)
!!       write(*,*)'location=',imatch
!!       call matching_delimiter(str,4,imatch)
!!       write(*,*)'location=',imatch
!!       call matching_delimiter(str,5,imatch)
!!       write(*,*)'location=',imatch
!!       call matching_delimiter(str,6,imatch)
!!       write(*,*)'location=',imatch
!!       call matching_delimiter(str,7,imatch)
!!       write(*,*)'location=',imatch
!!       call matching_delimiter(str,32,imatch)
!!       write(*,*)'location=',imatch
!!
!!    end program demo_matching_delimiter
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
impure elemental subroutine matching_delimiter(str,ipos,imatch)

! Sets imatch to the position in string of the delimiter matching the delimiter
! in position ipos. Allowable delimiters are (), [], {}, <>.

! pedigree?

character(len=*),intent(in) :: str
integer,intent(in) :: ipos
integer,intent(out) :: imatch

character :: delim1,delim2,ch
integer :: lenstr
integer :: idelim2
integer :: istart, iend
integer :: inc
integer :: isum
integer :: i

imatch=0
lenstr=len_trim(str)
delim1=str(ipos:ipos)
select case(delim1)
   case('(')
      idelim2=iachar(delim1)+1
      istart=ipos+1
      iend=lenstr
      inc=1
   case(')')
      idelim2=iachar(delim1)-1
      istart=ipos-1
      iend=1
      inc=-1
   case('[','{','<')
      idelim2=iachar(delim1)+2
      istart=ipos+1
      iend=lenstr
      inc=1
   case(']','}','>')
      idelim2=iachar(delim1)-2
      istart=ipos-1
      iend=1
      inc=-1
   case default
      write(*,*) delim1,' is not a valid delimiter'
      return
end select
if(istart < 1 .or. istart > lenstr) then
   write(*,*) delim1,' has no matching delimiter'
   return
endif
delim2=achar(idelim2) ! matching delimiter

isum=1
do i=istart,iend,inc
   ch=str(i:i)
   if(ch /= delim1 .and. ch /= delim2) cycle
   if(ch == delim1) isum=isum+1
   if(ch == delim2) isum=isum-1
   if(isum == 0) exit
enddo
if(isum /= 0) then
   write(*,*) delim1,' has no matching delimiter'
   return
endif
imatch=i

end subroutine matching_delimiter
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    longest_common_substring(3f) - [M_strings:COMPARE] function that
!!    returns the longest common substring of two strings.
!!##SYNOPSIS
!!
!!    function longest_common_substring(a,b) result(match)
!!
!!     character(len=*),intent(in)  :: a, b
!!     character(len=:),allocatable :: match
!!##DESCRIPTION
!!    function that returns the longest common substring of two strings.
!!
!!    Note that substrings are consecutive characters within a string.
!!    This distinguishes them from subsequences, which is any sequence of
!!    characters within a string, even if there are extraneous characters in
!!    between them.
!!
!!    Hence, the longest common subsequence between "thisisatest" and
!!    "testing123testing" is "tsitest", whereas the longest common substring
!!    is just "test".
!!##OPTIONS
!!    a,b  strings to search for the longest common substring.
!!##RETURNS
!!    longest_common_substring  the longest common substring found
!!##EXAMPLE
!!
!!  Sample program
!!
!!    program demo_longest_common_substring
!!    use M_strings, only : longest_common_substring
!!    implicit none
!!      call compare('testing123testingthing','thisis',             'thi')
!!      call compare('testing',             'sting',              'sting')
!!      call compare('thisisatest_stinger','testing123testingthing','sting')
!!      call compare('thisisatest_stinger', 'thisis',            'thisis')
!!      call compare('thisisatest',         'testing123testing',   'test')
!!      call compare('thisisatest',      'thisisatest',     'thisisatest')
!!    contains
!!
!!    subroutine compare(a,b,answer)
!!    character(len=*),intent(in) :: a, b, answer
!!    character(len=:),allocatable :: match
!!    character(len=*),parameter :: g='(*(g0))'
!!       match=longest_common_substring(a,b)
!!       write(*,g) 'comparing "',a,'" and "',b,'"'
!!       write(*,g) merge('(PASSED) "','(FAILED) "',answer == match), &
!!       & match,'"; expected "',answer,'"'
!!    end subroutine compare
!!
!!    end program demo_longest_common_substring
!!
!!   expected output
!!
!!    comparing "testing123testingthing" and "thisis"
!!    (PASSED) "thi"; expected "thi"
!!    comparing "testing" and "sting"
!!    (PASSED) "sting"; expected "sting"
!!    comparing "thisisatest_stinger" and "testing123testingthing"
!!    (PASSED) "sting"; expected "sting"
!!    comparing "thisisatest_stinger" and "thisis"
!!    (PASSED) "thisis"; expected "thisis"
!!    comparing "thisisatest" and "testing123testing"
!!    (PASSED) "test"; expected "test"
!!    comparing "thisisatest" and "thisisatest"
!!    (PASSED) "thisisatest"; expected "thisisatest"
function longest_common_substring(a,b) result(match)
character(len=*),intent(in)  :: a, b
character(len=:),allocatable :: match
character(len=:),allocatable :: a2, b2
integer :: left, foundat, len_a, i
   if(len(a) < len(b))then ! to reduce required comparisions look for shortest string in longest string
      a2=a
      b2=b
   else
      a2=b
      b2=a
   endif

   match=''

   do i=1,len(a2)-1
      len_a=len(a2)
      do left=1,len_a
         foundat=index(b2,a2(left:))
         if(foundat /= 0.and.len(match) < len_a-left+1)then
            if(len(a2(left:)) > len(match))then
               match=a2(left:)
               exit
            endif
         endif
      enddo

      if(len(a2) < len(match))exit
      a2=a2(:len(a2)-1)

   enddo

end function longest_common_substring
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

pure elemental function atoi (string) result(val)    ! Convert STRING to an integer value
integer(kind=int32) :: val
character(len=*), intent(in) :: string
character(len=1)            :: c
integer                     :: i
integer                     :: j
integer                     :: ilen
logical                     :: neg

   val = 0
   neg=.false.
   i=0
   c=' '

   ilen=len(string)
   do i=1, ilen                               ! Pass over any leading spaces
      c = string(i:i)
      if (c  /=  ' ') exit
   enddo

   if (c  ==  '-') then                       ! check for +- as first digit
      neg = .true.
      i = i + 1
   elseif (c  ==  '+') then
      neg = .false.
      i = i + 1
   endif

   do j=i,ilen                                ! Continue as long as its a digit ...
      c = string(j:j)
      if (lge(c, '0') .and. lle(c, '9')) then
         val = 10*val + ichar(c)-48           ! Shift number over and add new digit
      else
         exit
      endif
   enddo

   if (neg) val = -val                        ! Negate the result if necessary

end function atoi

pure elemental function atol (string) result(val)    ! Convert STRING to an integer value
integer(kind=int64) :: val
character(len=*), intent(in) :: string
character(len=1)            :: c
integer                     :: i
integer                     :: j
integer                     :: ilen
logical                     :: neg

   val = 0
   neg=.false.
   i=0
   c=' '

   ilen=len(string)
   do i=1, ilen                               ! Pass over any leading spaces
      c = string(i:i)
      if (c  /=  ' ') exit
   enddo

   if (c  ==  '-') then                       ! check for +- as first digit
      neg = .true.
      i = i + 1
   elseif (c  ==  '+') then
      neg = .false.
      i = i + 1
   endif

   do j=i,ilen                                ! Continue as long as its a digit ...
      c = string(j:j)
      if (lge(c, '0') .and. lle(c, '9')) then
         val = 10*val + ichar(c)-48           ! Shift number over and add new digit
      else
         exit
      endif
   enddo

   if (neg) val = -val                        ! Negate the result if necessary

end function atol
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    aton(3f) - [M_strings:TYPE] function returns argument as a numeric
!!    value from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    logical function aton(str,val[,msg])
!!
!!     character(len=*),intent(in)       :: str
!!     type(TYPE(kind=KIND)),intent(out) :: val
!!     character(len=:),allocatable,intent(out) :: msg
!!
!!##DESCRIPTION
!!    This function converts a string to a numeric value.
!!
!!##OPTIONS
!!
!!     str      holds string assumed to represent a numeric value
!!     val      returned value. May be REAL or INTEGER.
!!     msg      message describing error when ATON returns .false.
!!
!!##RETURNS
!!     aton     .true. if the conversion was successful, .false. otherwise
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!      program demo_aton
!!
!!       use M_strings, only: aton
!!       implicit none
!!       character(len=14),allocatable :: strings(:)
!!       doubleprecision               :: dv
!!       integer                       :: iv
!!       real                          :: rv
!!       integer                       :: i
!!
!!       ! different strings representing INTEGER, REAL, and DOUBLEPRECISION
!!       strings=[&
!!       &' 10.345       ',&
!!       &'+10           ',&
!!       &'    -3        ',&
!!       &'    -4.94e-2  ',&
!!       &'0.1           ',&
!!       &'12345.678910d0',&
!!       &'              ',& ! Note: will return zero without an error message
!!       &'1 2 1 2 1 . 0 ',& ! Note: spaces will be ignored
!!       &'WHAT?         ']  ! Note: error messages will appear, zero returned
!!
!!       do i=1,size(strings)
!!          write(*,'(a)',advance='no')'STRING:',strings(i)
!!          if(aton(strings(i),iv)) write(*,'(g0)',advance='no')':INTEGER ',iv
!!          if(aton(strings(i),rv)) write(*,'(g0)',advance='no')':INTEGER ',rv
!!          if(aton(strings(i),dv)) write(*,'(g0)',advance='no')':INTEGER ',dv
!!       enddo
!!
!!       end program demo_aton
!!
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
logical function ator_real32(str,val,msg)
use iso_fortran_env, only: wp => real32, ip => int64, int8
! Convert ASCII-text to DP and return .TRUE. if OK
character(len=*),intent(in) :: str
real(kind=wp) :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=int8),parameter  :: upper_e=iachar('E'), lower_e=iachar('e'), upper_d=iachar('D'), lower_d=iachar('d')
integer(kind=int8),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-'), decimal=iachar('.')
integer(kind=int8),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: sval(3)
integer                       :: digit_count(3)
integer(kind=int8)            :: value(3,len(str))
real(kind=wp)                 :: whole, fractional
integer                       :: power
integer                       :: cnt(6)
integer(kind=int8)            :: a, part
integer                       :: i, ipos, ios, too_many_digit_count

   value=0.0_wp
   cnt=0
   digit_count=0
   ipos=0
   ator_real32 = .false.
   sval = [1,0,1]
   part = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=int8)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         digit_count(part) = digit_count(part) + 1
         if(digit_count(part) < 19)then
            value(part,digit_count(part)) = a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
      case(decimal)                              ! if more than once should report error
         if(part > 2)cnt(5)=99999               ! decimal in exponent
         part = 2                                ! starting fractional value
         cnt(1)=cnt(1)+1
      case(upper_e,lower_e,upper_d,lower_d)      ! if more than once should report error
         part = 3
         cnt(2)=cnt(2)+1                         ! if more than one encountered an error
         ipos=0
      case(minus_sign)                           ! sign in non-standard position or duplicated should report error
         sval(part) = -1
         if(ipos /= 1)cnt(6)=99999               ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                         ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos /= 1)cnt(4)=99999               ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                         ! if more than one sign character an error, but caught by not being first
      case(space)                                ! should possibly not ignore all internal spaces
         ipos=ipos-1
      case default
         value(part,:) = 0.0_wp
         cnt(5)=99999                            ! unknown character
         !return
      end select
   enddo
   ! is no value after E an error?
   whole=0.0_wp
   do i = digit_count(1),1,-1
      whole=whole+value(1,i)*10**(digit_count(1)-i)
   enddo

   power=0
   do i = digit_count(3),1,-1
      power=power+value(3,i)*10**(digit_count(3)-i)
   enddo

   fractional=0.0_wp
   do i = digit_count(2),1,-1
      fractional=fractional+real(value(2,i),kind=wp)/10.0_wp**i
   enddo

   associate ( sgn=>sval(1), sexp=>sval(3) )
   val = sign(whole + fractional,real(sgn,kind=wp))* (10.0_wp**(power*sexp+too_many_digit_count))
   end associate
   if(all(cnt <= 1).and.ipos /= 0)then
      ator_real32 = .true.
   else
      read(str,fmt=*,iostat=ios) val ! use internal read for INF, NAN for now
      if(ios == 0)then
         ator_real32 = .true.
      else
         if(present(msg))then
            if(cnt(5) /= 0)then
                  msg='illegal character in value "'//trim(str)//'"'
               elseif(cnt(5) /= 0)then
                  msg='decimal in exponent in "'//trim(str)//'"'
               elseif(cnt(1) >= 2)then
                  msg='multiple decimals in "'//trim(str)//'"'
               elseif(cnt(2) >= 2)then
                  msg='more than one exponent prefix (e,d,E,D) in "'//trim(str)//'"'
               elseif(cnt(3) >= 2)then
                  msg='more than one sign character in "'//trim(str)//'"'
               elseif(cnt(6) /= 0)then
                  msg='- sign character not first in "'//trim(str)//'"'
               elseif(cnt(4) >= 2)then
                  msg='+ sign character not first in "'//trim(str)//'"'
               else
                  msg='error in data conversion in "'//trim(str)//'"'
               endif
         endif
         ator_real32 = .false.
      endif
   endif
end function ator_real32
logical function ator_real64(str,val,msg)
use iso_fortran_env, only: wp => real64, ip => int64, int8
! Convert ASCII-text to DP and return .TRUE. if OK
character(len=*),intent(in) :: str
real(kind=wp) :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=int8),parameter  :: upper_e=iachar('E'), lower_e=iachar('e'), upper_d=iachar('D'), lower_d=iachar('d')
integer(kind=int8),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-'), decimal=iachar('.')
integer(kind=int8),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: sval(3)
integer                       :: digit_count(3)
integer(kind=int8)            :: value(3,len(str))
real(kind=wp)                 :: whole, fractional
integer                       :: power
integer                       :: cnt(6)
integer(kind=int8)            :: a, part
integer                       :: i, ipos, ios, too_many_digit_count

   value=0.0_wp
   cnt=0
   digit_count=0
   ipos=0
   ator_real64 = .false.
   sval = [1,0,1]
   part = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=int8)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         digit_count(part) = digit_count(part) + 1
         if(digit_count(part) < 19)then
            value(part,digit_count(part)) = a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
      case(decimal)                              ! if more than once should report error
         if(part > 2)cnt(5)=99999               ! decimal in exponent
         part = 2                                ! starting fractional value
         cnt(1)=cnt(1)+1
      case(upper_e,lower_e,upper_d,lower_d)      ! if more than once should report error
         part = 3
         cnt(2)=cnt(2)+1                         ! if more than one encountered an error
         ipos=0
      case(minus_sign)                           ! sign in non-standard position or duplicated should report error
         sval(part) = -1
         if(ipos /= 1)cnt(6)=99999               ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                         ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos /= 1)cnt(4)=99999               ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                         ! if more than one sign character an error, but caught by not being first
      case(space)                                ! should possibly not ignore all internal spaces
         ipos=ipos-1
      case default
         value(part,:) = 0.0_wp
         cnt(5)=99999                            ! unknown character
         !return
      end select
   enddo
   ! is no value after E an error?
   whole=0.0_wp
   do i = digit_count(1),1,-1
      whole=whole+value(1,i)*10**(digit_count(1)-i)
   enddo

   power=0
   do i = digit_count(3),1,-1
      power=power+value(3,i)*10**(digit_count(3)-i)
   enddo

   fractional=0.0_wp
   do i = digit_count(2),1,-1
      fractional=fractional+real(value(2,i),kind=wp)/10.0_wp**i
   enddo

   associate ( sgn=>sval(1), sexp=>sval(3) )
   val = sign(whole + fractional,real(sgn,kind=wp))* (10.0_wp**(power*sexp+too_many_digit_count))
   end associate
   if(all(cnt <= 1).and.ipos /= 0)then
      ator_real64 = .true.
   else
      read(str,fmt=*,iostat=ios) val ! use internal read for INF, NAN for now
      if(ios == 0)then
         ator_real64 = .true.
      else
         if(present(msg))then
            if(cnt(5) /= 0)then
                  msg='illegal character in value "'//trim(str)//'"'
               elseif(cnt(5) /= 0)then
                  msg='decimal in exponent in "'//trim(str)//'"'
               elseif(cnt(1) >= 2)then
                  msg='multiple decimals in "'//trim(str)//'"'
               elseif(cnt(2) >= 2)then
                  msg='more than one exponent prefix (e,d,E,D) in "'//trim(str)//'"'
               elseif(cnt(3) >= 2)then
                  msg='more than one sign character in "'//trim(str)//'"'
               elseif(cnt(6) /= 0)then
                  msg='- sign character not first in "'//trim(str)//'"'
               elseif(cnt(4) >= 2)then
                  msg='+ sign character not first in "'//trim(str)//'"'
               else
                  msg='error in data conversion in "'//trim(str)//'"'
               endif
         endif
         ator_real64 = .false.
      endif
   endif
end function ator_real64
logical function atoi_int8(str,val,msg)
use iso_fortran_env, only: ip => int64, int8
! Convert ASCII-text to REAL and return .TRUE. if OK
character(len=*),intent(in)   :: str
integer(kind=int8)         :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=int8),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-')
integer(kind=int8),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: value, sval, digit_count
integer                       :: cnt(6)
integer(kind=int8)            :: a
integer                       :: i, ipos, too_many_digit_count

   value=0
   cnt=0
   digit_count=0
   ipos=0
   sval = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=int8)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         if(digit_count < 19)then
            value = value*10 + a-digit_0
         elseif(real(value*10)+real(a-digit_0) < huge(0_ip))then
            value = value*10 + a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
         digit_count = digit_count + 1
      case(minus_sign)                         ! sign in non-standard position or duplicated should report error
         sval = -1
         if(ipos /= 1)cnt(6)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos /= 1)cnt(4)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(space)                              ! should possibly not ignore all internal spaces (and maybe ignore commas too?)
         ipos=ipos-1
      case default
         value = 0
         cnt(5)=99999                          ! unknown character
      end select
   enddo
   val = sign(value,sval)* 10**too_many_digit_count
   if(all(cnt <= 1).and.ipos /= 0)then
      atoi_int8 = .true.
   else
      if(present(msg))then
         if(cnt(5) /= 0)then
               msg='illegal character in value "'//trim(str)//'"'
            elseif(cnt(3) >= 2)then
               msg='more than one sign character in "'//trim(str)//'"'
            elseif(cnt(6) /= 0)then
               msg='- sign character not first in "'//trim(str)//'"'
            elseif(cnt(4) >= 2)then
               msg='+ sign character not first in "'//trim(str)//'"'
            else
               msg='error in data conversion in "'//trim(str)//'"'
            endif
      endif
      atoi_int8 = .false.
   endif
end function atoi_int8
logical function atoi_int16(str,val,msg)
use iso_fortran_env, only: ip => int64, int8
! Convert ASCII-text to REAL and return .TRUE. if OK
character(len=*),intent(in)   :: str
integer(kind=int16)         :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=int8),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-')
integer(kind=int8),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: value, sval, digit_count
integer                       :: cnt(6)
integer(kind=int8)            :: a
integer                       :: i, ipos, too_many_digit_count

   value=0
   cnt=0
   digit_count=0
   ipos=0
   sval = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=int8)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         if(digit_count < 19)then
            value = value*10 + a-digit_0
         elseif(real(value*10)+real(a-digit_0) < huge(0_ip))then
            value = value*10 + a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
         digit_count = digit_count + 1
      case(minus_sign)                         ! sign in non-standard position or duplicated should report error
         sval = -1
         if(ipos /= 1)cnt(6)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos /= 1)cnt(4)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(space)                              ! should possibly not ignore all internal spaces (and maybe ignore commas too?)
         ipos=ipos-1
      case default
         value = 0
         cnt(5)=99999                          ! unknown character
      end select
   enddo
   val = sign(value,sval)* 10**too_many_digit_count
   if(all(cnt <= 1).and.ipos /= 0)then
      atoi_int16 = .true.
   else
      if(present(msg))then
         if(cnt(5) /= 0)then
               msg='illegal character in value "'//trim(str)//'"'
            elseif(cnt(3) >= 2)then
               msg='more than one sign character in "'//trim(str)//'"'
            elseif(cnt(6) /= 0)then
               msg='- sign character not first in "'//trim(str)//'"'
            elseif(cnt(4) >= 2)then
               msg='+ sign character not first in "'//trim(str)//'"'
            else
               msg='error in data conversion in "'//trim(str)//'"'
            endif
      endif
      atoi_int16 = .false.
   endif
end function atoi_int16
logical function atoi_int32(str,val,msg)
use iso_fortran_env, only: ip => int64, int8
! Convert ASCII-text to REAL and return .TRUE. if OK
character(len=*),intent(in)   :: str
integer(kind=int32)         :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=int8),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-')
integer(kind=int8),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: value, sval, digit_count
integer                       :: cnt(6)
integer(kind=int8)            :: a
integer                       :: i, ipos, too_many_digit_count

   value=0
   cnt=0
   digit_count=0
   ipos=0
   sval = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=int8)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         if(digit_count < 19)then
            value = value*10 + a-digit_0
         elseif(real(value*10)+real(a-digit_0) < huge(0_ip))then
            value = value*10 + a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
         digit_count = digit_count + 1
      case(minus_sign)                         ! sign in non-standard position or duplicated should report error
         sval = -1
         if(ipos /= 1)cnt(6)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos /= 1)cnt(4)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(space)                              ! should possibly not ignore all internal spaces (and maybe ignore commas too?)
         ipos=ipos-1
      case default
         value = 0
         cnt(5)=99999                          ! unknown character
      end select
   enddo
   val = sign(value,sval)* 10**too_many_digit_count
   if(all(cnt <= 1).and.ipos /= 0)then
      atoi_int32 = .true.
   else
      if(present(msg))then
         if(cnt(5) /= 0)then
               msg='illegal character in value "'//trim(str)//'"'
            elseif(cnt(3) >= 2)then
               msg='more than one sign character in "'//trim(str)//'"'
            elseif(cnt(6) /= 0)then
               msg='- sign character not first in "'//trim(str)//'"'
            elseif(cnt(4) >= 2)then
               msg='+ sign character not first in "'//trim(str)//'"'
            else
               msg='error in data conversion in "'//trim(str)//'"'
            endif
      endif
      atoi_int32 = .false.
   endif
end function atoi_int32
logical function atoi_int64(str,val,msg)
use iso_fortran_env, only: ip => int64, int8
! Convert ASCII-text to REAL and return .TRUE. if OK
character(len=*),intent(in)   :: str
integer(kind=int64)         :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=int8),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-')
integer(kind=int8),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: value, sval, digit_count
integer                       :: cnt(6)
integer(kind=int8)            :: a
integer                       :: i, ipos, too_many_digit_count

   value=0
   cnt=0
   digit_count=0
   ipos=0
   sval = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=int8)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         if(digit_count < 19)then
            value = value*10 + a-digit_0
         elseif(real(value*10)+real(a-digit_0) < huge(0_ip))then
            value = value*10 + a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
         digit_count = digit_count + 1
      case(minus_sign)                         ! sign in non-standard position or duplicated should report error
         sval = -1
         if(ipos /= 1)cnt(6)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos /= 1)cnt(4)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(space)                              ! should possibly not ignore all internal spaces (and maybe ignore commas too?)
         ipos=ipos-1
      case default
         value = 0
         cnt(5)=99999                          ! unknown character
      end select
   enddo
   val = sign(value,sval)* 10**too_many_digit_count
   if(all(cnt <= 1).and.ipos /= 0)then
      atoi_int64 = .true.
   else
      if(present(msg))then
         if(cnt(5) /= 0)then
               msg='illegal character in value "'//trim(str)//'"'
            elseif(cnt(3) >= 2)then
               msg='more than one sign character in "'//trim(str)//'"'
            elseif(cnt(6) /= 0)then
               msg='- sign character not first in "'//trim(str)//'"'
            elseif(cnt(4) >= 2)then
               msg='+ sign character not first in "'//trim(str)//'"'
            else
               msg='error in data conversion in "'//trim(str)//'"'
            endif
      endif
      atoi_int64 = .false.
   endif
end function atoi_int64
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_strings


!>>>>> build/dependencies/M_escape/src/M_list2.f90
!>
!!##NAME
!!    M_list2(3f) - [M_list2] maintain simple lists
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    use M_list2, only : insert, replace, remove
!!    use M_list2, only : dictionary
!!
!!##DESCRIPTION
!!
!!    The M_list2(3fm) module allows for maintaining an array as a sorted
!!    list. An example is given that creates a keyword-value dictionary
!!    using the lists.
!!
!!    The lists are maintained as simple allocatable arrays. Each time an
!!    entry is added or deleted the array is re-allocated. Because of the
!!    expense of reallocating the data these routines are best suited for
!!    maintaining small lists that do not change size frequently.
!!
!!    The advantage is that the dictionary components are simple arrays
!!    which can be easily accessed with standard routines.
!!
!!    BASIC LIST
!!
!!    subroutine locate(list,value,place,ier,errmsg)  finds the index where a
!!                                                    value is found or should
!!                                                    be in a sorted array and
!!                                                    flag if the value exists
!!                                                    already
!!    subroutine insert(list,value,place)     insert entry into an allocatable
!!                                            array at specified position
!!    subroutine replace(list,value,place)    replace entry in an allocatable
!!                                            array at specified position
!!    subroutine remove(list,place)           remove entry from an allocatable
!!                                            array at specified position
!!
!!    BASIC DICTIONARY
!!
!!    Due to bugs in gfortran up to at least 7.4.0, this next section
!!    does not work.
!!
!!    type dictionary
!!
!!       character(len=:),allocatable :: key(:)
!!       character(len=:),allocatable :: value(:)
!!       integer,allocatable          :: count(:)
!!
!!    %get      get value from type(dictionary) given an existing key
!!    %set      set or replace value for type(dictionary) given a key
!!    %del      delete an existing key from type(dictionary)
!!    %clr      empty a type(dictionary)
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_list
!!    use M_list2, only : insert, locate, replace, remove
!!    ! create a dictionary with character keywords, values, and value lengths
!!    ! using the routines for maintaining a list
!!
!!     use M_list2, only : locate, insert, replace
!!     implicit none
!!     character(len=:),allocatable   :: keywords(:)
!!     character(len=:),allocatable   :: values(:)
!!     integer,allocatable            :: counts(:)
!!     integer                        :: i
!!     ! insert and replace entries
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
!!     ! remove some entries
!!     call update('a')
!!     call update('c')
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
!!     ! get some values
!!     write(*,*)'get b=>',get('b')
!!     write(*,*)'get d=>',get('d')
!!     write(*,*)'get notthere=>',get('notthere')
!!
!!     contains
!!     subroutine update(key,valin)
!!     character(len=*),intent(in)           :: key
!!     character(len=*),intent(in),optional  :: valin
!!     integer                               :: place
!!     integer                               :: ilen
!!     character(len=:),allocatable          :: val
!!     if(present(valin))then
!!        val=valin
!!        ilen=len_trim(val)
!!        ! find where string is or should be
!!        call locate(keywords,key,place)
!!        ! if string was not found insert it
!!        if(place.lt.1)then
!!           call insert(keywords,key,iabs(place))
!!           call insert(values,val,iabs(place))
!!           call insert(counts,ilen,iabs(place))
!!        else
!!           call replace(values,val,place)
!!           call replace(counts,ilen,place)
!!        endif
!!     else
!!        call locate(keywords,key,place)
!!        if(place.gt.0)then
!!           call remove(keywords,place)
!!           call remove(values,place)
!!           call remove(counts,place)
!!        endif
!!     endif
!!     end subroutine update
!!     function get(key) result(valout)
!!     character(len=*),intent(in)   :: key
!!     character(len=:),allocatable  :: valout
!!     integer                       :: place
!!        ! find where string is or should be
!!        call locate(keywords,key,place)
!!        if(place.lt.1)then
!!           valout=''
!!        else
!!           valout=values(place)(:counts(place))
!!        endif
!!     end function get
!!    end program demo_M_list
!!
!!   Results:
!!
!!    d==>[value of d]
!!    c==>[value of c again]
!!    b==>[value of b]
!!    a==>[value of a again]
!!
!!    d==>[value of d]
!!    b==>[value of b]
!!
!!     get b=>value of b
!!     get d=>value of d
!!     get notthere=>
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_list2
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdout=>OUTPUT_UNIT    ! access computing environment
implicit none
private

public locate        ! [M_list2] find PLACE in sorted character array where value can be found or should be placed
   private locate_c
   private locate_d
   private locate_r
   private locate_i
public insert        ! [M_list2] insert entry into a sorted allocatable array at specified position
   private insert_c
   private insert_d
   private insert_r
   private insert_i
   private insert_l
public replace       ! [M_list2] replace entry by index from a sorted allocatable array if it is present
   private replace_c
   private replace_d
   private replace_r
   private replace_i
   private replace_l
public remove        ! [M_list2] delete entry by index from a sorted allocatable array if it is present
   private remove_c
   private remove_d
   private remove_r
   private remove_i
   private remove_l

!character(len=*),parameter::ident_1="&
!&@(#)M_list2::locate(3f): Generic subroutine locates where element is or should be in sorted allocatable array"
interface locate
   module procedure locate_c, locate_d, locate_r, locate_i
end interface

!character(len=*),parameter::ident_2="&
!&@(#)M_list2::insert(3f): Generic subroutine inserts element into allocatable array at specified position"
interface insert
   module procedure insert_c, insert_d, insert_r, insert_i, insert_l
end interface

!character(len=*),parameter::ident_3="&
!&@(#)M_list2::replace(3f): Generic subroutine replaces element from allocatable array at specified position"
interface replace
   module procedure replace_c, replace_d, replace_r, replace_i, replace_l
end interface

!character(len=*),parameter::ident_4="&
!&@(#)M_list2::remove(3f): Generic subroutine deletes element from allocatable array at specified position"
interface remove
   module procedure remove_c, remove_d, remove_r, remove_i, remove_l
end interface

logical,save :: debug=.false.
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
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    locate(3f) - [M_list2] finds the index where a string is found or should be in a sorted array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine locate(list,value,place,ier,errmsg)
!!
!!    character(len=:)|doubleprecision|real|integer,allocatable :: list(:)
!!    character(len=*)|doubleprecision|real|integer,intent(in)  :: value
!!    integer, intent(out)                  :: PLACE
!!
!!    integer, intent(out),optional         :: IER
!!    character(len=*),intent(out),optional :: ERRMSG
!!
!!##DESCRIPTION
!!
!!    LOCATE(3f) finds the index where the VALUE is found or should
!!    be found in an array. The array must be sorted in descending
!!    order (highest at top). If VALUE is not found it returns the index
!!    where the name should be placed at with a negative sign.
!!
!!    The array and list must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL,INTEGER)
!!
!!##OPTIONS
!!
!!    VALUE         the value to locate in the list.
!!    LIST          is the list array.
!!
!!##RETURNS
!!    PLACE         is the subscript that the entry was found at if it is
!!                  greater than zero(0).
!!
!!                  If PLACE is negative, the absolute value of
!!                  PLACE indicates the subscript value where the
!!                  new entry should be placed in order to keep the
!!                  list alphabetized.
!!
!!    IER           is zero(0) if no error occurs.
!!                  If an error occurs and IER is not
!!                  present, the program is stopped.
!!
!!    ERRMSG        description of any error
!!
!!##EXAMPLES
!!
!!
!!    Find if a string is in a sorted array, and insert the string into
!!    the list if it is not present ...
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_list2, only : locate
!!     implicit none
!!     character(len=:),allocatable  :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, plus, ii, end
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     write(*,*)'for "'//string//'" index is ',place, size(arr)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        plus=abs(place)
!!        ii=len(arr)
!!        end=size(arr)
!!        ! empty array
!!        if(end.eq.0)then
!!           arr=[character(len=ii) :: string ]
!!        ! put in front of array
!!        elseif(plus.eq.1)then
!!           arr=[character(len=ii) :: string, arr]
!!        ! put at end of array
!!        elseif(plus.eq.end)then
!!           arr=[character(len=ii) :: arr, string ]
!!        ! put in middle of array
!!        else
!!           arr=[character(len=ii) :: arr(:plus-1), string,arr(plus:) ]
!!        endif
!!        ! show array
!!        write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     endif
!!     end subroutine update
!!     end program demo_locate
!!
!!   Results:
!!
!!     for "b" index is            2           5
!!     for "[" index is           -4           5
!!    SIZE=5 xxx,b,aaa,[,ZZZ,
!!     for "c" index is           -2           6
!!    SIZE=6 xxx,c,b,aaa,[,ZZZ,
!!     for "ZZ" index is           -7           7
!!    SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     for "ZZZZ" index is           -6           8
!!    SIZE=8 xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!     for "z" index is           -1           9
!!    SIZE=9 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine locate_c(list,value,place,ier,errmsg)

!character(len=*),parameter::ident_5="&
!&@(#)M_list2::locate_c(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

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
   if(debug)write(stderr,*)'*locate_c* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
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
   if(debug)write(stderr,*)'*locate_c* END PLACE=',place,' ARRAYSIZE=',size(list),' LENGTH=',len(list)
end subroutine locate_c
subroutine locate_d(list,value,place,ier,errmsg)

!character(len=*),parameter::ident_6="&
!&@(#)M_list2::locate_d(3f): find PLACE in sorted doubleprecision array where VALUE can be found or should be placed"

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
   if(debug)write(stderr,*)'*locate_d* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
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
   if(debug)write(stderr,*)'*locate_d* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_d
subroutine locate_r(list,value,place,ier,errmsg)

!character(len=*),parameter::ident_7="&
!&@(#)M_list2::locate_r(3f): find PLACE in sorted real array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

real,allocatable                       :: list(:)
real,intent(in)                        :: value
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
      list=[real :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_r* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
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
   if(debug)write(stderr,*)'*locate_r* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_r
subroutine locate_i(list,value,place,ier,errmsg)

!character(len=*),parameter::ident_8="&
!&@(#)M_list2::locate_i(3f): find PLACE in sorted integer array where VALUE can be found or should be placed"

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
   if(debug)write(stderr,*)'*locate_i* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
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
   if(debug)write(stderr,*)'*locate_i* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    remove(3f) - [M_list2] remove entry from an allocatable array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine remove(list,place)
!!
!!    character(len=:)|doubleprecision|real|integer,intent(inout) :: list(:)
!!    integer, intent(out) :: PLACE
!!
!!##DESCRIPTION
!!
!!    Remove a value from an allocatable array at the specified index.
!!    The array is assumed to be sorted in descending order. It may be of
!!    type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER.
!!
!!##OPTIONS
!!
!!    list    is the list array.
!!    PLACE   is the subscript for the entry that should be removed
!!
!!##EXAMPLES
!!
!!
!!    Sample program
!!
!!     program demo_remove
!!     use M_sort, only : sort_shell
!!     use M_list2, only : locate, remove
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!     integer                       :: end
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'b', 'ab', 'bb', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,1)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,4)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end program demo_remove
!!
!!   Results:
!!
!!    Expected output
!!
!!     SIZE=9 xxx,bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=8 bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=7 bb,b,b,aaa,ZZZ,Z,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine remove_c(list,place)

!character(len=*),parameter::ident_9="@(#)M_list2::remove_c(3fp): remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(debug) write(stderr,*)'*remove_c* START PLACE=',place,' ORIGINALSIZE=',size(list)
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
   if(debug)write(stderr,*)'*remove_c* END PLACE=',place,' NEWSIZE=',size(list),' LENGTH=',len(list)
end subroutine remove_c
subroutine remove_d(list,place)

!character(len=*),parameter::ident_10="&
!&@(#)M_list2::remove_d(3fp): remove doubleprecision value from allocatable array at specified position"

doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(debug) write(stderr,*)'*remove_d* START PLACE=',place,' ORIGINALSIZE=',size(list)
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
   if(debug)write(stderr,*)'*remove_d* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_d
subroutine remove_r(list,place)

!character(len=*),parameter::ident_11="@(#)M_list2::remove_r(3fp): remove value from allocatable array at specified position"

real,allocatable    :: list(:)
integer,intent(in)  :: place
integer             :: end
   if(debug) write(stderr,*)'*remove_r* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_r* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_r
subroutine remove_l(list,place)

!character(len=*),parameter::ident_12="@(#)M_list2::remove_l(3fp): remove value from allocatable array at specified position"

logical,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(debug) write(stderr,*)'*remove_l* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_l* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_l
subroutine remove_i(list,place)

!character(len=*),parameter::ident_13="@(#)M_list2::remove_i(3fp): remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(debug) write(stderr,*)'*remove_i* START PLACE=',place,' ORIGINALSIZE=',size(list)
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
   if(debug)write(stderr,*)'*remove_i* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_list2] replace entry in a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine replace(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer, intent(out)          :: PLACE
!!
!!##DESCRIPTION
!!
!!    replace a value in an allocatable array at the specified index. Unless the
!!    array needs the string length to increase this is merely an assign of a value
!!    to an array element.
!!
!!    The array may be of type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER>
!!    It is assumed to be sorted in descending order without duplicate values.
!!
!!    The value and list must be of the same type.
!!
!!##OPTIONS
!!
!!    VALUE         the value to place in the array
!!    LIST          is the array.
!!    PLACE         is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!!   Replace key-value pairs in a dictionary
!!
!!     program demo_replace
!!     use M_list2, only  : insert, locate, replace
!!     ! Find if a key is in a list and insert it
!!     ! into the key list and value list if it is not present
!!     ! or replace the associated value if the key existed
!!     implicit none
!!     character(len=20)            :: key
!!     character(len=100)           :: val
!!     character(len=:),allocatable :: keywords(:)
!!     character(len=:),allocatable :: values(:)
!!     integer                      :: i
!!     integer                      :: place
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>",a,/))')(trim(keywords(i)),trim(values(i)),i=1,size(keywords))
!!
!!     call locate(keywords,'a',place)
!!     if(place.gt.0)then
!!        write(*,*)'The value of "a" is',trim(values(place))
!!     else
!!        write(*,*)'"a" not found'
!!     endif
!!
!!     contains
!!     subroutine update(key,val)
!!     character(len=*),intent(in)  :: key
!!     character(len=*),intent(in)  :: val
!!     integer                      :: place
!!
!!     ! find where string is or should be
!!     call locate(keywords,key,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(keywords,key,abs(place))
!!        call insert(values,val,abs(place))
!!     else ! replace
!!        call replace(values,val,place)
!!     endif
!!
!!     end subroutine update
!!    end program demo_replace
!!
!!   Expected output
!!
!!    d==>value of d
!!    c==>value of c again
!!    b==>value of b
!!    a==>value of a again
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine replace_c(list,value,place)

!character(len=*),parameter::ident_14="@(#)M_list2::replace_c(3fp): replace string in allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(debug) write(stderr,*)'*replace_c* START VALUE=',trim(value),' PLACE=',place,' ORIGINALSIZE=',size(list)
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
   if(debug)write(stderr,*)'*replace_c* END VALUE=',trim(value),' PLACE=',place,' NEWSIZE=',size(list),' LENGTH=',len(list)
end subroutine replace_c
subroutine replace_d(list,value,place)

!character(len=*),parameter::ident_15="&
!&@(#)M_list2::replace_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)   :: value
doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(debug) write(stderr,*)'*replace_d* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
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
   if(debug)write(stderr,*)'*replace_d* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_d
subroutine replace_r(list,value,place)

!character(len=*),parameter::ident_16="@(#)M_list2::replace_r(3fp): place value into allocatable array at specified position"

real,intent(in)       :: value
real,allocatable      :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(debug) write(stderr,*)'*replace_r* START REPLACE_R VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_r* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_r* END REPLACE_R VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_r
subroutine replace_l(list,value,place)

!character(len=*),parameter::ident_17="@(#)M_list2::replace_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*replace_l* START REPLACE_L VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_l* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_l* END REPLACE_L VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_l
subroutine replace_i(list,value,place)

!character(len=*),parameter::ident_18="@(#)M_list2::replace_i(3fp): place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*replace_i* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_i* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    insert(3f) - [M_list2] insert entry into a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine insert(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer,intent(in)    :: place
!!
!!##DESCRIPTION
!!
!!    Insert a value into an allocatable array at the specified index.
!!    The list and value must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL, or INTEGER)
!!
!!##OPTIONS
!!
!!    list    is the list array. Must be sorted in descending order.
!!    value   the value to place in the array
!!    PLACE   is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!!    Find if a string is in a sorted array, and insert the string into
!!    the list if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_list2, only : locate, insert
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!     ! add or replace values
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, end
!!
!!     end=size(arr)
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(arr,string,abs(place))
!!     endif
!!     ! show array
!!     end=size(arr)
!!     write(*,'("array is now SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end subroutine update
!!     end program demo_insert
!!
!!   Results:
!!
!!     array is now SIZE=5 xxx,b,aaa,ZZZ,,
!!     array is now SIZE=6 xxx,b,aaa,[,ZZZ,,
!!     array is now SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     array is now SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!     array is now SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!     array is now SIZE=10 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine insert_c(list,value,place)

!character(len=*),parameter::ident_19="@(#)M_list2::insert_c(3fp): place string into allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end
   if(debug) write(stderr,*)'*insert_c* START VALUE=',trim(value),' PLACE=',place,' ORIGINALSIZE=',size(list)

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

   if(debug)write(stderr,*)'*insert_c* END VALUE=',trim(value),' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_c
subroutine insert_r(list,value,place)

!character(len=*),parameter::ident_20="@(#)M_list2::insert_r(3fp): place real value into allocatable array at specified position"

real,intent(in)       :: value
real,allocatable      :: list(:)
integer,intent(in)    :: place
integer               :: end

   if(debug) write(stderr,*)'*insert_r* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif

   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_r* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_r* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_r
subroutine insert_d(list,value,place)

!character(len=*),parameter::ident_21="&
!&@(#)M_list2::insert_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)       :: value
doubleprecision,allocatable      :: list(:)
integer,intent(in)               :: place
integer                          :: end
   if(debug) write(stderr,*)'*insert_d* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
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
   if(debug)write(stderr,*)'*insert_d* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_d
subroutine insert_l(list,value,place)

!character(len=*),parameter::ident_22="@(#)M_list2::insert_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*insert_l* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_l* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_l* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_l
subroutine insert_i(list,value,place)

!character(len=*),parameter::ident_23="@(#)M_list2::insert_i(3fp): place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*insert_i* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
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

   if(debug)write(stderr,*)'*insert_i* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_delete(3f) - [M_list2] delete entry by name from an allocatable sorted string array if it is present
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine dict_delete(key,dict)
!!
!!    character(len=*),intent(in) :: key
!!    type(dictionary)            :: dict
!!
!!##DESCRIPTION
!!
!!    Find if a string is in a sorted array, and delete the string
!!    from the dictionary if it is present.
!!
!!##OPTIONS
!!
!!    KEY           the key name to find and delete from the dictionary.
!!    DICTIONARY    the dictionary.
!!
!!##EXAMPLES
!!
!!
!!    delete a key from a dictionary by name.
!!
!!     program demo_dict_delete
!!     use M_list2, only : dictionary
!!     implicit none
!!     type(dictionary) :: caps
!!     integer                       :: i
!!     call caps%set(caps,'A','aye')
!!     call caps%set(caps,'B','bee')
!!     call caps%set(caps,'C','see')
!!     call caps%set(caps,'D','dee')
!!
!!     write(*,101)(trim(arr(i)),i=1,size(caps%keys)) ! show array
!!     call  caps%del(caps,'A')
!!     call  caps%del(caps,'C')
!!     call  caps%del(caps,'z')
!!     write(*,101)(trim(arr(i)),i=1,size(arr)) ! show array
!!     101 format (1x,*("[",a,"]",:,","))
!!     end program demo_dict_delete
!!
!!    Results:
!!
!!     [z],[xxx],[c],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[b],[b],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[aaa],[ZZZ],[ZZ]
!!     [z],[xxx],[aaa],[ZZZ]
!!     [z],[xxx],[aaa],[ZZZ]
!!     [xxx],[aaa],[ZZZ]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine dict_delete(self,key)

!character(len=*),parameter::ident_24="@(#)M_list2::dict_delete(3f): remove string from sorted allocatable string array if present"

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
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_get(3f) - [M_list2] get value of key-value pair in a dictionary given key
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine dict_get(dict,key,value)
!!
!!    type(dictionary)            :: dict
!!    character(len=*),intent(in) :: key
!!    character(len=*),intent(in) :: VALUE
!!
!!##DESCRIPTION
!!
!!    get a value given a key from a key-value dictionary
!!
!!    If key is not found in dictionary , return a blank
!!
!!##OPTIONS
!!
!!    DICT     is the dictionary.
!!    KEY      key name
!!    VALUE    value associated with key
!!
!!##EXAMPLES
!!
!!
!!     program demo_locate
!!     use M_list2, only : dictionary
!!     implicit none
!!     type(dictionary)             :: table
!!     character(len=:),allocatable :: val
!!     integer          :: i
!!
!!     call table%set('A','value for A')
!!     call table%set('B','value for B')
!!     call table%set('C','value for C')
!!     call table%set('D','value for D')
!!     call table%set('E','value for E')
!!     call table%set('F','value for F')
!!     call table%set('G','value for G')
!!     write(*,*)table%get('A')
!!     write(*,*)table%get('B')
!!     write(*,*)table%get('C')
!!     write(*,*)table%get('D')
!!     write(*,*)table%get('E')
!!     write(*,*)table%get('F')
!!     write(*,*)table%get('G')
!!     write(*,*)table%get('H')
!!     end program demo_locate
!!
!!    Results:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function dict_get(self,key) result(value)

!character(len=*),parameter::ident_25="@(#)M_list2::dict_get(3f): get value of key-value pair in dictionary, given key"

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
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dict_add(3f) - [M_list2] add or replace a key-value pair in a dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine dict_add(dict,key,value)
!!
!!    type(dictionary)            :: dict
!!    character(len=*),intent(in) :: key
!!    character(len=*),intent(in) :: VALUE
!!
!!##DESCRIPTION
!!    Add or replace a key-value pair in a dictionary.
!!
!!##OPTIONS
!!    DICT     is the dictionary.
!!    key      key name
!!    VALUE    value associated with key
!!
!!##EXAMPLES
!!
!!    If string is not found in a sorted array, insert the string
!!
!!     program demo_add
!!     use M_list2, only : dictionary
!!     implicit none
!!     type(dictionary) :: dict
!!     integer          :: i
!!
!!     call dict%set('A','b')
!!     call dict%set('B','^')
!!     call dict%set('C',' ')
!!     call dict%set('D','c')
!!     call dict%set('E','ZZ')
!!     call dict%set('F','ZZZZ')
!!     call dict%set('G','z')
!!     call dict%set('A','new value for A')
!!     write(*,'(*(a,"==>","[",a,"]",/))')(trim(dict%key(i)),dict%value(i)(:dict%count(i)),i=1,size(dict%key))
!!     end program demo_add
!!
!!    Results:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine dict_add(self,key,value)

!character(len=*),parameter::ident_26="@(#)M_list2::dict_add(3f): place key-value pair into dictionary, adding the key if required"

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
end module M_list2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================


!>>>>> build/dependencies/M_escape/src/M_escape.f90
!>
!!##NAME
!!    M_escape(3f) - [M_escape::INTRO] substitute escape sequences for HTML-like
!!                   syntax in strings
!!
!!##SYNOPSIS
!!
!!     use M_escape, only : esc, esc_mode, update
!!     use M_escape, only : attr
!!     use M_escape, only : color, color_mode
!!
!!##DESCRIPTION
!!    M_escape is a Fortran module for using HTML-like syntax to add
!!    attributes to terminal output such as color.
!!
!!    ANSI escape sequences are not universally supported by all terminal
!!    emulators; and normally should be suppressed when not going to a tty
!!    device. This routine provides the basic structure to support such
!!    behaviors, or to perhaps in the future generate a CSS style sheet
!!    and HTML instead of text to the terminal, ...
!!
!!    Alternatively, direct use of the escape sequences is supported,
!!    as well as a functional interface, and an object-oriented approach.
!!
!!    The original concept was to allow formatting by using an existing
!!    HTML library to allow the user to write HTML and to format it on a
!!    terminal like w3m, lynx, and link do. And in some ways this is an
!!    opposite approach in that it is directly formatting the text by using
!!    a similar syntax to directly generate text attributes; but it is a
!!    much simpler approach programmatically.
!!
!!    Typically, you should use M_system::system_istty(3f) or the common
!!    Fortran extension ISATTY() to set the default
!!    to "plain" instead of "color" when the output file is not a terminal.
!!
!!##MAJOR FEATURES
!!    o Add ANSI terminal escape sequences with an HTML-like syntax with
!!      ESC(3f).
!!    o suppress the escape sequence output with ESC_MODE(3f).
!!    o add, delete, and replace what strings are produced using UPDATE(3f).
!!
!!##LIMITATIONS
!!      o colors are not nestable, keywords are case-sensitive,
!!      o not all terminals obey the sequences. On Windows, it is best if
!!        you use Windows 10+ and/or the Linux mode; although it has worked
!!        with all CygWin and MinGW and Putty windows and mintty(1).
!!
!!##FUTURE
!!     Full support for alternate output formats like HTML and popular markdown
!!     syntax. For example
!!
!!       ANSI  HTML        Markdown
!!             <h1></h1>   #
!!             <h2></h2>   ##
!!             <b></b>     ** and **
!!             <i></i>     __ and __
!!
!!    Apparently have to make a stack of colors to allow nesting colors
!!
!!    How common are extensions like xterm-256 has to set RGB values for
!!    colors and so on?
!!
!!    Should a call to system_istty(3f) be built in to turn off escape sequences
!!    when a terminal is not present?
!!
!!    For color or pre-defined words a case statement could be used to call
!!    specific functions to support platforms like old Microsoft consoles that
!!    require a function call to assign text attributes instead of in-band ANSI
!!    escape control sequences. See the "Rosetta Code" web site for examples
!!    of generating color in Microsoft consoles.
!!
!!    Attributes are currently ended at the end of each call to esc(3f). Perhaps
!!    allow multi-line formatting?
!!
!!    Ultimately, an object-oriented package with capabilities like ncurses to
!!    define a "pad" and move and resize and format it would be ideal and very
!!    useful. Also see fixedform(3f) in the GPF (General Fortran Package).
!!
!!    It is a shame xterm(1) does not support pixel-oriented abilities to define
!!    a "graphics" area or support canvas(3c)-like in-band graphics, somewhat
!!    like Tektronix terminals, although it does have a Tektronix 4010 mode.
!!
!!    Perhaps overload + to replace //
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_escape
!!    use M_escape, only : esc, esc_mode
!!    implicit none
!!    character(len=1024) :: line
!!    real :: value
!!       write(*,'(a)')&
!!       &esc('<r><W>ERROR:</W>This should appear as red text</y>')
!!       write(*,'(a)')&
!!       &esc('<y><B>WARNING:</B></y> This should appear as default text')
!!
!!       value=3.4567
!!       if( (value>0.0) .and. (value<100.0))then
!!          write(line,fmt=&
!!          &'("<w><G>GREAT</G></w>:&
!!          &The new value <Y><b>",f8.4,"</b></Y> is in range")')value
!!       else
!!          write(line,fmt=&
!!          &'("<R><e>ERROR</e></R>:&
!!          &The new value <Y><b>",g0,"</b></Y> is out of range")')value
!!       endif
!!
!!       write(*,'(a)')esc(trim(line))
!!       ! write as plain text
!!       call esc_mode(manner='plain')
!!       write(*,'(a)')esc(trim(line))
!!
!!    end program demo_M_escape
!!
!!##ALTERNATE DIRECT USE
!!
!!   Alternatively, you may use the escape sequences directly
!!
!!    program direct
!!       use M_escape, only : &
!!      ! FOREGROUND COLORS
!!         & fg_red, fg_cyan, fg_magenta, fg_blue, &
!!         & fg_green, fg_yellow, fg_white, fg_ebony, &
!!         & fg_default, &
!!      ! BACKGROUND COLORS
!!         & bg_red, bg_cyan, bg_magenta, bg_blue, &
!!         & bg_green, bg_yellow, bg_white, bg_ebony, &
!!         & bg_default, &
!!      ! ATTRIBUTES
!!         & bold, italic, inverse, underline,  &
!!         & unbold, unitalic, uninverse, ununderline,  &
!!         & reset, &
!!      ! DISPLAY
!!         & clear
!!       implicit none
!!         write(*,'(*(g0))')fg_red,bg_green,bold,'Hello!',reset
!!    end program direct
!!
!!##ALTERNATE FUNCTIONAL INTERFACE
!!
!!  If you prefer a functional interface, use the attr(3f) function with
!!  the same keywords as with the esc(3f) function. Note that esc_mode(3f)
!!  will work with this function.
!!
!!    program functional
!!    use M_escape, only : attr, esc_mode
!!    implicit none
!!         call printme('color')
!!         call printme('plain')
!!         call printme('raw')
!!    contains
!!    subroutine printme(mymode)
!!    character(len=*),intent(in) :: mymode
!!       call esc_mode(mymode)
!!       write(*,'(a)')mymode
!!       write(*,'(*(g0))',advance='no') &
!!        & attr('red:BLUE:bold','Hello!'), &
!!        & 'and everything is back to defaults or ', &
!!        & attr('RED:blue:bold'),'Hello Again!', &
!!        & attr('/BLUE'),' Well, the text color is still on.',attr('reset')
!!       write(*,'(*(g0))',advance='yes')' Back to normal writes.'
!!    end subroutine printme
!!    end program functional
module M_escape
use M_list2, only : insert, locate, replace, remove
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdin=>INPUT_UNIT    ! access computing environment
use, intrinsic :: iso_c_binding, only: c_int
implicit none
private
public esc
public esc_mode
public update
public print_dictionary
public M_escape_initialize

!-!public flush_colors, init_colors
public attr

! direct use of constant strings
public color
public color_mode
logical,save :: G_color=.true.

logical,save :: debug=.false.

character(len=:),allocatable,save :: keywords(:)
character(len=:),allocatable,save :: values(:)
integer,allocatable,save :: counts(:)

character(len=:),allocatable,save :: mode

! DECIMAL
! *-------*-------*-------*-------*-------*-------*-------*-------*
! | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
! | 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |
! | 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|
! | 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |
! | 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  ' |
! | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
! | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
! | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
! | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
! | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
! | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
! | 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |
! | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
! |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
! |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
! |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|
! *-------*-------*-------*-------*-------*-------*-------*-------*

! mnemonics
character(len=*),parameter  :: NL=new_line('a')                     ! New line character.
character(len=*),parameter  :: HT=char(09)                          ! Horizontal tab
character(len=*),parameter  :: ESCAPE=achar(27)                     ! "\" character.
! codes
character(len=*),parameter  :: CODE_START=ESCAPE//'['               ! Start ANSI code, "\[".
character(len=*),parameter  :: CODE_END='m'                         ! End ANSI code, "m".
character(len=*),parameter  :: CODE_RESET=CODE_START//'0'//CODE_END ! Clear all styles, "\[0m".

character(len=*),parameter  :: CLEAR_DISPLAY=CODE_START//'2J'
character(len=*),parameter  :: HOME_DISPLAY=CODE_START//'H'
character(len=*),parameter  :: BELL=achar(7)
character(len=*),parameter  :: NEWLINE=achar(7)

character(len=*),parameter  :: BOLD_ON='1',   ITALIC_ON='3',   UNDERLINE_ON='4',   INVERSE_ON='7'
character(len=*),parameter  :: BOLD_OFF='22', ITALIC_OFF='23', UNDERLINE_OFF='24', INVERSE_OFF='27'

character(len=*),parameter  :: COLOR_FG_BLACK='30', COLOR_FG_RED='31',     COLOR_FG_GREEN='32', COLOR_FG_YELLOW='33'
character(len=*),parameter  :: COLOR_FG_BLUE='34',  COLOR_FG_MAGENTA='35', COLOR_FG_CYAN='36',  COLOR_FG_WHITE='37'
character(len=*),parameter  :: COLOR_FG_DEFAULT='39'

character(len=*),parameter  :: COLOR_FG_BLACK_INTENSE='90', COLOR_FG_RED_INTENSE='91'
character(len=*),parameter  :: COLOR_FG_GREEN_INTENSE='92', COLOR_FG_YELLOW_INTENSE='93'
character(len=*),parameter  :: COLOR_FG_BLUE_INTENSE='94',  COLOR_FG_MAGENTA_INTENSE='95'
character(len=*),parameter  :: COLOR_FG_CYAN_INTENSE='96',  COLOR_FG_WHITE_INTENSE='97'

character(len=*),parameter  :: COLOR_BG_BLACK='40', COLOR_BG_RED='41',     COLOR_BG_GREEN='42', COLOR_BG_YELLOW='43'
character(len=*),parameter  :: COLOR_BG_BLUE='44',  COLOR_BG_MAGENTA='45', COLOR_BG_CYAN='46',  COLOR_BG_WHITE='47'

character(len=*),parameter  :: COLOR_BG_DEFAULT='49'

character(len=*),parameter  :: COLOR_BG_BLACK_INTENSE='100', COLOR_BG_RED_INTENSE='101'
character(len=*),parameter  :: COLOR_BG_GREEN_INTENSE='102', COLOR_BG_YELLOW_INTENSE='103'
character(len=*),parameter  :: COLOR_BG_BLUE_INTENSE='104',  COLOR_BG_MAGENTA_INTENSE='105'
character(len=*),parameter  :: COLOR_BG_CYAN_INTENSE='106',  COLOR_BG_WHITE_INTENSE='107'

! for direct use of escape sequences

! foreground colors
character(len=*),parameter,public :: fg_red      =  CODE_START//COLOR_FG_RED//CODE_END
character(len=*),parameter,public :: fg_cyan     =  CODE_START//COLOR_FG_CYAN//CODE_END
character(len=*),parameter,public :: fg_magenta  =  CODE_START//COLOR_FG_MAGENTA//CODE_END
character(len=*),parameter,public :: fg_blue     =  CODE_START//COLOR_FG_BLUE//CODE_END
character(len=*),parameter,public :: fg_green    =  CODE_START//COLOR_FG_GREEN//CODE_END
character(len=*),parameter,public :: fg_yellow   =  CODE_START//COLOR_FG_YELLOW//CODE_END
character(len=*),parameter,public :: fg_white    =  CODE_START//COLOR_FG_WHITE//CODE_END
character(len=*),parameter,public :: fg_ebony    =  CODE_START//COLOR_FG_BLACK//CODE_END
character(len=*),parameter,public :: fg_black    =  CODE_START//COLOR_FG_BLACK//CODE_END
character(len=*),parameter,public :: fg_default  =  CODE_START//COLOR_FG_DEFAULT//CODE_END

! background colors
character(len=*),parameter,public :: bg_red      =  CODE_START//COLOR_BG_RED//CODE_END
character(len=*),parameter,public :: bg_cyan     =  CODE_START//COLOR_BG_CYAN//CODE_END
character(len=*),parameter,public :: bg_magenta  =  CODE_START//COLOR_BG_MAGENTA//CODE_END
character(len=*),parameter,public :: bg_blue     =  CODE_START//COLOR_BG_BLUE//CODE_END
character(len=*),parameter,public :: bg_green    =  CODE_START//COLOR_BG_GREEN//CODE_END
character(len=*),parameter,public :: bg_yellow   =  CODE_START//COLOR_BG_YELLOW//CODE_END
character(len=*),parameter,public :: bg_white    =  CODE_START//COLOR_BG_WHITE//CODE_END
character(len=*),parameter,public :: bg_ebony    =  CODE_START//COLOR_BG_BLACK//CODE_END
character(len=*),parameter,public :: bg_black    =  CODE_START//COLOR_BG_BLACK//CODE_END
character(len=*),parameter,public :: bg_default  =  CODE_START//COLOR_BG_DEFAULT//CODE_END

! attributes
character(len=*),parameter,public :: bold        =  CODE_START//BOLD_ON//CODE_END
character(len=*),parameter,public :: italic      =  CODE_START//ITALIC_ON//CODE_END
character(len=*),parameter,public :: inverse     =  CODE_START//INVERSE_ON//CODE_END
character(len=*),parameter,public :: underline   =  CODE_START//UNDERLINE_ON//CODE_END
character(len=*),parameter,public :: unbold      =  CODE_START//BOLD_OFF//CODE_END
character(len=*),parameter,public :: unitalic    =  CODE_START//ITALIC_OFF//CODE_END
character(len=*),parameter,public :: uninverse   =  CODE_START//INVERSE_OFF//CODE_END
character(len=*),parameter,public :: ununderline =  CODE_START//UNDERLINE_OFF//CODE_END

character(len=*),parameter,public :: reset       =  CODE_RESET
character(len=*),parameter,public :: clear       =  HOME_DISPLAY//CLEAR_DISPLAY

interface
    function M_escape_initialize() result(r) bind(c, name="M_escape_initialize")
        import c_int
        integer(kind=c_int) :: r
    end function
end interface


contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    esc(3f) - [M_escape] substitute escape sequences for HTML-like syntax
!!              in strings
!!
!!##SYNOPSIS
!!
!!     function esc(string,clear_at_end) result (expanded)
!!
!!       character(len=*),intent(in) :: string
!!       logical,intent(in),optional :: clear_at_end
!!       character(len=:),allocatable :: expanded
!!
!!##DESCRIPTION
!!    Use HTML-like syntax to add attributes to terminal output such as color.
!!
!!    ANSI escape sequences are not universally supported by all terminal
!!    emulators; and normally should be suppressed when not going to a
!!    tty device. This routine provides the basic structure to support
!!    such behaviors.
!!
!!##OPTIONS
!!    string        input string  of form
!!
!!                    "<attribute_name>string</attribute_name> ...".
!!
!!                   where the current attributes are color names,
!!                   bold, italic, underline, ...
!!
!!    clear_at_end   By default, a sequence to clear all text attributes
!!                   is sent at the end of the returned text if an escape
!!                   character appears in the output string. This can be
!!                   turned off by setting this value to false.
!!##KEYWORDS
!!    current keywords
!!
!!     colors:
!!       r,         red,       R,  RED
!!       g,         green,     G,  GREEN
!!       b,         blue,      B,  BLUE
!!       m,         magenta,   M,  MAGENTA
!!       c,         cyan,      C,  CYAN
!!       y,         yellow,    Y,  YELLOW
!!       e,         ebony,     E,  EBONY
!!       w,         white,     W,  WHITE
!!     attributes:
!!       it,        italic
!!       bo,        bold
!!       un,        underline
!!      other:
!!       clear
!!       esc,       escape
!!       default
!!       gt
!!       lt
!!
!!    By default, if the color mnemonics (ie. the keywords) are uppercase
!!    they change the background color. If lowercase, the foreground color.
!!
!!    The "default" keyword is typically used explicitly when
!!    clear_at_end=.false.
!!
!!    Add, delete, and replace what strings are produced using UPDATE(3f).
!!
!!##LIMITATIONS
!!      o colors are not nestable, keywords are case-sensitive,
!!      o not all terminals obey the sequences. On Windows, it is best if
!!        you use Windows 10+ and/or the Linux mode; although it has worked
!!        with all CygWin and MinGW and Putty windows and mintty.
!!      o you should use "<gt>" and "<lt>" instead of ">" and "<" in a string
!!        processed by esc(3f) instead of in any plain text output so that
!!        the raw mode will create correct input for the esc(3f) function
!!        if read back in.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_esc
!!    use M_escape, only : esc, esc_mode, update
!!       write(*,'(a)') esc('<clear>TEST DEFAULTS:')
!!       call printstuff()
!!
!!       write(*,'(a)') esc('TEST MANNER=PLAIN:')
!!       call esc_mode(manner='plain')
!!       call printstuff()
!!
!!       write(*,'(a)') esc('TEST MANNER=RAW:')
!!       call esc_mode(manner='raw')
!!       call printstuff()
!!
!!       write(*,'(a)') esc('TEST MANNER=color:')
!!       call esc_mode(manner='color')
!!       call printstuff()
!!
!!       write(*,'(a)') esc('TEST ADDING A CUSTOM SEQUENCE:')
!!       call update('blink',char(27)//'[5m')
!!       call update('/blink',char(27)//'[38m')
!!       write(*,'(a)') esc('<blink>Items for Friday</blink>')
!!
!!    contains
!!    subroutine printstuff()
!!
!!      write(*,'(a)') esc('<r>RED</r>,<g>GREEN</g>,<b>BLUE</b>')
!!      write(*,'(a)') esc('<c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y>')
!!      write(*,'(a)') esc('<w>WHITE</w> and <e>EBONY</e>')
!!
!!      write(*,'(a)') esc('Adding <bo>bold</bo>')
!!      write(*,'(a)') esc('<bo><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></bo>')
!!      write(*,'(a)') esc('<bo><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y></bo>')
!!      write(*,'(a)') esc('<bo><w>WHITE</w> and <e>EBONY</e></bo>')
!!
!!      write(*,'(a)') esc('Adding <ul>underline</ul>')
!!      write(*,'(a)') esc(&
!!       &'<bo><ul><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></ul></bo>')
!!      write(*,'(a)') esc(&
!!       &'<bo><ul><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y></ul></bo>')
!!      write(*,'(a)') esc('<bo><ul><w>WHITE</w> and <e>EBONY</e></ul></bo>')
!!
!!      write(*,'(a)') esc('Adding <ul>italic</ul>')
!!      write(*,'(a)') esc(&
!!       &'<bo><ul><it><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></it></ul></bo>')
!!      write(*,'(a)') esc(&
!!       &'<bo><ul><it><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</it></y></ul></bo>')
!!      write(*,'(a)') esc('<bo><ul><it><w>WHITE</w> and <e>EBONY</e></ul></bo>')
!!
!!      write(*,'(a)') esc('Adding <in>inverse</in>')
!!      write(*,'(a)') esc(&
!!       &'<in><bo><ul><it><r>RED</r>,<g>GREEN</g>,&
!!       &<b>BLUE</b></it></ul></bo></in>')
!!      write(*,'(a)') esc(&
!!       &'<in><bo><ul><it><c>CYAN</c>,<m>MAGENTA</g>,&
!!       &<y>YELLOW</it></y></ul></bo></in>')
!!      write(*,'(a)') esc(&
!!       &'<in><bo><ul><it><w>WHITE</w> and <e>EBONY</e></ul></bo></in>')
!!    end subroutine printstuff
!!
!!    end program demo_esc
function esc(string,clear_at_end) result (expanded)
character(len=*),intent(in)  :: string
logical,intent(in),optional  :: clear_at_end
logical                      :: clear_at_end_local
character(len=:),allocatable :: padded
character(len=:),allocatable :: expanded
character(len=:),allocatable :: name
integer                      :: i
integer                      :: ii
integer                      :: maxlen
integer                      :: place
if(present(clear_at_end))then
   clear_at_end_local=clear_at_end
else
   clear_at_end_local=.false.
endif
if(.not.allocated(mode))then  ! set substitution mode
   mode='color' ! 'color'|'raw'|'plain'
   call vt102()
endif

if(mode=='raw')then
   expanded=string
   return
endif

maxlen=len(string)
padded=string//' '
i=1
expanded=''
do
   if(debug)write(*,*)'DEBUG:*esc*: processing',padded(i:i),' from',string(i:),' EXPANDED=',expanded
   select case(padded(i:i))
   case('>')  ! should not get here unless unmatched
      i=i+1
      expanded=expanded//'>'
   case('<')  ! assuming not nested for now
      ii=index(padded(i+1:),'>')
      if(ii == 0)then
         expanded=expanded//'<'
         i=i+1
      else
         name=padded(i+1:i+ii-1)
         name=trim(adjustl(name))
         if(debug)write(*,*)'DEBUG:*esc* 1: NAME=',name,get(name)
         call locate(keywords,name,place)
         if(debug)write(*,*)'DEBUG:*esc* 1: LOCATE=',place

         if(mode == 'plain')then
         elseif(place <= 0)then     ! unknown name; print what you found
            expanded=expanded//padded(i:i+ii)
         else
            expanded=expanded//get(name)
         endif
         if(name == 'debug')debug=.true.   !! development version
         if(name == '/debug')debug=.false. !! development version
         i=ii+i+1
      endif
   case default
      expanded=expanded//padded(i:i)
      i=i+1
   end select
   if(i >= maxlen+1)exit
enddo
if( (index(expanded,escape) /= 0).and.(.not.clear_at_end_local))then
   if((mode /= 'raw').and.(mode /= 'plain'))then
      expanded=expanded//CODE_RESET                                   ! Clear all styles
   endif
endif
end function esc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    color(3f) - [M_escape] colorize text using a simple function-based approach
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    use M_escape, only : color, color_mode, &
!!
!!     ! FOREGROUND COLORS
!!         & fg_red, fg_cyan, fg_magenta, fg_blue, &
!!         & fg_green, fg_yellow, fg_white, fg_ebony, &
!!         & fg_default, &
!!     ! BACKGROUND COLORS
!!         & bg_red, bg_cyan, bg_magenta, bg_blue, &
!!         & bg_green, bg_yellow, bg_white, bg_ebony, &
!!         & bg_default, &
!!      ! ATTRIBUTES
!!         & bold, italic, inverse, underline,  &
!!         & unbold, unitalic, uninverse, ununderline,  &
!!         & reset, &
!!      ! DISPLAY
!!         & clear
!!
!!     function color(string,fg,bg,style) result (out)
!!
!!      character(len=*),intent(in)          :: string
!!      character(len=*),intent(in),optional :: fg
!!      character(len=*),intent(in),optional :: bg
!!      character(len=*),intent(in),optional :: style
!!
!!##DESCRIPTION
!!     The color constant strings can be used directly but unconditionally
!!     in an output statement. To allow the attributes to be ignored they
!!     can be called with the color(3f) routine, which the color_mode(3f)
!!     procedure can be used to toggle on and off. Note that this routine
!!     does an implicit reset at the end of each use.
!!
!!##OPTIONS
!!    string     string to assign attributes to
!!    fg         foreground color constant
!!    bg         background color constant
!!    style      style keyword or concatenated style keywords
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_color
!!       use M_escape, only : color, color_mode, &
!!      ! FOREGROUND COLORS
!!         & fg_red, fg_cyan, fg_magenta, fg_blue, &
!!         & fg_green, fg_yellow, fg_white, fg_ebony, &
!!         & fg_default, &
!!      ! BACKGROUND COLORS
!!         & bg_red, bg_cyan, bg_magenta, bg_blue, &
!!         & bg_green, bg_yellow, bg_white, bg_ebony, &
!!         & bg_default, &
!!      ! ATTRIBUTES
!!         & bold, italic, inverse, underline,  &
!!         & unbold, unitalic, uninverse, ununderline,  &
!!         & reset, &
!!      ! DISPLAY
!!         & clear
!!       implicit none
!!         write(*,'(*(g0))')fg_red,bg_green,bold,' Hello! ',reset
!!
!!         write(*,'(a)')color(' Hello! ',&
!!          & fg=fg_white,bg=bg_red,style=italic//bold)
!!         call color_mode(.false.)
!!         write(*,'(a)')color(' Hello! ',&
!!          & fg=fg_red,bg=bg_red,style=italic//bold)
!!    end program demo_color
!!
!!##AUTHOR
!!    John S. Urban, 2020
!!##LICENSE
!!    Public Domain
function color(string,fg,bg,style) result (out)

! ident_1="@(#) use the color string constants optionally ignoring them if G_switch is .false. as set by color_mode(3f)"

character(len=*),intent(in)          :: string
character(len=*),intent(in),optional :: fg
character(len=*),intent(in),optional :: bg
character(len=*),intent(in),optional :: style
character(len=:),allocatable         :: out
out=''
if(G_color)then
   if(present(style))out=out//style
   if(present(bg))out=out//bg
   if(present(fg))out=out//fg
   out=out//string
   out=out//reset
else
   out=string
endif
end function color
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    color_mode(3f) - [M_escape] toggle style effects of color(3f) on and off
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine color_mode(switch)
!!
!!      logical,intent(in) :: switch
!!
!!##DESCRIPTION
!!     The color constant strings can be used directly but unconditionally
!!     in an output statement. To allow the attributes to be ignored they
!!     can be called with the color(3f) routine, which the color_mode(3f)
!!     procedure can be used to toggle on and off. Note that this routine
!!     does an implicit reset at the end of each use.
!!
!!##OPTIONS
!!     switch   turn attributes set by color(3f) on and off
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_color_mode
!!       use M_escape, only : color, color_mode, &
!!      ! FOREGROUND COLORS
!!         & fg_red, fg_cyan, fg_magenta, fg_blue, &
!!         & fg_green, fg_yellow, fg_white, fg_ebony, &
!!         & fg_default, &
!!      ! BACKGROUND COLORS
!!         & bg_red, bg_cyan, bg_magenta, bg_blue, &
!!         & bg_green, bg_yellow, bg_white, bg_ebony, &
!!         & bg_default, &
!!      ! ATTRIBUTES
!!         & bold, italic, inverse, underline,  &
!!         & unbold, unitalic, uninverse, ununderline,  &
!!         & reset, &
!!      ! DISPLAY
!!         & clear
!!       implicit none
!!         write(*,'(*(g0))')fg_red,bg_green,bold,' Hello! ',reset
!!
!!         write(*,'(a)')color(' Hello! ',&
!!          & fg=fg_white,bg=bg_red,style=italic//bold)
!!         call color_mode(.false.)
!!         write(*,'(a)')color(' Hello! ',&
!!          & fg=fg_red,bg=bg_red,style=italic//bold)
!!    end program demo_color_mode
!!
!!##AUTHOR
!!    John S. Urban, 2020
!!##LICENSE
!!    Public Domain
subroutine color_mode(switch)
logical,intent(in) :: switch
   G_color=switch
end subroutine color_mode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine vt102()
! create a dictionary with character keywords, values, and value lengths
! using the routines for maintaining a list

   call wipe_dictionary()
   ! insert and replace entries

   call update('bold',bold)
   call update('/bold',unbold)
   call update('bo',bold)
   call update('/bo',unbold)
   call update('livid',bold)
   call update('/livid',unbold)
   call update('li',bold)
   call update('/li',unbold)

   call update('italic',italic)
   call update('/italic',unitalic)
   call update('it',italic)
   call update('/it',unitalic)

   call update('inverse',inverse)
   call update('/inverse',uninverse)
   call update('in',inverse)
   call update('/in',uninverse)

   call update('underline',underline)
   call update('/underline',ununderline)
   call update('un',underline)
   call update('/un',ununderline)

   call update('esc',ESCAPE)
   call update('escape',ESCAPE)

   call update('clear',clear)
   call update('reset',reset)
   call update('bell',BELL)
   call update('n',NL)
   call update('t',HT)
   call update('gt','>')
   call update('lt','<')

   ! foreground colors
   call update('r',fg_red)
       call update('/r',fg_default)
       call update('red',fg_red)
       call update('/red',fg_default)
   call update('c',fg_cyan)
       call update('/c',fg_default)
       call update('cyan',fg_cyan)
       call update('/cyan',fg_default)
   call update('m',fg_magenta)
       call update('/m',fg_default)
       call update('magenta',fg_magenta)
       call update('/magenta',fg_default)
   call update('b',fg_blue)
       call update('/b',fg_default)
       call update('blue',fg_blue)
       call update('/blue',fg_default)
   call update('g',fg_green)
       call update('/g',fg_default)
       call update('green',fg_green)
       call update('/green',fg_default)
   call update('y',fg_yellow)
       call update('/y',fg_default)
       call update('yellow',fg_yellow)
       call update('/yellow',fg_default)
   call update('w',fg_white)
       call update('/w',fg_default)
       call update('white',fg_white)
       call update('/white',fg_default)
   call update('e',fg_ebony)
       call update('/e',fg_default)
       call update('ebony',fg_ebony)
       call update('/ebony',fg_default)
   call update('x',fg_ebony)
       call update('/x',fg_default)
       call update('black',fg_ebony)
       call update('/black',fg_default)

   ! background colors
   call update('R',bg_red)
       call update('/R',bg_default)
       call update('RED',bg_red)
       call update('/RED',bg_default)
   call update('C',bg_cyan)
       call update('/C',bg_default)
       call update('CYAN',bg_cyan)
       call update('/CYAN',bg_default)
   call update('M',bg_magenta)
       call update('/M',bg_default)
       call update('MAGENTA',bg_magenta)
       call update('/MAGENTA',bg_default)
   call update('B',bg_blue)
       call update('/B',bg_default)
       call update('BLUE',bg_blue)
       call update('/BLUE',bg_default)
   call update('G',bg_green)
       call update('/G',bg_default)
       call update('GREEN',bg_green)
       call update('/GREEN',bg_default)
   call update('Y',bg_yellow)
       call update('/Y',bg_default)
       call update('YELLOW',bg_yellow)
       call update('/YELLOW',bg_default)
   call update('W',bg_white)
       call update('/W',bg_default)
       call update('WHITE',bg_white)
       call update('/WHITE',bg_default)
   call update('E',bg_ebony)
       call update('/E',bg_default)
       call update('EBONY',bg_ebony)
       call update('/EBONY',bg_default)
   call update('X',bg_ebony)
       call update('/X',bg_default)
       call update('BLACK',bg_ebony)
       call update('/BLACK',bg_default)

end subroutine vt102
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    esc_mode(3f) - [M_escape] select processing mode for output from esc(3f)
!!
!!##SYNOPSIS
!!
!!    subroutine esc_mode(manner)
!!
!!       character(len=*),intent(in) :: manner
!!
!!##DESCRIPTION
!!       Turn off the generation of strings associated with the HTML keywords
!!       in the string generated by the esc(3f) function, or display the
!!       text in raw mode as it was passed to esc(3f) or return to ANSI
!!       escape control sequence generation.
!!
!!##OPTIONS
!!    MANNER  The current manners or modes supported via the ESC_MODE(3f)
!!            procedure are
!!
!!        plain          suppress the output associated with keywords
!!        color(default) commonly supported escape sequences
!!        raw            echo the input to ESC(3f) as its output
!!        reload         restore original keyword meanings deleted or
!!                       replaced by calls to update(3f).
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_esc_mode
!!    use M_escape, only : esc, esc_mode
!!    implicit none
!!    character(len=1024) :: line
!!    real :: value
!!
!!      value=3.4567
!!      if( (value>0.0) .and. (value<100.0))then
!!        write(line,fmt='("&
!!       &<w><G>GREAT</G></w>: The value <Y><b>",f8.4,"</b></Y> is in range &
!!       &")')value
!!      else
!!        write(line,fmt='("&
!!       &<R><e>ERROR</e></R>:The new value <Y><b>",g0,"</b></Y> is out of range&
!!       & ")')value
!!      endif
!!
!!      write(*,'(a)')esc(trim(line))
!!
!!      call esc_mode(manner='plain') ! write as plain text
!!      write(*,'(a)')esc(trim(line))
!!      call esc_mode(manner='raw')   ! write as-is
!!      write(*,'(a)')esc(trim(line))
!!      call esc_mode(manner='ansi')  ! return to default mode
!!      write(*,'(a)')esc(trim(line))
!!
!!    end program demo_esc_mode
subroutine esc_mode(manner)
character(len=*),intent(in) :: manner
   if(.not.allocated(mode))then  ! set substitution mode
      mode='color'
      call vt102()
   endif
   select case(manner)
   case('vt102','ANSI','ansi','color','COLOR')
      mode='color'
   case('reload')
      call vt102()
      mode='color'
   case('xterm')
      mode=manner
   case('raw')
      mode=manner
   case('dummy','plain','text')
      mode='plain'
   case default
      write(*,*)'unknown manner. Try color|raw|plain'
      mode='color'
   end select
end subroutine esc_mode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine wipe_dictionary()
   if(allocated(keywords))deallocate(keywords)
   allocate(character(len=0) :: keywords(0))
   if(allocated(values))deallocate(values)
   allocate(character(len=0) :: values(0))
   if(allocated(counts))deallocate(counts)
   allocate(counts(0))
end subroutine wipe_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    update(3f) - [M_escape] update internal dictionary given keyword and value
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine update(key,val)
!!
!!    character(len=*),intent(in)           :: key
!!    character(len=*),intent(in),optional  :: val
!!
!!##DESCRIPTION
!!    Update internal dictionary in M_escape(3fm) module.
!!
!!##OPTIONS
!!    key  name of keyword to add, replace, or delete from dictionary
!!    val  if present add or replace value associated with keyword. If not
!!         present remove keyword entry from dictionary.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_update
!!    use M_escape, only : esc, update
!!       write(*,'(a)') esc('<clear>TEST CUSTOMIZED:')
!!       ! add custom keywords
!!       call update('blink',char(27)//'[5m')
!!       call update('/blink',char(27)//'[38m')
!!
!!       write(*,'(a)') esc('<blink>Items for Friday</blink>')
!!
!!       write(*,'(a)',advance='no') esc('<r>RED</r>,')
!!       write(*,'(a)',advance='no') esc('<b>BLUE</b>,')
!!       write(*,'(a)',advance='yes') esc('<g>GREEN</g>')
!!
!!       ! delete
!!       call update('r')
!!       call update('/r')
!!       ! replace
!!       call update('b','<<<<')
!!       call update('/b','>>>>')
!!       write(*,'(a)',advance='no') esc('<r>RED</r>,')
!!       write(*,'(a)',advance='no') esc('<b>BLUE</b>,')
!!       write(*,'(a)',advance='yes') esc('<g>GREEN</g>')
!!
!!    end program demo_update
!!
!!##AUTHOR
!!    John S. Urban, 2020
!!
!!##LICENSE
!!    Public Domain
subroutine update(key,valin)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: valin
integer                               :: place
integer                               :: iilen
character(len=:),allocatable          :: val
if(present(valin))then
   val=valin
   iilen=len_trim(val)
   ! find where string is or should be
   call locate(keywords,key,place)
   ! if string was not found insert it
   if(place < 1)then
      call insert(keywords,key,iabs(place))
      call insert(values,val,iabs(place))
      call insert(counts,iilen,iabs(place))
   else
      call replace(values,val,place)
      call replace(counts,iilen,place)
   endif
else
   call locate(keywords,key,place)
   if(place > 0)then
      call remove(keywords,place)
      call remove(values,place)
      call remove(counts,place)
   endif
endif
end subroutine update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate(keywords,key,place)
   if(place < 1)then
      valout=''
   else
      valout=values(place)(:counts(place))
   endif
end function get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    print_dictionary(3f) - [ARGUMENTS:M_CLI2] print internal dictionary
!!                           created by calls to update(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    subroutine print_dictionary(header)
!!
!!     character(len=*),intent(in),optional :: header
!!
!!##DESCRIPTION
!!    Print the internal dictionary created by calls to update(3f).
!!    This routine is intended to print the state of the argument list
!!    if an error occurs in using the update(3f) procedure.
!!
!!##OPTIONS
!!     HEADER  label to print before printing the state of the command
!!             argument list.
!!##EXAMPLE
!!
!!
!!   Typical usage:
!!
!!    program demo_print_dictionary
!!    use M_escape, only : esc, update, print_dictionary
!!    implicit none
!!       write(*,'(a)') esc('<clear>TEST CUSTOMIZED:')
!!       ! add custom keywords
!!       call update('blink',char(27)//'[5m')
!!       call update('/blink',char(27)//'[38m')
!!       call print_dictionary('DICTIONARY')
!!       write(*,'(a)') esc('<blink>Items for Friday</blink>')
!!    end program demo_print_dictionary
!!
!!   Sample output
!!
!!    demo_print_dictionary |cat -v -e -t
!!
!!##AUTHOR
!!      John S. Urban, 2020
!!
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine print_dictionary(header)
character(len=*),intent(in),optional :: header
integer          :: i
   if(present(header))then
      if(header /= '')then
         write(stderr,'(a)')header
      endif
   endif
   if(allocated(keywords))then
      if(size(keywords) > 0)then
         write(stderr,'(*(a,t30,a))')'KEYWORD','VALUE'
         write(stderr,'(*(a,t30,"[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
      endif
   endif
end subroutine print_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine split(input_line,array,delimiters)
!-----------------------------------------------------------------------------------------------------------------------------------

!$@(#) M_escape::split(3f): parse string on delimiter characters and store tokens into an allocatable array

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
integer                       :: ii                     ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: iilen                  ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters /= '')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   if(allocated(iterm))deallocate(iterm)
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   iilen=len(input_line)                                          ! IILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (iilen)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (:0)                                                      ! command was totally blank
!-----------------------------------------------------------------------------------------------------------------------------------
   case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,iilen,1                                  ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)) == 0)then  ! if current character is not a delimiter
            iterm(i30)=iilen                                      ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):iilen),dlim(i10:i10))
               IF(ifound > 0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol > iilen)then                                    ! no text left
            exit INFINITE
         endif
      enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
      ireturn=inotnull
   if(allocated(array))deallocate(array)
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
!-----------------------------------------------------------------------------------------------------------------------------------
   ii=1
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20) < ibegin(i20))then
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+1
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if(allocated(ibegin))deallocate(ibegin)
   if(allocated(iterm))deallocate(iterm)
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    attr(3f) - [M_escape] colorize text using a simple function-based approach
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function attr(attribute) result (out)
!!
!!    character(len=*),intent(in)  :: attribute
!!    character(len=:),allocatable :: out
!!
!!##DESCRIPTION
!!    attr(3f) uses the same keywords as esc(3f) to send ANSI escape
!!    sequences to the display screen, except instead of using a pseudo-HTML
!!    string to select the codes it uses a simple colon-delimited list of
!!    the keywords.
!!
!!##OPTIONS
!!    attribute  colon, space, or comma-delimited list of attribute keywords
!!               as defined in the esc(3f) procedure.
!!    text       if supplied it is printed and then an attribute reset is added
!!
!!##RETURNS
!!    out        output the strings assigned by the keywords (by default
!!    ANSI video
!!               display escape sequences, see update(3f) )
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_attr
!!    use M_escape, only : attr, esc_mode
!!    implicit none
!!         call printme('color')
!!         call printme('plain')
!!         call printme('raw')
!!    contains
!!    subroutine printme(mymode)
!!    character(len=*),intent(in) :: mymode
!!       call esc_mode(mymode)
!!       write(*,'(a)')mymode
!!       write(*,'(*(g0))',advance='no')attr('red:BLUE:bold','Hello!'), &
!!        & 'and everything is back to defaults or ', &
!!        & attr('RED:blue:bold'),'Hello Again!', &
!!        & attr('/RED'),' Well, the text color is still blue.',attr('reset')
!!       write(*,'(*(g0))',advance='yes')' Back to a normal write statement.'
!!    end subroutine printme
!!    end program demo_attr
!!
!!##AUTHOR
!!    John S. Urban, 2020
!!
!!##LICENSE
!!    Public Domain
function attr(attribute,text) result(out)
! colon,space, or comma-delimited string of attributes
character(len=*),intent(in)          :: attribute
character(len=*),intent(in),optional :: text
character(len=:),allocatable         :: out
character(len=:),allocatable         :: array(:)
integer                              :: i
   if(.not.allocated(mode))then  ! set substitution mode
      mode='color'
      call vt102()
   endif
   out=''
   call split(attribute,array,delimiters=' :,')
   do i=1,size(array)
      if(mode=='raw')then
         out=out//'<'//trim(array(i))//'>'
      elseif(mode=='plain')then
         out=''
      else
         out=out//get(trim(array(i)))
      endif
   enddo
   if(present(text))then
   out=out//text//esc('<reset>')
   endif
end function attr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! one question would be if the strings should not be parameters so you could flush and reset them.
!-!subroutine color_reset()
!-!! for direct use of escape sequences
!-!
!-!! foreground colors
!-! fg_red      =  CODE_START//COLOR_FG_RED//CODE_END
!-! fg_cyan     =  CODE_START//COLOR_FG_CYAN//CODE_END
!-! fg_magenta  =  CODE_START//COLOR_FG_MAGENTA//CODE_END
!-! fg_blue     =  CODE_START//COLOR_FG_BLUE//CODE_END
!-! fg_green    =  CODE_START//COLOR_FG_GREEN//CODE_END
!-! fg_yellow   =  CODE_START//COLOR_FG_YELLOW//CODE_END
!-! fg_white    =  CODE_START//COLOR_FG_WHITE//CODE_END
!-! fg_ebony    =  CODE_START//COLOR_FG_BLACK//CODE_END
!-! fg_default  =  CODE_START//COLOR_FG_DEFAULT//CODE_END
!-!
!-!! background colors
!-! bg_red      =  CODE_START//COLOR_BG_RED//CODE_END
!-! bg_cyan     =  CODE_START//COLOR_BG_CYAN//CODE_END
!-! bg_magenta  =  CODE_START//COLOR_BG_MAGENTA//CODE_END
!-! bg_blue     =  CODE_START//COLOR_BG_BLUE//CODE_END
!-! bg_green    =  CODE_START//COLOR_BG_GREEN//CODE_END
!-! bg_yellow   =  CODE_START//COLOR_BG_YELLOW//CODE_END
!-! bg_white    =  CODE_START//COLOR_BG_WHITE//CODE_END
!-! bg_ebony    =  CODE_START//COLOR_BG_BLACK//CODE_END
!-! bg_default  =  CODE_START//COLOR_BG_DEFAULT//CODE_END
!-!
!-!! attributes
!-! bold        =  CODE_START//BOLD_ON//CODE_END
!-! italic      =  CODE_START//ITALIC_ON//CODE_END
!-! inverse     =  CODE_START//INVERSE_ON//CODE_END
!-! underline   =  CODE_START//UNDERLINE_ON//CODE_END
!-! unbold      =  CODE_START//BOLD_OFF//CODE_END
!-! unitalic    =  CODE_START//ITALIC_OFF//CODE_END
!-! uninverse   =  CODE_START//INVERSE_OFF//CODE_END
!-! ununderline =  CODE_START//UNDERLINE_OFF//CODE_END
!-!
!-! reset       =  CODE_RESET
!-! clear       =  CLEAR_DISPLAY
!-!subroutine color_reset()
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!-!subroutine color_flush()
!-!! for direct use of escape sequences
!-!
!-!! foreground colors
!-! fg_red      =  ''
!-! fg_cyan     =  ''
!-! fg_magenta  =  ''
!-! fg_blue     =  ''
!-! fg_green    =  ''
!-! fg_yellow   =  ''
!-! fg_white    =  ''
!-! fg_ebony    =  ''
!-! fg_default  =  ''
!-!
!-!! background colors
!-! bg_red      =  ''
!-! bg_cyan     =  ''
!-! bg_magenta  =  ''
!-! bg_blue     =  ''
!-! bg_green    =  ''
!-! bg_yellow   =  ''
!-! bg_white    =  ''
!-! bg_ebony    =  ''
!-! bg_default  =  ''
!-!
!-!! attributes
!-! bold        =  ''
!-! italic      =  ''
!-! inverse     =  ''
!-! underline   =  ''
!-! unbold      =  ''
!-! unitalic    =  ''
!-! uninverse   =  ''
!-! ununderline =  ''
!-!
!-! reset       =  CODE_RESET
!-! clear       =  CLEAR_DISPLAY
!-!end subroutine color_flush
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_escape
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================


!>>>>> app/main.f90
program fpm_M_CLI2
use M_CLI2_docs, only : help_intrinsics
use M_CLI2,       only : set_args, sget, lget, topics=>unnamed
use M_match,      only : getpat, match, regex_pattern
use M_match,      only : YES, ERR
use M_strings,    only : lower, indent, atleast
use M_escape,     only : esc
implicit none
type(regex_pattern)          :: p
character(len=:),allocatable :: help_text(:), version_text(:)
character(len=256),allocatable :: manual(:),section(:)
character(len=:),allocatable :: regex
integer                      :: i, j, k
logical                      :: topic
logical                      :: prefix, ignorecase, demo, color
integer                      :: start_keep, end_keep
    ! process command line
    call setup()
    call set_args(' --regex:e " " --ignorecase:i F --topic_only:t F --demo:d F --color:c',&
    & help_text,version_text)
    regex=sget('regex')
    topic=lget('topic_only')
    ignorecase=lget('ignorecase')
    demo=lget('demo')
    color=lget('color')

    ! if -t then just show topic names and exit
    if(topic)then
       manual = help_intrinsics('',topic=topic)
       ! could truncate if name is too long, could get a bit fancier or use
       ! M_display(3f) and have default just print one per line
       write(*,'(3(g0))') ( [character(len=80/3) :: manual(i)], i=1, size(manual) )
       stop
    endif

    ! compile any regular expression
    ! Also, if doing a regular expression and not the single topic "toc"
    ! add a section prefix when building manual


    ! initially assume prefixing is off unless a regular expression is used
    if(regex.ne.' ')then
       prefix=.true.
    else
       prefix=.false.
    endif

    ! normalize the topics list
    ! ensure there is at least one topic by applying a default
    if(size(topics).eq.0)then
       topics=['toc']
    endif

    if( ( size(topics).eq.1 .and. topics(1).eq.'toc') )then
       prefix=.false.
       ignorecase=.true.
    endif

    if(regex.ne.' ')then
       if (getpat(merge(lower(regex),regex,ignorecase), p%pat) .eq. ERR) then
          stop '*fpm-m_cli2* Illegal pattern.'
       endif
    endif

    if(lget('verbose'))then
       write(*,'(*(g0:,1x))')'<INFO>AFTER NORMALIZING:'
       write(*,'(*(g0:,1x))')'<INFO>REGEX       ',regex
       write(*,'(*(g0:,1x))')'<INFO>IGNORECASE  ',ignorecase
       write(*,'(*(g0:,1x))')'<INFO>TOPIC_ONLY  ',topic
       write(*,'(*(g0:,1x))')'<INFO>PREFIX      ',prefix
       write(*,'(*(g0:,1x))')'<INFO>DEMO        ',demo
       write(*,'(*(g0:,1x))')'<INFO>TOPICS      ',topics
    endif
    ! build text to display or search
    manual=[character(len=0) ::]
    do i=1, size(topics)
       section = help_intrinsics(topics(i),prefix=prefix)
       if(color)section=crayons(section)

       ! extract demo program if found (has to follow specific format)
       if(demo)then
          call find_demo()
       endif

       manual = [character(len=max(len(manual),len(section))) :: manual,section]
    enddo

    ! display selected text
    if(size(manual).eq.0)then
       write(*,'(g0)')'Sorry. did not find that. Perhaps you should search the TOC. try'
       write(*,'(g0)')'   fpm-m_cli2 -e TOPIC'
       write(*,'(g0)')'or search the entire manual:'
       write(*,'(g0)')'   fpm-m_cli2 manual -i -e TOPIC'
       stop 1
    else
       ! display what was found
       do i=1,size(manual)
          if(regex.ne.'')then
             select case(ignorecase)
             case(.true.)
                if(match(lower(trim(manual(i)))//char(10), p%pat) .eq. YES) then
                  write(*,'(g0)')trim(manual(i))
                endif
             case(.false.)
                if (match(trim(manual(i))//char(10), p%pat) .eq. YES) then
                  write(*,'(g0)')trim(manual(i))
                endif
             end select
          else
             write(*,'(g0)')trim(manual(i))
          endif
       enddo
    endif
contains
subroutine find_demo()
character(len=256),allocatable :: newsection(:)
integer :: ii,jj,kk
   if(allocated(newsection)) deallocate(newsection)
   allocate(newsection(0))
   if(demo)then
      start_keep=0
      end_keep=0
      jj=0
      do ii=1,size(section)
         jj=jj+1
         if(jj.gt.size(section))exit
         if(index(lower(section(jj)),'program demo_').ne.0)then
            start_keep=jj
            do kk=start_keep+1,size(section)
               if(kk.gt.size(section))exit
               if(index(lower(section(kk)),'end program demo_').ne.0)then
                  end_keep=kk
                  if(start_keep.ne.0 .and. end_keep.ne.0)then
                     newsection=[character(len=max(len(newsection),len(section))) :: newsection,section(start_keep:end_keep)]
                     jj=kk+1
                  endif
                  exit
               endif
            enddo
         endif
      enddo
    endif
    if(size(newsection).eq.0)then
       write(*,*)'!<ERROR> *fpm-m_cli2* standard demo code format not found for ',trim(topics(i))
       section=['']
    else
       section=newsection
       deallocate(newsection)
    endif
end subroutine find_demo
function crayons(oldblock) result(newblock)
! just playing. There is a lot of stuff not done robustly here
character(len=256),intent(in),allocatable :: oldblock(:)
character(len=256),allocatable :: newblock(:)
integer :: ilen
integer :: lead
logical :: program_text
   program_text=.false.
   newblock= oldblock
   do j=1,size(oldblock)
      if( index(oldblock(j),'end program demo_') .eq. 0 .and. index(oldblock(j),'program demo_') .ne. 0)then
         program_text=.true.
         lead=indent(oldblock(j))
      endif
      if(program_text .eqv. .true.)then
        newblock(j)=esc('<E>'//repeat(' ',lead)//'<E><y>'//atleast(oldblock(j)(lead+1:),80-lead) )
      elseif(verify(oldblock(j)(1:1), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ) == 0 .and. &
      & verify(oldblock(j), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ _') == 0 )then
         ilen=len_trim(oldblock(j))
         newblock(j)=esc('<E><y><bo> '//trim(oldblock(j))//' </bo>'//repeat(' ',max(0,80-ilen-2))//'<reset>')
       else
          ilen=len_trim(oldblock(j))
          ilen=len_trim(than(oldblock(j)))-ilen
          newblock(j)=esc('<E><w>'//atleast(than(oldblock(j)),80+ilen)//'<reset>')
       endif
      if( index(oldblock(j),'end program demo_') .ne.0)then
         program_text=.false.
      endif
   enddo
end function crayons

function than(in) result(out)
character(len=*),intent(in)  :: in
character(len=:),allocatable :: out
integer                      :: i
   out=''
   do i=1,len_trim(in)
      select case(in(i:i))
      case('<')
         out=out//'<lt>'
      case('>')
         out=out//'<gt>'
      case default
         out=out//in(i:i)
      endselect
   enddo
end function than

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'    fpm-m_cli2(1f) - [DEVELOPER] output descriptions of Fortran intrinsics  ',&
'    (LICENSE:PD)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    fpm-m_cli2 NAME(s) [[-ignorecase][--regex Regular_Expression]]|         ',&
'                [-topic_only][--color][--demo]                                  ',&
'                                                                                ',&
'    fpm-m_cli2 [ --help| --version]                                         ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   fpm-m_cli2(1) print descriptions of procedures as simple flat text.      ',&
'                                                                                ',&
'   The text is formatted in the txt2man(1) markdown language so one can easily  ',&
'   generate man-pages on ULS (Unix-Like Systems).                               ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'  TOPIC(s)          A list of Fortran intrinsic names or the special names      ',&
'                    "toc" and "manual" (which generate a table of contents      ',&
'                    and the entire set of documents respecively).               ',&
'                    The default is "toc" and to ignore case.                    ',&
'  --regex,-e        Search all output per the provided Regular Expression.      ',&
'                    Output is prefixed with the topic it was found in.          ',&
'  --itopic_only,-t  Only show topic names. Other switches are ignored.          ',&
'  --ignorecase,-i   Ignore case when searching for a Regular Expression.        ',&
'  --demo,-d         extract first demo program found for a topic (starting with ',&
'                    "program demo_" and ending with "end program demo_").       ',&
'  --color           Use ANSI in-line escape sequences to display the text in    ',&
'                    set colors. Does not work with all terminal emulators or    ',&
'                    terminals. Must use the -r switch with less(1) for less(1)  ',&
'                    to display colors.                                          ',&
'  --help            Display this help and exit                                  ',&
'  --version         Output version information and exit                         ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'  Sample commands                                                               ',&
'                                                                                ',&
'   fpm-m_cli2              # list table of contents                         ',&
'   fpm-m_cli2 -e character # check TOC for string. try "loc","prank","sort" ',&
'   fpm-m_cli2 set_args|less  # display a description of tan(3f)               ',&
'                                                                                ',&
'   fpm-m_cli2 --regex ''character'' # look for string in TOC ignoring case  ',&
'                                                                                ',&
'   fpm-m_cli2 manual>fortran.txt    # create a copy of all descriptions     ',&
'                                                                                ',&
'   # list the topic "response" if found and lines containing "response" from the entire ',&
'   # manual, prefixing the lines with the section name, while ignoring case.    ',&
'   fpm-m_cli2 -e response -i manual                                             ',&
'                                                                                ',&
'   fpm-m_cli2 -d set_mode >demo_set_mode.f90 # get sample program to try SET_MODE(3f).',&
'']

version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:         GPF (General Purpose Fortran) utilities and examples           ',&
'PROGRAM:         fpm-m_cli2(1)                                              ',&
'DESCRIPTION:     output Fortran intrinsic descriptions                          ',&
!'VERSION:         1.0.0, 20201215                                               ',&
!'VERSION:         1.0.1, 20201217                                               ',&
'VERSION:         1.0.2, 202100108                                               ',&
'AUTHOR:          John S. Urban                                                  ',&
'HOME PAGE:       http://www.urbanjost.altervista.org/index.html                 ',&
'LICENSE:         MIT License                                                    ',&
'']

end subroutine setup
end program fpm_M_CLI2
! kludge1: older versions of gfortran do not handle character arrays with both line and size allocatable

