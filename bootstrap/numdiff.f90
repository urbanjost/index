 
!>>>>> build/dependencies/M_CLI2/src/M_CLI2.f90
!VERSION 1.0 20200115
!VERSION 2.0 20200802
!VERSION 3.0 20201021  LONG:SHORT syntax
!VERSION 3.1 20201115  LONG:SHORT:: syntax
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     M_CLI2(3fm) - [ARGUMENTS::M_CLI2] - command line argument parsing
!!     using a prototype command
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   Available procedures and variables:
!!
!!      use M_CLI2, only : set_args, get_args, unnamed, remaining, args
!!      use M_CLI2, only : get_args_fixed_length, get_args_fixed_size
!!      use M_CLI2, only : specified
!!      ! convenience functions
!!      use M_CLI2, only : dget, iget, lget, rget, sget, cget
!!      use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets
!!
!!##DESCRIPTION
!!    Allow for command line parsing much like standard Unix command line
!!    parsing using a simple prototype.
!!
!!    Typically one call to SET_ARGS(3f) is made to define the command
!!    arguments, set default values and parse the command line. Then a call
!!    is made to the convenience commands based on GET_ARGS(3f) for each
!!    command keyword to obtain the argument values.
!!
!!    The documentation for SET_ARGS(3f) and GET_ARGS(3f) provides further
!!    details.
!!
!!##EXAMPLE
!!
!! Sample program using type conversion routines
!!
!!     program demo_M_CLI2
!!     use M_CLI2,  only : set_args, get_args
!!     use M_CLI2,  only : filenames=>unnamed
!!     use M_CLI2,  only : get_args_fixed_length, get_args_fixed_size
!!     implicit none
!!     integer                      :: i
!!     integer,parameter            :: dp=kind(0.0d0)
!!     !
!!     ! DEFINE ARGS
!!     real                         :: x, y, z
!!     real(kind=dp),allocatable    :: point(:)
!!     logical                      :: l, lbig
!!     logical,allocatable          :: logicals(:)
!!     character(len=:),allocatable :: title    ! VARIABLE LENGTH
!!     character(len=40)            :: label    ! FIXED LENGTH
!!     real                         :: p(3)     ! FIXED SIZE
!!     logical                      :: logi(3)  ! FIXED SIZE
!!     !
!!     ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!     !   o set a value for all keywords.
!!     !   o double-quote strings
!!     !   o set all logical values to F or T.
!!     !   o value delimiter is comma, colon, or space
!!     call set_args('                         &
!!             & -x 1 -y 2 -z 3                &
!!             & -p -1 -2 -3                   &
!!             & --point 11.11, 22.22, 33.33e0 &
!!             & --title "my title" -l F -L F  &
!!             & --logicals  F F F F F         &
!!             & -logi F T F                   &
!!             & --label " " &
!!             ! note space between quotes is required
!!             & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!     call get_args('x',x)         ! SCALARS
!!     call get_args('y',y)
!!     call get_args('z',z)
!!     call get_args('l',l)
!!     call get_args('L',lbig)
!!     call get_args('title',title) ! ALLOCATABLE STRING
!!     call get_args('point',point) ! ALLOCATABLE ARRAYS
!!     call get_args('logicals',logicals)
!!     !
!!     ! for NON-ALLOCATABLE VARIABLES
!!
!!     ! for non-allocatable string
!!     call get_args_fixed_length('label',label)
!!
!!     ! for non-allocatable arrays
!!     call get_args_fixed_size('p',p)
!!     call get_args_fixed_size('logi',logi)
!!     !
!!     ! USE VALUES
!!     write(*,*)'x=',x, 'y=',y, 'z=',z, x+y+z
!!     write(*,*)'p=',p
!!     write(*,*)'point=',point
!!     write(*,*)'title=',title
!!     write(*,*)'label=',label
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     write(*,*)'logicals=',logicals
!!     write(*,*)'logi=',logi
!!     !
!!     ! unnamed strings
!!     !
!!     if(size(filenames).gt.0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!     !
!!     end program demo_M_CLI2
!!
!!##AUTHOR
!!     John S. Urban, 2019
!!##LICENSE
!!     Public Domain
!===================================================================================================================================
module M_CLI2
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT, warn=>OUTPUT_UNIT

! copied to M_CLI2 for a stand-alone version
!use M_strings,                     only : upper, lower, quote, replace_str=>replace, unquote, split, string_to_value, atleast
!use M_list,                        only : insert, locate, remove, replace
!use M_args,                        only : longest_command_argument
!use M_journal,                     only : journal

implicit none
integer,parameter,private :: dp=kind(0.0d0)
integer,parameter,private :: sp=kind(0.0)
private
!logical,save :: debug_m_cli2=.true.
logical,public,save :: debug_m_cli2=.false.
!===================================================================================================================================
character(len=*),parameter          :: gen='(*(g0))'
character(len=:),allocatable,public :: unnamed(:)
character(len=:),allocatable,public :: args(:)
character(len=:),allocatable,public :: remaining
public                              :: set_args
public                              :: get_subcommand
public                              :: get_args
public                              :: get_args_fixed_size
public                              :: get_args_fixed_length
public                              :: specified
public                              :: print_dictionary

public                              :: dget, iget, lget, rget, sget, cget
public                              :: dgets, igets, lgets, rgets, sgets, cgets
public                              :: CLI_RESPONSE_FILE

private :: check_commandline
private :: wipe_dictionary
private :: prototype_to_dictionary
private :: update
private :: prototype_and_cmd_args_to_nlist
private :: get

type option
   character(:),allocatable :: shortname
   character(:),allocatable :: longname
   character(:),allocatable :: value
   integer                  :: length
   logical                  :: present_in
   logical                  :: mandatory
end type option
!===================================================================================================================================
character(len=:),allocatable,save :: keywords(:)
character(len=:),allocatable,save :: shorts(:)
character(len=:),allocatable,save :: values(:)
integer,allocatable,save          :: counts(:)
logical,allocatable,save          :: present_in(:)
logical,allocatable,save          :: mandatory(:)

logical,save                      :: G_keyword_single_letter=.true.
character(len=:),allocatable,save :: G_passed_in
logical,save                      :: G_remaining_on, G_remaining_option_allowed
character(len=:),allocatable,save :: G_remaining
character(len=:),allocatable,save :: G_subcommand              ! possible candidate for a subcommand
character(len=:),allocatable,save :: G_STOP_MESSAGE
integer,save                      :: G_STOP
logical,save                      :: G_QUIET
logical,save                      :: G_STRICT                  ! strict short and long rules or allow -longname and --shortname
!----------------------------------------------
! try out response files
logical,save                      :: CLI_RESPONSE_FILE=.false. ! allow @name abbreviations
logical,save                      :: G_APPEND                  ! whether to append or replace when duplicate keywords found
logical,save                      :: G_OPTIONS_ONLY            ! process response file only looking for options for get_subcommand()
logical,save                      :: G_RESPONSE                ! allow @name abbreviations
character(len=:),allocatable,save :: G_RESPONSE_IGNORED
!----------------------------------------------
!===================================================================================================================================
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
!==================================================================================================================================
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
!===================================================================================================================================
!intrinsic findloc
!===================================================================================================================================

! ident_1="@(#)M_CLI2::str(3f): {msg_scalar,msg_one}"

private str
interface str
   module procedure msg_scalar, msg_one
end interface str
!===================================================================================================================================

private locate        ! [M_CLI2] find PLACE in sorted character array where value can be found or should be placed
   private locate_c
private insert        ! [M_CLI2] insert entry into a sorted allocatable array at specified position
   private insert_c
   private insert_i
   private insert_l
private replace       ! [M_CLI2] replace entry by index from a sorted allocatable array if it is present
   private replace_c
   private replace_i
   private replace_l
private remove        ! [M_CLI2] delete entry by index from a sorted allocatable array if it is present
   private remove_c
   private remove_i
   private remove_l

! Generic subroutine inserts element into allocatable array at specified position
interface  locate;   module procedure locate_c                            ; end interface
interface  insert;   module procedure insert_c,      insert_i,  insert_l  ; end interface
interface  replace;  module procedure replace_c,     replace_i, replace_l ; end interface
interface  remove;   module procedure remove_c,      remove_i,  remove_l  ; end interface
!-----------------------------------------------------------------------------------------------------------------------------------
! convenience functions
interface cgets;module procedure cgs, cg;end interface
interface dgets;module procedure dgs, dg;end interface
interface igets;module procedure igs, ig;end interface
interface lgets;module procedure lgs, lg;end interface
interface rgets;module procedure rgs, rg;end interface
interface sgets;module procedure sgs, sg;end interface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     check_commandline(3f) - [ARGUMENTS:M_CLI2]check command and process
!!     pre-defined options
!!
!!##SYNOPSIS
!!
!!      subroutine check_commandline(help_text,version_text,ierr,errmsg)
!!
!!       character(len=:),allocatable,intent(in),optional :: help_text(:)
!!       character(len=:),allocatable,intent(in),optional :: version_text(:)
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
!!         help_text=[character(len=80) :: "wish I put instructions","here","I suppose?"]
!!         call set_args(cmd,help_text,version_text)
!!         call get_args('x',x,'y',y,'z',z)
!!         ! All done cracking the command line. Use the values in your program.
!!         write (*,*)x,y,z
!!         ! the optional unnamed values on the command line are
!!         ! accumulated in the character array "UNNAMED"
!!         if(size(unnamed).gt.0)then
!!            write (*,'(a)')'files:'
!!            write (*,'(i6.6,3a)') (i,'[',unnamed(i),']',i=1,size(unnamed))
!!         endif
!!      end program check_commandline
!===================================================================================================================================
subroutine check_commandline(help_text,version_text)
character(len=:),allocatable,intent(in),optional :: help_text(:)
character(len=:),allocatable,intent(in),optional :: version_text(:)
character(len=:),allocatable                     :: line
integer                                          :: i
integer                                          :: istart
integer                                          :: iback
   if(get('usage').eq.'T')then
      call print_dictionary('USAGE:')
      !x!call default_help()
      call mystop(32)
      return
   endif
   if(present(help_text))then
      if(get('help').eq.'T')then
         do i=1,size(help_text)
            call journal('sc',help_text(i))
         enddo
         call mystop(1,'displayed help text')
         return
      endif
   elseif(get('help').eq.'T')then
      call default_help()
      call mystop(2,'displayed default help text')
      return
   endif
   if(present(version_text))then
      if(get('version').eq.'T')then
         istart=1
         iback=0
         if(size(version_text).gt.0)then
            if(index(version_text(1),'@'//'(#)').eq.1)then ! allow for what(1) syntax
               istart=5
               iback=1
            endif
         endif
         if(debug_m_cli2)write(*,gen)'<DEBUG>CHECK_COMMANDLINE:VERSION_TEXT:ALLOCATED',allocated(version_text)
         if(allocated(version_text).and.debug_m_cli2)then
            write(*,gen)'<DEBUG>CHECK_COMMANDLINE:VERSION_TEXT:LEN',len(version_text)
            write(*,gen)'<DEBUG>CHECK_COMMANDLINE:VERSION_TEXT:SIZE',size(version_text)
            write(*,gen)'<DEBUG>CHECK_COMMANDLINE:VERSION_TEXT:LEN',version_text
         endif
         do i=1,size(version_text)
            !xINTEL BUG*!call journal('sc',version_text(i)(istart:len_trim(version_text(i))-iback))
            line=version_text(i)(istart:len_trim(version_text(i))-iback)
            call journal('sc',line)
         enddo
         call mystop(3,'displayed version text')
         return
      endif
   elseif(get('version').eq.'T')then

      if(G_QUIET)then
         G_STOP_MESSAGE = 'no version text'
      else
         call journal('sc','*check_commandline* no version text')
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
   call substitute(G_passed_in,' --',NEW_LINE('A')//' --')
   if(.not.G_QUIET)then
      call journal('sc',cmd_name,G_passed_in) ! no help text, echo command and default options
   endif
   deallocate(cmd_name)
end subroutine default_help
end subroutine check_commandline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     set_args(3f) - [ARGUMENTS:M_CLI2] command line argument parsing
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine set_args(definition,help_text,version_text,ierr,errmsg)
!!
!!      character(len=*),intent(in),optional              :: definition
!!      character(len=:),intent(in),allocatable,optional  :: help_text
!!      character(len=:),intent(in),allocatable,optional  :: version_text
!!      integer,intent(out),optional                      :: ierr
!!      character(len=:),intent(out),allocatable,optional :: errmsg
!!##DESCRIPTION
!!
!!     SET_ARGS(3f) requires a unix-like command prototype for defining
!!     arguments and default command-line options. Argument values are then
!!     read using GET_ARGS(3f).
!!
!!     The --help and --version options require the optional
!!     help_text and version_text values to be provided.
!!
!!##OPTIONS
!!
!!      DESCRIPTION   composed of all command arguments concatenated
!!                    into a Unix-like command prototype string. For
!!                    example:
!!
!!                      call set_args('-L F -ints 10,20,30 -title "my title" -R 10.3')
!!
!!                    DESCRIPTION is pre-defined to act as if started with
!!                    the reserved options '--verbose F --usage F --help
!!                    F --version F'. The --usage option is processed when
!!                    the set_args(3f) routine is called. The same is true
!!                    for --help and --version if the optional help_text
!!                    and version_text options are provided.
!!
!!                    see "DEFINING THE PROTOTYPE" in the next section for
!!                    further details.
!!
!!      HELP_TEXT     if present, will be displayed if program is called with
!!                    --help switch, and then the program will terminate. If
!!                    not supplied, the command line initialization string
!!                    will be shown when --help is used on the commandline.
!!
!!      VERSION_TEXT  if present, will be displayed if program is called with
!!                    --version switch, and then the program will terminate.
!!      IERR          if present a non-zero option is returned when an
!!                    error occurs instead of program execution being
!!                    terminated
!!      ERRMSG        a description of the error if ierr is present
!!
!!##DEFINING THE PROTOTYPE
!!         o all keywords on the prototype MUST get a value.
!!
!!         o logicals MUST be set to F or T.
!!
!!         o strings MUST be delimited with double-quotes and
!!           must be at least one space. Internal double-quotes
!!           are represented with two double-quotes.
!!
!!         o numeric keywords are not allowed; but this allows
!!           negative numbers to be used as values.
!!
!!         o lists of values should be comma-delimited unless a
!!           user-specified delimiter is used. The prototype
!!           must use the same array delimiters as the call to
!!           the family of get_args*(3f) called.
!!
!!         o long names (--keyword) should be all lowercase
!!
!!         o The simplest way to have short names is to suffix the long
!!           name with :LETTER If this syntax is used then logical shorts
!!           may be combined on the command line and -- and - prefixes are
!!           strictly enforced.
!!
!!           mapping of short names to long names not using the
!!           --LONGNAME:SHORTNAME syntax is demonstrated in the manpage
!!           for SPECIFIED(3f).
!!
!!         o A very special behavior occurs if the keyword name ends in ::.
!!           The next parameter is taken as a value even if it starts with -.
!!           This is not generally recommended but is noted here for
!!           completeness.
!!
!!         o to define a zero-length allocatable array make the
!!           value a delimiter (usually a comma).
!!
!!         o all unused values go into the character array UNNAMED
!!
!!         o If the prototype ends with "--" a special mode is turned
!!           on where anything after "--" on input goes into the variable
!!           REMAINING and the array ARGS instead of becoming elements in
!!           the UNNAMED array. This is not needed for normal processing.
!!
!!##USAGE
!!      When invoking the program line note that (subject to change) the
!!      following variations from other common command-line parsers:
!!
!!         o Long names should be all lowercase and always more than one
!!           character.
!!
!!         o values for duplicate keywords are appended together with a space
!!           separator when a command line is executed.
!!
!!         o numeric keywords are not allowed; but this allows
!!           negative numbers to be used as values.
!!
!!         o Although not generally recommended you can equivalence
!!           keywords (usually for multi-lingual support). Be aware that
!!           specifying both names of an equivalenced keyword on a command
!!           line will have undefined results (currently, their ASCII
!!           alphabetical order will define what the Fortran variable
!!           values become).
!!
!!           The second of the names should only be called with a
!!           GET_ARGS*(3f) routine if the SPECIFIED(3f) function is .TRUE.
!!           for that name.
!!
!!           Note that allocatable arrays cannot be EQUIVALENCEd in Fortran.
!!
!!         o short keywords cannot be combined unless they were defined
!!           using the --LONGNAME:SHORTNAME syntax. Even then -a -b -c
!!           is required not -abc unless all the keywords are logicals
!!           (Boolean keys).
!!
!!         o shuffling is not supported. Values should follow their
!!           keywords.
!!
!!         o if a parameter value of just "-" is supplied it is
!!           converted to the string "stdin".
!!
!!         o values not matching a keyword go into the character
!!           array "UNUSED".
!!
!!         o if the keyword "--" is encountered the rest of the
!!           command arguments go into the character array "UNUSED".
!!##EXAMPLE
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
!!        ! set all logical values to F or T.
!!        & -l F -L F &
!!        ! set allocatable size to zero if you like by using a delimiter
!!        & -ints , &
!!        ! string should be a single character at a minimum
!!        & --label " " &
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
!!     if(size(filenames).gt.0)then
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
!!     use M_CLI2, only : CLI_response_file
!!     CLI_response_file=.true.
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
!!  Note that more than one response name may appear on a command line.
!!
!!  They are case-sensitive names.
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
!!    comment|#  Line is a comment line
!!    system|!   System command.
!!               System commands are executed as a simple call to
!!               system (so a cd(1) or setting a shell variable
!!               would not effect subsequent lines, for example)
!!    print|>    Message to screen
!!    stop       display message and stop program.
!!
!!  So if a program that does nothing but echos its parameters
!!
!!    program testit
!!    use M_CLI2, only : set_args, rget, sget, lget
!!    use M_CLI2, only : CLI_response_file
!!    implicit none
!!       real :: x,y                           ; namelist/args/ x,y
!!       character(len=:),allocatable :: title ; namelist/args/ title
!!       logical :: big                        ; namelist/args/ big
!!       CLI_response_file=.true.
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
!!    Note that here `basename` means the last leaf  of the
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
!!  "@" is encountered.  Lines will then be processed until another line
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
!!    options run --release T --compiler gfortran --runner "install -vbp -m 0711 -t ~/.local/bin"
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
!!    anymore. It also allows the @NAME syntax anywhere on the command
!!    line, not just at the beginning. --  20201212
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!
!!##LICENSE
!!      Public Domain

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_args(prototype,help_text,version_text,string,ierr,errmsg)

! ident_2="@(#)M_CLI2::set_args(3f): parse prototype string"

character(len=*),intent(in)                       :: prototype
character(len=:),intent(in),allocatable,optional  :: help_text(:)
character(len=:),intent(in),allocatable,optional  :: version_text(:)
character(len=*),intent(in),optional              :: string
integer,intent(out),optional                      :: ierr
character(len=:),intent(out),allocatable,optional :: errmsg
character(len=:),allocatable                      :: hold               ! stores command line argument
integer                                           :: ibig
   G_response=CLI_RESPONSE_FILE
   G_options_only=.false.
   G_append=.true.
   G_passed_in=''
   G_STOP=0
   G_STOP_MESSAGE=''
   if(present(ierr))then
      G_QUIET=.true.
   else
      G_QUIET=.false.
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   if(allocated(unnamed)) deallocate(unnamed)
   allocate(character(len=ibig) :: unnamed(0))
   if(allocated(args)) deallocate(args)
   allocate(character(len=ibig) :: args(0))

   call wipe_dictionary()
   hold='--version F --usage F --help F --version F '//adjustl(prototype)
   call prototype_and_cmd_args_to_nlist(hold,string)
   if(allocated(G_RESPONSE_IGNORED))then
      if(debug_m_cli2)write(*,gen)'<DEBUG>SET_ARGS:G_RESPONSE_IGNORED:',G_RESPONSE_IGNORED
      if(size(unnamed).ne.0)write(*,*)'LOGIC ERROR'
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
!!    !x!    demo_get_subcommand run -x -y -z -title -l -L
!!    !x!    demo_get_subcommand test -title -l -L -testname
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
!!    use M_CLI2, only : get_subcommand
!!    use M_CLI2, only : CLI_RESPONSE_FILE
!!    character(len=*)              :: name    ! the subcommand name
!!    character(len=:),allocatable  :: help_text(:), version_text(:)
!!       CLI_RESPONSE_FILE=.true.
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
!!         '   * run  -l -L -title -x -y -z   ', &
!!         '   * test -l -L -title            ', &
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

! ident_3="@(#)M_CLI2::get_subcommand(3f): parse prototype string to get subcommand, allowing for response files"

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
      if(adjustl(cmdarg(1:1)) .eq. '@')then
         call get_prototype(cmdarg,prototype)
         call split(prototype,array)
         ! assume that if using subcommands first word not starting with dash is the subcommand
         do j=1,size(array)
            if(adjustl(array(j)(1:1)) .ne. '-')then
            G_subcommand=trim(array(j))
            sub=G_subcommand
            exit
         endif
         enddo
      endif
   enddo

   if(G_subcommand.ne.'')then
      sub=G_subcommand
   elseif(size(unnamed).ne.0)then
      sub=unnamed(1)
   else
      cmdarg(:) = ''
      do i = 1, command_argument_count()
         call get_command_argument(i, cmdarg)
         if(adjustl(cmdarg(1:1)) .ne. '-')then
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
!===================================================================================================================================
subroutine set_usage(keyword,description,value)
character(len=*),intent(in) :: keyword
character(len=*),intent(in) :: description
character(len=*),intent(in) :: value
write(*,*)keyword
write(*,*)description
write(*,*)value
! store the descriptions in an array and then apply them when set_args(3f) is called.
! alternatively, could allow for a value as well in lieue of the prototype
end subroutine set_usage
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      prototype_to_dictionary(3f) - [ARGUMENTS:M_CLI2] parse user command
!!      and store tokens into dictionary
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
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
!!          o logical values must have a value
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
!!     Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
recursive subroutine prototype_to_dictionary(string)
implicit none

! ident_4="@(#)M_CLI2::prototype_to_dictionary(3f): parse user command and store tokens into dictionary"

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
         if(forwrd.eq.'-')then                      ! change --var to -var so "long" syntax is supported
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
            if(keyword.ne.' ')then
               call update(keyword,value)            ! store name and its value
            elseif( G_remaining_option_allowed)then  ! meaning "--" has been encountered
               call update('_args_',trim(value))
            else
               !x!write(warn,'(*(g0))')'*prototype_to_dictionary* warning: ignoring string [',trim(value),'] for ',trim(keyword)
               G_RESPONSE_IGNORED=TRIM(VALUE)
               if(debug_m_cli2)write(*,gen)'<DEBUG>PROTOTYPE_TO_DICTIONARY:G_RESPONSE_IGNORED:',G_RESPONSE_IGNORED
            endif
         else
            call locate_key(keyword,place)
            if(keyword.ne.' '.and.place.lt.0)then
               call update(keyword,'F')           ! store name and null value (first pass)
            elseif(keyword.ne.' ')then
               call update(keyword,' ')           ! store name and null value (second pass)
            elseif(.not.G_keyword_single_letter.and.ipoint-2.eq.islen) then ! -- at end of line
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
         if(currnt == " ".and.itype  ==  KEYW)then
            ! switch from building a keyword string to building a value string
            itype=VAL
            ! beginning of a delimited value
         elseif(currnt  ==  """".and.itype  ==  VAL)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
               if(itype.eq.VAL)then
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
               if(itype.eq.VAL)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
            endif
         else     ! add character to current keyword or value
            if(itype.eq.VAL)then
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
!!##OPTIONS
!!
!!    NAME   name of commandline argument to query the presence of
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
!!    use M_CLI2,  only : set_args, get_args, specified
!!    implicit none
!!    ! DEFINE ARGS
!!    integer                 :: flag
!!    integer,allocatable     :: ints(:)
!!    real,allocatable        :: twonames(:)
!!
!!    ! IT IS A BAD IDEA TO NOT HAVE THE SAME DEFAULT VALUE FOR ALIASED
!!    ! NAMES BUT CURRENTLY YOU STILL SPECIFY THEM
!!       call set_args(' -flag 1 -f 1 -ints 1,2,3 -i 1,2,3 -twonames 11.3 -T 11.3')
!!
!!    ! ASSIGN VALUES TO ELEMENTS CONDITIONALLY CALLING WITH SHORT NAME
!!       call get_args('flag',flag)
!!       if(specified('f'))call get_args('f',flag)
!!       call get_args('ints',ints)
!!       if(specified('i'))call get_args('i',ints)
!!       call get_args('twonames',twonames)
!!       if(specified('T'))call get_args('T',twonames)
!!
!!       ! IF YOU WANT TO KNOW IF GROUPS OF PARAMETERS WERE SPECIFIED USE
!!       ! ANY(3f) and ALL(3f)
!!       write(*,*)specified(['twonames','T       '])
!!       write(*,*)'ANY:',any(specified(['twonames','T       ']))
!!       write(*,*)'ALL:',all(specified(['twonames','T       ']))
!!
!!       ! FOR MUTUALLY EXCLUSIVE
!!       if (all(specified(['twonames','T       '])))then
!!           write(*,*)'You specified both names -T and -twonames'
!!       endif
!!
!!       ! FOR REQUIRED PARAMETER
!!       if (.not.any(specified(['twonames','T       '])))then
!!           write(*,*)'You must specify -T or -twonames'
!!       endif
!!
!!    ! USE VALUES
!!       write(*,*)'flag=',flag
!!       write(*,*)'ints=',ints
!!       write(*,*)'twonames=',twonames
!!    end program demo_specified
!!
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!===================================================================================================================================
elemental impure function specified(key)
character(len=*),intent(in) :: key
logical                     :: specified
integer                     :: place
   call locate_key(key,place)                   ! find where string is or should be
   if(place.lt.1)then
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
!!      update(3f) - [ARGUMENTS:M_CLI2] update internal dictionary given
!!      keyword and value
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!
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
   call split(trim(key),long_short,':',nulls='return') ! split long:short keyname or long:short:: or long:: or short::
   ! check for :: on end
   isize=size(long_short)
   if(isize.gt.0)then                     ! very special-purpose syntax where if ends in :: next field is a value even
      if(long_short(isize).eq.'')then     ! if it starts with a dash, for --flags option on fpm(1).
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
      if(len_trim(long).eq.1)then
         !x!ii= findloc (shorts, long, dim=1) ! if parsing arguments on line and a short keyword look up long value
         ii=maxloc([0,merge(1, 0, shorts.eq.long)],dim=1)
         if(ii.gt.1)then
            long=keywords(ii-1)
         endif
         short=long
      else
         short=''
      endif
   case(2)
      G_STRICT=.true.  ! strict short and long rules so do not allow -longname and --shortname
      long=trim(long_short(1))
      short=trim(long_short(2))
   case default
      write(warn,*)'WARNING: incorrect syntax for key: ',trim(key)
      long=trim(long_short(1))
      short=trim(long_short(2))
   end select
   if(present(val))then
      val_local=val
      iilen=len_trim(val_local)
      call locate_key(long,place)                  ! find where string is or should be
      if(place.lt.1)then                                ! if string was not found insert it
         call insert(keywords,long,iabs(place))
         call insert(values,val_local,iabs(place))
         call insert(counts,iilen,iabs(place))
         call insert(shorts,short,iabs(place))
         call insert(present_in,.true.,iabs(place))
         call insert(mandatory,set_mandatory,iabs(place))
      else
         if(present_in(place))then                      ! if multiple keywords append values with space between them
            if(G_append)then
               if(values(place)(1:1).eq.'"')then
               ! UNDESIRABLE: will ignore previous blank entries
                  val_local='"'//trim(unquote(values(place)))//' '//trim(unquote(val_local))//'"'
               else
                  val_local=values(place)//' '//val_local
               endif
            endif
            iilen=len_trim(val_local)
         endif
         call replace(values,val_local,place)
         call replace(counts,iilen,place)
         call replace(present_in,.true.,place)
      endif
   else                                                 ! if no value is present remove the keyword and related values
      call locate_key(long,place)                       ! check name as long and short
      if(place.gt.0)then
         call remove(keywords,place)
         call remove(values,place)
         call remove(counts,place)
         call remove(shorts,place)
         call remove(present_in,place)
         call remove(mandatory,place)
      endif
   endif
end subroutine update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      wipe_dictionary(3fp) - [ARGUMENTS:M_CLI2] reset private M_CLI2(3fm) dictionary to empty
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
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
!!      get(3f) - [ARGUMENTS:M_CLI2] get dictionary value associated with key name in private M_CLI2(3fm) dictionary
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!      Get dictionary value associated with key name in private M_CLI2(3fm) dictionary.
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
   if(place.lt.1)then
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
!!      prototype_and_cmd_args_to_nlist(3f) - [ARGUMENTS:M_CLI2] convert Unix-like command arguments to table
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!     subroutine prototype_and_cmd_args_to_nlist(prototype)
!!
!!      character(len=*)             :: prototype
!!##DESCRIPTION
!!    create dictionary with character keywords, values, and value lengths
!!    using the routines for maintaining a list from command line arguments.
!!##OPTIONS
!!      prototype
!!##EXAMPLE
!!
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
!!      ! uppercase keywords get an underscore to make it easier o remember
!!      logical            :: l_,h_,v_
!!      character(len=256) :: a_,b_                  ! character variables must be long enough to hold returned value
!!      integer            :: c_(3)
!!
!!         ! give command template with default values
!!         ! all values except logicals get a value.
!!         ! strings must be delimited with double quotes
!!         ! A string has to have at least one character as for -A
!!         ! lists of numbers should be comma-delimited. No spaces are allowed in lists of numbers
!!         call prototype_and_cmd_args_to_nlist('&
!!         & -l -v -h -LVH -x 0 -y 0.0 -z 0.0d0 -p 0,0 &
!!         & -A " " -B "Value B" -C 10,20,30 -c (-123,-456)',readme)
!!
!!         call get_args('x',x,'y',y,'z',z)
!!            something=sqrt(x**2+y**2+z**2)
!!            write (*,*)something,x,y,z
!!            if(size(unnamed).gt.0)then
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
implicit none

! ident_5="@(#)M_CLI2::prototype_and_cmd_args_to_nlist: create dictionary from prototype if not null and update from command line"

character(len=*),intent(in)           :: prototype
character(len=*),intent(in),optional  :: string
integer                               :: ibig
integer                               :: itrim
integer                               :: iused

   if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:START'
   G_passed_in=prototype                            ! make global copy for printing
   G_STRICT=.false.  ! strict short and long rules or allow -longname and --shortname

   ibig=longest_command_argument()                  ! bug in gfortran. len=0 should be fine
   ibig=max(ibig,1)
   if(allocated(unnamed))deallocate(unnamed)
   allocate(character(len=ibig) :: unnamed(0))
   if(allocated(args))deallocate(args)
   allocate(character(len=ibig) :: args(0))

   G_remaining_option_allowed=.false.
   G_remaining_on=.false.
   G_remaining=''
   if(prototype.ne.'')then
      call prototype_to_dictionary(prototype)       ! build dictionary from prototype

      ! if short keywords not used by user allow them for standard options

      call locate_key('h',iused)
      if(iused.le.0)then
         call update('help')
         call update('help:h','F')
      endif

      call locate_key('v',iused)
      if(iused.le.0)then
         call update('version')
         call update('version:v','F')
      endif

      call locate_key('V',iused)
      if(iused.le.0)then
         call update('verbose')
         call update('verbose:V','F')
      endif

      call locate_key('u',iused)
      if(iused.le.0)then
         call update('usage')
         call update('usage:u','F')
      endif

      present_in=.false.                            ! reset all values to false so everything gets written
   endif

   if(present(string))then                          ! instead of command line arguments use another prototype string
      if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:CALL PROTOTYPE_TO_DICTIONARY:STRING=',STRING
      call prototype_to_dictionary(string)          ! build dictionary from prototype
   else
      if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:CALL CMD_ARGS_TO_DICTIONARY:CHECK=',.true.
      call cmd_args_to_dictionary()
   endif

   if(len(G_remaining).gt.1)then                    ! if -- was in prototype then after -- on input return rest in this string
      itrim=len(G_remaining)
      if(G_remaining(itrim:itrim).eq.' ')then       ! was adding a space at end as building it, but do not want to remove blanks
         G_remaining=G_remaining(:itrim-1)
      endif
      remaining=G_remaining
   endif
   if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:NORMAL END'
end subroutine prototype_and_cmd_args_to_nlist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine expand_response(name)
character(len=*),intent(in) :: name
character(len=:),allocatable :: prototype
logical :: hold
   if(debug_m_cli2)write(*,gen)'<DEBUG>EXPAND_RESPONSE:START:NAME=',name
   call get_prototype(name,prototype)
   if(prototype.ne.'')then
      hold=G_append
      G_append=.false.
      if(debug_m_cli2)write(*,gen)'<DEBUG>EXPAND_RESPONSE:CALL PROTOTYPE_TO_DICTIONARY:PROTOTYPE=',prototype
      call prototype_to_dictionary(prototype)       ! build dictionary from prototype
      G_append=hold
   endif
   if(debug_m_cli2)write(*,gen)'<DEBUG>EXPAND_RESPONSE:END'
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
   if(debug_m_cli2)write(*,gen)'<DEBUG>GET_PROTOTYPE:OS=',OS

   search_for=''
   ! look for NAME.rsp and see if there is an @OS  section in it and position to it and read
   if(os.ne.'@')then
      search_for=os
      call find_and_read_response_file(plain_name)
      if(lines_processed.ne.0)return
   endif

   ! look for NAME.rsp and see if there is anything before an OS-specific section
   search_for=''
   call find_and_read_response_file(plain_name)
   if(lines_processed.ne.0)return

   ! look for ARG0.rsp  with @OS@NAME  section in it and position to it
   if(os.ne.'@')then
      search_for=os//name
      call find_and_read_response_file(basename(get_name(),suffix=.true.))
      if(lines_processed.ne.0)return
   endif

   ! look for ARG0.rsp  with a section called @NAME in it and position to it
   search_for=name
   call find_and_read_response_file(basename(get_name(),suffix=.true.))
   if(lines_processed.ne.0)return

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
   filename=rname//'.rsp'
   if(debug_m_cli2)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:FILENAME=',filename

   ! look for name.rsp in directories from environment variable assumed to be a colon-separated list of directories
   call split(get_env('CLI_RESPONSE_PATH'),paths)
   paths=[character(len=len(paths)) :: ' ',paths]
   if(debug_m_cli2)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:PATHS=',paths

   do i=1,size(paths)
      testpath=join_path(paths(i),filename)
      lun=fileopen(testpath,message)
      if(lun.ne.-1)then
         if(debug_m_cli2)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:SEARCH_FOR=',search_for
         if(search_for.ne.'') call position_response() ! set to end of file or where string was found
         call process_response()
         if(debug_m_cli2)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:LINES_PROCESSED=',LINES_PROCESSED
         close(unit=lun,iostat=ios)
         if(debug_m_cli2)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:CLOSE:LUN=',LUN,' IOSTAT=',IOS
         if(lines_processed.ne.0)exit
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
         if(debug_m_cli2)write(*,gen)'<DEBUG>POSITION_RESPONSE:EOF'
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios.ne.0)then
         write(*,gen)'<ERROR>*position_response*:'//trim(message)
         exit INFINITE
      endif
      line=adjustl(line)
      if(line.eq.search_for)return
   enddo INFINITE
end subroutine position_response
!===================================================================================================================================
subroutine process_response()
   line=''
   lines_processed=0
      INFINITE: do
      read(unit=lun,fmt='(a)',iostat=ios,iomsg=message)line
      if(is_iostat_end(ios))then
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios.ne.0)then
         write(*,gen)'<ERROR>*process_response*:'//trim(message)
         exit INFINITE
      endif
      line=adjustl(line)
      if(index(line//' ','#').eq.1)cycle
      if(line.ne.'')then

         if(index(line,'@').eq.1.and.lines_processed.ne.0)exit INFINITE

         call split(line,array) ! get first word
         itrim=len_trim(array(1))+2
         line=line(itrim:)

         PROCESS: select case(lower(array(1)))
         case('comment','#','')
         case('system','!','$')
            if(G_options_only)exit PROCESS
            lines_processed= lines_processed+1
            call execute_command_line(line)
         case('options','option','-')
            lines_processed= lines_processed+1
            prototype=prototype//' '//trim(line)
         case('print','>','echo')
            if(G_options_only)exit PROCESS
            lines_processed= lines_processed+1
            write(*,'(a)')trim(line)
         case('stop')
            if(G_options_only)exit PROCESS
            write(*,'(a)')trim(line)
            stop
         case default
            if(array(1)(1:1).eq.'@')cycle INFINITE !skip adjacent @ lines from first
            lines_processed= lines_processed+1
            write(*,'(*(g0))')'unknown response keyword [',array(1),'] with options of [',trim(line),']'
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

   if(ios.ne.0)then
      lun=-1
      if(present(message))then
         message=trim(message_local)
      else
         write(*,gen)trim(message_local)
      endif
   endif
   if(debug_m_cli2)write(*,gen)'<DEBUG>FILEOPEN:FILENAME=',filename,' LUN=',lun,' IOS=',IOS,' MESSAGE=',trim(message_local)

end function fileopen
!===================================================================================================================================
function get_env(NAME,DEFAULT) result(VALUE)
implicit none
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
   ! get length required to hold value
   length=0
   if(NAME.ne.'')then
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
          if(stat.ne.0)VALUE=''
      end select
   else
      VALUE=''
   endif
   if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
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
   if(a1.ne.'')then
      path = trim(a1) // filesep // trim(a2)
   else
      path = trim(a2)
   endif
   if (present(a3)) path = path // filesep // trim(a3)
   if (present(a4)) path = path // filesep // trim(a4)
   if (present(a5)) path = path // filesep // trim(a5)
   path=adjustl(path//'  ')
   call substitute(path,filesep//filesep,'',start=2) ! some systems allow names starting with '//' or '\\'
   path=trim(path)
end function join_path
!===================================================================================================================================
function get_name() result(name)
! get the pathname of arg0
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
character(len=4096)          :: long_name
character(len=:),allocatable :: name
   arg0_length=0
   name=''
   long_name=''
   call get_command_argument(0,length=arg0_length,status=istat)
   if(istat.eq.0)then
      if(allocated(arg0))deallocate(arg0)
      allocate(character(len=arg0_length) :: arg0)
      call get_command_argument(0,arg0,status=istat)
      if(istat.eq.0)then
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
      if(size(file_parts).gt.0)then
         base = trim(file_parts(size(file_parts)))
      else
         base = ''
      endif
   else
      call split(path,file_parts,delimiters='\/.')
      if(size(file_parts).ge.2)then
         base = trim(file_parts(size(file_parts)-1))
      else
         base = ''
      endif
   endif
end function basename
!===================================================================================================================================
function separator2() result(sep)
! use the pathname returned as arg0 to determine pathname separator
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
logical                      :: existing
character(len=1)             :: sep
character(len=4096)          :: name
character(len=:),allocatable :: fname
   arg0_length=0
   name=' '
   call get_command_argument(0,length=arg0_length,status=istat)
   if(allocated(arg0))deallocate(arg0)
   allocate(character(len=arg0_length) :: arg0)
   call get_command_argument(0,arg0,status=istat)
   ! check argument name
   if(index(arg0,'\').ne.0)then
      sep='\'
   elseif(index(arg0,'/').ne.0)then
      sep='/'
   else
      ! try name returned by INQUIRE(3f)
      existing=.false.
      name=' '
      inquire(file=arg0,iostat=istat,exist=existing,name=name)
      if(index(name,'\').ne.0)then
         sep='\'
      elseif(index(name,'/').ne.0)then
         sep='/'
      else
         ! well, try some common syntax and assume in current directory
         fname='.\'//arg0
         inquire(file=fname,iostat=istat,exist=existing)
         if(existing)then
            sep='/'
         else
            fname='./'//arg0
            inquire(file=fname,iostat=istat,exist=existing)
            if(existing)then
               sep='/'
            else
               !x!write(*,gen)'<WARNING>unknown system directory path separator'
               sep='/'
            endif
         endif
      endif
   endif
end function separator2
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
!!   examined first for a backslash, then a slash.  Assuming basically the
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
implicit none
integer                      :: ios
integer                      :: i
logical                      :: existing=.false.
character(len=1)             :: sep
!x!IFORT BUG:character(len=1),save        :: sep_cache=' '
integer,save                 :: isep=-1
character(len=4096)          :: name
character(len=:),allocatable :: envnames(:)

    ! NOTE:  A parallel code might theoretically use multiple OS
    !x!FORT BUG:if(sep_cache.ne.' ')then  ! use cached value.
    !x!FORT BUG:    sep=sep_cache
    !x!FORT BUG:    return
    !x!FORT BUG:endif
    if(isep.ne.-1)then  ! use cached value.
        sep=char(isep)
        return
    endif
    FOUND: block
    ! simple, but does not work with ifort
    ! most MSWindows environments see to work with backslash even when
    ! using POSIX filenames to do not rely on '\.'.
    inquire(file='/.',exist=existing,iostat=ios,name=name)
    if(existing.and.ios.eq.0)then
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
       if(index(get_env(envnames(i)),'\').ne.0)then
          sep='\'
          exit FOUND
       elseif(index(get_env(envnames(i)),'/').ne.0)then
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
   if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START'
   next_mandatory=.false.
   nomore=.false.
   pointer=0
   lastkeyword=' '
   G_keyword_single_letter=.true.
   i=1
   GET_ARGS: do while (get_next_argument()) ! insert and replace entries

      if( current_argument .eq. '-' .and. nomore .eqv. .true. )then   ! sort of
      elseif( current_argument .eq. '-')then                          ! sort of
         current_argument='"stdin"'
      endif
      if( current_argument .eq. '--' .and. nomore .eqv. .true. )then  ! -- was already encountered
      elseif( current_argument .eq. '--' )then                        ! everything after this goes into the unnamed array
         nomore=.true.
         pointer=0
         if(G_remaining_option_allowed)then
            G_remaining_on=.true.
         endif
         cycle GET_ARGS
      endif

      dummy=current_argument//'   '
      current_argument_padded=current_argument//'   '

      !x!guess_if_value=maybe_value()

      if(.not.next_mandatory.and..not.nomore.and.current_argument_padded(1:2).eq.'--')then    ! beginning of long word
         G_keyword_single_letter=.false.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate_key(current_argument_padded(3:),pointer)
         if(pointer.le.0)then
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
      & .and.current_argument_padded(1:1).eq.'-' &
      & .and.index("0123456789.",dummy(2:2)).eq.0)then
      ! short word
         G_keyword_single_letter=.true.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate_key(current_argument_padded(2:),pointer)
         if(pointer.le.0)then
            jj=len(current_argument)
            if(G_STRICT.and.jj.gt.2)then  ! in strict mode this might be multiple single-character values
              do kk=2,jj
                 letter=current_argument_padded(kk:kk)
                 call locate_key(letter,pointer)
                 if(pointer.gt.0)then
                    call update(keywords(pointer),'T')
                 else
                    call print_dictionary('UNKNOWN COMPOUND SHORT KEYWORD:'//letter//' in '//current_argument)
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
            else
               call print_dictionary('UNKNOWN SHORT KEYWORD: '//current_argument)
               if(G_QUIET)then
                  lastkeyword="UNKNOWN"
                  pointer=0
                  cycle GET_ARGS
               endif
               call mystop(2)
               return
            endif
         endif
         lastkeyword=trim(current_argument_padded(2:))
         next_mandatory=mandatory(pointer)
      elseif(pointer.eq.0)then                                       ! unnamed arguments
         if(G_remaining_on)then
            if(len(current_argument).lt.1)then
               G_remaining=G_remaining//'"" '
            elseif(current_argument(1:1).eq.'-')then
               !get fancier to handle spaces and =!G_remaining=G_remaining//current_argument//' '
               G_remaining=G_remaining//'"'//current_argument//'" '
            else
               G_remaining=G_remaining//'"'//current_argument//'" '
            endif
            imax=max(len(args),len(current_argument))
            args=[character(len=imax) :: args,current_argument]
         else
            imax=max(len(unnamed),len(current_argument))
            if(index(current_argument//' ','@').eq.1.and.G_response)then
               if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:1:CALL EXPAND_RESPONSE:CURRENT_ARGUMENT=',current_argument
               call expand_response(current_argument)
            else
               unnamed=[character(len=imax) :: unnamed,current_argument]
            endif
         endif
      else
         oldvalue=get(keywords(pointer))//' '
         if(oldvalue(1:1).eq.'"')then
            current_argument=quote(current_argument(:ilength))
         endif
         if(upper(oldvalue).eq.'F'.or.upper(oldvalue).eq.'T')then  ! assume boolean parameter
            if(current_argument.ne.' ')then
               if(G_remaining_on)then
                  if(len(current_argument).lt.1)then
                        G_remaining=G_remaining//'"" '
                  elseif(current_argument(1:1).eq.'-')then
                       !get fancier to handle spaces and =!G_remaining=G_remaining//current_argument//' '
                        G_remaining=G_remaining//'"'//current_argument//'" '
                  else
                        G_remaining=G_remaining//'"'//current_argument//'" '
                  endif
                  imax=max(len(args),len(current_argument))
                  args=[character(len=imax) :: args,current_argument]
               else
                  imax=max(len(unnamed),len(current_argument))
                  if(index(current_argument//' ','@').eq.1.and.G_response)then
               if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:2:CALL EXPAND_RESPONSE:CURRENT_ARGUMENT=',current_argument
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
   if(lastkeyword.ne.'')then
      call ifnull()
   endif
   if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:NORMAL END'

contains

subroutine ifnull()
   oldvalue=get(lastkeyword)//' '
   if(upper(oldvalue).eq.'F'.or.upper(oldvalue).eq.'T')then
      call update(lastkeyword,'T')
   elseif(oldvalue(1:1).eq.'"')then
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
       elseif(len(current_argument).eq.0)then
       else
          iright=index(current_argument,' ')
          if(iright.eq.0)iright=len(current_argument)
          iequal=index(current_argument(:iright),'=')
          if(next_mandatory)then
          elseif(iequal.ne.0.and.current_argument(1:1).eq.'-')then
             if(iequal.ne.len(current_argument))then
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

function maybe_value()
! if previous keyword value type is a string and it was
! given a null string because this value starts with a -
! try to see if this is a string value starting with a -
! to try to solve the vexing problem of values starting
! with a dash.
logical :: maybe_value
integer :: pointer
character(len=:),allocatable :: oldvalue

   oldvalue=get(lastkeyword)//' '
   if(current_argument_padded(1:1).ne.'-')then
      maybe_value=.true.
   elseif(oldvalue(1:1).ne.'"')then
      maybe_value=.false.
   elseif(index(current_argument,' ').ne.0)then
      maybe_value=.true.
   elseif(scan(current_argument,",:;!@#$%^&*+=()[]{}\|'""./><?").ne.0)then
      maybe_value=.true.
   else  ! the last value was a null string so see if this matches an allowed parameter
      pointer=0
      if(current_argument_padded(1:2).eq.'--')then
         call locate_key(current_argument_padded(3:),pointer)
      elseif(current_argument_padded(1:1).eq.'-')then
         call locate_key(current_argument_padded(2:),pointer)
      endif
      if(pointer.le.0)then
         maybe_value=.true.
      else                   ! matched an option name so LIKELY is not a value
         maybe_value=.false.
      endif
   endif
end function maybe_value

end subroutine cmd_args_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     print_dictionary(3f) - [ARGUMENTS:M_CLI2] print internal dictionary created by calls to set_args(3f)
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!
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
      if(header.ne.'')then
         write(warn,'(a)')header
      endif
   endif
   if(allocated(keywords))then
      if(size(keywords).gt.0)then
         write(warn,'(a,1x,a,1x,a,1x,a)')atleast('KEYWORD',max(len(keywords),8)),'SHORT','PRESENT','VALUE'
         write(warn,'(*(a,1x,a5,1x,l1,8x,"[",a,"]",/))') &
         & (atleast(keywords(i),max(len(keywords),8)),shorts(i),present_in(i),values(i)(:counts(i)),i=1,size(keywords))
      endif
   endif
   if(allocated(unnamed))then
      if(size(unnamed).gt.0)then
         write(warn,'(a)')'UNNAMED'
         write(warn,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
      endif
   endif
   if(allocated(args))then
      if(size(args).gt.0)then
         write(warn,'(a)')'ARGS'
         write(warn,'(i6.6,3a)')(i,'[',args(i),']',i=1,size(args))
      endif
   endif
   if(G_remaining.ne.'')then
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
FUNCTION strtok(source_string,itoken,token_start,token_end,delimiters) result(strtok_status)
! JSU- 20151030

! ident_6="@(#)M_CLI2::strtok(3f): Tokenize a string"

character(len=*),intent(in)  :: source_string    ! Source string to tokenize.
character(len=*),intent(in)  :: delimiters       ! list of separator characters. May change between calls
integer,intent(inout)        :: itoken           ! token count since started
logical                      :: strtok_status    ! returned value
integer,intent(out)          :: token_start      ! beginning of token found if function result is .true.
integer,intent(inout)        :: token_end        ! end of token found if function result is .true.
integer                      :: isource_len
!----------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(itoken.le.0)then                           ! this is assumed to be the first call
      token_start=1
   else                                          ! increment start to previous end + 1
      token_start=token_end+1
   endif
!----------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!----------------------------------------------------------------------------------------------------------------------------
   if(token_start.gt.isource_len)then            ! user input error or at end of string
      token_end=isource_len                      ! assume end of token is end of string until proven otherwise so it is set
      strtok_status=.false.
      return
   endif
!----------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   do while (token_start .le. isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_start:token_start)) .ne. 0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end .le. isource_len-1)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_end+1:token_end+1)) .ne. 0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   if (token_start .gt. isource_len) then        ! determine if finished
      strtok_status=.false.                      ! flag that input string has been completely processed
   else
      itoken=itoken+1                            ! increment count of tokens found
      strtok_status=.true.                       ! flag more tokens may remain
   endif
!----------------------------------------------------------------------------------------------------------------------------
end function strtok
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     get_args(3f) - [ARGUMENTS:M_CLI2] return keyword values when parsing command line arguments
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
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
!!      character(len=:),allocatable :: value
!!      ! or
!!      character(len=:),allocatable :: value(:)
!!      ! or
!!      [real|doubleprecision|integer|logical|complex] :: value
!!      ! or
!!      [real|doubleprecision|integer|logical|complex],allocatable :: value(:)
!!
!!      character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    GET_ARGS(3f) returns the value of keywords after SET_ARGS(3f)
!!    has been called. For fixed-length CHARACTER variables
!!    see GET_ARGS_FIXED_LENGTH(3f). For fixed-size arrays see
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
!!
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
!!     ! DEFINE ARGS
!!     real                         :: x, y, z
!!     real,allocatable             :: p(:)
!!     character(len=:),allocatable :: title
!!     logical                      :: l, lbig
!!     ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!     !   o only quote strings and use double-quotes
!!     !   o set all logical values to F or T.
!!     call set_args(' &
!!        &-x 1 -y 2 -z 3 &
!!        &-p -1,-2,-3 &
!!        &--title "my title" &
!!        & -l F -L F  &
!!        & --label " " &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!     ! SCALARS
!!     call get_args('x',x,'y',y,'z',z)
!!     call get_args('l',l)
!!     call get_args('L',lbig)
!!     ! ALLOCATABLE STRING
!!     call get_args('title',title)
!!     ! NON-ALLOCATABLE ARRAYS
!!     call get_args('p',p)
!!     ! USE VALUES
!!     write(*,'(1x,g0,"=",g0)')'x',x, 'y',y, 'z',z
!!     write(*,*)'p=',p
!!     write(*,*)'title=',title
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     if(size(filenames).gt.0)then
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
!!    get_args_fixed_length(3f) - [ARGUMENTS:M_CLI2] return keyword values for fixed-length string when parsing command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine get_args_fixed_length(name,value)
!!
!!     character(len=:),allocatable :: value
!!     character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    GET_ARGS_fixed_length(3f) returns the value of a string
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
!!     ! DEFINE ARGS
!!     character(len=80)   :: title
!!     call set_args(' &
!!        & -title "my title" &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!        call get_args_fixed_length('title',title)
!!     ! USE VALUES
!!        write(*,*)'title=',title
!!     end program demo_get_args_fixed_length
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!>
!!##NAME
!!    get_args_fixed_size(3f) - [ARGUMENTS:M_CLI2] return keyword values for fixed-size array when parsing command line arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine get_args_fixed_size(name,value)
!!
!!     [real|doubleprecision|integer|logical|complex] :: value(NNN)
!!        or
!!     character(len=MMM) :: value(NNN)
!!
!!     character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    GET_ARGS_FIXED_SIZE(3f) returns the value of keywords for
!!    fixed-size arrays after SET_ARGS(3f) has been called.
!!    On input on the command line all values of the array must
!!    be specified.
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
!!        & -title "my title" &
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

! ident_7="@(#)M_CLI2::get_anyarray_l(3f): given keyword fetch logical array from string in dictionary(F on err)"

character(len=*),intent(in)  :: keyword                    ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
logical,allocatable          :: larray(:)                  ! convert value to an array
character(len=*),intent(in),optional   :: delimiters
character(len=:),allocatable :: carray(:)                  ! convert value to an array
character(len=:),allocatable :: val
integer                      :: i
integer                      :: place
integer                      :: iichar                     ! point to first character of word unless first character is "."
   call locate_key(keyword,place)                          ! find where string is or should be
   if(place.gt.0)then                                      ! if string was found
      val=values(place)(:counts(place))
      call split(adjustl(upper(val)),carray,delimiters=delimiters)  ! convert value to uppercase, trimmed; then parse into array
   else
      call journal('sc','*get_anyarray_l* unknown keyword '//keyword)
      call mystop(8 ,'*get_anyarray_l* unknown keyword '//keyword)
      if(allocated(larray))deallocate(larray)
      allocate(larray(0))
      return
   endif
   if(size(carray).gt.0)then                                  ! if not a null string
      if(allocated(larray))deallocate(larray)
      allocate(larray(size(carray)))                          ! allocate output array
      do i=1,size(carray)
         larray(i)=.false.                                    ! initialize return value to .false.
         if(carray(i)(1:1).eq.'.')then                        ! looking for fortran logical syntax .STRING.
            iichar=2
         else
            iichar=1
         endif
         select case(carray(i)(iichar:iichar))             ! check word to see if true or false
         case('T','Y',' '); larray(i)=.true.               ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
         case('F','N');     larray(i)=.false.              ! assume this is false or no
         case default
            call journal('sc',"*get_anyarray_l* bad logical expression for "//trim(keyword)//'='//carray(i))
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

! ident_8="@(#)M_CLI2::get_anyarray_d(3f): given keyword fetch dble value array from Language Dictionary (0 on err)"

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
   if(place.gt.0)then                                 ! if string was found
      val=values(place)(:counts(place))
      val=replace_str(val,'(','')
      val=replace_str(val,')','')
      call split(val,carray,delimiters=delimiters)    ! find value associated with keyword and split it into an array
   else
      call journal('sc','*get_anyarray_d* unknown keyword '//keyword)
      call mystop(9 ,'*get_anyarray_d* unknown keyword '//keyword)
      if(allocated(darray))deallocate(darray)
      allocate(darray(0))
      return
   endif
   if(allocated(darray))deallocate(darray)
   allocate(darray(size(carray)))                     ! create the output array
   do i=1,size(carray)
      call a2d(carray(i), darray(i),ierr) ! convert the string to a numeric value
      if(ierr.ne.0)then
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
complex,allocatable                  :: xarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: half,sz,i
   call get_anyarray_d(keyword,darray,delimiters)
   sz=size(darray)
   half=sz/2
   if(sz.ne.half+half)then
      call journal('sc','*get_anyarray_x* uneven number of values defining complex value '//keyword)
      call mystop(11,'*get_anyarray_x* uneven number of values defining complex value '//keyword)
      if(allocated(xarray))deallocate(xarray)
      allocate(xarray(0))
   endif

   !x!================================================================================================
   !x!IFORT,GFORTRAN OK, NVIDIA RETURNS NULL ARRAY: xarray=cmplx(real(darray(1::2)),real(darray(2::2)))
   if(allocated(xarray))deallocate(xarray)
   allocate(xarray(half))
   do i=1,sz,2
      xarray((i+1)/2)=cmplx( darray(i),darray(i+1) )
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
      call journal('sc','*get_anyarray_c* unknown keyword '//keyword)
      call mystop(12,'*get_anyarray_c* unknown keyword '//keyword)
      if(allocated(strings))deallocate(strings)
      allocate(character(len=0)::strings(0))
   endif
end subroutine get_anyarray_c
!===================================================================================================================================
!===================================================================================================================================
subroutine get_args_fixed_length_a_array(keyword,strings,delimiters)

! ident_9="@(#)M_CLI2::get_args_fixed_length_a_array(3f): Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)          :: keyword       ! name to look up in dictionary
character(len=*),allocatable         :: strings(:)
character(len=*),intent(in),optional :: delimiters
character(len=:),allocatable         :: strings_a(:)
integer                              :: place
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,strings_a,delimiters=delimiters)   ! find value associated with keyword and split it into an array
      if(len(strings_a).le.len(strings))then
         strings=strings_a
      else
         call journal('sc','*get_args_fixed_length_a_array* values too long. Longest is',len(strings_a),'allowed is',len(strings))
         write(*,'("strings=",3x,*(a,1x))')strings
         call journal('sc','*get_args_fixed_length_a_array* keyword='//keyword)
         call mystop(13,'*get_args_fixed_length_a_array* keyword='//keyword)
         strings=[character(len=len(strings)) ::]
      endif
   else
      call journal('sc','*get_args_fixed_length_a_array* unknown keyword '//keyword)
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
   call get_anyarray_d(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(iarray,dim=1).eq.dsize)then
      iarray=nint(darray)
   else
      call journal('sc','*get_fixedarray_i* wrong number of values for keyword',keyword,'got',dsize,'expected',size(iarray))
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
   call get_anyarray_r(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(rarray,dim=1).eq.dsize)then
      rarray=darray
   else
      call journal('sc','*get_fixedarray_r* wrong number of values for keyword',keyword,'got',dsize,'expected',size(rarray))
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
   call get_anyarray_x(keyword,darray,delimiters)
   dsize=size(darray)
   sz=dsize*2
   half=sz/2
   if(sz.ne.half+half)then
      call journal('sc','*get_fixed_size_complex* uneven number of values defining complex value '//keyword)
      call mystop(15,'*get_fixed_size_complex* uneven number of values defining complex value '//keyword)
      xarray=0
      return
   endif
   if(ubound(xarray,dim=1).eq.dsize)then
      xarray=darray
   else
      call journal('sc','*get_fixed_size_complex* wrong number of values for keyword',keyword,'got',dsize,'expected',size(xarray))
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
   call get_anyarray_d(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(darr,dim=1).eq.dsize)then
      darr=darray
   else
      call journal('sc','*get_fixedarray_d* wrong number of values for keyword',keyword,'got',dsize,'expected',size(darr))
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
   call get_anyarray_l(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(larray,dim=1).eq.dsize)then
      larray=darray
   else
      call journal('sc','*get_fixedarray_l* wrong number of values for keyword',keyword,'got',dsize,'expected',size(larray))
      call print_dictionary('USAGE:')
      call mystop(36)
      larray=.false.
   endif
end subroutine get_fixedarray_l
!===================================================================================================================================
subroutine get_fixedarray_fixed_length_c(keyword,strings,delimiters)

! ident_10="@(#)M_CLI2::get_fixedarray_fixed_length_c(3f): Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*)                     :: strings(:)
character(len=*),intent(in),optional :: delimiters
character(len=:),allocatable         :: str(:)
character(len=*),intent(in)          :: keyword   ! name to look up in dictionary
integer                              :: place
integer                              :: ssize
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                 ! find where string is or should be
   if(place > 0)then                              ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,str,delimiters=delimiters)   ! find value associated with keyword and split it into an array
      ssize=size(str)
      if(ssize==size(strings))then
         strings(:ssize)=str
      else
         call journal('sc','*get_fixedarray_fixed_length_c* wrong number of values for keyword',&
            & keyword,'got',ssize,'expected ',size(strings)) !,ubound(strings,dim=1)
         call print_dictionary('USAGE:')
         call mystop(30,'*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
         strings=''
      endif
   else
      call journal('sc','*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
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
   call get_anyarray_d(keyword,darray)
   if(size(darray).eq.1)then
      d=darray(1)
   else
      call journal('sc','*get_anyarray_d* incorrect number of values for keyword',keyword,'expected one found',size(darray))
      call print_dictionary('USAGE:')
      call mystop(31,'*get_anyarray_d* incorrect number of values for keyword'//keyword//'expected one')
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

! ident_11="@(#)M_CLI2::get_scalar_anylength_c(3f): Fetch string value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)   :: keyword              ! name to look up in dictionary
character(len=:),allocatable,intent(out)  :: string
integer                       :: place
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return string
      string=unquote(values(place)(:counts(place)))
   else
      call mystop(17,'*get_anyarray_c* unknown keyword '//keyword)
      call journal('sc','*get_anyarray_c* unknown keyword '//keyword)
      string=''
   endif
end subroutine get_scalar_anylength_c
!===================================================================================================================================
elemental impure subroutine get_args_fixed_length_scalar_c(keyword,string)

! ident_12="@(#)M_CLI2::get_args_fixed_length_scalar_c(3f): Fetch string value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)   :: keyword              ! name to look up in dictionary
character(len=*),intent(out)  :: string
integer                       :: place
integer                       :: unlen
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return string
      string=unquote(values(place)(:counts(place)))
   else
      call mystop(18,'*get_args_fixed_length_scalar_c* unknown keyword '//keyword)
      string=''
   endif
   unlen=len_trim(unquote(values(place)(:counts(place))))
   if(unlen>len(string))then
      call journal('sc','*get_args_fixed_length_scalar_c* value too long for',keyword,'allowed is',len(string),&
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
   if(size(d).eq.2)then
      x=cmplx(d(1),d(2),kind=sp)
   else
      call journal('sc','*get_scalar_complex* expected two values found',size(d))
      call mystop(20,'*get_scalar_complex* incorrect number of values for keyword '//keyword)
      x=cmplx(0.0,0.0)
   endif
end subroutine get_scalar_complex
!===================================================================================================================================
subroutine get_scalar_logical(keyword,l)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
logical                       :: l
logical,allocatable           :: larray(:)    ! function type
   call get_anyarray_l(keyword,larray)
   if(size(larray).eq.1)then
      l=larray(1)
   else
      call journal('sc','*get_anyarray_l* expected one value found',size(larray))
      call mystop(21,'*get_anyarray_l* incorrect number of values for keyword '//keyword)
      l=.false.
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
!!    longest_command_argument(3f) - [ARGUMENTS:M_args] length of longest argument on command line
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function longest_command_argument() result(ilongest)
!!
!!     integer :: ilongest
!!
!!##DESCRIPTION
!!    length of longest argument on command line. Useful when allocating storage for holding arguments.
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
      elseif(ilength.gt.0)then
         ilongest=max(ilongest,ilength)
      endif
   enddo GET_LONGEST
end function longest_command_argument
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine journal(where, g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep)
implicit none

! ident_13="@(#)M_CLI2::journal(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1, g2, g3, g4, g5, g6, g7, g8 ,g9
class(*),intent(in),optional  :: ga, gb, gc, gd, ge, gf, gg, gh ,gi, gj
character(len=*),intent(in),optional :: sep
if(debug_m_cli2)write(*,*)'<DEBUG>JOURNAL:',present(g1)
if(debug_m_cli2)write(*,*)'<DEBUG>JOURNAL:',present(g2)
if(debug_m_cli2)write(*,*)'<DEBUG>JOURNAL:',present(sep)
write(*,'(a)')str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep)
end subroutine journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    str(3f) - [M_CLI2] converts any standard scalar type to a string
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function str(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
!!
!!     class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!     character(len=*),intent(in),optional :: sep
!!     character,len=(:),allocatable :: str
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
!!    sep         separator to place between values. Defaults to a space.
!!##RETURNS
!!    str     description to print
!!##EXAMPLES
!!
!! Sample program:
!!
!!       program demo_msg
!!       use M_CLI2, only : str
!!       implicit none
!!       character(len=:),allocatable :: pr
!!       character(len=:),allocatable :: frmt
!!       integer                      :: biggest
!!
!!       pr=str('HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!       write(*,'(a)')pr
!!       pr=str('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!       write(*,'(a)')pr
!!       pr=str('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!       write(*,'(a)')pr
!!       pr=str('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!       write(*,'(a)')pr
!!
!!       ! create a format on the fly
!!       biggest=huge(0)
!!       frmt=str('(*(i',int(log10(real(biggest))),':,1x))',sep=' ')
!!       write(*,*)'format=',frmt
!!
!!       ! although it will often work, using str(3f) in an I/O statement is not recommended
!!       ! because if an error occurs str(3f) will try to write while part of an I/O statement
!!       ! which not all compilers can handle and is currently non-standard
!!       write(*,*)str('program will now stop')
!!
!!       end program demo_msg
!!
!!  Output
!!
!!     HUGE(3f) integers 2147483647 and real 3.40282347E+38 and double 1.7976931348623157E+308
!!     real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!     doubleprecision : 1.7976931348623157E+308 0.0000000000000000 12345.678900000001 2.2250738585072014E-308
!!     complex         : (3.40282347E+38,1.17549435E-38)
!!      format=(*(i9:,1x))
!!      program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function msg_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none

! ident_14="@(#)M_CLI2::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(debug_m_cli2)write(*,gen)'<DEBUG>:MSG_SCALAR'
   if(present(sep))then
      sep_local=sep
      increment=len(sep_local)+1
   else
      sep_local=' '
      increment=2
   endif
   if(debug_m_cli2)write(*,gen)'<DEBUG>:MSG_SCALAR'

   istart=1
   line=''
   if(debug_m_cli2)write(*,gen)'<DEBUG>:MSG_SCALAR:CALL GENERIC:GENERIC0'
   if(present(generic0))call print_generic(generic0)
   if(debug_m_cli2)write(*,gen)'<DEBUG>:MSG_SCALAR:CALL GENERIC:GENERIC1'
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
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:START'
   if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:LINE',trim(line)
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64))
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:REAL64'
         write(line(istart:),'(1pg0)') generic
      !x! DOES NOT WORK WITH NVFORTRAN: type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical)
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:REAL64'
         write(line(istart:),'(l1)') generic
      type is (character(len=*))
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:CHARACTER'
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:ISTART:',istart
         write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:START'
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function msg_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

! ident_15="@(#)M_CLI2::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep_local)+1
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
   msg_one=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
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
      !x! DOES NOT WORK WITH nvfortran: type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      !x! DOES NOT WORK WITH ifort:     type is (real(kind=real256));     write(error_unit,'(1pg0)',advance='no') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*))
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:CHARACTER'
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:ISTART:',istart
         write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
      class default
         call mystop(-22,'unknown type in *print_generic*')
   end select
   istart=len_trim(line)+increment+1
   line=trim(line)//"]"//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function upper(str) result (string)

! ident_16="@(#)M_CLI2::upper(3f): Changes a string to uppercase"

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

! ident_17="@(#)M_CLI2::lower(3f): Changes a string to lowercase over specified range"

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

! ident_18="@(#)M_CLI2::a2i(3fp): subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8.le.huge(valu))then
      if(valu8.le.huge(valu))then
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

! ident_19="@(#)M_CLI2::a2d(3fp): subroutine returns double value from string"

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
   if(len(local_chars).eq.0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd.ne.0)then
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
         frmt='(Z'//i2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         frmt='(B'//i2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         frmt='(O'//i2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr.ne.0)then                                            ! if an error occurred ierr will be non-zero.
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
      if(local_chars.ne.'eod')then                           ! print warning message except for special value "eod"
         call journal('sc','*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg.ne.'')then
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
!!    split(3f) - [M_CLI2:TOKENS] parse string into an array using specified delimiters
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

! ident_20="@(#)M_CLI2::split(3f): parse string on delimiter characters and store tokens into an allocatable array"

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
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
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
   if(iilen.gt.0)then                                             ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,iilen,1                                  ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=iilen                                      ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):iilen),dlim(i10:i10))
               IF(ifound.gt.0)then
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
         if(icol.gt.iilen)then                                     ! no text left
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
      if(iterm(i20).lt.ibegin(i20))then
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
!!    replace_str(3f) - [M_CLI2:EDITING] function globally replaces one substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function replace_str(targetline[,old,new|cmd],range,ierr) result (newline)
!!
!!     character(len=*)                       :: targetline
!!     character(len=*),intent(in),optional   :: old
!!     character(len=*),intent(in),optional   :: new
!!     character(len=*),intent(in),optional   :: cmd
!!     integer,intent(in),optional            :: range(2)
!!     integer,intent(out),optional           :: ierr
!!     logical,intent(in),optional            :: clip
!!     character(len=:),allocatable           :: newline
!!##DESCRIPTION
!!    Globally replace one substring for another in string.
!!    Either CMD or OLD and NEW must be specified.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     cmd         alternate way to specify old and new string, in
!!                 the form c/old/new/; where "/" can be any character
!!                 not in "old" or "new"
!!     range       if present, only change range(1) to range(2) of occurrences of old string
!!     ierr        error code. iF ier = -1 bad directive, >= 0 then
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
!!       write(*,*)'Examples of the use of RANGE='
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','A')
!!       write(*,*)'replace a with A ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','A',range=[3,5])
!!       write(*,*)'replace a with A instances 3 to 5 ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','',range=[3,5])
!!       write(*,*)'replace a with null instances 3 to 5 ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa aa aa a a a aa aaaaaa','aa','CCCC',range=[3,5])
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
!!       write(*,*)':TEST    [',targetline.eq.expected,']'
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
!!     Examples of the use of RANGE=
!!     replace a with A [A b Ab bAAA AAAA]
!!     replace a with A instances 3 to 5 [a b ab bAAA aaaa]
!!     replace a with null instances 3 to 5 [a b ab b aaaa]
!!     replace aa with CCCC instances 3 to 5 [a b ab baaa aaCCCC CCCC CCCC a a a aa aaaaaa]
!!
!!##AUTHOR
!!    John S. Urban
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

   if(lmax.ge.4)then                      ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)               ! find delimiter in expected location
      itoken=0                            ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id).eq.cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token .eq. (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
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
function replace_str(targetline,old,new,ierr,cmd,range) result (newline)

! ident_21="@(#)M_CLI2::replace_str(3f): Globally replace one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
! parameters
character(len=*),intent(in)            :: targetline   ! input line to be changed
character(len=*),intent(in),optional   :: old          ! old substring to replace
character(len=*),intent(in),optional   :: new          ! new substring
integer,intent(out),optional           :: ierr         ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
character(len=*),intent(in),optional   :: cmd          ! contains the instructions changing the string
integer,intent(in),optional            :: range(2)     ! start and end of which changes to make
!-----------------------------------------------------------------------------------------------------------------------------------
! returns
character(len=:),allocatable  :: newline               ! output string buffer
!-----------------------------------------------------------------------------------------------------------------------------------
! local
character(len=:),allocatable  :: new_local, old_local
integer                       :: icount,ichange,ier2
integer                       :: original_input_length
integer                       :: len_old, len_new
integer                       :: ladd
integer                       :: left_margin, right_margin
integer                       :: ind
integer                       :: ic
integer                       :: iichar
integer                       :: range_local(2)
!-----------------------------------------------------------------------------------------------------------------------------------
!  get old_local and new_local from cmd or old and new
   if(present(cmd))then
      call crack_cmd(cmd,old_local,new_local,ier2)
      if(ier2.ne.0)then
         newline=targetline  ! if no changes are made return original string on error
         if(present(ierr))ierr=ier2
         return
      endif
   elseif(present(old).and.present(new))then
      old_local=old
      new_local=new
   else
      newline=targetline  ! if no changes are made return original string on error
      call journal('sc','*replace_str* must specify OLD and NEW or CMD')
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   len_old=len(old_local)                              ! length of old substring to be replaced
   len_new=len(new_local)                              ! length of new substring to replace old substring
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
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      iichar=len_new + original_input_length
      if(len_new.gt.0)then
         newline=new_local(:len_new)//targetline(left_margin:original_input_length)
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
      ind=index(targetline(ic:),old_local(:len_old))+ic-1 ! try finding start of OLD in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.right_margin)then          ! did not find old string or found old string past edit window
         exit loop                                        ! no more changes left to make
      endif
      icount=icount+1                                  ! found an old string to change, so increment count of change candidates
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         newline=newline(:iichar-1)//targetline(ic:ind-1)
         iichar=iichar+ladd
      endif
      if(icount.ge.range_local(1).and.icount.le.range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new.ne.0)then                                          ! put in new string
            newline=newline(:iichar-1)//new_local(:len_new)
            iichar=iichar+len_new
         endif
      else
         if(len_old.ne.0)then                                          ! put in copy of old string
            newline=newline(:iichar-1)//old_local(:len_old)
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
      if(ic.le.len(targetline))then                    ! if there is more after last change on original line add it
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
!!     quote(3f) - [M_CLI2:QUOTES] add quotes to string as if written with list-directed input
!!     (LICENSE:PD)
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
!!                list-directed input (single quotes are replaced by two adjacent quotes)
!!    mode        alternate quoting methods are supported:
!!
!!                   DOUBLE   default. replace quote with double quotes
!!                   ESCAPE   replace quotes with backslash-quote instead of double quotes
!!
!!    clip        default is to trim leading and trailing spaces from the string. If CLIP
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
!!           if(ios.ne.0)then
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
!-----------------------------------------------------------------------------------------------------------------------------------
   local_mode=merge_str(mode,'DOUBLE',present(mode))
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
!!     unquote(3f) - [M_CLI2:QUOTES] remove quotes from string as if read with list-directed input
!!     (LICENSE:PD)
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
!!    unquoted_str  The output string, which is based on removing quotes from quoted_str.
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
!!          if(ios.ne.0)then
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
!!          if(ios.ne.0)then
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
   if(inlen.ge.1)then                             ! double_quote is the default quote unless the first character is single_quote
      if(quoted_str(1:1).eq.single_quote)then
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
      if(before.eq.iesc)then                      ! if previous character was escape use current character unconditionally
           iput=iput-1                            ! backup
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2                              ! this could be second esc or quote
      elseif(current.eq.quote)then                ! if current is a quote it depends on whether previous character was a quote
         if(before.eq.quote)then
           unquoted_str(iput:iput)=char(quote)    ! this is second quote so retain it
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before.ne.iesc)then
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
function i2s(ivalue,fmt) result(outstr)

! ident_22="@(#)M_CLI2::i2s(3fp): private function returns string given integer value"

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
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    merge_str(3f) - [M_CLI2:LENGTH] pads strings to same length and then calls MERGE(3f)
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
!! Sample Program:
!!
!!     program demo_merge_str
!!     use M_CLI2, only : merge_str
!!     implicit none
!!     character(len=:), allocatable :: answer
!!        answer=merge_str('first string', 'second string is longer',10.eq.10)
!!        write(*,'("[",a,"]")') answer
!!        answer=merge_str('first string', 'second string is longer',10.ne.10)
!!        write(*,'("[",a,"]")') answer
!!     end program demo_merge_str
!!
!!   Expected output
!!
!!     [first string]
!!     [second string is longer]
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces

! ident_23="@(#)M_CLI2::merge_str(3f): pads first and second arguments to MERGE(3f) to same length"

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
!!
!!    decodebase(3f) - [M_CLI2:BASE] convert whole number string in base [2-36] to base 10 number
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
!!         if(x.eq.'0') exit INFINITE
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
implicit none

! ident_24="@(#)M_CLI2::decodebase(3f): convert whole number string in base [2-36] to base 10 number"

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
  if(basein.eq.0.and.ipound.gt.1)then                                  ! split string into two values
     call a2i(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local.ge.0)then                                         ! allow for a negative sign prefix
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
        if(ch.eq.'-'.and.k.eq.1)then
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
!!    lenset(3f) - [M_CLI2:LENGTH] return string trimmed or padded to specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lenset(str,length) result(strout)
!!
!!     character(len=*)                     :: str
!!     character(len=length)                :: strout
!!     integer,intent(in)                   :: length
!!##DESCRIPTION
!!    lenset(3f) truncates a string or pads it with spaces to the specified
!!    length.
!!##OPTIONS
!!    str     input string
!!    length  output string length
!!##RESULTS
!!    strout  output string
!!##EXAMPLE
!!
!! Sample Program:
!!
!!     program demo_lenset
!!      use M_CLI2, only : lenset
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
!!##LICENSE
!!    Public Domain
function lenset(line,length) result(strout)

! ident_25="@(#)M_CLI2::lenset(3f): return string trimmed or padded to specified length"

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
!!      value_to_string(3f) - [M_CLI2:NUMERIC] return numeric string from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine value_to_string(value,chars[,iilen,ierr,fmt,trimz])
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
!!     integer,intent(out),optional             :: iilen
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
!!               23 characters long; or what is required by optional FMT if longer.
!!       IILEN   position of last non-blank character in returned string; optional.
!!       IERR    If not zero, error occurred; optional.
!!##EXAMPLE
!!
!! Sample program:
!!
!!      program demo_value_to_string
!!      use M_CLI2, only: value_to_string
!!      implicit none
!!      character(len=80) :: string
!!      integer           :: iilen
!!         call value_to_string(3.0/4.0,string,iilen)
!!         write(*,*) 'The value is [',string(:iilen),']'
!!
!!         call value_to_string(3.0/4.0,string,iilen,fmt='')
!!         write(*,*) 'The value is [',string(:iilen),']'
!!
!!         call value_to_string(3.0/4.0,string,iilen,fmt='("THE VALUE IS ",g0)')
!!         write(*,*) 'The value is [',string(:iilen),']'
!!
!!         call value_to_string(1234,string,iilen)
!!         write(*,*) 'The value is [',string(:iilen),']'
!!
!!         call value_to_string(1.0d0/3.0d0,string,iilen)
!!         write(*,*) 'The value is [',string(:iilen),']'
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
!!##LICENSE
!!    Public Domain
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

! ident_26="@(#)M_CLI2::value_to_string(3fp): subroutine returns a string from a value"

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
         call journal('sc','*value_to_string* UNKNOWN TYPE')
         chars=' '
      end select
      if(fmt.eq.'') then
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
      if(index(chars,'.').ne.0) call trimzeros_(chars)
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
   elseif(err_local.ne.0)then
      !-! cannot currently do I/O from a function being called from I/O
      !-!write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    trimzeros_(3fp) - [M_CLI2:NUMERIC] Delete trailing zeros from numeric decimal string
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine trimzeros_(str)
!!
!!     character(len=*)  :: str
!!##DESCRIPTION
!!    TRIMZEROS_(3f) deletes trailing zeros from a string representing a
!!    number. If the resulting string would end in a decimal point, one
!!    trailing zero is added.
!!##OPTIONS
!!    str   input string will be assumed to be a numeric value and have trailing
!!          zeros removed
!!##EXAMPLES
!!
!! Sample program:
!!
!!       program demo_trimzeros_
!!       use M_CLI2, only : trimzeros_
!!       character(len=:),allocatable :: string
!!          write(*,*)trimzeros_('123.450000000000')
!!          write(*,*)trimzeros_('12345')
!!          write(*,*)trimzeros_('12345.')
!!          write(*,*)trimzeros_('12345.00e3')
!!       end program demo_trimzeros_
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine trimzeros_(string)

! ident_27="@(#)M_CLI2::trimzeros_(3fp): Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: expo         ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      expo=str(ipos:)                        ! keep exponent string so it can be added back as a suffix
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
      string=trim(str)//trim(expo)
   else
      string=str
   endif
end subroutine trimzeros_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    substitute(3f) - [M_CLI2:EDITING] subroutine globally substitutes one substring for another in string
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
!! Sample Program:
!!
!!     program demo_substitute
!!     use M_CLI2, only : substitute
!!     implicit none
!!     ! must be long enough to hold changed line
!!     character(len=80) :: targetline
!!
!!     targetline='this is the input string'
!!     write(*,*)'ORIGINAL    : '//trim(targetline)
!!
!!     ! changes the input to 'THis is THe input string'
!!     call substitute(targetline,'th','TH')
!!     write(*,*)'th => TH    : '//trim(targetline)
!!
!!     ! a null old substring means "at beginning of line"
!!     ! changes the input to 'BEFORE:this is the input string'
!!     call substitute(targetline,'','BEFORE:')
!!     write(*,*)'"" => BEFORE: '//trim(targetline)
!!
!!     ! a null new string deletes occurrences of the old substring
!!     ! changes the input to 'ths s the nput strng'
!!     call substitute(targetline,'i','')
!!     write(*,*)'i => ""     : '//trim(targetline)
!!
!!     end program demo_substitute
!!
!!   Expected output
!!
!!     ORIGINAL    : this is the input string
!!     th => TH    : THis is THe input string
!!     "" => BEFORE: BEFORE:THis is THe input string
!!     i => ""     : BEFORE:THs s THe nput strng
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine substitute(targetline,old,new,ierr,start,end)

! ident_28="@(#)M_CLI2::substitute(3f): Globally substitute one substring for another in string"

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
integer                        :: iichar
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
   id=mr-ml                                            ! check for window option !-! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id.le.0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      iichar=len_new + original_input_length
      if(iichar.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new.gt.0)then
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
   iichar=il                                           ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(iichar-1+ladd.gt.maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(iichar:)=targetline(ic:ind-1)
         iichar=iichar+ladd
      endif
      if(iichar-1+len_new.gt.maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new.ne.0)then
         dum1(iichar:)=new(:len_new)
         iichar=iichar+len_new
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
      if(iichar+ladd.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic.lt.len(targetline))then
         dum1(iichar:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    locate(3f) - [M_CLI2] finds the index where a string is found or should be in a sorted array
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
!! Find if a string is in a sorted array, and insert the string into
!! the list if it is not present ...
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate
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

! ident_29="@(#)M_CLI2::locate_c(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

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
      elseif(value.gt.list(place))then
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
   elseif(error.ne.0)then
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
!!    remove(3f) - [M_CLI2] remove entry from an allocatable array at specified position
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
!! Sample program
!!
!!     program demo_remove
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate, remove
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

! ident_30="@(#)M_CLI2::remove_c(3fp): remove string from allocatable string array at specified position"

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
subroutine remove_l(list,place)

! ident_31="@(#)M_CLI2::remove_l(3fp): remove value from allocatable array at specified position"

logical,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

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

end subroutine remove_l
subroutine remove_i(list,place)

! ident_32="@(#)M_CLI2::remove_i(3fp): remove value from allocatable array at specified position"
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
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_CLI2] replace entry in a string array at specified position
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
!! Replace key-value pairs in a dictionary
!!
!!     program demo_replace
!!     use M_CLI2, only  : insert, locate, replace
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
!!     call locate_key('a',place)
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
!!     call locate_key(key,place)
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

! ident_33="@(#)M_CLI2::replace_c(3fp): replace string in allocatable string array at specified position"

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
           write(warn,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value).le.len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
end subroutine replace_c
subroutine replace_l(list,value,place)

! ident_34="@(#)M_CLI2::replace_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(warn,*)'*replace_l* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_l
subroutine replace_i(list,value,place)

! ident_35="@(#)M_CLI2::replace_i(3fp): place value into allocatable array at specified position"

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
      write(warn,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    insert(3f) - [M_CLI2] insert entry into a string array at specified position
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
!! Find if a string is in a sorted array, and insert the string into
!! the list if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate, insert
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

! ident_36="@(#)M_CLI2::insert_c(3fp): place string into allocatable string array at specified position"

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
      write(warn,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_c
subroutine insert_l(list,value,place)

! ident_37="@(#)M_CLI2::insert_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
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
      write(warn,*)'*insert_l* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_l
subroutine insert_i(list,value,place)

! ident_38="@(#)M_CLI2::insert_i(3fp): place value into allocatable array at specified position"

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
      write(warn,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine many_args(n0,g0, n1,g1, n2,g2, n3,g3, n4,g4, n5,g5, n6,g6, n7,g7, n8,g8, n9,g9, &
                   & na,ga, nb,gb, nc,gc, nd,gd, ne,ge, nf,gf, ng,gg, nh,gh, ni,gi, nj,gj )
implicit none

! ident_39="@(#)M_CLI2::many_args(3fp): allow for multiple calls to get_args(3f)"

character(len=*),intent(in)          :: n0, n1
character(len=*),intent(in),optional ::         n2, n3, n4, n5, n6, n7, n8, n9, na, nb, nc, nd, ne, nf, ng, nh, ni, nj
class(*),intent(out)           :: g0, g1
class(*),intent(out),optional  ::         g2, g3, g4, g5, g6, g7, g8, g9
class(*),intent(out),optional  :: ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
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
      if(hold(1:1).eq.'.')then                 ! looking for fortran logical syntax .STRING.
         iichar=2
      else
         iichar=1
      endif
      select case(hold(iichar:iichar))         ! check word to see if true or false
      case('T','Y',' '); lg(i)=.true.          ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
      case('F','N');     lg(i)=.false.         ! assume this is false or no
      case default
         call journal('sc',"*lg* bad logical expression for element",i,'=',hold)
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
function sg()
character(len=:),allocatable :: sg(:)
   sg=unnamed
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
   !x!write(*,*)'MYSTOP:',sig,trim(msg)
   if(sig.lt.0)then
      if(present(msg))call journal('sc',msg)
      !x!stop abs(sig)
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
      !x!write(*,*)'G_STOP:',g_stop,trim(msg)
   endif
end subroutine mystop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function atleast(line,length,pattern) result(strout)

! ident_40="@(#)M_strings::atleast(3f): return string padded to at least specified length"

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

! ident_41="@(#)M_CLI2::locate_key(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
integer                                 :: ii
   if(len_trim(value).eq.1)then
      !x!ii=findloc(shorts,value,dim=1)
      ii=maxloc([0,merge(1, 0, shorts.eq.value)],dim=1)
      if(ii.gt.1)then
         place=ii-1
      else
         call locate(keywords,value,place)
      endif
   else
      call locate(keywords,value,place)
   endif
end subroutine locate_key
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_CLI2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! REVISION:  nvfortran does not support real128 from iso_fortran_env x86_64 GNU/Linux
!            nvfortran 20.7-0 LLVM 64-bit target on x86-64 Linux -tp nehalem
! < !NVFORTRAN-S-0000-Internal compiler error. size_of: attempt to get size of assumed size character       0  (M_CLI2.f90: 2012)
! < !  0 inform,   0 warnings,   1 severes, 0 fatal for get_anyarray_cc
! Changed
!       allocate(character(len=*)::strings(0))
! to
!       strings=[character(len=len(strings)) ::]
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
end module M_journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
 
!>>>>> build/dependencies/M_msg/src/M_verify.f90
!>
!!##NAME
!!    M_verify(3fm) - [M_verify] a collection of Fortran routines for
!!                    supporting code development by providing error
!!                    processing, debugging procedures and unit testing.
!!                    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!  Module procedures
!!
!!    use M_verify, only : unit_check, unit_check_start, unit_check_done, unit_check_stop
!!    use M_verify, only : unit_check_good, unit_check_bad
!!    use M_verify, only : unit_check_msg
!!    use M_verify, only : debug
!!    use M_verify, only : fstop
!!    use M_verify, only : assert
!!
!!  Module values
!!
!!    use M_verify, only : unit_check_limit, unit_check_keep_going
!!    use M_verify, only : unit_check_command
!!
!!##QUOTE
!!    Do not let your victories go to your head, nor let your failures go
!!    to your heart.
!!
!!##DESCRIPTION
!!    The M_verify(3fm) Fortran module provides procedures and variables
!!    useful for providing error processing, debugging capabilities, and
!!    unit testing.
!!
!!     o allows for a user-defined command to be called to collect results or
!!       mail failure alerts, ...
!!     o supports easily composing a message from up to nine scalar
!!       intrinsic values and different message levels
!!     o allows stopping on first failure or continuing
!!     o provides for a non-zero exit code if any tests fail
!!
!!    SET MODES
!!    unit_check_keep_going  logical variable that can be used to turn off
!!                           program termination on errors.
!!    unit_check_level       An integer that can be used to specify
!!                           different debug levels
!!    unit_check_command     name of command to execute. Defaults to the name
!!                           "goodbad".
!!    UNIT TESTS
!!    unit_check_start(3f)   start tests of a procedure and optionally call
!!
!!                              command NAME start ...
!!    unit_check(3f)         if expression is false optionally call
!!
!!                              command NAME bad
!!
!!                           and stop program (by default)
!!
!!    unit_check_done(3f)    call
!!
!!                              command NAME good
!!
!!                           if no failures; else call
!!
!!                              command NAME bad
!!   unit_check_stop(3f)     stop program with exit value of 0 if no failures
!!                           else with an exit value of 1
!!
!!    unit_check_good(3f)    call command
!!
!!                              command NAME good
!!
!!    unit_check_bad(3f)     call command
!!
!!                              command NAME bad
!!
!!                           and stop program by default
!!    unit_check_msg(3f)     write message
!!
!!    BASIC DEBUGGING
!!    fstop(3f)             calls 'STOP VALUE' passing in a value (1-32),
!!                          with optional message
!!    pdec(3f)              write ASCII Decimal Equivalent (ADE) numbers
!!                          vertically beneath string
!!    debug                 logical variable that can be tested by routines
!!                          as a flag to process debug statements.
!!
!!    For unit testing, the existence of a command called "goodbad" is
!!    initially assumed. This is generally a script that makes entries
!!    for each unit in an SQLite data file which is then used to create
!!    CSV and HTML reports on the status of each unit. A sample goodbad(1)
!!    command written in the bash(1) shell and using the sqlite3(1) command
!!    should be included in this distribution as an example.
!!
!!    The flexibility introduced by calling an external script or program
!!    is that The goodbad(1) command can be changed as desired to write CSV
!!    files or simple logs or to notify developers with e-mail as desired.
!!
!!    RELATED FUNCTIONS
!!
!!    The routines in M_verify(3f) are often combined with the M_hashkeys(3fm)
!!    routines and various math and statistical routines to quickly create
!!    unit tests.
!!
!!    Comparisons of real values can be done with a tolerance with
!!    M_Compare_Float_Numbers(3fm), for example.
!!
!!    The intrinsics ANY(3f) and ALL(3f) are particularly useful in calls
!!    to unit_check(3f).
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!     !!program demo_unit_tests
!!     module M_demo
!!     private
!!     public one !! regular routines
!!     public two !! regular routines
!!     public test_suite_M_demo !! special name for use with test_suite(1bash).
!!     contains
!!
!!     !!  regular routines
!!     subroutine one()
!!     end subroutine one
!!
!!     subroutine two()
!!     end subroutine two
!!
!!     !! unit test
!!     subroutine test_suite_M_demo
!!     use M_verify, only: unit_check_start, unit_check
!!     use M_verify, only: unit_check_good, unit_check_bad, unit_check_done
!!     use M_verify, only: unit_check_msg, unit_check_stop
!!     implicit none
!!     integer :: i, j, k
!!     integer,allocatable :: array(:)
!!     integer :: arr(4)=[21,51,14,45]
!!     integer :: a=21, b=51, c=14, d=45
!!     ! TEST-DRIVEN DEVELOPMENT
!!     ! optional set-up       perform initialization operations common to all tests within a module
!!        i=1
!!        j=2
!!        k=3
!!        array=[10,20,30,40,50,60,70]
!!        call test_one()
!!        call test_two()
!!     ! optional tear-down    perform finalization operations common to all tests within a module
!!     contains
!!
!!     subroutine test_one()
!!     !  register an entry for specified name ("one") in database with status of zero (0)
!!     call unit_check_start('one')
!!
!!     !  if mask test fails, can
!!     !  * produce a SUCCESS: or FAIL: message and stop program
!!     !  * change database status for specified entry to -1 and stop program, else continue
!!     !  * produce a SUCCESS: or FAIL: message and keep going
!!     !  * produce a FAIL: message if test fails but no SUCCESS: message if test passes
!!     call unit_check('one',i.gt.0,msg='I > 0')
!!
!!     ! using ANY(3f) and ALL(3f)
!!     call unit_check('one',all([i,j,k].gt.0),      'testing if everyone greater than zero')
!!     ! display message built of scalars as well
!!     call unit_check('one',all(.not.[i,j,k].eq.4),'for set ',i,j,k,'testing if no one is equal to four')
!!
!!     ! for tests that are hard to reduce to a logical test just call unit_check_bad(3f) if fail
!!     if(i+j+k.lt.1)then
!!        call unit_check_bad('one')
!!     endif
!!
!!     call unit_check_done('one','checks on "one" ended')
!!     end subroutine test_one
!!
!!     subroutine test_two
!!     ! use of all(3f), any(3f), merge(3f) can be useful
!!     ! if you know what these would produce
!!     ! write(*,*)['A','X','X','X','X','B'].eq.'B'      ! this would return an array, the last element having the value T, else F
!!     ! write(*,*)all(['A','X','X','X','X','X'].eq.'X') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','X'].eq.'B') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','B'].eq.'B') ! this would return T
!!     ! write(*,*).not.all(array.lt.100)
!!     ! write(*,*)all(array.lt.100)
!!     ! write(*,*)all([a,b,c,d].eq.[21,51,14,45]) ! compare a list. This would return T
!!     ! write(*,*)all(arr.eq.[21,51,14,45])       ! compare an array. This would return T
!!     ! you know how valuable ANY(3f) and ALL(3f) will be
!!     call unit_check_start('two','check on "two" passed')
!!     call unit_check('two', 1.gt.0 .and. abs(10.10000-10.10001).lt.0.0001,msg='two looks good')
!!     call unit_check_done('two','checks on "two" ended')
!!     end subroutine test_two
!!
!!     end subroutine test_suite_M_demo
!!
!!     end module M_demo
!!
!!     program demo_M_verify
!!     use M_demo,  only: test_suite_M_demo
!!     use M_verify, only: unit_check_command, unit_check_keep_going,unit_check_level
!!     unit_check_command=''
!!     unit_check_keep_going=.true.
!!     unit_check_level=0
!!       call test_suite_M_demo
!!     end program demo_M_verify
!!
!!   Expected output:
!!
!!     unit_check:       one                  SUCCESS:I > 0
!!     unit_check:       one                  SUCCESS:testing if everyone greater than zero
!!     unit_check:       one                  SUCCESS:for set 1 2 3 testing if no one is equal to four
!!     unit_check_done:  one                  PASSED   GOOD:3  BAD:0
!!
!!     unit_check:       two                  SUCCESS:two looks good
!!     unit_check_done:  two                  PASSED   GOOD:1  BAD:0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_verify
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64 !  1           2           4           8
use, intrinsic :: iso_fortran_env, only : real32, real64, real128   !  4           8          10
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT,OUTPUT_UNIT    ! access computing environment
use            :: M_msg,           only : str
implicit none
private

integer,save,public :: io_debug=ERROR_UNIT            ! mutable copy of ERROR_UNIT, but initialized to the unit used for stderr
integer,save,public :: unit_check_lun=ERROR_UNIT      ! mutable copy of ERROR_UNIT, but initialized to the unit used for stderr
logical,save,public :: debug=.false.

logical,save,public :: unit_check_keep_going=.false.  ! logical variable that can be used to turn off program termination on errors.
integer,save,public :: unit_check_level=0             ! a level that can be used to select different debug levels
character(len=4096),public ::  unit_check_command='goodbad'  ! name of command to execute. Defaults to the name "goodbad".
public no_news_is_good_news

integer,parameter,public   :: realtime=kind(0.0d0)            ! type for julian days
integer,parameter,public   :: EXIT_SUCCESS=0
integer,parameter,public   :: EXIT_FAILURE=1
real(kind=realtime),save   :: duration=0.0d0
real(kind=realtime),save   :: duration_all=0.0d0
integer,save               :: clicks=0.0d0
integer,save               :: clicks_all=0.0d0

logical,save ::  STOP_G=.true.                       ! global value indicating whether failed unit checks should stop program or not
integer,save :: IPASSED_G=0                          ! counter of successes initialized by unit_check_start(3f)
integer,save :: IFAILED_G=0                          ! counter of failures  initialized by unit_check_start(3f)
integer,save :: IUNTESTED=0                          ! counter of untested  initialized by unit_check_start(3f)
integer,save :: IPASSED_ALL_G=0                      ! counter of successes initialized at program start
integer,save :: IFAILED_ALL_G=0                      ! counter of failures  initialized at program start
integer,save :: IUNTESTED_ALL=0                      ! counter of untested  initialized at program start
logical,save :: no_news_is_good_news=.false.         ! flag on whether to display SUCCESS: messages

public stderr
public assert
public pdec
public fstop
public unit_check_start
public unit_check
public unit_check_good
public unit_check_bad
public unit_check_done
public unit_check_stop
public unit_check_msg
! COMPARING AND ROUNDING FLOATING POINT VALUES
public accdig         ! compare two real numbers only up to a specified number of digits
public almost         ! function compares two numbers only up to a specified number of digits
public dp_accdig      ! compare two double numbers only up to a specified number of digits
public in_margin      ! check if two reals are approximately equal using a relative margin
public round          ! round val to specified number of significant digits
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_check_msg(3f) - [M_verify] converts up to nine standard scalar values to a message for unit testing
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function unit_check_msg(name,g1,g2g3,g4,g5,g6,g7,g8,g9)
!!
!!     character(len=*),intent(in)  :: name
!!     class(*),intent(in),optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!##DESCRIPTION
!!    unit_check_msg(3f) builds a string from up to nine scalar values and
!!    prints it to the error long.
!!
!!##OPTIONS
!!    name    name of unit being tested
!!    g[1-9]  optional value to print the value of after the message. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_check_msg
!!    use M_verify, only : unit_check_start,unit_check_msg,unit_check_done
!!    implicit none
!!
!!    call unit_check_start('myroutine')
!!    call unit_check_msg('myroutine','HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!    call unit_check_msg('myroutine','real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    call unit_check_msg('myroutine','doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    call unit_check_msg('myroutine','complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!    call unit_check_done('myroutine')
!!
!!    end program demo_unit_check_msg
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_msg(name,g1, g2, g3, g4, g5, g6, g7, g8, g9)
implicit none

! ident_1="@(#)M_verify::unit_check_msg(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: name
class(*),intent(in),optional  :: g1 ,g2 ,g3 ,g4 ,g5
class(*),intent(in),optional  :: g6 ,g7 ,g8 ,g9

   ! write message to standard error
   call stderr('unit_check_msg:   '//atleast(name,20)//' INFO    : '//str(g1,g2,g3,g4,g5,g6,g7,g8,g9))

end subroutine unit_check_msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine stderr(msg, gen0, gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9)
implicit none

! ident_2="@(#)M_verify::stderr(3f): writes a message to standard error using a standard f2003 method"

class(*),intent(in),optional :: msg
class(*),intent(in),optional :: gen0, gen1, gen2, gen3, gen4
class(*),intent(in),optional :: gen5, gen6, gen7, gen8, gen9
integer                      :: ios

   write(error_unit,'(a)',iostat=ios) str(msg, gen0, gen1, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9)
   flush(unit=output_unit,iostat=ios)
   flush(unit=error_unit,iostat=ios)
end subroutine stderr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fstop(3f) - [M_verify] call stop with both a number and a message
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine fstop(ierr,stdout,stderr)
!!
!!     integer,intent(in)                   :: ierr
!!     character(len=*),intent(in),optional :: stdout
!!     character(len=*),intent(in),optional :: stderr
!!##DESCRIPTION
!!    FSTOP(3f) call STOP(3f). What a call to STOP does is very system
!!    dependent, so using an abstraction layer is useful, as it allows just
!!    the fstop() routine to be changed; and STOP does not allow a variable
!!    to be used on the numeric access status (this has changed at f2015).
!!
!!##OPTIONS
!!    ierr    - value in range 0 to 32
!!    stdout  - description to be printed to standard output
!!    stderr  - description to be printed to standard error
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_fstop
!!    use M_verify, only: fstop
!!    implicit none
!!    integer :: int
!!    !*!write(*,*)'Enter stop value'
!!    !*!read(*,*) int
!!    int=25
!!    select case(int)
!!    case(10) ; call fstop(int)
!!    case(20) ; call fstop(int,stderr='error: program will now stop')
!!    case(25) ; call fstop(int,stdout='stdout message',stderr='stderr message')
!!    case(30) ; call fstop(int,stdout='error: program will now stop')
!!    case default
!!               call fstop(int)
!!    endselect
!!
!!    end program demo_fstop
!!
!!   Results:
!!
!!##SEE ALSO
!!   Look for common extensions, such as abort(3f), backtrace(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine fstop(ierr,stdout,stderr)

! ident_3="@(#)M_verify::fstop(3f): calls 'STOP VALUE' passing in a value (1-32), with optional message"

integer,intent(in)                   :: ierr
character(len=*),optional,intent(in) :: stdout
character(len=*),optional,intent(in) :: stderr
character(len=132)                   :: message
! The standard states:
!   If the stop-code is an integer, it is recommended that the value also be used as the process exit status, if the
!   processor supports that concept. If the integer stop-code is used as the process exit status, the processor
!   might be able to interpret only values within a limited range, or only a limited portion of the integer value
!   (for example, only the least-significant 8 bits).

!   If the stop-code is of type character or does not appear, or if an END PROGRAM statement is executed,
!   it is recommended that the value zero be supplied as the process exit status, if the processor supports that
!   concept.
!   A STOP statement or ALL STOP statement shall not be executed during execution of an input/output statement.
!
! Conforming variants I have encountered include
!    o printing a message such as 'STOP nnn' when the integer value is called
!    o having a limit on the length of the message string passed
!    o prefixing the message with the string 'STOP '
!    o different ranges on allowed integer values, and/or not having a one-to-one correspondence between the argument
!      value and what the system is given (usually encountered with large values, which are masked or run thru modulo math, ...)
!    o whether messages appear on stdout or stderr.
!    o no value being returned to the system at all.
!
!  So it is best to test (csh/tcsh sets $status, sh/ksh/bash/... sets $?) to verify what exit codes are supported.
!  What happens with negative values, values above 256; how long of a message is supported? Are messages line-terminated?
!
!  And for some reason STOP only takes constant values. I sometimes want to be able to pass a variable value.
!  Only allowing constants would have the advantage of letting the compiler detect values invalid for a particular system,
!  but I sometimes want to return variables.
!
!  So, using STOP with an argument is not as straight-forward as one might guess, especially if you do not want a message
!  to appear when using integer values
!
!  In practice the C exit(int signal) routine seems to work successfully when called from Fortran but I consider it risky
!  as it seems reasonable to assume Fortran cleanup operations such as removing scratch files and closing and flushing Fortran
!  files may not be properly performed. So it is tempting to call the C function, especially on systems where C returns a
!  value to the system and Fortran does not, but I do not recommend it.
!
!  Note that the C function "exit(int signal)" not only works more consistently but that the global values EXIT_SUCCESS and
!  EXIT_FAILURE are defined for portability, and that the signal value can be a variable instead of a constant.
!
!  If the system supports calls to produce a traceback on demand, that is a useful option to add to this procedure.
!-----------------------------------------------------------------------------------------------------------------------------------
!STOP       'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!&aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab'
!-----------------------------------------------------------------------------------------------------------------------------------
if(present(stderr))then       ! write message to stderr, assuming string length is allowed
   if(stderr.ne.'')then
      write(error_unit,'(a)')trim(stderr)
   endif
!f2015!   select case(ierr)             ! have executable return an exit status to the system (IF SUPPORTED)
!f2015!      case(0); allstop 0
!f2015!      case(1); allstop 1
!f2015!      case(2); allstop 2
!f2015!      case(3); allstop 3
!f2015!      case(4); allstop 4
!f2015!      case(5); allstop 5
!f2015!      case(6); allstop 6
!f2015!      case(7); allstop 7
!f2015!      case(8); allstop 8
!f2015!      case(9); allstop 8
!f2015!      case(10); allstop 10
!f2015!      case(11); allstop 11
!f2015!      case(12); allstop 12
!f2015!      case(13); allstop 13
!f2015!      case(14); allstop 14
!f2015!      case(15); allstop 15
!f2015!      case(16); allstop 16
!f2015!      case(17); allstop 17
!f2015!      case(18); allstop 18
!f2015!      case(19); allstop 19
!f2015!      case(20); allstop 20
!f2015!      case(21); allstop 21
!f2015!      case(22); allstop 22
!f2015!      case(23); allstop 23
!f2015!      case(24); allstop 24
!f2015!      case(25); allstop 25
!f2015!      case(26); allstop 26
!f2015!      case(27); allstop 27
!f2015!      case(28); allstop 28
!f2015!      case(29); allstop 29
!f2015!      case(30); allstop 30
!f2015!      case(31); allstop 31
!f2015!      case(32); allstop 32
!f2015!   case default
!f2015!      write(message,'(a,i0,a)')'*fstop*: stop value of ',ierr,' returning 1 to system'
!f2015!      write(error_unit,'(a)')trim(message) ! write message to standard error
!f2015!      allstop 1
!f2015!   end select
endif
if(present(stdout))then       ! write message to stdout, assuming string length is allowed
   if(stdout.ne.'')then
      write(*,'(a)')trim(stdout)
   endif
endif
select case(ierr)             ! have executable return an exit status to the system (IF SUPPORTED)
   case(0); stop 0
   case(1); stop 1
   case(2); stop 2
   case(3); stop 3
   case(4); stop 4
   case(5); stop 5
   case(6); stop 6
   case(7); stop 7
   case(8); stop 8
   case(9); stop 8
   case(10); stop 10
   case(11); stop 11
   case(12); stop 12
   case(13); stop 13
   case(14); stop 14
   case(15); stop 15
   case(16); stop 16
   case(17); stop 17
   case(18); stop 18
   case(19); stop 19
   case(20); stop 20
   case(21); stop 21
   case(22); stop 22
   case(23); stop 23
   case(24); stop 24
   case(25); stop 25
   case(26); stop 26
   case(27); stop 27
   case(28); stop 28
   case(29); stop 29
   case(30); stop 30
   case(31); stop 31
   case(32); stop 32
case default
   write(message,'(a,i0,a)')'*fstop*: stop value of ',ierr,' returning 1 to system'
   write(error_unit,'(a)')trim(message) ! write message to standard error
   stop 1
end select
end subroutine fstop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check(3f) - [M_verify] if logical expression is false, call command "goodbad NAME bad" and stop program by default
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check(name,expression,msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)
!!
!!     character(len=*),intent(in) :: name
!!     logical,intent(in) :: expression
!!     class(*),intent(in),optional :: msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9
!!
!!##DESCRIPTION
!!    unit_check(3f) tests the expression and if it is false, calls the
!!    shell command
!!
!!         goodbad NAME bad
!!
!!    and stops the program.
!!##OPTIONS
!!     NAME             the unit test name passed on to the goodbad(1)
!!                      command
!!     EXPRESSION       the logical expression to evaluate
!!     MSG,MSG1...MSG9  optional message to display when performing test,
!!                      composed of any scalar intrinsics of type INTEGER,
!!                      REAL, DOUBLEPRECISION, COMPLEX, LOGICAL, or
!!                      CHARACTER, with a space placed between each value.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_check
!!    use M_verify, only: unit_check
!!    use M_verify, only: unit_check_start
!!    use M_verify, only: unit_check_done
!!    use M_verify,  only: almost
!!
!!    !!use M_verify, only: unit_check_keep_going         ! default is unit_check_keep_going=.false.
!!    !!use M_verify, only: debug              ! default is .false.
!!    !!use M_verify, only: unit_check_command ! default is unit_check_command='goodbad'
!!
!!    implicit none
!!    integer :: i
!!    integer :: x
!!    integer,allocatable :: arr(:)
!!    real,allocatable :: arr1(:)
!!    real,allocatable :: arr2(:)
!!
!!       !!unit_check_command=''
!!       x=10
!!       arr1=[1.0,10.0,100.0]
!!       arr2=[1.0001,10.001,100.01]
!!       call unit_check_start('myroutine')
!!
!!       call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!       call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!       do i=1,size(arr1)
!!          call unit_check('myroutine', almost(arr1(i),arr2(i),3.9,verbose=.true.) )
!!       enddo
!!
!!       arr=[10,20,30]
!!       call unit_check('myroutine', .not.any(arr.lt.0) ,'test if any negative values in array ARR')
!!       call unit_check('myroutine', all(arr.lt.100) ,'test if all values less than 100 in array ARR')
!!
!!       call unit_check_done('myroutine',msg='checks on "myroutine" all passed')
!!
!!    end program demo_unit_check
!!
!!   Sample output (varies with what goodbad(1) command is used):
!!
!!    unit_check:      myroutine        SUCCESS:test if big enough
!!    unit_check:      myroutine        SUCCESS:test if small enough
!!    unit_check:      myroutine        SUCCESS:test if any negative values in array ARR
!!    unit_check:      myroutine        SUCCESS:test if all values less than 100 in array ARR
!!     *almost* for values 1.00000000 1.00010002 agreement of 3.99997139 digits out of requested 3.90000010
!!     *almost* for values 10.0000000 10.0010004 agreement of 3.99986792 digits out of requested 3.90000010
!!     *almost* for values 100.000000 100.010002 agreement of 3.99995065 digits out of requested 3.90000010
!!    unit_check_good: myroutine        PASSED:checks on "myroutine" all passed
!!
!!
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check(name,logical_expression,msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)

! ident_4="@(#)M_verify::unit_check(3f):if .not.expression call 'goodbad NAME bad' & stop program"

character(len=*),intent(in)          :: name
logical,intent(in)                   :: logical_expression
class(*),intent(in),optional         :: msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9
character(len=:),allocatable         :: msg_local
!-----------------------------------------------------------------------------------------------------------------------------------
msg_local=str(msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.logical_expression)then
      call stderr('unit_check:       '//atleast(name,20)//' FAILURE : '//trim(msg_local))  ! write message to standard error
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' bad')
      endif
      if(.not.unit_check_keep_going) then
         call stderr('unit_check:         STOPPING PROGRAM ON FAILED TEST OF '//trim(name))    ! write to standard error
         call fstop(1)
      endif
      IFAILED_G=IFAILED_G+1
      IFAILED_ALL_G=IFAILED_ALL_G+1
   else
      if(.not.no_news_is_good_news)then
         call stderr('unit_check:       '//atleast(name,20)//' SUCCESS : '//trim(msg_local))  ! write message to standard error
      endif
      IPASSED_G=IPASSED_G+1
      IPASSED_ALL_G=IPASSED_ALL_G+1
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_check_start(3f) - [M_verify] call command "goodbad NAME start" and optionally set options
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_start(name,options,msg)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: options
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    unit_check_start(3f) is an initialization command that by default
!!    calls the shell command
!!
!!       goodbad NAME start [options]
!!
!!    The command can be changed by setting the environment variable
!!    UNIT_CHECK_COMMAND or the global module variable UNIT_CHECK_COMMAND.
!!    The environment variable overrides the global module variable.
!!
!!    By default if a unit_check(3f) logical expression is false or the
!!    unit_check_bad(3f) procedure is called the program will be stopped.
!!
!!    This has the same effect as setting the environment variable
!!    M_verify_STOP to "FALSE" or the global module variable
!!    UNIT_CHECK_KEEP_GOING to .FALSE. . Set the value to .true. and the
!!    program will continue even when tests fail.
!!
!!##OPTIONS
!!       NAME  name of the shell command to execute. If blank, no command
!!             is executed.
!!    OPTIONS  pass additional options to the shell command
!!
!!    MSG      print message
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_start
!!     use M_verify, only: unit_check_start
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_done
!!
!!     implicit none
!!     integer :: ival
!!     call unit_check_start('myroutine')
!!     ! the goodbad(1) command called here takes many options
!!     ! used to build an SQLite3 entry
!!     call unit_check_start('myroutine_long',' &
!!       & -section        3                    &
!!       & -library        libGPF               &
!!       & -filename       `pwd`/M_verify.FF     &
!!       & -documentation  y                    &
!!       & -prep           y                    &
!!       & -ccall          n                    &
!!       & -archive        GPF.a                &
!!       & ')
!!
!!     ival=10
!!     call unit_check('myroutine', ival.gt.3 ,   msg='test if big enough')
!!     call unit_check('myroutine', ival.lt.100 , msg='test if small enough')
!!
!!     call unit_check_done('myroutine',msg='completed checks of "myroutine"')
!!
!!     end program demo_unit_check_start
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_start(name,options,msg)

! ident_5="@(#)M_verify::unit_check_start(3f): call 'goodbad NAME start'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: options
character(len=*),intent(in),optional :: msg
character(len=4096)                  :: var
logical,save                         :: called=.false.
!-----------------------------------------------------------------------------------------------------------------------------------
   call get_environment_variable('UNIT_CHECK_COMMAND',var)
   if(var.ne.'')unit_check_command=var
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(options))then
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' start '//trim(options))
      endif
   else
      if(unit_check_command.ne.'')then
         call execute_command_line(unit_check_command//' '//trim(name)//' start')
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call system_clock(clicks)
   duration=julian()
   if(.not.called)then
      call system_clock(clicks_all)
      duration_all=julian()
      called=.true.
   endif
   if(present(msg))then
     if(msg.ne.'')then
        call stderr('unit_check_start: '//atleast(name,20)//' START   : '//trim(msg)) ! write message to standard error
     endif
   endif
   call get_environment_variable('M_verify_STOP',var)
   select case(var)
   case('FALSE','false','1','no','NO')
         unit_check_keep_going=.false.
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   IPASSED_G=0
   IFAILED_G=0
   IUNTESTED=0
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_start
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_stop(3f) - [M_verify] call command "goodbad NAME good" or "goodbad NAME bad" depending on whether failures were found
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_stop(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!     give a tally of all calls to unit_check(3f)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_stop
!!     use M_verify, only: unit_check_start, unit_check_done
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_good, unit_check_stop, unit_check_bad
!!     use M_verify, only: unit_check_command, unit_check_keep_going, unit_check_level
!!
!!     implicit none
!!     integer :: x
!!
!!     unit_check_command=''
!!     unit_check_keep_going=.true.
!!     unit_check_level=0
!!
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     if(x.ne.0)then
!!        call unit_check_bad  ('myroutine',msg='x.ne.0' )
!!     endif
!!     call unit_check_done  ('myroutine',msg='checks on "myroutine"' )
!!
!!     call unit_check_stop()
!!     end program demo_unit_check_stop
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_stop(msg)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64

! ident_6="@(#)M_verify::unit_check_stop(3f):  stop program with report on calls to unit_check(3f)"

character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=4096)                  :: out
character(len=:),allocatable         :: PF
integer(kind=int64)                  :: milliseconds
integer                              :: clicks_now
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   call system_clock(clicks_now)
   milliseconds=(julian()-duration_all)*1000
   milliseconds=clicks_now-clicks_all
   PF=merge('PASSED  :','FAILED  :',ifailed_all_G.eq.0)
   if(PF.eq.'PASSED  :'.and.ipassed_all_G.eq.0)then
      PF='UNTESTED:'
   endif
   write(out,'("unit_check_stop:  ", &
       & "TALLY:              ",          &
       & 1x,a,                            &
       & " DURATION:",i14.14,             &
       & " GOOD:",i0,                     &
       & 1x," BAD:",i0                    &
       & )')                              &
       & PF,                              &
       & milliseconds,                    &
       & IPASSED_ALL_G,                   &
       & IFAILED_ALL_G
   if(present(msg))then
      call stderr(trim(out)//': '//trim(msg))
   else
      call stderr(trim(out))
   endif
   if(IFAILED_ALL_G.eq.0)then
      stop EXIT_SUCCESS
   else
      stop EXIT_FAILURE
   endif
end subroutine unit_check_stop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_done(3f) - [M_verify] call command "goodbad NAME good" or "goodbad NAME bad" depending on whether failures were found
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_done(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    If there have been no failures the shell command
!!
!!         goodbad NAME good [opts]
!!
!!    is executed, else the command
!!
!!         goodbad NAME bad [opts]
!!
!!    is executed and by default stops the program if their have been
!!    any failures.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_done
!!     use M_verify, only: unit_check_start
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_good, unit_check_done, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     if(x.ne.0)then
!!        call unit_check_done ('myroutine',msg='checks on "myroutine"' ) ! program execution stopped
!!     endif
!!
!!     end program demo_unit_check_done
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_done(name,opts,msg)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64

! ident_7="@(#)M_verify::unit_check_done(3f): call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
character(len=4096)                  :: out
character(len=9)                     :: pf
integer(kind=int64)                  :: milliseconds
integer                              :: clicks_now
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(unit_check_command.ne.'')then                           ! if system command name is not blank call system command
      if(ifailed_g.eq.0)then
         call execute_command_line(unit_check_command//' '//trim(name)//' bad '//trim(opts))
         if(.not.unit_check_keep_going) call fstop(1)            ! stop program depending on mode
      else
         call execute_command_line(unit_check_command//' '//trim(name)//' good '//trim(opts))
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   PF=merge('PASSED  :','FAILED  :',ifailed_G.eq.0)
   if(PF.eq.'PASSED  :'.and.ipassed_G.eq.0)then
      PF='UNTESTED:'
   endif
   if(duration.ne.0.0d0)then
      call system_clock(clicks_now)
      milliseconds=(julian()-duration)*1000
      milliseconds=clicks_now-clicks
      write(out,'("unit_check_done:  ",a, &
       & 1x,a,                            &
       & " DURATION:",i14.14,             &
       & " GOOD:",i0,                     &
       & 1x," BAD:",i0                    &
       & )')                              &
       & atleast(name,20),                &
       & PF,                              &
       & milliseconds,                    &
       & IPASSED_G,                       &
       & IFAILED_G
   else
      write(out,'("unit_check_done:  ",a,1x,a," GOOD:",i0,1x," BAD:",i0)') atleast(name,20),PF,IPASSED_G,IFAILED_G
   endif
   if(present(msg))then
      call stderr(trim(out)//': '//trim(msg))
   else
      call stderr(trim(out))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   IPASSED_G=0
   IFAILED_G=0
   IUNTESTED=0
   duration=0.0d0
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_done
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_bad(3f) - [M_verify] call command "goodbad NAME bad" and stop program
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_bad(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    unit_check_bad(3f) calls the shell command
!!
!!         goodbad NAME bad [opts]
!!
!!    and stops the program. It is just a shortcut for calling
!!         call unit_check(name,.false.)
!!         call unit_check_done(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_bad
!!     use M_verify, only: unit_check_start
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     if(x.ne.0)then
!!        call unit_check_bad ('myroutine',msg='checks on "myroutine" failed') ! program execution stopped
!!     endif
!!
!!     end program demo_unit_check_bad
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine unit_check_bad(name,opts,msg)

! ident_8="@(#)M_verify::unit_check_bad(3f): call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
   call unit_check(name,.false.)
   call unit_check_done(name,opts_local,msg_local)
end subroutine unit_check_bad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_good(3f) - [M_verify] call command "goodbad NAME good"
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_good(name,opts,msg)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    A shortcut for
!!
!!       call unit_check(name,.true.)
!!       call unit_check_done(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_good
!!     use M_verify, only: unit_check_start, unit_check_done
!!     use M_verify, only: unit_check
!!     use M_verify, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x.gt.3 ,'test if big enough')
!!     call unit_check('myroutine', x.lt.100 ,'test if small enough')
!!
!!     call unit_check_good('myroutine',msg='checks on "myroutine" ')
!!
!!     end program demo_unit_check_good
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_good(name,opts,msg)

! ident_9="@(#)M_verify::unit_check_good(3f): call 'goodbad NAME good'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif
   call unit_check(name,.true.,msg=msg_local)
   call unit_check_done(name,opts_local)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine unit_check_good
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      pdec(3f) - [M_verify] write out string with ASCII decimal equivalent vertically under it
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Usage:
!!
!!     subroutine pdec(string)
!!     character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!
!!    Given a string to print, PDEC() writes out the ASCII Decimal equivalent
!!    of the string directly underneath it. This can help you to locate
!!    unprintable characters or non-standard white-space such as a backspace
!!    character or tab character in input strings that your program could
!!    not interpret. On output, non-printable characters are replaced with
!!    a space, and trailing spaces are ignored.
!!
!!    You read the numbers vertically.
!!
!!    1. ignore trailing spaces
!!    2. print the character if it has an ADE of 32 on up
!!    3. print a space if it has an ADE of less than 32
!!    4. underneath each character print the ADE value vertically
!!    5. strings are assumed under 32767 characters in length.
!!       Format integer constants > 32767 are not supported on HP-UX
!!       when newer compilers are available use unlimited
!!
!!##EXAMPLES
!!
!!
!!    Sample program:
!!
!!       program demo_pdec
!!       use M_verify, only : pdec
!!       call pdec(' ABCDEFG abcdefg    ')
!!       end program demo_pdec
!!
!!    would produce (notice trailing space is trimmed):
!!
!!      > ABCDEFG abcdefg
!!      >0000000000001111
!!      >3666667739990000
!!      >2567890127890123
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine pdec(string)

! ident_10="@(#)M_verify::pdec(3f): write ASCII Decimal Equivalent (ADE) numbers vertically beneath string"

character(len=*),intent(in) :: string   ! the string to print
integer                     :: ilen     ! number of characters in string to print
integer                     :: i        ! counter used to step thru string
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(string(:len(string)))  ! get trimmed length of input string

   write(*,101)(char(max(32,ichar(string(i:i)))),i=1,ilen) ! replace lower unprintable characters with spaces

   ! print ADE value of character underneath it
   write(*,202)     (ichar(string(i:i))/100,    i=1,ilen)
   write(*,202)(mod( ichar(string(i:i)),100)/10,i=1,ilen)
   write(*,202)(mod((ichar(string(i:i))),10),   i=1,ilen)
101   format(32767a1:)  ! format for printing string characters
202   format(32767i1:)  ! format for printing ADE values
end subroutine pdec
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function atleast(line,length) result(strout)

! ident_11="@(#)M_verify::atleast(3fp): return string padded to at least specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=max(length,len(trim(line)))) ::  strout
   strout=line
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    assert(3f) - [M_verify] print filename, linenumber, and message to stderr and stop program
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function assert(file,linenum,expr,g1,g2g3,g4,g5,g6,g7,g8,g9)
!!
!!     character(len=*),intent(in)  :: file
!!     character(len=*),intent(in)  :: linenum
!!     logical,intent(in)           :: expr
!!     class(*),intent(in),optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!##DESCRIPTION
!!    assert(3f) prints strings to stderr and then stops program with exit
!!    code 1 It labels the first string as the filename, the next integer
!!    parameter as the linenumber, and then up to nine scalar values.
!!
!!    It is primarily intended for use by the prep(1) preprocessor $ASSERT
!!    directive
!!
!!##OPTIONS
!!
!!    filename   a string assumed to be the current filename when compiling
!!    linenum    assumed to be the line number of the source code the ASSERT(3f)
!!               procedure was called at.
!!    expr       logical value
!!    g[1-9]  optional value(s) to print as a message before stopping. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_assert
!!    use M_verify, only : assert
!!    implicit none
!!    real :: a, toobig=1024
!!    a=2000
!!    call assert('myroutine', 101, a.gt.toobig, 'The value is too large', a, '.gt.', toobig)
!!    end program demo_assert
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine assert(filename,linen,expr,g1, g2, g3, g4, g5, g6, g7, g8, g9)
implicit none

! ident_12="@(#)M_verify::assert(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: filename
integer,intent(in)            :: linen
logical,intent(in)            :: expr
class(*),intent(in),optional  :: g1 ,g2 ,g3 ,g4 ,g5
class(*),intent(in),optional  :: g6 ,g7 ,g8 ,g9

   ! write message to standard error
   if(.not.expr)then
      call stderr('ERROR:filename:',filename,':line number:',linen,':',str(g1,g2,g3,g4,g5,g6,g7,g8,g9))
      stop 1
   endif

end subroutine assert
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function julian()
! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19

! ident_13="@(#)M_verify::julian(3f): Converts proleptic Gregorian DAT date-time array to Julian Date"

real(kind=realtime)              :: julian   ! Julian Date (non-negative, but may be non-integer)
integer                          :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
integer                          :: year, month, day, utc, hour, minute
real(kind=realtime)              :: second
integer                          :: A, Y, M, JDN

   call date_and_time(values=dat)
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0d0    ! Second   ! correction for time zone and milliseconds

!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or February, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for February
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + second/86400.0d0
end function julian
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    almost(3f) - [M_verify] return true or false if two numbers agree up to specified number of digits
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function almost(x,y,digits)
!!
!!     class(*),intent(in)         :: x,y
!!     class(*),intent(in)         :: rdigits
!!     logical,intent(in),optional :: verbose
!!     logical                     :: almost
!!
!!##DESCRIPTION
!!    Returns true or false depending on whether the two numbers given agree
!!    to within the specified number of digits as calculated by ACCDIG(3f).
!!##OPTIONS
!!    x,y      expected and calculated values to be compared. May be of
!!             type REAL, INTEGER, or DOUBLEPRECISION.
!!    rdigits  real number representing number of digits of precision
!!             to compare
!!    verbose  optional value that specifies to print the results of the
!!             comparison when set to .TRUE..
!!##RETURNS
!!    almost   TRUE if the input values compare up to the specified number
!!             of values
!!##EXAMPLE
!!
!!   sample:
!!
!!    program demo_almost
!!    use M_verify, only : almost
!!    implicit none
!!    real    :: x, y
!!    logical :: z
!!    integer :: i
!!    x=1.2345678
!!    y=1.2300000
!!    do i=1,8
!!       z=almost(x,y,real(i),verbose=.true.)
!!       write(*,*)i,z
!!    enddo
!!    end program demo_almost
!!
!!   output:
!!
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 1.0
!!            1   T
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 2.0
!!            2   T
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 3.0
!!            3   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 4.0
!!            4   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 5.0
!!            5   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 6.0
!!            6   F
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 7.0
!!            7   F
!!     *accdig* significant digit request too high= 8.00000000
!!     *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 digits out of requested 8.0
!!            8   F
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function almost(x,y,digits,verbose)
use M_journal,  only : journal

! ident_14="@(#)M_verify::almost(3f): function to compare two real numbers only up to a specified number of digits by calling DP_ACCDIG(3f)"

class(*),intent(in)         :: x,y
class(*),intent(in)         :: digits
logical,intent(in),optional :: verbose
logical                     :: almost

logical                     :: verbose_local
real                        :: acurcy
real                        :: digits_local
integer                     :: ind

   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif

   digits_local=anyscalar_to_real128(digits)
   acurcy=0.0
   select type(x)
   type is(real)
      select type(y)
      type is(real)
         call accdig(x,y,digits_local,acurcy,ind)
         if(verbose_local)then
            call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
         endif
      class default
         call dp_accdig(x,y,digits_local,acurcy,ind)
         if(verbose_local)then
            call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
         endif
      end select
   class default
      call dp_accdig(x,y,digits,acurcy,ind)
      if(verbose_local)then
         call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
      endif
   end select

   if(ind.eq.0)then
      almost=.true.
   else
      almost=.false.
   endif

end function almost
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      accdig(3f) - [M_verify] compare two real numbers only up to a specified number of digits
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine accdig(x,y,digio,acurcy,ind)
!!
!!        real,intent(in)     :: X
!!        real,intent(in)     :: Y
!!        real,intent(in)     :: DIGI0
!!        real,intent(out)    :: acurcy
!!        integer,intent(out) :: ind
!!
!!##DESCRIPTION
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    The values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisfied.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!            ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!            ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!            ACURCY=8                 if X=Y
!!
!!            ACURCY is never less than -8 or greater than 8
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare then  to 1.2345678 ...
!!
!!       ================================================
!!       A number     |    ACURCY       |   ACURCY
!!                    |    1.2345678=Y  |   1.2345678=X
!!       ================================================
!!        1.234680    |    3.7900571    |   3.7901275
!!        1.2345378   |    4.6144510    |   4.6144404
!!        2.2234568   |    0.096367393  |   0.35188114
!!        1.2345678   |    8.0000000    |   8.0000000
!!        1.2345679   |    7.0732967    |   7.0731968
!!       -1.2345678   |   -0.30103000   |  -0.30103000
!!       76.234567    |   -1.7835463    |   0.0070906729
!!        2.4691356   |    0.0          |   0.3010300
!!        0.0         |    0.0          |  -0.91514942.
!!
!!    Due to the typical limits of the log function, the number of
!!    significant digits in the result is best considered to be three.
!!
!!    Notice that 1.2345678=Y produces different values than 1.2345678=X
!!
!!    A negative result indicates the two values being compared either do
!!    not agree in the first digit or they differ with respect to sign. An
!!    example of two numbers which do not agree in their leading digit (and
!!    actually differ in order of magnitude) is given above by X=76.234567
!!    and Y=1.2345678; the accuracy reported is -1.7835463. An example of
!!    two numbers which do not agree in sign in X=-1.2345678 and Y=1.2345678;
!!    here the accuracy reported is -0.30103000.
!!
!!##EXAMPLE
!!
!!
!!   Example program:
!!
!!    program demo_accdig ! fortran 90 example
!!    use M_verify, only : accdig
!!    implicit none
!!    integer :: digi
!!    integer :: i10, i20, i30
!!    integer :: ind, ind1, ind2
!!    real    :: acurcy, acurcy1, acurcy2
!!    real    :: a, b
!!    real    :: vals(9)
!!    data vals/ &
!!      &1.234680,   1.2345378,  2.2234568, 1.2345678, &
!!      &1.2345679, -1.2345678, 76.234567,  2.4691356, &
!!      &0.0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0
!!          b=a+1.0/(10**i10)
!!          call accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0
!!          b=a+1.0/(10**i20)
!!          call accdig(a,b,real(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call accdig(1.2345678,vals(i30),8.0,acurcy1,ind1)
!!          call accdig(vals(i30),1.2345678,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_accdig
!!
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. accdig V 7.00  2/14/90. **
!!       David Hogben,
!!       Statistical Engineering Division,
!!       Center for Computing and Applied Mathematics,
!!       A337 Administration Building,
!!       National Institute of Standards and Technology,
!!       Gaithersburg, MD 20899
!!                      TELEPHONE 301-975-2845
!!           ORIGINAL VERSION -  October, 1969.
!!            CURRENT VERSION - February, 1990.
!!            JSU     VERSION - February, 1991.
!!
!!##DEPENDENCIES
!!    o M_journal(),log10(), abs(1)
!!
!!##AUTHOR
!!    David Hogben, John S. Urban
!!
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE accdig(X,Y,digi0,ACURCY,IND)
use M_journal, only : journal
implicit none

! ident_15="@(#)M_verify::accdig(3f): compare two real numbers only up to a specified number of digits"

!     INPUT ...
real,intent(in) :: x           ! First  of two real numbers to be compared.
real,intent(in) :: y           ! Second of two real numbers to be compared.
real,intent(in) :: digi0       ! Number of digits to be satisfied in relative tolerance.
!     OUTPUT ...
integer,intent(out) :: ind     ! = 0, If tolerance is     satisfied.
! = 1, If tolerance is not satisfied.
real,intent(out)    :: acurcy  ! = - LOG10(ABS((X-Y)/Y)))

real     :: diff
real     :: digi
integer  :: ireal_significant_digits
!-----------------------------------------------------------------------------------------------------------------------------------
   ireal_significant_digits=int(log10(2.**digits(0.0))) ! maximum number of significant digits in a real number.
   digi=digi0
   if(digi.le.0)then
      call journal('sc','*accdig* bad number of significant digits=',digi)
      digi=ireal_significant_digits
   elseif(digi .gt. ireal_significant_digits)then
      call journal('sc','*accdig* significant digit request too high=',digi)
      digi=min(digi,real(ireal_significant_digits))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   diff = x - y
   if(diff .eq. 0.0) then
      acurcy = digi
   elseif(y .eq. 0.0) then
      acurcy = - log10(abs(x))
   else
      acurcy = - log10(abs(diff)) + log10(abs(y))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(acurcy .lt. digi ) then
      ind = 1
   else
      ind = 0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
END SUBROUTINE accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      dp_accdig(3f) - [M_verify] compare two numbers only up to a specified number of digits
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine dp_accdig(x,y,digio,acurcy,ind)
!!
!!        class(*),intent(in)  :: X
!!        class(*),intent(in)  :: Y
!!        class(*),intent(in)  :: DIGI0
!!        real,intent(out)     :: acurcy
!!        integer,intent(out)  :: ind
!!
!!##DESCRIPTION
!!
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call dp_accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    The values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisfied.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!         ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!         ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!         ACURCY=8                 if X=Y
!!
!!         ACURCY is never less than -8 or greater than 8 for REAL values
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare then  to 1.2345678 ...
!!
!!       ================================================
!!       A number     |    ACURCY       |   ACURCY
!!                    |    1.2345678=Y  |   1.2345678=X
!!       ================================================
!!        1.234680    |    3.7900571    |   3.7901275
!!        1.2345378   |    4.6144510    |   4.6144404
!!        2.2234568   |    0.096367393  |   0.35188114
!!        1.2345678   |    8.0000000    |   8.0000000
!!        1.2345679   |    7.0732967    |   7.0731968
!!       -1.2345678   |   -0.30103000   |  -0.30103000
!!       76.234567    |   -1.7835463    |   0.0070906729
!!        2.4691356   |    0.0          |   0.3010300
!!        0.0         |    0.0          |  -0.91514942.
!!
!!    Due to the typical limits of the log function, the number of
!!    significant digits in the result is best considered to be three.
!!
!!    Notice that 1.2345678=Y produces different values than 1.2345678=X
!!
!!    A negative result indicates the two values being compared either do
!!    not agree in the first digit or they differ with respect to sign. An
!!    example of two numbers which do not agree in their leading digit (and
!!    actually differ in order of magnitude) is given above by X=76.234567
!!    and Y=1.2345678; the accuracy reported is -1.7835463. An example of
!!    two numbers which do not agree in sign in X=-1.2345678 and Y=1.2345678;
!!    here the accuracy reported is -0.30103000.
!!
!!##EXAMPLE
!!
!!
!!   Example program:
!!
!!    program demo_dp_accdig ! fortran 90 example
!!    use M_verify, only : dp_accdig
!!    implicit none
!!    integer         :: digi
!!    doubleprecision :: a, b
!!    integer         :: i10, i20, i30
!!    integer         :: ind, ind1, ind2
!!    real            :: acurcy, acurcy1, acurcy2
!!    doubleprecision :: vals(9)
!!    data vals/ &
!!      &1.234680d0,   1.2345378d0,  2.2234568d0, 1.2345678d0, &
!!      &1.2345679d0, -1.2345678d0, 76.234567d0,  2.4691356d0, &
!!      &0.0d0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0d0
!!          b=a+1.0d0/(10**i10)
!!          call dp_accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0d0
!!          b=a+1.0d0/(10**i20)
!!          call dp_accdig(a,b,dble(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call dp_accdig(1.2345678d0,vals(i30),8.0,acurcy1,ind1)
!!          call dp_accdig(vals(i30),1.2345678d0,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_dp_accdig
!!
!!##NOTES
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. dp_accdig V 7.00  2/14/90. **
!!       David Hogben,
!!       Statistical Engineering Division,
!!       Center for Computing and Applied Mathematics,
!!       A337 Administration Building,
!!       National Institute of Standards and Technology,
!!       Gaithersburg, MD 20899
!!                      TELEPHONE 301-975-2845
!!           ORIGINAL VERSION -  October, 1969.
!!            CURRENT VERSION - February, 1990.
!!            JSU     VERSION - February, 1991.
!!
!!##DEPENDENCIES
!!         o M_journal(), log10(), abs(1)
!!
!!##AUTHORS
!!      David Hogben, John S. Urban
!!
!!##LICENSE
!!      Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE dp_accdig(x,y,digi0,ACURCY,IND)
use,intrinsic :: iso_fortran_env, only : real128
use M_journal,  only : journal
implicit none

! ident_16="@(#)M_verify::dp_accdig(3f): compare two values only up to a specified number of digits"

!  INPUT ...
class(*),intent(in)  :: x           ! FIRST  OF TWO NUMBERS TO BE COMPARED.
class(*),intent(in)  :: y           ! SECOND OF TWO NUMBERS TO BE COMPARED.
class(*),intent(in)  :: digi0       ! NUMBER OF DIGITS TO BE SATISFIED IN RELATIVE TOLERANCE.

real(kind=real128)   :: x_local
real(kind=real128)   :: y_local

!  OUTPUT ...
integer,intent(out)  :: ind         ! = 0, IF TOLERANCE IS     SATISFIED.
                                              ! = 1, IF TOLERANCE IS NOT SATISFIED.
real,intent(out)     :: acurcy      ! = - LOG10(ABS((x_local-y_local)/y_local)))
real(kind=real128)   :: diff
real(kind=real128)   :: digi
integer              :: idble_significant_digits
!-----------------------------------------------------------------------------------------------------------------------------------
   x_local=anyscalar_to_real128(x)
   y_local=anyscalar_to_real128(y)
   digi=anyscalar_to_real128(digi0)
!-----------------------------------------------------------------------------------------------------------------------------------
   idble_significant_digits=int(log10(2.0_real128**digits(0.0_real128))) ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A REAL128 NUMBER.
   if(digi.le.0)then
      call journal('sc','*dp_accdig* bad number of significant digits=',real(digi,kind=real128))
      digi=idble_significant_digits
   elseif(digi .gt. idble_significant_digits)then
      call journal('sc','*dp_accdig* significant digit request too high=',real(digi,kind=real128))
      digi=min(digi,real(idble_significant_digits,kind=real128))
   endif
   diff = x_local - y_local
   if(diff .eq. 0.0_real128) then
      acurcy = digi
   elseif(y_local .eq. 0.0_real128) then
      acurcy = - log10(abs(x_local))
   else
      acurcy = - log10(abs(diff)) + log10(abs(y_local))
   endif
   if(acurcy .lt. digi ) then
      ind = 1
   else
      ind = 0
   endif
end subroutine dp_accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!   in_margin(3f) - [M_verify] check if two reals are approximately equal using a relative margin
!!
!!##SYNOPSIS
!!
!!     elemental pure function in_margin( expected_value, measured_value, allowed_margin )
!!
!!      real, intent(in)    :: expected_value
!!      real, intent(in)    :: measured_value
!!      real, intent(in)    :: allowed_margin
!!      class(*),intent(in) :: invalue
!!
!!##DESCRIPTION
!!   Compare two values to see if they are relatively equal using the
!!   specified allowed margin. That is, see if VALUE_MEASURED is in
!!   the range VALUE_EXPECTED +- ALLOWED_ERROR where the allowed error
!!   varies with the magnitude of the values, such that the allowed error
!!   is margin * average magnitude of measured and expected).
!!
!!   So the allowed error is smaller when the magnitudes are smaller.
!!
!!##OPTIONS
!!   expected_value   First value
!!   measured_value   Second value
!!   allowed_margin   Allowed relative margin
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_in_margin
!!    use :: M_verify, only : in_margin
!!    implicit none
!!    write(*,*) in_margin(4.00000,3.99999,0.000000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.00000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.0000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.000001)
!!
!!    write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], [3.9,39.9,399.9,3999.9,39999.9] ,0.000001)
!!    write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], [3.9,39.9,399.9,3999.9,39999.9] ,0.00001)
!!
!!    write(*,*) in_margin(4.00000,3.99999,0.00001)
!!    write(*,*) in_margin(4.00000,3.99999,0.0001)
!!    write(*,*) in_margin(4.00000,3.99999,0.001)
!!    write(*,*) in_margin(4.00000,3.99999,0.01)
!!
!!    end program demo_in_margin
!!
!!   Results:
!!
!!     F
!!     F
!!     F
!!     F
!!     F F F F F
!!     F F F F T
!!     T
!!     T
!!     T
!!     T
!===================================================================================================================================
elemental pure function in_margin(expected_value, measured_value, allowed_margin)
implicit none

! ident_17="@(#)M_verify::in_margin(3f): check if two reals are approximately equal using a relative margin"

class(*),intent(in) :: expected_value, measured_value, allowed_margin
logical             :: in_margin

   doubleprecision     :: expected, measured, margin

   expected=anyscalar_to_double(expected_value)
   measured=anyscalar_to_double(measured_value)
   margin=anyscalar_to_double(allowed_margin)

   if ( abs(expected-measured) > 0.50d0 * margin * (abs(expected)+abs(measured)) ) then
      in_margin=.false.  ! values not comparable
   else
      in_margin=.true.   ! values comparable
   endif

end function in_margin
function round(val,idigits0)
implicit none

! ident_18="@(#)M_verify::round(3f): round val to specified number of significant digits"

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
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function anyscalar_to_real128(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_19="@(#)M_verify::anyscalar_to_real128(3f): convert integer or real parameter of any kind to real128"

class(*),intent(in)          :: valuein
real(kind=real128)           :: d_out
character(len=3)             :: readable
   select type(valuein)
   type is (integer(kind=int8));   d_out=real(valuein,kind=real128)
   type is (integer(kind=int16));  d_out=real(valuein,kind=real128)
   type is (integer(kind=int32));  d_out=real(valuein,kind=real128)
   type is (integer(kind=int64));  d_out=real(valuein,kind=real128)
   type is (real(kind=real32));    d_out=real(valuein,kind=real128)
   type is (real(kind=real64));    d_out=real(valuein,kind=real128)
   Type is (real(kind=real128));   d_out=valuein
   type is (logical);              d_out=merge(0.0_real128,1.0_real128,valuein)
   type is (character(len=*));     read(valuein,*) d_out
   class default
    !!d_out=huge(0.0_real128)
    readable='NaN'
    read(readable,*)d_out
    !!stop '*M_verify::anyscalar_to_real128: unknown type'
   end select
end function anyscalar_to_real128
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function anyscalar_to_double(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_20="@(#)M_verify::anyscalar_to_double(3f): convert integer or real parameter of any kind to doubleprecision"

class(*),intent(in)       :: valuein
doubleprecision           :: d_out
doubleprecision,parameter :: big=huge(0.0d0)
   select type(valuein)
   type is (integer(kind=int8));   d_out=dble(valuein)
   type is (integer(kind=int16));  d_out=dble(valuein)
   type is (integer(kind=int32));  d_out=dble(valuein)
   type is (integer(kind=int64));  d_out=dble(valuein)
   type is (real(kind=real32));    d_out=dble(valuein)
   type is (real(kind=real64));    d_out=dble(valuein)
   Type is (real(kind=real128))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
      !!endif
      d_out=dble(valuein)
   type is (logical);              d_out=merge(0.0d0,1.0d0,valuein)
   type is (character(len=*));      read(valuein,*) d_out
   !type is (real(kind=real128))
   !   if(valuein.gt.big)then
   !      write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
   !   endif
   !   d_out=dble(valuein)
   class default
     d_out=0.0d0
     !!stop '*M_verify::anyscalar_to_double: unknown type'
   end select
end function anyscalar_to_double
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_verify
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
 
 
!>>>>> app/numdiff.f90
! (Regression testing)
!  numdiff(1): Detect numeric changes in output files
!
!  A simple Fortran program for comparing two files for numeric differences.
!  numdiff(1) is typically used to detect changes in numeric results in ported or recompiled routines.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
program numdiff
! These routines are available for general use. If you change this code, please acknowledge the original author
      use M_CLI2, only : set_args,dget,lget,iget,sget,rget     ! module for cracking command line parameters
      implicit none
      character(len=:),allocatable :: filein_old              ! name of template file
      character(len=:),allocatable :: filein_new              ! name of new file to test against template
      logical                :: verbose=.false.               ! flag if -verbose option has been specified
      integer                :: idigits                       ! number of significant digits to require as equal
      doubleprecision        :: tolerance                     ! percentage delta to report if idigits is 0
      integer                :: ier                           ! flag indicating if an error occurred
      integer                :: significant_digits            ! number of significant digits in a dble
      character(len=132)     :: warning                       ! string used for writing error messages
      integer                :: GLOBAL_COUNT=0
      real                   :: margin
      character(len=:),allocatable :: help_text(:)
      character(len=:),allocatable :: version_text(:)
!-----------------------------------------------------------------------------------------------------------------------------------
!     define the command options and default values and apply arguments from user command line
      call setup()
      call set_args(' --percent 0.0001d0 --digits 0 --margin 0 --old " " --new " "',help_text,version_text)

      filein_old=sget("old")                                           ! get -old filein_old
      filein_new=sget("new")                                           ! get -new filein_new

      verbose=lget("verbose")                                          ! get -verbose

      tolerance=dget("percent")                                        ! get -percent TOLERANCE
      idigits=iget("digits")                                           ! get -digits NNN
      margin=rget("margin")                                            ! get -margin XXX.XX

!-----------------------------------------------------------------------------------------------------------------------------------
      if(filein_old.eq.' '.or.filein_new.eq.' ')then                           ! if filenames are not specified stop
         print *, "*numdiff* error: old and new filenames are required."       ! report missing filename
         stop 10
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(idigits.ne.0)then                                     ! check for inappropriate number of digits to test that is too small
         significant_digits=int(log10(2.0**digits(0.0d0)))     ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A DOUBLE NUMBER.
         if(idigits.lt.0)then                                  ! check for inappropriate number of digits to test that is too small
            write(warning,'(a,i0)')'*numdiff* bad number of significant digits=',idigits
            call remark(warning)
            idigits=significant_digits                         ! set bad value to maximum allowed for double precision number
         else if(idigits .gt. significant_digits)then          ! check for inappropriate number of digits to test  that is too big
            write(warning,'(a,i0)')'*numdiff* significant digit requested too high=',idigits
            call remark(warning)
            idigits=significant_digits                         ! set bad value to maximum allowed for double precision number
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      write(*,'(132("-"))')
      print *, "       | *numdiff* numerical differences"                      ! display command options
      print *, "       | old file=",trim(filein_old)
      print *, "       | new file=",trim(filein_new)
      if(idigits.ne.0)then
         write(*,'(a,i0)')"        | digits of precision=",idigits
      elseif(margin.ne.0)then
         write(*,'(a,g0)')"        | relative margin =",margin
      else
         write(*,'(a,g0)')"        | percent threshold =",tolerance,"(%)"
      endif
      write(*,'(132("-"))')
      call num_diff(filein_old,filein_new,tolerance,idigits,margin)            ! compare the files
      write(*,'(132("-"))')
      write(*,*)'Numerical Differences=',GLOBAL_COUNT
      write(*,'(132("-"))')
      if(GLOBAL_COUNT.ne.0)then    ! return command exit value to system where supported
         STOP 1
      endif
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine num_diff(file_old,file_new,percent_tolerance,idigits,margin)
!     num_diff: compare numbers in otherwise identical files
!
!     Author: John S. Urban
!     Date:   1990 ,2009, 2013
!
!     given two files (width less than 264 characters) who are assumed to be
!     identical except for some numeric values, find matching lines that
!     have values which differ by a certain percentage. assume numbers are
!     delimited by spaces. not totally generic but has potential.
!     originally made to check steam table qa program output
!
!     make sure all numbers are delimited by spaces. "x=10" or "10,20"
!     will not work! could sometimes change "=" to "= ", "," to ", ".
!
!     having some problems with a generic definition of percent change
!     when one of the values is at or near 0.
!
implicit none
! ident_1="@(#)num_diff(3f): compare numbers in otherwise identical files"
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)    :: file_old
      character(len=*),intent(in)    :: file_new
      doubleprecision,intent(in)     :: percent_tolerance         ! percent threshold
      integer,intent(in)             :: idigits                   ! number of digits  to match
      real,intent(in)                :: margin                    ! relative margin
!-----------------------------------------------------------------------------------------------------------------------------------
      doubleprecision                :: per
      doubleprecision                :: digirank
      doubleprecision                :: marginrank
      character(len=264)             :: lline
      character(len=264)             :: rline
      character(len=264)             :: dline
      logical                        :: not_end_of_file_1
      logical                        :: not_end_of_file_2
      integer                        :: lcount
      integer                        :: ilen
      integer                        :: ilen2
      integer                        :: ios
      integer                        :: ind
      doubleprecision                :: permax
      doubleprecision                :: digirankmax
      doubleprecision                :: marginrankmax
!-----------------------------------------------------------------------------------------------------------------------------------
      open(unit=21,file=file_old(:len_trim(file_old)),status="old",iostat=ios)            ! open template file
      if(ios.ne.0)then
         call remark('*numdiff* error: could not open file specified with -old')
         stop 20
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      open(unit=22,file=file_new(:len_trim(file_new)),status="old",iostat=ios)            ! open file of new values
      if(ios.ne.0)then
         call remark('*numdiff* error: could not open file specified with -new')
         stop 30
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      not_end_of_file_1=.true.                                                            ! set flag not at end of input on old file
      not_end_of_file_2=.true.                                                            ! set flag not at end of input on new file
      lcount=0                                                                            ! number of lines read
      permax=0.0d0                                                                        ! highest percentage difference found
      digirankmax=dble(idigits)                                                           ! highest digit difference found
!-----------------------------------------------------------------------------------------------------------------------------------
      INFINITE: do
         !--------------------------------------------------------------------------------------------------------------------------
         if(not_end_of_file_1)then                                     ! get next line from file 1
            read(21,"(a)",iostat=ios)lline
            if(ios.ne.0)then
               not_end_of_file_1=.false.
               lline=" "
            endif
         endif
         !--------------------------------------------------------------------------------------------------------------------------
         if(not_end_of_file_2)then                                     ! get next line from file 2
            read(22,"(a)",iostat=ios)rline
            if(ios.ne.0)then
               not_end_of_file_2=.false.
               rline=" "
            endif
         endif
         !--------------------------------------------------------------------------------------------------------------------------
         if(.not.(not_end_of_file_1.or.not_end_of_file_2))then         ! hit end of file on input, exit program
            if(idigits.eq.0)then
               write(*,*)"       |maximum difference=",permax,"(%)"
            else
               write(*,*)"       |minimum match=",digirankmax,"(digits)"
            endif
            exit INFINITE
         endif
         !--------------------------------------------------------------------------------------------------------------------------
         lcount=lcount+1                                                  ! count of lines of input successfully read
         if(lline.ne.rline)then                                           ! if lines are not equal parse them and check differences
            if(idigits.ne.0)then
               call digits_diff(lline,rline,dline,digirank,idigits,ier,ilen,ilen2,ind)
               if(ind.ne.0)then
                  digirankmax=min(digirankmax,digirank)
                  write(*,'("--------|",a)')repeat('-',132-9)
                  write(*,"('old     |',a)")trim(lline(:ilen))
                  write(*,"(i8,     '|',a)")lcount,trim(dline(:max(ilen,ilen2)))
                  write(*,"('new     |',a)")trim(rline(:ilen2))
                  write(*,"('>>>>>>>>|minimum match =',g0,'(digits)')")digirank
                  write(*,'("--------|",a)')repeat('-',132-9)
               elseif(verbose)then
                  write(*,"(i8,     '|',a)")lcount,lline(:ilen)
               endif
            elseif(margin.ne.0)then
               call margin_diff(lline,rline,dline,marginrank,margin,ier,ilen,ilen2,ind)
               if(ind.ne.0)then
                  marginrankmax=min(marginrankmax,marginrank)
                  write(*,'("--------|",a)')repeat('-',132-9)
                  write(*,"('old     |',a)")trim(lline(:ilen))
                  write(*,"(i8,     '|',a)")lcount,trim(dline(:max(ilen,ilen2)))
                  write(*,"('new     |',a)")trim(rline(:ilen2))
                  write(*,"('>>>>>>>>|minimum match =',g0,'(digits)')")marginrank
                  write(*,'("--------|",a)')repeat('-',132-9)
               elseif(verbose)then
                  write(*,"(i8,     '|',a)")lcount,lline(:ilen)
               endif
            else
               ! note that PERCENT_DIFF(3f) errors come out before printout of line
               call percent_diff(lline,rline,dline,per,percent_tolerance,ier,ilen,ilen2)
               permax=max(permax,per)                                     ! if a new maximum percent difference was found record it
               if(per.ge.percent_tolerance)then                           ! print lines over percentage tolerance
                  write(*,'("--------|",a)')repeat('-',132-9)
                  write(*,"('old     |',a)")trim(lline(:ilen))
                  write(*,"(i8,     '|',a)")lcount,dline(:max(ilen,ilen2))
                  write(*,"('new     |',a)")trim(rline(:ilen2))
                  write(*,"('>>>>>>>>|maximum difference =',g0,'(%)')")per
                  write(*,'("--------|",a)')repeat('-',132-9)
               elseif(verbose)then
                  write(*,"(i8,     '|',a)")lcount,lline(:ilen)
               endif
            endif
         elseif(verbose)then
            write(*,"(i8,     '=',a)")lcount,trim(lline)
         endif
      enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine num_diff
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine margin_diff(line,line2,dline,per,margin,ier,ilen,ilen2,ind_line)
!     Author: John S. Urban
!     Date:   1989,1990, 2013
!  assuming same number of strings on two lines to be compared, find strings that do not match, then if both strings are numbers
!  find if they are equal within a specified relative margin.  makes assumption that only difference in lines is values of numbers
!  that string word differences can be ignored, and same number of words on each line
!-----------------------------------------------------------------------------------------------------------------------------------
use :: M_verify, only : in_margin
implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)    :: line                 ! line from file 1
   character(len=*),intent(in)    :: line2                ! line from file 2
   character(len=*),intent(out)   :: dline                ! line showing difference
   doubleprecision, intent(out)   :: per                  ! maximum difference
   real, intent(in)               :: margin               ! relative margin
   integer,intent(out)            :: ier                  ! returned error code; 0 is no error
   integer,intent(out)            :: ilen                 ! length of line trimmed
   integer,intent(out)            :: ilen2                ! length of line2 trimmed
   integer,intent(out)            :: ind_line
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                        :: ind
   integer                        :: i
   integer                        :: ic
   integer                        :: ic2
   doubleprecision                :: acurcy
   doubleprecision                :: value
   doubleprecision                :: value2
   doubleprecision                :: diff
   integer,parameter              :: ip=264/2
   integer                        :: iflag
   integer                        :: iflag2
   integer                        :: ii
   integer is(ip),ie(ip),is2(ip),ie2(ip)
!-----------------------------------------------------------------------------------------------------------------------------------
   dline=' '                                               ! line filled with # where differences occur
   per=0.0d0                                               !
   ier=0                                                   ! error flag: 0 ok, -1 error
   acurcy=0.0d0
   call parse(line ,ic ,is ,ie ,ilen )                     ! fill array with tokens from line 1
   call parse(line2,ic2,is2,ie2,ilen2)                     ! fill array with tokens from line 2
   ind_line=0
   if(ic.ne.0)then                                         ! ic=number of strings found
     do i=1,ic
        ind=0
        call getnum(line(is(i):ie(i)),value,iflag)         ! try to convert substring line(is(i):ie(i) to number
        if(iflag.eq.0)then                                 ! value = real value of string
           if(i.gt.ic2)then
              ier=-1
              write(*,*)"[error]"," no words left to compare"
              exit
           else
              call getnum(line2(is2(i):ie2(i)),value2,iflag2) ! try to convert substing from line2 to number
           endif
           if(iflag2.ne.0)then                             ! line1 was number but line2 was not
              ier=-1

              write(*,*)"[error]",line2(is2(i):ie2(i))," non-numeric"
           else
              if(value.eq.value2)then
                 diff=0.0d0
              else
                 if(in_margin(value,value2,margin))then ! compare two double numbers
                 else
                    ind_line=max(ind,ind_line)
                    ! if found a difference over limit, fill dline with # characters
                    GLOBAL_COUNT=GLOBAL_COUNT+1
                    do ii=is(i),ie(i)
                       dline(ii:ii)='#'
                    enddo
                 endif
              endif
           endif
        elseif(line(is(i):ie(i)).ne.line2(is2(i):ie2(i)))then  ! non-numeric stings
           ! first string was not numeric, but strings are not supposed to differ
           write(*,*)"[error]",line(is(i):ie(i)),"><",line2(is2(i):ie2(i))
        endif
     enddo
   endif
end subroutine margin_diff
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine digits_diff(line,line2,dline,per,idigits,ier,ilen,ilen2,ind_line)
!     Author: John S. Urban
!     Date:   1989,1990, 2013
!  assuming same number of strings on two lines to be compared, find strings that do not match, then if both strings are numbers
!  find if they have IDIGITS matching significant digits.  makes assumption that only difference in lines is values of numbers
!  that string word differences can be ignored, and same number of words on each line
!-----------------------------------------------------------------------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)    :: line                 ! line from file 1
   character(len=*),intent(in)    :: line2                ! line from file 2
   character(len=*),intent(out)   :: dline                ! line showing difference
   doubleprecision, intent(out)   :: per                  ! maximum difference
   integer, intent(in)            :: idigits              ! number of digits to match
   integer,intent(out)            :: ier                  ! returned error code; 0 is no error
   integer,intent(out)            :: ilen                 ! length of line trimmed
   integer,intent(out)            :: ilen2                ! length of line2 trimmed
   integer,intent(out)            :: ind_line
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                        :: ind
   integer                        :: i
   integer                        :: ic
   integer                        :: ic2
   doubleprecision                :: value
   doubleprecision                :: value2
   doubleprecision                :: diff
   doubleprecision                :: acurcy
   integer,parameter              :: ip=264/2
   integer                        :: iflag
   integer                        :: iflag2
   integer                        :: ii
   integer is(ip),ie(ip),is2(ip),ie2(ip)
!-----------------------------------------------------------------------------------------------------------------------------------
   dline=' '                                               ! line filled with # where differences occur
   per=0.0d0                                               !
   ier=0                                                   ! error flag: 0 ok, -1 error
   acurcy=0.0d0
   call parse(line ,ic ,is ,ie ,ilen )                     ! fill array with tokens from line 1
   call parse(line2,ic2,is2,ie2,ilen2)                     ! fill array with tokens from line 2
   ind_line=0
   if(ic.ne.0)then                                         ! ic=number of strings found
     do i=1,ic
        ind=0
        call getnum(line(is(i):ie(i)),value,iflag)         ! try to convert substring line(is(i):ie(i) to number
        if(iflag.eq.0)then                                 ! value = real value of string
           if(i.gt.ic2)then
              ier=-1
              write(*,*)"[error]"," no words left to compare"
              exit
           else
              call getnum(line2(is2(i):ie2(i)),value2,iflag2) ! try to convert substing from line2 to number
           endif
           if(iflag2.ne.0)then                             ! line1 was number but line2 was not
              ier=-1

              write(*,*)"[error]",line2(is2(i):ie2(i))," non-numeric"
           else
              if(value.eq.value2)then
                 diff=0.0d0
              else
                 call same8(value,value2,idigits,ACURCY,IND) ! compare two double numbers up to a specified number of digits
                 ind_line=max(ind,ind_line)
              endif
              per=max(per,dble(acurcy))
              if(ind.ne.0)then  ! if found a difference over limit, fill dline with # characters
                 GLOBAL_COUNT=GLOBAL_COUNT+1
                 do ii=is(i),ie(i)
                    dline(ii:ii)='#'
                 enddo
              endif
           endif
        elseif(line(is(i):ie(i)).ne.line2(is2(i):ie2(i)))then  ! non-numeric stings
           ! first string was not numeric, but strings are not supposed to differ
           write(*,*)"[error]",line(is(i):ie(i)),"><",line2(is2(i):ie2(i))
        endif
     enddo
   endif
end subroutine digits_diff
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine percent_diff(line,line2,dline,per,percent_tolerance,ier,ilen,ilen2)
!     Author: John S. Urban
!     Date:   1989,1990
!  assuming same number of strings on two lines to be compared, find
!  strings that do not match, then if both strings are numbers find
!  the maximum percentage difference.
!  makes assumption that only difference in lines is values of numbers
!  that string word differences can be ignored,
!  and same number of words on each line
!-----------------------------------------------------------------------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)    :: line                 ! line from file 1
   character(len=*),intent(in)    :: line2                ! line from file 2
   character(len=*),intent(out)   :: dline                ! line showing difference
   doubleprecision, intent(out)   :: per                  ! maximum percentage difference
   doubleprecision, intent(in)    :: percent_tolerance    ! percentage limit
   integer,intent(out)            :: ier                  ! returned error code; 0 is no error
   integer,intent(out)            :: ilen                 ! length of line trimmed
   integer,intent(out)            :: ilen2                ! length of line2 trimmed
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                        :: i
   integer                        :: ic
   integer                        :: ic2
   doubleprecision                :: value
   doubleprecision                :: value2
   doubleprecision                :: x1
   doubleprecision                :: x2
   doubleprecision                :: bot
   doubleprecision                :: diff
   integer,parameter              :: ip=264/2
   integer                        :: iflag
   integer                        :: iflag2
   integer                        :: ii
   integer is(ip),ie(ip),is2(ip),ie2(ip)
!-----------------------------------------------------------------------------------------------------------------------------------
   dline=' '                                               ! line filled with # where differences occur
   per=0.0d0                                               !
   ier=0                                                   ! error flag: 0 ok, -1 error
   call parse(line ,ic ,is ,ie ,ilen )                     ! fill array with tokens from line 1
   call parse(line2,ic2,is2,ie2,ilen2)                     ! fill array with tokens from line 2
   if(ic.ne.0)then                                         ! ic=number of strings found
     do i=1,ic
        call getnum(line(is(i):ie(i)),value,iflag)         ! try to convert substring line(is(i):ie(i) to number
        if(iflag.eq.0)then                                 ! value = real value of string
           call getnum(line2(is2(i):ie2(i)),value2,iflag2) ! try to convert substing from line2 to number
           if(iflag2.ne.0)then                             ! line1 was number but line2 was not
              ier=-1
              write(*,*)"[error]",line2(is2(i):ie2(i))," non-numeric"
           else
              x1=abs(value)
              x2=abs(value2)
              if(x1.eq.x2)then
                 diff=0.0d0
              else
                 bot=min(x1,x2)
                 if(bot.eq.0.0)bot=max(x1,x2)
                 ! if smaller value is zero, per is always 100,
                 ! could possibly get an overflow/underflow condition here
                 diff=abs(value-value2)/bot ! maximized percentage difference
              endif
              per=max(per,diff*100.0)
              if(diff*100.0.ge.percent_tolerance)then  ! if found a difference over limit, fill dline with # characters
                 GLOBAL_COUNT=GLOBAL_COUNT+1
                 do ii=is(i),ie(i)
                    dline(ii:ii)='#'
                 enddo
              endif
           endif
        elseif(line(is(i):ie(i)).ne.line2(is2(i):ie2(i)))then  ! non-numeric stings
           ! first string was not numeric, but strings are not supposed to differ
           write(*,*)"[error]",line(is(i):ie(i)),"><",line2(is2(i):ie2(i))
        endif
     enddo
   endif
end subroutine percent_diff
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
   subroutine parse(input_line,inotnull,ibegin,iterm,ilen)
!-----------------------------------------------------------------------------------------------------------------------------------
!$@(#) M_strings::parse(3f): parse string on delimiter characters and store token extents into arrays
!  Author: John S. Urban
!  Date:   1986,1990,2013
!
!  given a line of up to 264 characters of structure " par1 par2 par3 ... par(n) " where par(i) are separated by a delimiter
!  find how many pars and beginning and ending column of each, ignoring leading and trailing blanks
!  output array sizes should be istart(132), iterm(132)
!
!  adjacent delimiters in the input string do not create an empty string in the output array
!  no quoting of delimiters is supported
!
!  also return position of last non-blank character (even if more than 132 elements were found).
!
!  delimiters are from set ",; |".
!
!  no checking for more than ipars parameters, if any more they are ignored
!-----------------------------------------------------------------------------------------------------------------------------------
   integer,parameter             :: ipars=264              ! max number of strings INPUT_LINE could split into if all delimiter
   character(len=*),intent(in)   :: input_line             ! input string to tokenize
   integer,intent(out)           :: ibegin((ipars+1)/2)    ! positions in input string where tokens start
   integer,intent(out)           :: iterm((ipars+1)/2)     ! positions in input string where tokens end
   integer,intent(out)           :: inotnull               ! count strings not composed of delimiters
   integer,intent(out)           :: ilen                   ! length of input string with trailing spaces trimmed
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
   integer                       :: icount                 ! number of tokens found
   integer                       :: i10,i30                ! loop counters
   integer                       :: icol                   ! pointer into input string as it is being parsed
   integer                       :: idlim                  ! number of delimiter characters
   integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
   integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   dlim=' ;,|'                                             ! decide on value for optional DELIMITERS parameter
   idlim=len(dlim)                                         ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(input_line)                                      ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.eq.0)return                                            ! command was totally blank
!-----------------------------------------------------------------------------------------------------------------------------------
                                                                  ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(inotnull+1)=icol                                  ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(inotnull+1)=ilen                                ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(inotnull+1):ilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(inotnull+1)=min(iterm(inotnull+1),ifound+ibegin(inotnull+1)-2)
               endif
            enddo
            icol=iterm(inotnull+1)+2                              ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(inotnull+1)=icol-1                              ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(inotnull+1)-ibegin(inotnull+1)+1)
         icount=inotnull                                          ! increment count of number of tokens found
         if(icol.gt.ilen)then                                     ! text left
            exit INFINITE
         endif
      enddo INFINITE
      ilen=iterm(inotnull)
   end subroutine parse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine parse1(line,ic,is,ie,ilen)
!  Author: John S. Urban
!  Date:   1986,1990,2013
!
!  given a line of structure " par1 par2 par3 ... par(n) "
!  find how many pars and beginning and ending column of each ignoring leading and trailing blanks
!
!  also return position of last non-blank character (even if more than 132 elements were found).
!
!  spaces are only legal delimiters.
!  no checking for more than ipars parameters, if any more they are ignored
!-----------------------------------------------------------------------------------------------------------------------------------
   implicit none
   integer,parameter              :: ipars=264/2   ! maximum number of values expected in a 264-character line
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)    :: line          ! input line to parse
   integer,intent(out)            :: ic            ! how many tokens were found
   integer,intent(out)            :: is(ipars)     ! start position of tokens
   integer,intent(out)            :: ie(ipars)     ! end position of tokens
   integer,intent(out)            :: ilen          ! length of input line; or last position parsed
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=1),parameter     :: delim=" "
   integer                        :: icol
   integer                        :: iarray
   integer                        :: istart
   integer                        :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
   ic=0                                                               ! initialize count of number of tokens found
   ilen=len_trim(line)                                                ! position of last non-blank character in input line
!-----------------------------------------------------------------------------------------------------------------------------------
!  command was totally blank
   if(ilen.eq.0)return
!-----------------------------------------------------------------------------------------------------------------------------------
!  there is at least one non-blank character in the command
!  find next non-delimiter
   icol=1
   do iarray=1,ipars,1
      !-----------------------------------------------------------------------------------------------------------------------------
      INFINITE: do
         if(line(icol:icol).ne.delim)then
            istart=icol
            is(iarray)=icol
            iend=index(line(istart:ilen),delim)
            if(iend.le.0)then
               ie(iarray)=ilen
               ic=iarray
               return
            else
               iend=iend+istart-2
               ie(iarray)=iend
            endif
            icol=iend+2
            exit INFINITE
         else
            icol=icol+1
         endif
      enddo INFINITE
      !-----------------------------------------------------------------------------------------------------------------------------
      !  last character in line was a delimiter, so no text left
      !  (should not happen where blank=delimiter)
      if(icol.gt.ilen)then
         ic=iarray
         return
      endif
   enddo
!  more than ipars elements
   ic=ipars
end subroutine parse1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine getnum(chars,value,iflag)
!  Author: John S. Urban
!  Date:   07/15/1986
!  o  works with any g-format input, including integer, real, and exponential.
!  o  prints an error message to output and returns a value of zero if an error occurs.
!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
! ident_2="@(#)getnum(3f): returns a real value from a numeric character string"
      character(len=*),intent(in)    ::  chars
      doubleprecision,intent(out)    ::  value
      integer,intent(out)            ::  iflag
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=13)              ::  frmt
      integer                        :: ios
!-----------------------------------------------------------------------------------------------------------------------------------
      write(frmt,'("(bn,g",i5,".0)")')len(chars)
      read(chars,fmt=frmt,iostat=ios)value
      if(ios.ne.0)then
         value=0.0d0
         iflag=1
         !write(*,*)"error occurred in read in function getnum, iostat=",ierr
         !write(*,*)"input string=",chars
      else
         iflag=0
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine getnum
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine remark(string)
use iso_fortran_env, only : error_unit ! access computing environment
implicit none
! ident_3="@(#)remark(3f): writes a message to standard error using a standard f2003 method"
character(len=*),intent(in) :: string
      write(error_unit,*)trim(string)
end subroutine remark
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
SUBROUTINE same8(X,Y,IDIGIT,ACURCY,IND)
!-----------------------------------------------------------------------------------------------------------------------------------
!     Compare two double numbers only up to a specified number of digits
!
!     If two numbers agree to IDIGIT digits of accuracy return IND=0 else return IND=1.
!     Also return a number ACURCY that indicates how many digits do agree.
!
!     Tolerance ...
!        X and Y are considered equal within IDIGIT relative tolerance,
!        if ACURCY is greater than IDIGIT.
!
!     Based on ...
!     ** NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ACCDIG V 7.00  2/14/90. **
!        DAVID HOGBEN,
!        STATISTICAL ENGINEERING DIVISION,
!        CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
!        A337 ADMINISTRATION BUILDING,
!        NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!        GAITHERSBURG, MD 20899
!           TELEPHONE 301-975-2845
!           ORIGINAL VERSION - OCTOBER,  1969.
!           CURRENT VERSION  - FEBRUARY, 1990.
!           CURRENT VERSION  - FEBRUARY, 1991. JSU
!           CURRENT VERSION  - FEBRUARY, 2013. JSU
!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
! ident_4="@(#)same8(3f): compare two double numbers only up to a specified number of digits"
!-----------------------------------------------------------------------------------------------------------------------------------
!     INPUT ...
      doubleprecision,intent(in)  :: x                         ! FIRST  OF TWO DOUBLE NUMBERS TO BE COMPARED.
      doubleprecision,intent(in)  :: y                         ! SECOND OF TWO DOUBLE NUMBERS TO BE COMPARED.
      integer,intent(in)          :: idigit                    ! NUMBER OF DIGITS TO BE SATISFIED IN RELATIVE TOLERANCE.
!-----------------------------------------------------------------------------------------------------------------------------------
!     OUTPUT ...
      integer,intent(out)         :: ind                       ! = 0, IF TOLERANCE IS     SATISFIED.
                                                               ! = 1, IF TOLERANCE IS NOT SATISFIED.
      doubleprecision,intent(out) :: acurcy                    ! = - LOG10 (ABS((X-Y)/Y)))
!-----------------------------------------------------------------------------------------------------------------------------------
      doubleprecision             :: diff                      ! delta of X and Y values
      doubleprecision             :: digi
      integer                     :: ireal_significant_digits  ! maximum number of significant digits for double precision values
      character(len=132)          :: warning                   ! string user for writing error messages
!-----------------------------------------------------------------------------------------------------------------------------------
      ireal_significant_digits=int(log10(2.0**digits(0.0d0)))  ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A DOUBLE NUMBER.
      digi=idigit                                              ! floating value copy of IDIGIT
!-----------------------------------------------------------------------------------------------------------------------------------
      if(digi.le.0)then                                        ! check for inappropriate number of digits to test that is too small
         write(warning,'(a,i0)')'*same8* bad number of significant digits=',idigit; call remark(warning)
         call remark(warning)                                  ! write warning to stderr
         digi=ireal_significant_digits                         ! set bad value to maximum allowed for double precision number
      else if(digi .gt. ireal_significant_digits)then          ! check for inappropriate number of digits to test  that is too big
         write(warning,'(a,i0)')'*same8* significant digit requested too high=',idigit; call remark(warning)
         call remark(warning)                                  ! write warning to stderr
         digi=real(ireal_significant_digits)                   ! set bad value to maximum allowed for double precision number
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      diff = x - y                                             ! find difference between to values to be compared
!-----------------------------------------------------------------------------------------------------------------------------------
      if (diff .eq. 0.0) then                                  ! no difference
         acurcy = digi
      else if (y .eq. 0.0) then                                ! special case where Y is zero
         acurcy = - log10 (abs (x))                            ! X cannot be zero so get measure of X
      else
         acurcy = - log10 ( abs(diff) ) + log10 ( abs(y) )     ! get measure
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if (acurcy .lt. digi ) then
         ind = 1                                               ! not equal to desired number of digits
      else
         ind = 0                                               ! equal to desired number of digits
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine same8
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
' numdiff(1f) - [DEVELOPER] Compare numeric differences in a file                ',&
' (LICENSE:PD)                                                                   ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
' numdiff                                                                        ',&
'    -old FILENAME -new FILENAME                                                 ',&
'    [ -percent REAL_VALUE|-digits N|-margin XXX.XX]                             ',&
'    [ -verbose]|                                                                ',&
'    [ --help|--version]                                                         ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
' NUMDIFF assumes two files are basically identical except for numeric           ',&
' differences, and finds values whose differences exceed a specified             ',&
' tolerance.                                                                     ',&
' o file widths are required to be less than 264 characters.                     ',&
' o numbers are assumed to be delimited by spaces, commas, semi-colons,          ',&
'   or vertical line characters (" ,;|"). Adjacent delimiters are ignored.       ',&
'                                                                                ',&
' This program was originally written to simplify the comparison of values       ',&
' generated by new versions of numeric libraries to previous versions            ',&
' of the libraries.                                                              ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    The options are                                                             ',&
'                                                                                ',&
'    -old FILENAME                                                               ',&
'         name of file containing template values                                ',&
'                                                                                ',&
'    -new FILENAME                                                               ',&
'         name of file containing new values                                     ',&
'                                                                                ',&
'    -percent REAL_VALUE                                                         ',&
'             set threshold at which to report values as a percentage of the     ',&
'             template value                                                     ',&
'                                                                                ',&
'    -digits N                                                                   ',&
'            set threshold at which to report values as a number of digits       ',&
'            if -digits is specified -percent is ignored.                        ',&
'                                                                                ',&
'    -margin XXX.XX                                                              ',&
'            set threshold to a relative margin of the magnitude of the values   ',&
'                                                                                ',&
'    -verbose                                                                    ',&
'       shows the lines that pass the criteria from OLDFILE as well.             ',&
'                                                                                ',&
'    --help                                                                      ',&
'       display this help text and exit                                          ',&
'                                                                                ',&
'    --version                                                                   ',&
'       display information on the code version and exit                         ',&
'                                                                                ',&
'USAGE                                                                           ',&
' 1. GENERATE TEMPLATE:  call all your numeric procedures over their             ',&
'    allowed ranges and print the values to a file on your original system.      ',&
'    Save this file as your master QA template.                                  ',&
' 2. GENERATE TRIAL DATA:  When you port the procedures to another system or     ',&
'    recompile using a new compiler run your QA program again and save           ',&
'    the second file.                                                            ',&
' 3. TEST/COMPARE:  run the numdiff(1) program:                                  ',&
'                                                                                ',&
'      numdiff -old MASTER_TEMPLATE_FILE -new NEW_OUTPUT_FILE -percent 0.0001    ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'                                                                                ',&
'   We will assume we have two files meeting the above criteria called           ',&
'   "cray_results.txt" and "cygwin_results.txt". To compare the values           ',&
'   we enter                                                                     ',&
'                                                                                ',&
'     numdiff -old cray_results.txt -new cygwin_results.txt -percent 0.00001     ',&
'                                                                                ',&
'   A diff(1) of the following input files would show every line, as one         ',&
'   uses the "E" prefix for exponents, while the other uses "D". Even when a     ',&
'   diff(1) would show few lines, you have to inspect each difference to see     ',&
'   how large a difference in value was found. Using numdiff(1) you can          ',&
'   ignore most insignificant differences.                                       ',&
'                                                                                ',&
'   OUTPUT                                                                       ',&
'                                                                                ',&
'   Results from the run are                                                     ',&
'                                                                                ',&
'     >        | *numdiff* numerical differences                                 ',&
'     >        | old file=in1                                                    ',&
'     >        | new file=in2                                                    ',&
'     >        | percent threshold =  0.1E-006 (%)                               ',&
'     >--------|------------------------------------------------------------------------------------------------------------',&
'     >old     |    0.32000000E+02    0.88589141E-01   -0.17863993E-01   -0.36879559E-04    0.16022057E-01    0.31990327E+02',&
'     >       8|                                       ###############   ###############',&
'     >new     |    0.32000000D+02    0.88589141D-01   -0.17863994D-01   -0.36879562D-04    0.16022057D-01    0.31990327D+02',&
'     >>>>>>>>>|maximum difference =.81345875138179356E-005(%)                   ',&
'     >--------|------------------------------------------------------------------------------------------------------------',&
'                                                                                ',&
'   This indicates values did not pass on line 8. "#" characters underline the values.',&
'                                                                                ',&
'                                                                                ',&
'Input file "cray_results.txt"                                                   ',&
'                                                                                ',&
'   1TESTS STARTED.                                                              ',&
'                                                                                ',&
'                                                                                ',&
'   1          T,PSL(T),HSL(T),SSL(T),VSL(T),TSLH(H)                             ',&
'   0         PMIN=    0.885891E-01    PMAX=    0.320823E+04    DELP=    0.100000E+03',&
'             TMIN=    0.320000E+02    TMAX=    0.705470E+03    DELT=    0.200000E+02',&
'   0                                                                            ',&
'       0.32000000E+02    0.88589141E-01   -0.17863993E-01   -0.36879559E-04    0.16022057E-01    0.31990327E+02',&
'       0.40000000E+02    0.12163360E+00    0.80272719E+01    0.16194175E-01    0.16018940E-01    0.40003431E+02',&
'       0.60000000E+02    0.25611401E+00    0.28059533E+02    0.55503370E-01    0.16033165E-01    0.60005984E+02',&
'       0.80000000E+02    0.50682853E+00    0.48036541E+02    0.93222577E-01    0.16071928E-01    0.79998151E+02',&
'       0.10000000E+03    0.94923553E+00    0.67998872E+02    0.12954114E+00    0.16129956E-01    0.99994830E+02',&
'       0.12000000E+03    0.16927366E+01    0.87966229E+02    0.16459153E+00    0.16204270E-01    0.11999684E+03',&
'       0.14000000E+03    0.28891787E+01    0.10794963E+03    0.19847772E+00    0.16293096E-01    0.14000080E+03',&
'       0.16000000E+03    0.47413557E+01    0.12795766E+03    0.23128914E+00    0.16395272E-01    0.16000351E+03',&
'       0.18000000E+03    0.75110274E+01    0.14799946E+03    0.26310734E+00    0.16510005E-01    0.18000351E+03',&
'       0.20000000E+03    0.11526035E+02    0.16808606E+03    0.29400880E+00    0.16636808E-01    0.20000116E+03',&
'       0.22000000E+03    0.17186197E+02    0.18823058E+03    0.32406594E+00    0.16775474E-01    0.21999785E+03',&
'       0.24000000E+03    0.24967794E+02    0.20844802E+03    0.35334719E+00    0.16926045E-01    0.23999513E+03',&
'       0.26000000E+03    0.35426601E+02    0.22875486E+03    0.38191689E+00    0.17088786E-01    0.25999384E+03',&
'       0.28000000E+03    0.49199533E+02    0.24916873E+03    0.40983516E+00    0.17264170E-01    0.27999361E+03',&
'       0.30000000E+03    0.67005075E+02    0.26970823E+03    0.43715817E+00    0.17452875E-01    0.29999280E+03',&
'       0.32000000E+03    0.89642735E+02    0.29039297E+03    0.46393855E+00    0.17655789E-01    0.31998899E+03',&
'       0.34000000E+03    0.11799178E+03    0.31124389E+03    0.49022610E+00    0.17874025E-01    0.33998018E+03',&
'       0.36000000E+03    0.15300954E+03    0.33228370E+03    0.51606862E+00    0.18108958E-01    0.35996652E+03',&
'       0.38000000E+03    0.19572950E+03    0.35353750E+03    0.54151277E+00    0.18362259E-01    0.38000205E+03',&
'       0.40000000E+03    0.24725940E+03    0.37509279E+03    0.56667770E+00    0.18637646E-01    0.40003002E+03',&
'       0.42000000E+03    0.30877960E+03    0.39689616E+03    0.59150311E+00    0.18935141E-01    0.41998032E+03',&
'       0.44000000E+03    0.38154169E+03    0.41902722E+03    0.61609004E+00    0.19258996E-01    0.43998928E+03',&
'       0.46000000E+03    0.46686778E+03    0.44153583E+03    0.64049662E+00    0.19613160E-01    0.46000991E+03',&
'       0.48000000E+03    0.56615075E+03    0.46448172E+03    0.66478705E+00    0.20002609E-01    0.48002037E+03',&
'       0.50000000E+03    0.68085599E+03    0.48793750E+03    0.68903400E+00    0.20433708E-01    0.50001633E+03',&
'       0.52000000E+03    0.81252603E+03    0.51199311E+03    0.71332237E+00    0.20914751E-01    0.52000390E+03',&
'       0.54000000E+03    0.96279001E+03    0.53676266E+03    0.73775486E+00    0.21456790E-01    0.53999326E+03',&
'       0.56000000E+03    0.11333816E+04    0.56239502E+03    0.76246065E+00    0.22074947E-01    0.55999280E+03',&
'       0.58000000E+03    0.13261708E+04    0.58909059E+03    0.78760893E+00    0.22790623E-01    0.58000418E+03',&
'       0.60000000E+03    0.15432192E+04    0.61712901E+03    0.81343122E+00    0.23635444E-01    0.60001858E+03',&
'       0.62000000E+03    0.17868695E+04    0.64691984E+03    0.84026231E+00    0.24659110E-01    0.62001710E+03',&
'       0.64000000E+03    0.20598878E+04    0.67911317E+03    0.86863104E+00    0.25947611E-01    0.63998803E+03',&
'       0.66000000E+03    0.23656783E+04    0.71493433E+03    0.89954128E+00    0.27678770E-01    0.66001396E+03',&
'       0.68000000E+03    0.27085898E+04    0.75845725E+03    0.93651903E+00    0.30369395E-01    0.68000016E+03',&
'       0.70000000E+03    0.30943291E+04    0.82243999E+03    0.99006883E+00    0.36618048E-01    0.70000112E+03',&
'       0.70547000E+03    0.32082348E+04    0.90600741E+03    0.10611600E+01    0.50778529E-01    0.00000000E+00',&
'   0         SUM=  0.6353636479732371E+05                                       ',&
'                                                                                ',&
'                                                                                ',&
'Input file "cygwin_results.txt"                                                 ',&
'                                                                                ',&
'   1TESTS STARTED.                                                              ',&
'                                                                                ',&
'                                                                                ',&
'   1          T,PSL(T),HSL(T),SSL(T),VSL(T),TSLH(H)                             ',&
'   0         PMIN=    0.885891E-01    PMAX=    0.320823E+04    DELP=    0.100000E+03',&
'             TMIN=    0.320000E+02    TMAX=    0.705470E+03    DELT=    0.200000E+02',&
'   0                                                                            ',&
'       0.32000000D+02    0.88589141D-01   -0.17863994D-01   -0.36879562D-04    0.16022057D-01    0.31990327D+02',&
'       0.40000000D+02    0.12163360D+00    0.80272719D+01    0.16194175D-01    0.16018940D-01    0.40003431D+02',&
'       0.60000000D+02    0.25611401D+00    0.28059533D+02    0.55503370D-01    0.16033165D-01    0.60005984D+02',&
'       0.80000000D+02    0.50682853D+00    0.48036541D+02    0.93222577D-01    0.16071928D-01    0.79998151D+02',&
'       0.10000000D+03    0.94923553D+00    0.67998872D+02    0.12954114D+00    0.16129956D-01    0.99994830D+02',&
'       0.12000000D+03    0.16927366D+01    0.87966229D+02    0.16459153D+00    0.16204270D-01    0.11999684D+03',&
'       0.14000000D+03    0.28891787D+01    0.10794963D+03    0.19847772D+00    0.16293096D-01    0.14000080D+03',&
'       0.16000000D+03    0.47413557D+01    0.12795766D+03    0.23128914D+00    0.16395272D-01    0.16000351D+03',&
'       0.18000000D+03    0.75110274D+01    0.14799946D+03    0.26310734D+00    0.16510005D-01    0.18000351D+03',&
'       0.20000000D+03    0.11526035D+02    0.16808606D+03    0.29400880D+00    0.16636808D-01    0.20000116D+03',&
'       0.22000000D+03    0.17186197D+02    0.18823058D+03    0.32406594D+00    0.16775474D-01    0.21999785D+03',&
'       0.24000000D+03    0.24967794D+02    0.20844802D+03    0.35334719D+00    0.16926045D-01    0.23999513D+03',&
'       0.26000000D+03    0.35426601D+02    0.22875486D+03    0.38191689D+00    0.17088786D-01    0.25999384D+03',&
'       0.28000000D+03    0.49199533D+02    0.24916873D+03    0.40983516D+00    0.17264170D-01    0.27999361D+03',&
'       0.30000000D+03    0.67005075D+02    0.26970823D+03    0.43715817D+00    0.17452875D-01    0.29999280D+03',&
'       0.32000000D+03    0.89642735D+02    0.29039297D+03    0.46393855D+00    0.17655789D-01    0.31998899D+03',&
'       0.34000000D+03    0.11799178D+03    0.31124389D+03    0.49022610D+00    0.17874025D-01    0.33998018D+03',&
'       0.36000000D+03    0.15300954D+03    0.33228370D+03    0.51606862D+00    0.18108958D-01    0.35996652D+03',&
'       0.38000000D+03    0.19572950D+03    0.35353750D+03    0.54151277D+00    0.18362259D-01    0.38000205D+03',&
'       0.40000000D+03    0.24725940D+03    0.37509279D+03    0.56667770D+00    0.18637646D-01    0.40003002D+03',&
'       0.42000000D+03    0.30877960D+03    0.39689616D+03    0.59150311D+00    0.18935141D-01    0.41998032D+03',&
'       0.44000000D+03    0.38154169D+03    0.41902722D+03    0.61609004D+00    0.19258996D-01    0.43998928D+03',&
'       0.46000000D+03    0.46686778D+03    0.44153583D+03    0.64049662D+00    0.19613160D-01    0.46000991D+03',&
'       0.48000000D+03    0.56615075D+03    0.46448172D+03    0.66478705D+00    0.20002609D-01    0.48002037D+03',&
'       0.50000000D+03    0.68085599D+03    0.48793750D+03    0.68903400D+00    0.20433708D-01    0.50001633D+03',&
'       0.52000000D+03    0.81252603D+03    0.51199311D+03    0.71332237D+00    0.20914751D-01    0.52000390D+03',&
'       0.54000000D+03    0.96279001D+03    0.53676266D+03    0.73775486D+00    0.21456790D-01    0.53999326D+03',&
'       0.56000000D+03    0.11333816D+04    0.56239502D+03    0.76246065D+00    0.22074947D-01    0.55999280D+03',&
'       0.58000000D+03    0.13261708D+04    0.58909059D+03    0.78760893D+00    0.22790623D-01    0.58000418D+03',&
'       0.60000000D+03    0.15432192D+04    0.61712901D+03    0.81343122D+00    0.23635444D-01    0.60001858D+03',&
'       0.62000000D+03    0.17868695D+04    0.64691984D+03    0.84026231D+00    0.24659110D-01    0.62001710D+03',&
'       0.64000000D+03    0.20598878D+04    0.67911317D+03    0.86863104D+00    0.25947611D-01    0.63998803D+03',&
'       0.66000000D+03    0.23656783D+04    0.71493433D+03    0.89954128D+00    0.27678770D-01    0.66001396D+03',&
'       0.68000000D+03    0.27085898D+04    0.75845725D+03    0.93651903D+00    0.30369395D-01    0.68000016D+03',&
'       0.70000000D+03    0.30943291D+04    0.82243999D+03    0.99006883D+00    0.36618048D-01    0.70000112D+03',&
'       0.70547000D+03    0.32082348D+04    0.90600741D+03    0.10611600D+01    0.50778529D-01    0.00000000D+00',&
'   0         SUM=  0.6353636479675987E+05                                       ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'                                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
!>
!!##NAME
!!  numdiff(1f) - [DEVELOPER] Compare numeric differences in a file
!!  (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  numdiff
!!     -old FILENAME -new FILENAME
!!     [ -percent REAL_VALUE|-digits N|-margin XXX.XX]
!!     [ -verbose]|
!!     [ --help|--version]
!!
!!##DESCRIPTION
!!  NUMDIFF assumes two files are basically identical except for numeric
!!  differences, and finds values whose differences exceed a specified
!!  tolerance.
!!  o file widths are required to be less than 264 characters.
!!  o numbers are assumed to be delimited by spaces, commas, semi-colons,
!!    or vertical line characters (" ,;|"). Adjacent delimiters are ignored.
!!
!!  This program was originally written to simplify the comparison of values
!!  generated by new versions of numeric libraries to previous versions
!!  of the libraries.
!!
!!##OPTIONS
!!     The options are
!!
!!     -old FILENAME
!!          name of file containing template values
!!
!!     -new FILENAME
!!          name of file containing new values
!!
!!     -percent REAL_VALUE
!!              set threshold at which to report values as a percentage of the
!!              template value
!!
!!     -digits N
!!             set threshold at which to report values as a number of digits
!!             if -digits is specified -percent is ignored.
!!
!!     -margin XXX.XX
!!             set threshold to a relative margin of the magnitude of the values
!!
!!     -verbose
!!        shows the lines that pass the criteria from OLDFILE as well.
!!
!!     --help
!!        display this help text and exit
!!
!!     --version
!!        display information on the code version and exit
!!
!!##USAGE
!!  1. GENERATE TEMPLATE:  call all your numeric procedures over their
!!     allowed ranges and print the values to a file on your original system.
!!     Save this file as your master QA template.
!!  2. GENERATE TRIAL DATA:  When you port the procedures to another system or
!!     recompile using a new compiler run your QA program again and save
!!     the second file.
!!  3. TEST/COMPARE:  run the numdiff(1) program:
!!
!!       numdiff -old MASTER_TEMPLATE_FILE -new NEW_OUTPUT_FILE -percent 0.0001
!!
!!##EXAMPLE
!!
!!
!!    We will assume we have two files meeting the above criteria called
!!    "cray_results.txt" and "cygwin_results.txt". To compare the values
!!    we enter
!!
!!      numdiff -old cray_results.txt -new cygwin_results.txt -percent 0.00001
!!
!!    A diff(1) of the following input files would show every line, as one
!!    uses the "E" prefix for exponents, while the other uses "D". Even when a
!!    diff(1) would show few lines, you have to inspect each difference to see
!!    how large a difference in value was found. Using numdiff(1) you can
!!    ignore most insignificant differences.
!!
!!    OUTPUT
!!
!!    Results from the run are
!!
!!      >        | *numdiff* numerical differences
!!      >        | old file=in1
!!      >        | new file=in2
!!      >        | percent threshold =  0.1E-006 (%)
!!      >--------|------------------------------------------------------------------------------------------------------------
!!      >old     |    0.32000000E+02    0.88589141E-01   -0.17863993E-01   -0.36879559E-04    0.16022057E-01    0.31990327E+02
!!      >       8|                                       ###############   ###############
!!      >new     |    0.32000000D+02    0.88589141D-01   -0.17863994D-01   -0.36879562D-04    0.16022057D-01    0.31990327D+02
!!      >>>>>>>>>|maximum difference =.81345875138179356E-005(%)
!!      >--------|------------------------------------------------------------------------------------------------------------
!!
!!    This indicates values did not pass on line 8. "#" characters underline the values.
!!
!!
!! Input file "cray_results.txt"
!!
!!    1TESTS STARTED.
!!
!!
!!    1          T,PSL(T),HSL(T),SSL(T),VSL(T),TSLH(H)
!!    0         PMIN=    0.885891E-01    PMAX=    0.320823E+04    DELP=    0.100000E+03
!!              TMIN=    0.320000E+02    TMAX=    0.705470E+03    DELT=    0.200000E+02
!!    0
!!        0.32000000E+02    0.88589141E-01   -0.17863993E-01   -0.36879559E-04    0.16022057E-01    0.31990327E+02
!!        0.40000000E+02    0.12163360E+00    0.80272719E+01    0.16194175E-01    0.16018940E-01    0.40003431E+02
!!        0.60000000E+02    0.25611401E+00    0.28059533E+02    0.55503370E-01    0.16033165E-01    0.60005984E+02
!!        0.80000000E+02    0.50682853E+00    0.48036541E+02    0.93222577E-01    0.16071928E-01    0.79998151E+02
!!        0.10000000E+03    0.94923553E+00    0.67998872E+02    0.12954114E+00    0.16129956E-01    0.99994830E+02
!!        0.12000000E+03    0.16927366E+01    0.87966229E+02    0.16459153E+00    0.16204270E-01    0.11999684E+03
!!        0.14000000E+03    0.28891787E+01    0.10794963E+03    0.19847772E+00    0.16293096E-01    0.14000080E+03
!!        0.16000000E+03    0.47413557E+01    0.12795766E+03    0.23128914E+00    0.16395272E-01    0.16000351E+03
!!        0.18000000E+03    0.75110274E+01    0.14799946E+03    0.26310734E+00    0.16510005E-01    0.18000351E+03
!!        0.20000000E+03    0.11526035E+02    0.16808606E+03    0.29400880E+00    0.16636808E-01    0.20000116E+03
!!        0.22000000E+03    0.17186197E+02    0.18823058E+03    0.32406594E+00    0.16775474E-01    0.21999785E+03
!!        0.24000000E+03    0.24967794E+02    0.20844802E+03    0.35334719E+00    0.16926045E-01    0.23999513E+03
!!        0.26000000E+03    0.35426601E+02    0.22875486E+03    0.38191689E+00    0.17088786E-01    0.25999384E+03
!!        0.28000000E+03    0.49199533E+02    0.24916873E+03    0.40983516E+00    0.17264170E-01    0.27999361E+03
!!        0.30000000E+03    0.67005075E+02    0.26970823E+03    0.43715817E+00    0.17452875E-01    0.29999280E+03
!!        0.32000000E+03    0.89642735E+02    0.29039297E+03    0.46393855E+00    0.17655789E-01    0.31998899E+03
!!        0.34000000E+03    0.11799178E+03    0.31124389E+03    0.49022610E+00    0.17874025E-01    0.33998018E+03
!!        0.36000000E+03    0.15300954E+03    0.33228370E+03    0.51606862E+00    0.18108958E-01    0.35996652E+03
!!        0.38000000E+03    0.19572950E+03    0.35353750E+03    0.54151277E+00    0.18362259E-01    0.38000205E+03
!!        0.40000000E+03    0.24725940E+03    0.37509279E+03    0.56667770E+00    0.18637646E-01    0.40003002E+03
!!        0.42000000E+03    0.30877960E+03    0.39689616E+03    0.59150311E+00    0.18935141E-01    0.41998032E+03
!!        0.44000000E+03    0.38154169E+03    0.41902722E+03    0.61609004E+00    0.19258996E-01    0.43998928E+03
!!        0.46000000E+03    0.46686778E+03    0.44153583E+03    0.64049662E+00    0.19613160E-01    0.46000991E+03
!!        0.48000000E+03    0.56615075E+03    0.46448172E+03    0.66478705E+00    0.20002609E-01    0.48002037E+03
!!        0.50000000E+03    0.68085599E+03    0.48793750E+03    0.68903400E+00    0.20433708E-01    0.50001633E+03
!!        0.52000000E+03    0.81252603E+03    0.51199311E+03    0.71332237E+00    0.20914751E-01    0.52000390E+03
!!        0.54000000E+03    0.96279001E+03    0.53676266E+03    0.73775486E+00    0.21456790E-01    0.53999326E+03
!!        0.56000000E+03    0.11333816E+04    0.56239502E+03    0.76246065E+00    0.22074947E-01    0.55999280E+03
!!        0.58000000E+03    0.13261708E+04    0.58909059E+03    0.78760893E+00    0.22790623E-01    0.58000418E+03
!!        0.60000000E+03    0.15432192E+04    0.61712901E+03    0.81343122E+00    0.23635444E-01    0.60001858E+03
!!        0.62000000E+03    0.17868695E+04    0.64691984E+03    0.84026231E+00    0.24659110E-01    0.62001710E+03
!!        0.64000000E+03    0.20598878E+04    0.67911317E+03    0.86863104E+00    0.25947611E-01    0.63998803E+03
!!        0.66000000E+03    0.23656783E+04    0.71493433E+03    0.89954128E+00    0.27678770E-01    0.66001396E+03
!!        0.68000000E+03    0.27085898E+04    0.75845725E+03    0.93651903E+00    0.30369395E-01    0.68000016E+03
!!        0.70000000E+03    0.30943291E+04    0.82243999E+03    0.99006883E+00    0.36618048E-01    0.70000112E+03
!!        0.70547000E+03    0.32082348E+04    0.90600741E+03    0.10611600E+01    0.50778529E-01    0.00000000E+00
!!    0         SUM=  0.6353636479732371E+05
!!
!!
!! Input file "cygwin_results.txt"
!!
!!    1TESTS STARTED.
!!
!!
!!    1          T,PSL(T),HSL(T),SSL(T),VSL(T),TSLH(H)
!!    0         PMIN=    0.885891E-01    PMAX=    0.320823E+04    DELP=    0.100000E+03
!!              TMIN=    0.320000E+02    TMAX=    0.705470E+03    DELT=    0.200000E+02
!!    0
!!        0.32000000D+02    0.88589141D-01   -0.17863994D-01   -0.36879562D-04    0.16022057D-01    0.31990327D+02
!!        0.40000000D+02    0.12163360D+00    0.80272719D+01    0.16194175D-01    0.16018940D-01    0.40003431D+02
!!        0.60000000D+02    0.25611401D+00    0.28059533D+02    0.55503370D-01    0.16033165D-01    0.60005984D+02
!!        0.80000000D+02    0.50682853D+00    0.48036541D+02    0.93222577D-01    0.16071928D-01    0.79998151D+02
!!        0.10000000D+03    0.94923553D+00    0.67998872D+02    0.12954114D+00    0.16129956D-01    0.99994830D+02
!!        0.12000000D+03    0.16927366D+01    0.87966229D+02    0.16459153D+00    0.16204270D-01    0.11999684D+03
!!        0.14000000D+03    0.28891787D+01    0.10794963D+03    0.19847772D+00    0.16293096D-01    0.14000080D+03
!!        0.16000000D+03    0.47413557D+01    0.12795766D+03    0.23128914D+00    0.16395272D-01    0.16000351D+03
!!        0.18000000D+03    0.75110274D+01    0.14799946D+03    0.26310734D+00    0.16510005D-01    0.18000351D+03
!!        0.20000000D+03    0.11526035D+02    0.16808606D+03    0.29400880D+00    0.16636808D-01    0.20000116D+03
!!        0.22000000D+03    0.17186197D+02    0.18823058D+03    0.32406594D+00    0.16775474D-01    0.21999785D+03
!!        0.24000000D+03    0.24967794D+02    0.20844802D+03    0.35334719D+00    0.16926045D-01    0.23999513D+03
!!        0.26000000D+03    0.35426601D+02    0.22875486D+03    0.38191689D+00    0.17088786D-01    0.25999384D+03
!!        0.28000000D+03    0.49199533D+02    0.24916873D+03    0.40983516D+00    0.17264170D-01    0.27999361D+03
!!        0.30000000D+03    0.67005075D+02    0.26970823D+03    0.43715817D+00    0.17452875D-01    0.29999280D+03
!!        0.32000000D+03    0.89642735D+02    0.29039297D+03    0.46393855D+00    0.17655789D-01    0.31998899D+03
!!        0.34000000D+03    0.11799178D+03    0.31124389D+03    0.49022610D+00    0.17874025D-01    0.33998018D+03
!!        0.36000000D+03    0.15300954D+03    0.33228370D+03    0.51606862D+00    0.18108958D-01    0.35996652D+03
!!        0.38000000D+03    0.19572950D+03    0.35353750D+03    0.54151277D+00    0.18362259D-01    0.38000205D+03
!!        0.40000000D+03    0.24725940D+03    0.37509279D+03    0.56667770D+00    0.18637646D-01    0.40003002D+03
!!        0.42000000D+03    0.30877960D+03    0.39689616D+03    0.59150311D+00    0.18935141D-01    0.41998032D+03
!!        0.44000000D+03    0.38154169D+03    0.41902722D+03    0.61609004D+00    0.19258996D-01    0.43998928D+03
!!        0.46000000D+03    0.46686778D+03    0.44153583D+03    0.64049662D+00    0.19613160D-01    0.46000991D+03
!!        0.48000000D+03    0.56615075D+03    0.46448172D+03    0.66478705D+00    0.20002609D-01    0.48002037D+03
!!        0.50000000D+03    0.68085599D+03    0.48793750D+03    0.68903400D+00    0.20433708D-01    0.50001633D+03
!!        0.52000000D+03    0.81252603D+03    0.51199311D+03    0.71332237D+00    0.20914751D-01    0.52000390D+03
!!        0.54000000D+03    0.96279001D+03    0.53676266D+03    0.73775486D+00    0.21456790D-01    0.53999326D+03
!!        0.56000000D+03    0.11333816D+04    0.56239502D+03    0.76246065D+00    0.22074947D-01    0.55999280D+03
!!        0.58000000D+03    0.13261708D+04    0.58909059D+03    0.78760893D+00    0.22790623D-01    0.58000418D+03
!!        0.60000000D+03    0.15432192D+04    0.61712901D+03    0.81343122D+00    0.23635444D-01    0.60001858D+03
!!        0.62000000D+03    0.17868695D+04    0.64691984D+03    0.84026231D+00    0.24659110D-01    0.62001710D+03
!!        0.64000000D+03    0.20598878D+04    0.67911317D+03    0.86863104D+00    0.25947611D-01    0.63998803D+03
!!        0.66000000D+03    0.23656783D+04    0.71493433D+03    0.89954128D+00    0.27678770D-01    0.66001396D+03
!!        0.68000000D+03    0.27085898D+04    0.75845725D+03    0.93651903D+00    0.30369395D-01    0.68000016D+03
!!        0.70000000D+03    0.30943291D+04    0.82243999D+03    0.99006883D+00    0.36618048D-01    0.70000112D+03
!!        0.70547000D+03    0.32082348D+04    0.90600741D+03    0.10611600D+01    0.50778529D-01    0.00000000D+00
!!    0         SUM=  0.6353636479675987E+05
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>       ',&
'@(#)PROGRAM:        numdiff(1f)>                                                ',&
'@(#)DESCRIPTION:    compare otherwise essentially identical files for numeric differences.>',&
'@(#)VERSION:        3.0, 20131201>                                              ',&
'@(#)AUTHOR:         John S. Urban>                                              ',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>                       ',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>             ',&
'@(#)COPYRIGHT:      1985, 1986, 1989, 1990, 20090501, 20131129 John. S. Urban>  ',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'                    There is NO WARRANTY, to the extent permitted by law.>      ',&
'']
end subroutine setup
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
end program numdiff
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
 
