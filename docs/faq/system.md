#How to issue a command to the operating system

To execute a shell command use the f2008 procedure

       call execute_command_line(command [, wait, exitstat, cmdstat, cmdmsg ])

If it is not available, most Fortran compilers support an extension called system(3f).

##DESCRIPTION

The COMMAND argument is passed to the shell and executed.  (The shell
is generally sh(1) on Unix systems, and cmd.exe on Windows.) If **WAIT**
is present and has the value false, the execution of the command is
asynchronous if the system supports it; otherwise, the command is
executed synchronously.

The three last arguments allow the user to get status information.
After synchronous execution, **EXITSTAT** contains the integer exit code of
the command, as returned by **SYSTEM**. **CMDSTAT** is set to zero if the command
line was executed (whatever its exit status was). **CMDMSG** is assigned an
error message if an error has occurred.

Note that the system call need not be thread-safe. It is the
responsibility of the user to ensure that the system is not called
concurrently if required.

When the command is executed synchronously, **EXECUTE\_COMMAND\_LINE**(3f)
returns after the command line has completed execution. Otherwise,
**EXECUTE\_COMMAND\_LINE**(3f) returns without waiting.

##ARGUMENTS

###COMMAND
  a default **CHARACTER** scalar conntaining the command line to be executed. The interpretation is programming-
  environment dependent.

###WAIT
  (Optional) a default **LOGICAL** scalar.  If **WAIT** is present with the value .false., and the processor supports
  asynchronous execution of the command, the command is executed asynchronously; otherwise it is executed
  synchronously.

###EXITSTAT
  (Optional) an **INTEGER** of the default kind with intent(INOUT).  If the command is executed synchronously, it is
  assigned the value of the processor-dependent exit status. Otherwise, the value of **EXITSTAT** is unchanged.

###CMDSTAT
  (Optional) an **INTEGER** of  default kind with intent(INOUT).  If an error condition occurs and **CMDSTAT** is not
  present, error termination of execution of the image is initiated.

  It is assigned the value -1 if the processor does not support command line execution, a processor-dependent
  positive value if an error condition occurs, or the value -2 if no error condition occurs but **WAIT** is present with
  the value false and the processor does not support asynchronous execution. Otherwise it is assigned the value 0.

###CMDMSG
  (Optional) a **CHARACTER** scalar of the default kind.  It is an **INTENT (INOUT)** argument. If an error condition
  occurs, it is assigned a processor-dependent explanatory message. Otherwise, it is unchanged.

##EXAMPLE

Sample program:

    program test_exec
    implicit none
    integer :: i,istat
    character(len=1024) :: message

       message=''
       istat=0
       call execute_command_line("ls -l", exitstat=i,cmdmsg=message,cmdstat=istat)
       print *, "Exit status of 'ls -l' was ", i
       if(istat.ne.0)then
          print *, 'status=',istat
          print *, trim(message)
       endif

       !!call execute_command_line("reindex_files.exe", wait=.false.)
       !!print *, "Now reindexing files in the background"

    end program test_exec

###NOTE

Because this intrinsic is making a system call, it is very system
dependent. Its behavior with respect to signaling is processor
dependent. In particular, on POSIX-compliant systems, the SIGINT and
SIGQUIT signals will be ignored, and the SIGCHLD will be blocked. As
such, if the parent process is terminated, the child process might not
be terminated alongside.

#Calling a process

A module called M\_process(3f) that uses the **ISO\_C\_BINDING**(3f) to call **POSIX** C routines
to spawn a process and either write data to the stdin of the process or
read the stdout of the process can be found in the [**GPF**](../../GPF.html) (General Purpose Fortran) collection.

##EXAMPLE

Following is an example program that calls the M\_process module to start a plotting
program called gnuplot(1) and give it enough commands to generate
a plot. It then lets you interactively interact with the program or
continue on in the program.

     program gnuExample
        ! @(#)  Example of Fortran writing GNUPLOT command and data file.
        use M_process ,only: process_open_write, process_writeline
        use M_process ,only: streampointer, process_close
        implicit none
        !*! line of data to write (assumed long enough to hold any command line)
        character(len=4096) :: line
        !*! C file pointer returned by process_open()
        type(streampointer) :: fp
        !*! check status of calls to process module routines
        integer :: ierr
        !*! DO loop counter
        integer :: i
     
        !*! data file to create and put X,Y values into
        character(len=60)   :: fidmap='SAMPLE.MAP'
        !*! number of points to put into curve to be plotted
        integer,parameter   :: n=50
        !*! arrays to fill with curve data to be plotted
        real                :: x(n),y(n)
        integer             :: ios
     
        !*! open data file to hold X,Y values to be plotted
        open(70,file=fidmap)
        !*! Define sample X,Y array.
        do i=1,n
           !*! set X() values as whole numbers 1 to N
           x(i)=i
           !*!
           y(i)=(x(i)+0.5)**2
        enddo
     
        !*! Write the X,Y array as coordinates to be plotted.
        write(70,'(2f10.3)')(x(i),y(i),i=1,n)
        !*! if not closed or flushed, subprocess may not be able to read
        flush(70, iostat = ios)
     
        !*! Write the GnuPlot commands
        !*! open process to write to (ie. start gnuplot(1) program)
        call process_open_write('gnuplot',fp,ierr)
        call process_writeline &
        & ('set title " Example of GNUPlot data and command file generation"',fp,ierr)
        !*! write a command to the process
        call process_writeline('set nokey',fp,ierr)
     
        !*! build a command with a formatted write
        write(line,101) trim(fidmap)
     101 format('plot "',A,'" with lines')
        !*! write a command to the process
        call process_writeline(trim(line),fp,ierr)
     
        !*! Additional gnuplot commands; in this case interactively entered
        write(*,'(a)')'enter gnuplot commands or "." to exit'
        do
           write(*,'(a)',advance='no')'gnu>>'
           read(*,'(a)',iostat=ios)line
           if(line.eq.'.')exit
           call process_writeline(trim(line),fp,ierr)
        enddo
     
        !*! Wrap up
        call process_close(fp,ierr)
        write(*,*)'CLOSED THE PROCESS. RETURNING TO PROGRAM'
        !*! delete the data file
        close(70,status='delete',iostat=ios)
     end program gnuExample
