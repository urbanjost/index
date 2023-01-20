# block comments in Fortran

Fortran does not support block comments. 

## Editor support of block text

Some editors can support editing block comment
sections in Fortran, such as _emacs_(1) and _vim_(1).

## Using pre-processors

In general, a pre-processor can be used to provide support for
documentation being combined with source code. For example, the commonly
available _fpp_(1) or _cpp_(1) commands can be used If the file *source.F90*
contains

     #ifdef DOCUMENT

	This is a block of text that 
	can be used as documentation

     #else 

     program demo
	write(*,*)'Hello world'
     end program demo 

     #endif

Then the _cpp_(1) command can be used to extract the comments

     # extract text block info file source.txt 
     cpp -DDOCUMENT -P -C -traditional source.F90 >source.txt
     # compile code skipping text block.
     f90 source.F90

Unfortunately, the text block can be placed in a seperate file, but
will then not appear in the source file.  The much more powerful _m4_(1)
pre-processor can be used to maintain code and documentation in the
same file more flexibly, but has a steeper learning curve than _fpp_(1)
or _cpp_(1).

[ufpp](http://www.urbanjost.altervista.org/LIBRARY/libGPF/GPF.html)
is a Fortran pre-processor included in the GPF (General Purpose Fortran)
repository that supports several types of block text options to support
generating man(1) pages as well as documented code. For example, the following
input file

     $!==============================================================================
     $BLOCK COMMENT -file example.3.man
     NAME
        example(3f) - [FORTRAN] subroutine example using ufpp(1)
     SYNOPSIS
        subroutine example()
     DESCRIPTION
        This is an example program built using several of the modes
        of the ufpp(1) $DOCUMENT directive. This section will become
        comments in the code, and optionally also be written to the file
        "$UFPP_DOCUMENT_DIR/example.3.man" if the environment variable
        $UFPP_DOCUMENT_DIR is set.
        
        In this case, the data could easily be processed with txt2man(1)
        and made into an automatic man(1) page.
     
        Other formats often used here are "markdown" documents, Tex, HTML,
        and *roff files.
     
     EXAMPLE
     $BLOCK
     $!==============================================================================
     program testit
     implicit none
     integer :: i,io=6
     $!==============================================================================
     $BLOCK WRITE !  These will become write statements to unit IO
     hello world!
       hello world,again!
         hello world, once more!
     $BLOCK
     end program testit
     $!==============================================================================

would generate the following code, and optionally generate a seperate file
with the help text in it.

    ! NAME
    !    example(3f) - [FORTRAN] subroutine example using ufpp(1)
    ! SYNOPSIS
    !    subroutine example()
    ! DESCRIPTION
    !    This is an example program built using several of the modes
    !    of the ufpp(1) $DOCUMENT directive. This section will become
    !    comments in the code, and optionally also be written to the file
    !    "$UFPP_DOCUMENT_DIR/example.3.man" if the environment variable
    !    $UFPP_DOCUMENT_DIR is set.
    ! 
    !    In this case, the data could easily be processed with txt2man(1)
    !    and made into an automatic man(1) page.
    ! 
    !    Other formats often used here are "markdown" documents, Tex, HTML,
    !    and *roff files.
    ! 
    ! EXAMPLE
    !============================================================================
    program testit
    implicit none
    integer :: i,io=6
    write(io,'(a)')'hello world!'
    write(io,'(a)')'  hello world,again!'
    write(io,'(a)')'    hello world, once more!'
    end program testit
