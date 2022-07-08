#special values (ie. Nan, Infinity, ...)

In general you are probably trying to avoid generating infinite values
and NaNs (Not a Number) ; but there are now facilities for handing
them in a standard fashion. Partly because of this you should consider
making your procedures handle a Nan or Infinity appropriately  if they
are likely to be used in a number of applications.

When available, The best alternative is to use the IEEE_ARITHMETIC module.
The IEEE interface lets you test for or set speical values.

There are also a few examples included that might be useful in
environments that do not support the IEEE_ARITHMETIC module; so test
and use them carefully.

#INFINITY


    program demo_inf
    use ieee_arithmetic ! If your compiler supports ISO TR 15580 IEEE Arithmetic !use procedures from ieee_* modules.
    implicit none
    
    block IEEE
    real :: ieee_support_inf
    if (ieee_support_inf(0.0)) then
       inf_ieee = ieee_value(0.0,ieee_negative_inf)
    endif
       write(*,*)'IEEE method ',inf_ieee
    endblock IEEE
    
    ! Arguably, the next method to try to define an infinity is
    block BYREAD
    real :: inf_by_read
    character(len=8),save :: infinity='infinity'
       read(infinity,*)inf_by_read
       write(*,*)'By read ',inf_by_read
    endblock BYREAD
    
    ! divide by zero may work
    block DIVIDE
    real :: inf_divide_by_zero
       inf_divide_by_zero=1.0/0.0
       write(*,*)'Divide by zero is ',inf_divide_by_zero
    end block DIVIDE
    
    ! or if you know the size in bytes of your REAL value
    ! you can try
    
      write(*,*)inf_by_equivalence()
    ! or maybe 
      write(*,*)inf_inf_log()
    ! or
      write(*,*)inf_huge()
    
    ! so if you can generate infinite values what do these produce?
       inf=inf_ieee
       write(*,*)inf
       write(*,*)inf/huge(0.0)
       write(*,*)inf/inf
       write(*,*)2*inf/inf
       write(*,*)0.0/inf
       write(*,*)inf/0.0
       write(*,*)inf.eq.inf
    !===============================================================================
    contains
    !===============================================================================
    function inf_by_equivalence()
    integer,save :: infi
    real,save    :: inf
    equivalence (inf,infi) !stores two variable at the same address
    data inf/z'7f800000'/ !Hex for +Infinity
       inf_by_equivalence=inf
    end function inf_by_equivalence
    !===============================================================================
    real function inf_huge()
    real :: x
       x = huge(1.0)
       inf_huge = x + x
     end function inf_huge
    !===============================================================================
    real function inf_log()
    real :: x
    ! a nice mathematical way of reaching infinity as -log(0) that works nicely for complex variables
    ! will work on IEEE compliant machines, if you do not enable FPE trapping (but then you probably 
    ! are not working with infinities).
       x=0.0
       inf_log=-log(x)
    end function inf_log
    !===============================================================================
    end program demo_inf
    !===============================================================================

#How to tell if a value is a NaN?
