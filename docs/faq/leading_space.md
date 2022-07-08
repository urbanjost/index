#LEADING SPACE

  If you find it annoying that list-directed output lines
  always start with a space, but that when you use a 
  format statement output starts with column 1,

  1. add "1x" to the beginning of all your format
     statements so they line up with the list-directed output
  1. come very close to list-directed output format
     by using the G descriptor. For example:

      character(len=*),parameter :: any = '(*(g0,1x))' ! a space between entries
      character(len=*),parameter :: g =   "(*(g0))"    ! no spaces between items
      i=10
      print g, "i = ", i  
      write(*,any)10,1234.567,9876543210.123456789d0,.true.,(123.456,789.012),'Gee!'
      end

      i = 10
      10 1234.56702 9876543210.1234570 T 123.456001 789.012024 Gee!

