
A useful strategy to avoid allocatable variables is to calculate (or
read) size(s) or length(s) of variable(s), and then declare the
variable(s) in a BLOCK construct as automatic variables. E.g.,

  read ( *, * ) M, N
    block
    character(m) :: C
    real :: A(4*n,4*n), B(n)
   ....
    endblock
