# Short-circuit of compound Boolean expressions

Short-circuit evaluation (aka. minimal evaluation or McCarthy evaluation)
is when the remaining arguments in a Boolean operation are executed or
evaluated only if the previous operations are true. Fortran does not
provide short-circuit evaluations. This allows for optimizations such as
parallel evaluation. Some compilers do provide an extension to provide
short-circuit evaluation but to keep your code standard-conforming you
should assume all sections of an expression may or may not be evaluated
and executed, including function calls in the expression.

The solution is to use nested conditionals. For example:

    val=0.0
    if(val.ne.0 .and. (top/val.gt.10.0) )then ! DO NOT DO THIS
       write(*,*)'met conditions'
    endif

Should be written as

    val=0.0
    if(val.ne.0 )then
       if (top/val.gt.10.0) )then
          write(*,*)'met conditions'
       endif
    endif

So any time you have a compound Boolean expression (anywhere, not just in
an IF(3f) expression) make sure all subexpressions are safe to evaluate
or change the expression to conditionally executed sections.

# Common errors ...

    IF ( INDX .GT.0 .AND. ARRAY(INDX) .EQ. 'xxx' ) THEN ...

If INDX can be out of range for array ARRAY this could cause such
problems as getting a run-time out-of-bounds error. This is true
not just in IF statements but anywhere a compound Boolean expression
is used ...
```fortran
    MERGE (A(ii), 0, ii .GE. 1)
```
# By the way ...

I have seen several proposals to add lazy or short-circuit syntax for
expressions. My favorite was one that looked like this ...
```fortran
    IF (i>0) AND (arr(i) > 20) OR (( X>0 ) AND (SQRT(X) > 3)) THEN ...
```
         


