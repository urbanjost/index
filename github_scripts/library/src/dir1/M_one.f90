module M_one
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64 
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
interface proc1
module procedure proc_i8
module procedure proc_i16
module procedure proc_i32
module procedure proc_i64
module procedure proc_r32
module procedure proc_r64
module procedure proc_r128
module procedure proc_c
end interface proc1
contains

function proc_i8(in) result (out)
   integer(kind=int8)             :: out
   integer(kind=int8),intent(in)  :: in
   out=in
end function proc_i8


function proc_i16(in) result (out)
   integer(kind=int16)             :: out
   integer(kind=int16),intent(in)  :: in
   out=in
end function proc_i16


function proc_i32(in) result (out)
   integer(kind=int32)             :: out
   integer(kind=int32),intent(in)  :: in
   out=in
end function proc_i32


function proc_i64(in) result (out)
   integer(kind=int64)             :: out
   integer(kind=int64),intent(in)  :: in
   out=in
end function proc_i64


function proc_r32(in) result (out)
   real(kind=real32)             :: out
   real(kind=real32),intent(in)  :: in
   out=in
end function proc_r32


function proc_r64(in) result (out)
   real(kind=real64)             :: out
   real(kind=real64),intent(in)  :: in
   out=in
end function proc_r64


function proc_r128(in) result (out)
   real(kind=real128)             :: out
   real(kind=real128),intent(in)  :: in
   out=in
end function proc_r128


function proc_c(in) result (out)
   complex             :: out
   complex,intent(in)  :: in
   out=in
end function proc_c

end module M_one

program testit
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64 
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
use M_one, only : proc1
write(*,*)proc1(10_int8)
write(*,*)proc1(10_int16)
write(*,*)proc1(10_int32)
write(*,*)proc1(10_int64)
write(*,*)proc1(10.0_real32)
write(*,*)proc1(10.0_real64)
write(*,*)proc1(10.0_real128)
end program testit
