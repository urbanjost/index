program whatisthediff
implicit none
real :: DUM
real :: DUMA(1)
equivalence(DUM,DUMA)
   DUMA=-9999999.0
   DUM=10.0
   call addarr(DUMA)

   !!call addarr(DUM) <-- this is illegal because passing scalar to array

   write(*,*)DUM,DUMA !! ,arr 
contains
subroutine addarr(arr)
real,intent(inout) :: arr(:)
   arr=arr+10
end subroutine addarr
end program whatisthediff
