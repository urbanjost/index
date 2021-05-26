module index
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, index!"
  end subroutine say_hello
end module index
