module chdir_mod
  ! A convenient wrapper around chdir.
  ! Sam Trahan, Dec 2023
  implicit none
  private
  public :: f90chdir

  interface
    function chdir(path) bind(C)
      ! Interface to the POSIX chdir function. This requires a C string.
      ! The f90chdir constructs a temporary C string for this function.
      use iso_c_binding
      implicit none
      integer(kind=c_int) :: chdir
      type(c_ptr), value :: path
    end function chdir
  end interface

contains

  subroutine f90chdir(path, ierr)
    ! Changes the working directory to the specified path.
    ! ierr is 0 on success or -1 on failure
    ! This is a wrapper around the POSIX chdir function.
    use iso_c_binding
    character(len=*), intent(in) :: path
    integer, intent(out) :: ierr
    character(:), pointer :: array_path(:)
    type(c_ptr) :: c_path
    integer(c_int) :: c_ierr
    integer :: slen, i

    slen = len_trim(path)
    allocate(character(len=1) :: array_path(slen+1))
    do i=1,slen
      array_path(i) = path(i:i)
    enddo
    array_path(slen+1) = c_null_char
    c_path = c_loc(array_path)
    c_ierr = chdir(c_path)
    ierr = int(c_ierr)
    deallocate(array_path)
  end subroutine f90chdir

end module chdir_mod
