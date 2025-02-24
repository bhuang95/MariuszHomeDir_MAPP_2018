
program chdir_test
  use chdir_mod, only: f90chdir
  implicit none
  integer :: ierr

  call f90chdir('/usr', ierr)
  if(ierr/=0) write(0,*) 'bad! no /usr'

  call f90chdir('/dev', ierr)
  if(ierr/=0) write(0,*) 'bad! no /dev'

  call f90chdir('/usr/lib', ierr)
  if(ierr/=0) write(0,*) 'bad! no /usr/lib'

  call f90chdir('/invalid/directory', ierr)
  if(ierr==0) write(0,*) 'bad! got into /invalid/directory'
end program chdir_test
