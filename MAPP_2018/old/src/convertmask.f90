PROGRAM convertmask

!convert mask to big endian and to r_kind
!this code is only necessary for older files which were regular real, now r_kind/bin_endian

  USE kinds, only: i_kind,r_kind

  IMPLICIT NONE

  INTEGER :: i,j

  INTEGER :: nx,ny
  INTEGER(i_kind) :: nx_kind,ny_kind

  REAL, ALLOCATABLE, DIMENSION(:,:) :: dsmask
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:) :: dsmask_kind

  CHARACTER(len=250) :: filebin_in,filebin_out,filename
  
  INTEGER :: iargc
  INTEGER :: inunit=101,outunit=102

  IF (iargc() < 2) THEN
     PRINT *,"needs input and output files"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,filebin_in)
  CALL getarg(2,filebin_out)


  filename=filebin_in

  OPEN(unit=inunit,file=filename,form="unformatted")
  READ(inunit)nx,ny
  ALLOCATE(dsmask(nx,ny))
  READ(inunit)
  READ(inunit)dsmask
  CLOSE(inunit)

  nx_kind=nx
  ny_kind=ny
  ALLOCATE(dsmask_kind(nx_kind,ny_kind))
  dsmask_kind=dsmask

  filename=filebin_out

  OPEN(unit=outunit,file=filename,form='unformatted',convert='big_endian')
  WRITE(outunit)nx_kind,ny_kind
  WRITE(outunit)dsmask_kind
  CLOSE(outunit)
     
  DEALLOCATE(dsmask,dsmask_kind)

END PROGRAM convertmask

