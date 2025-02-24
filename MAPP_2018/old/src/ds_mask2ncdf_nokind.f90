PROGRAM ds_mask2ncdf

!to add ds mask to netcdf file

  USE netcdf_io

  IMPLICIT NONE

  REAL :: small
  INTEGER, PARAMETER :: nvars=1
  INTEGER :: i,j,ivar,vl
  CHARACTER(len=10) :: varname = "DSMASK"

  INTEGER :: nx,ny,nt

  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: dsmask
  CHARACTER(len=20), DIMENSION(3)                   :: dimstring
  INTEGER,           DIMENSION(3)                   :: dims
  CHARACTER(len=25)                                 :: varstringname

  CHARACTER(len=250) :: filenc_inout,filebin_in,filename
  
  INTEGER :: iargc
  INTEGER :: inunit=101

  small=TINY(1.)

  IF (iargc() < 2) THEN
     PRINT *,"needs two input files"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,filebin_in)
  CALL getarg(2,filenc_inout)

  nt=1

  filename=filebin_in

  OPEN(unit=inunit,file=filename,form="unformatted")
  READ(inunit)nx,ny
  ALLOCATE(dsmask(nx,ny,nt))
  READ(inunit)
  READ(inunit)dsmask
  CLOSE(inunit)

!  DO i=1,nx
!     DO j=1,ny
!        IF (dsmask(i,j,nt) > 0) PRINT *,i,j,dsmask(i,j,nt)
!     ENDDO
!  ENDDO

!  stop

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "Time"

  filename=filenc_inout

  CALL netcdfdimension(filename,3,dimstring,dims)  

  IF (nx /= dims(1) .OR. ny /= dims(2) .OR. nt /= dims(3)) THEN
     PRINT *,'Size mismatch in nx,ny'
     PRINT *,nx,ny,nt
     PRINT *,dims(1),dims(2),dims(3)
     PRINT *,'Stopping'
     STOP
  ENDIF

  PRINT *,nx,ny,nt
  PRINT *,MINVAL(dsmask),MAXVAL(dsmask)

  varstringname=varname

  CALL writenetcdfdata3(filename,dsmask,varstringname,nx,ny,nt)
     
  DEALLOCATE(dsmask)

END PROGRAM ds_mask2ncdf

