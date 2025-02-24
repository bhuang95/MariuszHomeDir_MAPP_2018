PROGRAM aod2ncdf

!MZP, Oct 2016
!to add aod to netcdf file
!may later re-wrok for other times/channels

  USE netcdf_io
  USE kinds, ONLY: i_kind,r_kind

  IMPLICIT NONE

  INTEGER, PARAMETER :: nvars=1
  INTEGER :: i,j,ivar,vl,it
  CHARACTER(len=25) :: varname = "AOD"

  INTEGER :: nx,ny,nt

  INTEGER(i_kind) :: nx_kind,ny_kind,nt_kind
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:) :: aod_kind
  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: aod
  CHARACTER(len=20), DIMENSION(3)                   :: dimstring
  INTEGER,           DIMENSION(3)                   :: dims
  CHARACTER(len=25)                                 :: varstringname
  
  CHARACTER(len=250) :: filenc_inout,filebin_in,filename
  
  CHARACTER(len=2) :: ctime,cit


  INTEGER :: iargc
  INTEGER :: inunit=101

  IF (iargc() < 2) THEN
     PRINT *,"needs two input files"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,filebin_in)
  CALL getarg(2,filenc_inout)

  nt=1

  filename=TRIM(filebin_in)
  
  OPEN(unit=inunit,file=filename,form="unformatted")
  READ(inunit)nx_kind,ny_kind
  nx=nx_kind
  ny=ny_kind
  ALLOCATE(aod_kind(nx,ny,nt),aod(nx,ny,nt))
  READ(inunit)aod_kind
  CLOSE(inunit)

  aod=aod_kind

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "Time"

  filename=filenc_inout

  CALL netcdfdimension(filename,3,dimstring,dims)  

  IF (nx /= dims(1) .OR. ny /= dims(2) .OR. nt /= dims(3)) THEN
     PRINT *,'Size mismatch in nx,ny,nt'
     PRINT *,nx,ny,nt
     PRINT *,dims(1),dims(2),dims(3)
     PRINT *,'Stopping'
     STOP
  ENDIF

  PRINT *,nx,ny,nt
  PRINT *,MINVAL(aod),MAXVAL(aod)

  varstringname=varname
  
  CALL writenetcdfdata3(filename,aod,varstringname,nx,ny,nt)
     
  DEALLOCATE(aod,aod_kind)

END PROGRAM aod2ncdf

