PROGRAM aod2ncdf

!MZP, Oct 2016
!MZP, Sept 2018
!to add aod to netcdf file
!to add time and multichannel AOD


  USE netcdf_io
  USE kinds, ONLY: i_kind,r_kind

  IMPLICIT NONE

  INTEGER, PARAMETER :: nvars=1,ndims=4,nchan_nnr=6
  INTEGER :: i,j,ivar,it
  CHARACTER(len=25) :: varname

  REAL(r_kind), DIMENSION(nchan_nnr), PARAMETER :: nnr_channels=&
       &[440_r_kind,470_r_kind,500_r_kind,550_r_kind,660_r_kind,870_r_kind]

  REAL(r_kind), ALLOCATABLE, DIMENSION(:) :: channels

  INTEGER :: nx,ny,nch,nt

  INTEGER(i_kind) :: nx_kind,ny_kind,nch_kind,nt_kind
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:,:) :: aod_kind
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: aod
  REAL, ALLOCATABLE, DIMENSION(:) :: time
  CHARACTER(len=20), DIMENSION(ndims)                   :: dimstring
  INTEGER,           DIMENSION(ndims)                   :: dims
  CHARACTER(len=25)                                 :: varstringname
  
  CHARACTER(len=250) :: filenc_inout,filebin_in,filename
  
  CHARACTER(len=3) :: cfhr
  INTEGER :: fhr


  INTEGER :: iargc
  INTEGER :: inunit=101

  IF (iargc() < 3) THEN
     PRINT *,"needs fcst hour and two input files"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,cfhr)
  CALL getarg(2,filebin_in)
  CALL getarg(3,filenc_inout)

  READ(cfhr,'(i3)')fhr

  nt=1

  filename=filebin_in

  OPEN(unit=inunit,file=filename,form="unformatted")
  READ(inunit)nx_kind,ny_kind,nch_kind
  nx=nx_kind
  ny=ny_kind
  nch=nch_kind
  ALLOCATE(aod_kind(nx,ny,nch,nt),aod(nx,ny,nch,nt),channels(nch))
  READ(inunit)channels
  READ(inunit)aod_kind(:,:,:,nt)
  CLOSE(inunit)

  IF (nch /= nchan_nnr) THEN
      PRINT *,'Channel mismatch nch = ',nch,' nchan_nnr = ' ,nchan_nnr
      STOP
   ENDIF

  IF (MAXVAL(nnr_channels-channels) > 0.1) THEN
     PRINT *,'Channel mismatch'
     PRINT *,'nnr_channels = ',nnr_channels
     PRINT *,'channels = ',channels
     STOP
  ENDIF

  aod=aod_kind

  dimstring(1) = "grid_xt"
  dimstring(2) = "grid_yt"
  dimstring(3) = "nchannels"
  dimstring(4) = "time"

  filename=filenc_inout

  CALL netcdfdimension(filename,4,dimstring,dims)  

  IF (nx /= dims(1) .OR. ny /= dims(2) .OR. nch /= dims(3) .OR. nt /= dims(4)) THEN
     PRINT *,'Size mismatch in nx,ny,nch,nt'
     PRINT *,nx,ny,nch,nt
     PRINT *,dims(1),dims(2),dims(3),dims(4)
     PRINT *,'Stopping'
     STOP
  ENDIF

  PRINT *,nx,ny,nch,nt
  PRINT *,MINVAL(aod),MAXVAL(aod)

  varname = "AOD"
  varstringname=varname
  CALL writenetcdfdata4(filename,aod,varstringname,nx,ny,nch,nt)

  varname = "time"
  varstringname=varname
  allocate(time(nt))
  time=3600.*fhr
  CALL writenetcdfdata1(filename,time,varstringname,nt)
     
  DEALLOCATE(aod,aod_kind,time)

END PROGRAM aod2ncdf

