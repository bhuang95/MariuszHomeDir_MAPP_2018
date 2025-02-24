PROGRAM partition_increment_soa

!to multiply soa aerosols by ratio

  USE netcdf_io

  IMPLICIT NONE

  REAL, PARAMETER :: ratio_min=1.e-2,ratio_max=1.e1,pm2_5_max=300.
  REAL :: small
  INTEGER, PARAMETER :: nvars=32
  INTEGER :: i,j,ivar,vl
  CHARACTER(len=10), DIMENSION(nvars) :: &
       &varnames = (/&
       &"asoa1j","asoa1i","asoa2j","asoa2i","asoa3j","asoa3i","asoa4j","asoa4i",&
       &"bsoa1j","bsoa1i","bsoa2j","bsoa2i","bsoa3j","bsoa3i","bsoa4j","bsoa4i",&
       &"ecj","eci","so4aj","so4ai","nh4aj","nh4ai","no3aj","no3ai",&
       &"orgpaj","orgpai","p25j","p25i","claj","clai","naaj","naai"/)

  INTEGER :: nx,ny,nz,nt

  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: ratios,aerosol
  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  INTEGER,           DIMENSION(4)                   :: dims
  CHARACTER(len=25)                                 :: varstringname

  CHARACTER(len=250) :: aerofilein,aerofileout,filename
  
  INTEGER :: iargc

  small=TINY(1.)

  IF (iargc() < 2) THEN
     PRINT *,"needs two input files before and after pm2-% assimilation on input"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,aerofilein)
  CALL getarg(2,aerofileout)

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "bottom_top"
  dimstring(4) = "Time"

  filename=aerofilein

  CALL netcdfdimension(filename,4,dimstring,dims)  

  nx=dims(1)
  ny=dims(2)
  nz=dims(3)
  nt=dims(4)

  ALLOCATE(ratios(nx,ny,nz,nt),aerosol(nx,ny,nz,nt))

  varstringname='PM2_5_DRY'

  CALL readnetcdfdata4(filename,ratios,varstringname,nx,ny,nz,nt)

  PRINT *,nx,ny,nz,nt

  WHERE(ratios < small) ratios=small

  filename=aerofileout

  CALL netcdfdimension(filename,4,dimstring,dims)  

  IF (nx /= dims(1) .OR. ny /= dims(2) .OR. nz /= dims(3) .OR. nt /= dims(4) ) THEN
     PRINT *,'Size mismatch in nx,ny,nz'
     PRINT *,'Stopping'
     STOP
  ENDIF

  CALL readnetcdfdata4(filename,aerosol,varstringname,nx,ny,nz,nt)

  WHERE(aerosol > pm2_5_max) aerosol=pm2_5_max

  ratios=aerosol/ratios

  WHERE (ratios < ratio_min) ratios=ratio_min
  WHERE (ratios > ratio_max) ratios=ratio_max

  DO ivar=1,nvars
     
     PRINT *,varnames(ivar),ivar
     
     varstringname=varnames(ivar)

     CALL readnetcdfdata4(filename,aerosol,&
          &varstringname,nx,ny,nz,nt)
     
     aerosol=aerosol*ratios

     CALL writenetcdfdata4(filename,aerosol,varstringname,nx,ny,nz,nt)
     
  ENDDO

  DEALLOCATE(ratios,aerosol)

END PROGRAM partition_increment_soa

