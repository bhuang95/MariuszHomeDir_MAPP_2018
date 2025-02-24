PROGRAM factor_emiss

!to multiply emissions by time factors and scale i's based on j's

  USE netcdf_io

  IMPLICIT NONE

  INTEGER, PARAMETER :: nvars=3
  REAL, PARAMETER :: ijfactor=0.25
  CHARACTER(len=12), DIMENSION(nvars) :: &
       &varnames = (/"E_ECJ","E_ORGJ","E_PM25J"/) 

  CHARACTER(len=12), DIMENSION(nvars) :: &
       &rnames = (/"ratio_ec","ratio_oc","ratio_pm25"/) 


  REAL, DIMENSION(1,1,1,24) :: tfactors,tfactors_local
  INTEGER :: nx,ny,nz,nt,i,vl,ivar
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: emissionsin,emissionsout
  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  INTEGER,           DIMENSION(4)                   :: dims
  CHARACTER(len=25)                                 :: varstringname

  CHARACTER(len=2) :: utc
  INTEGER :: iutc
  CHARACTER(len=250) :: emissfilein,emissfileout1,emissfileout2,&
       &tfactorfile,filename
  
  INTEGER :: iargc

  PRINT *,"This code has a bug - STOPPING"
  PRINT *,"emissions at upper levels are zeros"
  STOP

  IF (iargc() < 5) THEN
     PRINT *,"Needs time,  name of emission files and  tfactor file "&
          &"on input"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,utc)
  CALL getarg(2,tfactorfile)
  CALL getarg(3,emissfilein)
  CALL getarg(4,emissfileout1)
  CALL getarg(5,emissfileout2)

  IF ( INDEX(emissfileout1,'00z') < 1 .OR. INDEX(emissfileout2,'12z') < 1) THEN
     PRINT *,'Emissions files must be in correct order first 00z next 12z'
     PRINT *,'Stopping'
     STOP
  ENDIF

  READ(utc,'(i2)') iutc
  IF (iutc == 0) iutc=24

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "emissions_zdim"
  dimstring(4) = "Time"

  DO ivar=1,nvars

     PRINT *,varnames(ivar)
  
     filename=tfactorfile
     varstringname=rnames(ivar)
     CALL readnetcdfdata4(filename,tfactors,varstringname,1,1,1,24)

     tfactors_local=tfactors/tfactors(1,1,1,iutc)
     
     filename=emissfilein
     CALL netcdfdimension(filename,4,dimstring,dims)  
     nx=dims(1)
     ny=dims(2)
     nz=dims(3)
     
     IF (.NOT. ALLOCATED(emissionsin)) ALLOCATE(emissionsin(nx,ny,nz,1))
     
     varstringname=varnames(ivar)
     vl=LEN_TRIM(varstringname)
     
     CALL readnetcdfdata4(filename,emissionsin,varstringname,nx,ny,nz,1)
     
     filename=emissfileout1
     CALL netcdfdimension(filename,4,dimstring,dims)  
     nx=dims(1)
     ny=dims(2)
     nz=dims(3)
     nt=dims(4)
     
     IF (nt /= 12) THEN
        PRINT *,TRIM(emissfileout1)
        PRINT *,"nt should be 12 for input emissions"
        PRINT *,"Stopping"
        STOP
     ENDIF
     
     IF (.NOT. ALLOCATED(emissionsout))&
          & ALLOCATE(emissionsout(nx,ny,nz,nt))

!start of 00z file

     DO i=1,12
        emissionsout(:,:,1,i)=emissionsin(:,:,1,1)*tfactors_local(1,1,1,i)
     ENDDO
     
     CALL writenetcdfdata4(filename,emissionsout,varstringname,nx,ny,nz,nt)
     
     varstringname(vl:vl)='I'
     emissionsout=emissionsout*ijfactor
     
     CALL writenetcdfdata4(filename,emissionsout,varstringname,nx,ny,nz,nt)
     
!start of 12z file
     
     filename=emissfileout2
     
     DO i=1,12
        emissionsout(:,:,1,i)=emissionsin(:,:,1,1)*tfactors_local(1,1,1,i+12)
     ENDDO
     
     varstringname(vl:vl)='J'
     CALL writenetcdfdata4(filename,emissionsout,varstringname,nx,ny,nz,nt)
     
     emissionsout=emissionsout*ijfactor
     
     varstringname(vl:vl)='I'
     CALL writenetcdfdata4(filename,emissionsout,varstringname,nx,ny,nz,nt)
     
  ENDDO

END PROGRAM factor_emiss

