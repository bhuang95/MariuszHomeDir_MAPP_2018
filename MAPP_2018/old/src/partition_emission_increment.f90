PROGRAM partition_emission_increment

!to update emissions using increment based on a priori ratios 

  USE netcdf_io

  IMPLICIT NONE

  REAL, PARAMETER :: epsilon=TINY(1.),ratiomax=10.
  INTEGER, PARAMETER :: nvars=3,nhours=1
  REAL, PARAMETER :: default_ratio=1./REAL(nvars)
  INTEGER :: i,j,k
  CHARACTER(len=12), DIMENSION(nvars) :: &
       &varnames = (/"E_ECJ","E_ORGJ","E_PM25J"/) 
  CHARACTER(len=12) :: varname_emisstot="E_PM25T"

  INTEGER :: nx,ny,nz,nt,ivar
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: emissionsin,emissionsout,demiss,emissionspec
  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  INTEGER,           DIMENSION(4)                   :: dims
  CHARACTER(len=25)                                 :: varstringname

  INTEGER :: ii,jj,kk
  CHARACTER(len=250) :: filenamein,filenameout
  
  INTEGER :: iargc

  IF (iargc() < 2) THEN
     PRINT *,"Needs input and output filenames"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,filenamein)
  CALL getarg(2,filenameout)

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "emissions_zdim"
  dimstring(4) = "Time"

  CALL netcdfdimension(filenamein,4,dimstring,dims)  
  nx=dims(1)
  ny=dims(2)
  nz=dims(3)
  nt=dims(4)

  IF (nt /= nhours) THEN
     PRINT *,"Need to be one hour input only"
     PRINT *,"Stopping"
     STOP
  ENDIF


  ALLOCATE(emissionsin(nx,ny,nz,nt),emissionsout(nx,ny,nz,nt),emissionspec(nx,ny,nz,nt),&
       &demiss(nx,ny,nz,nt))

  varstringname=varname_emisstot

  CALL readnetcdfdata4(filenamein,emissionsin,&
                &varstringname,nx,ny,nz,nt)

  CALL readnetcdfdata4(filenameout,emissionsout,&
                &varstringname,nx,ny,nz,nt)

  demiss=emissionsout-emissionsin

  DO ivar=1,nvars

     PRINT *,varnames(ivar)

     varstringname=varnames(ivar)
     
     CALL readnetcdfdata4(filenamein,emissionspec,&
          &varstringname,nx,ny,nz,nt)
  
     DO i=1,nx
        DO j=1,ny
           IF (emissionsin(i,j,1,1) < epsilon)  THEN
              emissionspec(i,j,1,1)=MAX(demiss(i,j,1,1)*default_ratio+emissionspec(i,j,1,1),&
                   &epsilon)
           ELSE

              emissionspec(i,j,1,1)=MAX(demiss(i,j,1,1)*&
                   &MIN(ratiomax,emissionspec(i,j,1,1)/emissionsin(i,j,1,1))+&
                   &emissionspec(i,j,1,1),epsilon)
           ENDIF
        ENDDO
     ENDDO

     CALL writenetcdfdata4(filenameout,emissionspec,&
          &varstringname,nx,ny,nz,nt)
     
  ENDDO
 
  DEALLOCATE(emissionsin,emissionsout,emissionspec,demiss)

END PROGRAM partition_emission_increment

