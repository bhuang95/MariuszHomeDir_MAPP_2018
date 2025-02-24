PROGRAM factor_emiss_spline_cubic

!to multiply emissions by time factors and scale i's based on j's

  USE netcdf_io
  USE module_splines

  IMPLICIT NONE

  REAL, PARAMETER :: epsilon=1.e-8
  INTEGER, PARAMETER :: nvars=3,nhours=24
  INTEGER :: i,j
  REAL, DIMENSION(nhours), PARAMETER :: times=(/(i,i=0,nhours-1)/)
  REAL, PARAMETER :: ijfactor=0.25
  CHARACTER(len=12), DIMENSION(nvars) :: &
       &varnames = (/"E_ECJ","E_ORGJ","E_PM25J"/) 

  INTEGER :: nx,ny,nz,ntt,vl,ivar
  INTEGER, PARAMETER :: nt=4
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: emissionsnt,emissionsnhours,emissionsin,emissionsout
  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  INTEGER,           DIMENSION(4)                   :: dims
  CHARACTER(len=25)                                 :: varstringname

  REAL, ALLOCATABLE, DIMENSION(:) :: timesnt,stimesnt,b,c,d,semissionsnt
  INTEGER, ALLOCATABLE, DIMENSION(:) :: indx

  CHARACTER(len=2) :: utc
  INTEGER :: iutc,ii
  CHARACTER(len=250) :: emissfilein,emissfileout1,emissfileout2,&
       &tfactorfile,filename
  
  CHARACTER(len=250), DIMENSION(nt) :: filenames

  INTEGER :: iargc

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

  IF ( INDEX(emissfileout1,'00z') < 1 .OR. &
       &INDEX(emissfileout2,'12z') < 1) THEN
     PRINT *,'Emissions files must be in correct order first 00z next 12z'
     PRINT *,'Stopping'
     STOP
  ENDIF

  READ(utc,'(i2)') iutc

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "emissions_zdim"
  dimstring(4) = "Time"

  filename='/scratch1/portfolios/BMC/chem-var/pagowski/enkf_runs/wrf_run_gfs_0th/Output_001/2012_05_30_18/wrfchemi_d01_output_enkf'

  filenames=filename

  CALL netcdfdimension(filename,4,dimstring,dims)  
  nx=dims(1)
  ny=dims(2)
  nz=dims(3)

  ALLOCATE(emissionsnt(nx,ny,nz,nt))
  ALLOCATE(emissionsnhours(nx,ny,nz,nhours))
  ALLOCATE(timesnt(nt),indx(nt),&
       &stimesnt(nt+1),semissionsnt(nt+1),&
       &b(nt+1),c(nt+1),d(nt+1))

  DO i=1,nt
     ii=MOD(iutc+i*6,24)-1
     IF (ii < 0) ii=ii+24
     timesnt(i)=REAL(ii)
  ENDDO
  
  CALL indexx(nt,timesnt,indx)
  
  stimesnt(2:nt+1)=timesnt(indx)
  stimesnt(1)=timesnt(indx(1))-6
  
  DO ivar=1,nvars

     PRINT *,varnames(ivar)
  
     varstringname=varnames(ivar)

     DO i=1,nt
        CALL readnetcdfdata4(filenames(i),emissionsnt(:,:,:,i),&
             &varstringname,nx,ny,nz,1)
     ENDDO

     emissionsnt(:,:,1,2)=3*emissionsnt(:,:,1,2)
     emissionsnt(:,:,1,3)=7*emissionsnt(:,:,1,2)
     emissionsnt(:,:,1,4)=0*emissionsnt(:,:,1,2)
     
     DO i=20,20 !1,nx
        DO j=30,30 !1,ny

           semissionsnt(2:nt+1)=emissionsnt(i,j,1,indx)
           semissionsnt(1)=emissionsnt(i,j,1,indx(nt))

           IF (SUM(emissionsnt) > epsilon) THEN 
              CALL spline(nt+1,stimesnt,semissionsnt,b,c,d)
              
              DO ii=1,nhours
                 emissionsnhours(i,j,1,ii)=MAX(seval(nt+1,times(ii),&
                      &stimesnt,semissionsnt,b,c,d),0.)
              ENDDO
           ELSE
              emissionsnhours(i,j,1,:)=0.
           ENDIF
        ENDDO
     ENDDO

     DO ii=1,nt+1
        WRITE(70,*)stimesnt(ii),semissionsnt(ii)
     ENDDO

     DO ii=1,nhours
        WRITE(71,*)times(ii),emissionsnhours(20,30,1,ii)
     ENDDO


     stop

     filename=emissfilein

     IF (.NOT. ALLOCATED(emissionsin)) ALLOCATE(emissionsin(nx,ny,nz,1))
     
     varstringname=varnames(ivar)
     vl=LEN_TRIM(varstringname)
     
     CALL readnetcdfdata4(filename,emissionsin,varstringname,nx,ny,nz,1)
     
     filename=emissfileout1

     CALL netcdfdimension(filename,4,dimstring,dims)  

     IF (nx /= dims(1) .OR.  ny /= dims(2) .OR.  nz /= dims(3)) THEN
        PRINT *,'Size mismatch in nx,ny,nz'
        PRINT *,'Stopping'
        STOP
     ENDIF

     ntt=dims(4)
     
     IF (ntt /= 12) THEN
        PRINT *,TRIM(emissfileout1)
        PRINT *,"ntt should be 12 for input emissions"
        PRINT *,"Stopping"
        STOP
     ENDIF
     
     IF (.NOT. ALLOCATED(emissionsout))&
          & ALLOCATE(emissionsout(nx,ny,nz,ntt))

!start of 00z file

     DO i=1,12
        emissionsout(:,:,1,i)=emissionsnhours(:,:,1,i)
     ENDDO
     
     CALL writenetcdfdata4(filename,emissionsout,varstringname,nx,ny,nz,ntt)
     
     varstringname(vl:vl)='I'
     emissionsout=emissionsout*ijfactor
     
     CALL writenetcdfdata4(filename,emissionsout,varstringname,nx,ny,nz,ntt)
     
!start of 12z file
     
     filename=emissfileout2
     
     DO i=1,12
        emissionsout(:,:,1,i)=emissionsnhours(:,:,1,i+12)
     ENDDO
     
     varstringname(vl:vl)='J'
     CALL writenetcdfdata4(filename,emissionsout,varstringname,nx,ny,nz,ntt)
     
     emissionsout=emissionsout*ijfactor
     
     varstringname(vl:vl)='I'
     CALL writenetcdfdata4(filename,emissionsout,varstringname,nx,ny,nz,ntt)
     
  ENDDO

END PROGRAM factor_emiss_spline_cubic

