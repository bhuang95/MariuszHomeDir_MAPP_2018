PROGRAM factor_emiss_spline_tspack

!to multiply emissions by time factors and scale i's based on j's

  USE netcdf_io
  USE module_splines  !for indexx only
  USE tension_mod

  IMPLICIT NONE

  REAL, PARAMETER :: epsilon=1.e-8
  INTEGER, PARAMETER :: nvars=3,nhours=24
  INTEGER :: i,j,k
  REAL, DIMENSION(nhours), PARAMETER :: times=(/(i,i=0,nhours-1)/)
  REAL, PARAMETER :: ijfactor=0.25
  CHARACTER(len=12), DIMENSION(nvars) :: &
       &varnames = (/"E_ECJ","E_ORGJ","E_PM25J"/) 

  INTEGER :: nx,ny,nz,ntt,vl,ivar
  INTEGER, PARAMETER :: nt=4
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: emissionsnt,emissionsnhours
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: emissionsin
  REAL, ALLOCATABLE, DIMENSION(:,:) :: demiss
  CHARACTER(len=20), DIMENSION(4)                   :: dimstring
  INTEGER,           DIMENSION(4)                   :: dims
  CHARACTER(len=25)                                 :: varstringname

  REAL, ALLOCATABLE, DIMENSION(:) :: timesnt,stimesnt,b,c,d,semissionsnt
  INTEGER, ALLOCATABLE, DIMENSION(:) :: indx
  
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: dstimesnt,&
       &dsemissionsnt,yp,sigma
  DOUBLE PRECISION,  DIMENSION(nhours) :: dtimes
  INTEGER :: ier,siger

  CHARACTER(len=2) :: utc
  INTEGER :: iutc,ii,jj,kk
  CHARACTER(len=250) :: prefix,filename
  
  CHARACTER(len=20), PARAMETER :: emissfileout1="wrfchemi_00z_d01",&
       &emissfileout2="wrfchemi_12z_d01"
  CHARACTER(len=250), DIMENSION(nt) :: filenames
  CHARACTER(len=2) :: ci
  
  INTEGER :: iargc
  CHARACTER(len=1) :: allexist !1 if all files exist, 0 - otherwise

  CHARACTER(len=8) :: cdatestart,cdateend
  CHARACTER(len=10) :: ctimestart,ctimeend
  REAL :: timestart,timeend
  
  CALL DATE_AND_TIME(cdatestart,ctimestart)
  READ(ctimestart(3:10),'(f8.3)') timestart


  IF (iargc() < 3) THEN
     PRINT *,"Needs time, prefix with location of input,"&
          &" and if all files exist"
     PRINT *,"Stopping"
     STOP
  ENDIF

  CALL getarg(1,utc)
  CALL getarg(2,prefix)
  CALL getarg(3,allexist)

  READ(utc,'(i2)') iutc

!iutc stands for UTC of the beginning of the current wrf run

  dimstring(1) = "west_east"
  dimstring(2) = "south_north"
  dimstring(3) = "emissions_zdim"
  dimstring(4) = "Time"

  DO i=1,nt
     WRITE(ci,'(i2.2)')i
     filenames(i)=TRIM(prefix)//'_'//ci
  ENDDO

  filename=filenames(1)

  CALL netcdfdimension(filename,4,dimstring,dims)  
  nx=dims(1)
  ny=dims(2)
  nz=dims(3)

  ALLOCATE(emissionsnhours(nx,ny,nz,nhours))
  ALLOCATE(emissionsin(nx,ny,nz,12))

  IF (allexist=='1') THEN

     PRINT *,'Using splines to assign tfactors'

     ALLOCATE(timesnt(nt),indx(nt),&
          &stimesnt(nt+1),semissionsnt(nt+1),&
          &b(nt+1),c(nt+1),d(nt+1))
     ALLOCATE(dstimesnt(nt+1),dsemissionsnt(nt+1),&
          &yp(nt+1),sigma(nt+1))
     ALLOCATE(emissionsnt(nx,ny,nz,nt))

     dtimes=times

     DO i=1,nt
        ii=iutc-i*6+5
        IF (ii < 0) ii=ii+24
        timesnt(i)=REAL(ii)
     ENDDO

     CALL indexx(nt,timesnt,indx)
  
     stimesnt(2:nt+1)=timesnt(indx)
     stimesnt(1)=timesnt(indx(1))-6

     dstimesnt=stimesnt

     DO ivar=1,nvars

        PRINT *,varnames(ivar)

        varstringname=varnames(ivar)
        vl=LEN_TRIM(varstringname)

        DO i=1,nt
           CALL readnetcdfdata4(filenames(i),emissionsnt(:,:,:,i),&
                &varstringname,nx,ny,nz,1)
        ENDDO


        DO i=1,nx
           DO j=1,ny

              semissionsnt(2:nt+1)=emissionsnt(i,j,1,indx)
              semissionsnt(1)=emissionsnt(i,j,1,indx(nt))

              dsemissionsnt=semissionsnt


              IF (SUM(semissionsnt) > epsilon) THEN 
!              CALL spline(nt+1,stimesnt,semissionsnt,b,c,d)
                 CALL tspsi (nt+1,dstimesnt,dsemissionsnt,yp,sigma,ier,siger)

                 IF (ier /= 0) THEN
                    PRINT *,'tspsi ier= ',ier,' in tspack'
                    STOP
                 ENDIF

!              IF (siger /= 0) THEN
!                 PRINT *,'tspsi siger= ',siger,' in tspack'
!                 STOP
!              ENDIF


                 DO ii=1,nhours
!                 emissionsnhours(i,j,1,ii)=MAX(seval(nt+1,times(ii),&
!                      &stimesnt,semissionsnt,b,c,d),0.)
                    emissionsnhours(i,j,1,ii)=MAX(hval(dtimes(ii),nt+1,&
                         &dstimesnt,dsemissionsnt,yp,sigma,ier),0.)

                    IF (ier /= 0) THEN
                       PRINT *,'hval ier= ',ier,' in tspack'
                       STOP
                    ENDIF
                 ENDDO
              ELSE
                 emissionsnhours(i,j,1,:)=0.
              ENDIF
           ENDDO
        ENDDO

!     k=0
!     DO i=1,nx,10
!        DO j=1,ny,10
!           k=k+1
!           DO ii=1,nhours
!              WRITE(100+k,*)times(ii),emissionsnhours(i,j,1,ii)
!           ENDDO
!        ENDDO
!     ENDDO

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

!start of 00z file

        varstringname(vl:vl)='J'     

        CALL readnetcdfdata4(filename,emissionsin,&
             &varstringname,nx,ny,nz,ntt)

        emissionsnhours(:,:,2:nz,1:12)=emissionsin(:,:,2:nz,:)

        CALL writenetcdfdata4(filename,emissionsnhours(:,:,:,1:12),&
             &varstringname,nx,ny,nz,ntt)

        varstringname(vl:vl)='I'

        CALL writenetcdfdata4(filename,ijfactor*emissionsnhours(:,:,:,1:12),&
             &varstringname,nx,ny,nz,ntt)

!start of 12z file

        filename=emissfileout2

        varstringname(vl:vl)='J'

        CALL readnetcdfdata4(filename,emissionsin,&
             &varstringname,nx,ny,nz,ntt)

        emissionsnhours(:,:,2:nz,13:24)=emissionsin(:,:,2:nz,:)

        CALL writenetcdfdata4(filename,emissionsnhours(:,:,:,13:24),&
             &varstringname,nx,ny,nz,ntt)

        varstringname(vl:vl)='I'
        CALL writenetcdfdata4(filename,ijfactor*emissionsnhours(:,:,:,13:24),&
             &varstringname,nx,ny,nz,ntt)

     ENDDO

     DEALLOCATE(timesnt,indx,stimesnt,semissionsnt,b,c,d,dstimesnt,dsemissionsnt,yp,sigma)
     DEALLOCATE(emissionsnt)

  ELSE

     PRINT *,'Using defaults to assign tfactors'

!ii is actual hour of the input
!jj is 1 for 00z, 2 for 12z
!kk is order of this hour in the file jj

     ii=MOD(iutc+6,24)-1
     IF (ii < 0) ii=ii+24
     IF (ii <= 11) THEN
        kk=ii+1 
        jj=1
     ELSE
        kk=ii-12+1
        jj=2
     ENDIF

     ALLOCATE(demiss(nx,ny))

     DO ivar=1,nvars

        PRINT *,varnames(ivar)

        varstringname=varnames(ivar)
        vl=LEN_TRIM(varstringname)

        CALL readnetcdfdata4(filenames(1),emissionsnhours(:,:,:,ii+1),&
             &varstringname,nx,ny,nz,1)

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

        IF (jj == 1) THEN
           filename=emissfileout1
        ELSE
           filename=emissfileout2
        ENDIF

        varstringname(vl:vl)='J'     
        
        CALL readnetcdfdata4(filename,emissionsin,&
             &varstringname,nx,ny,nz,ntt)

        demiss=emissionsnhours(:,:,1,ii+1)-emissionsin(:,:,1,kk)

        DO i=1,ntt
           emissionsin(:,:,1,i)=MAX(emissionsin(:,:,1,i)+demiss,0.)
        ENDDO

        CALL writenetcdfdata4(filename,emissionsin,&
             &varstringname,nx,ny,nz,ntt)

        varstringname(vl:vl)='I'

        CALL writenetcdfdata4(filename,ijfactor*emissionsin,&
             &varstringname,nx,ny,nz,ntt)


        IF (jj == 1) THEN
           filename=emissfileout2
        ELSE
           filename=emissfileout1
        ENDIF

        varstringname(vl:vl)='J'

        CALL readnetcdfdata4(filename,emissionsin,&
             &varstringname,nx,ny,nz,ntt)

        DO i=1,ntt
           emissionsin(:,:,1,i)=MAX(emissionsin(:,:,1,i)+demiss,0.)
        ENDDO

        CALL writenetcdfdata4(filename,emissionsin,&
             &varstringname,nx,ny,nz,ntt)

        varstringname(vl:vl)='I'
        CALL writenetcdfdata4(filename,ijfactor*emissionsin,&
             &varstringname,nx,ny,nz,ntt)

     ENDDO

     DEALLOCATE(demiss)

  ENDIF

  DEALLOCATE(emissionsnhours,emissionsin)

  CALL DATE_AND_TIME(cdateend,ctimeend)
  READ(ctimeend(3:10),'(f8.3)') timeend

  PRINT *, 'cputime=',timeend-timestart
  
END PROGRAM factor_emiss_spline_tspack

