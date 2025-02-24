PROGRAM convert_aero_be_pm25
!overwrite aerosol becs with those read from a file
!since original met becs need to be latitude dependent bu not aerosols
!reworked from read_BE.f90 from gsi util directory
!add pm25 to gocart berror files


  use kinds,only : r_single,i_kind,r_kind
  implicit none
!
  integer(i_kind) :: msig,mlat
  real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn
  real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn
  real(r_single),dimension(:,:,:),allocatable:: agv_avn

!
  INTEGER(i_kind):: inunit_be=12,inunit_r=13,istat
  integer(i_kind):: outunit_be=24
  INTEGER(i_kind):: isig,k,i,j,nlevs

  CHARACTER(len=250) :: infile_be,infile_r,outfile_be

  character*5 var
!
  real(r_single),dimension(:,:),allocatable:: corz_avn,hwll_avn,vztdq_avn
  REAL(r_single),DIMENSION(:,:),ALLOCATABLE:: corz_avn_pm25,hwll_avn_pm25,vztdq_avn_pm25
  real(r_single),dimension(:,:),allocatable::  corqq_avn
!
  real(r_kind) fctr_agv,fctr_bv,fctr_wgv


  CALL getarg(1,infile_be)
  CALL getarg(2,infile_r)
  CALL getarg(3,outfile_be)

  open(inunit_be,file=infile_be,form='unformatted',status='old')
  OPEN(outunit_be,file=outfile_be,form='unformatted')

  rewind inunit_be
  read(inunit_be) msig,mlat
  write(outunit_be) msig,mlat

  write(*,*) msig,mlat
!
! Read background error file to get balance variables
  allocate ( clat_avn(mlat) )
  allocate ( sigma_avn(1:msig) )
  allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
  allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )

  read(inunit_be)clat_avn,(sigma_avn(k),k=1,msig)
  read(inunit_be)agv_avn,bv_avn,wgv_avn

  write(outunit_be)clat_avn,(sigma_avn(k),k=1,msig)
  write(outunit_be)agv_avn,bv_avn,wgv_avn

! Read amplitudes
  read: do
     read(inunit_be,iostat=istat) var, isig
     if (istat /= 0) exit
     write(outunit_be) var, isig
     write(*,*) 'var, isig= ',var, isig
     allocate ( corz_avn(1:mlat,1:isig) )
     allocate ( hwll_avn(0:mlat+1,1:isig) )
     allocate ( vztdq_avn(1:isig,0:mlat+1) )


     if (var/='q') then
        read(inunit_be) corz_avn

        IF (var=='PMTOT') THEN
           OPEN(inunit_r,file=infile_r,form='formatted',status='old')
           READ(inunit_r,*)nlevs
           IF (nlevs /= isig ) THEN
              PRINT *, 'Inconsistent number of levels isig = ',isig,&
                   &' nlevs in R = ',nlevs
              PRINT *,'Stopping'
              STOP
           ENDIF
           ALLOCATE ( corz_avn_pm25(1:mlat,1:isig) )
           ALLOCATE ( hwll_avn_pm25(0:mlat+1,1:isig) )
           ALLOCATE ( vztdq_avn_pm25(1:isig,0:mlat+1) )
           DO k=1,isig
              PRINT *,k,corz_avn(1,k),'corz before'
              READ(inunit_r,*),i,corz_avn_pm25(1,k)
              PRINT *,k,corz_avn(1,k),'corz after'
           ENDDO
        ENDIF
        write(outunit_be) corz_avn
     else
        allocate ( corqq_avn(1:mlat,1:isig) )
        read(inunit_be) corz_avn,corqq_avn
        write(outunit_be) corz_avn,corqq_avn
     end if

     read(inunit_be) hwll_avn
     IF (var=='PMTOT') THEN
        READ(inunit_r,*)nlevs
        IF (nlevs /= isig ) THEN
           PRINT *, 'Inconsistent number of levels isig = ',isig,&
                &' nlevs in R = ',nlevs
           PRINT *,'Stopping'
           STOP
        ENDIF

        DO k=1,isig
           PRINT *,k,hwll_avn(1,k),'hwll before'
           READ(inunit_r,*),i,hwll_avn_pm25(1,k)
           PRINT *,k,hwll_avn(1,k),'hwll after'
        ENDDO
     ENDIF
     write(outunit_be) hwll_avn

     IF (isig>1) THEN
        read(inunit_be) vztdq_avn

        IF (var=='PMTOT') THEN
           READ(inunit_r,*)nlevs
           IF (nlevs /= isig ) THEN
              PRINT *, 'Inconsistent number of levels isig = ',isig,&
                   &' nlevs in R = ',nlevs
              PRINT *,'Stopping'
              STOP
           ENDIF
           DO k=1,isig
              PRINT *,k,vztdq_avn(k,1),'vz before'
              READ(inunit_r,*),i,vztdq_avn_pm25(k,1)
              PRINT *,k,vztdq_avn(k,1),'vz after'
           ENDDO
        ENDIF
        WRITE(outunit_be) vztdq_avn
     END IF

     deallocate ( corz_avn )

     deallocate ( hwll_avn )
     deallocate ( vztdq_avn )
     if (var=='q') deallocate ( corqq_avn )
    
  enddo read

  var='PM25'

  WRITE(outunit_be,iostat=istat) var, isig
  
  DO k=1,isig
     corz_avn_pm25(:,k)=corz_avn_pm25(1,k)
     hwll_avn_pm25(:,k)=hwll_avn_pm25(1,k)
     vztdq_avn_pm25(k,:)=vztdq_avn_pm25(k,1)
  ENDDO

  WRITE(outunit_be) corz_avn_pm25
  WRITE(outunit_be) hwll_avn_pm25
  WRITE(outunit_be) vztdq_avn_pm25

  DEALLOCATE(corz_avn_pm25,hwll_avn_pm25,vztdq_avn_pm25)

  close(inunit_be)
  close(outunit_be)
  CLOSE(inunit_r)
  
END PROGRAM convert_aero_be_pm25
