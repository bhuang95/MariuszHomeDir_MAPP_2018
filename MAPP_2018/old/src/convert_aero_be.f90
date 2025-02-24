PROGRAM convert_aero_be
!overwrite aerosol becs with those read from a file
!since original met becs need to be latitude dependent bu not aerosols
!reworked from read_BE.f90 from gsi util directory

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

  CHARACTER(len=250) :: infile_be,prefix_r,suffix_r,outfile_be,infile

  character*5 var
!
  real(r_single),dimension(:,:),allocatable:: corz_avn,hwll_avn,vztdq_avn
  real(r_single),dimension(:,:),allocatable::  corqq_avn
!
  real(r_kind) fctr_agv,fctr_bv,fctr_wgv

  logical :: logread


  CALL getarg(1,infile_be)
  CALL getarg(2,prefix_r)
  CALL getarg(3,suffix_r)
  CALL getarg(4,outfile_be)

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

     logread=.FALSE.

     IF (var/='q') THEN
        read(inunit_be) corz_avn

        SELECT CASE (var)
        CASE('BC1','BC2','OC1','OC2','sulf','P25','SEAS1','SEAS2',&
             &'SEAS3','SEAS4','DUST1','DUST2','DUST3','DUST4','DUST5',&
             &'PMTOT','PM25')
           infile=TRIM(prefix_r)//TRIM(var)//trim(suffix_r)
           logread=.TRUE.
           OPEN(inunit_r,file=infile,form='formatted',status='old')
           READ(inunit_r,*)nlevs
           IF (nlevs /= isig ) THEN
              PRINT *, 'Inconsistent number of levels isig = ',isig,&
                   &' nlevs in R = ',nlevs
              PRINT *,'Stopping'
              STOP
           ENDIF
           DO k=1,isig
              PRINT *,k,corz_avn(1,k),'corz before'
              READ(inunit_r,*),i,corz_avn(1,k)
              PRINT *,k,corz_avn(1,k),'corz after'
              corz_avn(:,k)=corz_avn(1,k)
           ENDDO
           PRINT *,'Modified corz ',var
        END SELECT
        WRITE(outunit_be) corz_avn
     ELSE
        ALLOCATE ( corqq_avn(1:mlat,1:isig) )
        READ(inunit_be) corz_avn,corqq_avn
        WRITE(outunit_be) corz_avn,corqq_avn
     END IF
     
     READ(inunit_be) hwll_avn
     
     IF(logread) THEN
        READ(inunit_r,*)nlevs
        IF (nlevs /= isig ) THEN
           PRINT *, 'Inconsistent number of levels isig = ',isig,&
                &' nlevs in R = ',nlevs
           PRINT *,'Stopping'
           STOP
        ENDIF
        
        DO k=1,isig
           PRINT *,k,hwll_avn(1,k),'hwll before'
           READ(inunit_r,*),i,hwll_avn(1,k)
           PRINT *,k,hwll_avn(1,k),'hwll after'
           hwll_avn(:,k)=hwll_avn(1,k)
        ENDDO
        PRINT *,'Modified hzscl ',var
     ENDIF

     WRITE(outunit_be) hwll_avn

     IF (isig>1) THEN
        read(inunit_be) vztdq_avn

        IF (logread) THEN
           READ(inunit_r,*)nlevs
           IF (nlevs /= isig ) THEN
              PRINT *, 'Inconsistent number of levels isig = ',isig,&
                   &' nlevs in R = ',nlevs
              PRINT *,'Stopping'
              STOP
           ENDIF
           DO k=1,isig
              PRINT *,k,vztdq_avn(k,1),'vz before'
              READ(inunit_r,*),i,vztdq_avn(k,1)
              PRINT *,k,vztdq_avn(k,1),'vz after'
              vztdq_avn(k,:)=vztdq_avn(k,1)
           ENDDO
           PRINT *,'Modified vscale ',var
           CLOSE(inunit_r)
        ENDIF

        WRITE(outunit_be) vztdq_avn

     END IF
     
     deallocate ( corz_avn )

     deallocate ( hwll_avn )
     deallocate ( vztdq_avn )
     if (var=='q') deallocate ( corqq_avn )

  enddo read

  close(inunit_be)
  close(outunit_be)

END PROGRAM convert_aero_be
