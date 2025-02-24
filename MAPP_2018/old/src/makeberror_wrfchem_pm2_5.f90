PROGRAM makeberror_wrfchem_pm2_5

!Mariusz Pagowski, CIRA/NOAA, 2010-11-18

!to produce background error file for GSI
!reads gsi regional berror file and suplements it with pm2_5
!PM2.5 standard dev and vert and hori lengthscales 


  USE kinds, ONLY : i_kind,r_single

  IMPLICIT NONE

  REAL(r_single), parameter :: ptop=50._r_single,psfc=101300_r_single
  
  INTEGER(i_kind) :: mlat, msig
  REAL(r_single), DIMENSION(:), ALLOCATABLE :: clat_avn,sigma_avn,&
       &hwll,vztdq,corz,rsig,dsig,vz
  REAL(r_single), DIMENSION(:,:), ALLOCATABLE :: bv_avn,wgv_avn
  REAL(r_single), DIMENSION(:,:,:), ALLOCATABLE :: agv_avn
  REAL(r_single), DIMENSION(:,:), ALLOCATABLE :: corz_avn,hwll_avn,&
       &vztdq_avn
  CHARACTER(len=120) :: infile,outfile
  CHARACTER(len=12) :: var='pm2_5'
  character(len=1) :: junk

  INTEGER(i_kind) :: inunit=555, outunit=556,k,iargc,l
  
  IF(iargc().NE.2) THEN
     PRINT *,' need two string inputs, got',iargc()
     PRINT *,'Stopping'
     STOP
  ENDIF

  CALL getarg(1,infile)
  CALL getarg(2,outfile)

  OPEN(unit=inunit,file=infile,form='formatted')
  READ(inunit,*)msig
  PRINT *,msig

  mlat=3

  ALLOCATE ( sigma_avn(1:msig),hwll(1:msig),vztdq(1:msig),corz(1:msig))
  ALLOCATE ( rsig(1:msig),dsig(1:msig),vz(1:msig))

  DO k=1,msig
     READ(inunit,*)l,sigma_avn(k)
     PRINT *,k,sigma_avn(k)
     sigma_avn(k)=sigma_avn(k)*(1.-ptop/psfc)+ptop/psfc
     PRINT *,k,sigma_avn(k),'***'
  ENDDO

  DO k=1,msig
     rsig(k)=LOG(sigma_avn(k))
  ENDDO

  dsig(1)=rsig(1)-rsig(2)
  DO k=2,msig-1
     dsig(k)=0.5*(rsig(k-1)-rsig(k+1))
  ENDDO
  dsig(msig)=rsig(msig-1)-rsig(msig)
  
  READ(inunit,*)junk

  DO k=1,msig
     READ(inunit,*)l,hwll(k)
     PRINT *,k,hwll(k)
  ENDDO

  READ(inunit,*) junk

  DO k=1,msig
     READ(inunit,*)l,vz(k)
     PRINT *,k,vz(k)
     vztdq(k)=1./vz(k)/dsig(k)     
  ENDDO

  READ(inunit,*) junk

  DO k=1,msig
     READ(inunit,*)l,corz(k)
     PRINT *,k,corz(k)
  ENDDO

  CLOSE(inunit)
  
  ALLOCATE ( clat_avn(mlat) )
  clat_avn=(/-90.,0.,90./)

  ALLOCATE ( agv_avn(0:mlat+1,1:msig,1:msig) )
  ALLOCATE ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )
  agv_avn=0.
  bv_avn=0.
  wgv_avn=0.

  ALLOCATE ( hwll_avn(0:mlat+1,1:msig), vztdq_avn(1:msig,0:mlat+1),&
       &corz_avn(1:mlat,1:msig) )

  DO k=1,msig
     hwll_avn(:,k)=hwll(k)
     vztdq_avn(k,:)=vztdq(k)
     corz_avn(:,k)=corz(k)
     WRITE(101,*)k,hwll(k),vztdq(k),corz(k)
  ENDDO

  OPEN(unit=outunit,file=outfile,form='unformatted')
  WRITE(outunit)msig,mlat
  WRITE(outunit)clat_avn,(sigma_avn(k),k=1,msig)
  WRITE(outunit)agv_avn,bv_avn,wgv_avn
  WRITE(outunit)var,msig
  WRITE(outunit)corz_avn
  WRITE(outunit)hwll_avn
  WRITE(outunit)vztdq_avn
  CLOSE(outunit)

END PROGRAM makeberror_wrfchem_pm2_5
