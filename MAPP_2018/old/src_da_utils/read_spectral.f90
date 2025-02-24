PROGRAM read_spectral

! reworked from jsw in GSI trunk

! uses gaussian-based length scale estimate (eq 5 in pannekoucke, berre and
! desroziers paper qjr 2008 497-508)

! linear grid: nlons=2*(ntrunc+1), nlats=nlons/2
! quadratic grid: nlons=3*(ntrunc+2), nlats=nlons/2
! in GSI global bckg_error files nlats increased by 2 
! GSI global bckg_error files are named after latitudes: e.g. global_berror.l64y258.f77
! per NCEP
!! t878      ; t574        t382       t190quad   t254       t170       t126       t62
!! nlat=882  ; nlat=578  ; nlat=386 ; nlat=290 ; nlat=258 ; nlat=192 ; nlat=130 ; nlat=96
!! nlon=1760 ; nlon=1152 ; nlon=768 ; nlon=576 ; nlon=512 ; nlon=384 ; nlon=256 ; nlon=192


  USE kinds, ONLY : r_kind, i_kind, num_bytes_for_r_kind
  USE sigio_module
  USE constants, ONLY: init_constants, init_constants_derived, rd, grav, cp, rearth, pi,&
       &rad2deg,deg2rad,zero,one,two,half
  USE specmod, ONLY: gaulats,init_spec_vars
  USE mpimod, ONLY : mpi_rtype

  IMPLICIT NONE

  TYPE(sigio_head) :: sigheadi
  TYPE(sigio_data) :: sigdatai

  CHARACTER(len=500) filenamein
  CHARACTER(len=120) filenameout
  CHARACTER(len=10) datestring
  CHARACTER(len=3) :: ctrac
  INTEGER(i_kind) ::  iret,nlats,nlons,nlevs,ntrac,ntrunc,itrac,k,ierr,nanals,&
       nanal,numproc,nproc,iunit,&
       i,j,nskip
  REAL(r_kind):: delta,coslat
  CHARACTER(len=4) charnlons,charnlats,charnanal
!gaulats sins of gaussian latitudes - to be consistent with JSW's code
  REAL(r_kind), DIMENSION(:,:), ALLOCATABLE :: deltav, psg, &
       psgmean, zsg
  REAL(r_kind), DIMENSION(:,:,:), ALLOCATABLE :: psigshiftn,psigshifts,&
       psig,psigmean,psigvar,psigcorrn,psigcorrs,pressl,pressi,&
       psigshiftnvar,psigshiftsvar,psigshiftncov,psigshiftscov,&
       &lengthscalex,lengthscaley,vlengthscale

  REAL(r_kind), DIMENSION(:), ALLOCATABLE :: ak,bk

! mpi definitions.

  REAL(r_kind) :: clip

  INCLUDE 'mpif.h'


  clip=TINY(zero)

! get nanals,datestring from command line.
  CALL getarg(1,charnlons)
  READ(charnlons,'(i4)') nanals
  CALL getarg(2,datestring)
  CALL getarg(3,charnlons)
  CALL getarg(4,charnlats)
  READ(charnlons,'(i4)') nlons
  READ(charnlats,'(i4)') nlats

  CALL init_constants(.FALSE.)
  CALL init_constants_derived()

! read header from ensemble member 1.
!  filenamein = "sfg_"//datestring//"_fhr06s_mem001"//"_t254"
  filenamein = "sanl_"//datestring//"_ensmean"
  iunit = 7
  CALL sigio_sropen(iunit,TRIM(filenamein),ierr)
  IF (ierr /= 0) THEN
     PRINT *,'cannot read file ',filenamein,ierr
     FLUSH(6)
     FLUSH(0)
     STOP
  END IF
  CALL sigio_srhead(iunit,sigheadi,ierr)
  CALL sigio_sclose(iunit,ierr)

  ntrunc = sigheadi%jcap
  ntrac = sigheadi%ntrac

  PRINT *,ntrunc

  IF (nlons <= 0 .OR. nlats <= 0) THEN
     nlats = sigheadi%latf
     nlons = sigheadi%lonf
  ENDIF
  
  nlevs = sigheadi%levs

  ALLOCATE(psig(nlons,nlats,nlevs))
  ALLOCATE(psg(nlons,nlats))
  ALLOCATE(zsg(nlons,nlats))
  ALLOCATE(psgmean(nlons,nlats))
  ALLOCATE(pressl(nlons,nlats,nlevs))
  ALLOCATE(psigmean(nlons,nlats,nlevs))
  ALLOCATE(psigvar(nlons,nlats,nlevs))
  ALLOCATE(psigshiftn(nlons,nlats,nlevs))
  ALLOCATE(psigshifts(nlons,nlats,nlevs))
  ALLOCATE(psigshiftnvar(nlons,nlats,nlevs))
  ALLOCATE(psigshiftsvar(nlons,nlats,nlevs))
  ALLOCATE(psigshiftncov(nlons,nlats,nlevs))
  ALLOCATE(psigshiftscov(nlons,nlats,nlevs))
  ALLOCATE(psigcorrn(nlons,nlats,nlevs))
  ALLOCATE(psigcorrs(nlons,nlats,nlevs))
  ALLOCATE(lengthscalex(nlons,nlats,nlevs))
  ALLOCATE(lengthscaley(nlons,nlats,nlevs))
  ALLOCATE(vlengthscale(nlons,nlats,nlevs))
  ALLOCATE(deltav(nlons,nlats))
  ALLOCATE(ak(nlevs+1),bk(nlevs+1))

! read each ensemble member fhdfi forecast.

!  filenamein = "sfg_"//datestring//"_fhr06s_mem"//TRIM(charnanal)//"_t254"
  filenamein = "sanl_"//datestring//"_ensmean"
  PRINT *,TRIM(filenamein)

  CALL sigio_srohdc(iunit,TRIM(filenamein),sigheadi,sigdatai,iret)

  
  CALL getsigdata(sigheadi,sigdatai,psig,psg,pressl,zsg,ak,bk,&
       &nlons,nlats,nlevs,ntrunc)
  CALL sigio_axdata(sigdatai,ierr)
  CALL sigio_sclose(iunit,ierr)

  DEALLOCATE(psg,zsg)
  DEALLOCATE(psig,pressl,psigmean,psigvar)
  DEALLOCATE(psigshiftn,psigshifts,psigshiftnvar,psigshiftsvar)
  DEALLOCATE(psigshiftncov,psigshiftscov,psigcorrn,psigcorrs)
  DEALLOCATE(lengthscalex,lengthscaley,vlengthscale)
  DEALLOCATE(deltav,ak,bk)

END PROGRAM read_spectral

SUBROUTINE getsigdata(sighead,sigdata,psig,psg,pressl,zsg,ak,bk,&
     &nlons,nlats,nlevs,ntrunc)

  USE kinds, ONLY : r_kind, i_kind
  USE sigio_module
  USE constants, ONLY: rd, grav, cp, rearth, init_constants, init_constants_derived,zero,&
       &one,r0_01,r10
  USE specmod, ONLY: sptezv_s, sptez_s, init_spec_vars
  IMPLICIT NONE
  TYPE (sigio_data), INTENT(in out) :: sigdata
  TYPE (sigio_head), INTENT(in) :: sighead
  REAL(r_kind), DIMENSION(nlons,nlats,nlevs), INTENT(out) :: psig,pressl
  REAL(r_kind), DIMENSION(nlons,nlats), INTENT(out) :: psg,zsg
  REAL(r_kind), INTENT(out) :: ak(nlevs+1),bk(nlevs+1)
  REAL(r_kind), DIMENSION(nlons,nlats,nlevs+1) :: pressi
  REAL(r_kind) :: invlap((ntrunc+1)*(ntrunc+2))
  INTEGER(i_kind), INTENT(in) :: ntrunc,nlevs,nlons,nlats
  INTEGER(i_kind) :: k,ierr,n,m,nm
  REAL(r_kind) :: kap,kapr,kap1

  REAL(r_kind), DIMENSION((ntrunc+1)*(ntrunc+2)) :: wave

!probably
!itrac=1 is water vapor mixratio
!itrac=2 is ozone
!itrac=3 is cloud water

  kap = rd/cp
  kapr = cp/rd
  kap1 = kap + one
  IF (ntrunc < 0) THEN
     PRINT *,'illegal ntrunc = ',ntrunc
     STOP
  ENDIF

  CALL init_spec_vars(nlons,nlats,ntrunc,4)

!==> get tracer on gaussian grid.

  DO k=1,nlevs
     wave=sigdata%t(:,k)
     CALL sptez_s(wave,psig(:,:,k),1)
     PRINT *,k,MINVAL(psig(:,:,k)),MAXVAL(psig(:,:,k))

  ENDDO

  stop

  DO k=1,nlevs
     wave=sigdata%q(:,k,1)
     CALL sptez_s(wave,psig(:,:,k),1)
  ENDDO
  
!==> get pressures on model levels.
  IF (sighead%idvc == 0) THEN ! sigma coordinate, old file format.
     ak = zero
     bk = sighead%si(1:nlevs+1)
  ELSE IF (sighead%idvc == 1) THEN ! sigma coordinate
     ak = zero
     bk = sighead%vcoord(1:nlevs+1,2)
  ELSE IF (sighead%idvc == 2 .OR. sighead%idvc == 3) THEN ! hybrid coordinate
     bk = sighead%vcoord(1:nlevs+1,2) 
     ak = r0_01*sighead%vcoord(1:nlevs+1,1)  ! convert to mb
  ELSE
     PRINT *,'unknown vertical coordinate type',sighead%idvc
  END IF

  wave=sigdata%ps
  CALL sptez_s(wave,psg,1)
  psg = r10*EXP(psg)
  
  wave=sigdata%hs
  CALL sptez_s(wave,zsg,1)

  DO k=1,nlevs+1
     pressi(:,:,k) = ak(k)+bk(k)*psg
  ENDDO

  DO k=1,nlevs
! layer pressure from phillips vertical interpolation.
     pressl(:,:,k) = ((pressi(:,:,k)**kap1-pressi(:,:,k+1)**kap1)/&
          (kap1*(pressi(:,:,k)-pressi(:,:,k+1))))**kapr
  END DO

! convert to -log(p/ps)

  DO k=1,nlevs
     pressl(:,:,k) = -LOG(pressl(:,:,k)/psg)
  ENDDO

  k=nlevs+1

END SUBROUTINE getsigdata

