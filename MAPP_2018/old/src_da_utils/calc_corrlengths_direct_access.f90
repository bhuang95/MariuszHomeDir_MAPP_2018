PROGRAM calc_corrlengths

!reworked from jsw in GSI trunk

! uses gaussian-based length scale estimate (eq 5 in pannekoucke, berre and
! desroziers paper qjr 2008 497-708)

  USE kinds, ONLY : r_kind, i_kind, num_bytes_for_r_kind
  USE sigio_module
  USE constants, ONLY: rd, grav, cp, rearth, init_constants, init_constants_derived, pi,&
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
       psig,psigmean,psigvar,psigcorrn,psigcorrs,pressl,vlengthscale,&
       psigshiftnvar,psigshiftsvar,psigshiftncov,psigshiftscov,lengthscalex,lengthscaley
! mpi definitions.

  REAL(r_kind) :: clip

  INCLUDE 'mpif.h'

  clip=TINY(zero)

  CALL mpi_init(ierr)
! nproc is process number, numproc is total number of processes.
  CALL mpi_comm_rank(mpi_comm_world,nproc,ierr)
  CALL mpi_comm_size(mpi_comm_world,numproc,ierr)

! get nanals,datestring from command line.
  CALL getarg(1,charnlons)
  READ(charnlons,'(i4)') nanals
  CALL getarg(2,datestring)
  CALL getarg(3,charnlons)
  CALL getarg(4,charnlats)
  READ(charnlons,'(i4)') nlons
  READ(charnlats,'(i4)') nlats

  IF (numproc < nanals) THEN
     PRINT *,numproc,nanals
     PRINT *,'warning, numproc too small!'
     FLUSH(6)
     FLUSH(0)
     CALL mpi_abort(mpi_comm_world,101,ierr)
     STOP
  END IF

  CALL init_constants(.FALSE.)
  CALL init_constants_derived()

! read header from ensemble member 1.
!  filenamein = "sfg_"//datestring//"_fhr06s_mem001"//"_t254"
  filenamein = "sfg_"//datestring//"_fhr06_mem001"
  iunit = 7
  CALL sigio_sropen(iunit,TRIM(filenamein),ierr)
  IF (ierr /= 0) THEN
     PRINT *,'cannot read file ',filenamein,ierr
     FLUSH(6)
     FLUSH(0)
     CALL mpi_abort(mpi_comm_world,101,ierr)
     STOP
  END IF
  CALL sigio_srhead(iunit,sigheadi,ierr)
  CALL sigio_sclose(iunit,ierr)

  ntrunc = sigheadi%jcap
  ntrac = sigheadi%ntrac
! if nlons, nlats not given by env vars, use value in sigma file.
  IF (nlons <= 0 .OR. nlats <= 0) THEN
     nlats = sigheadi%latf
     nlons = sigheadi%lonf
  ENDIF
  nlevs = sigheadi%levs
  IF (nproc == 0) THEN
     PRINT *,filenamein
     PRINT *,'nlons,nlats,nlevs,ntrunc,ntrac=',nlons,nlats,nlevs,ntrunc,ntrac
  ENDIF
  
  nanal = nproc + 1
  WRITE(charnanal,'(i3.3)') nanal

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

! read each ensemble member fhdfi forecast.

!  filenamein = "sfg_"//datestring//"_fhr06s_mem"//TRIM(charnanal)//"_t254"
  filenamein = "sfg_"//datestring//"_fhr06_mem"//TRIM(charnanal)
  PRINT *,TRIM(filenamein)

  CALL sigio_srohdc(iunit,TRIM(filenamein),sigheadi,sigdatai,iret)

  DO itrac=1,ntrac

     CALL getsigdata(sigheadi,sigdatai,psig,psg,pressl,zsg,nlons,nlats,nlevs,ntrunc,itrac,&
       &mpi_comm_world)

!  IF (nproc == 0) PRINT *,MINVAL(zsg),MAXVAL(zsg)

     IF (itrac == ntrac) THEN
        CALL sigio_axdata(sigdatai,ierr)
        CALL sigio_sclose(iunit,ierr)
     ENDIF

!  compute ensemble means

     CALL init_spec_vars(nlons,nlats,ntrunc,4) !for gaulats

     CALL mpi_allreduce(psig,psigmean,nlons*nlats*nlevs,mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigmean = psigmean/float(nanals)

! compute perturbations.
     psig = psig-psigmean

! compute horizontal length scale.
! shift north and south by 1 grid point.
     DO j=2,nlats
        psigshiftn(:,j,:) = psig(:,j-1,:)
     ENDDO
     DO j=1,nlats-1
        psigshifts(:,j,:) = psig(:,j+1,:)
     ENDDO
     psigshiftn(:,1,:) = psigshifts(:,1,:)
     psigshifts(:,nlats,:) = psigshiftn(:,nlats,:)

! calculate variances
     CALL mpi_allreduce(psig**2,psigvar,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigvar = psigvar/float(nanals-1)
     WHERE(psigvar < clip) psigvar = clip
     psigvar = SQRT(psigvar)
     WHERE(psigvar < clip) psigvar = clip
     CALL mpi_allreduce(psigshiftn**2,psigshiftnvar,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftnvar = psigshiftnvar/float(nanals-1)
     CALL mpi_allreduce(psigshifts**2,psigshiftsvar,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftsvar = psigshiftsvar/float(nanals-1)
     WHERE(psigshiftnvar < clip) psigshiftnvar = clip
     psigshiftnvar = SQRT(psigshiftnvar)
     WHERE(psigshiftnvar < clip) psigshiftnvar = clip
     WHERE(psigshiftsvar < clip) psigshiftsvar = clip
     psigshiftsvar = SQRT(psigshiftsvar)
     WHERE(psigshiftsvar < clip) psigshiftsvar = clip

! calculate covariances
     CALL mpi_allreduce(psigshiftn*psig,psigshiftncov,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftncov = psigshiftncov/float(nanals-1)
     CALL mpi_allreduce(psigshifts*psig,psigshiftscov,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftscov = psigshiftscov/float(nanals-1)

! compute correlation
     psigcorrn = psigshiftncov/(psigshiftnvar*psigvar)
     WHERE (psigcorrn < clip) psigcorrn = clip
     WHERE (psigcorrn > one-clip) psigcorrn = one-clip
     psigcorrs = psigshiftscov/(psigshiftsvar*psigvar)
     WHERE (psigcorrs < clip) psigcorrs = clip
     WHERE (psigcorrs > one-clip) psigcorrs = one-clip

! calculate length scale (eqn 5 in pannekoucke, berre and desroziers)
     delta = zero ! av lat spacing in radians (nearly constant)
     
     DO j=2,nlats
        delta = delta + ABS(ASIN(gaulats(j))-ASIN(gaulats(j-1)))/(nlats-1)
     ENDDO
     
     lengthscaley = half*rearth*delta*((one/SQRT(-two*LOG(psigcorrn))) + &
          (one/SQRT(-two*LOG(psigcorrs))))
     
! compute horizontal length scale.
! shift east and west by nskip grid points
! nskip set so that deltax is nearly constant with latitude.
     DO j=1,nlats
        coslat = SQRT(one-gaulats(j)**2)
        nskip = INT(one/coslat)
        IF (nskip < 1) nskip = 1
        DO i=1,nlons-nskip
           psigshiftn(i,j,:) = psig(i+nskip,j,:)
        ENDDO
        DO i=nlons-nskip+1,nlons
           psigshiftn(i,j,:) = psig(nlons-i+1,j,:)
        ENDDO
        DO i=1+nskip,nlons
           psigshifts(i,j,:) = psig(i-nskip,j,:)
        ENDDO
        DO i=1,nskip
           psigshifts(i,j,:) = psig(nlons-i+1,j,:)
        ENDDO
     ENDDO
! do i=1,nlons-1
!    psigshiftn(i,:,:) = psig(i+1,:,:)
! enddo
! psigshiftn(nlons,:,:) = psig(1,:,:)
! do i=2,nlons
!    psigshiftn(i,:,:) = psig(i-1,:,:)
! enddo
! psigshiftn(1,:,:) = psig(nlons,:,:)

! calculate variances
     CALL mpi_allreduce(psig**2,psigvar,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigvar = psigvar/float(nanals-1)
     WHERE(psigvar < clip) psigvar = clip
     psigvar = SQRT(psigvar)
     WHERE(psigvar < clip) psigvar = clip
     CALL mpi_allreduce(psigshiftn**2,psigshiftnvar,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftnvar = psigshiftnvar/float(nanals-1)
     CALL mpi_allreduce(psigshifts**2,psigshiftsvar,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftsvar = psigshiftsvar/float(nanals-1)
     WHERE(psigshiftnvar < clip) psigshiftnvar = clip
     psigshiftnvar = SQRT(psigshiftnvar)
     WHERE(psigshiftnvar < clip) psigshiftnvar = clip
     WHERE(psigshiftsvar < clip) psigshiftsvar = clip
     psigshiftsvar = SQRT(psigshiftsvar)
     WHERE(psigshiftsvar < clip) psigshiftsvar = clip

! calculate covariances
     CALL mpi_allreduce(psigshiftn*psig,psigshiftncov,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftncov = psigshiftncov/float(nanals-1)
     CALL mpi_allreduce(psigshifts*psig,psigshiftscov,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftscov = psigshiftscov/float(nanals-1)

! compute correlation
     psigcorrn = psigshiftncov/(psigshiftnvar*psigvar)
     WHERE (psigcorrn < clip) psigcorrn = clip
     WHERE (psigcorrn > one-clip) psigcorrn = one-clip
     psigcorrs = psigshiftscov/(psigshiftsvar*psigvar)
     WHERE (psigcorrs < clip) psigcorrs = clip
     WHERE (psigcorrs > one-clip) psigcorrs = one-clip
     DO j=1,nlats
        coslat = SQRT(one-gaulats(j)**2)
        nskip = INT(one/coslat)
        IF (nskip < 1) nskip = 1
        delta = two*pi*coslat*nskip/nlons
        lengthscalex(:,j,:) = half*rearth*delta*((one/SQRT(-two*LOG(psigcorrn(:,j,:)))) + &
             (one/SQRT(-two*LOG(psigcorrs(:,j,:)))))
     ENDDO
     
! now compute vertical length scale.
! shift up and down by 1 grid point.
     DO k=2,nlevs
        psigshiftn(:,:,k) = psig(:,:,k-1)
     ENDDO
     DO k=1,nlevs-1
        psigshifts(:,:,k) = psig(:,:,k+1)
     ENDDO
     psigshiftn(:,:,1) = psigshifts(:,:,1)
     psigshifts(:,nlevs,k) = psigshiftn(:,nlevs,k)
     CALL mpi_allreduce(psigshiftn**2,psigshiftnvar,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftnvar = psigshiftnvar/float(nanals-1)
     CALL mpi_allreduce(psigshifts**2,psigshiftsvar,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftsvar = psigshiftsvar/float(nanals-1)
     WHERE(psigshiftnvar < clip) psigshiftnvar = clip
     psigshiftnvar = SQRT(psigshiftnvar)
     WHERE(psigshiftnvar < clip) psigshiftnvar = clip
     WHERE(psigshiftsvar < clip) psigshiftsvar = clip
     psigshiftsvar = SQRT(psigshiftsvar)
     WHERE(psigshiftsvar < clip) psigshiftsvar = clip

! calculate covariances
     CALL mpi_allreduce(psigshiftn*psig,psigshiftncov,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftncov = psigshiftncov/float(nanals-1)
     CALL mpi_allreduce(psigshifts*psig,psigshiftscov,nlons*nlats*nlevs,&
          &mpi_rtype,mpi_sum,mpi_comm_world,ierr)
     psigshiftscov = psigshiftscov/float(nanals-1)

! compute correlation
     psigcorrn = psigshiftncov/(psigshiftnvar*psigvar)
     WHERE (psigcorrn < clip) psigcorrn = clip
     WHERE (psigcorrn > one-clip) psigcorrn = one-clip
     psigcorrs = psigshiftscov/(psigshiftsvar*psigvar)
     WHERE (psigcorrs < clip) psigcorrs = clip
     WHERE (psigcorrs > one-clip) psigcorrs = one-clip

! calculate length scale (eqn 5 in pannekoucke, berre and desroziers)
     DO k=2,nlevs
        deltav = ABS(pressl(:,:,k)-pressl(:,:,k-1))
        vlengthscale(:,:,k) = deltav*(1./SQRT(-2.*LOG(psigcorrn(:,:,k)))) 
     ENDDO
     DO k=1,nlevs-1
        deltav = ABS(pressl(:,:,k)-pressl(:,:,k+1))
        IF (k .EQ. 1) THEN
           vlengthscale(:,:,k) = deltav*(one/SQRT(-two*LOG(psigcorrs(:,:,k)))) 
        ELSE
           vlengthscale(:,:,k) = half*vlengthscale(:,:,k) + &
                half*deltav*(one/SQRT(-two*LOG(psigcorrs(:,:,k)))) 
        ENDIF
     ENDDO

     IF (nproc == 0) THEN

        PRINT *,'@@1',lengthscalex(10,10,10),lengthscaley(10,10,10),vlengthscale(10,10,10)

        WRITE(ctrac,'(i3.3)')itrac
        filenameout='lscaletracer_'//ctrac//'.dat'

        OPEN(9,form='unformatted',access='direct',file=filenameout,&
             &recl=num_bytes_for_r_kind*nlons*nlats*nlevs)

        WRITE(9,rec=1) lengthscalex
        WRITE(9,rec=2) lengthscaley
        WRITE(9,rec=3) vlengthscale
        CLOSE(9)

        filenameout='lscaletracer_'//ctrac//'.txt'

        OPEN(19,form='formatted',file=filenameout)
        DO i=1,nlats
           DO k=1,nlevs
              WRITE(19,'(i5,f8.3,2f15.3,f10.4,e15.5)')k,ASIN(gaulats(i))*rad2deg,&
                   &SUM(lengthscalex(:,i,k))/float(nlons)*1.e-3_r_kind,&
                   &SUM(lengthscaley(:,i,k))/float(nlons)*1.e-3_r_kind,&
                   &SUM(vlengthscale(:,i,k))/float(nlons),&
                   &SUM(psigvar(:,i,k))/float(nlons)
           ENDDO
        ENDDO
        CLOSE(19)
     ENDIF

  ENDDO

  DEALLOCATE(psg,zsg)
  DEALLOCATE(psig,pressl,psigmean,psigvar)
  DEALLOCATE(psigshiftn,psigshifts,psigshiftnvar,psigshiftsvar)
  DEALLOCATE(psigshiftncov,psigshiftscov,psigcorrn,psigcorrs)
  DEALLOCATE(lengthscalex,lengthscaley,vlengthscale)
  DEALLOCATE(deltav)

  CALL mpi_barrier(mpi_comm_world,ierr)
  IF (nproc == 0) WRITE(6,*) 'all done!'
  CALL mpi_finalize(ierr)
  IF (nproc == 0 .AND. ierr /= 0) THEN
     PRINT *, 'mpi_finalize error status = ',ierr
  END IF
END PROGRAM calc_corrlengths

SUBROUTINE getsigdata(sighead,sigdata,psig,psg,pressl,zsg,nlons,nlats,nlevs,ntrunc,itrac,&
     &mpi_comm_world)
  
  USE kinds, ONLY : r_kind, i_kind
  USE sigio_module
  USE constants, ONLY: rd, grav, cp, rearth, init_constants, init_constants_derived,zero,&
       &one,r0_01,r10
  USE specmod, ONLY: sptezv_s, sptez_s, init_spec_vars
  IMPLICIT NONE
  TYPE (sigio_data), INTENT(in out) :: sigdata
  TYPE (sigio_head), INTENT(in) :: sighead
  REAL(r_kind), DIMENSION(nlons,nlats,nlevs), INTENT(out) :: psig,pressl
  REAL(r_kind), DIMENSION(nlons,nlats,nlevs+1) :: pressi
  REAL(r_kind), DIMENSION(nlons,nlats), INTENT(out) :: psg,zsg
  REAL(r_kind):: ak(nlevs+1),bk(nlevs+1)
  REAL(r_kind) :: invlap((ntrunc+1)*(ntrunc+2))
  INTEGER(i_kind), INTENT(in) :: ntrunc,nlevs,nlons,nlats,itrac,mpi_comm_world
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
     CALL mpi_abort(mpi_comm_world,101,ierr)
     STOP
  ENDIF

  CALL init_spec_vars(nlons,nlats,ntrunc,4)

!==> get tracer on gaussian grid.

  DO k=1,nlevs
     wave=sigdata%q(:,k,itrac)
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

END SUBROUTINE getsigdata

