PROGRAM calc_be_binary

! reworked from jsw in GSI trunk
! reads spectral files
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


  USE kinds, ONLY : r_kind, i_kind, num_bytes_for_r_kind,r_single
  USE constants, ONLY: init_constants, init_constants_derived, rd, grav, cp, rearth, pi,&
       &rad2deg,deg2rad,zero,one,two,half
  USE mpimod, ONLY : mpi_rtype

  IMPLICIT NONE

  CHARACTER(len=500) filenamein
  CHARACTER(len=120) filenameout
  CHARACTER(len=10) datestring
  INTEGER(i_kind) ::  iret,nlats,nlons,nlevs,ntrac,ntrunc,itrac,k,ierr,nanals,&
       nanal,numproc,nproc,inunit,outunit,&
       i,j,nskip,km,kp
  REAL(r_kind):: delta,coslat
  CHARACTER(len=4) charnlons,charnlats,charnanal
!gaulats sins of gaussian latitudes - to be consistent with JSW's code
  REAL(r_kind), DIMENSION(:,:), ALLOCATABLE :: deltav, psg, &
       psgmean, zsg
  REAL(r_kind), DIMENSION(:,:,:), ALLOCATABLE :: psigshiftn,psigshifts,&
       psig,psigmean,psigvar,psigcorrn,psigcorrs,pressl,pressi,&
       psigshiftnvar,psigshiftsvar,psigshiftncov,psigshiftscov,&
       &lengthscalex,lengthscaley,vlengthscale

  REAL(r_kind), DIMENSION(:), ALLOCATABLE :: pk,bk
  REAL(r_kind) :: clip
  REAL(r_kind), DIMENSION(:), ALLOCATABLE :: gaulats
  REAL(r_kind) :: kap,kapr,kap1

  INTEGER :: im,jm,lm,ntracers,ii
  REAL,  DIMENSION(:), ALLOCATABLE :: record,vrecord,lat,lon

  CHARACTER(len=16) :: vname

! mpi definitions.


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

  kap = rd/cp
  kapr = cp/rd
  kap1 = kap + one
  
  nanal = nproc + 1
  WRITE(charnanal,'(i3.3)') nanal

! read each ensemble member fhdfi forecast.

  filenamein = "bin_"//datestring//"_mem"//TRIM(charnanal)

  inunit = 7
  OPEN(unit=inunit,file=TRIM(filenamein),form='unformatted',status='old',iostat=ierr)
  IF (ierr /= 0) THEN
     PRINT *,'cannot read file ',filenamein,ierr
     FLUSH(6)
     FLUSH(0)
     CALL mpi_abort(mpi_comm_world,101,ierr)
     STOP
  END IF
  
  READ(inunit)im,jm,lm,ntracers

  nlons=im
  nlats=jm
  nlevs=lm
  ntrac=ntracers

  IF (nproc == 0) THEN
     PRINT *,filenamein
     PRINT *,'nlons,nlats,nlevs,ntrunc,ntrac=',nlons,nlats,nlevs,ntrac
  ENDIF

  ALLOCATE(record(nlons*nlats),vrecord(nlevs+1),lat(nlats),lon(nlons))
  ALLOCATE(gaulats(nlats))
  ALLOCATE(psig(nlons,nlats,nlevs))
  ALLOCATE(psg(nlons,nlats))
  ALLOCATE(zsg(nlons,nlats))
  ALLOCATE(psgmean(nlons,nlats))
  ALLOCATE(pressi(nlons,nlats,nlevs+1))
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
  ALLOCATE(pk(nlevs+1),bk(nlevs+1))

  READ(inunit)vname
  IF (TRIM(vname) /= 'pk') CALL mpi_abort(mpi_comm_world,101,ierr)
  READ(inunit)vrecord
  pk=vrecord
  READ(inunit)vname
  IF (TRIM(vname) /= 'bk') CALL mpi_abort(mpi_comm_world,101,ierr)
  READ(inunit)vrecord
  bk=vrecord
  READ(inunit)vname
  IF (TRIM(vname) /= 'lat') CALL mpi_abort(mpi_comm_world,101,ierr)
  READ(inunit)record(1:jm)
  lat=deg2rad*record(1:jm)
  gaulats=SIN(lat)
  READ(inunit)vname
  IF (TRIM(vname) /= 'lon') CALL mpi_abort(mpi_comm_world,101,ierr)
  READ(inunit)record(1:im)
  lon=record(1:im)
  READ(inunit)vname
  IF (TRIM(vname) /= 'pres') CALL mpi_abort(mpi_comm_world,101,ierr)
  READ(inunit)record

  ii=0
  DO j=1,nlats
     DO i=1,nlons
        ii=ii+1
        psg(i,j)=record(ii)
     ENDDO
  ENDDO
  
  DO k=1,nlevs+1
     DO j=1,nlats
        pressi(:,j,k) = pk(k)+bk(k)*psg(:,j)
     ENDDO
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
  
  DO itrac=1,ntrac

     READ(inunit)vname
     IF (nproc == 0) PRINT *,TRIM(vname)
     DO k=1,nlevs
        READ(inunit)record
        ii=0
        DO j=1,nlats
           DO i=1,nlons
              ii=ii+1
              psig(i,j,k)=record(ii)
           ENDDO
        ENDDO
     ENDDO

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
        delta = delta + ABS(lat(j)-lat(j-1))/(nlats-1)
     ENDDO
     
     lengthscaley = half*rearth*delta*((one/SQRT(-two*LOG(psigcorrn))) + &
          (one/SQRT(-two*LOG(psigcorrs))))
     
     WHERE(lengthscaley < clip) lengthscaley=clip

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
        WHERE(lengthscalex < clip) lengthscalex=clip
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
!original jsw

!     DO k=2,nlevs
!        deltav = ABS(pressl(:,:,k)-pressl(:,:,k-1))
!        vlengthscale(:,:,k) = deltav*(one/SQRT(-two*LOG(psigcorrn(:,:,k)))) 
!     ENDDO
!     DO k=1,nlevs-1
!        deltav = ABS(pressl(:,:,k)-pressl(:,:,k+1))
!        IF (k .EQ. 1) THEN
!           vlengthscale(:,:,k) = deltav*(one/SQRT(-two*LOG(psigcorrs(:,:,k)))) 
!        ELSE
!           vlengthscale(:,:,k) = half*vlengthscale(:,:,k) + &
!                half*deltav*(one/SQRT(-two*LOG(psigcorrs(:,:,k)))) 
!        ENDIF
!     ENDDO


!from rizvi manuscript on WRF to GSI
     DO k=1,nlevs
        kp=MIN(nlevs,k+1)
        km=MAX(1,k-1)
        deltav = ABS(pressl(:,:,kp)-pressl(:,:,km))
        vlengthscale(:,:,k) = deltav*SQRT(one/(ABS(two-psigcorrn(:,:,k)-psigcorrs(:,:,k))))
     ENDDO

     IF (nproc == 0) THEN

        IF (itrac == 1) THEN
           filenameout='be_tracers_base.bin'
           OPEN(9,file=filenameout,form='unformatted')
           WRITE(9) INT(nlons,i_kind),INT(nlats,i_kind),INT(nlevs,i_kind),INT(nanals,i_kind)
           WRITE(9) lat*REAL(rad2deg,r_single)
           WRITE(9) REAL(pressl,r_single)
           DO k=1,nlevs+1
              pressi(:,:,k) = -LOG(pk(k)/psg+bk(k))
           ENDDO
           WRITE(9) REAL(pressi,r_single)
           DEALLOCATE(pressi)
           CLOSE(9)
        ENDIF
        
        filenameout='be_tracer_'//TRIM(vname)//'.bin'
        OPEN(9,file=filenameout,form='unformatted')
        WRITE(9) INT(nlons,i_kind),INT(nlats,i_kind),INT(nlevs,i_kind),INT(nanals,i_kind)
        WRITE(9) REAL(psigvar,r_single)
        WRITE(9) REAL(lengthscalex,r_single)
        WRITE(9) REAL(lengthscaley,r_single)
        WRITE(9) REAL(vlengthscale,r_single)
        CLOSE(9)
        
        filenameout='be_tracer_'//TRIM(vname)//'.txt'

        OPEN(19,form='formatted',file=filenameout)
        DO i=1,nlats
           DO k=1,nlevs
              WRITE(19,'(i5,f8.3,e15.5,2f15.3,f10.4)')&
                   &k,lat(i)*rad2deg,&
                   &SUM(psigvar(:,i,k))/float(nlons),&
                   &SUM(lengthscalex(:,i,k))/float(nlons)*1.e-3_r_kind,&
                   &SUM(lengthscaley(:,i,k))/float(nlons)*1.e-3_r_kind,&
                   &SUM(vlengthscale(:,i,k))/float(nlons)
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
  DEALLOCATE(deltav,pk,bk)

  CALL mpi_barrier(mpi_comm_world,ierr)
  IF (nproc == 0) WRITE(6,*) 'all done!'
  CALL mpi_finalize(ierr)
  IF (nproc == 0 .AND. ierr /= 0) THEN
     PRINT *, 'mpi_finalize error status = ',ierr
  END IF
END PROGRAM calc_be_binary

