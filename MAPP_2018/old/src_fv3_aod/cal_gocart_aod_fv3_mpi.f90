PROGRAM cal_gocart_aod_fv3_mpi

!Mariusz Pagowski Apr, 2017

  USE netcdf
  USE module_netcdf_io_generic
  USE module_kinds

  USE kinds,ONLY: r_kind,i_kind,r_single
  USE constants, ONLY : max_varname_length,zero,one,ten,one_tenth,h300,&
       &r100,rd_over_cp_mass,rd,cp,&
       &init_constants, init_constants_derived
  USE crtm_interface, ONLY: init_crtm, call_crtm, destroy_crtm
  USE module_domain, ONLY : nsig,nlat,nlon,ntime
  USE module_fv3_gocart, ONLY : naero_gocart_fv3, n_aerosols_crtm, &
       &aeronames_gocart_fv3, aeronames_gocart_fv3_gsi


  IMPLICIT NONE

  INTEGER(i_kind), PARAMETER :: nchan_viirs=11,nchan_modis=20
  INTEGER(i_kind), PARAMETER :: nchan_nnr=6

!run program codes/src_aod/get_crtm_vawelengths.sh to find indices for crtm
!matching wavelenghts for crtm channels are approximate

  INTEGER(i_kind), DIMENSION(nchan_nnr), PARAMETER :: &
!viirs: 443.,486.,486.,551.,671.,861.
       &nchan_viirs_select=[2,3,11,4,5,7],&
!modis: 442.,466.,530.,553.,666.,867.
       &nchan_modis_select=[9,3,10,4,13,16]

  INTEGER(i_kind), DIMENSION(nchan_nnr) :: nchanl_select

  INTEGER(i_kind), PARAMETER :: nchan_viirs_select_550=4,nchan_modis_select_550=4
  INTEGER(i_kind) :: nchanl_select_550

  CHARACTER(len=NF90_MAX_NAME) :: string

  CHARACTER(10) :: obstype
  CHARACTER(20) :: isis

  REAL(r_kind),PARAMETER:: qsmall  = 1.e-6_r_kind, &
       &aero_small=1.e-16_r_kind
  REAL(r_kind)    :: ppmv_conv = 96.06_r_kind/28.964_r_kind*1.0e+3_r_kind
  INTEGER, PARAMETER :: nmaxhoures=25
  LOGICAL :: ice

  INTEGER(i_kind) :: nchanl
  INTEGER(i_kind) :: i,j,k,it,ii,iii

  

  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: sulf,bc1,bc2,oc1,oc2,&
       &dust1,dust2,dust3,dust4,dust5,seas1,seas2,seas3,seas4,seas5
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: qvapor

  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:) :: pres_r,tsen_r,qsat_r
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:) :: layer_od
  REAL(r_kind), ALLOCATABLE, DIMENSION(:) :: h,q,qs,presl,presi
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:) :: aero
  REAL(r_kind) :: kap,kapr,kap1


  INTEGER(i_kind) :: status_i,ndim


  CHARACTER(len=19), DIMENSION(nmaxhoures)  :: datestr

  CHARACTER (len= 4) :: staggering=' N/A'
  CHARACTER (len= 3) :: ordering
  INTEGER(i_kind), DIMENSION(4)  :: start_index, end_index
  CHARACTER (len=80), DIMENSION(3)  ::  dimnames
  CHARACTER(len=3) :: cfhr

  INTEGER :: ncid,ifhr,&
       &iargc,YYYY,MM,DD,HH,stat,varid

  INTEGER :: nthreads,tid

  CHARACTER(len=NF90_MAX_NAME) :: varname
  CHARACTER(len=NF90_MAX_NAME) :: fname_akbk_in,fname_core_in,fname_tracer_in,fname_aod_out

  REAL(SINGLE), ALLOCATABLE :: psfc(:,:)
  REAL(SINGLE), ALLOCATABLE :: ak(:),bk(:)
  REAL(SINGLE), ALLOCATABLE :: aodsfc(:,:,:,:),aodlayers(:,:,:,:,:)
  REAL(SINGLE), ALLOCATABLE :: aodsfc_nproc(:,:,:)
  REAL(SINGLE), ALLOCATABLE :: vardata(:,:,:,:,:)
  REAL(SINGLE), DIMENSION(nchan_nnr), PARAMETER :: nnr_channels=&
       &[440_SINGLE,470_SINGLE,500_SINGLE,550_SINGLE,660_SINGLE,870_SINGLE]



!mpi stuff
  INTEGER, ALLOCATABLE, DIMENSION(:) :: jbegin,jend,displs,sendcounts
  REAL, ALLOCATABLE, DIMENSION(:) :: sendbuf,recvbuf
  INTEGER :: jextra,num_j_groups,nx

  INTEGER, DIMENSION(2) :: maxlocs

! mpi definitions
  INCLUDE 'mpif.h'
  INTEGER MPI_Status(MPI_STATUS_SIZE),numprocs,nproc,iret,ierr

  CALL MPI_Init(iret)
! nproc is process number, numprocs is total number of processes.
  CALL MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
  CALL MPI_Comm_size(MPI_COMM_WORLD,numprocs,iret)

  IF (iargc() < 4) THEN
     WRITE(6,*)'4 input fields required - stopping'
     CALL MPI_Abort(MPI_COMM_WORLD,101,iret)
  ENDIF

  cfhr='001'
  obstype='viirs_aod'


!  CALL getarg(1,fname_in)
!  CALL getarg(2,cfhr)
!  CALL getarg(3,obstype)
!  CALL getarg(4,fname_out)

  fname_akbk_in='/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/build/fv3-bundle/fv3-jedi/test/Data/inputs/gfs_aero_c12/akbk.nc'
  fname_core_in='/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/build/fv3-bundle/fv3-jedi/test/Data/inputs/gfs_aero_c12/ensmean/20180415.000000.fv_core.res.tile1.nc'
  fname_tracer_in='/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/jedi/build/fv3-bundle/fv3-jedi/test/Data/inputs/gfs_aero_c12/ensmean/20180415.000000.fv_tracer.res.tile1.nc'

  fname_aod_out='/scratch1/BMC/gsd-fv3-dev/MAPP_2018/pagowski/tmpdir/aod_test.nc'


  READ(cfhr,'(i3.1)')  ifhr

!  READ(analdate(1:4),'(i4)')  YYYY
!  READ(analdate(5:6),'(i2)')  MM
!  READ(analdate(7:8),'(i2)')  DD
!  READ(analdate(9:10),'(i2)') HH

  obstype='viirs_aod'

  IF (TRIM(obstype) == 'viirs_aod') THEN
     isis='v.viirs-m_npp'
     nchanl=nchan_viirs
     nchanl_select=nchan_viirs_select
     nchanl_select_550=nchan_viirs_select_550
  ELSE IF (TRIM(obstype) == 'modis_aod') THEN     
     isis='v.modis_aqua'
     nchanl=nchan_modis
     nchanl_select=nchan_modis_select
     nchanl_select_550=nchan_modis_select_550
  ELSE
     IF (nproc == 0) PRINT *,'Unknown satellite type - Stopping'
     CALL mpi_abort(mpi_comm_world, iret)
  ENDIF


  CALL init_constants(.TRUE.)
  CALL init_constants_derived()

  kap = rd/cp
  kapr = cp/rd
  kap1 = kap + one

  aeronames_gocart_fv3_gsi=aeronames_gocart_fv3

!until here all cores work together
!here node=0 needs to read qvapor and fcst to cores
!same next for tsen
!@mzp

!only nproc=0 reads netcdf
  
  IF (nproc == 0) THEN
     
     varname='DELP'
     CALL netcdf_read(fname_core_in,varname,vardata)
     nlon=SIZE(vardata,1)
     nlat=SIZE(vardata,2)
     nsig=SIZE(vardata,3)
     ALLOCATE(psfc(nlon,nlat))

     psfc=SUM(vardata(:,:,:,ifhr,1),3)

     ALLOCATE(ak(nsig+1),bk(nsig+1))

     varname='ak'
     CALL netcdf_read(fname_akbk_in,varname,vardata)
     ak=vardata(:,1,1,1,1)

     varname='bk'
     CALL netcdf_read(fname_akbk_in,varname,vardata)
     bk=vardata(:,1,1,1,1)

     psfc=psfc+ak(1)

     ALLOCATE(sendbuf(nlon*nlat))

     ii=0
     
     DO j=1,nlon
        DO i=1,nlat
           ii=ii+1
           sendbuf(ii)=psfc(j,i)
        END DO
     END DO

     DEALLOCATE(psfc)

     ierr=0
   
  ENDIF

  CALL mpi_bcast(ierr,1,mpi_integer,0,mpi_comm_world,iret)  

  IF (ierr /= 0) THEN
     CALL MPI_Barrier(MPI_COMM_WORLD,iret)
     CALL MPI_Finalize(iret)
     IF (iret .NE. 0) THEN
        PRINT *, 'MPI_Finalize error status = ',iret
     END IF
     CALL mpi_abort(mpi_comm_world, iret)
  ENDIF

  CALL mpi_bcast(nlon,1,mpi_integer,0,mpi_comm_world,iret)
  CALL mpi_bcast(nlat,1,mpi_integer,0,mpi_comm_world,iret)
  CALL mpi_bcast(nsig,1,mpi_integer,0,mpi_comm_world,iret)

  ALLOCATE(jbegin(0:numprocs),jend(0:numprocs-1),displs(0:numprocs-1),sendcounts(0:numprocs-1))

  num_j_groups=nlon/numprocs
  jextra=nlon-num_j_groups*numprocs
  
  jbegin(0)=1
  IF(jextra > 0) THEN
     DO j=1,jextra
        jbegin(j)=jbegin(j-1)+1+num_j_groups
     END DO
  END IF
  DO j=jextra+1,numprocs
     jbegin(j)=jbegin(j-1)+num_j_groups
  END DO

  DO j=0,numprocs-1
     jend(j)=MIN(jbegin(j+1)-1,nlon)
  END DO

  nx=jend(nproc)-jbegin(nproc)+1

  IF (.NOT. ALLOCATED(ak)) ALLOCATE(ak(nsig+1)) 
  IF (.NOT. ALLOCATED(bk)) ALLOCATE(bk(nsig+1)) 

  CALL mpi_bcast(ak,nsig+1,mpi_real,0,mpi_comm_world,iret)
  CALL mpi_bcast(bk,nsig+1,mpi_real,0,mpi_comm_world,iret)

  DO j=0,numprocs-1
     sendcounts(j)=(jend(j)-jbegin(j)+1)*nlat
  ENDDO

  displs(0)=0
  DO j=1,numprocs-1
     displs(j)=displs(j-1)+sendcounts(j-1)
  ENDDO

  ALLOCATE(recvbuf(sendcounts(nproc)))
  
  recvbuf=0.

!scatter psfc
  CALL mpi_scatterv(sendbuf,sendcounts,displs,mpi_real, &
       recvbuf,sendcounts(nproc),mpi_real,0,mpi_comm_world,iret)

  ALLOCATE(psfc(nx,nlat))
  
  ii=0
  
  DO j=1,nx
     DO i=1,nlat
        ii=ii+1
        psfc(j,i)=recvbuf(ii)
     ENDDO
  ENDDO

  IF (nproc == 0) THEN

     DEALLOCATE(sendbuf)
     
     ALLOCATE(sendbuf(nlon*nlat*nsig))

     varname='sphum'
     CALL netcdf_read(fname_tracer_in,varname,vardata)
     ii=0
     
     DO j=1,nlon
        DO k=1,nsig
           DO i=1,nlat
              ii=ii+1
              sendbuf(ii)=vardata(j,i,k,ifhr,1)
           END DO
        END DO
     END DO
  ENDIF
  
  CALL mpi_bcast(ierr,1,mpi_integer,0,mpi_comm_world,iret)

  IF (ierr /= 0) THEN
     CALL MPI_Barrier(MPI_COMM_WORLD,iret)
     CALL MPI_Finalize(iret)
     IF (iret .NE. 0) THEN
        PRINT *, 'MPI_Finalize error status = ',iret
     END IF
     CALL mpi_abort(mpi_comm_world, 1191,iret)
  ENDIF


!recalculate displs and sendcounts for 3d array

  DO j=0,numprocs-1
     sendcounts(j)=(jend(j)-jbegin(j)+1)*nlat*nsig
  ENDDO
  
  displs(0)=0
  DO j=1,numprocs-1
     displs(j)=displs(j-1)+sendcounts(j-1)
  ENDDO


  DEALLOCATE(recvbuf)
  ALLOCATE(recvbuf(sendcounts(nproc)))
  
  recvbuf=0.

  CALL mpi_scatterv(sendbuf,sendcounts,displs,mpi_real, &
       recvbuf,sendcounts(nproc),mpi_real,0,mpi_comm_world,iret)

  ALLOCATE(qvapor(nx,nlat,nsig))

  ii=0
  
  DO j=1,nx
     DO k=1,nsig
        DO i=1,nlat
           ii=ii+1
           qvapor(j,i,k)=recvbuf(ii)
        END DO
     END DO
  END DO

  IF (nproc == 0) THEN

     varname='T'
     CALL netcdf_read(fname_core_in,varname,vardata)

     ii=0
     
     DO j=1,nlon
        DO k=1,nsig
           DO i=1,nlat
              ii=ii+1
              sendbuf(ii)=vardata(j,i,k,ifhr,1)
           END DO
        END DO
     END DO
     
  ENDIF

  recvbuf=0.

  CALL mpi_scatterv(sendbuf,sendcounts,displs,mpi_real, &
       recvbuf,sendcounts(nproc),mpi_real,0,mpi_comm_world,iret)

  ALLOCATE(tsen_r(nx,nlat,nsig),pres_r(nx,nlat,nsig),qsat_r(nx,nlat,nsig))
  ALLOCATE(presi(nsig+1))

  ii=0
  
  DO j=1,nx
     DO i=1,nlat
        DO k=1,nsig+1
           presi(k)=ak(k)+bk(k)*psfc(j,i)
        ENDDO
        DO k=1,nsig
           pres_r(j,i,k)=1.e-3_r_kind*(&
                &(presi(k)**kap1-presi(k+1)**kap1)/&
                (kap1*(presi(k)-presi(k+1))))**kapr
        ENDDO
     ENDDO
  ENDDO
  
  DO j=1,nx
     DO k=1,nsig
        DO i=1,nlat
           ii=ii+1
           tsen_r(j,i,k)=recvbuf(ii)
        END DO
     END DO
  END DO

  DEALLOCATE(presi)

!remember that genqsat is different for a global model - recheck with the current version
  ice=.TRUE.
  CALL genqsat(qsat_r,tsen_r,pres_r,nx,nlat,nsig,ice)

  DEALLOCATE(pres_r)

  ALLOCATE(sulf(nx,nlat,nsig),bc1(nx,nlat,nsig),bc2(nx,nlat,nsig),&
       &oc1(nx,nlat,nsig),oc2(nx,nlat,nsig),&
       &dust1(nx,nlat,nsig),dust2(nx,nlat,nsig),&
       &dust3(nx,nlat,nsig),dust4(nx,nlat,nsig),dust5(nx,nlat,nsig),&
       &seas1(nx,nlat,nsig),seas2(nx,nlat,nsig),&
       &seas3(nx,nlat,nsig),seas4(nx,nlat,nsig),seas5(nx,nlat,nsig))
  
  DO iii=1,naero_gocart_fv3
     
     varname=TRIM(aeronames_gocart_fv3(iii))
     
     IF (nproc == 0) THEN
        
        CALL netcdf_read(fname_tracer_in,varname,vardata)

        ii=0
        
        DO j=1,nlon
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 sendbuf(ii)=vardata(j,i,k,ifhr,1)
              END DO
           END DO
        END DO

     ENDIF

     recvbuf=0.

     CALL mpi_scatterv(sendbuf,sendcounts,displs,mpi_real, &
          recvbuf,sendcounts(nproc),mpi_real,0,mpi_comm_world,iret)

     ii=0

     SELECT CASE ( TRIM(aeronames_gocart_fv3(iii)) )
     CASE ('sulf')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 sulf(j,i,k)=recvbuf(ii) 
              ENDDO
           ENDDO
        ENDDO
     CASE ('bc1')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 bc1(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('bc2')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 bc2(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('oc1')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 oc1(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('oc2')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 oc2(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('dust1')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 dust1(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('dust2')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 dust2(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('dust3')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 dust3(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('dust4')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 dust4(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('dust5')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 dust5(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('seas1')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 seas1(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('seas2')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 seas2(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('seas3')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 seas3(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('seas4')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 seas4(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('seas5')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 seas5(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE default
        WRITE(6,*)'Unknown aerosol'
        CALL MPI_Abort(MPI_COMM_WORLD,124,iret)
     END SELECT
  ENDDO

  CALL init_crtm(nchanl,naero_gocart_fv3,isis,obstype)

  IF (ALLOCATED(vardata)) DEALLOCATE(vardata)
  ALLOCATE(h(nsig),q(nsig),qs(nsig),presl(nsig),presi(nsig+1))
  ALLOCATE(layer_od(nsig,nchanl))

  ALLOCATE(aero(nsig,naero_gocart_fv3),aodsfc_nproc(nx,nlat,nchanl))

  DO j=1,nx

     PRINT *,nx-j+1,nproc

     DO i=1,nlat

        DO k=1,nsig+1
           presi(k)=ak(k)+bk(k)*psfc(j,i)
        ENDDO

        DO k=1,nsig
           h(k)=tsen_r(j,i,k)
           qs(k)=qsat_r(j,i,k)
           q(k)=MAX(qvapor(j,i,k),qsmall)
           presl(k)=1.e-3_r_kind*(&
                &(presi(k)**kap1-presi(k+1)**kap1)/&
                &(kap1*(presi(k)-presi(k+1))))**kapr
           presi(k)=1.e-3_r_kind*presi(k)

           DO ii=1,naero_gocart_fv3
              SELECT CASE ( TRIM(aeronames_gocart_fv3(ii)) )
              CASE ('sulf')
                 aero(k,ii)=MAX(sulf(j,i,k),aero_small)
              CASE ('bc1')
                 aero(k,ii)=MAX(bc1(j,i,k),aero_small)
              CASE ('bc2')
                 aero(k,ii)=MAX(bc2(j,i,k),aero_small)
              CASE ('oc1')
                 aero(k,ii)=MAX(oc1(j,i,k),aero_small)
              CASE ('oc2')
                 aero(k,ii)=MAX(oc2(j,i,k),aero_small)
              CASE ('dust1')
                 aero(k,ii)=MAX(dust1(j,i,k),aero_small)
              CASE ('dust2')
                 aero(k,ii)=MAX(dust2(j,i,k),aero_small)
              CASE ('dust3')
                 aero(k,ii)=MAX(dust3(j,i,k),aero_small)
              CASE ('dust4')
                 aero(k,ii)=MAX(dust4(j,i,k),aero_small)
              CASE ('dust5')
                 aero(k,ii)=MAX(dust5(j,i,k),aero_small)
              CASE ('seas1')
                 aero(k,ii)=MAX(seas1(j,i,k),aero_small)
              CASE ('seas2')
                 aero(k,ii)=MAX(seas2(j,i,k),aero_small)
              CASE ('seas3')
                 aero(k,ii)=MAX(seas3(j,i,k),aero_small)
              CASE ('seas4')
                 aero(k,ii)=MAX(seas4(j,i,k),aero_small)
              CASE ('seas5')
                 aero(k,ii)=MAX(seas5(j,i,k),aero_small)
              END SELECT
           ENDDO
        ENDDO

        presi(nsig+1)=1.e-3_r_kind*presi(nsig+1)

        CALL call_crtm(obstype,nchanl, &
             h,q,qs,presl,presi,aero,status_i,layer_od=layer_od)

        DO ii=1,nchanl
           aodsfc_nproc(j,i,ii)=SUM(layer_od(:,ii))
        ENDDO

     ENDDO
     
  ENDDO

  PRINT *,'@@@',MINVAL(aodsfc_nproc),MAXVAL(aodsfc_nproc)

  maxlocs=MAXLOC(aodsfc_nproc(:,:,nchanl_select_550))

  CALL destroy_crtm
  
  DEALLOCATE(sulf,bc1,bc2,oc1,oc2,dust1,dust2,dust3,dust4,dust5,seas1,seas2,seas3,seas4,seas5)
  DEALLOCATE(qsat_r,tsen_r,qvapor,psfc)
  DEALLOCATE(aero,h,q,qs,presl,presi,layer_od)
  DEALLOCATE(ak,bk)
  
  DO j=0,numprocs-1
     sendcounts(j)=(jend(j)-jbegin(j)+1)*nlat*nchanl
  ENDDO

  displs(0)=0
  DO j=1,numprocs-1
     displs(j)=displs(j-1)+sendcounts(j-1)
  ENDDO

  IF (ALLOCATED(sendbuf)) DEALLOCATE(sendbuf)
  DEALLOCATE(recvbuf)
  
  ALLOCATE(sendbuf(sendcounts(nproc)))

  ii=0

  DO j=1,nx
     DO i=1,nlat
        DO k=1,nchanl
           ii=ii+1
           sendbuf(ii)=aodsfc_nproc(j,i,k)
        ENDDO
     ENDDO
  ENDDO

  DEALLOCATE(aodsfc_nproc)

  ntime=1
  ALLOCATE(recvbuf(nlon*nlat*nchanl))
  recvbuf=0.
  
  CALL mpi_gatherv(sendbuf,sendcounts(nproc),mpi_real, &
       recvbuf,sendcounts,displs,mpi_real,0,mpi_comm_world,iret)
  
  IF (nproc == 0) THEN
     
     ALLOCATE(aodsfc(nlon,nlat,nchanl,ntime))
     it=1
     ii=0
     DO j=1,nlon
        DO i=1,nlat
           DO k=1,nchanl
              ii=ii+1
              aodsfc(j,i,k,it)=recvbuf(ii)
           ENDDO
        ENDDO
     ENDDO

     PRINT *,MINVAL(aodsfc(:,:,nchanl_select_550,it)),&
          &MAXVAL(aodsfc(:,:,nchanl_select_550,it))
     PRINT *,MINLOC(aodsfc(:,:,nchanl_select_550,it)),&
          &MAXLOC(aodsfc(:,:,nchanl_select_550,it))

     ALLOCATE(aodlayers(nlon,nlat,nsig,nchan_nnr,1))
     aodlayers=1.

     CALL netcdf_write_fv3_aod(fname_aod_out,aodsfc(:,:,nchanl_select,:),&
          &aodlayers(:,:,:,nchanl_select,:),nnr_channels)

!     OPEN(unit=binunit,file=TRIM(fname_out),form='unformatted')
!     WRITE(binunit)nlon,nlat,nchan_nnr,ntime
!     WRITE(binunit)nnr_channels
!     DO it=1,ntime
!        WRITE(binunit)aodsfc(:,:,nchanl_select,it)
!     ENDDO
!     CLOSE(binunit)
!     WRITE(6,*) 'Output done on node = 0!'

     DEALLOCATE(aodsfc,aodlayers)

  ENDIF
  
  DEALLOCATE(sendbuf,recvbuf,sendcounts,jbegin,jend)

  CALL MPI_Barrier(MPI_COMM_WORLD,iret)

  CALL MPI_Finalize(iret)
  IF (iret .NE. 0) THEN
     PRINT *, 'MPI_Finalize error status = ',iret
  ELSE
     IF (nproc == 0)  WRITE(6,*) 'All done !!!'
  END IF

END PROGRAM cal_gocart_aod_fv3_mpi
