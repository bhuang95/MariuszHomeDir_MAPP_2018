PROGRAM cal_gocart_aod_mpi

!Mariusz Pagowski Nov, 2016

  USE kinds,ONLY: r_kind,i_kind,r_single
  USE constants, ONLY : max_varname_length,one,zero,ten,one_tenth,h300,r100,rd_over_cp_mass, &
       &init_constants, init_constants_derived
  USE module_wrf_gocart
  USE module_domain, ONLY : nsig,nlat,nlon,ntime
  USE crtm_interface, ONLY: init_crtm, call_crtm, destroy_crtm
  USE module_utils, ONLY: upper2lower, replace_text

  IMPLICIT NONE

  INTEGER(i_kind), PARAMETER :: nchan_viirs=11,nchan_modis=20,nchan_abi=6
  INTEGER(i_kind), PARAMETER :: nchan_viirs_select=4,&
       &nchan_modis_select=4, nchan_abi_select=1
  CHARACTER(len=max_varname_length) :: string
  
  CHARACTER(10) :: obstype
  CHARACTER(20) :: isis
  
  REAL(r_kind),PARAMETER:: qsmall  = 1.e-6_r_kind
  REAL(r_kind)    :: ppmv_conv = 96.06_r_kind/28.964_r_kind*1.0e+3_r_kind
  INTEGER, PARAMETER :: nmaxhoures=25
  LOGICAL :: ice

  INTEGER(i_kind) :: nchanl,nchanl_select
  INTEGER(i_kind) :: i,j,k,it,ii,iii,itime

  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: sulf,bc1,bc2,oc1,oc2,&
       &dust1,dust2,dust3,dust4,dust5,seas1,seas2,seas3,seas4,p25
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: qvapor,tmp3d
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:) :: ps,tmp2d
  REAL(r_single), ALLOCATABLE, DIMENSION(:) :: eta,etap
  REAL(r_single) :: ptop

  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:) :: pres_r,tsen_r,qsat_r
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:,:) :: total_aod
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:) :: total_aod_nproc
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:) :: layer_od
  REAL(r_kind), ALLOCATABLE, DIMENSION(:) :: h,q,qs,presl,presi
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:) :: aero

  INTEGER(i_kind) :: ncunit, binunit=112,status_i,ndim
  CHARACTER(len=250) :: flnm_in,flnm_out

  INTEGER(i_kind) :: iyear,imonth,iday,ihour,iminute,isecond
  CHARACTER(len=19), DIMENSION(nmaxhoures)  :: datestr
  CHARACTER (len=31) :: varname
  CHARACTER (len= 4) :: staggering=' N/A'
  CHARACTER (len= 3) :: ordering
  INTEGER(i_kind) :: wrftype,  wrf_real
  INTEGER(i_kind), DIMENSION(4)  :: start_index, end_index
  CHARACTER (len=80), DIMENSION(3)  ::  dimnames
  CHARACTER (len=80) :: sysdepinfo

  INTEGER :: iargc

  INTEGER :: nthreads,tid
  
  CHARACTER(len=2) :: ctime

!mpi stuff
  INTEGER, ALLOCATABLE, DIMENSION(:) :: jbegin,jend,displs,sendcounts
  REAL, ALLOCATABLE, DIMENSION(:) :: sendbuf,recvbuf
  INTEGER :: jextra,num_j_groups,nx

  INTEGER, DIMENSION(2) :: maxlocs

! mpi definitions.
  INCLUDE 'mpif.h'
  INTEGER MPI_Status(MPI_STATUS_SIZE),numprocs,nproc,iret
  
  CALL MPI_Init(iret)
! nproc is process number, numprocs is total number of processes.
  CALL MPI_Comm_rank(MPI_COMM_WORLD,nproc,iret)
  CALL MPI_Comm_size(MPI_COMM_WORLD,numprocs,iret)

  IF (iargc() < 3) THEN
     WRITE(6,*)'Input/Output file names required and obstype - stopping'
     CALL MPI_Abort(MPI_COMM_WORLD,101,iret)
  ENDIF

  CALL getarg(1,flnm_in)
  CALL getarg(2,flnm_out)
  CALL getarg(3,obstype)
  IF (iargc() == 4) THEN
     CALL getarg(4,ctime)
     READ(ctime,'(i2)')itime
  ELSE
     itime=1
  ENDIF

  IF (TRIM(obstype) == 'viirs_aod') THEN
     isis='v.viirs-m_npp'
     nchanl=nchan_viirs
     nchanl_select=nchan_viirs_select
  ELSE IF (TRIM(obstype) == 'modis_aod') THEN
     isis='v.modis_aqua'
     nchanl=nchan_modis
     nchanl_select=nchan_modis_select
  ELSE IF (TRIM(obstype) == 'abi_aod') then
     isis='v.abi_gr'
     nchanl=nchan_abi
     nchanl_select=nchan_abi_select
  ELSE
     IF (nproc == 0) PRINT *,'Unknown satellite type - Stopping'
     CALL mpi_abort(mpi_comm_world, iret)
  ENDIF

  CALL init_constants(.TRUE.)
  CALL init_constants_derived()

  DO i=1,naero_gocart_wrf
     string=upper2lower(aeronames_gocart_wrf(i))
     aeronames_gocart_wrf_gsi(i)=replace_text(string,'_','')
  ENDDO

  wrf_real=104

!until here all cores work together
!here node=0 needs to read qvapor and bcst to cores
!same next for tsen
!@mzp

!only nproc=0 reads netcdf
  
  IF (nproc == 0) THEN

     CALL ext_ncd_ioinit(sysdepinfo,status_i)
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i
        CALL MPI_Abort(MPI_COMM_WORLD,101,iret)
     ENDIF

     CALL ext_ncd_open_for_read(TRIM(flnm_in),0,0, "", ncunit, status_i)
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i
        CALL MPI_Abort(MPI_COMM_WORLD,102,iret)
     ENDIF

     CALL ext_ncd_get_next_time(ncunit, datestr(1), status_i)
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i
        CALL MPI_Abort(MPI_COMM_WORLD,103,iret)
     ENDIF
    
     varname='P_TOP'
     
     CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
          &staggering,start_index,end_index,wrftype,status_i)
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,104,iret)
     ENDIF
     
     READ(datestr(1),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,&
          &ihour,iminute,isecond
     
     it=1
     
     DO WHILE (.TRUE.)
        
!assume hourly intervals
        
        CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
             ptop,wrf_real,0,0,0,ordering,          &
             staggering, dimnames ,               &
             start_index,end_index,               & !dom
             start_index,end_index,               & !mem
             start_index,end_index,               & !pat
             status_i                                 )
        IF ( status_i /= 0 .AND. it == 1)THEN
           WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
                TRIM(flnm_in),', status = ', status_i, varname
           CALL MPI_Abort(MPI_COMM_WORLD,105,iret)
        ENDIF

        IF ( status_i /= 0) EXIT

        it=it+1

        IF (it > nmaxhoures) THEN
           WRITE(6,*)'file flnm_in= ',TRIM(flnm_in),' has outputfor more than ',nmaxhoures,' times'
           status_i=1
           IF ( status_i /= 0 )THEN
              WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
                   TRIM(flnm_in),', status = ', status_i
              CALL MPI_Abort(MPI_COMM_WORLD,106,iret)
           ENDIF

        ENDIF
     
        ihour=ihour+1
        
        datestr(it)=datestr(it-1)
        
        WRITE(datestr(it)(12:13),'(i2.2)') ihour
        
     ENDDO
     
     it=itime
     
     varname='QVAPOR'
     CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
          &staggering,start_index,end_index,wrftype,status_i) 
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,107,iret)
     ENDIF
     

     nlon=end_index(1)
     nlat=end_index(2)
     nsig=end_index(3)
     
     ALLOCATE(ps(nlon,nlat),tmp2d(nlon,nlat))
     ALLOCATE(eta(nsig),etap(nsig+1))
     
     varname='P_TOP'
     CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
          &staggering,start_index,end_index,wrftype,status_i) 
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,108,iret)
     ENDIF

     
     CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
          ptop,wrf_real,0,0,0,ordering,          &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          status_i                                 )
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,109,iret)
     ENDIF
     
     
     varname='MU'
     CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
          &staggering,start_index,end_index,wrftype,status_i) 
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,110,iret)
     ENDIF
     
     
     CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
          ps,wrf_real,0,0,0,ordering,          &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          status_i                                 )
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,111,iret)
     ENDIF

    
     varname='MUB'
     CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
       &staggering,start_index,end_index,wrftype,status_i) 
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,112,iret)
     ENDIF
     
     
     CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
          tmp2d,wrf_real,0,0,0,ordering,          &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          status_i                                 )
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,113,iret)
     ENDIF
     
     
     ps=tmp2d+ps+ptop
     
     DEALLOCATE(tmp2d)
     
     ALLOCATE(sendbuf(nlon*nlat))

     ii=0
     
     DO j=1,nlon
        DO i=1,nlat
           ii=ii+1
           sendbuf(ii)=ps(j,i)
        END DO
     END DO

     DEALLOCATE(ps)
   
     varname='ZNU'
     CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
          &staggering,start_index,end_index,wrftype,status_i) 
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,114,iret)
     ENDIF
     
     
     CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
          eta,wrf_real,0,0,0,ordering,          &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          status_i                                 )
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,115,iret)
     ENDIF
     
     varname='ZNW'
     CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
          &staggering,start_index,end_index,wrftype,status_i) 
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,116,iret)
     ENDIF

     
     CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
          etap,wrf_real,0,0,0,ordering,          &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          status_i                                 )
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,117,iret)
     ENDIF
       
  ENDIF

!  CALL mpi_bcast((/nlon,nlat,nsig/),3,mpi_integer,0,mpi_comm_world,iret)

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

  IF (.NOT. ALLOCATED(eta)) ALLOCATE(eta(nsig)) 
  IF (.NOT. ALLOCATED(etap)) ALLOCATE(etap(nsig+1)) 

  CALL mpi_bcast(ptop,1,mpi_real,0,mpi_comm_world,iret)
  CALL mpi_bcast(eta,nsig,mpi_real,0,mpi_comm_world,iret)
  CALL mpi_bcast(etap,nsig+1,mpi_real,0,mpi_comm_world,iret)

  DO j=0,numprocs-1
     sendcounts(j)=(jend(j)-jbegin(j)+1)*nlat
  ENDDO

  displs(0)=0
  DO j=1,numprocs-1
     displs(j)=displs(j-1)+sendcounts(j-1)
  ENDDO

  ALLOCATE(recvbuf(sendcounts(nproc)))
  
  recvbuf=0.

!scatter ps
  CALL mpi_scatterv(sendbuf,sendcounts,displs,mpi_real, &
       recvbuf,sendcounts(nproc),mpi_real,0,mpi_comm_world,iret)

  ALLOCATE(ps(nx,nlat))
  
  ii=0
  
  DO j=1,nx
     DO i=1,nlat
        ii=ii+1
        ps(j,i)=recvbuf(ii)
     ENDDO
  ENDDO

  IF (nproc == 0) THEN

     DEALLOCATE(sendbuf)
     
     ALLOCATE(tmp3d(nlon,nlat,nsig),sendbuf(nlon*nlat*nsig))

     varname='QVAPOR'
     
     CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
          &staggering,start_index,end_index,wrftype,status_i) 
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,118,iret)
     ENDIF
     
     
     CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
          tmp3d,wrf_real,0,0,0,ordering,          &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          status_i                                 )
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,119,iret)
     ENDIF
     

     ii=0
     
     DO j=1,nlon
        DO k=1,nsig
           DO i=1,nlat
              ii=ii+1
              sendbuf(ii)=tmp3d(j,i,k)
           END DO
        END DO
     END DO
     
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
     
     CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
          &staggering,start_index,end_index,wrftype,status_i) 
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,120,iret)
     ENDIF
     
     CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
          tmp3d,wrf_real,0,0,0,ordering,          &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          status_i                                 )
     IF ( status_i /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', status_i, varname
        CALL MPI_Abort(MPI_COMM_WORLD,121,iret)
     ENDIF


     ii=0
     
     DO j=1,nlon
        DO k=1,nsig
           DO i=1,nlat
              ii=ii+1
              sendbuf(ii)=tmp3d(j,i,k)
           END DO
        END DO
     END DO
     
  ENDIF

  recvbuf=0.

  CALL mpi_scatterv(sendbuf,sendcounts,displs,mpi_real, &
       recvbuf,sendcounts(nproc),mpi_real,0,mpi_comm_world,iret)

  ALLOCATE(tsen_r(nx,nlat,nsig),pres_r(nx,nlat,nsig),qsat_r(nx,nlat,nsig))

  ii=0
  
  DO j=1,nx
     DO k=1,nsig
        DO i=1,nlat
           pres_r(j,i,k)=1.e-3_r_kind*(eta(k)*&
                &(ps(j,i)-ptop)+ptop)
           ii=ii+1
           tsen_r(j,i,k)=(recvbuf(ii)+h300)*&
                &(pres_r(j,i,k)/r100)**rd_over_cp_mass
        END DO
     END DO
  END DO

!remember that genqsat is different for a global model - recheck with the current version
  ice=.TRUE.
  CALL genqsat(qsat_r,tsen_r,pres_r,nx,nlat,nsig,ice)

  DEALLOCATE(pres_r)

  ALLOCATE(sulf(nx,nlat,nsig),bc1(nx,nlat,nsig),bc2(nx,nlat,nsig),&
       &oc1(nx,nlat,nsig),oc2(nx,nlat,nsig),&
       &dust1(nx,nlat,nsig),dust2(nx,nlat,nsig),&
       &dust3(nx,nlat,nsig),dust4(nx,nlat,nsig),dust5(nx,nlat,nsig),&
       &seas1(nx,nlat,nsig),seas2(nx,nlat,nsig),&
       &seas3(nx,nlat,nsig),seas4(nx,nlat,nsig),p25(nx,nlat,nsig))
  
  DO iii=1,naero_gocart_wrf
     
     varname=TRIM(aeronames_gocart_wrf(iii))
     
     IF (nproc == 0) THEN
        
        CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
             &staggering,start_index,end_index,wrftype,status_i) 
        IF ( status_i /= 0 )THEN
           WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
                TRIM(flnm_in),', status = ', status_i, varname
           CALL MPI_Abort(MPI_COMM_WORLD,122,iret)
        ENDIF
     
        CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
             tmp3d,wrf_real,0,0,0,ordering,          &
             staggering, dimnames ,               &
             start_index,end_index,               & !dom
             start_index,end_index,               & !mem
             start_index,end_index,               & !pat
             status_i                                 )
        IF ( status_i /= 0 )THEN
           WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
                TRIM(flnm_in),', status = ', status_i, varname
           CALL MPI_Abort(MPI_COMM_WORLD,123,iret)
        ENDIF

        
        ii=0
        
        DO j=1,nlon
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 sendbuf(ii)=tmp3d(j,i,k)
              END DO
           END DO
        END DO

     ENDIF

     recvbuf=0.

     CALL mpi_scatterv(sendbuf,sendcounts,displs,mpi_real, &
          recvbuf,sendcounts(nproc),mpi_real,0,mpi_comm_world,iret)

     ii=0

     SELECT CASE ( TRIM(aeronames_gocart_wrf(iii)) )
     CASE ('sulf')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 sulf(j,i,k)=recvbuf(ii)*ppmv_conv 
              ENDDO
           ENDDO
        ENDDO
     CASE ('BC1')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 bc1(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('BC2')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 bc2(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('OC1')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 oc1(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('OC2')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 oc2(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('DUST_1')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 dust1(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('DUST_2')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 dust2(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('DUST_3')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 dust3(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('DUST_4')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 dust4(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('DUST_5')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 dust5(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('SEAS_1')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 seas1(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('SEAS_2')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 seas2(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('SEAS_3')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 seas3(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('SEAS_4')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 seas4(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE ('P25')
        DO j=1,nx
           DO k=1,nsig
              DO i=1,nlat
                 ii=ii+1
                 p25(j,i,k)=recvbuf(ii)
              ENDDO
           ENDDO
        ENDDO
     CASE default
        WRITE(6,*)'Unknown aerosol'
        CALL MPI_Abort(MPI_COMM_WORLD,124,iret)
     END SELECT
  ENDDO

  CALL init_crtm(nchanl,naero_gocart_wrf,isis,obstype)

  IF (ALLOCATED(tmp3d)) DEALLOCATE(tmp3d)
  ALLOCATE(h(nsig),q(nsig),qs(nsig),presl(nsig),presi(nsig+1))
  ALLOCATE(layer_od(nsig,nchanl))

  ALLOCATE(aero(nsig,naero_gocart_wrf),total_aod_nproc(nx,nlat,nchanl))

  presi(nsig+1)=1.e-3_r_kind*ptop

  DO j=1,nx

     PRINT *,nx-j+1,nproc

     DO i=1,nlat
        
        DO k=1,nsig
           h(k)=tsen_r(j,i,k)
!not sure if qvapor in GSI converted to specific humidity 
!but it should be 
           q(k)=MAX(qvapor(j,i,k)/(one+qvapor(j,i,k)),qsmall)
!           q(k)=qsmall
           qs(k)=qsat_r(j,i,k)

           presl(k)=1.e-3_r_kind*(eta(k)*(ps(j,i)-ptop) + ptop)
           presi(k)=1.e-3_r_kind*(etap(k)*(ps(j,i)-ptop) + ptop)
           
           DO ii=1,naero_gocart_wrf
              SELECT CASE ( TRIM(aeronames_gocart_wrf(ii)) )
              CASE ('sulf')
                 aero(k,ii)=sulf(j,i,k)
              CASE ('BC1')
                 aero(k,ii)=bc1(j,i,k)
              CASE ('BC2')
                 aero(k,ii)=bc2(j,i,k)
              CASE ('OC1')
                 aero(k,ii)=oc1(j,i,k)
              CASE ('OC2')
                 aero(k,ii)=oc2(j,i,k)
              CASE ('DUST_1')
                 aero(k,ii)=dust1(j,i,k)
              CASE ('DUST_2')
                 aero(k,ii)=dust2(j,i,k)
              CASE ('DUST_3')
                 aero(k,ii)=dust3(j,i,k)
              CASE ('DUST_4')
                 aero(k,ii)=dust4(j,i,k)
              CASE ('DUST_5')
                 aero(k,ii)=dust5(j,i,k)
              CASE ('SEAS_1')
                 aero(k,ii)=seas1(j,i,k)
              CASE ('SEAS_2')
                 aero(k,ii)=seas2(j,i,k)
              CASE ('SEAS_3')
                 aero(k,ii)=seas3(j,i,k)
              CASE ('SEAS_4')
                 aero(k,ii)=seas4(j,i,k)
              CASE ('P25')
                 aero(k,ii)=p25(j,i,k)
              END SELECT
           ENDDO
        ENDDO

        CALL call_crtm(obstype,nchanl, &
             h,q,qs,presl,presi,aero,status_i,layer_od=layer_od)

        DO ii=1,nchanl
           total_aod_nproc(j,i,ii)=SUM(layer_od(:,ii))
        ENDDO

     ENDDO
     
  ENDDO

  maxlocs=MAXLOC(total_aod_nproc(:,:,nchanl_select))

  CALL destroy_crtm
  
  DEALLOCATE(sulf,bc1,bc2,oc1,oc2,dust1,dust2,dust3,dust4,dust5,seas1,seas2,seas3,seas4,p25)
  DEALLOCATE(qsat_r,tsen_r,qvapor,ps)
  DEALLOCATE(aero,h,q,qs,presl,presi,layer_od)
  DEALLOCATE(eta,etap)
  
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
           sendbuf(ii)=total_aod_nproc(j,i,k)
        ENDDO
     ENDDO
  ENDDO

  DEALLOCATE(total_aod_nproc)

  ntime=1
  ALLOCATE(recvbuf(nlon*nlat*nchanl))
  recvbuf=0.
  
  CALL mpi_gatherv(sendbuf,sendcounts(nproc),mpi_real, &
       recvbuf,sendcounts,displs,mpi_real,0,mpi_comm_world,iret)
  
  IF (nproc == 0) THEN
     
     ALLOCATE(total_aod(nlon,nlat,nchanl,ntime))
     ii=0
     it=1
     DO j=1,nlon
        DO i=1,nlat
           DO k=1,nchanl
              ii=ii+1
              total_aod(j,i,k,it)=recvbuf(ii)
           ENDDO
        ENDDO
     ENDDO

     PRINT *,datestr(itime)

     PRINT *,MINVAL(total_aod(:,:,nchanl_select,it)),&
          &MAXVAL(total_aod(:,:,nchanl_select,it))
     PRINT *,MINLOC(total_aod(:,:,nchanl_select,it)),&
          &MAXLOC(total_aod(:,:,nchanl_select,it))
    
     OPEN(unit=binunit,file=TRIM(flnm_out),form='unformatted')
     WRITE(binunit)nlon,nlat,ntime
     DO it=1,ntime
        WRITE(binunit)total_aod(:,:,nchanl_select,it)
     ENDDO
     CLOSE(binunit)
     WRITE(6,*) 'Output done on node = 0!'

     DEALLOCATE(total_aod)

  ENDIF
  
  DEALLOCATE(sendbuf,recvbuf,sendcounts,jbegin,jend)
  
  CALL MPI_Barrier(MPI_COMM_WORLD,iret)

  CALL MPI_Finalize(iret)
  IF (iret .NE. 0) THEN
     PRINT *, 'MPI_Finalize error status = ',iret
  ELSE
     IF (nproc == 0)  WRITE(6,*) 'All done !!!'
  END IF

END PROGRAM cal_gocart_aod_mpi
