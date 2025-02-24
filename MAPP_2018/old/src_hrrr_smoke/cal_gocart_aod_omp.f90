PROGRAM cal_gocart_aod_omp

!MZP May 16, 2016
  USE kinds,ONLY: r_kind,i_kind,r_single
  USE constants, ONLY : max_varname_length,ten,one_tenth,h300,r100,rd_over_cp_mass, &
       &init_constants, init_constants_derived
  USE module_wrf_gocart
  USE module_domain, ONLY : nsig,nlat,nlon,ntime
  USE crtm_interface, ONLY: init_crtm, call_crtm, destroy_crtm
  USE module_utils, ONLY: upper2lower, replace_text

  USE omp_lib

  IMPLICIT NONE

  INTEGER(i_kind) :: nchan_viirs=11
  CHARACTER(len=max_varname_length) :: string
  
  CHARACTER(10) :: obstype
  CHARACTER(20) :: isis
  
  REAL(r_kind),PARAMETER:: qsmall  = 1.e-6_r_kind
  REAL(r_kind)    :: ppmv_conv = 96.06_r_kind/28.964_r_kind*1.0e+3_r_kind
  INTEGER, PARAMETER :: nmaxhoures=25
  LOGICAL :: ice

  INTEGER(i_kind) :: nchanl
  INTEGER(i_kind) :: i,j,k,it,ii
  INTEGER(i_kind) :: error_status

  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: sulf,bc1,bc2,oc1,oc2,&
       &dust1,dust2,dust3,dust4,dust5,seas1,seas2,seas3,seas4,p25
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:,:) :: tsen,qvapor,tmp3d
  REAL(r_single), ALLOCATABLE, DIMENSION(:,:) :: ps,tmp2d
  REAL(r_single), ALLOCATABLE, DIMENSION(:) :: eta,etap
  REAL(r_single) :: ptop

  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:) :: pres_r,tsen_r,qsat_r
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:,:,:) :: total_aod
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:) :: layer_od
  REAL(r_kind), ALLOCATABLE, DIMENSION(:) :: h,q,qs,presl,presi
  REAL(r_kind), ALLOCATABLE, DIMENSION(:,:) :: aero

  INTEGER(i_kind) :: ncunit, binunit=112,status, ierr, ndim
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
  
  CHARACTER(len=8) :: cdatestart,cdateend
  CHARACTER(len=10) :: ctimestart,ctimeend
  REAL :: timestart,timeend

  IF (iargc() < 2) THEN
     WRITE(6,*)'Input/Output file names required - stopping'
     STOP
  ENDIF

  CALL getarg(1,flnm_in)
  CALL getarg(2,flnm_out)

  isis='v.viirs-m_npp'
  obstype='viirs_aod'
  nchanl=nchan_viirs
  
  CALL init_constants(.TRUE.)
  CALL init_constants_derived()

  DO i=1,naero_gocart_wrf
     string=upper2lower(aeronames_gocart_wrf(i))
     aeronames_gocart_wrf_gsi(i)=replace_text(string,'_','')
  ENDDO

  wrf_real=104

  CALL ext_ncd_ioinit(sysdepinfo,status)

  CALL ext_ncd_open_for_read(TRIM(flnm_in),0,0, "", ncunit, status)
  IF ( status /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_open_for_read = ',&
          TRIM(flnm_in),', status = ', status
     STOP(10)
  ENDIF

  CALL ext_ncd_get_next_time(ncunit, datestr(1), status)
  IF ( status /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_next_time = ',&
          TRIM(flnm_in),', status = ', status, varname
     STOP(11)
  ENDIF


  WRITE(6,*)'Date= ',datestr(1)

  it=0
  varname='P_TOP'

  CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
       &staggering,start_index,end_index,wrftype,ierr)
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(12)
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
          ierr                                 )
     IF ( ierr /= 0 ) EXIT

     it=it+1
     IF (it > nmaxhoures) THEN
        WRITE(6,*)'file flnm_in= ',TRIM(flnm_in),' has outputfor more than ',nmaxhoures,' times'
        STOP(15)
     ENDIF
     
     ihour=ihour+1
     
     datestr(it)=datestr(it-1)

     WRITE(datestr(it)(12:13),'(i2.2)') ihour

  ENDDO

  ntime=it

  it=1

  varname='QVAPOR'
  CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
       &staggering,start_index,end_index,wrftype,ierr) 
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(12)
  ENDIF

  nlon=end_index(1)
  nlat=end_index(2)
  nsig=end_index(3)

  ALLOCATE(tsen(nlon,nlat,nsig),qvapor(nlon,nlat,nsig),ps(nlon,nlat),tmp2d(nlon,nlat))
  ALLOCATE(aero(nsig,naero_gocart_wrf))
  ALLOCATE(eta(nsig),etap(nsig+1))

  CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
       qvapor,wrf_real,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(13)
  ENDIF

  varname='P_TOP'
  CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
       &staggering,start_index,end_index,wrftype,ierr) 
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(12)
  ENDIF

  CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
       ptop,wrf_real,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(13)
  ENDIF

  varname='MU'
  CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
       &staggering,start_index,end_index,wrftype,ierr) 
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(12)
  ENDIF

  CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
       ps,wrf_real,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(13)
  ENDIF

  varname='MUB'
  CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
       &staggering,start_index,end_index,wrftype,ierr) 
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(12)
  ENDIF

  CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
       tmp2d,wrf_real,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(13)
  ENDIF

  ps=tmp2d+ps+ptop

  DEALLOCATE(tmp2d)

  varname='ZNU'
  CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
       &staggering,start_index,end_index,wrftype,ierr) 
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(12)
  ENDIF

  CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
       eta,wrf_real,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(13)
  ENDIF

  varname='ZNW'
  CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
       &staggering,start_index,end_index,wrftype,ierr) 
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(12)
  ENDIF

  CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
       etap,wrf_real,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(13)
  ENDIF

  varname='T'
  CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
       &staggering,start_index,end_index,wrftype,ierr) 
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(12)
  ENDIF

  CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
       tsen,wrf_real,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  IF ( ierr /= 0 )THEN
     WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
          TRIM(flnm_in),', status = ', ierr, varname
     STOP(13)
  ENDIF

  ALLOCATE(qsat_r(nlon,nlat,nsig),tsen_r(nlon,nlat,nsig),pres_r(nlon,nlat,nsig))
  
  DO i=1,nlon
     DO j=1,nlat
        DO k=1,nsig
           pres_r(i,j,k)=1.e-3_r_kind*(eta(k)*(ps(i,j)-ptop)+ptop)
           tsen_r(i,j,k)=(tsen(i,j,k)+h300)*(pres_r(i,j,k)/r100)**rd_over_cp_mass
        ENDDO
     ENDDO
  ENDDO

  ALLOCATE(layer_od(nsig,nchanl),total_aod(nlon,nlat,nchanl,ntime))
  ALLOCATE(h(nsig),q(nsig),qs(nsig),presl(nsig),presi(nsig+1))

!because of memory issues better to call routine multiple times
  ice=.TRUE.
  
!remember that genqsat is different for a global model - recheck with the current version

  CALL genqsat(qsat_r,tsen_r,pres_r,nlat,nlon,nsig,ice)

  DEALLOCATE(tsen,pres_r)


  ALLOCATE(sulf(nlon,nlat,nsig),bc1(nlon,nlat,nsig),bc2(nlon,nlat,nsig),&
       &oc1(nlon,nlat,nsig),oc2(nlon,nlat,nsig),&
       &dust1(nlon,nlat,nsig),dust2(nlon,nlat,nsig),&
       &dust3(nlon,nlat,nsig),dust4(nlon,nlat,nsig),dust5(nlon,nlat,nsig),&
       &seas1(nlon,nlat,nsig),seas2(nlon,nlat,nsig),&
       &seas3(nlon,nlat,nsig),seas4(nlon,nlat,nsig),p25(nlon,nlat,nsig))

  ALLOCATE(tmp3d(nlon,nlat,nsig))

  DO ii=1,naero_gocart_wrf
     varname=TRIM(aeronames_gocart_wrf(ii))
     CALL ext_ncd_get_var_info(ncunit,TRIM(varname),ndim,ordering,&
          &staggering,start_index,end_index,wrftype,ierr) 
     IF ( ierr /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', ierr, varname
        STOP(12)
     ENDIF

     CALL ext_ncd_read_field(ncunit,datestr(it),TRIM(varname), &
          tmp3d,wrf_real,0,0,0,ordering,          &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          ierr                                 )
     IF ( ierr /= 0 )THEN
        WRITE(6,*)'WRF input file flnm_in in ext_ncd_get_var_info = ',&
             TRIM(flnm_in),', status = ', ierr, varname
        STOP(13)
     ENDIF
     
     SELECT CASE ( TRIM(aeronames_gocart_wrf(ii)) )
     CASE ('sulf')
        sulf=tmp3d*ppmv_conv 
     CASE ('BC1')
        bc1=tmp3d
     CASE ('BC2')
        bc2=tmp3d
     CASE ('OC1')
        oc1=tmp3d
     CASE ('OC2')
        oc2=tmp3d
     CASE ('DUST_1')
        dust1=tmp3d
     CASE ('DUST_2')
        dust2=tmp3d
     CASE ('DUST_3')
        dust3=tmp3d
     CASE ('DUST_4')
        dust4=tmp3d
     CASE ('DUST_5')
        dust5=tmp3d
     CASE ('SEAS_1')
        seas1=tmp3d
     CASE ('SEAS_2')
        seas2=tmp3d
     CASE ('SEAS_3')
        seas3=tmp3d
     CASE ('SEAS_4')
        seas4=tmp3d
     CASE ('P25')
        p25=tmp3d
     CASE default
        WRITE(6,*)'Unknown aerosol'
        STOP(14)
     END SELECT
  ENDDO

  CALL init_crtm(nchanl,naero_gocart_wrf,isis,obstype)

!$omp parallel default(none) &

!$omp private(timestart,timeend,ctimestart,ctimeend,cdatestart,cdateend,i,j,tid,h,q,qs,presl,presi,aero,error_status,layer_od) shared(it,nchanl,obstype,nlon,nlat,nsig,nthreads,tsen_r,qvapor,qsat_r,ps,eta,etap,ptop,sulf,bc1,bc2,oc1,oc2,dust1,dust2,dust3,dust4,dust5,seas1,seas2,seas3,seas4,p25,total_aod,ii,k) 


  tid = omp_get_thread_num()

  IF (tid == 0) THEN
     nthreads = omp_get_num_threads()
     PRINT *, 'nthreads = ',nthreads
  END IF

  DO i=1,nlon
     PRINT *,nlon-i+1,tid

!$omp do schedule(static)

     DO j=1,nlat

        DO k=1,nsig
           h(k)=tsen_r(i,j,k)
           q(k)=MAX(qvapor(i,j,k),qsmall)
           qs(k)=qsat_r(i,j,k)

           presl(k)=1.e-3_r_kind*(eta(k)*(ps(i,j)-ptop) + ptop)
           presi(k)=1.e-3_r_kind*(etap(k)*(ps(i,j)-ptop) + ptop)

           DO ii=1,naero_gocart_wrf
              SELECT CASE ( TRIM(aeronames_gocart_wrf(ii)) )
              CASE ('sulf')
                 aero(k,ii)=sulf(i,j,k)
              CASE ('BC1')
                 aero(k,ii)=bc1(i,j,k)
              CASE ('BC2')
                 aero(k,ii)=bc2(i,j,k)
              CASE ('OC1')
                 aero(k,ii)=oc1(i,j,k)
              CASE ('OC2')
                 aero(k,ii)=oc2(i,j,k)
              CASE ('DUST_1')
                 aero(k,ii)=dust1(i,j,k)
              CASE ('DUST_2')
                 aero(k,ii)=dust2(i,j,k)
              CASE ('DUST_3')
                 aero(k,ii)=dust3(i,j,k)
              CASE ('DUST_4')
                 aero(k,ii)=dust4(i,j,k)
              CASE ('DUST_5')
                 aero(k,ii)=dust5(i,j,k)
              CASE ('SEAS_1')
                 aero(k,ii)=seas1(i,j,k)
              CASE ('SEAS_2')
                 aero(k,ii)=seas2(i,j,k)
              CASE ('SEAS_3')
                 aero(k,ii)=seas3(i,j,k)
              CASE ('SEAS_4')
                 aero(k,ii)=seas4(i,j,k)
              CASE ('P25')
                 aero(k,ii)=p25(i,j,k)
              END SELECT
           ENDDO
        ENDDO
        presi(nsig+1)=1.e-3_r_kind*(etap(nsig+1)*(ten*ps(i,j)-ptop) + ptop)
        CALL call_crtm(obstype,nchanl, &
             h,q,qs,presl,presi,aero,error_status,layer_od=layer_od)
        DO ii=1,nchanl
           total_aod(i,j,ii,it)=SUM(layer_od(:,ii))
        ENDDO
     ENDDO

!$omp end do nowait

     IF (tid==0) THEN
        CALL DATE_AND_TIME(cdateend,ctimeend)
        READ(ctimeend(3:10),'(f8.3)') timeend
        PRINT *,timeend-timestart, ' tid==0'
     ENDIF

  ENDDO


!$omp end parallel

  DEALLOCATE(sulf,bc1,bc2,oc1,oc2,dust1,dust2,dust3,dust4,dust5,seas1,seas2,seas3,seas4,p25)
  DEALLOCATE(qsat_r,tsen_r,qvapor,ps)
  DEALLOCATE(h,q,presl,presi,layer_od)
  DEALLOCATE(eta,etap,tmp3d)
  
  CALL destroy_crtm

  ntime=1
  OPEN(unit=binunit,file=TRIM(flnm_out),form='unformatted')
  WRITE(binunit)nlon,nlat,ntime
  DO it=1,ntime
     WRITE(binunit)total_aod(:,:,4,it)
  ENDDO
  CLOSE(binunit)

  PRINT *,MINVAL(total_aod(:,:,4,ntime)),MAXVAL(total_aod(:,:,4,ntime))

  DEALLOCATE(total_aod,aero)

END PROGRAM cal_gocart_aod_omp
