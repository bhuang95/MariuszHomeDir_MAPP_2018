MODULE module_viirs2ioda

  USE netcdf
  USE timedelta_mod
  USE datetime_mod
  
CONTAINS

  SUBROUTINE read_viirsaod

    USE module_viirs2aeronet_args, ONLY: infile,&
         &validtimestr,validtime,viirs_errors

    USE module_viirs_vars, ONLY: inst,sat,retrieval_type,&
         &viirstimestr,viirstdiff,viirs_aod_input,&
         &viirs_aod_output,nobs_in,&
         &reg_coeffs_bias,reg_coeffs_uncertainty,qc_retain,viirs_aod_max

    IMPLICIT NONE

!locals

    INTEGER :: ncid 
    INTEGER :: dimid 
    INTEGER :: rowid, colid

    INTEGER :: nrows, ncols, nobs

    INTEGER :: varid
    INTEGER :: qcid

    REAL, ALLOCATABLE, DIMENSION(:,:) :: in_lats, in_lons
    REAL, ALLOCATABLE, DIMENSION(:) :: in_lats1d, in_lons1d, in_aodtmp
    REAL, ALLOCATABLE, DIMENSION(:,:) :: in_AOD550
    REAL, ALLOCATABLE, DIMENSION(:) :: in_AOD5501d
    INTEGER(SELECTED_INT_KIND(2)), ALLOCATABLE, DIMENSION(:,:) ::&
         & in_qcpath,in_qcall
    INTEGER(SELECTED_INT_KIND(2)), ALLOCATABLE, DIMENSION(:) ::&
         & in_qcpath1d,in_qcall1d

    INTEGER :: i,j,qcval
    REAL :: ab,bb,au,bu
    INTEGER :: yyyy,mm,dd,hh,ii

    INTEGER :: status,ipos

    TYPE(datetime_type) :: datatime
    TYPE(timedelta_type) dt

    CALL check_nc(nf90_open(infile, nf90_nowrite, ncid))

    CALL check_nc(nf90_inq_dimid(ncid,"Rows",dimid))
    CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=nrows))
    CALL check_nc(nf90_inq_dimid(ncid,"Columns",dimid))
    CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=ncols))

    nobs = nrows*ncols

    ALLOCATE(in_lats(ncols,nrows),in_lons(ncols,nrows))
    ALLOCATE(in_lats1d(nobs),in_lons1d(nobs))
    ALLOCATE(in_aodtmp(nobs))
    ALLOCATE(in_qcpath(ncols,nrows),in_qcall(ncols,nrows))
    ALLOCATE(in_qcpath1d(nobs),in_qcall1d(nobs))
    ALLOCATE(in_AOD550(ncols,nrows),in_AOD5501d(nobs))

    CALL check_nc(nf90_inq_varid(ncid,"Latitude",varid))
    CALL check_nc(nf90_get_var(ncid,varid,in_lats))
    CALL check_nc(nf90_inq_varid(ncid,"Longitude",varid))
    CALL check_nc(nf90_get_var(ncid,varid,in_lons))
    CALL check_nc(nf90_inq_varid(ncid,"AOD550",varid))
    CALL check_nc(nf90_get_var(ncid,varid,in_AOD550))
    CALL check_nc(nf90_inq_varid(ncid,"QCPath",qcid))
    CALL check_nc(nf90_get_var(ncid,qcid,in_qcpath))
    CALL check_nc(nf90_inq_varid(ncid,"QCAll",qcid))
    CALL check_nc(nf90_get_var(ncid,qcid,in_qcall))
! metadata

    status = nf90_get_att(ncid,NF90_GLOBAL,"satellite_name",sat)
    status = nf90_get_att(ncid,NF90_GLOBAL,"instrument_name",inst)
    IF (status /= 0 .OR. TRIM(inst)=="VIIRS") inst="v.viirs-m_npp"
    status = nf90_get_att(ncid,NF90_GLOBAL,"summary",retrieval_type)
    IF (retrieval_type(1:10)=="Enterprise") THEN
       retrieval_type="NESDIS Enterprise AOD"
    ELSE
       retrieval_type="unknown"
    ENDIF

    status = nf90_get_att(ncid,NF90_GLOBAL,"time_coverage_end" &
         &,viirstimestr)

    CALL check_nc(nf90_close(ncid))

    IF (status == 0) THEN
       READ( viirstimestr(1:4), '(i4)' )  yyyy
       READ( viirstimestr(6:7), '(i2)' )  mm
       READ( viirstimestr(9:10), '(i2)' )  dd
       READ( viirstimestr(12:13), '(i2)' )  hh
       READ( viirstimestr(15:17), '(i2)' )  ii 
    ELSE
       ipos=INDEX(infile,'_e')
       viirstimestr=infile(ipos+2:ipos+13)
       READ( viirstimestr(1:4), '(i4)' )  yyyy
       READ( viirstimestr(5:6), '(i2)' )  mm
       READ( viirstimestr(7:8), '(i2)' )  dd
       READ( viirstimestr(9:10), '(i2)' )  hh
       READ( viirstimestr(11:12), '(i2)' )  ii
    ENDIF

! place into viirs_aod type array

    in_aodtmp = PACK(in_qcall,in_qcall <= qc_retain)
    in_lats1d = RESHAPE(in_lats,SHAPE(in_lats1d))
    in_lons1d = RESHAPE(in_lons,SHAPE(in_lons1d))
    in_AOD5501d = RESHAPE(in_AOD550,SHAPE(in_AOD5501d))
    in_qcall1d = RESHAPE(in_qcall,SHAPE(in_qcall1d))
    in_qcpath1d = RESHAPE(in_qcpath,SHAPE(in_qcpath1d))
    nobs = SIZE(in_aodtmp)
    WHERE(in_AOD5501d < 0.) in_qcall1d=3
    WHERE(in_AOD5501d > viirs_aod_max) in_qcall1d=3

    IF (nobs  <= 0) THEN 
       PRINT *,TRIM(infile)
       PRINT *,'No valid obs at '//TRIM(viirstimestr)
       STOP
    ENDIF

    PRINT *,TRIM(infile)
    PRINT *,'There are valid viirs obs at '//TRIM(viirstimestr)

    ALLOCATE(viirs_aod_input(nobs))
    i=1
    DO j=1,nrows*ncols
       IF (in_qcall1d(j) <= qc_retain) THEN 
          viirs_aod_input(i)%value550=in_AOD5501d(j)
          viirs_aod_input(i)%lat=in_lats1d(j)
          viirs_aod_input(i)%lon=in_lons1d(j)
          viirs_aod_input(i)%qcall=in_qcall1d(j)
          IF (BTEST(in_qcpath1d(j),0)) THEN ! water
             qcval = 0 + in_qcall1d(j) 
             viirs_aod_input(i)%stype = 0
          ELSE
             IF (BTEST(in_qcpath1d(j),1)) THEN ! bright land
                qcval = 10 + in_qcall1d(j)
                viirs_aod_input(i)%stype = 2
             ELSE ! dark land
                qcval = 20 + in_qcall1d(j)
                viirs_aod_input(i)%stype = 1
             END IF
          END IF


          IF (viirs_errors == 0) THEN

             ab = 0; bb = 0.; au = 0; bu = 0.

             viirs_aod_input(i)%bias = 0.
             viirs_aod_input(i)%uncertainty = 0.
             
          ELSE IF (viirs_errors == 1) THEN
             
!             PRINT *,'Using bias/uncertaintly 2019'

             SELECT CASE(qcval)
                
! case all land high quality
!ab = -0.0137694 ; bb = 0.153738 ; au = 0.111351 ; bu =
!0.148685
! case all land medium quality
!ab = 0.0177766 ; bb = 0.383993; au = 0.0468670 ; bu =
!0.259278 
             CASE (10) ! case dark land high quality
                ab = -0.0138969; bb = 0.157877; au = 0.111431; bu = 0.128699
             CASE (11) ! case dark land medium quality
                ab = 0.0193166; bb = 0.376421; au = 0.0374849; bu = 0.266073
             CASE (20) ! case bright land high quality
                ab = -0.0107621; bb = 0.150480; au = 0.0550472; bu =&
                     & 0.299558
             CASE (21) ! case bright land medium quality
                ab = 0.0124126; bb = 0.261174; au = 0.0693246; bu = 0.270070
             CASE (0) ! case water high quality
                ab = 0.0151799; bb = 0.0767385; au = 0.00784394; bu =&
                     & 0.219923
             CASE (1) ! case water medium quality
                ab = 0.0377016; bb = 0.283547; au = 0.0416146; bu =&
                     & 0.0808841
             CASE default
                ab = 0; bb = 100.; au = 0; bu = 100.
             END SELECT

             viirs_aod_input(i)%bias = ab + bb*in_AOD5501d(j)
             viirs_aod_input(i)%uncertainty = au + bu*in_AOD5501d(j)

          ELSE

!from quadratic regression fitting in viirs_stats.R using Shobha's
!2021 estimates 

!             PRINT *,'Using bias/uncertaintly 2021'

!original coeeffs from Shobha for global obs
             IF (in_AOD5501d(j) < 0.1) THEN
                viirs_aod_input(i)%bias=0.4489*in_AOD5501d(j)-0.0446
                viirs_aod_input(i)%uncertainty=0.2761*in_AOD5501d(j)+&
                     &0.0235
             ELSEIF (in_AOD5501d(j) < 0.8) THEN
                viirs_aod_input(i)%bias=-0.0865*in_AOD5501d(j)+0.0040
                viirs_aod_input(i)%uncertainty=0.2858*in_AOD5501d(j)+&
                     &0.0306
             ELSE 
                viirs_aod_input(i)%bias=0.1395*in_AOD5501d(j)-0.1195
                viirs_aod_input(i)%uncertainty=0.2262*in_AOD5501d(j)+&
                     &0.0639
             ENDIF


!not very good approximations
!             viirs_aod_input(i)%bias=&
!                  &reg_coeffs_bias(1)+&
!                  &reg_coeffs_bias(2)*in_AOD5501d(j)+&
!                  &reg_coeffs_bias(3)*in_AOD5501d(j)**2
!
!             viirs_aod_input(i)%uncertainty=&
!                  &reg_coeffs_uncertainty(1)+&
!                  &reg_coeffs_uncertainty(2)*in_AOD5501d(j)+&
!                  &reg_coeffs_uncertainty(3)*in_AOD5501d(j)**2
!

          ENDIF

          viirs_aod_input(i)%value550=MAX(viirs_aod_input(i)%value550-&
               &viirs_aod_input(i)%bias,0.)

          i = i+1
       ELSE
          CYCLE
       END IF
    END DO

    IF (viirs_errors /= 0) PRINT *,'!!! VIIRS retrievals are debiased !!!'

    DEALLOCATE(in_lats,in_lats1d,in_lons,in_lons1d,in_AOD550,&
         &in_AOD5501d)

    datatime = create_datetime(year=yyyy,month=mm,day=dd,hour=hh,&
         &minute=ii)
    dt = datatime-validtime
    viirstdiff = dt%total_hours()      

    nobs_in=i-1

    ALLOCATE(viirs_aod_output(nobs_in))
    viirs_aod_output=viirs_aod_input(1:nobs_in)
    
    DEALLOCATE(viirs_aod_input)

  END SUBROUTINE read_viirsaod

  SUBROUTINE read_viirsaod_lunar

    USE module_viirs2aeronet_args, ONLY: infile,&
         &validtimestr,validtime

    USE module_viirs_vars, ONLY: inst,sat,retrieval_type,&
         &viirstimestr,viirstdiff,viirs_aod_input,nobs_in,&
         &qc_retain_lunar

    IMPLICIT NONE

    INTEGER :: ncid 
    INTEGER :: dimid 
    INTEGER :: rowid, colid
    INTEGER :: nrows, ncols
    INTEGER :: varid
    INTEGER :: qcid
    
    REAL, ALLOCATABLE, DIMENSION(:,:) :: in_lats, in_lons
    REAL, ALLOCATABLE, DIMENSION(:) :: in_lats1d, in_lons1d, in_aodtmp
    REAL, ALLOCATABLE, DIMENSION(:,:) :: in_AOD550
    REAL, ALLOCATABLE, DIMENSION(:) :: in_AOD5501d
    REAL, ALLOCATABLE, DIMENSION(:,:) :: in_qc
    REAL, ALLOCATABLE, DIMENSION(:) :: in_qc1d
    
    INTEGER :: i,j,qcval,nobsvalid,nobs
    REAL,PARAMETER :: ab=0.,bb=0.,au=0.085,bu=0.1
    INTEGER :: yyyy,mm,dd,hh,ii
    
    INTEGER :: status,ipos
    
    TYPE(datetime_type) :: datatime
    TYPE(timedelta_type) dt
    
    CALL check_nc(nf90_open(infile, nf90_nowrite, ncid))
    CALL check_nc(nf90_inq_dimid(ncid,"lat",dimid))
    CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=nrows))
    CALL check_nc(nf90_inq_dimid(ncid,"lon",dimid))
    CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=ncols))
    
    nobs = nrows*ncols
    
    ALLOCATE(in_lats(ncols,nrows),in_lons(ncols,nrows))
    ALLOCATE(in_lats1d(nobs),in_lons1d(nobs))
    ALLOCATE(in_aodtmp(nobs))
    ALLOCATE(in_qc(ncols,nrows))
    ALLOCATE(in_qc1d(nobs))
    ALLOCATE(in_AOD550(ncols,nrows),in_AOD5501d(nobs))
    
    CALL check_nc(nf90_inq_varid(ncid,"latitude",varid))
    CALL check_nc(nf90_get_var(ncid,varid,in_lats))
    CALL check_nc(nf90_inq_varid(ncid,"longitude",varid))
    CALL check_nc(nf90_get_var(ncid,varid,in_lons))
    CALL check_nc(nf90_inq_varid(ncid,"AOD_550nm",varid))
    CALL check_nc(nf90_get_var(ncid,varid,in_AOD550))
    CALL check_nc(nf90_inq_varid(ncid,"AOD_QA",qcid))
    CALL check_nc(nf90_get_var(ncid,qcid,in_qc))

    inst="v.viirs-m_npp"
    retrieval_type="UofIowa lunar"
 
    ipos=INDEX(infile,'.nc')
    viirstimestr=infile(ipos-15:ipos-4)

    READ( viirstimestr(1:4), '(i4)' )  yyyy
    READ( viirstimestr(5:6), '(i2)' )  mm
    READ( viirstimestr(7:8), '(i2)' )  dd
    READ( viirstimestr(9:10), '(i2)' )  hh
    READ( viirstimestr(11:12), '(i2)' )  ii
  
    in_lats1d = RESHAPE(in_lats,SHAPE(in_lats1d))
    in_lons1d = RESHAPE(in_lons,SHAPE(in_lons1d))
    in_AOD5501d = RESHAPE(in_AOD550,SHAPE(in_AOD5501d))
    in_qc1d = RESHAPE(in_qc,SHAPE(in_qc1d))

    nobsvalid=COUNT(in_qc1d(:) >= qc_retain_lunar &
         & .AND. in_AOD5501d(:) >= 0. .AND. in_AOD5501d(:) <= 5.)
    
    PRINT *,TRIM(infile)
    PRINT *,'There are ',nobsvalid,' valid obs at '//TRIM(viirstimestr)
    
    ALLOCATE(viirs_aod_input(nobsvalid))
    i=1
    DO j=1,nobs
       
       IF (in_qc1d(j) >= qc_retain_lunar &
            & .AND. in_AOD5501d(j) >= 0. .AND. in_AOD5501d(j) <= 5.) THEN 
          
          in_qc1d(j)=0
          
          viirs_aod_input(i)%value550=in_AOD5501d(j)
          viirs_aod_input(i)%lat=in_lats1d(j)
          viirs_aod_input(i)%lon=in_lons1d(j)
          viirs_aod_input(i)%qcall=in_qc1d(j)
          viirs_aod_input(i)%bias = ab + bb*in_AOD5501d(j)
          viirs_aod_input(i)%uncertainty = au + bu*in_AOD5501d(j)
          viirs_aod_input(i)%stype = 1
          
          i = i+1
          
       ELSE
          CYCLE
          
       END IF
    END DO
    
    nobs_in=nobsvalid
    
    DEALLOCATE(in_lats,in_lats1d,in_lons,in_lons1d,&
         &in_AOD550,in_AOD5501d,in_qc,in_qc1d)
    
    datatime = create_datetime(year=yyyy,month=mm,day=dd,hour=hh,&
         &minute=ii)
    dt = datatime-validtime
    viirstdiff = dt%total_hours()      
    
    CALL check_nc(nf90_close(ncid))
    
  END SUBROUTINE read_viirsaod_lunar

  SUBROUTINE read_aeronet

    USE, INTRINSIC :: iso_fortran_env

    USE m_unirnk
    USE m_mrgrnk
    USE m_mulcnt

    USE module_constants, ONLY: date_string_length, max_string_length, &
         &small, rmissing

    USE module_viirs2aeronet_args, ONLY: infile_aeronet,&
         &validtimestr,validtime,tdiff_aeronet_max,tdiff_v2a_max

    USE module_aeronet_vars

    USE module_viirs_vars, ONLY: viirs_wavelength,viirstdiff

    IMPLICIT NONE

    INTEGER :: ncid,grpid,varid,dimid
    INTEGER :: nchans,nlocs
    INTEGER(int64), ALLOCATABLE :: aerotime(:)

    REAL, ALLOCATABLE :: aer_wavelengths(:),obs(:,:)
    INTEGER, ALLOCATABLE :: qc(:,:), irnk(:),jrnk(:),krnk(:)

    INTEGER :: i,nunique,j
    INTEGER :: status

    CHARACTER(len=max_string_length) :: units

    LOGICAL :: all_obs_valid

    TYPE(datetime_type) :: datatime_ref,datatime
    TYPE(timedelta_type) dt

    CONTINUE

    CALL check_nc(nf90_open(infile_aeronet, nf90_nowrite, ncid))

    CALL check_nc(nf90_inq_dimid(ncid,"Channel",dimid))
    CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=nchans))
    CALL check_nc(nf90_inq_dimid(ncid,"Location",dimid))
    CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=nlocs))

    ALLOCATE(aeronet_aod_input(nlocs),aerotime(nlocs),&
         &aer_wavelengths(nchans),obs(nchans,nlocs),qc(nchans,nlocs))

    CALL check_nc(nf90_inq_grp_ncid(ncid,"MetaData",grpid))
    CALL check_nc(nf90_inq_varid(grpid,"dateTime",varid))
    CALL check_nc(nf90_get_var(grpid,varid,aerotime))

!assume that date is always 1970-01-01T00:00:00Z since f90 cannot
!read strings
    datatime_ref = create_datetime(year=1970,month=1,day=1,hour=0,&
         &minute=0)

    DO i=1,nlocs
       dt=timedelta(minutes=aerotime(i)/60.)
       datatime=datatime_ref+dt
       dt=datatime-validtime
       aeronet_aod_input(i)%tdiff=dt%total_hours()
    ENDDO

!    CALL check_nc(nf90_inq_varid(grpid,"stationIdentification",varid))
!    CALL check_nc(nf90_get_var(grpid,varid,aeronet_aod_input(:)%station_id))
!temp fixes - all needs to be sorted using latitude and no time info
    aeronet_aod_input(:)%station_id='XXXXX'

    CALL check_nc(nf90_inq_varid(grpid,"latitude",varid))
    CALL check_nc(nf90_get_var(grpid,varid,aeronet_aod_input(:)%lat))
    CALL check_nc(nf90_inq_varid(grpid,"longitude",varid))
    CALL check_nc(nf90_get_var(grpid,varid,aeronet_aod_input(:)%lon))
    CALL check_nc(nf90_inq_varid(grpid,"sensorCentralWavelength",varid))
    CALL check_nc(nf90_get_var(grpid,varid,aer_wavelengths))

    aeronet_aod_input(:)%stype=1 ! dark land
    aer_wavelengths=aer_wavelengths*1.e-6 ! convert to [m] as viirs

    DO i=1,nchans
       IF (ABS(aer_wavelengths(i)-viirs_wavelength) < small) EXIT
    ENDDO

    IF (ABS(aer_wavelengths(i)-viirs_wavelength) > small) THEN
       PRINT *,"No matching AERONET-VIIRS wavelengths - Stopping"
       PRINT *,"AERONET: ",aer_wavelengths(i)
       PRINT *,"VIIRS: ",viirs_wavelength
       STOP 1
    ENDIF

    CALL check_nc(nf90_inq_grp_ncid(ncid,"ObsValue",grpid))
    CALL check_nc(nf90_inq_varid(grpid,"aerosolOpticalDepth",varid))
    CALL check_nc(nf90_get_var(grpid,varid,obs))

    aeronet_aod_input(:)%value550=obs(i,:)

    CALL check_nc(nf90_inq_grp_ncid(ncid,"PreQC",grpid))
    CALL check_nc(nf90_inq_varid(grpid,"aerosolOpticalDepth",varid))
    CALL check_nc(nf90_get_var(grpid,varid,qc))

    aeronet_aod_input(:)%qc=qc(i,:)

    CALL check_nc(nf90_close(ncid))

    WHERE(aeronet_aod_input(:)%qc > qc_aeronet_retain .OR. &
         &ABS(aeronet_aod_input(:)%tdiff) > tdiff_aeronet_max .OR. &
         &aeronet_aod_input(:)%value550 < 0. .OR. &
         &ABS(aeronet_aod_input(:)%tdiff - viirstdiff) > tdiff_v2a_max)&
         &aeronet_aod_input(:)%lat=-rmissing

    ALLOCATE(irnk(nlocs),jrnk(nlocs),krnk(nlocs))

    CALL mrgrnk(aeronet_aod_input(:)%lat,irnk)

    CALL unirnk(aeronet_aod_input(irnk)%lat,krnk,nunique)

    IF (ANY(aeronet_aod_input(1:nunique)%lat == -rmissing) ) &
         &nunique=nunique-1

    PRINT *,"There are ",nunique," AERONET observations at ",&
         &validtime%isoformat()

    ALLOCATE(aeronet_aod_output(nunique))

    aeronet_aod_output(:)=aeronet_aod_input(irnk(krnk(1:nunique)))

    DEALLOCATE(obs,qc,aerotime,aeronet_aod_input,irnk,jrnk,krnk)

  END SUBROUTINE read_aeronet

  SUBROUTINE write_iodav3_viirsaod_clim(outfile)

    USE, INTRINSIC :: iso_fortran_env

    USE module_viirs_vars, ONLY: inst,sat,retrieval_type,&
         &nchans,channels,viirs_wavelength,viirstimestr,&
         &viirs_aod_output,nobs_in,nobs_out

    USE module_constants

    IMPLICIT NONE

    CHARACTER(len=*), INTENT(in) :: outfile

    CHARACTER(len=10) :: validtimestr
    INTEGER :: ncid ! netCDF file ID
    INTEGER :: nlocsid, nchansid, ndtimeid ! dimension IDs
    CHARACTER(len=max_varname_length) :: attname,attvalue
    
    INTEGER :: varids(50),grpids(50)

    INTEGER :: i,j,nlocs,ndtime
    INTEGER :: validtimeint,yyyy,mm,dd,hh

    TYPE(datetime_type) :: datatime
    TYPE(timedelta_type) dt
    
    REAL, DIMENSION(nchans) :: freqs

    REAL(real32), ALLOCATABLE :: zeros_r32(:)
    REAL(real64), ALLOCATABLE :: zeros_r64(:)
    INTEGER(int32), ALLOCATABLE :: zeros_i32(:),series(:)
    INTEGER(int64), ALLOCATABLE :: zeros_i64(:)

    REAL(real32)    :: r32_missing
    REAL(real64)    :: r64_missing
    INTEGER(int32)  :: i32_missing
    INTEGER(int64)  :: i64_missing
    
    i32_missing = -9999
    i64_missing = -9999
    r32_missing = -9999.
    r64_missing = -9999.

    CALL check_nc(nf90_create(path=outfile,cmode=nf90_netcdf4,&
         &ncid=ncid))

    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"_ioda_layout","ObsGroup"))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"_ioda_layout_version",0))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"observation_type","Aod"))

    READ( viirstimestr(1:4), '(i4)' )  yyyy
    READ( viirstimestr(5:6), '(i2)' )  mm
    READ( viirstimestr(7:8), '(i4)' )  dd
    READ( viirstimestr(9:10), '(i2)' )  hh

    datatime = create_datetime(year=yyyy,month=mm,day=dd,hour=hh)

    validtimestr=viirstimestr(1:10)

    READ(validtimestr,"(i)") validtimeint
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"date_time",validtimeint))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"satellite",sat))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"sensor",inst))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"retrieval_type",&
         &TRIM(retrieval_type)))

    nlocs=nobs_out

    ALLOCATE(zeros_r32(nlocs),zeros_r64(nlocs),&
         &zeros_i32(nlocs),zeros_i64(nlocs))

    zeros_r32=0.
    zeros_r64=0.
    zeros_i32=0
    zeros_i64=0
    series=(/(i,i=1,nlocs)/)

    CALL check_nc(nf90_def_dim(ncid,'Channel',nchans,nchansid)) 
    CALL check_nc(nf90_def_dim(ncid,'Location',nlocs,nlocsid))

    CALL check_nc(nf90_def_var(ncid,'Channel',nf90_int,nchansid,&
         &varids(1)))
    CALL check_nc(nf90_def_var(ncid,'Location',nf90_int,nlocsid,&
         &varids(2)))

    CALL check_nc(nf90_def_grp(ncid,'MetaData',grpids(1)))

    CALL check_nc(nf90_def_var(grpids(1),'dateTime',nf90_int64,&
         &(/nlocsid/),varids(3)))
    attvalue="seconds since "//datatime%isoformat()
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(3), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(3),0,i64_missing))

    CALL check_nc(nf90_def_var(grpids(1),'latitude',nf90_float,&
         &(/nlocsid/),varids(4)))
    attvalue="degrees_north"
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(4), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(4),0,r32_missing))

    CALL check_nc(nf90_def_var(grpids(1),'longitude',nf90_float,&
         &(/nlocsid/),varids(5)))
    attvalue="degrees_east"
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(5), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(5),0,r32_missing))

    CALL check_nc(nf90_def_var(grpids(1),'sensorCentralFrequency',&
         &nf90_float, (/nchansid/),varids(6)))
    attvalue="Hz"
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(6), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(6),0,r32_missing))

    CALL check_nc(nf90_def_var(grpids(1),'sensorCentralWavelength',&
         &nf90_float, (/nchansid/),varids(7)))
    attvalue="microns"
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(7), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(7),0,r32_missing))

    CALL check_nc(nf90_def_var(grpids(1),'sensorChannelNumber',&
         &nf90_int, (/nchansid/),varids(8)))
    attvalue=" "
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(8), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(8),0,i32_missing))

    CALL check_nc(nf90_def_grp(ncid,'ObsError',grpids(2)))

    CALL check_nc(nf90_def_var(grpids(2),'aerosolOpticalDepth',&
         &nf90_float, (/nchansid,nlocsid/),varids(9)))
    attvalue=" "
    attname="units"
    CALL check_nc(nf90_put_att(grpids(2), varids(9), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(2),varids(9),0,r32_missing))

    CALL check_nc(nf90_def_grp(ncid,'ObsValue',grpids(3)))

    CALL check_nc(nf90_def_var(grpids(3),'aerosolOpticalDepth',&
         &nf90_float, (/nchansid,nlocsid/),varids(10)))
    attvalue=" "
    attname="units"
    CALL check_nc(nf90_put_att(grpids(3), varids(10), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(3),varids(10),0,r32_missing))

    CALL check_nc(nf90_def_grp(ncid,'PreQC',grpids(4)))

    CALL check_nc(nf90_def_var(grpids(4),'aerosolOpticalDepth',&
         &nf90_int, (/nchansid,nlocsid/),varids(11)))
    attvalue=" "
    attname="units"
    CALL check_nc(nf90_put_att(grpids(4), varids(11), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(4),varids(11),0,i32_missing))

    CALL check_nc(nf90_enddef(ncid))

    freqs = speed_of_light/viirs_wavelength

    CALL check_nc(nf90_put_var(ncid,varids(1),channels))
    CALL check_nc(nf90_put_var(ncid,varids(2),zeros_i32))

    CALL check_nc(nf90_put_var(grpids(1),varids(3),zeros_i64))
    CALL check_nc(nf90_put_var(grpids(1),varids(4),&
         &viirs_aod_output(:)%lat))
    CALL check_nc(nf90_put_var(grpids(1),varids(5),&
         &viirs_aod_output(:)%lon))
    CALL check_nc(nf90_put_var(grpids(1),varids(6),&
         &freqs))
    CALL check_nc(nf90_put_var(grpids(1),varids(7),&
         &(/viirs_wavelength*1.e3/)))
    CALL check_nc(nf90_put_var(grpids(1),varids(8),&
         &channels))

    CALL check_nc(nf90_put_var(grpids(2),varids(9),&
         &RESHAPE(zeros_r32,(/1,nlocs/))))

    CALL check_nc(nf90_put_var(grpids(3),varids(10),&
         &RESHAPE(viirs_aod_output(:)%value550,(/1,nlocs/))))

    CALL check_nc(nf90_put_var(grpids(4),varids(11),&
         &RESHAPE(zeros_i32,(/1,nlocs/))))

    CALL check_nc(nf90_close(ncid))

    PRINT *, 'Wrote output to ', TRIM(outfile)

  END SUBROUTINE write_iodav3_viirsaod_clim

!@mzp start

  SUBROUTINE write_iodav3_viirsaod(outfile)

    USE, INTRINSIC :: iso_fortran_env

    USE module_viirs_vars, ONLY: inst,sat,retrieval_type,&
         &nchans,channels,viirs_wavelength,viirstimestr,&
         &viirs_aod_output,nobs_in,nobs_out

    USE module_constants

    IMPLICIT NONE

    CHARACTER(len=*), INTENT(in) :: outfile

    CHARACTER(len=10) :: validtimestr
    INTEGER :: ncid ! netCDF file ID
    INTEGER :: nlocsid, nchansid, ndtimeid ! dimension IDs
    CHARACTER(len=max_varname_length) :: attname,attvalue
    
    INTEGER :: varids(50),grpids(50)

    INTEGER :: i,j,nlocs,ndtime
    INTEGER :: validtimeint,yyyy,mm,dd,hh,min,sec

    TYPE(datetime_type) :: datatime
    TYPE(timedelta_type) dt
    
    REAL, DIMENSION(nchans) :: freqs

    REAL(real32), ALLOCATABLE :: zeros_r32(:)
    REAL(real64), ALLOCATABLE :: zeros_r64(:)
    INTEGER(int32), ALLOCATABLE :: zeros_i32(:),series(:)
    INTEGER(int64), ALLOCATABLE :: zeros_i64(:),secs(:)

    REAL(real32)    :: r32_missing
    REAL(real64)    :: r64_missing
    INTEGER(int32)  :: i32_missing
    INTEGER(int64)  :: i64_missing
    
    i32_missing = -9999
    i64_missing = -9999
    r32_missing = -9999.
    r64_missing = -9999.

    CALL check_nc(nf90_create(path=outfile,cmode=nf90_netcdf4,&
         &ncid=ncid))

    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"_ioda_layout","ObsGroup"))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"_ioda_layout_version",0))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"observation_type","Aod"))

    READ( viirstimestr(1:4), '(i4)' )  yyyy
    READ( viirstimestr(6:7), '(i2)' )  mm
    READ( viirstimestr(9:10), '(i4)' )  dd
    READ( viirstimestr(12:13), '(i2)' )  hh
    READ( viirstimestr(15:16), '(i2)' )  min
    READ( viirstimestr(18:19), '(i2)' )  sec

    datatime = create_datetime(year=yyyy,month=mm,day=dd,hour=hh)

    validtimestr=viirstimestr(1:4)//viirstimestr(6:7)//&
         &viirstimestr(9:10)//viirstimestr(12:13)

    READ(validtimestr,"(i)") validtimeint
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"date_time",validtimeint))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"satellite",sat))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"sensor",inst))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"retrieval_type",&
         &TRIM(retrieval_type)))

    nlocs=nobs_out

    ALLOCATE(zeros_r32(nlocs),zeros_r64(nlocs),&
         &zeros_i32(nlocs),zeros_i64(nlocs),secs(nlocs))

    zeros_r32=0.
    zeros_r64=0.
    zeros_i32=0
    zeros_i64=0
    series=(/(i,i=1,nlocs)/)
    secs=sec+min*60

    CALL check_nc(nf90_def_dim(ncid,'Channel',nchans,nchansid)) 
    CALL check_nc(nf90_def_dim(ncid,'Location',nlocs,nlocsid))

    CALL check_nc(nf90_def_var(ncid,'Channel',nf90_int,nchansid,&
         &varids(1)))
    CALL check_nc(nf90_def_var(ncid,'Location',nf90_int,nlocsid,&
         &varids(2)))

    CALL check_nc(nf90_def_grp(ncid,'MetaData',grpids(1)))

    CALL check_nc(nf90_def_var(grpids(1),'dateTime',nf90_int64,&
         &(/nlocsid/),varids(3)))
    attvalue="seconds since "//datatime%isoformat()
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(3), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(3),0,i64_missing))

    CALL check_nc(nf90_def_var(grpids(1),'latitude',nf90_float,&
         &(/nlocsid/),varids(4)))
    attvalue="degrees_north"
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(4), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(4),0,r32_missing))

    CALL check_nc(nf90_def_var(grpids(1),'longitude',nf90_float,&
         &(/nlocsid/),varids(5)))
    attvalue="degrees_east"
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(5), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(5),0,r32_missing))

    CALL check_nc(nf90_def_var(grpids(1),'sensorCentralFrequency',&
         &nf90_float, (/nchansid/),varids(6)))
    attvalue="Hz"
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(6), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(6),0,r32_missing))

    CALL check_nc(nf90_def_var(grpids(1),'sensorCentralWavelength',&
         &nf90_float, (/nchansid/),varids(7)))
    attvalue="microns"
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(7), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(7),0,r32_missing))

    CALL check_nc(nf90_def_var(grpids(1),'sensorChannelNumber',&
         &nf90_int, (/nchansid/),varids(8)))
    attvalue=" "
    attname="units"
    CALL check_nc(nf90_put_att(grpids(1), varids(8), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(1),varids(8),0,i32_missing))

    CALL check_nc(nf90_def_grp(ncid,'ObsError',grpids(2)))

    CALL check_nc(nf90_def_var(grpids(2),'aerosolOpticalDepth',&
         &nf90_float, (/nchansid,nlocsid/),varids(9)))
    attvalue=" "
    attname="units"
    CALL check_nc(nf90_put_att(grpids(2), varids(9), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(2),varids(9),0,r32_missing))

    CALL check_nc(nf90_def_grp(ncid,'ObsValue',grpids(3)))

    CALL check_nc(nf90_def_var(grpids(3),'aerosolOpticalDepth',&
         &nf90_float, (/nchansid,nlocsid/),varids(10)))
    attvalue=" "
    attname="units"
    CALL check_nc(nf90_put_att(grpids(3), varids(10), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(3),varids(10),0,r32_missing))

    CALL check_nc(nf90_def_grp(ncid,'PreQC',grpids(4)))

    CALL check_nc(nf90_def_var(grpids(4),'aerosolOpticalDepth',&
         &nf90_int, (/nchansid,nlocsid/),varids(11)))
    attvalue=" "
    attname="units"
    CALL check_nc(nf90_put_att(grpids(4), varids(11), &
         &TRIM(attname),attvalue))
    CALL check_nc(nf90_def_var_fill(grpids(4),varids(11),0,i32_missing))

    CALL check_nc(nf90_enddef(ncid))

    freqs = speed_of_light/viirs_wavelength

    CALL check_nc(nf90_put_var(ncid,varids(1),channels))
    CALL check_nc(nf90_put_var(ncid,varids(2),zeros_i32))

    CALL check_nc(nf90_put_var(grpids(1),varids(3),secs))
    CALL check_nc(nf90_put_var(grpids(1),varids(4),&
         &viirs_aod_output(:)%lat))
    CALL check_nc(nf90_put_var(grpids(1),varids(5),&
         &viirs_aod_output(:)%lon))
    CALL check_nc(nf90_put_var(grpids(1),varids(6),&
         &freqs))
    CALL check_nc(nf90_put_var(grpids(1),varids(7),&
         &(/viirs_wavelength*1.e6/)))
    CALL check_nc(nf90_put_var(grpids(1),varids(8),&
         &channels))

    CALL check_nc(nf90_put_var(grpids(2),varids(9),&
         &RESHAPE(viirs_aod_output(:)%uncertainty,(/1,nlocs/))))

    CALL check_nc(nf90_put_var(grpids(3),varids(10),&
         &RESHAPE(viirs_aod_output(:)%value550,(/1,nlocs/))))

    CALL check_nc(nf90_put_var(grpids(4),varids(11),&
         &RESHAPE(viirs_aod_output(:)%qcall,(/1,nlocs/))))

    CALL check_nc(nf90_close(ncid))

    PRINT *, 'Wrote output to ', TRIM(outfile)

  END SUBROUTINE write_iodav3_viirsaod

!@mzp end


  SUBROUTINE write_viirs2aeronet

    USE module_aeronet_vars, ONLY: viirs2aeronet_out
    USE module_constants, ONLY: date_string_length
    USE module_viirs2aeronet_args, ONLY: validtimestr,outfile,&
         &radius_pixel_output,satellite

    IMPLICIT NONE

    INTEGER :: ncid ! netCDF file ID
    INTEGER :: nlocsid, ndtimeid ! dimension IDs
    INTEGER :: varids(50)

    INTEGER :: i,j,nlocs,ndtime
    INTEGER :: validtimeint
    INTEGER :: yyyy,mm,dd,hh,ii


    CHARACTER(len=date_string_length), ALLOCATABLE :: datetimestr(:)
    
    CALL check_nc(nf90_create(path=outfile,cmode=nf90_netcdf4,&
         &ncid=ncid))

    READ(validtimestr,"(i)") validtimeint
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"date_time",&
         &validtimeint))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"radius",&
         &radius_pixel_output*1.e3))
    CALL check_nc(nf90_put_att(ncid,NF90_GLOBAL,"satellite",&
         &satellite))

    nlocs=SIZE(viirs2aeronet_out(:)%lat)

    CALL check_nc(nf90_def_dim(ncid,'nlocs',NF90_UNLIMITED,nlocsid))

    CALL check_nc(nf90_def_var(ncid,'time',nf90_real,nlocsid,&
         &varids(3)))
    CALL check_nc(nf90_def_var(ncid,'latitude',nf90_real,nlocsid,&
         &varids(4)))
    CALL check_nc(nf90_def_var(ncid,'longitude',nf90_real,nlocsid,&
         &varids(5)))
    CALL check_nc(nf90_def_var(ncid,'surface_type',nf90_int,nlocsid,&
         &varids(6)))
    CALL check_nc(nf90_def_var(ncid,'aod_aeronet',nf90_real,nlocsid,&
         &varids(7)))
    CALL check_nc(nf90_def_var(ncid,'time_v2a',nf90_real,nlocsid,&
         &varids(8)))
    CALL check_nc(nf90_def_var(ncid,'coverage_ratio',nf90_real,nlocsid,&
         &varids(9)))
    CALL check_nc(nf90_def_var(ncid,'aod_viirs_nn',nf90_real,&
         &nlocsid,varids(10)))
    CALL check_nc(nf90_def_var(ncid,'aod_viirs_arithmetic',nf90_real,&
         &nlocsid,varids(11)))
    CALL check_nc(nf90_def_var(ncid,'aod_viirs_geometric',nf90_real,&
         &nlocsid,varids(12)))
    CALL check_nc(nf90_def_var(ncid,'aod_viirs_random',nf90_real,&
         &nlocsid,varids(13)))
    CALL check_nc(nf90_enddef(ncid))

!    CALL check_nc(nf90_put_var(ncid,varids(1),viirs2aeronet_out(:)%station_id))
!    CALL check_nc(nf90_put_var(ncid,varids(2),datetimestr))

    CALL check_nc(nf90_put_var(ncid,varids(3),viirs2aeronet_out(:)%tdiff))
    CALL check_nc(nf90_put_var(ncid,varids(4),viirs2aeronet_out(:)%lat))
    CALL check_nc(nf90_put_var(ncid,varids(5),viirs2aeronet_out(:)%lon))
    CALL check_nc(nf90_put_var(ncid,varids(6),viirs2aeronet_out(:)%stype))
    CALL check_nc(nf90_put_var(ncid,varids(7),viirs2aeronet_out(:)%value550_aero))
    CALL check_nc(nf90_put_var(ncid,varids(8),viirs2aeronet_out(:)%tdiff_v2a))
    CALL check_nc(nf90_put_var(ncid,varids(9),viirs2aeronet_out(:)%coverage_ratio))
    CALL check_nc(nf90_put_var(ncid,varids(10),viirs2aeronet_out(:)%value550_viirs(1)))
    CALL check_nc(nf90_put_var(ncid,varids(11),viirs2aeronet_out(:)%value550_viirs(2)))
    CALL check_nc(nf90_put_var(ncid,varids(12),viirs2aeronet_out(:)%value550_viirs(3)))
    CALL check_nc(nf90_put_var(ncid,varids(13),viirs2aeronet_out(:)%value550_viirs(4)))
    
    CALL check_nc(nf90_close(ncid)) ! close and finish writing out

    PRINT *, 'Wrote ',nlocs,' viirs2aero obs to outfile: ', TRIM(outfile)

  END SUBROUTINE write_viirs2aeronet
 
 SUBROUTINE read_viirsaod_clim(infile)

    USE module_viirs_vars, ONLY: inst,sat,retrieval_type,&
         &nchans,channels,viirs_wavelength,viirstimestr,&
         &viirs_aod_output,nobs_in,nobs_out
    
    IMPLICIT NONE

    CHARACTER(len=*), INTENT(in) :: infile

    INTEGER :: ncid 
    INTEGER :: dimid 
    INTEGER :: rowid, colid
    INTEGER :: nlats, nlons
    INTEGER :: varid
    
    REAL, ALLOCATABLE, DIMENSION(:,:) :: in_lats, in_lons
    REAL, ALLOCATABLE, DIMENSION(:) :: in_lats1d, in_lons1d, in_aodtmp
    REAL, ALLOCATABLE, DIMENSION(:,:) :: in_AOD550
    REAL, ALLOCATABLE, DIMENSION(:) :: in_AOD5501d
    
    INTEGER :: i,j,k,nobsvalid
    INTEGER :: yyyy,mm,dd,hh,ii
    
    INTEGER :: status,ipos
    
    CALL check_nc(nf90_open(infile, nf90_nowrite, ncid))
    CALL check_nc(nf90_inq_dimid(ncid,"lat",dimid))
    CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=nlats))
    CALL check_nc(nf90_inq_dimid(ncid,"lon",dimid))
    CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=nlons))
    
    nobs_in = nlats*nlons
    
    ALLOCATE(in_lats(nlons,nlats),in_lons(nlons,nlats))
    ALLOCATE(in_lats1d(nobs_in),in_lons1d(nobs_in))
    ALLOCATE(in_AOD550(nlons,nlats),in_AOD5501d(nobs_in))

    CALL check_nc(nf90_inq_varid(ncid,"lat",varid))
    CALL check_nc(nf90_get_var(ncid,varid,in_lats(1,:)))
    CALL check_nc(nf90_inq_varid(ncid,"lon",varid))
    CALL check_nc(nf90_get_var(ncid,varid,in_lons(:,1)))
    CALL check_nc_nostop(nf90_inq_varid(ncid,"aod",varid),status)
    IF (status == 0) THEN
!monthly clim
       CALL check_nc(nf90_inq_varid(ncid,"aod",varid))
    ELSE
!daily clim
       CALL check_nc(nf90_inq_varid(ncid,"AOD550",varid))
    ENDIF

    CALL check_nc(nf90_get_var(ncid,varid,in_AOD550))

    DO i=1,nlats
       in_lats(:,i)=in_lats(1,i)
    ENDDO

    DO i=1,nlons
       in_lons(i,:)=in_lons(i,1)
    ENDDO

    sat="NPP"
    inst="v.viirs-m_npp"
    retrieval_type="NESDIS CLIM"

    ipos=INDEX(infile,'.nc')
 
    IF (INDEX(infile,'monthly') > 0) THEN
       viirstimestr(1:6)=infile(ipos-6:ipos-1)
       viirstimestr(7:8)="01"
       viirstimestr(9:10)="00"
    ELSE
       viirstimestr(1:8)=infile(ipos-8:ipos-1)
       viirstimestr(9:10)="00"
    ENDIF

    in_lats1d = RESHAPE(in_lats,SHAPE(in_lats1d))
    in_lons1d = RESHAPE(in_lons,SHAPE(in_lons1d))
    in_AOD5501d = RESHAPE(in_AOD550,SHAPE(in_AOD5501d))

    nobsvalid=COUNT(in_AOD5501d >= 0.)

    PRINT *,'There are ',nobsvalid,' valid clim obs at '//viirstimestr(:10)
    
    ALLOCATE(viirs_aod_output(nobsvalid))

    i=1
    DO j=1,nobs_in
       IF (in_AOD5501d(j) >= 0.) THEN
          viirs_aod_output(i)%value550=MAX(in_AOD5501d(j),0.)
          viirs_aod_output(i)%lat=in_lats1d(j)
          viirs_aod_output(i)%lon=in_lons1d(j)
          viirs_aod_output(i)%qcall=0
          viirs_aod_output(i)%stype=-9999
          i = i+1
       ENDIF
    END DO
    
    nobs_out=nobsvalid
    
    DEALLOCATE(in_lats,in_lats1d,in_lons,in_lons1d,&
         &in_AOD550,in_AOD5501d)
    
    CALL check_nc(nf90_close(ncid))

  END SUBROUTINE read_viirsaod_clim

  SUBROUTINE read_viirsaod_obs_thinned(infile_obs)

    USE module_constants, ONLY: date_string_length, small, rmissing

    USE module_viirs_vars, ONLY: nchans,channels,viirs_wavelength,&
         &viirstimestr,viirs_aod_output,nobs_out
    
    IMPLICIT NONE

    CHARACTER(len=*), INTENT(in) :: infile_obs

    INTEGER :: ncid,grpid,varid,dimid

    REAL, ALLOCATABLE :: obs(:,:)

    INTEGER :: i,nlocs,nch,idate
    INTEGER :: status

    CONTINUE

    CALL check_nc(nf90_open(infile_obs, nf90_nowrite, ncid))

    CALL check_nc(nf90_inq_dimid(ncid,"Channel",dimid))
    CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=nch))
    IF (nch /= nchans) THEN
       PRINT *,'Only one channel for VIIRS 550nm expected - Stopping'
       STOP 1
    ENDIF
    CALL check_nc(nf90_inq_dimid(ncid,"Location",dimid))
    CALL check_nc(nf90_inquire_dimension(ncid,dimid,len=nobs_out))

    PRINT *,'There are ',nobs_out,' input observations'

    CALL check_nc(nf90_get_att(ncid,NF90_GLOBAL,"date_time",idate))
    WRITE(viirstimestr,'(i10)')idate
!change viirstimestr from clim date to the date of VIIRS observation    

    ALLOCATE(viirs_aod_output(nobs_out),obs(nchans,nobs_out))

    CALL check_nc(nf90_inq_grp_ncid(ncid,"MetaData",grpid))
    CALL check_nc(nf90_inq_varid(grpid,"latitude",varid))
    CALL check_nc(nf90_get_var(grpid,varid,viirs_aod_output(:)%lat))
    CALL check_nc(nf90_inq_varid(grpid,"longitude",varid))
    CALL check_nc(nf90_get_var(grpid,varid,viirs_aod_output(:)%lon))
    CALL check_nc(nf90_inq_varid(grpid,"surfaceQualifier",varid))
    CALL check_nc(nf90_get_var(grpid,varid,viirs_aod_output(:)%stype))

    CALL check_nc(nf90_inq_grp_ncid(ncid,"ObsValue",grpid))
    CALL check_nc(nf90_inq_varid(grpid,"aerosolOpticalDepth",varid))
    CALL check_nc(nf90_get_var(grpid,varid,obs))

    viirs_aod_output(:)%value550=obs(nchans,:)

    CALL check_nc(nf90_close(ncid))

  END SUBROUTINE read_viirsaod_obs_thinned

  SUBROUTINE check_nc(status)

    IMPLICIT NONE

    INTEGER, INTENT(in) :: status

    IF(status /= nf90_noerr) THEN
       PRINT *, TRIM(nf90_strerror(status))
       STOP "netCDF error...Stopped."
    END IF

  END SUBROUTINE check_nc

  SUBROUTINE check_nc_nostop(status,error)

    IMPLICIT NONE

    INTEGER, INTENT(in) :: status
    INTEGER, INTENT(out) :: error

    IF(status /= nf90_noerr) THEN
       PRINT *, TRIM(nf90_strerror(status))
       PRINT *, "netCDF data missing but continues using alternate data."
    END IF

    error=status

  END SUBROUTINE check_nc_nostop
  
END MODULE module_viirs2ioda
