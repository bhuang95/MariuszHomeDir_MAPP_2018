MODULE module_interp_output

  USE netcdf
  USE datetime_mod
  USE timedelta_mod


  USE module_misc, ONLY: max_name_length, unit_namelist
  USE module_netcdf_handles
  USE module_aod_nnr, ONLY: aod_nnr
  USE module_fv3_interp, ONLY: aod_m2o

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: aod_output_ncdf

CONTAINS

  SUBROUTINE aod_output_ncdf(analdate,fcstdate,aod_nnr_record,aod_m2o_record,nobs)

    TYPE(datetime_type), INTENT(in) :: analdate,fcstdate
    TYPE(aod_nnr), DIMENSION(nobs), INTENT(in) :: aod_nnr_record
    TYPE(aod_m2o), DIMENSION(nobs), INTENT(in) :: aod_m2o_record
    INTEGER, INTENT(in) :: nobs
    
    CHARACTER(len = max_name_length) :: output_dir,output_file
    INTEGER :: nchan_nnr

    INTEGER :: status,mcid,dim_nobs_id,dim_nchannels_id,channels_id,&
         &aod_obs_id,aod_model_id,lat_id,lon_id,dtime_id
    CHARACTER(len = max_name_length) :: attname,attvalue,obstype
    CHARACTER(len = max_name_length) :: filenamencdf

    REAL, DIMENSION(:,:), ALLOCATABLE :: values
    INTEGER, DIMENSION(:), ALLOCATABLE :: dtime

    TYPE(timedelta_type) :: dt

    INTEGER :: i,j,k

    INTEGER :: Open_Status,Read_Status
    INTEGER :: stderr

    NAMELIST /record_output/ output_dir,output_file

    CONTINUE

    stderr = 0

    OPEN(unit=unit_namelist, file = "namelist.modis_nnr_correlation_4hl", &
         &status="old",action = "read",iostat=open_status)
    IF (open_status /= 0) THEN
       WRITE(stderr,*) "error: there is no namelist file (namelist.modis_nnr_correlation)"
       STOP
    END IF

    WRITE(stderr,*) 'Reading namelist.modis_nnr_correlation_4hl record_output'
    READ (unit_namelist, NML=record_output, IOSTAT=Read_Status)
    CLOSE(unit_namelist)

    nchan_nnr=SIZE(aod_nnr_record(1)%channels)
    obstype=aod_nnr_record(1)%obstype

    filenamencdf=TRIM(output_dir)//'/'//TRIM(output_file)

    status = nf90_create(filenamencdf,nf90_write,mcid)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_def_dim(mcid,'nobs',NF90_UNLIMITED,dim_nobs_id)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_def_dim(mcid,'nchannels_'//TRIM(obstype),&
         &nchan_nnr,dim_nchannels_id)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_def_var(mcid,'channels_'//TRIM(obstype),NF90_REAL,&
         &(/dim_nchannels_id/),channels_id)
    IF (status /= nf90_noerr) CALL handle_err(status)

    attname="units"
    attvalue="nm"
    status = nf90_put_att(mcid, channels_id, TRIM(attname),attvalue)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_def_var(mcid,'AOD_obs',NF90_REAL,&
         &(/dim_nchannels_id,dim_nobs_id/),aod_obs_id)
    IF (status /= nf90_noerr) CALL handle_err(status)

    attname="satellite"
    attvalue=aod_nnr_record(1)%satellite
    status = nf90_put_att(mcid, aod_obs_id, TRIM(attname),attvalue)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_def_var(mcid,'AOD_model',NF90_REAL,&
         &(/dim_nchannels_id,dim_nobs_id/),aod_model_id)
    IF (status /= nf90_noerr) CALL handle_err(status)

    attname="Forecast_hour"
    dt=fcstdate-analdate
    WRITE(attvalue,'(f5.2)')dt%total_hours()

    status = nf90_put_att(mcid, aod_model_id, TRIM(attname), attvalue)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_def_var(mcid,'dtime',NF90_INT,&
         &(/dim_nobs_id/),dtime_id)
    IF (status /= nf90_noerr) CALL handle_err(status)

    attname="Time_difference"
    attvalue="obs-model in minutes"
    status = nf90_put_att(mcid, dtime_id, TRIM(attname),attvalue)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_def_var(mcid,'lat',NF90_REAL,&
         &(/dim_nobs_id/),lat_id)
    IF (status /= nf90_noerr) CALL handle_err(status)

    attname="name"
    attvalue="Latitude"
    status = nf90_put_att(mcid, lat_id, TRIM(attname),attvalue)
    IF (status /= nf90_noerr) CALL handle_err(status)

    attname="unit"
    attvalue="degree"
    status = nf90_put_att(mcid, lat_id, TRIM(attname),attvalue)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_def_var(mcid,'lon',NF90_REAL,&
         &(/dim_nobs_id/),lon_id)
    IF (status /= nf90_noerr) CALL handle_err(status)

    attname="name"
    attvalue="Longitude"
    status = nf90_put_att(mcid, lon_id, TRIM(attname),attvalue)
    IF (status /= nf90_noerr) CALL handle_err(status)

    attname="unit"
    attvalue="degree"
    status = nf90_put_att(mcid, lon_id, TRIM(attname),attvalue)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_enddef(mcid)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_put_var(mcid,channels_id,aod_nnr_record(1)%channels)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_put_var(mcid,lat_id,aod_nnr_record(:)%lat)
    IF (status /= nf90_noerr) CALL handle_err(status)

    status = nf90_put_var(mcid,lon_id,aod_nnr_record(:)%lon)
    IF (status /= nf90_noerr) CALL handle_err(status)

    ALLOCATE(values(nchan_nnr,nobs))

    DO i=1,nobs
       values(:,i)=aod_nnr_record(i)%values
    ENDDO

    status = nf90_put_var(mcid,aod_obs_id,values)
    IF (status /= nf90_noerr) CALL handle_err(status)

    DO i=1,nobs
       values(:,i)=aod_m2o_record(i)%values
    ENDDO

    status = nf90_put_var(mcid,aod_model_id,values)
    IF (status /= nf90_noerr) CALL handle_err(status)

    DEALLOCATE(values)

    ALLOCATE(dtime(nobs))

    DO i=1,nobs
       dtime(i)=NINT(aod_m2o_record(i)%dt%total_minutes())
    ENDDO

    status = nf90_put_var(mcid,dtime_id,dtime)
    IF (status /= nf90_noerr) CALL handle_err(status)

    DEALLOCATE(dtime)

    status = nf90_close(mcid)
    IF (status /= nf90_noerr) CALL handle_err(status)

!    PRINT *,aod_nnr_record(nobs)%channels
!    PRINT *,aod_nnr_record(nobs)%lat,aod_nnr_record(nobs)%lon
!    PRINT *,aod_nnr_record(nobs)%values
!    PRINT *,aod_nnr_record(nobs)%satellite
!    PRINT *,aod_nnr_record(nobs)%obstype
!    PRINT *,aod_nnr_record(nobs)%obsdate%isoformat()


  END SUBROUTINE aod_output_ncdf

END MODULE module_interp_output

