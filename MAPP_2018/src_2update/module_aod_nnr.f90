MODULE module_aod_nnr

!  reads nasa's nnr multichannel aods and puts in a structure
!  MP, Sept 2018

  USE netcdf
  USE datetime_mod  
  USE timedelta_mod

  USE module_netcdf_handles
  USE module_misc, ONLY: indexx, unit_namelist, max_name_length

  IMPLICIT NONE

  PRIVATE 
  PUBLIC :: get_aod_nnr,aod_nnr

  TYPE aod_nnr
     CHARACTER(len=max_name_length) :: satellite
     CHARACTER(len=max_name_length) :: obstype !ocean, land, or deep
     TYPE(datetime_type) :: obsdate
     REAL :: lat
     REAL :: lon
     REAL, ALLOCATABLE :: channels(:)
     REAL, ALLOCATABLE :: values(:) ! obs for all channels
  END TYPE aod_nnr

CONTAINS

  SUBROUTINE get_aod_nnr(aod_nnr_record)

    TYPE(aod_nnr), ALLOCATABLE, DIMENSION(:) :: aod_nnr_record

    REAL, PARAMETER :: aod_missing=100.

    REAL     :: lat_ll, lat_ur, lon_ll, lon_ur, rlat, rlon
    INTEGER :: stderr
    INTEGER :: Open_Status,Read_Status

    LOGICAL :: select_domain

    INTEGER :: ncid, status, RecordDimID
    CHARACTER(len = max_name_length) :: RecordDimName
    CHARACTER(len = max_name_length), PARAMETER :: &
         &latname='lat',lonname='lon',timename='time',obsname='obs',&
         &qcname='qcexcl',dayname='syn_beg',levname='lev'

    CHARACTER(len=max_name_length) :: aname
    CHARACTER(len=max_name_length)  :: input_dir_obs,input_file_obs
    CHARACTER(len=10) :: cdate_ref

    INTEGER, DIMENSION(:,:), ALLOCATABLE :: time,qc,lat,lon
    INTEGER, DIMENSION(:), ALLOCATABLE :: indx

    REAL, DIMENSION(:,:), ALLOCATABLE :: obs,lev,aods
    REAL, DIMENSION(:), ALLOCATABLE :: channels,obs_tmp,lats,lons
    INTEGER, DIMENSION(:), ALLOCATABLE :: times

    INTEGER :: latid,lonid,timeid,obsid,qcid,numdims,dayid,levid

    INTEGER, DIMENSION(nf90_max_var_dims) :: dimids

    INTEGER :: nbatches,batchlen,jday_ref,jday_current,jday_current_tmp,hour_current,i,j,k,&
         &nobs_total,nobs_channel,nchannels
    INTEGER :: year,month,day,hour,minute,second,&
         &year_ref,month_ref,day_ref,time_offset

    CHARACTER(len=5), PARAMETER :: caqua='MYD04', cterra='MOD04'
    CHARACTER(len=5), PARAMETER :: cocean='ocean'
    CHARACTER(len=4), PARAMETER :: cland='land'
    CHARACTER(len=4), PARAMETER :: cdeep='deep'

    CHARACTER(max_name_length) :: satellite,obstype

    LOGICAL :: aqua,terra,land,ocean,deep

    LOGICAL :: inside_domain

    TYPE(datetime_type) :: date_start, date_end, obsdate
    TYPE(timedelta_type) :: dt

    NAMELIST /record_obs/ &
         &input_dir_obs, input_file_obs, &
         &select_domain, lat_ll, lat_ur, lon_ll, lon_ur

    CONTINUE

    stderr = 0
    inside_domain = .TRUE.  ! initialize
    select_domain = .TRUE. ! initialize

    lat_ll=-90.
    lat_ur=90.
    lon_ll=-180.
    lon_ur=180.

    aqua=.FALSE.
    terra=.FALSE.
    land=.FALSE.
    ocean=.FALSE.
    deep=.FALSE.

    OPEN (unit=unit_namelist, file = "namelist.modis_nnr_correlation_4hl", &
         &status="old",action = "read",iostat=open_status)

    IF (open_status /= 0) THEN
       WRITE(stderr,*) "error: there is no namelist file (namelist.modis_nnr_correlation)"
       STOP
    END IF

    WRITE(stderr,*) 'Reading namelist.modis_nnr_correlation_4hl record_obs'
    READ (unit_namelist, NML=record_obs, IOSTAT=Read_Status)
    CLOSE(unit_namelist)

    IF (Read_Status /= 0) THEN
       WRITE(stderr,*) 'Error reading record1 of namelist.modis_nnr_correlation'
       STOP
    END IF

!ncdf file names

    satellite=''

    IF (INDEX(TRIM(input_file_obs),caqua) > 0 ) THEN
       aqua=.TRUE.
       satellite='Aqua '//caqua
    ELSE
       aqua=.false.
    ENDIF

    IF (INDEX(TRIM(input_file_obs),cterra) > 0 ) THEN 
       terra=.TRUE.
       satellite='Terra '//cterra
    ELSE
       terra=.FALSE.
    ENDIF

    obstype=''

    IF (INDEX(TRIM(input_file_obs),cland) > 0 ) THEN 
       land=.TRUE.
       obstype=cland
    ELSE
       land=.FALSE.
    ENDIF

    IF (INDEX(TRIM(input_file_obs),cocean) > 0 ) THEN 
       ocean =.TRUE.
       obstype=cocean
    ELSE
       ocean=.FALSE.
    ENDIF

    IF (INDEX(TRIM(input_file_obs),cdeep) > 0 ) THEN 
       deep=.TRUE.
       obstype=cdeep
    ELSE
       deep=.FALSE.
    ENDIF

    IF ( (COUNT([ocean,land,deep]) /= 1) .OR. (COUNT([aqua,terra]) /= 1) ) THEN
       PRINT *,'Wrong names ocean/land/deep ',ocean,land,deep
       PRINT *,'or satellites ',aqua,terra
       STOP
    ENDIF

    input_file_obs=TRIM(input_dir_obs)//'/'//TRIM(input_file_obs)

    status = nf90_open(input_file_obs, nf90_nowrite, ncid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inquire(ncid, unlimitedDimId = RecordDimID)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inquire_dimension(ncid,RecordDimID,RecordDimName,i)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inq_varid(ncid, dayname, dayid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_get_att(ncid, dayid, 'value_at_reference_date',jday_ref)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_get_att(ncid, dayid, 'reference_date',cdate_ref)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

!cdate_ref='1968-05-23'
    READ(cdate_ref,'(i4,2(X,i2))')year_ref,month_ref,day_ref

    status = nf90_get_att(ncid, dayid, 'first_julian_day',jday_current)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_get_att(ncid, dayid, 'latest_synoptic_hour',hour_current)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    date_start=create_datetime(year=year_ref, month=month_ref, day=day_ref)
    dt=timedelta(days=jday_current-jday_ref)
    date_end=date_start+dt

!  PRINT *,date_start%isoformat()
!  PRINT *,date_end%isoformat()

    status = nf90_inq_varid(ncid, latname, latid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inq_varid(ncid, lonname, lonid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inq_varid(ncid, timename, timeid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_get_att(ncid, timeid, 'add_offset',time_offset)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inq_varid(ncid, qcname, qcid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inq_varid(ncid, obsname, obsid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inq_varid(ncid, levname, levid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inquire_variable(ncid, latid, aname, ndims=numDims)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inquire_variable(ncid, latid, dimids = dimids(:numDims))
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    IF (numDims /= 2) THEN
       PRINT *,'Should be 2 variable ',TRIM(latname),' Stopping'
       STOP
    ENDIF

    status = nf90_inquire_dimension(ncid,dimids(1),len=nbatches)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inquire_dimension(ncid,dimids(2),len=batchlen)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    ALLOCATE(lat(nbatches,batchlen),lon(nbatches,batchlen),&
         &time(nbatches,batchlen),qc(nbatches,batchlen),&
         &obs(nbatches,batchlen),lev(nbatches,batchlen))

    status = nf90_get_var(ncid, latid, lat) 
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_get_var(ncid, lonid, lon) 
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_get_var(ncid, timeid, time) 
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_get_var(ncid, qcid, qc) 
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_get_var(ncid, obsid, obs) 
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_get_var(ncid, levid, lev) 
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_close(ncid)

    nobs_channel=COUNT(lev==lev(1,1))
    nobs_total=COUNT(obs < aod_missing)

    IF (MOD(nobs_total,nobs_channel)) THEN
       PRINT *,'Incorrrect number of obs ',nobs_total,nobs_channel
       STOP
    ENDIF

    nchannels=nobs_total/nobs_channel

    ALLOCATE(channels(nchannels),aods(nchannels,nobs_channel),&
         &lats(nobs_channel),lons(nobs_channel),times(nobs_channel),indx(nchannels))

!    aods=RESHAPE(lev,SHAPE(aods),order=[2,1]) ! check if works OK
    aods=RESHAPE(obs,SHAPE(aods),order=[2,1]) !order refers to dimensions of aods
    lats=RESHAPE(lat,SHAPE(lats))*1.e-2
    lons=RESHAPE(lon,SHAPE(lons))*1.e-2
    times=RESHAPE(time,SHAPE(times))

    ALLOCATE(obs_tmp(nobs_total))
    obs_tmp=RESHAPE(lev,SHAPE(obs_tmp))

    channels=[obs_tmp(1::nobs_channel)]

    DEALLOCATE(obs_tmp)

    CALL indexx(nchannels,channels,indx)

!  check 
!  WRITE(6,'(6(f7.0,2x))')[1:nchannels]
!  DO i=1,nchannels
!     WRITE(6,'(6(f7.5,2x))')corr_tmp(i,:)
!  ENDDO
!
!  PRINT *,'***'
!
!  PRINT *,indx

    WRITE(6,'(a,6f7.0,2x)') 'Channels = ',channels(indx)
    WRITE(6,'(a,i10)') 'Number of coincident multichannel observations = ',nobs_channel
    
    ALLOCATE(aod_nnr_record(nobs_channel))

    DO i=1,nobs_channel

       IF (ALLOCATED(aod_nnr_record(i)%channels)) &
            &DEALLOCATE(aod_nnr_record(i)%channels)

       IF (ALLOCATED(aod_nnr_record(i)%values))&
            &DEALLOCATE(aod_nnr_record(i)%values)

       ALLOCATE(aod_nnr_record(i)%channels(nchannels),&
            &aod_nnr_record(i)%values(nchannels))

       aod_nnr_record(i)%satellite=satellite
       aod_nnr_record(i)%obstype=obstype
       aod_nnr_record(i)%channels(:)=channels(indx)
       aod_nnr_record(i)%values(:)=aods(indx,i)
       aod_nnr_record(i)%lat=lats(i)
       aod_nnr_record(i)%lon=lons(i)
       aod_nnr_record(i)%obsdate=date_end+timedelta(minutes=times(i)+time_offset)
    ENDDO

    DEALLOCATE(channels,aods,times,lats,lons,indx)

  CONTAINS

    FUNCTION check_domain(rlat, rlon, lat_ll, lat_ur, lon_ll, lon_ur)
      REAL, INTENT(IN) :: rlat, rlon, lat_ll, lat_ur, lon_ll, lon_ur
      LOGICAL                  :: check_domain
      IF ( (rlon >= lon_ll .AND. rlon <= lon_ur) .AND. &
           (rlat >= lat_ll .AND. rlat <= lat_ur) ) THEN
         check_domain =  .TRUE.
      ELSE
         check_domain =  .FALSE.
      END IF
    END FUNCTION check_domain
    
  END SUBROUTINE get_aod_nnr

END MODULE module_aod_nnr

