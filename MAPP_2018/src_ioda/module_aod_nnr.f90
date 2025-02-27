MODULE module_aod_nnr

!  reads nasa's nnr multichannel aods and puts in a structure
!  MP, Sept 2018

  USE,INTRINSIC :: iso_fortran_env

  USE netcdf
  USE datetime_mod  
  USE timedelta_mod

  USE module_constants
  USE module_netcdf_io, ONLY: handle_err,ndims_max
  USE module_utils, ONLY: indexx, replace_text

  IMPLICIT NONE


  PRIVATE 
  PUBLIC :: get_aod_nnr,aod_nnr,&
       &modis_params,modis_nnr_errors,diffmax_550

!after Giles et al., 2019, AMT for 1.5 level data
!1.5 level AERONET bias = 0.02
  REAL, PARAMETER :: bias_aeronet=0.00, uncertainty_aeronet=0.02,&
       &aod_550_max=5.
  REAL, PARAMETER :: diffmax_550 = 5.
  

  TYPE aod_nnr
     CHARACTER(len=max_varname_length) :: satellite
     CHARACTER(len=max_varname_length) :: obstype !ocean, land, or deep
     TYPE(datetime_type) :: obsdate
     REAL :: lat
     REAL :: lon
     REAL, ALLOCATABLE :: channels(:)
     REAL, ALLOCATABLE :: values(:) ! obs for all channels
  END TYPE aod_nnr

CONTAINS

  SUBROUTINE get_aod_nnr(aod_nnr_record)

    TYPE(aod_nnr), ALLOCATABLE, DIMENSION(:) :: aod_nnr_record

! not sure what value set for short/long channels
    REAL, PARAMETER :: aod_missing=100. 

    REAL     :: lat_ll, lat_ur, lon_ll, lon_ur, rlat, rlon
    INTEGER(int32) :: stderr
    INTEGER(int32) :: Open_Status,Read_Status

    LOGICAL :: select_domain

    INTEGER(int32) :: ncid, status, RecordDimID
    CHARACTER(len = max_varname_length) :: RecordDimName
    CHARACTER(len = max_varname_length), PARAMETER :: &
         &latname='lat',lonname='lon',timename='time',obsname='obs',&
         &qcname='qcexcl',dayname='syn_beg',levname='lev'

    CHARACTER(len=max_varname_length) :: aname
    CHARACTER(len=max_string_length)  :: input_dir_obs,input_file_obs
    CHARACTER(len=10) :: cdate_ref

    INTEGER(int32), DIMENSION(:,:), ALLOCATABLE :: time,qc,lat,lon
    INTEGER(int32), DIMENSION(:), ALLOCATABLE :: indx

    REAL, DIMENSION(:,:), ALLOCATABLE :: obs,lev,aods
    REAL, DIMENSION(:), ALLOCATABLE :: channels,obs_tmp,lats,lons
    INTEGER(int32), DIMENSION(:), ALLOCATABLE :: times

    INTEGER(int32) :: latid,lonid,timeid,obsid,qcid,numdims,dayid,levid

    INTEGER(int32), DIMENSION(nf90_max_var_dims) :: dimids

    INTEGER(int32) :: nbatches,batchlen,jday_ref,jday_current,jday_current_tmp,&
         &hour_current,i,j,k,i550,&
         &nobs_total,nobs_channel,nobs_channel_screened,nchannels
    INTEGER(int32) :: year,month,day,hour,minute,second,&
         &year_ref,month_ref,day_ref,time_offset

    CHARACTER(len=5), PARAMETER :: caqua='MYD04', cterra='MOD04'
    CHARACTER(len=5), PARAMETER :: cocean='ocean'
    CHARACTER(len=4), PARAMETER :: cland='land'
    CHARACTER(len=4), PARAMETER :: cdeep='deep'

    CHARACTER(max_varname_length) :: satellite,obstype

    LOGICAL :: aqua,terra,land,ocean,deep

    LOGICAL :: inside_domain

    INTEGER(int32) :: unit_namelist=101


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

    OPEN (unit=unit_namelist, file = "nnr2ioda.nl", &
         &status="old",action = "read",iostat=open_status)

    IF (open_status /= 0) THEN
       WRITE(stderr,*) "error: there is no namelist file nnr2ioda.nl)"
       STOP
    END IF

    WRITE(stderr,*) 'Reading nnr2ioda.nl record_obs'
    READ (unit_namelist, NML=record_obs, IOSTAT=Read_Status)
    CLOSE(unit_namelist)

    IF (Read_Status /= 0) THEN
       WRITE(stderr,*) 'Error reading nnr2ioda.nl record_obs'
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

!this in fact should be reversed  since nbatches is unlimited
!but still works fine - don't reverse in the code

    status = nf90_inquire_dimension(ncid,dimids(1),len=nbatches)
    IF (status .NE. nf90_noerr) CALL handle_err(status)


    status = nf90_inquire_dimension(ncid,dimids(2),len=batchlen)
    IF (status .NE. nf90_noerr) CALL handle_err(status)


    IF (nbatches == 0 .or. batchlen == 0) THEN
       status = nf90_close(ncid)
       IF (status .NE. nf90_noerr) CALL handle_err(status)
       PRINT *,'No obs in file ',TRIM(input_file_obs)
       STOP(0)
    ENDIF

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

    IF (MOD(nobs_total,nobs_channel) /= 0) THEN
       PRINT *,'Incorrrect number of obs ',nobs_total,nobs_channel
       STOP(1)
    ENDIF

    nchannels=nobs_total/nobs_channel

    ALLOCATE(channels(nchannels),aods(nchannels,nobs_channel),&
         &lats(nobs_channel),lons(nobs_channel),times(nobs_channel),indx(nchannels))

!order refers to dimensions of aods
    aods=RESHAPE(obs,SHAPE(aods),order=[2,1]) 
    lats=RESHAPE(lat,SHAPE(lats))*1.e-2
    lons=RESHAPE(lon,SHAPE(lons))*1.e-2
    times=RESHAPE(time,SHAPE(times))

    ALLOCATE(obs_tmp(nobs_total))
    obs_tmp=RESHAPE(lev,SHAPE(obs_tmp))

    channels=[obs_tmp(1::nobs_channel)]

    DEALLOCATE(obs_tmp)

    DO i=1,nchannels
       IF (ABS(channels(i) - 550.) <= diffmax_550) THEN
          i550=i
          EXIT
       ENDIF
    ENDDO

    CALL indexx(nchannels,channels,indx)

!screen obs

    IF ( ANY(aods > aod_missing) .OR. ANY(aods < 0.) .OR. &
         &ANY(aods > aod_550_max)) THEN
       PRINT *,'Screenning obs: missing or negative values'
       j=0
       DO i=1,nobs_channel
          IF ( ANY(aods(:,i) > aod_missing) .OR. ANY(aods(:,i) < 0.) &
               &.OR. aods(i550,i) > aod_550_max) &
               &CYCLE
          j=j+1
       ENDDO
       nobs_channel_screened=j
    ELSE
       nobs_channel_screened=nobs_channel
    ENDIF

!  check 
!  WRITE(6,'(6(f7.0,2x))')[1:nchannels]
!  DO i=1,nchannels
!     WRITE(6,'(6(f7.5,2x))')corr_tmp(i,:)
!  ENDDO
!
!  PRINT *,'***'
!
!  PRINT *,indx

    
    ALLOCATE(aod_nnr_record(nobs_channel_screened))

    WRITE(6,'(a,6f7.0,2x)') 'Channels = ',channels(indx)
    WRITE(6,'(a,i10)') 'Number of coincident multichannel observations = ',nobs_channel_screened

    j=0

    DO i=1,nobs_channel

       IF ( ANY(aods(:,i) > aod_missing) .OR. ANY(aods(:,i) < 0.) &
            &.OR. aods(i550,i) > aod_550_max) &
            &CYCLE

       j=j+1

       IF (ALLOCATED(aod_nnr_record(j)%channels)) &
            &DEALLOCATE(aod_nnr_record(j)%channels)

       IF (ALLOCATED(aod_nnr_record(j)%values))&
            &DEALLOCATE(aod_nnr_record(j)%values)

       ALLOCATE(aod_nnr_record(j)%channels(nchannels),&
            &aod_nnr_record(j)%values(nchannels))

       aod_nnr_record(j)%satellite=satellite
       aod_nnr_record(j)%obstype=obstype
       aod_nnr_record(j)%channels(:)=channels(indx)
       aod_nnr_record(j)%values(:)=aods(indx,i)
       aod_nnr_record(j)%lat=lats(i)
       aod_nnr_record(j)%lon=lons(i)
       aod_nnr_record(j)%obsdate=date_end+timedelta(minutes=times(i)+time_offset)
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

  SUBROUTINE modis_params(satellite,channels,&
       &satellite_id,sensor_id,channel_ids,wavenumbers,frequencies)

    CHARACTER(len=*), INTENT(in) :: satellite
    REAL, INTENT(in) :: channels(:)
    CHARACTER(len=*), INTENT(out) :: satellite_id,&
         &sensor_id
    INTEGER(int32), ALLOCATABLE, INTENT(out) :: channel_ids(:)
    REAL, ALLOCATABLE, INTENT(out) :: wavenumbers(:),frequencies(:)

!see src_mpi_dev/gocart_aod_fv3_mpi.f90

    INTEGER(int32) , PARAMETER :: nchannels_modis=20

    REAL, DIMENSION(nchannels_modis), PARAMETER :: &
         &modis_aqua_channels=[&
         &644.8408, 856.7571, 466.1064, 553.2176, 1242.038,&
         &1627.984, 2112.483, 412.7716, 442.4463, 487.9652,&
         &530.2004, 546.8652, 666.4443, 678.0049, 747.0498,&
         &867.3760, 904.2808, 935.8021, 935.7385, 1382.167]

    REAL, DIMENSION(nchannels_modis), PARAMETER :: &
         &modis_terra_channels=[&
         &644.1409, 855.8562, 465.6163, 553.3287, 1242.543,&
         &1628.997, 2112.717, 410.8992, 441.9383, 487.2798,&
         &529.7424, 546.4805, 666.2437, 676.7939, 746.2441,&
         &866.4699, 904.0203, 935.8414, 935.3293, 1381.289]

    INTEGER(int32) :: nchannels,i

    nchannels=SIZE(channels)

    ALLOCATE(channel_ids(nchannels),wavenumbers(nchannels),&
         &frequencies(nchannels))

    DO i=1,nchannels

       SELECT CASE(NINT(channels(i)))
       CASE(440)
          channel_ids(i)=9
       CASE(470)
          channel_ids(i)=3
       CASE(500)
          channel_ids(i)=10
       CASE(550)
          channel_ids(i)=4
       CASE(660)
          channel_ids(i)=13
       CASE(870)
          channel_ids(i)=16
       CASE default
          PRINT *,'in module_aod_nnr: modis_params - cannot be here'
          STOP(1)
       END SELECT
       
    ENDDO

    IF (INDEX(satellite,'Aqua') > 0) THEN
       satellite_id='aqua'
       sensor_id='v.modis_aqua'
       wavenumbers=10000000./modis_aqua_channels(channel_ids)
       frequencies=speed_of_light/(modis_aqua_channels(channel_ids)*1.e-9)
    ELSE IF (INDEX(satellite,'Terra') > 0) THEN
       satellite_id='terra'
       sensor_id='v.modis_terra'
       wavenumbers=10000000./modis_terra_channels(channel_ids)
       frequencies=speed_of_light/(modis_terra_channels(channel_ids)*1.e-9)
    ELSE
       PRINT *,'in module_aod_nnr: modis_params - cannot be here'
       STOP(2)
    ENDIF
    
  END SUBROUTINE modis_params

  SUBROUTINE modis_nnr_errors(aod_nnr_record,nobs,bias,uncertainty)

    TYPE(aod_nnr), INTENT(in) :: aod_nnr_record(nobs)
    INTEGER(int32), INTENT(in) ::  nobs

    REAL, INTENT(inout) :: bias(:,:),uncertainty(:,:)
    
    CHARACTER(len=max_varname_length) :: obstype
    INTEGER(int32) :: nchannels,i,j

    REAL, ALLOCATABLE :: a(:),b(:)

!errors are taken from H-L method for NNR thinned to C768 (? to fine)
!and eps=0.01

    nchannels=SIZE(aod_nnr_record(1)%channels)

    ALLOCATE(a(nchannels),b(nchannels))

    obstype=aod_nnr_record(1)%obstype

    SELECT CASE(TRIM(obstype))
    CASE("ocean")
       a(:)=[0.001, 0.001, 0.001, 0.001, 0.001]
       b(:)=[0.126, 0.123, 0.119, 0.116, 0.100]
       IF (SIZE([0.001, 0.001, 0.001, 0.001, 0.001]) /= nchannels) THEN
          PRINT*,'in module_aod_nnr:nnr_errors - wrong nchannels'
          STOP(1)
       ENDIF
    CASE("land")
       a(:)=[0.002, 0.002, 0.002, 0.002, 0.002, 0.002]
       b(:)=[0.215, 0.212, 0.210, 0.206, 0.198, 0.187]
       IF (SIZE([0.002, 0.002, 0.002, 0.002, 0.002, 0.002]) /= nchannels) THEN
          PRINT*,'in module_aod_nnr:nnr_errors - wrong nchannels'
          STOP(2)
       ENDIF

    CASE("deep")
       a(:)=[0.002, 0.002, 0.002, 0.002, 0.002]
       b(:)=[0.202, 0.196, 0.192, 0.188, 0.179]
       IF (SIZE([0.002, 0.002, 0.002, 0.002, 0.002]) /= nchannels) THEN
          PRINT*,'in module_aod_nnr:nnr_errors - wrong nchannels'
          STOP(3)
       ENDIF
    CASE default
       PRINT*,'in module_aod_nnr:nnr_errors - cannot be here'
       STOP(4)
    END SELECT

    DO i=1,nobs
       bias=bias_aeronet
       DO j=1,nchannels
          uncertainty(i,j)=MAX(a(j) + b(j)*aod_nnr_record(i)%values(j),&
               &uncertainty_aeronet)
       ENDDO
    ENDDO

    DEALLOCATE(a,b)

  END SUBROUTINE modis_nnr_errors

END MODULE module_aod_nnr

