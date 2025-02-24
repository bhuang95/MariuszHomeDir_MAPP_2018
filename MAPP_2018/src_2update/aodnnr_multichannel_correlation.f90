PROGRAM aodnnr_multichannel_correlation
!  to calcaulte correlation matrix for nasa's nnr multichannel aods 
!  MP, Sept 2018

  USE netcdf

  USE datetime_mod
  USE timedelta_mod
  
  USE lapack95

  IMPLICIT NONE

!for 6-hourly bufr
  INTEGER, PARAMETER :: cycle_frequency=6
  REAL, PARAMETER :: aod_missing=100.

  REAL     :: lat_ll, lat_ur, lon_ll, lon_ur, rlat, rlon
  INTEGER :: stderr = 0
  INTEGER :: Read_Status
  INTEGER :: Open_Status

  LOGICAL :: select_domain
  
  INTEGER :: ncid, status, RecordDimID
  CHARACTER(len = nf90_max_name) :: RecordDimName
  CHARACTER(len = nf90_max_name), PARAMETER :: &
       &latname='lat',lonname='lon',timename='time',obsname='obs',&
       &qcname='qcexcl',dayname='syn_beg',levname='lev'

  CHARACTER(len = nf90_max_name) :: aname
  CHARACTER (len=250)  :: input_dir,input_file,output_dir,output_file  
  CHARACTER (len=10) :: cdate_ref

  INTEGER, DIMENSION(:,:), ALLOCATABLE :: time,qc,lat,lon
  INTEGER, DIMENSION(:), ALLOCATABLE :: indx
  
  REAL, DIMENSION(:,:), ALLOCATABLE :: obs,lev,aods,corr,corr_tmp,eval_tmp
  REAL, DIMENSION(:), ALLOCATABLE :: channels,aod_means,obs_tmp,eval
  
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

  LOGICAL :: aqua,terra,land,ocean,deep

  LOGICAL :: inside_domain

  INTEGER :: unit_out=10,unit_namelist=30

  TYPE(datetime_type) :: date_start, date_end, date_obs
  TYPE(timedelta_type) :: dt

  NAMELIST /record1/ &
       &input_dir, input_file, &
       &output_dir, output_file,&
       &select_domain, lat_ll, lat_ur, lon_ll, lon_ur
  
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

  OPEN (unit=unit_namelist, file = "namelist.modis_nnr_correlation", &
       &status="old",action = "read",iostat=open_status)
  
  IF (open_status /= 0) THEN
     WRITE(stderr,*) "error: there is no namelist file (namelist.modis_nnr_correlation)"
     STOP
  END IF
  
  WRITE(stderr,*) 'Reading namelist.modis_nnr_correlation'
  READ (unit_namelist, NML=record1, IOSTAT=Read_Status)

  IF (Read_Status /= 0) THEN
     WRITE(stderr,*) 'Error reading record1 of namelist.modis_nnr_correlation'
     STOP
  END IF

!ncdf file names

  IF (INDEX(TRIM(input_file),caqua) > 0 ) THEN
     aqua=.TRUE.
  ELSE
     aqua=.false.
  ENDIF

  IF (INDEX(TRIM(input_file),cterra) > 0 ) THEN 
     terra=.TRUE.
  ELSE
     terra=.FALSE.
  ENDIF

  IF (INDEX(TRIM(input_file),cland) > 0 ) THEN 
     land=.TRUE.
  ELSE
     land=.FALSE.
  ENDIF

  IF (INDEX(TRIM(input_file),cocean) > 0 ) THEN 
     ocean =.TRUE.
  ELSE
     ocean=.FALSE.
  ENDIF

  IF (INDEX(TRIM(input_file),cdeep) > 0 ) THEN 
     deep=.TRUE.
  ELSE
     deep=.FALSE.
  ENDIF

  IF ( (COUNT([ocean,land,deep]) /= 1) .OR. (COUNT([aqua,terra]) /= 1) ) THEN
     PRINT *,'Wrong names ocean/land/deep ',ocean,land,deep
     PRINT *,'or satellites ',aqua,terra
     STOP
  ENDIF

  input_file=TRIM(input_dir)//'/'//TRIM(input_file)

  status = nf90_open(input_file, nf90_nowrite, ncid)
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

  hour_current=(hour_current/cycle_frequency)*cycle_frequency

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

  status = nf90_inquire_variable(ncid, latid, aname, ndims=numdims)
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

  output_file=TRIM(output_dir)//'/'//TRIM(output_file)

  OPEN(unit_out,file=TRIM(output_file),action='write',&
       &form='unformatted')

  nobs_channel=COUNT(lev==lev(1,1))
  nobs_total=COUNT(obs < aod_missing)

  IF (MOD(nobs_total,nobs_channel)) THEN
     PRINT *,'Incorrrect number of obs ',nobs_total,nobs_channel
     STOP
  ENDIF

  nchannels=nobs_total/nobs_channel

  ALLOCATE(channels(nchannels),aods(nchannels,nobs_channel),aod_means(nchannels),&
       &corr(nchannels,nchannels),corr_tmp(nchannels,nchannels),&
       &eval_tmp(nchannels,nchannels),indx(nchannels))

!  aods=RESHAPE(lev,SHAPE(aods),order=[2,1]) ! check if works OK
  aods=RESHAPE(obs,SHAPE(aods),order=[2,1]) !order refers to dimenisons of aods

  ALLOCATE(obs_tmp(nobs_total))
  obs_tmp=RESHAPE(lev,SHAPE(obs_tmp))

  channels=[obs_tmp(1::nobs_channel)]

  DEALLOCATE(lev,obs_tmp)

  aod_means=SUM(aods,dim=2)/nobs_channel

  DO i=1,nchannels
     aods(i,:)=aods(i,:)-aod_means(i)
  ENDDO

  DO i=1,nchannels
     corr_tmp(i,i)=SQRT(SUM(aods(i,:)**2))
  ENDDO

  DO i=1,nchannels
     DO j=i+1,nchannels
        corr_tmp(i,j)=SUM(aods(i,:)*aods(j,:))/(corr_tmp(i,i)*corr_tmp(j,j))
        corr_tmp(j,i)=corr_tmp(i,j)
     ENDDO
     corr_tmp(i,i)=1.
  ENDDO

  CALL indexx(nchannels,channels,indx)

  corr(:,:)=corr_tmp(indx,indx)


  WRITE(6,'(a,6f7.0)') 'Channels = ',channels
  WRITE(6,'(a,i10)') 'Number of coincident multichannel observations = ',nobs_channel
  WRITE(6,'(a,6(f7.5,2x))') 'Channel means = ',aod_means

!  check 
!  WRITE(6,'(6(f7.0,2x))')[1:nchannels]
!  DO i=1,nchannels
!     WRITE(6,'(6(f7.5,2x))')corr_tmp(i,:)
!  ENDDO
!
!  PRINT *,'***'
!
!  PRINT *,indx

  WRITE(6,'(6(f7.0,2x))')channels(indx)
  DO i=1,nchannels
     WRITE(6,'(6(f7.5,2x))')corr(i,:)
  ENDDO

  corr_tmp=corr
  
  ALLOCATE(eval(nchannels))

  CALL syev(corr,eval,'V')

  eval_tmp=0.

  FORALL(i=1:nchannels)eval_tmp(i,i)=eval(i)

  WRITE(6,'(a,6(f7.5,2x))') 'Eigenvalues = ',eval

!check if egv's calculated correctly  
!  PRINT *,'***'
!  DO i=1,nchannels
!     WRITE(6,'(6(f7.5,2x))')corr_tmp(i,:)
!  ENDDO
!
!  PRINT *,'***'
!  WRITE(6,'(6(f7.5,2x))')MATMUL(MATMUL(corr,eval_tmp),TRANSPOSE(corr))

!corr contains eigenvectors
!eval contains eigenvalues


  DEALLOCATE(channels,aods,aod_means,corr,corr_tmp,indx,eval,eval_tmp)

CONTAINS

!---------------------------------------------------------------------
  SUBROUTINE handle_err(status)
    INTEGER status
    WRITE(6,*) 'Error number ',status
    STOP
  END SUBROUTINE handle_err
!---------------------------------------------------------------------
  
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
  
END PROGRAM aodnnr_multichannel_correlation




