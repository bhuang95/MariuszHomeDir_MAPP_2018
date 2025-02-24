PROGRAM aodnnr_multichannel_2bufr
!  to convert nasa's nnr multichannel aod to bufr for GSI only for 550nm
!  MP, Aug 2018

  USE netcdf

  USE datetime_mod
  USE timedelta_mod

  IMPLICIT NONE

  INTEGER, PARAMETER :: Byte   = SELECTED_INT_KIND(1), &   ! Byte integer
       SHORT  = SELECTED_INT_KIND(4), &   ! Short integer
       LONG   = SELECTED_INT_KIND(8), &   ! Long integer
       SINGLE = SELECTED_REAL_KIND(6), &  ! Single precision
       DOUBLE = SELECTED_REAL_KIND(12)    ! Double precision

!for 6-hourly bufr
  INTEGER, PARAMETER :: cycle_frequency=6,aod_550=550

  REAL(DOUBLE), PARAMETER :: bmiss = 10.e+10
  INTEGER(LONG) :: idate
  REAL(DOUBLE) :: fov(13), fov1(2), fov2(3)
  INTEGER(LONG), PARAMETER :: NESDIS_ID = 1   

  REAL(SINGLE)     :: lat_ll, lat_ur, lon_ll, lon_ur, rlat, rlon
  INTEGER(LONG) :: stderr = 0
  INTEGER(LONG) :: Read_Status
  INTEGER(LONG) :: Open_Status

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
  
  REAL, DIMENSION(:,:), ALLOCATABLE :: obs,lev
  
  INTEGER :: latid,lonid,timeid,obsid,qcid,numdims,dayid,levid

  INTEGER, DIMENSION(nf90_max_var_dims) :: dimids

  INTEGER :: nbatches,batchlen,jday_ref,jday_current,jday_current_tmp,hour_current,i,j,k
  INTEGER :: year,month,day,hour,minute,second,&
       &year_ref,month_ref,day_ref,time_offset

  CHARACTER(LEN=19)  :: ob_time

  CHARACTER(len=5), PARAMETER :: caqua='MYD04', cterra='MOD04'
  CHARACTER(len=5), PARAMETER :: cocean='ocean'
  CHARACTER(len=4), PARAMETER :: cland='land'
  CHARACTER(len=4), PARAMETER :: cdeep='deep'

  LOGICAL :: aqua,terra,land,ocean,deep

  LOGICAL :: inside_domain

  INTEGER :: unit_out=10,unit_table=20,unit_namelist=30

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

  OPEN (unit=unit_namelist, file = "namelist.modis_nnr_2bufr", &
       &status="old",action = "read",iostat=open_status)
  
  IF (open_status /= 0) THEN
     WRITE(stderr,*) "error: there is no namelist file (namelist.modis_nnr_2bufr)"
     STOP
  END IF
  
  WRITE(stderr,*) 'Reading namelist.modis_nnr_2bufr'
  READ (unit_namelist, NML=record1, IOSTAT=Read_Status)

  IF (Read_Status /= 0) THEN
     WRITE(stderr,*) 'Error reading record1 of namelist.modis_nnr_2bufr'
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

  idate=date_end%year*1000000+date_end%month*10000+ &
       date_end%day*100+hour_current

  IF (terra) THEN
     fov1(1) = 783
  ELSE IF (aqua) THEN
     fov1(1) = 784
  ENDIF

  fov1(2)=NESDIS_ID

  IF (ocean) THEN
     fov2(1)=5
  ELSE IF (land) THEN
     fov2(1)=6
  ELSE !deep blue
     fov2(1)=5 !since 7 cannot be coded as STYP only 3 bits allowed in the table 
  ENDIF

  fov2(2)=0
  fov2(3)=3
! in BUFR: 'STYP DBCF QAOD'
! styp=0,1,2 for different error =.2*(aod+0.01) 
! dbcf=0 not important  only for deep blue for modis - deos not count for NNR
! qaod=3 obs is retained when > qaod

  fov(9) = bmiss
  fov(10) = bmiss
  fov(11) = bmiss
!  fov(12) = bmiss -> obs
  fov(13) = bmiss 

!  fov(9) = MODAerRec%Solar_Zenith(Index_2D)
!  fov(10) = MODAerRec%Solar_Azimuth(Index_2D) + 180.0
!  fov(11) = MODAerRec%Scattering_Angle(Index_2D)
!  fov(12) = MODAerRec%OD_Land_and_Ocean(Index_2D)
!  fov(13) = MODAerRec%Aerosol_Type_Land(Index_2D)
!  SOZA SOLAZI SCATTA OPTD AEROTP
!  AEROTP used to code wavelength

  output_file=TRIM(output_dir)//'/'//TRIM(output_file)

  OPEN(unit_table,file='bufrtab.008')

  OPEN(unit_out,file=TRIM(output_file),action='write' &
       ,form='unformatted')

  CALL openbf(unit_out,'OUT',unit_table)
  CALL openmb(unit_out,'NC008041',idate)

  k=0

  DO j=1,batchlen  
     DO i=1,nbatches
        IF ( (qc(i,j) == 0) .AND. ((NINT(lev(i,j))-aod_550) == 0) ) THEN

           dt=timedelta(minutes=time(i,j)+time_offset)

           date_obs=date_end+dt

           rlon=lon(i,j)*.01
           rlat=lat(i,j)*.01

           IF ( select_domain ) THEN
              inside_domain = check_domain(&
                   &rlat,rlon,lat_ll, lat_ur, lon_ll, lon_ur)
              IF (.NOT. inside_domain) CYCLE
           ENDIF

           fov(1)=rlon
           fov(2)=rlat
           fov(3)=date_obs%year
           fov(4)=date_obs%month
           fov(5)=date_obs%day
           fov(6)=date_obs%hour
           fov(7)=date_obs%minute
           fov(8)=0
           fov(12)=obs(i,j)

!           PRINT *,date_obs%isoformat()
!           PRINT *,obs(i,j),lev(i,j)

           CALL ufbint(unit_out,fov1,2,1,status, 'SAID AODS')
           CALL ufbint(unit_out,fov,13,1,status,  &
                'CLONH CLATH YEAR MNTH DAYS HOUR MINU SECO &
                &SOZA SOLAZI SCATTA OPTD AEROTP')
           CALL ufbint(unit_out,fov2,3,1,status, 'STYP DBCF QAOD')
           CALL writcp(unit_out)
           k=k+1
        ENDIF
     ENDDO
  ENDDO

  CALL closmg(unit_out)
  CALL closbf(unit_out)
  CLOSE(unit_table)
  
  PRINT *,k,' observations written to bufr'

CONTAINS

!---------------------------------------------------------------------
  SUBROUTINE handle_err(status)
    INTEGER status
    WRITE(6,*) 'Error number ',status
    STOP
  END SUBROUTINE handle_err
!---------------------------------------------------------------------
  
  FUNCTION check_domain(rlat, rlon, lat_ll, lat_ur, lon_ll, lon_ur)
    REAL(SINGLE), INTENT(IN) :: rlat, rlon, lat_ll, lat_ur, lon_ll, lon_ur
    LOGICAL                  :: check_domain
    IF ( (rlon >= lon_ll .AND. rlon <= lon_ur) .AND. &
         (rlat >= lat_ll .AND. rlat <= lat_ur) ) THEN
       check_domain =  .TRUE.
    ELSE
       check_domain =  .FALSE.
    END IF
  END FUNCTION check_domain
  
END PROGRAM aodnnr_multichannel_2bufr




