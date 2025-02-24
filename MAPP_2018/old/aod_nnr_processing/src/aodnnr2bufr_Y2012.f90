PROGRAM aodnnr2bufr
!  to convert nasa's nnr aod to bufr for GSI
!  loosely based on W. Wolf's modis_aod_hdf2bufr
!  MP, March 2014

  USE netcdf
  USE type_kinds

  IMPLICIT NONE

  INTEGER :: ncid, status, RecordDimID
  CHARACTER(len = nf90_max_name) :: RecordDimName
  CHARACTER(len = nf90_max_name), PARAMETER :: &
       &latname='lat',lonname='lon',timename='time',obsname='obs',&
       &qcname='qcexcl',dayname='syn_beg'

  CHARACTER(len = nf90_max_name) :: aname
  CHARACTER (len=250)  :: input_dir,input_file,output_dir,output_file  

  INTEGER, DIMENSION(:,:), ALLOCATABLE :: time,qc,lat,lon
  
  REAL, DIMENSION(:,:), ALLOCATABLE :: obs
  
  INTEGER :: latid,lonid,timeid,obsid,qcid,numdims,dayid

  INTEGER, DIMENSION(nf90_max_var_dims) :: dimids

  INTEGER :: nbatches,batchlen,jday_start,jday_current,hour_current,i,j,k
  INTEGER :: year,month,day,hour,minute,second,&
       &year_ref,month_ref,day_ref,time_offset

  CHARACTER(LEN=19)  :: ob_time

  CHARACTER(len=5), PARAMETER :: caqua='MYD04', cterra='MOD04'
  CHARACTER(len=5), PARAMETER :: cocean='ocean'
  CHARACTER(len=4), PARAMETER :: cland='land'

  LOGICAL :: aqua,terra,land,ocean

  LOGICAL :: inside_domain
  LOGICAL :: check_time, check_domain    ! external functions

  REAL(DOUBLE), PARAMETER :: bmiss = 10.e+10
  INTEGER(long) :: idate
  REAL(DOUBLE) :: fov(13), fov1(2), fov2(3)
  INTEGER(LONG), PARAMETER :: NESDIS_ID = 1   

  INTEGER :: unit_out=10,unit_table=20,unit_namelist=30

  REAL(SINGLE)     :: lat_ll, lat_ur, lon_ll, lon_ur, rlat, rlon
  LOGICAL :: select_domain
  INTEGER(LONG) :: stderr = 0
  INTEGER(LONG) :: Read_Status
  INTEGER(LONG) :: Open_Status

  NAMELIST /record1/ &
       &input_dir, input_file, &
       &output_dir, output_file,&
       &select_domain, lat_ll, lat_ur, lon_ll, lon_ur
  
  inside_domain = .TRUE.  ! initialize
  select_domain = .FALSE. ! initialize
  
  lat_ll=-90.
  lat_ur=90.
  lon_ll=-180.
  lon_ur=180.

  OPEN (unit=unit_namelist, file = "namelist.modis_nnr_converter", &
       &status="old",action = "read",iostat=open_status)
  
  IF (open_status /= 0) THEN
     WRITE(stderr,*) "error: there is no namelist file (namelist.modis_nnr_converter)"
     STOP
  END IF
  
  WRITE(stderr,*) 'Reading namelist.modis_nnr_converter'
  READ (unit_namelist, NML=record1, IOSTAT=Read_Status)

  IF (Read_Status /= 0) THEN
     WRITE(stderr,*) 'Error reading record1 of namelist.modis_nnr_converter'
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

  IF ( (ocean .OR. land) == .FALSE. ) THEN
     PRINT *,'Wrong names ocean/land ',ocean,land
  ENDIF

  IF ( (ocean .AND. land) == .TRUE. ) THEN
     PRINT *,'Wrong names ocean/land ',ocean,land
  ENDIF

  IF ( (aqua .OR. terra) == .FALSE. ) THEN
     PRINT *,'Wrong names aqua/terra ',aqua,terra
  ENDIF

  IF ( (aqua .AND. terra) == .TRUE. ) THEN
     PRINT *,'Wrong names aqua/terra ',aqua,terra
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

  status = nf90_get_att(ncid, dayid, 'value_at_reference_date',jday_start)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  CALL gdate (jday_start, year_ref,month_ref,day_ref)
  
  status = nf90_get_att(ncid, dayid, 'first_julian_day',jday_current)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  status = nf90_get_att(ncid, dayid, 'latest_synoptic_hour',hour_current)
  IF (status .NE. nf90_noerr) CALL handle_err(status)

  hour_current=hour_current/6*6

  CALL gdate (jday_current, year,month,day)

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
       &obs(nbatches,batchlen))

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
 
  status = nf90_close(ncid)

  idate=year*1000000+month*10000+ &
       day*100+hour_current

  IF (terra) THEN
     fov1(1) = 783
  ELSE IF (aqua) THEN
     fov1(1) = 784
  ENDIF

  fov1(2)=NESDIS_ID
     
  IF (ocean) THEN
     fov2(1)=5
  ELSE
     fov2(1)=6
  ENDIF

  fov2(2)=0
  fov2(3)=3
! in BUFR: 'STYP DBCF QAOD'
! styp=5 for different error =.2*(aod+0.01) and modify setup aod
! dbcf=0 not importand only for deep blue
! qaod=3 obs is retained when > qaod

  fov(9) = bmiss
  fov(10) = bmiss
  fov(11) = bmiss
!  fov(12) = bmiss os obs
  fov(13) = bmiss

!  fov(9) = MODAerRec%Solar_Zenith(Index_2D)
!  fov(10) = MODAerRec%Solar_Azimuth(Index_2D) + 180.0
!  fov(11) = MODAerRec%Scattering_Angle(Index_2D)
!  fov(12) = MODAerRec%OD_Land_and_Ocean(Index_2D)
!  fov(13) = MODAerRec%Aerosol_Type_Land(Index_2D)
!  SOZA SOLAZI SCATTA OPTD AEROTP

  output_file=TRIM(output_dir)//'/'//TRIM(output_file)

  OPEN(unit_table,file='bufrtab.008')

  OPEN(unit_out,file=TRIM(output_file),action='write' &
       ,form='unformatted')

  PRINT *,'@@1',unit_out,unit_table


  CALL openbf(unit_out,'OUT',unit_table)
  PRINT *,'@@2'
  CALL openmb(unit_out,'NC008041',idate)

  PRINT *,'@@3'

  k=0

  DO i=1,nbatches
     DO j=1,batchlen  
        IF (qc(i,j) == 0) THEN
           time(i,j)=time(i,j)+time_offset
           hour=time(i,j)/60
           minute=MOD(time(i,j),60)
           rlon=lon(i,j)*.01
           rlat=lat(i,j)*.01

           IF ( select_domain ) THEN
              inside_domain = check_domain(&
                   &rlat,rlon,lat_ll, lat_ur, lon_ll, lon_ur)
              IF (.NOT. inside_domain) CYCLE
           ENDIF

           fov(1)=rlon
           fov(2)=rlat
           fov(3)=year
           fov(4)=month
           fov(5)=day
           fov(6)=hour
           fov(7)=minute
           fov(8)=0
           fov(12)=obs(i,j)
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
  
END PROGRAM aodnnr2bufr

!---------------------------------------------------------------------
SUBROUTINE handle_err(status)
  INTEGER status
  WRITE(6,*) 'Error number ',status
  STOP
END SUBROUTINE handle_err
!---------------------------------------------------------------------


FUNCTION check_domain(rlat, rlon, lat_ll, lat_ur, lon_ll, lon_ur)
   USE type_kinds
   REAL(SINGLE), INTENT(IN) :: rlat, rlon, lat_ll, lat_ur, lon_ll, lon_ur
   LOGICAL                  :: check_domain
   IF ( (rlon >= lon_ll .and. rlon <= lon_ur) .and. &
        (rlat >= lat_ll .and. rlat <= lat_ur) ) THEN
      check_domain =  .true.
   ELSE
      check_domain =  .false.
   END IF
END FUNCTION check_domain



