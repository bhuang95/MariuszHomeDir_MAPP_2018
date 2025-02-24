PROGRAM aodncdf2grib
!based on Ning Wang's fv3pop
!still requires changes to grib by explicitly writing on the Gaussian
!grid rather than on the natuve FV3 grid  as it should be 
!will need to be mpodieife to output all wavelengths - now only 550 nm

  USE slint, ONLY: slint_init, slint_init_read, bilinear_interp

  USE netcdf

  IMPLICIT NONE

  INTEGER, PARAMETER         :: max_path_len = NF90_MAX_NAME
  INTEGER, PARAMETER         :: max_vars = 200
  INTEGER, PARAMETER         :: max_varname_len = 16
  INTEGER, PARAMETER         :: ntiles=6
  INTEGER, PARAMETER         :: nlevels=1


  LOGICAL                    :: file_ex

  CHARACTER(len=19      )    :: date_str
  CHARACTER(len=10      )    :: jdate 
  CHARACTER(len=10)          :: yyyymmddhh
  CHARACTER(len=4)           :: output_fmt
  CHARACTER(len=16)          :: input_filetype
  CHARACTER(len=1)           :: ich
  CHARACTER(max_path_len)    :: input, output, grid_file
  CHARACTER(max_path_len)    :: gribfile,  data_file
  CHARACTER(max_path_len)    :: gribtable, inputdir, gridfiledir, outputdir
  CHARACTER(max_path_len)    :: coeff_file
  CHARACTER(max_varname_len) :: var_list(max_vars)
  CHARACTER(max_varname_len) :: var_name
  CHARACTER(len=3)           :: ct

  INTEGER                    :: mx, my, t1, t2, nfct
  INTEGER                    :: ierr, ncsp, tile_sz
  INTEGER                    :: ncids(ntiles), stat, ncid
  INTEGER                    :: varid, tile_beg, tile_end
  INTEGER                    :: file_handle 
  INTEGER                    :: var_num
  INTEGER                    :: year, month, day, hour, minute, jday, IW3JDN
  INTEGER                    :: cres, grid_id, is, numvars, tbeg, tend, delta_t 
  INTEGER                    :: nt, nt_h, i, j, k, idx
  INTEGER                    :: nchan_nnr,nchan_nnr_select


  REAL                       :: r2d, d2r, pi
  REAL                       :: vlevels(max_vars)
  REAL, ALLOCATABLE          :: data_xyz(:,:,:), vardata(:), src_data(:), tgt_data(:)
  REAL, ALLOCATABLE          :: ll_src(:,:), ll_tgt(:,:), lat(:), lon(:)


  NAMELIST  /record/ yyyymmddhh, cres, inputdir, gridfiledir, outputdir, input_filetype, &
       gribtable, grid_id, is, numvars, var_list, nchan_nnr, nchan_nnr_select, &
       &tbeg, tend, delta_t

! check if a NIM namelist file exists

  CONTINUE

  INQUIRE(FILE='aodncdf2grib.nl', EXIST = file_ex) 

  output_fmt="grib"

  IF (.NOT. file_ex) THEN
     WRITE(0,*) 'Usage: aodncdf2grib.x'
     WRITE(0,*) 'Note: The namelist file aodncdf2grib.nl should be in the current directory'
     STOP
  END IF

  PRINT*, 'Running aodncdf2grib.x  ...'

  pi = ACOS(-1.0)
  r2d = 180.0 / pi 
  d2r = pi / 180.0

! read namelist
  OPEN (10,file="aodncdf2grib.nl",status='old',action='read',iostat=ierr)
  IF (ierr.NE.0) THEN
     WRITE (0,'(a)') 'ERROR: Could not open namelsit file aodncdf2grib.nl'
     STOP
  ENDIF
  READ (10,NML=record,iostat=ierr)
  IF (ierr /= 0) THEN
     PRINT *,yyyymmddhh, cres, inputdir, gridfiledir, outputdir, input_filetype, &
       gribtable, grid_id, is, numvars, var_list, nchan_nnr, nchan_nnr_select, &
       &tbeg, tend, delta_t
     WRITE (0,'(a)') 'ERROR: Could not read namelist entry aodncdf2grib.nl.'
     STOP
  ENDIF
  CLOSE(10)

  tile_sz = cres*cres
  ncsp = tile_sz*6
  ALLOCATE(lat(cres*cres), lon(cres*cres))

  t1 = tbeg
  t2 = tend

! get date info from the date string
  READ(UNIT=yyyymmddhh, FMT='(i4,3i2)') year,month,day,hour

! create a year 'month-date-hour-minute' date string
  date_str = yyyymmddhh(1:4) // "-" // yyyymmddhh(5:6) // "-" & 
       // yyyymmddhh(7:8) // "-" // yyyymmddhh(9:10) &
       // ":00:00"
  PRINT*, 'Model starting date and time:', date_str
  PRINT*, 't1, t2, delta_t:', t1, t2, delta_t

! create the jdate string
  jday = IW3JDN(year,month,day) - IW3JDN(year,1, 1) + 1

  WRITE(jdate, FMT='(I2.2,I3.3,I2.2,I3.3)') MOD (year, 100),jday,hour,0

! compute the number of forecast time 
  nfct = (t2 - t1) / delta_t 

! assigne the mx and my values for the lat/lon grid mesh
  CALL gridid2mxmy(grid_id, mx, my)

! allocate memory for different interpolation schemes
  ALLOCATE(vardata(ncsp * nchan_nnr)) ! one cubed sphere model data field
  ALLOCATE(data_xyz(mx, my, nchan_nnr)) ! one Cartesian data field
  ALLOCATE(src_data(ncsp)) 
  ALLOCATE(tgt_data(mx*my))

! open and init GRIB file

  CALL initgrib(gribtable)

  inputdir = inputdir(1:LEN_TRIM(inputdir)) // '/'
  gridfiledir = gridfiledir(1:LEN_TRIM(gridfiledir)) // '/'

! read in grid latlon info
  ALLOCATE(ll_src(ncsp, 2))

  DO i = 1, ntiles
     WRITE(ich, '(I1)') i
     grid_file = TRIM(gridfiledir) //"grid_spec.tile" // ich // ".nc"
     stat = nf90_open(TRIM(grid_file), NF90_NOWRITE, ncid)
     CALL nc_opchk(stat,'nf90_open')
!        PRINT*, 'Read in longitude from ',  trim(grid_file)
     stat = nf90_inq_varid(ncid,'grid_lont',varid)
     CALL nc_opchk(stat,'nf90_inq_dimid')
     stat = nf90_get_var(ncid,varid,lon, start =(/ 1, 1 /), count=(/ cres, cres /))
     CALL nc_opchk(stat,'nf90_get_var')
     stat = nf90_inq_varid(ncid,'grid_latt',varid)
     CALL nc_opchk(stat,'nf90_inq_dimid')
     stat = nf90_get_var(ncid,varid,lat, start =(/ 1, 1 /), count=(/ cres, cres /))
     CALL nc_opchk(stat,'nf90_get_var')
     stat = nf90_close(ncid)
     CALL nc_opchk(stat,'nf90_close')

     tile_beg = (i -1)*tile_sz + 1
     tile_end = i*tile_sz 
     ll_src(tile_beg:tile_end, 1) = lat * d2r
     ll_src(tile_beg:tile_end, 2) = lon * d2r

  END DO

! if interpolation scheme is not 0, initialize the horizontal interpolation.
  ALLOCATE(ll_tgt(mx*my, 2))

  CALL getTgtGrid(grid_id, ll_tgt, mx,my)
  CALL slint_init(ll_src, ncsp, ll_tgt, mx*my)

  DEALLOCATE(ll_src)
  DEALLOCATE(ll_tgt)

  PRINT*, 'Starting remapping, output file encoding, and writing ...'

  gribfile = TRIM(outputdir) // '/' // 'aod_'//TRIM(yyyymmddhh)//".grib"
  PRINT *,TRIM(gribfile)
  CALL opengrib(gribfile)

  vardata=0.
  data_xyz=0.

  DO nt = t1, t2, delta_t

     WRITE(ct,'(i3.3)')nt

     DO i = 1, ntiles

        WRITE(ich, '(I1)') i

        IF (input_filetype == 'aod') THEN
           data_file = TRIM(inputdir) //"aod_"//TRIM(yyyymmddhh)//&
                &".tile" // ich//"_"//ct//".nc"
        ELSE
           PRINT *,"ONLY aod file type allowed here - stopping"
           STOP
        END IF

        stat = nf90_open(TRIM(data_file), NF90_NOWRITE, ncids(i))

        CALL nc_opchk(stat,'nf90_open')

     END DO

     DO var_num = 1, numvars ! for each variable

        var_name = TRIM(var_list(var_num))

        CALL read_aod(ncids, 1, var_name, vardata, nchan_nnr)

        DO k=1,nchan_nnr

           DO i = 1, ncsp
              src_data(i) = vardata((k - 1) * ncsp + i)
           END DO

           CALL bilinear_interp(src_data, tgt_data)
           
           DO i = 1, mx
              DO j = 1, my
                 data_xyz(i, j, k) = tgt_data((j - 1) * mx + i)
              END DO
           END DO

        END DO

        nt_h = nt

        PRINT *,nt,'AOD nnr 550 nm min/max ',&
             &MINVAL(data_xyz(:,:,nchan_nnr_select)),&
             &MAXVAL(data_xyz(:,:,nchan_nnr_select))

!just single channel output - may need to be corrected later 
        CALL writegrib(var_name, mx, my, nlevels, 0, 0, data_xyz(:,:,nchan_nnr_select), & 
             jdate, nt_h,tba(nt,t1,delta_t,var_name), vlevels)

     END DO

     CALL mmdddateadj(date_str, delta_t)  

  END DO

  CALL closegrib()

  CALL endgrib()

  DEALLOCATE(vardata, data_xyz, src_data, tgt_data)

  STOP

CONTAINS

  INTEGER FUNCTION tba (nt, t1, delta_t, varname)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nt, t1, delta_t
    CHARACTER(len=*), INTENT(in) :: varname
    IF (nt >= delta_t .AND. (varname == 'r12D' .OR.  &
         varname == 'r22D' .OR. varname == 'r32D')) THEN
       tba = nt - delta_t
    ELSE IF (nt >= 6 .AND. (varname == 'r42D' .OR.  &
         varname == 'r52D' .OR. varname == 'r62D')) THEN
       tba = nt - 6
    ELSE IF (nt >= delta_t .AND. (varname == 's12D' .OR.  &
         varname == 's22D' .OR. varname == 's32D')) THEN
       tba = nt - delta_t
    ELSE IF (nt >= 6 .AND. (varname == 's42D' .OR.  &
         varname == 's52D' .OR. varname == 's62D')) THEN
       tba = nt - 6
    ELSE
       tba = 0
    ENDIF
    RETURN
  END FUNCTION tba

  SUBROUTINE mmdddateadj(mmdddate, delta_t)
    IMPLICIT NONE
    CHARACTER*19 mmdddate
    INTEGER delta_t

    INTEGER year, month, day, hour, minute,second,ly 
    INTEGER month_ny(12), month_ly(12)
    CHARACTER dash, col
    DATA month_ny/31,28,31,30,31,30,31,31,30,31,30,31/
    DATA month_ly/31,29,31,30,31,30,31,31,30,31,30,31/
    dash = '-'
    col = ':'

    READ(mmdddate,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') year,month,day,hour,minute,second
    IF (MOD(year, 4) == 0 .AND. MOD(year, 100) /= 0) THEN
       ly = 1
    ELSE
       ly = 0 
    ENDIF
    IF (hour + delta_t .GE. 24) THEN
       day = day + 1
       IF ((ly == 0 .AND. day > month_ny(month)) .OR. (ly == 1 .AND. day > month_ly(month))) THEN
          day = 1
          month = month + 1
       ENDIF
       IF (month > 12) THEN
          month = 1
          year = year + 1
       ENDIF
    ENDIF
    hour = MOD(hour + delta_t, 24)
    WRITE(mmdddate,'(i4.4,A1,i2.2,A1,i2.2,A1,i2.2,A1,i2.2,A1,i2.2)') year,dash,month,dash,day,dash,hour,col,minute,col,second 

  END SUBROUTINE mmdddateadj

  SUBROUTINE nc_opchk(stat,opname)
    IMPLICIT NONE
    INTEGER stat
    CHARACTER(len=*) opname
    CHARACTER(64) msg

    IF (stat .NE.0)  THEN
       msg = TRIM(opname) // ' Error, status code and message:'
       PRINT*,TRIM(msg), stat, nf90_strerror(stat)
       STOP
    END IF

  END SUBROUTINE nc_opchk

  SUBROUTINE read_aod(ncids, nt, var_name, vardata, nchans)
    IMPLICIT NONE

    INTEGER ncids(:), nt, nchans
    CHARACTER (len=*) :: var_name
    REAL vardata(:)

    INTEGER varid, varid_vl, i, numTimes, grid_xt, grid_yt, nchans_ncdf
    INTEGER, DIMENSION(nf90_max_var_dims) :: dimIDs
    REAL datatmp(cres*cres*nchans)

    ncid=ncids(1)

    stat = nf90_inq_varid(ncid,trim(var_name),varid)
    CALL nc_opchk(stat,'nf90_inq_varid, in read_aod')

    stat = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
    CALL nc_opchk(stat,'nf90_inquire_variable, in read_aod, nchan/= 1')
    stat = nf90_inquire_dimension(ncid, dimIDs(4), len = numTimes)
    CALL nc_opchk(stat,'nf90_inquire_dimension, in read_aod, nchan/= 1')
    stat = nf90_inquire_dimension(ncid, dimIDs(3), len = nchans_ncdf)
    CALL nc_opchk(stat,'nf90_inquire_dimension, in read_aod, nchan/= 1')
    stat = nf90_inquire_dimension(ncid, dimIDs(2), len = grid_yt)
    CALL nc_opchk(stat,'nf90_inquire_dimension, in read_aod, nchan/= 1')
    stat = nf90_inquire_dimension(ncid, dimIDs(1), len = grid_xt)
    CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var, nchan/= 1')
    
    IF (nchans_ncdf /= nchans) THEN
       PRINT *,'NNR channels missing in aod file'
       PRINT *,'nchans_ncdf = ',nchans_ncdf
       PRINT *,'input nchans = ',nchans
       STOP
    ENDIF

    DO i = 1, ntiles

       stat = nf90_get_var(ncids(i),varid,datatmp, &
            start = (/ 1, 1, 1, nt /), &
            count = (/ cres, cres, nchans, 1 /) )
       CALL nc_opchk(stat,'nf90_get_var, in read_aod')
       vardata((i-1)*cres*cres*nchans+1:i*cres*cres*nchans) =  &
            datatmp(1:cres*cres*nchans)

    END DO

  END SUBROUTINE read_aod

END PROGRAM
