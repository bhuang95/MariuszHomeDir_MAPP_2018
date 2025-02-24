!===============================================================================
! Post processor utility for fv3
!
!
! Main packages: slint (spherical linear interpolation),
!                vlint (vertical linear interpolation),
!                nimnc (netCDF utility routines),
!                gribio (grib utility routines) 
!
! Ning Wang, March 2007 
!
!
!===============================================================================

PROGRAM fv3pop
      USE slint, ONLY: slint_init, slint_init_read, bilinear_interp, &
                       bilinear_interp_uv
!      USE nimnc, only: init_cdf_vars, init_cdf_vars_v, write_data, &
!                       close_cdf,  write_cdf_cdfapi
      USE varinfo, only: var_info, var_attr, set_model_nlevels, set_model_nplevels
      USE netcdf

      IMPLICIT NONE

      INTEGER, PARAMETER         :: max_path_len = 512
      INTEGER, PARAMETER         :: max_vars = 200
      INTEGER, PARAMETER         :: max_varname_len = 16

      LOGICAL                    :: file_ex

      CHARACTER(len=6)           :: ats
      CHARACTER(len=8)           :: TIMESTEPFMT='(I5.5)'
      CHARACTER(len=19      )    :: date_str
      CHARACTER(len=10      )    :: jdate 
      CHARACTER(len=2)           :: gls
      CHARACTER(len=14)          :: yyyymmddhhmmss
      CHARACTER(len=4)           :: output_fmt
      CHARACTER(len=16)          :: input_filetype
      CHARACTER(len=1)           :: ich
      CHARACTER(max_path_len)    :: input, output, grid_file
      CHARACTER(max_path_len)    :: gribfile, binfile, data_file
      CHARACTER(max_path_len)    :: gribtable, inputdir, outputdir
      CHARACTER(max_path_len)    :: coeff_file
      CHARACTER(max_varname_len) :: var_list(max_vars)
      CHARACTER(max_varname_len) :: var_name, var_name2

      INTEGER                    :: mx, my, t1, t2, nfct
      INTEGER                    :: ierr, ncsp, tile_sz
      INTEGER                    :: ncids(6), stat, ncid, nsmooth_var(max_vars)
      INTEGER                    :: varid, tile_beg, tile_end
      INTEGER                    :: file_handle 
      INTEGER                    :: var_num, nlevels
      INTEGER                    :: year, month, day, hour, minute, jday, IW3JDN, is2Dvar
      INTEGER                    :: cres, nz, grid_id, is, npls, numvars, tbeg, tend, delta_t 
      INTEGER                    :: nt, nt_h, i, j, k, idx

      TYPE(VAR_ATTR)             :: var_attr_info

      REAL                       :: r2d, d2r, pi
      REAL                       :: vlevels(200)
      REAL, ALLOCATABLE          :: data_xyz(:,:,:), vardata(:), src_data(:), tgt_data(:)
      REAL, ALLOCATABLE          :: data_xyz2(:,:,:), vardata2(:), src_data2(:), tgt_data2(:) 
      REAL, ALLOCATABLE          :: ll_src(:,:), ll_tgt(:,:), lat(:), lon(:)


      NAMELIST  /fv3post/ yyyymmddhhmmss, cres, nz, inputdir, outputdir, input_filetype, &
       output_fmt, gribtable, grid_id, is, npls, numvars, var_list, tbeg, tend, delta_t

! check if a NIM namelist file exists
      INQUIRE(FILE='FV3pop.nl', EXIST = file_ex) 
      IF (.not. file_ex) THEN
        WRITE(0,*) 'Usage: fv3pop'
        WRITE(0,*) 'Note: The namelist file FV3pop.nl should be in the current directory'
        STOP
      END IF
  
      PRINT*, 'Running fv3pop ...'

      pi = acos(-1.0)
      r2d = 180.0 / pi 
      d2r = pi / 180.0

! read namelist
      OPEN (10,file="FV3pop.nl",status='old',action='read',iostat=ierr)
      IF (ierr.ne.0) THEN
        WRITE (0,'(a)') 'ERROR: Could not open namelsit file FV3pop.nl.'
        STOP
      ENDIF
      READ (10,NML=fv3post,iostat=ierr)
      IF (ierr /= 0) THEN
        WRITE (0,'(a)') 'ERROR: Could not read namelist entry fv3post.'
        STOP
      ENDIF
      CLOSE(10)

! display namelist

#ifdef DIAG
      PRINT*, 'yyyymmddhhmmss:', yyyymmddhhmmss
      PRINT*, 'cres, nz:', cres, nz
      PRINT*, 'inputdir, outputdir, output_fmt, gribtable, grid_id:'
      PRINT*, trim(inputdir),  '        ', trim(outputdir), '         ', &
              trim(output_fmt), '      ',trim(gribtable),  grid_id
      PRINT*, 'is, npls, numvars, var_list:'
      PRINT*,  is, npls, numvars, var_list
      PRINT*, 'tbeg, tend, delta_t:', tbeg, tend, delta_t
#endif
      
      tile_sz = cres*cres
      ncsp = tile_sz*6
      ALLOCATE(lat(cres*cres), lon(cres*cres))

      nsmooth_var=0  ! don't do any smoothing at this point.

      t1 = tbeg
      t2 = tend
      
! get date info from the date string
      READ(UNIT=yyyymmddhhmmss(1:4), FMT='(I4)') year
      READ(UNIT=yyyymmddhhmmss(5:6), FMT='(I2)') month
      READ(UNIT=yyyymmddhhmmss(7:8), FMT='(I2)') day
      READ(UNIT=yyyymmddhhmmss(9:10), FMT='(I2)') hour
      READ(UNIT=yyyymmddhhmmss(11:12), FMT='(I2)') minute

! create a year 'month-date-hour-minute' date string
      date_str = yyyymmddhhmmss(1:4) // "-" // yyyymmddhhmmss(5:6) // "-" & 
                 // yyyymmddhhmmss(7:8) // "-" // yyyymmddhhmmss(9:10) &
                 // ":" // yyyymmddhhmmss(11:12) // ":00"
      PRINT*, 'Model starting date and time:', date_str
      PRINT*, 't1, t2, delta_t:', t1, t2, delta_t
      PRINT*, 'nz:', nz 
     
! create the jdate string
      jday = IW3JDN(year,month,day) - IW3JDN(year,1, 1) + 1
      WRITE(UNIT=jdate(1:2), FMT='(I2.2)') MOD (year, 100) 
      WRITE(UNIT=jdate(3:5), FMT='(I3.3)') jday 
      WRITE(UNIT=jdate(6:7), FMT='(I2.2)') hour 
      jdate = jdate(1:7) // '000'

! compute the number of forecast time 
      nfct = (t2 - t1) / delta_t 

! assigne the mx and my values for the lat/lon grid mesh
      IF (output_fmt == "grib") THEN
        CALL gridid2mxmy(grid_id, mx, my)
      ELSE
        mx = 720
        my = 361
      ENDIF

! allocate memory for different interpolation schemes
      ALLOCATE(vardata(ncsp * (nz+1))) ! one cubed sphere model data field
      ALLOCATE(vardata2(ncsp * (nz+1))) ! one cubed sphere model data field
      ALLOCATE(data_xyz(mx, my, nz+1)) ! one Cartesian data field
      ALLOCATE(data_xyz2(mx, my, nz+1)) ! one Cartesian data field
      ALLOCATE(src_data(ncsp), src_data2(ncsp)) ! one level of 
      ALLOCATE(tgt_data(mx*my), tgt_data2(mx*my))

! open and init the netCDF or GRIB file
      IF (output_fmt == "nc") THEN
        output = "ncfile" 
!        CALL init_cdf_vars(output, file_handle, mx, my, nz+1, nfct + 1,&
!               1, numvars, var_list, date_str)
      ELSEIF (output_fmt == "grib") THEN
        CALL set_model_nlevels(nz)
        CALL set_model_nplevels(npls)
        CALL initgrib(gribtable)
      END IF
    
      inputdir = inputdir(1:LEN_TRIM(inputdir)) // '/'

! read in grid latlon info
      ALLOCATE(ll_src(ncsp, 2))
      DO i = 1, 6
        WRITE(ich, '(I1)') i
        grid_file = trim(inputdir) // yyyymmddhhmmss //  ".grid_spec.tile" // ich // ".nc"
#ifdef DIAG
        PRINT*, 'Open cubed sphere grid spec file ',  trim(grid_file)
#endif
        stat = nf90_open(trim(grid_file), NF90_NOWRITE, ncid)
        CALL nc_opchk(stat,'nf90_open')
!        PRINT*, 'Read in longitude from ',  trim(grid_file)
        stat = nf90_inq_varid(ncid,'grid_lont',varid)
        CALL nc_opchk(stat,'nf90_inq_dimid')
        stat = nf90_get_var(ncid,varid,lon, start =(/ 1, 1 /), count=(/ cres, cres /))
        CALL nc_opchk(stat,'nf90_get_var')
!        PRINT*, 'Read in latitude from ',  trim(grid_file)
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
      IF (is /= 0) THEN
        ALLOCATE(ll_tgt(mx*my, 2))
        CALL getTgtGrid(grid_id, ll_tgt, mx,my)
        WRITE(coeff_file, '("grid_",I3.3,"_coeffs")') grid_id
        INQUIRE(FILE=coeff_file, EXIST = file_ex) 
        IF (file_ex) THEN
          CALL slint_init_read(coeff_file)
        ELSE
          CALL slint_init(ll_src, ncsp, ll_tgt, mx*my)
        ENDIF
        DEALLOCATE(ll_src)
        DEALLOCATE(ll_tgt)
      ENDIF
    
      PRINT*, 'Starting remapping, output file encoding, and writing ...'

      DO i = 1, 6
        WRITE(ich, '(I1)') i
        IF (input_filetype == 'fcst_prod') THEN
          data_file = trim(inputdir) // yyyymmddhhmmss //  ".atmos_4xdaily.tile" // ich // ".nc"
        ELSE IF (input_filetype == 'nggps3d') THEN
          data_file = trim(inputdir) // yyyymmddhhmmss //  ".nggps3d.tile" // ich // ".nc"
        ELSE IF (input_filetype == 'nggps2d') THEN
          data_file = trim(inputdir) // yyyymmddhhmmss //  ".nggps2d.tile" // ich // ".nc"
        END IF
#ifdef DIAG
        PRINT*, 'Open cubed sphere date file ',  trim(data_file)
#endif
        stat = nf90_open(trim(data_file), NF90_NOWRITE, ncids(i))
        CALL nc_opchk(stat,'nf90_open')
      END DO

      IF (is == 1) THEN ! Horizontal interpolation
        DO nt = t1, t2, delta_t
          IF (output_fmt.eq."grib") THEN
            WRITE(ats, TIMESTEPFMT) nt
            nt_h = nt
            gribfile = TRIM(outputdir) // '/' // jdate(1:LEN_TRIM(jdate)-2) // ats
            CALL opengrib(gribfile)
          ELSEIF (output_fmt.eq."bin") THEN
            WRITE(ats, TIMESTEPFMT) nt
            binfile = TRIM(outputdir) // '/' // jdate(1:LEN_TRIM(jdate)) // TRIM(ats) // '.LL'
            OPEN(10,file=binfile,form='unformatted')
          ENDIF
          DO var_num = 1, numvars ! for each variable
            var_name = TRIM(var_list(var_num))
            CALL var_info(var_name, var_attr_info)
            nlevels = var_attr_info%nlevels
            vlevels = 0.0
            IF (.NOT. isVvar(var_name)) THEN
              CALL read_1var(ncids, nt/delta_t, var_name, vardata, nlevels, vlevels)
            ENDIF
            IF (isUvar(var_name)) THEN
              var_name2 = 'v' // var_name(2:LEN_TRIM(var_name))
              CALL var_info(var_name2, var_attr_info)
              CALL read_1var(ncids, nt/delta_t, var_name2, vardata2, nlevels, vlevels)
            ENDIF

            IF (isUvar(var_name)) THEN
              DO k=1,nlevels
                DO i = 1, ncsp
                  src_data(i) = vardata((k - 1) * ncsp + i)
                  src_data2(i) = vardata2((k - 1) * ncsp + i)
                END DO
                CALL bilinear_interp_uv(src_data, tgt_data, src_data2, tgt_data2) 
                DO i = 1, mx
                  DO j = 1, my
                    idx = (j - 1) * mx + i 
!                    data_xyz(i,j,k) = cs_rot(idx,1)*tgt_data(idx)-cs_rot(idx,2)*tgt_data2(idx)
!                    data_xyz2(i,j,k) = cs_rot(idx,2)*tgt_data(idx)+cs_rot(idx,1)*tgt_data2(idx) 
                    data_xyz(i,j,k) = tgt_data(idx)
                    data_xyz2(i,j,k) = tgt_data2(idx) 
                  END DO
                END DO
              ENDDO
            ELSE IF (isVvar(var_name)) THEN
              CYCLE
            ELSE
              DO k=1,nlevels
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
            ENDIF
            DO i = 1, nsmooth_var(var_num)
              CALL smooth(data_xyz,mx,my,nlevels,0.2)
            END DO
            IF (nlevels < nz + 1) THEN
              data_xyz(:,:,nlevels + 1:nz + 1) = 0.0
            END IF
            IF (output_fmt == "nc") THEN
!              CALL write_data (file_handle, date_str, var_name, & 
!              var_description, data_xyz, units, nt / delta_t) 
            ELSEIF (output_fmt == "grib") THEN
              CALL var_info(var_name, var_attr_info)
              var_name = TRIM(var_attr_info%var_postname)
              CALL writegrib(var_name, mx, my, nlevels, 0, 0, data_xyz, & 
                   jdate, nt_h,tba(nt,t1,delta_t,var_name), vlevels)
              IF (isUvar(var_name)) THEN
                CALL var_info(var_name2, var_attr_info)
                var_name2 = TRIM(var_attr_info%var_postname)
                CALL writegrib(var_name2, mx, my, nlevels, 0, 0, data_xyz2, &
                   jdate, nt_h,tba(nt,t1,delta_t,var_name), vlevels)
              ENDIF
            ELSEIF (output_fmt == "bin") THEN
              CALL write_bin(var_name,yyyymmddhhmmss,mx,my,nlevels,data_xyz,nt)
              IF (isUvar(var_name)) THEN
                CALL write_bin(var_name2,yyyymmddhhmmss,mx,my,nlevels,data_xyz2,nt)
              ENDIF
            ENDIF
          END DO 
          CALL mmdddateadj(date_str, delta_t)  
          IF (output_fmt.eq."grib") CALL closegrib()
          PRINT "('*'$)"  ! progress '*'
        END DO 
      ENDIF

      IF (output_fmt == "nc") THEN
!        CALL close_cdf(file_handle)
!        IF (is == 1) THEN
!          CALL write_cdf_cdfapi(output, mx, my, nz+1, nfct + 1, date_str, delta_t, 0)
!        ELSEIF (is == 2) THEN
!          CALL write_cdf_cdfapi(output, mx, my, npls, nfct + 1, date_str, delta_t, 1)
!        ELSEIF (is == 3) THEN
!          CALL write_cdf_cdfapi(output, mx, my, npls, nfct + 1, date_str, delta_t, 2)
!        ENDIF
      ELSEIF (output_fmt == "grib") THEN
        CALL endgrib()
      ELSEIF (output_fmt == "bin") THEN
        CLOSE(10)
      ENDIF

      PRINT*, '  '

      IF (is == 1) THEN
        DEALLOCATE(vardata, vardata2, data_xyz,data_xyz2)
        DEALLOCATE(src_data, src_data2, tgt_data, tgt_data2)
      ENDIF

      STOP

CONTAINS

INTEGER FUNCTION tba (nt, t1, delta_t, varname)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nt, t1, delta_t
      CHARACTER(len=*), INTENT(in) :: varname
      IF (nt >= delta_t .and. (varname == 'r12D' .or.  &
          varname == 'r22D' .or. varname == 'r32D')) then
        tba = nt - delta_t
      ELSE IF (nt >= 6 .and. (varname == 'r42D' .or.  &
          varname == 'r52D' .or. varname == 'r62D')) then
        tba = nt - 6
      ELSE IF (nt >= delta_t .and. (varname == 's12D' .or.  &
          varname == 's22D' .or. varname == 's32D')) then
        tba = nt - delta_t
      ELSE IF (nt >= 6 .and. (varname == 's42D' .or.  &
          varname == 's52D' .or. varname == 's62D')) then
        tba = nt - 6
      ELSE
        tba = 0
      ENDIF
      RETURN
END FUNCTION tba


SUBROUTINE read_1var(ncids, nt, var_name, vardata, nz, vlevels)
      IMPLICIT NONE
      
      INTEGER ncids(:), nt, nz
      CHARACTER (len=*) :: var_name
      REAL vardata(:), vlevels(nz)

      INTEGER varid, varid_vl, i, numTimes, grid_xt, grid_yt, nvlevs
      INTEGER, DIMENSION(nf90_max_var_dims) :: dimIDs
      REAL datatmp(cres*cres*nz)

      stat = nf90_inq_varid(ncids(1),trim(var_name),varid)
      CALL nc_opchk(stat,'nf90_inq_varid, in read_1var')

      IF (nz == 1) THEN
        nvlevs = 1
        stat = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
        CALL nc_opchk(stat,'nf90_inquire_variable, in read_1var')
        stat = nf90_inquire_dimension(ncid, dimIDs(3), len = numTimes)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var')
        stat = nf90_inquire_dimension(ncid, dimIDs(2), len = grid_yt)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var')
        stat = nf90_inquire_dimension(ncid, dimIDs(1), len = grid_xt)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var')
      ELSE 
        stat = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
        CALL nc_opchk(stat,'nf90_inquire_variable, in read_1var, nz/= 1')
        stat = nf90_inquire_dimension(ncid, dimIDs(4), len = numTimes)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var, nz/= 1')
        stat = nf90_inquire_dimension(ncid, dimIDs(3), len = nvlevs)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var, nz/= 1')
        stat = nf90_inquire_dimension(ncid, dimIDs(2), len = grid_yt)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var, nz/= 1')
        stat = nf90_inquire_dimension(ncid, dimIDs(1), len = grid_xt)
        CALL nc_opchk(stat,'nf90_inquire_dimension, in read_1var, nz/= 1')
        IF (nvlevs == 63) THEN
          stat = nf90_inq_varid(ncids(1),'pfull',varid_vl)
          CALL nc_opchk(stat,'nf90_inq_varid - pfull, in read_1var')
          stat = nf90_get_var(ncids(1),varid_vl,vlevels, &
                  start = (/ 1 /) , count = (/ nvlevs /) )
          CALL nc_opchk(stat,'nf90_get_var, in read_1var - pfull, nz /= 1')
        ELSE IF (nvlevs == 64) THEN 
          stat = nf90_inq_varid(ncids(1),'phalf',varid_vl)
          CALL nc_opchk(stat,'nf90_inq_varid - phalf, in read_1var')
          stat = nf90_get_var(ncids(1),varid_vl,vlevels, &
                 start = (/ 1 /) , count = (/ nvlevs /) )
          CALL nc_opchk(stat,'nf90_get_var, in read_1var - phalf, nz /= 1')
        ENDIF
      ENDIF

      
      DO i = 1, 6 
#ifdef DIAG
        PRINT*, 'Read in data in tile #', i
#endif
        IF (nvlevs == 1) THEN
          stat = nf90_get_var(ncids(i),varid,datatmp, &
                 start = (/ 1, 1, nt /), &
                 count = (/ cres, cres, 1 /) )
          CALL nc_opchk(stat,'nf90_get_var, in read_1var')
          vardata((i-1)*cres*cres*nz+1:i*cres*cres*nz) =  &
            datatmp(1:cres*cres*nz)
         ELSE
          stat = nf90_get_var(ncids(i),varid,datatmp, &
                 start = (/ 1, 1, 1, nt /), &
                 count = (/ cres, cres, nvlevs, 1 /) )
          CALL nc_opchk(stat,'nf90_get_var, in read_1var, nz /= 1')
          vardata((i-1)*cres*cres*nz+1:i*cres*cres*nz) =  &
            datatmp(1:cres*cres*nz)
         END IF
 
      END DO
      
END SUBROUTINE read_1var

LOGICAL FUNCTION isUvar(var_name)
      IMPLICIT NONE

      CHARACTER (len=*) :: var_name

      IF (var_name(1:1) == 'u' ) THEN
        isUvar = .true.
      ELSE
        isUvar = .false.
      ENDIF
      RETURN
END FUNCTION isUvar

LOGICAL FUNCTION isVvar(var_name)
      IMPLICIT NONE

      CHARACTER (len=*) :: var_name

      IF (var_name(1:1) == 'v' ) THEN
        isVvar = .true.
      ELSE
        isVvar = .false.
      ENDIF
      RETURN
END FUNCTION isVvar

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
      IF (mod(year, 4) == 0 .AND. mod(year, 100) /= 0) THEN
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
      msg = trim(opname) // ' Error, status code and message:'
      PRINT*,trim(msg), stat, nf90_strerror(stat)
      STOP
    END IF

END SUBROUTINE nc_opchk

END PROGRAM
