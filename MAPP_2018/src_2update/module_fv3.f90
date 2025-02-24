MODULE module_fv3

!interpolate fv3 AOD to observations
!based on fv3pop, Ning Wang, March 2007

  USE netcdf
  USE datetime_mod
  USE timedelta_mod

  USE module_netcdf_handles
  USE module_misc, ONLY: max_name_length, max_vars, max_dims,&
       &unit_namelist, separator_varnames

  USE module_utils, ONLY: upper2lower, extract_names
  
  IMPLICIT NONE

  INTEGER, PARAMETER :: ntiles=6
  CHARACTER(len=1), DIMENSION(ntiles), PARAMETER :: ctiles = ["1","2","3","4","5","6"]

  PRIVATE

  PUBLIC :: get_aod_fv3, aod_fv3, read_fv3_grid, ntiles, ctiles
  
  TYPE aod_fv3
     TYPE(datetime_type) :: analdate
     TYPE(datetime_type) :: fcstdate
     REAL, ALLOCATABLE :: lats(:)
     REAL, ALLOCATABLE :: lons(:)
     REAL, ALLOCATABLE :: channels(:)
     REAL, ALLOCATABLE :: values(:,:)
  END TYPE aod_fv3

  CHARACTER(len = max_name_length), ALLOCATABLE, DIMENSION(:) :: varnames
  CHARACTER(len = max_name_length), PARAMETER :: &
       &varname_lon='grid_lont',varname_lat='grid_latt'
  INTEGER :: nvars,fcst_hour,nx,ny,nxy,nxyg,nchan,nt

CONTAINS

  SUBROUTINE namelist_fv3(analdate,fcstdate,grid_files,fv3_files)
    
    TYPE(datetime_type), INTENT(inout) :: analdate, fcstdate
    CHARACTER(len = max_name_length), DIMENSION(ntiles), INTENT(inout) :: &
         &grid_files,fv3_files
    CHARACTER(len = 10) :: yyyymmddhh
    INTEGER :: year,month,day,hour
    CHARACTER(len = 3) :: cfhr

    CHARACTER(len = max_name_length) :: input_dir_fv3,input_dir_grid,&
         &var_list

    CHARACTER(len = max_name_length), DIMENSION(max_vars) :: allnames

    INTEGER :: Open_Status,Read_Status
    INTEGER :: stderr

    NAMELIST /record_model/&
         &input_dir_fv3,input_dir_grid,yyyymmddhh,fcst_hour,var_list
    
    CONTINUE

    stderr = 0

    OPEN(unit=unit_namelist, file = "namelist.modis_nnr_correlation_4hl", &
         &status="old",action = "read",iostat=open_status)
    IF (open_status /= 0) THEN
       WRITE(stderr,*) "error: there is no namelist file (namelist.modis_nnr_correlation)"
       STOP
    END IF

    WRITE(stderr,*) 'Reading namelist.modis_nnr_correlation_4hl record_model'
    READ (unit_namelist, NML=record_model, IOSTAT=Read_Status)
    CLOSE(unit_namelist)

    READ(yyyymmddhh,'(i4,3i2)')year,month,day,hour

    analdate=create_datetime(year=year, month=month, day=day, hour=hour)
    fcstdate=analdate+timedelta(hours=fcst_hour)

    CALL extract_names(var_list,separator_varnames,nvars,allnames)

    ALLOCATE(varnames(nvars))
    varnames=allnames(1:nvars)

    WRITE(cfhr,'(i3.3)')fcst_hour

    fv3_files=TRIM(input_dir_fv3)//'/'//upper2lower(TRIM(varnames(1)))//"_"//&
         &TRIM(yyyymmddhh)//".tile"//ctiles//"_"//TRIM(cfhr)//".nc"
    grid_files=TRIM(input_dir_grid)//'/'//"grid_spec.tile"//ctiles//".nc"

  END SUBROUTINE namelist_fv3

  SUBROUTINE read_fv3_grid(griddata,grid_files)
    
    REAL, ALLOCATABLE, DIMENSION(:,:), INTENT(out) :: griddata
    CHARACTER(len = max_name_length), DIMENSION(ntiles) :: grid_files 
    
    INTEGER, DIMENSION(max_dims) :: dimids,dims
    CHARACTER(len = max_name_length) :: input_file
    
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: tmpdata
    INTEGER :: ncid,status,varid_lon,varid_lat,numdims,i,j,l,ij
    CHARACTER(len = max_name_length) :: aname
    
    CONTINUE

    input_file=TRIM(grid_files(1))

    status = nf90_open(input_file, nf90_nowrite, ncid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)
    
    status = nf90_inq_varid(ncid, varname_lon, varid_lon)
    IF (status .NE. nf90_noerr) CALL handle_err(status)
    
    status = nf90_inquire_variable(ncid, varid_lon, aname, ndims=numdims)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inquire_variable(ncid, varid_lon, dimids = dimids(:numdims))
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inq_varid(ncid, varname_lat, varid_lat)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    dims=1

    DO i=1,numdims
       status = nf90_inquire_dimension(ncid,dimids(i),len=dims(i))
       IF (status .NE. nf90_noerr) CALL handle_err(status)
    ENDDO

    nx=dims(1)
    ny=dims(2)
    nxy=nx*ny
    nxyg=ntiles*nxy
    ALLOCATE(tmpdata(nx,ny,2),griddata(2,nxyg))

    status = nf90_close(ncid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)
    
    DO l=1,ntiles
       
       input_file=TRIM(grid_files(l))

       status = nf90_open(input_file, nf90_nowrite, ncid)
       IF (status .NE. nf90_noerr) CALL handle_err(status)

       status = nf90_get_var(ncid,varid_lat,tmpdata(:,:,1), &
            start = (/ 1, 1 /), &
            count = (/ nx, ny /) )
       IF (status .NE. nf90_noerr) CALL handle_err(status)

       status = nf90_get_var(ncid,varid_lon,tmpdata(:,:,2), &
            start = (/ 1, 1 /), &
            count = (/ nx, ny /) )
       IF (status .NE. nf90_noerr) CALL handle_err(status)


       status = nf90_close(ncid)
       IF (status .NE. nf90_noerr) CALL handle_err(status)

       ij=1
       DO j=1,ny
          DO i=1,nx
             griddata(:,ij+(l-1)*nxy) = tmpdata(i,j,:)
             ij=ij+1
          ENDDO
       END DO
       
    ENDDO

    DEALLOCATE(tmpdata)

  END SUBROUTINE read_fv3_grid

  SUBROUTINE read_fv3_vars(varname,vardata,fv3_files)
      
    CHARACTER(len = max_name_length), INTENT(in) :: varname
    REAL, ALLOCATABLE, DIMENSION(:,:,:), INTENT(out) :: vardata
    CHARACTER(len = max_name_length), DIMENSION(ntiles) :: fv3_files

    INTEGER, DIMENSION(max_dims) :: dimids,dims
    CHARACTER(len = max_name_length) :: input_file

    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: tmpdata
    INTEGER :: ncid,status,varid,numdims,i,j,k,l,ij,it
    CHARACTER(len = max_name_length) :: aname

    CONTINUE

    input_file=TRIM(fv3_files(1))

    status = nf90_open(input_file, nf90_nowrite, ncid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inq_varid(ncid, varname, varid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inquire_variable(ncid, varid, aname, ndims=numdims)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inquire_variable(ncid, varid, dimids = dimids(:numdims))
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    dims=1

    DO i=1,numdims
       status = nf90_inquire_dimension(ncid,dimids(i),len=dims(i))
       IF (status .NE. nf90_noerr) CALL handle_err(status)
    ENDDO

    IF (nx /= dims(1) .OR. ny /= dims(2)) THEN
       PRINT *,'Incompatible grid and variable output - Stopping'
       PRINT *,'nx= ',dims(1),'ny= ',dims(2)
       PRINT *,'nxgrid= ',nx,'nygrid= ',ny
       STOP
    ENDIF

    nchan=dims(3)
    nt=dims(4)
    ALLOCATE(tmpdata(nx,ny,nchan,nt),vardata(nt,nchan,nxyg))

    status = nf90_close(ncid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    DO l=1,ntiles

       input_file=TRIM(fv3_files(l))

       status = nf90_open(input_file, nf90_nowrite, ncid)
       IF (status .NE. nf90_noerr) CALL handle_err(status)

       status = nf90_get_var(ncid,varid,tmpdata, &
            start = (/ 1, 1, 1, 1/), &
            count = (/ nx,ny,nchan,nt/) )
       IF (status .NE. nf90_noerr) CALL handle_err(status)

       DO it=1,nt
          DO k=1,nchan
             ij=1
             DO j=1,ny
                DO i=1,nx
                   vardata(it,k,ij+(l-1)*nxy) = tmpdata(i,j,k,it)
                   ij=ij+1
                ENDDO
             END DO
          ENDDO
       ENDDO

       status = nf90_close(ncid)
       IF (status .NE. nf90_noerr) CALL handle_err(status)

    ENDDO

    DEALLOCATE(tmpdata)

  END SUBROUTINE read_fv3_vars

  SUBROUTINE read_fv3_channels(channels,fv3_files)
      
    REAL, ALLOCATABLE, DIMENSION(:), INTENT(out) :: channels
    CHARACTER(len = max_name_length), DIMENSION(ntiles) :: fv3_files

    CHARACTER(len = max_name_length) :: input_file

    INTEGER, DIMENSION(1) :: dimids,dims
    INTEGER :: ncid,status,varid,numdims,i
    CHARACTER(len = max_name_length) :: varname

    CONTINUE

    input_file=TRIM(fv3_files(1))

    status = nf90_open(input_file, nf90_nowrite, ncid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    varname="channels"

    status = nf90_inq_varid(ncid, varname, varid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inquire_variable(ncid, varid, dimids = dimids(:))
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_inquire_dimension(ncid,dimids(1),len=dims(1))
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    IF (nchan /= dims(1)) THEN
       PRINT *,'Incompatible channel dimension'
       PRINT *,'nchan= ',nchan
       PRINT *,'dims(1)= ',dims(1)
       STOP
    ENDIF

    ALLOCATE(channels(nchan))

    status = nf90_get_var(ncid,varid,channels, &
         start = (/ 1 /), &
         count = (/ nchan /) )
    IF (status .NE. nf90_noerr) CALL handle_err(status)

    status = nf90_close(ncid)
    IF (status .NE. nf90_noerr) CALL handle_err(status)

  END SUBROUTINE read_fv3_channels

  SUBROUTINE get_aod_fv3(aod_fv3_record)

    TYPE(aod_fv3) :: aod_fv3_record

    REAL, ALLOCATABLE, DIMENSION(:) :: channels
    REAL, ALLOCATABLE, DIMENSION(:,:) :: griddata
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: vardata

    CHARACTER(len = max_name_length), DIMENSION(ntiles) :: &
         &grid_files,fv3_files
    

    CALL namelist_fv3(aod_fv3_record%analdate,aod_fv3_record%fcstdate,&
         &grid_files,fv3_files)

    IF ((nvars > 1) .OR. (upper2lower(varnames(1)) /= 'aod') ) THEN
       PRINT *,'This code for AOD only - stopping'
       STOP
    ENDIF

    CALL read_fv3_grid(griddata,grid_files)
    ALLOCATE(aod_fv3_record%lats(nxyg),aod_fv3_record%lons(nxyg))

    aod_fv3_record%lats=griddata(1,:)
    aod_fv3_record%lons=griddata(2,:)

    DEALLOCATE(griddata)

    CALL read_fv3_vars(varnames(1),vardata,fv3_files)

    ALLOCATE(aod_fv3_record%values(nchan,nxyg))

    aod_fv3_record%values(:,:)=vardata(1,:,:)

    DEALLOCATE(vardata)

    CALL read_fv3_channels(channels,fv3_files)

    ALLOCATE(aod_fv3_record%channels(nchan))

    aod_fv3_record%channels=channels

    DEALLOCATE(channels)
    
  END SUBROUTINE get_aod_fv3

END MODULE module_fv3

