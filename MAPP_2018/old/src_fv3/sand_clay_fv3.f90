PROGRAM sand_clay_fv3

!MZP to convert sand and clay binaries to ncdf

  USE hdf5
  USE netcdf

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

!neglect agriculture (contains only NH3 species) for GOCART
  INTEGER, PARAMETER :: ndims=3, nspec=2

  CHARACTER(len=32), DIMENSION(nspec), PARAMETER :: &
       &spec_names=(/'sand','clay'/)

  CHARACTER(len=80) :: filenamebin,&
       &filename_sand_bin="sand60_1KM.1gd4r",&
       &filename_clay_bin="clay60_1KM.1gd4r",&
       &filenamencdf="sand_clay_fv3.nc"

  INTEGER, PARAMETER :: nlon=4500, nlat=2250
  
  REAL, PARAMETER :: lon1=-180.,lat1=-90.

  INTEGER :: nx,ny,i,j
  REAL, DIMENSION(nlon,nlat,1,nspec) :: src_dummy
  REAL :: dlon,dlat
  REAL, DIMENSION(nlon) :: lon
  REAL, DIMENSION(nlat) :: lat
  INTEGER :: mcid,status

  INTEGER, DIMENSION(1) :: start_dims1d=(/1/),end_dims1d
  INTEGER, DIMENSION(ndims) :: start_dims3d=(/1,1,1/),&
       &end_dims3d
  CHARACTER(len=80) :: attname,attvalue
  INTEGER :: dimtime_id,time_id, dimlon_id,lon_id, dimlat_id,lat_id

  INTEGER, DIMENSION(nspec) :: var_id

  dlon=360./nlon
  dlat=180./nlat

  lon(1)=lon1
  DO j=2,nlon  
     lon(j)=lon(1) + (j-1)*dlon
  ENDDO

  lat(1)=lat1
  DO i=2,nlat
     lat(i)= lat(1) + (i-1)*dlat
  ENDDO

  i=1
  filenamebin=filename_sand_bin
  PRINT *,TRIM(filenamebin)
  status=LoadFlatDirtData(filenamebin,src_dummy(:,:,1,i))

  i=2
  filenamebin=filename_clay_bin
  PRINT *,TRIM(filenamebin)
  status=LoadFlatDirtData(filenamebin,src_dummy(:,:,1,i))

  DO i=1,2
     PRINT *,MINVAL(src_dummy(:,:,1,i)),MAXVAL(src_dummy(:,:,1,i))
  END DO

  status = nf_create(filenamencdf,ncwrite,mcid)
  IF (status /= NF_NOERR) PRINT*,nf90_strerror(status)

  status = nf_redef(mcid)

  status = nf_def_dim(mcid,'time',NF_UNLIMITED,dimtime_id)
  status = nf_def_var(mcid,'time',NF_REAL,1,dimtime_id,time_id)
  attname="calendar"
  attvalue="none"
  status = nf_put_att_text(mcid, time_id, attname,&
       &LEN_TRIM(attvalue),attvalue)

  status = nf_def_dim(mcid,'lon',nlon,dimlon_id)
  status = nf_def_var(mcid,'lon',NF_REAL,1,dimlon_id,lon_id)

  IF (status /= NF_NOERR) THEN 
     PRINT*,nf90_strerror(status),'lon'
     STOP
  ENDIF
  
  attname="long_name"
  attvalue="longitude at grid cell midpoints"
  status = nf_put_att_text(mcid, lon_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
  attname="units"
  attvalue="degrees_east"
  status = nf_put_att_text(mcid, lon_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
  attname="standard_name"
  attvalue="longitude"
  status = nf_put_att_text(mcid, lon_id, attname,&
       &LEN_TRIM(attvalue),attvalue)

  status = nf_def_dim(mcid,'lat',nlat,dimlat_id)
  status = nf_def_var(mcid,'lat',NF_REAL,1,dimlat_id,lat_id)

  attname="long_name"
  attvalue="latitude at grid cell midpoints"
  status = nf_put_att_text(mcid, lat_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
  attname="units"
  attvalue="degrees_north"
  status = nf_put_att_text(mcid, lat_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
  attname="standard_name"
  attvalue="latitude"
  status = nf_put_att_text(mcid, lat_id, attname,&
       &LEN_TRIM(attvalue),attvalue)

  DO i=1,nspec

     status = nf_def_var(mcid,spec_names(i),NF_REAL,ndims,&
          &(/dimlon_id,dimlat_id,dimtime_id/),var_id(i))
  
     attname="long_name"
     attvalue=TRIM(spec_names(i))
     status = nf_put_att_text(mcid, var_id, attname,&
          &LEN_TRIM(attvalue),attvalue)
     attname="units"
     attvalue="none"
     status = nf_put_att_text(mcid, var_id, attname,&
       &LEN_TRIM(attvalue),attvalue)
     
  ENDDO

  status = nf_enddef(mcid)
     
  end_dims1d=(/1/)
  status = nf_put_vara_real(mcid,time_id,start_dims1d,end_dims1d,&
       &1)

  end_dims1d=(/nlon/)
  status = nf_put_vara_real(mcid,lon_id,start_dims1d,end_dims1d,&
       &lon)

!  IF (status /= NF_NOERR) THEN 
!     PRINT*,nf90_strerror(status),'lon'
!     STOP
!  ENDIF

  end_dims1d=(/nlat/)
  status = nf_put_vara_real(mcid,lat_id,start_dims1d,end_dims1d,&
       &lat)

  end_dims3d=(/nlon,nlat,1/)

  DO i=1,nspec
     status = nf_put_vara_real(mcid,var_id(i),start_dims3d,end_dims3d,&
          &src_dummy(:,:,1,i))
  ENDDO

  status = nf_close(mcid)

CONTAINS

  FUNCTION LoadFlatDirtData ( fname, array ) RESULT ( ostat )
    
    IMPLICIT NONE
    
    CHARACTER ( * ), INTENT ( in ) :: fname  !~ Name of the file to load
    INTEGER                        :: ostat  !~ Function status variable.
!~ Non-zero is bad.
    
    REAL,INTENT (out)  :: array(:,:)
    
!~ Utility variables.
!  ------------------
    INTEGER                        :: i,j,k  !~ Dummy iterators
    INTEGER                        :: retval !~ Internal status variable
    INTEGER                        :: lun    !~ Dummy logical unit number
    INTEGER                        :: nx,ny  !~ Incoming grid dimenstions
    INTEGER                        :: irec   !~ File record number
    LOGICAL                        :: exstat !~ File existance flag
    REAL, ALLOCATABLE              :: DATA ( :,: ) !~ Temporary data array.
    CHARACTER ( 16 )               :: self   !~ Function name
    PARAMETER ( self = 'LoadFlatDirtData' )
    
!~ Initialize variables.
!  ---------------------
    ostat  = 0
    retval = 0
    nx     = 36000
    ny     = 18000
    
!~ Make sure the submitted data file exists.
!  -----------------------------------------
    INQUIRE ( file = TRIM (fname), exist = exstat ) 
    IF ( .NOT. exstat ) THEN
       WRITE ( *,* ) ''
       WRITE ( *,* ) ' ERROR: Submitted dataset does not exist.'
       WRITE ( *,* ) ' Submitted file name: ' // TRIM ( fname )
       WRITE ( *,* ) ' Exiting ' // self
       WRITE ( *,* ) ''
       ostat = 1
       RETURN
    END IF
       
!~ Open the submitted file.
!  -------------------------
    lun = 102
    OPEN ( lun, file = TRIM (fname), action = 'read', form = 'unformatted' &
         ,convert='BIG_ENDIAN', access = 'direct', recl = 36000, iostat = retval ) ! Linux
!         , access = 'direct', recl = 4*36000, iostat = retval )       ! IBM
    
    IF ( retval /= 0 ) THEN
       WRITE ( *,* ) ''
       WRITE ( *,* ) ' ERROR: Unable to open submitted file.' 
       WRITE ( *,* ) ' File name:     ' // TRIM ( fname )
       WRITE ( *,* ) ' Return status: ', retval 
       WRITE ( *,* ) ' Exiting ' // self 
       WRITE ( *,* ) ''
       ostat = retval
       RETURN
    END IF
       
!~ Allocate the temporary storage.
!  -------------------------------
    ALLOCATE ( DATA (nx,ny), stat = retval )
    IF ( retval /= 0 ) THEN
       WRITE ( *,* ) ''
       WRITE ( *,* ) ' ERROR: Unable to allocate storage space for flat data.' 
       WRITE ( *,* ) ' Return status: ', retval
       WRITE ( *,* ) ' Requested dataset: ', TRIM ( fname )
       WRITE ( *,* ) ' Exiting ' // self 
       WRITE ( *,* ) ''
       ostat = retval 
       RETURN
    END IF
       
!~ Initialize temporary array.
!  ---------------------------
    WRITE ( *,* )  ' Initializing...'
    DATA = 0.
    
!~ Loop through the file, load the data.
!  -------------------------------------
    WRITE ( *,* ) ' Reading file...'
    irec = 1
    DO
       READ ( lun, rec = irec, iostat = retval ) DATA ( :,irec+3000 )
       IF ( retval /= 0 ) EXIT    
       irec = irec + 1
    END DO
    
    IF ( irec < ny-3000  ) THEN
       WRITE ( *,* ) ''
       WRITE ( *,* ) ' ERROR: Did not complete reading data file. '
       WRITE ( *,* ) ' File :             ' // TRIM ( fname )
       WRITE ( *,* ) ' Final read record: ', irec
       WRITE ( *,* ) ' Exiting ' // self
       WRITE ( *,* ) ' '
       ostat = 1
       
       IF ( ALLOCATED (DATA) ) DEALLOCATE ( DATA )
       CLOSE ( lun )
       RETURN 
    END IF
    
!~ Close the data file.
!  --------------------
    CLOSE ( lun ) 
    
    array = DATA ( 1:nx:8,1:ny:8 )
    DO i=1,nx/8
       DO j=1,ny/8
          IF (array(i,j) .EQ. -.99) THEN
             array(i,j)=0.
          ENDIF
       ENDDO
    ENDDO
    
    IF ( ALLOCATED (DATA) ) DEALLOCATE ( DATA ) 
    
  END FUNCTION LoadFlatDirtData
  
END PROGRAM sand_clay_fv3
   
