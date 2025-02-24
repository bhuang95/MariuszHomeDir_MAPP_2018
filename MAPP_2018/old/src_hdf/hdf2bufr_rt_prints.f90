PROGRAM hdf2bufr_rt

!to convert VIIRS aot and dust/smoke mask hdf files to bufr

  USE HDF5
  USE ISO_C_BINDING

  IMPLICIT NONE

  CHARACTER(LEN=250):: filename_aot,filename_mask,filename_out

  CHARACTER(LEN=80) :: dataset_faot = &
       &"/AOD550"
  INTEGER, PARAMETER :: nqaotflags=1
  CHARACTER(LEN=80), DIMENSION(nqaotflags) :: dataset_qaot = &
       &(/ "/QCAll" /)
  INTEGER(HID_T)  :: file_aot, space_faot, dset_faot, space_qaot, dset_qaot
  INTEGER :: rank_faot,rank_qaot
  REAL, DIMENSION(:,:), ALLOCATABLE, TARGET :: faot
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE, TARGET :: qaot
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: dims_faot,dims_qaot

  CHARACTER(len=8) :: cdate
  CHARACTER(len=4) :: ctime

  CHARACTER(LEN=80) :: dataset_lat = &
       &"/VIIRS_ADP_IP/Geolocation Fields/Latitude"
  CHARACTER(LEN=80) :: dataset_lon = &
       &"/VIIRS_ADP_IP/Geolocation Fields/Longitude"
  CHARACTER(LEN=80) :: dataset_adp_flags = &
       &"/VIIRS_ADP_IP/Data Fields/VIIRS ADP Flags"
  CHARACTER(LEN=80) :: dataset_adp_qflags = &
       &"/VIIRS_ADP_IP/Data Fields/VIIRS ADP Quality Flags"

  INTEGER, PARAMETER :: dust_index=3, smoke_index=4

  INTEGER(HID_T)  :: file_mask, space_lat, dset_lat, space_lon, dset_lon,&
       &space_adp_flags,dset_adp_flags,space_adp_qflags,dset_adp_qflags

  INTEGER :: rank_lat,rank_lon,rank_adp_flags,rank_adp_qflags
  REAL, DIMENSION(:,:), ALLOCATABLE, TARGET :: lat,lon
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE, TARGET :: adp_flags,adp_qflags
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: dims_lat,dims_lon
  INTEGER(HSIZE_T), DIMENSION(1:3)   :: dims_adp_flags,dims_adp_qflags

  CHARACTER(len=8), DIMENSION(:,:,:), ALLOCATABLE :: adp_qflags_binary
  
  TYPE(C_PTR) :: f_ptr_real, f_ptr_integer
  INTEGER(HSIZE_T), DIMENSION(1:3) :: maxdims
  CHARACTER(len=8) :: char
  INTEGER :: hdferr,i,j,k,iargc
  

!  maxdims = (/H5S_UNLIMITED_f, H5S_UNLIMITED_f, H5S_UNLIMITED_f/)

  IF (iargc() < 3) THEN
     PRINT *,'Needs input and output file names - Stopping'
     STOP
  ENDIF
  
  CALL getarg(1,filename_aot)
  CALL getarg(2,filename_mask)
  CALL getarg(3,filename_out)

  i=INDEX(filename_aot,'Enterprise_d',back=.TRUE.)
  j=LEN('Enterprise_d')

  cdate=filename_aot(i+j:i+j+8)
  ctime=filename_aot(i+j+10:i+j+13)

  CALL h5open_f(hdferr)
!  PRINT *,hdferr
  CALL h5fopen_f(filename_aot, H5F_ACC_RDONLY_F, file_aot, hdferr)
!  PRINT *,hdferr  

!faot
  CALL h5dopen_f(file_aot, dataset_faot, dset_faot, hdferr)
!  PRINT *,hdferr
  CALL h5dget_space_f (dset_faot, space_faot, hdferr)
!  PRINT *,hdferr
  CALL h5sget_simple_extent_ndims_f(space_faot, rank_faot, hdferr)
!  PRINT *,hdferr,rank_faot
  CALL h5sget_simple_extent_dims_f(space_faot, dims_faot, maxdims(1:rank_faot), hdferr)
!  PRINT *,hdferr,dims_faot,maxdims(1:rank_faot)
  ALLOCATE(faot(dims_faot(1),dims_faot(2)))
!  PRINT *,SIZE(faot(:,1)),SIZE(faot(1,:))

  f_ptr_real = C_LOC(faot(1,1))
  CALL h5dread_f(dset_faot, H5T_NATIVE_REAL, f_ptr_real, hdferr)
!  PRINT *,hdferr

!  PRINT *,'faot(1890,318),faot(1890,319)'
!  PRINT *,faot(1890,318),faot(1890,319)

  CALL h5dclose_f (dset_faot, hdferr)
!  PRINT *,hdferr
  CALL h5sclose_f (space_faot, hdferr)
!  PRINT *,hdferr
  
!qflags

  DO k=1,nqaotflags
     CALL h5dopen_f(file_aot, dataset_qaot(k), dset_qaot, hdferr)
!     PRINT *,hdferr,k
     CALL h5dget_space_f (dset_qaot, space_qaot, hdferr)
!     PRINT *,hdferr,k

     IF (NOT(ALLOCATED(qaot))) THEN
        CALL h5sget_simple_extent_ndims_f(space_qaot, rank_qaot, hdferr)
!        PRINT *,hdferr,rank_qaot
        CALL h5sget_simple_extent_dims_f(space_qaot, dims_qaot, maxdims(1:rank_qaot), hdferr)
!        PRINT *,hdferr,dims_qaot,maxdims(1:rank_qaot)
        ALLOCATE(qaot(dims_qaot(1),dims_qaot(2),nqaotflags))
!        PRINT *,SIZE(qaot(:,1,1)),SIZE(qaot(1,:,1))
     ENDIF

     f_ptr_integer = C_LOC(qaot(1,1,k))
     CALL h5dread_f(dset_qaot, H5T_NATIVE_INTEGER, f_ptr_integer, hdferr)
!     PRINT *,hdferr,k
     
!     PRINT *,'qaot(1890,317,k),qaot(1890,318,k)'
!     PRINT *,qaot(1890,317,k),qaot(1890,318,k)
     
     CALL h5dclose_f (dset_qaot, hdferr)
!     PRINT *,hdferr,k
     CALL h5sclose_f (space_qaot, hdferr)
!     PRINT *,hdferr,k

  ENDDO

  CALL h5fclose_f (file_aot, hdferr)
!  PRINT *,hdferr

!  PRINT *,'all read and closed for file ',TRIM(filename_aot)

  CALL h5open_f(hdferr)
!  PRINT *,hdferr
  CALL h5fopen_f(filename_mask, H5F_ACC_RDONLY_F, file_mask, hdferr)
!  PRINT *,hdferr  

!lat
  CALL h5dopen_f(file_mask, dataset_lat, dset_lat, hdferr)
!  PRINT *,hdferr
  CALL h5dget_space_f (dset_lat, space_lat, hdferr)
!  PRINT *,hdferr
  CALL h5sget_simple_extent_ndims_f(space_lat, rank_lat, hdferr)
!  PRINT *,hdferr,rank_lat
  CALL h5sget_simple_extent_dims_f(space_lat, dims_lat, maxdims(1:rank_lat), hdferr)
!  PRINT *,hdferr,dims_lat,maxdims(1:rank_lat)
  ALLOCATE(lat(dims_lat(1),dims_lat(2)))
!  PRINT *,SIZE(lat(:,1)),SIZE(lat(1,:))

  f_ptr_real = C_LOC(lat(1,1))
  CALL h5dread_f(dset_lat, H5T_NATIVE_REAL, f_ptr_real, hdferr)
!  PRINT *,hdferr

!  PRINT *,'lat(1890,318),lat(1890,319)'
!  PRINT *,lat(1890,318),lat(1890,319)

  CALL h5dclose_f (dset_lat, hdferr)
!  PRINT *,hdferr
  CALL h5sclose_f (space_lat, hdferr)
!  PRINT *,hdferr

!lon
  CALL h5dopen_f(file_mask, dataset_lon, dset_lon, hdferr)
! PRINT *,hdferr
  CALL h5dget_space_f (dset_lon, space_lon, hdferr)
! PRINT *,hdferr
  CALL h5sget_simple_extent_ndims_f(space_lon, rank_lon, hdferr)
! PRINT *,hdferr,rank_lon
  CALL h5sget_simple_extent_dims_f(space_lon, dims_lon, maxdims(1:rank_lon), hdferr)
! PRINT *,hdferr,dims_lon,maxdims(1:rank_lon)
  ALLOCATE(lon(dims_lon(1),dims_lon(2)))
! PRINT *,SIZE(lon(:,1)),SIZE(lon(1,:))
  
  f_ptr_real = C_LOC(lon(1,1))
  CALL h5dread_f(dset_lon, H5T_NATIVE_REAL, f_ptr_real, hdferr)
! PRINT *,hdferr
  
! PRINT *,'lon(1890,318),lon(1890,319)'
! PRINT *,lon(1890,318),lon(1890,319)
  
  CALL h5dclose_f (dset_lon, hdferr)
! PRINT *,hdferr
  CALL h5sclose_f (space_lon, hdferr)
! PRINT *,hdferr

!adp_flags
  CALL h5dopen_f(file_mask, dataset_adp_flags, dset_adp_flags, hdferr)
!  PRINT *,hdferr
  CALL h5dget_space_f (dset_adp_flags, space_adp_flags, hdferr)
!  PRINT *,hdferr
  CALL h5sget_simple_extent_ndims_f(space_adp_flags, rank_adp_flags, hdferr)
!  PRINT *,hdferr,rank_adp_flags
  CALL h5sget_simple_extent_dims_f(space_adp_flags, dims_adp_flags, maxdims, hdferr)
!  PRINT *,hdferr,dims_adp_flags,maxdims
  ALLOCATE(adp_flags(dims_adp_flags(1),dims_adp_flags(2),dims_adp_flags(3)))
  
!  PRINT *,SIZE(adp_flags(:,1,1)),SIZE(adp_flags(1,:,1)),SIZE(adp_flags(1,1,:))
  
  f_ptr_integer = C_LOC(adp_flags(1,1,1))
  CALL h5dread_f(dset_adp_flags, H5T_NATIVE_INTEGER, f_ptr_integer, hdferr)
!  PRINT *,hdferr

  DO k=1,dims_adp_flags(3)
!     PRINT *,'MAXVAL(adp_flags(:,:,k)) ',k
!     PRINT *,MAXVAL(adp_flags(:,:,k))
     IF (MAXVAL(adp_flags(:,:,dust_index)) > 0) THEN
        PRINT *,MAXLOC(adp_flags(:,:,dust_index))
!        PRINT *,faot(MAXLOC(adp_flags(:,:,dust_index))),&
!             &qaot(MAXLOC(adp_flags(:,:,dust_index)))
     ENDIF
!     WRITE(char,'(B8.8)')MAXVAL(adp_flags(:,:,k))
!     PRINT *,char
  ENDDO

  CALL h5dclose_f (dset_adp_flags, hdferr)
!  PRINT *,hdferr
  CALL h5sclose_f (space_adp_flags, hdferr)
!  PRINT *,hdferr

!adp_qflags
  CALL h5dopen_f(file_mask, dataset_adp_qflags, dset_adp_qflags, hdferr)
!  PRINT *,hdferr
  CALL h5dget_space_f (dset_adp_qflags, space_adp_qflags, hdferr)
!  PRINT *,hdferr
  CALL h5sget_simple_extent_ndims_f(space_adp_qflags, rank_adp_qflags, hdferr)
!  PRINT *,hdferr,rank_adp_qflags
  CALL h5sget_simple_extent_dims_f(space_adp_qflags, dims_adp_qflags, maxdims, hdferr)
!  PRINT *,hdferr,dims_adp_qflags,maxdims
  ALLOCATE(adp_qflags(dims_adp_qflags(1),dims_adp_qflags(2),dims_adp_qflags(3)))
  ALLOCATE(adp_qflags_binary(dims_adp_qflags(1),dims_adp_qflags(2),dims_adp_qflags(3)))
!  PRINT *,SIZE(adp_qflags(:,1,1)),SIZE(adp_qflags(1,:,1)),SIZE(adp_qflags(1,1,:))
  
  f_ptr_integer = C_LOC(adp_qflags(1,1,1))
  CALL h5dread_f(dset_adp_qflags, H5T_NATIVE_INTEGER, f_ptr_integer, hdferr)
!  PRINT *,hdferr

!  DO k=1,dims_adp_qflags(3)
!     PRINT *,'MAXVAL(adp_qflags(:,:,k)) ',k
!     PRINT *,MAXVAL(adp_qflags(:,:,k))
!     WRITE(char,'(B8.8)')MAXVAL(adp_qflags(:,:,k))
!     PRINT *,char
!     PRINT *,'MAXLOC(adp_qflags(:,:,k))'
!     PRINT *,MAXLOC(adp_qflags(:,:,k)),k
!  ENDDO


  CALL h5dclose_f (dset_adp_qflags, hdferr)
!  PRINT *,hdferr
  CALL h5sclose_f (space_adp_qflags, hdferr)
!  PRINT *,hdferr

  CALL h5fclose_f (file_mask, hdferr)
!  PRINT *,hdferr

!  PRINT *,'all read and closed for file ',TRIM(filename_mask)

  stop

  DO i=1,dims_adp_qflags(1)
     DO j=1,dims_adp_qflags(2)  
        DO k=1,dims_adp_qflags(3)
           WRITE(adp_qflags_binary(i,j,k),'(B8.8)')adp_qflags(i,j,k)
           PRINT *,adp_qflags_binary(i,j,k)
        ENDDO
     ENDDO
  ENDDO

END PROGRAM hdf2bufr_rt

