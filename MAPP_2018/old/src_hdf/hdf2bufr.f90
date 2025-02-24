PROGRAM hdf2bufr

!to convert VIIRS aot and dust/smoke mask hdf files to bufr
!updates for new VIIRS table NC008043 so 11 channels plus 550nm 

  USE HDF5
  USE ISO_C_BINDING

  IMPLICIT NONE

  REAL, PARAMETER :: aotmax=5.,unknown=-999.,sfc_fill=255,dsi_threshold=5.
  INTEGER, PARAMETER :: nchannels=12
  REAL, PARAMETER, DIMENSION(nchannels) :: channels=(/&
       &0.412e-6,0.445e-6,0.488e-6,0.555e-6,0.672e-6,0.746e-6,&
       &0.865e-6,1.240e-6,1.378e-6,1.610e-6,2.250e-6,0.550e-6/)

  CHARACTER(LEN=250):: filename_aot,filename_gmtco,filename_mask,&
       &filename_aot_out,filename_dust_mask_out,filename_smoke_mask_out

!aot
  CHARACTER(LEN=80) :: dataset_aot550 = &
       &"/AOD550"
  CHARACTER(LEN=80) :: dataset_aots = &
       &"/AOD_channel"
  CHARACTER(LEN=80) :: dataset_sfc = &
       &"/AerMdl"
  INTEGER, PARAMETER :: nqaotflags=1
  CHARACTER(LEN=80), DIMENSION(nqaotflags) :: dataset_qaot = &
       &(/ "/QCAll" /)
  INTEGER(HID_T)  :: file_aot, space_aot550, dset_aot550, space_qaot, dset_qaot,&
       &space_aots, dset_aots, space_sfc, dset_sfc

  INTEGER :: rank_aot550,rank_qaot,rank_aots,rank_sfc
  REAL, DIMENSION(:,:), ALLOCATABLE, TARGET :: aot550
  REAL, DIMENSION(:,:,:), ALLOCATABLE, TARGET :: aots
  INTEGER, DIMENSION(:,:), ALLOCATABLE, TARGET :: sfc
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE, TARGET :: qaot
  INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:)   :: dims_aot550,dims_qaot,dims_aots,dims_sfc


!gmtco
  CHARACTER(LEN=80) :: dataset_lat = &
       &"/All_Data/VIIRS-MOD-GEO-TC_All/Latitude"
  CHARACTER(LEN=80) :: dataset_lon = &
       &"/All_Data/VIIRS-MOD-GEO-TC_All/Longitude"
  CHARACTER(LEN=80) :: dataset_solza = &
       &"/All_Data/VIIRS-MOD-GEO-TC_All/SolarZenithAngle"
  CHARACTER(LEN=80) :: dataset_solaz = &
       &"/All_Data/VIIRS-MOD-GEO-TC_All/SolarAzimuthAngle"

  CHARACTER(LEN=80) :: dataset_datetime = &
       &"Data_Products/VIIRS-MOD-GEO-TC/VIIRS-MOD-GEO-TC_Aggr"

  CHARACTER(LEN=80) :: attribute_date="AggregateBeginningDate"
  CHARACTER(LEN=80) :: attribute_time="AggregateBeginningTime"

  INTEGER(HID_T)  :: file_gmtco, space_lat, dset_lat, space_lon, dset_lon,&
       &dset_solza, dset_solaz, space_solza, space_solaz

  INTEGER(HID_T)  :: dset_datetime, attr_date, attr_time, space_date, space_time

  INTEGER(SIZE_T) :: size_date

  INTEGER :: rank_lat,rank_lon,rank_solza,rank_solaz, rank_date
  REAL, DIMENSION(:,:), ALLOCATABLE, TARGET :: lat,lon,solza,solaz
!  CHARACTER(len=80), DIMENSION(:,:), ALLOCATABLE, TARGET :: date
  CHARACTER(len=80), DIMENSION(:,:), ALLOCATABLE, TARGET :: date
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: dims_lat,dims_lon, dims_solza, dims_solaz, dims_date

!adp - maskfile
  CHARACTER(LEN=80) :: dataset_adp_flags = &
       &"/VIIRS_ADP_IP/Data Fields/VIIRS ADP Flags"
  CHARACTER(LEN=80) :: dataset_adp_qflags = &
       &"/VIIRS_ADP_IP/Data Fields/VIIRS ADP Quality Flags"
  CHARACTER(LEN=80) :: dataset_latm = &
       &"/VIIRS_ADP_IP/Geolocation Fields/Latitude"
  CHARACTER(LEN=80) :: dataset_lonm = &
       &"/VIIRS_ADP_IP/Geolocation Fields/Longitude"
  CHARACTER(LEN=80) :: dataset_dsaindex = &
       &"/VIIRS_ADP_IP/Data Fields/Dust_Smoke Aerosol Index"



  INTEGER, PARAMETER :: dust_index=3, smoke_index=4

  CHARACTER(len=8) :: cdate
  CHARACTER(len=4) :: ctime
  CHARACTER(len=10) :: cidate

  INTEGER(HID_T)  :: file_mask, &
       &space_adp_flags,dset_adp_flags,space_adp_qflags,dset_adp_qflags,&
       &space_lonm,space_latm,dset_lonm,dset_latm,&
       &space_dsaindex,dset_dsaindex

  INTEGER :: rank_adp_flags,rank_adp_qflags,rank_lonm,rank_latm,rank_dsaindex
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE, TARGET :: adp_flags,adp_qflags
  REAL, DIMENSION(:,:), ALLOCATABLE, TARGET :: lonm,latm
  REAL, DIMENSION(:,:,:), ALLOCATABLE, TARGET :: dsaindex
  INTEGER(HSIZE_T), DIMENSION(1:3)   :: dims_adp_flags,dims_adp_qflags
  INTEGER(HSIZE_T), DIMENSION(1:3)   :: dims_dsaindex
  INTEGER(HSIZE_T), DIMENSION(1:2)   :: dims_latm,dims_lonm

  CHARACTER(len=8), DIMENSION(:,:,:), ALLOCATABLE :: adp_qflags_binary

  TYPE(C_PTR) :: f_ptr_real, f_ptr_integer, f_ptr_char
  INTEGER(HSIZE_T), DIMENSION(1:3) :: maxdims
  INTEGER :: hdferr
!  maxdims = (/H5S_UNLIMITED_f, H5S_UNLIMITED_f, H5S_UNLIMITED_f/)

!begin bufr
  CHARACTER(80):: &
       &hdstr_1='SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI',&
       &hdstr_2='CHWL',&
       &hdstr_3='AOPT',&
       &hdstr_4='RSST AOTQ'

  REAL(8) :: hdr_1(10,1),hdr_2(1,nchannels),hdr_3(1,nchannels),hdr_4(2,1)
  
  REAL, PARAMETER :: said=224
  CHARACTER(len=8), PARAMETER :: subset='NC008043'
  INTEGER :: unit_bufr=10,unit_table=20,&
       &unit_dust_mask=51,unit_smoke_mask=52
  INTEGER :: iret,idate
  INTEGER,DIMENSION(5):: idate5

  CHARACTER(len=8) :: char
  INTEGER :: i,j,k,iargc

  INTEGER, DIMENSION(2) :: ij
  LOGICAL :: logmask=.FALSE.

  IF (iargc() == 3) THEN
!     PRINT *,'Only processing AOT and GMTCO files'
     CALL getarg(1,filename_aot)
     CALL getarg(2,filename_gmtco)
     CALL getarg(3,filename_aot_out)
     
  ELSE IF (iargc() == 6) then
!     PRINT *,'Processing AOT, GMTCO, and ADP(mask) files'
     logmask=.TRUE.
     CALL getarg(1,filename_aot)
     CALL getarg(2,filename_gmtco)
     CALL getarg(3,filename_mask)
     CALL getarg(4,filename_aot_out)
     CALL getarg(5,filename_dust_mask_out)
     CALL getarg(6,filename_smoke_mask_out)
  ELSE
     PRINT *,'Incorrect number of input files - Stopping'
     STOP
  ENDIF


  i=INDEX(filename_aot,'Enterprise_d',back=.TRUE.)
  j=LEN('Enterprise_d')

  cdate=filename_aot(i+j:i+j+8)
  ctime=filename_aot(i+j+10:i+j+13)

  CALL h5open_f(hdferr)
  CALL h5fopen_f(filename_aot, H5F_ACC_RDONLY_F, file_aot, hdferr)

!aot550
  CALL h5dopen_f(file_aot, dataset_aot550, dset_aot550, hdferr)
  CALL h5dget_space_f (dset_aot550, space_aot550, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_aot550, rank_aot550, hdferr)
  ALLOCATE(dims_aot550(1:rank_aot550))
  CALL h5sget_simple_extent_dims_f(space_aot550, dims_aot550, maxdims(1:rank_aot550), hdferr)
  ALLOCATE(aot550(dims_aot550(1),dims_aot550(2)))

  aot550=unknown

  f_ptr_real = C_LOC(aot550(1,1))
  CALL h5dread_f(dset_aot550, H5T_NATIVE_REAL, f_ptr_real, hdferr)
  CALL h5dclose_f (dset_aot550, hdferr)
  CALL h5sclose_f (space_aot550, hdferr)

!sfc
  CALL h5dopen_f(file_aot, dataset_sfc, dset_sfc, hdferr)
  CALL h5dget_space_f (dset_sfc, space_sfc, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_sfc, rank_sfc, hdferr)
  ALLOCATE(dims_sfc(1:rank_sfc))
  CALL h5sget_simple_extent_dims_f(space_sfc, dims_sfc, maxdims(1:rank_sfc), hdferr)
  ALLOCATE(sfc(dims_sfc(1),dims_sfc(2)))

  sfc=unknown

  f_ptr_integer = C_LOC(sfc(1,1))
  CALL h5dread_f(dset_sfc, H5T_NATIVE_INTEGER, f_ptr_integer, hdferr)
  CALL h5dclose_f (dset_sfc, hdferr)
  CALL h5sclose_f (space_sfc, hdferr)

!aots
  CALL h5dopen_f(file_aot, dataset_aots, dset_aots, hdferr)
  CALL h5dget_space_f (dset_aots, space_aots, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_aots, rank_aots, hdferr)
  ALLOCATE(dims_aots(1:rank_aots))
  CALL h5sget_simple_extent_dims_f(space_aots, dims_aots, maxdims(1:rank_aots), hdferr)

  ALLOCATE(aots(dims_aots(1),dims_aots(2),dims_aots(3)))

  aots=unknown

  f_ptr_real = C_LOC(aots(1,1,1))
  CALL h5dread_f(dset_aots, H5T_NATIVE_REAL, f_ptr_real, hdferr)
  CALL h5dclose_f (dset_aots, hdferr)
  CALL h5sclose_f (space_aots, hdferr)

!qflags

  DO k=1,nqaotflags
     CALL h5dopen_f(file_aot, dataset_qaot(k), dset_qaot, hdferr)
     CALL h5dget_space_f (dset_qaot, space_qaot, hdferr)
     
     IF (NOT(ALLOCATED(qaot))) THEN
        CALL h5sget_simple_extent_ndims_f(space_qaot, rank_qaot, hdferr)
        ALLOCATE(dims_qaot(1:rank_qaot))
        CALL h5sget_simple_extent_dims_f(space_qaot, dims_qaot, maxdims(1:rank_qaot), hdferr)
        ALLOCATE(qaot(dims_qaot(1),dims_qaot(2),nqaotflags))
        qaot=unknown
     ENDIF
     
     f_ptr_integer = C_LOC(qaot(1,1,k))
     CALL h5dread_f(dset_qaot, H5T_NATIVE_INTEGER, f_ptr_integer, hdferr)
     CALL h5dclose_f (dset_qaot, hdferr)
     CALL h5sclose_f (space_qaot, hdferr)
     
  ENDDO

  CALL h5fclose_f (file_aot, hdferr)

!begin gmtco

  CALL h5open_f(hdferr)
  CALL h5fopen_f(filename_gmtco, H5F_ACC_RDONLY_F, file_gmtco, hdferr)

!lat
  CALL h5dopen_f(file_gmtco, dataset_lat, dset_lat, hdferr)
  CALL h5dget_space_f (dset_lat, space_lat, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_lat, rank_lat, hdferr)
  CALL h5sget_simple_extent_dims_f(space_lat, dims_lat, maxdims(1:rank_lat), hdferr)
  ALLOCATE(lat(dims_lat(1),dims_lat(2)))

  f_ptr_real = C_LOC(lat(1,1))
  CALL h5dread_f(dset_lat, H5T_NATIVE_REAL, f_ptr_real, hdferr)
  CALL h5dclose_f (dset_lat, hdferr)
  CALL h5sclose_f (space_lat, hdferr)

!lon
  CALL h5dopen_f(file_gmtco, dataset_lon, dset_lon, hdferr)
  CALL h5dget_space_f (dset_lon, space_lon, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_lon, rank_lon, hdferr)
  CALL h5sget_simple_extent_dims_f(space_lon, dims_lon, maxdims(1:rank_lon), hdferr)
  ALLOCATE(lon(dims_lon(1),dims_lon(2)))
  
  f_ptr_real = C_LOC(lon(1,1))
  CALL h5dread_f(dset_lon, H5T_NATIVE_REAL, f_ptr_real, hdferr)
  CALL h5dclose_f (dset_lon, hdferr)
  CALL h5sclose_f (space_lon, hdferr)

!  PRINT *,MINVAL(lon),MAXVAL(lon),MINVAL(lat),MAXVAL(lat)

!solza
  CALL h5dopen_f(file_gmtco, dataset_solza, dset_solza, hdferr)
  CALL h5dget_space_f (dset_solza, space_solza, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_solza, rank_solza, hdferr)
  CALL h5sget_simple_extent_dims_f(space_solza, dims_solza, maxdims(1:rank_solza), hdferr)
  ALLOCATE(solza(dims_solza(1),dims_solza(2)))
  
  f_ptr_real = C_LOC(solza(1,1))
  CALL h5dread_f(dset_solza, H5T_NATIVE_REAL, f_ptr_real, hdferr)
  CALL h5dclose_f (dset_solza, hdferr)
  CALL h5sclose_f (space_solza, hdferr)


!solaz
  CALL h5dopen_f(file_gmtco, dataset_solaz, dset_solaz, hdferr)
  CALL h5dget_space_f (dset_solaz, space_solaz, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_solaz, rank_solaz, hdferr)
  CALL h5sget_simple_extent_dims_f(space_solaz, dims_solaz, maxdims(1:rank_solaz), hdferr)
  ALLOCATE(solaz(dims_solaz(1),dims_solaz(2)))
  
  f_ptr_real = C_LOC(solaz(1,1))
  CALL h5dread_f(dset_solaz, H5T_NATIVE_REAL, f_ptr_real, hdferr)
  CALL h5dclose_f (dset_solaz, hdferr)
  CALL h5sclose_f (space_solaz, hdferr)

!  PRINT *,MINVAL(solza),MAXVAL(solza),MINVAL(solaz),MAXVAL(solaz)
!  PRINT *,TRIM(cdate),' ',TRIM(ctime)

!reading attributes to annoying - forget about it

!  CALL h5dopen_f(file_gmtco, dataset_datetime, dset_datetime, hdferr)
!  PRINT *,hdferr,'1'
!  CALL h5aopen_f(dset_datetime, attribute_date, attr_date, hdferr)
!  PRINT *,hdferr,'2'
!  CALL h5aget_space_f(attr_date, space_date, hdferr)
!  PRINT *,hdferr,'3'
!  CALL h5sget_simple_extent_ndims_f(space_date, rank_date, hdferr)
!  PRINT *,hdferr,rank_date
!  CALL h5sget_simple_extent_dims_f(space_date, dims_date, maxdims(1:rank_date), hdferr)
!  PRINT *,hdferr,dims_date
!  ALLOCATE(date(1:dims_date(1),1:dims_date(2)))
!  CALL H5Aget_type_f(attr_date, file_gmtco, hdferr)
!  PRINT *,hdferr
!  CALL H5Tget_size_f(file_gmtco, size_date, hdferr)
!  PRINT *,hdferr,size_date
!
!  f_ptr_char=C_LOC(date(1,1)(1:1))
!!  CALL h5aread_f(attr_date,H5T_NATIVE_CHARACTER, f_ptr_char, hdferr)
!  CALL h5aread_f(attr_date,H5T_FORTRAN_S1, f_ptr_char, hdferr)
!  PRINT *,hdferr,'4',LEN(date),date
!  CALL h5aclose_f(attr_date, hdferr)
!  PRINT *,hdferr
!  CALL h5dclose_f(dset_datetime, hdferr)
!  PRINT *,hdferr
!  CALL h5sclose_f(space_date, hdferr)
!  PRINT *,hdferr
  
  CALL h5fclose_f (file_gmtco, hdferr)
!  PRINT *,hdferr
  
!end gmtco

  IF (.NOT. logmask) GOTO 111
 
  CALL h5open_f(hdferr)
  CALL h5fopen_f(filename_mask, H5F_ACC_RDONLY_F, file_mask, hdferr)

!adp_flags

  CALL h5dopen_f(file_mask, dataset_adp_flags, dset_adp_flags, hdferr)
  CALL h5dget_space_f (dset_adp_flags, space_adp_flags, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_adp_flags, rank_adp_flags, hdferr)
  CALL h5sget_simple_extent_dims_f(space_adp_flags, dims_adp_flags, maxdims, hdferr)
  ALLOCATE(adp_flags(dims_adp_flags(1),dims_adp_flags(2),dims_adp_flags(3)))

  adp_flags=unknown

  f_ptr_integer = C_LOC(adp_flags(1,1,1))
  CALL h5dread_f(dset_adp_flags, H5T_NATIVE_INTEGER, f_ptr_integer, hdferr)

  CALL h5dclose_f (dset_adp_flags, hdferr)
  CALL h5sclose_f (space_adp_flags, hdferr)

!adp_qflags
  CALL h5dopen_f(file_mask, dataset_adp_qflags, dset_adp_qflags, hdferr)
  CALL h5dget_space_f (dset_adp_qflags, space_adp_qflags, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_adp_qflags, rank_adp_qflags, hdferr)
  CALL h5sget_simple_extent_dims_f(space_adp_qflags, dims_adp_qflags, maxdims, hdferr)
  ALLOCATE(adp_qflags(dims_adp_qflags(1),dims_adp_qflags(2),dims_adp_qflags(3)))
  ALLOCATE(adp_qflags_binary(dims_adp_qflags(1),dims_adp_qflags(2),dims_adp_qflags(3)))
  
  f_ptr_integer = C_LOC(adp_qflags(1,1,1))
  CALL h5dread_f(dset_adp_qflags, H5T_NATIVE_INTEGER, f_ptr_integer, hdferr)

  CALL h5dclose_f (dset_adp_qflags, hdferr)
  CALL h5sclose_f (space_adp_qflags, hdferr)

!latm
  CALL h5dopen_f(file_mask, dataset_latm, dset_latm, hdferr)
  CALL h5dget_space_f (dset_latm, space_latm, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_latm, rank_latm, hdferr)
  CALL h5sget_simple_extent_dims_f(space_latm, dims_latm, maxdims(1:rank_latm), hdferr)
  ALLOCATE(latm(dims_latm(1),dims_latm(2)))

  f_ptr_real = C_LOC(latm(1,1))
  CALL h5dread_f(dset_latm, H5T_NATIVE_REAL, f_ptr_real, hdferr)

!  PRINT *,'latm(1890,318),latm(1890,319)'
!  PRINT *,latm(1890,318),latm(1890,319)

  CALL h5dclose_f (dset_latm, hdferr)
  CALL h5sclose_f (space_latm, hdferr)

!lonm
  CALL h5dopen_f(file_mask, dataset_lonm, dset_lonm, hdferr)
  CALL h5dget_space_f (dset_lonm, space_lonm, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_lonm, rank_lonm, hdferr)
  CALL h5sget_simple_extent_dims_f(space_lonm, dims_lonm, maxdims(1:rank_lonm), hdferr)
  ALLOCATE(lonm(dims_lonm(1),dims_lonm(2)))
  
  f_ptr_real = C_LOC(lonm(1,1))
  CALL h5dread_f(dset_lonm, H5T_NATIVE_REAL, f_ptr_real, hdferr)
  
  CALL h5dclose_f (dset_lonm, hdferr)
  CALL h5sclose_f (space_lonm, hdferr)
  
!dsaindex
  CALL h5dopen_f(file_mask, dataset_dsaindex, dset_dsaindex, hdferr)
  CALL h5dget_space_f (dset_dsaindex, space_dsaindex, hdferr)
  CALL h5sget_simple_extent_ndims_f(space_dsaindex, rank_dsaindex, hdferr)
  CALL h5sget_simple_extent_dims_f(space_dsaindex, dims_dsaindex, maxdims(1:rank_dsaindex), hdferr)
  ALLOCATE(dsaindex(dims_dsaindex(1),dims_dsaindex(2),dims_dsaindex(3)))

  f_ptr_real = C_LOC(dsaindex(1,1,1))
  CALL h5dread_f(dset_dsaindex, H5T_NATIVE_REAL, f_ptr_real, hdferr)

!  PRINT *,'dsaindex(555,568,1),dsaindex(556,569,1)'
!  PRINT *,dsaindex(555,568,1),dsaindex(556,569,1)

!  PRINT *,MAXVAL(dsaindex)

  CALL h5dclose_f (dset_dsaindex, hdferr)
  CALL h5sclose_f (space_dsaindex, hdferr)

  CALL h5fclose_f (file_mask, hdferr)

!process input data
111 CONTINUE

  WHERE (aots > aotmax) 
     aots=unknown
  END WHERE

  DO i=1,dims_aots(3)
     WHERE(aots(:,:,i) < 0)
        qaot(:,:,1)=unknown
     END WHERE
  ENDDO
  
  WHERE (aot550 > aotmax .OR. aot550 < 0. ) 
     aot550=unknown
     qaot(:,:,1)=unknown
  END WHERE

  WHERE(sfc==sfc_fill)
     qaot(:,:,1)=unknown
  END WHERE

!write viirs bufr  

  READ(cdate,'(i4,2i2)') idate5(1),idate5(2),idate5(3)
  READ(ctime,'(2i2)')idate5(4),idate5(5)
  cidate=TRIM(cdate)//TRIM(ctime)
  READ(cidate,'(i10)')idate

  OPEN(unit_table,file='AOT_BUFR_Table.txt')
  OPEN(unit_bufr,file=TRIM(filename_aot_out),action='write' &
       ,form='unformatted')
  CALL datelen(10)

  CALL openbf(unit_bufr,'OUT',unit_table)
  CALL openmb(unit_bufr,subset,idate)

  hdr_1(1,1)=said
  hdr_1(4,1)=idate5(1)
  hdr_1(5,1)=idate5(2)
  hdr_1(6,1)=idate5(3)
  hdr_1(7,1)=idate5(4)
  hdr_1(8,1)=idate5(5)

  DO i=1,dims_lat(1)
     DO j=1,dims_lat(2)
        IF (qaot(i,j,1) > 0) THEN

           IF (qaot(i,j,1) == 3) THEN
              qaot(i,j,1)=0
           ELSE
              qaot(i,j,1)=1
           ENDIF

           hdr_1(2,1)=lat(i,j)
           hdr_1(3,1)=lon(i,j)
           hdr_1(9,1)=solza(i,j)
           IF (solaz(i,j) < 0.) THEN
              hdr_1(10,1)=solaz(i,j)+360.
           ELSE
              hdr_1(10,1)=solaz(i,j)
           ENDIF
           CALL ufbint(unit_bufr,hdr_1,10,1,iret,hdstr_1)
           hdr_2(1,:)=channels
           CALL ufbrep(unit_bufr,hdr_2,1,nchannels,iret,hdstr_2)
           hdr_3(1,1:11)=aots(i,j,1:11)
           hdr_3(1,12)=aot550(i,j)
           CALL ufbrep(unit_bufr,hdr_3,1,nchannels,iret,hdstr_3)
           hdr_4(1,1)=sfc(i,j)
           hdr_4(2,1)=qaot(i,j,1)

           CALL ufbint(unit_bufr,hdr_4,2,1,iret,hdstr_4)
           CALL writsb(unit_bufr)
       ENDIF
     ENDDO
  ENDDO

  CALL closmg(unit_bufr)
  CALL closbf(unit_bufr)
  
  CLOSE(unit_table)


  IF (.NOT. logmask)  STOP

  OPEN(unit=unit_dust_mask,file=filename_dust_mask_out,&
       &position='APPEND',form='formatted')
  OPEN(unit=unit_smoke_mask,file=filename_smoke_mask_out,&
       &position='APPEND',form='formatted')

!process maskfile

!  adp_flags(:,:,dust_index)
!  adp_flags(:,:,smoke_index)  

  DO i=1,dims_adp_qflags(1)
     DO j=1,dims_adp_qflags(2)  

        DO k=1,dims_adp_qflags(3)
           WRITE(adp_qflags_binary(i,j,k),'(B8.8)')adp_qflags(i,j,k)
        ENDDO
        
!correct sunglint over land
        IF (adp_qflags_binary(i,j,3)(6:6) == '1') &
             &adp_qflags_binary(i,j,3)(7:7) = '0'
           
!eliminate sunglint over water
        IF (adp_qflags_binary(i,j,3)(7:7) == '1') CYCLE
        
!eliminate volcanic ash, cloud, unclear, snow/ice
        IF (&
             &adp_flags(i,j,1) == 1 .OR. &
             &adp_flags(i,j,2) == 1 .OR. &
             &adp_flags(i,j,5) == 1 .OR. &
             &adp_flags(i,j,6) == 1 ) CYCLE
        
        IF ( qaot(i,j,1) /= 0 ) CYCLE
        
!quality of detection           
!bits in quality flags for dust/smoke are from least significant (from right) so 
!for dust   3:4 -> 5:6 in the string (from left)
!for smoke  5:6 -> 3:4 in the string (from left)
        

!eliminate low quality dust
        
        IF (adp_qflags_binary(i,j,1)(3:4) /= '00') THEN
!              PRINT *,adp_flags(i,j,dust_index),' dust ',i,j
!write out dust mask file
!eliminate IR and vis
!           PRINT *,adp_qflags_binary(i,j,5)(1:2), 'dust'
           IF (adp_qflags_binary(i,j,5)(1:2) /= '10') THEN 
              WRITE(unit_dust_mask,'(2f13.5,a15,i3,f13.5)')&
                   &latm(i,j),lonm(i,j),cidate,&
                   &adp_flags(i,j,dust_index),&
                   &MIN(dsaindex(i,j,1),dsi_threshold)/dsi_threshold*&
                   &adp_flags(i,j,dust_index)
           ENDIF

        ENDIF
        
!eliminate low quality smoke

        IF (adp_qflags_binary(i,j,1)(5:6) /= '00') THEN
!              PRINT *,adp_flags(i,j,smoke_index),' smoke ',i,j
!write out smoke mask file
!eliminate IR and vis
!           PRINT *,adp_qflags_binary(i,j,5)(3:4), 'smoke'
           IF (adp_qflags_binary(i,j,5)(3:4) /= '10') THEN 
              WRITE(unit_smoke_mask,'(2f13.5,a15,i3,f13.5)')&
                   &latm(i,j),lonm(i,j),cidate,&
                   &adp_flags(i,j,smoke_index),&
                   &MIN(dsaindex(i,j,1),dsi_threshold)/dsi_threshold*&
                   &adp_flags(i,j,smoke_index)
           ENDIF
        ENDIF
        
     ENDDO
  ENDDO
  
END PROGRAM hdf2bufr

