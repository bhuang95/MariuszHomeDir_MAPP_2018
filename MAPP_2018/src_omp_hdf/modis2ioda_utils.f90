MODULE modis2ioda_utils

  USE netcdf
  USE datetime_mod
  USE module_constants, ONLY: MAXVARLEN

  IMPLICIT NONE

  INTEGER MAX_NC_NAME , MAX_VAR_DIMS , MAX_NAMES
  PARAMETER ( MAX_NC_NAME = MAXVARLEN , MAX_VAR_DIMS = 32 , MAX_NAMES = 50)

  CHARACTER(len=NF90_MAX_NAME) :: infile,outfile
  CHARACTER(len=10) :: validtimestr
  INTEGER :: validtimeint
  TYPE(datetime_type) :: validtime,datatime_modis
  CHARACTER(len=20) :: tendstr
  CHARACTER(len=MAX_NC_NAME) :: instrument="modis", satellite

  INTEGER :: nobs_in,nobs_out
  INTEGER, PARAMETER :: nvars=1,ichannel=4
  REAL, PARAMETER :: wavelength=550.e-9
  REAL, PARAMETER :: rmissing=-999.
  INTEGER, PARAMETER :: yyyy_modis=1993,mm_modis=1,dd_modis=1,hh_modis=0

CONTAINS 

  SUBROUTINE read_modis(infile,name,default_real_array,fail)

    CHARACTER(len=*), INTENT(in) :: infile,name
    REAL, ALLOCATABLE, INTENT(out) :: default_real_array(:,:)
    LOGICAL, INTENT(inout) :: fail

!locals

    INTEGER DFACC_READ, DFNT_INT32
    PARAMETER ( DFACC_READ = 1, DFNT_INT32 = 24 )
    INTEGER sfstart, sfselect, sfrdata, sfendacc, sfend
    INTEGER sffinfo , sfginfo
    INTEGER sd_id, sds_id, sds_index
    INTEGER status, n_attrs
    INTEGER n_datasets, n_file_attrs , index
    INTEGER rank, data_type
    INTEGER dim_sizes( MAX_VAR_DIMS )
    CHARACTER(len=MAX_NC_NAME) :: sds_name
    INTEGER start(3), edges(3), stride(3)
    INTEGER sfn2index
    INTEGER :: AllocateStatus, DeAllocateStatus
    INTEGER :: i

! data type 20 => integer 8 bits
! data type 22 => integer 16 bits
! data type 5 => real
! data type 6 => double

    INTEGER*1, DIMENSION(:,:), ALLOCATABLE :: short_array
    INTEGER*2, DIMENSION(:,:), ALLOCATABLE :: int_array
    REAL, DIMENSION(:,:), ALLOCATABLE :: real_array
    REAL(kind=8), DIMENSION(:,:), ALLOCATABLE :: double_array

    INTEGER sffattr, sfrattr, sfgainfo
    INTEGER      attr_index, n_values
    CHARACTER(len=MAXVARLEN) attr_name
    REAL(KIND=8) scale_factor
    REAL(KIND=8) add_offset



    CONTINUE

    sd_id = sfstart(infile, DFACC_READ)
    status = sffinfo( sd_id , n_datasets , n_file_attrs )

    IF (status /= 0) THEN
       PRINT *,'Unable to read '//TRIM(infile) 
       fail=.TRUE.
       RETURN
    ENDIF

    index = sfn2index(sd_id, TRIM(name))

    sds_id = sfselect(sd_id, index)
    status = sfginfo( sds_id , name , rank , dim_sizes , data_type , n_attrs )

    IF (status /= 0) THEN
       PRINT *,'Unable to read '//TRIM(name)//'from '//TRIM(infile)
       fail=.TRUE.
       RETURN
    ENDIF

    sds_name = name(1:LEN(name))

    DO i = 1 , rank
       start(i) = 0
       edges(i) = dim_sizes(i)
       stride(i) = 1
    END DO

    IF (ALLOCATED(default_real_array)) DEALLOCATE(default_real_array)
    ALLOCATE(default_real_array(dim_sizes(1),dim_sizes(2)))

    SELECT CASE(data_type)
    CASE(20)
       ALLOCATE(short_array(dim_sizes(1),dim_sizes(2)),STAT=AllocateStatus)
       status = sfrdata( sds_id, start, stride, edges, short_array )
       default_real_array=short_array
    CASE(22)
       ALLOCATE(int_array(dim_sizes(1),dim_sizes(2)),STAT=AllocateStatus)
       status = sfrdata( sds_id, start, stride, edges, int_array )
       default_real_array=int_array
    CASE(5)
       ALLOCATE(real_array(dim_sizes(1),dim_sizes(2)),STAT=AllocateStatus)
       status = sfrdata( sds_id, start, stride, edges, real_array )
       default_real_array=real_array
    CASE(6)
       ALLOCATE(double_array(dim_sizes(1),dim_sizes(2)),STAT=AllocateStatus)
       status = sfrdata( sds_id, start, stride, edges, double_array )
       default_real_array=double_array
    CASE default
       PRINT *,'Data type not allowed'
       STOP(2)
    END SELECT

    attr_index = sffattr(sds_id, 'scale_factor')
    status = sfrattr(sds_id, attr_index, scale_factor)

    attr_index = sffattr(sds_id, 'add_offset')
    status = sfrattr(sds_id, attr_index, add_offset)
    
    default_real_array=(default_real_array-add_offset)*scale_factor
    
    IF (ALLOCATED(short_array)) DEALLOCATE(short_array)
    IF (ALLOCATED(int_array)) DEALLOCATE(int_array)
    IF (ALLOCATED(real_array)) DEALLOCATE(real_array)
    IF (ALLOCATED(double_array)) DEALLOCATE(double_array)

    status = sfendacc(sds_id)
    status = sfend(sd_id)
  
  END SUBROUTINE read_modis

END MODULE modis2ioda_utils
