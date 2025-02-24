MODULE module_write2ioda
  
  USE,INTRINSIC :: iso_fortran_env
  
  USE :: ioda_variable_mod
  USE :: ioda_has_variables_mod
  USE :: ioda_vecstring_mod
  USE :: ioda_group_mod
  USE :: ioda_engines_mod
  USE :: ioda_variable_creation_parameters_mod
  USE :: ioda_dimensions_mod

  USE module_aod_nnr, ONLY: aod_nnr,modis_params,modis_nnr_errors
  USE datetime_mod
  USE timedelta_mod
  USE netcdf
  USE module_constants, ONLY: max_varname_length,max_string_length
  USE module_utils, ONLY: indexx, replace_text

CONTAINS
  
  SUBROUTINE write_nnr2ioda(aod_nnr_record,nobs,center_date_time)

    IMPLICIT NONE

    INTEGER(int32) :: nobs
    INTEGER(int32), PARAMETER  :: ndims_max=4,nchannels_max=100
    REAL(real32) :: output_channels_nm(nchannels_max)
    REAL(real32) :: diffmax=1.
    INTEGER(int32) :: isfc(nobs), ikeep(nchannels_max)
    INTEGER(int32), ALLOCATABLE :: channel_ids(:)

    TYPE(aod_nnr), DIMENSION(nobs), INTENT(in) :: aod_nnr_record
    CHARACTER(len=10), INTENT(in) :: center_date_time

    CHARACTER(len=max_varname_length) :: satellite,satellite_id,&
         &sensor_id,varname

    INTEGER(int32) :: i,j,k,nchannels,nchannels_out,ndims

    INTEGER(int32) :: unit_namelist=101,stderr,Open_Status,Read_Status
    
    REAL(real32), ALLOCATABLE :: wavenumbers(:), frequencies(:)
    REAL(real32), ALLOCATABLE :: bias(:,:),uncertainty(:,:)
    REAL(real32), ALLOCATABLE :: values(:)

    CHARACTER(len=max_string_length) :: output_dir,output_file,fnameout

    TYPE(datetime_type) :: validdate
    TYPE(timedelta_type) :: dt(nobs)
    REAL(real32) :: qc(nobs),tdiff(nobs)
    INTEGER(int32) :: yyyy, mm, dd, hh

    TYPE(ioda_group) :: g
    TYPE(ioda_has_variables) :: gvars
    TYPE(ioda_variable) :: ivar,dvar,zvar,ivar2
    TYPE(ioda_vecstring) :: vlist
    TYPE(ioda_dimensions) :: vdims
    INTEGER(int64) :: nd,ns
    LOGICAL :: rlog
    INTEGER(int64) :: cdims(ndims_max)

    NAMELIST /record_output/ output_channels_nm,output_dir,output_file

    CONTINUE

    PRINT *,'In write_nnr2ioda'

    nchannels=SIZE(aod_nnr_record(1)%channels(:))

    output_channels_nm=0.
    ikeep=0

    OPEN(unit=unit_namelist, file = "nnr2ioda.nl", &
         &status="old",action = "READ",iostat=open_status)
    IF (open_status /= 0) THEN
       PRINT *,'error: there is no NAMELIST file (nnr2ioda.nl)'
       STOP
    END IF
    PRINT *, 'Reading nnr2ioda.nl record_output'
    READ (unit_namelist, NML=record_output, IOSTAT=Read_Status)
    CLOSE(unit_namelist)

    fnameout=TRIM(output_dir)//'/'//TRIM(output_file)
    nchannels_out=COUNT(output_channels_nm > 0.)

    CALL modis_params(aod_nnr_record(1)%satellite,&
         &aod_nnr_record(1)%channels,&
         &satellite_id,sensor_id,channel_ids,wavenumbers,frequencies)

    ALLOCATE(bias(nobs,nchannels),uncertainty(nobs,nchannels))

    CALL modis_nnr_errors(aod_nnr_record,nobs,bias,uncertainty)

    DO i=1,nchannels_out
       DO j=1,nchannels
          IF (ABS(aod_nnr_record(1)%channels(j)-output_channels_nm(i)) &
               & < diffmax) THEN
             ikeep(j)=1
             EXIT
          ELSE IF(j==nchannels) THEN
             PRINT *,'Channel not available ',output_channels_nm(i)
             STOP 1
          ENDIF
       ENDDO
    ENDDO

    qc=0.

    SELECT CASE(aod_nnr_record(1)%obstype)
    CASE ('ocean')
       isfc=0
    CASE ('land')
       isfc=1 !assume dark land as for viirs 
    CASE ('deep')
       isfc=2 !specific for deep blue similar to bright land for viirs
    END SELECT

    READ(center_date_time(1:4), '(i4)' )  yyyy
    READ(center_date_time(5:6), '(i2)' )  mm
    READ(center_date_time(7:8), '(i2)' )  dd
    READ(center_date_time(9:10), '(i2)' )  hh

    validdate=create_datetime(year=yyyy,month=mm,day=dd,hour=hh)

    DO i=1,nobs
       dt(i)=aod_nnr_record(i)%obsdate-validdate
       tdiff(i)=dt(i)%total_seconds()
    ENDDO

    ALLOCATE(values(nobs*nchannels_out))

    k=0
    DO j=1,nobs             
       DO i=1,nchannels
          IF (ikeep(i) == 1) THEN
             k=k+1
             values(k)=aod_nnr_record(j)%values(i)
          ENDIF
       ENDDO
    ENDDO

    cdims=0

    fnameout='abc.hdf5'
    g=ioda_engines_construct_from_command_line(TRIM(fnameout))
    gvars=g%has_variables()
    cdims(1)=nobs
    cdims(2)=nchannels_out
    nd=2
    ns=cdims(1)*cdims(2)
    varname='aod'
    ivar = gvars%create_float(TRIM(varname),nd,cdims)
    rlog = ivar%write(ns,values)

    DEALLOCATE(bias,uncertainty)

  END SUBROUTINE write_nnr2ioda

END MODULE module_write2ioda
