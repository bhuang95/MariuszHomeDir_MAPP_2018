MODULE module_fv3_interp

!interpolate fv3 AOD to observations
!based on fv3pop, Ning Wang, March 2007

  USE datetime_mod
  USE timedelta_mod
  USE slint

  USE module_misc, ONLY: max_name_length,small_wavelength_difference,d2r,&
       &unit_namelist
  USE module_aod_nnr, ONLY: aod_nnr
  USE module_fv3, ONLY: aod_fv3

  IMPLICIT NONE

  TYPE aod_m2o
     TYPE(timedelta_type) :: dt !difference obstime - model time
     REAL, ALLOCATABLE :: values(:) ! obs for all channels
  END TYPE aod_m2o

  PRIVATE
  PUBLIC :: aod_m2o,fv3_interp,alloc_m2o_record

CONTAINS

  SUBROUTINE alloc_m2o_record(aod_m2o_record,nobs,nchan_nnr)

    TYPE(aod_m2o), ALLOCATABLE, DIMENSION(:), INTENT(out) :: aod_m2o_record
    INTEGER, INTENT(in) :: nobs,nchan_nnr

    INTEGER :: i
    
    ALLOCATE(aod_m2o_record(nobs))

    DO i=1,nobs
       ALLOCATE(aod_m2o_record(i)%values(nchan_nnr))
       aod_m2o_record(i)%values=0.
    ENDDO

  END SUBROUTINE alloc_m2o_record

  SUBROUTINE fv3_interp(aod_fv3_record,aod_nnr_record,aod_m2o_record,nobs,interp)

    TYPE(aod_fv3), INTENT(in) :: aod_fv3_record
    TYPE(aod_nnr), DIMENSION(nobs), INTENT(in) :: aod_nnr_record
    INTEGER, INTENT(in) :: nobs
    TYPE(aod_m2o), DIMENSION(nobs), INTENT(inout) :: aod_m2o_record
    LOGICAL, INTENT(out) :: interp

    CHARACTER(len = max_name_length) :: method

    INTEGER :: nchan_nnr,nchan_fv3,nxy
    INTEGER, ALLOCATABLE, DIMENSION(:) :: indx
    REAL, ALLOCATABLE, DIMENSION(:,:) :: ll_src,ll_tgt
    REAL, ALLOCATABLE, DIMENSION(:) :: src_data,tgt_data
    INTEGER :: i,j,k

    INTEGER :: Open_Status,Read_Status
    INTEGER :: stderr

    NAMELIST /record_model_interp/ method

    CONTINUE

    stderr = 0

    OPEN(unit=unit_namelist, file = "namelist.modis_nnr_correlation_4hl", &
         &status="old",action = "read",iostat=open_status)
    IF (open_status /= 0) THEN
       WRITE(stderr,*) "error: there is no namelist file (namelist.modis_nnr_correlation)"
       STOP
    END IF

    WRITE(stderr,*) 'Reading namelist.modis_nnr_correlation_4hl record_model_interp'
    READ (unit_namelist, NML=record_model_interp, IOSTAT=Read_Status)
    CLOSE(unit_namelist)

    IF (TRIM(method) == "bilinear") THEN
       interp=.TRUE.
    ELSEIF (TRIM(method) == "none") THEN
       interp=.FALSE.
       PRINT *,"Not interpolating"
       RETURN
    ELSE
       interp=.FALSE.
       PRINT *,"Only bilenear interpolation implemented - Stopping"
       STOP
    ENDIF

    nchan_nnr=SIZE(aod_nnr_record(1)%channels)

!    PRINT *,aod_nnr_record(nobs)%channels
!    PRINT *,aod_nnr_record(nobs)%lat,aod_nnr_record(nobs)%lon
!    PRINT *,aod_nnr_record(nobs)%values
!    PRINT *,aod_nnr_record(nobs)%satellite
!    PRINT *,aod_nnr_record(nobs)%obstype
!    PRINT *,aod_nnr_record(nobs)%obsdate%isoformat()

    nchan_fv3=SIZE(aod_fv3_record%channels(:))

!    PRINT *,MINVAL(aod_fv3_record%lons),MAXVAL(aod_fv3_record%lons)
!    PRINT *,MINVAL(aod_fv3_record%lats),MAXVAL(aod_fv3_record%lats)
!    PRINT *,MINVAL(aod_fv3_record%values),MAXVAL(aod_fv3_record%values)
!    PRINT *,aod_fv3_record%channels
!    PRINT *,aod_nnr_record(1)%channels
!    PRINT *,aod_fv3_record%channels

    ALLOCATE(indx(nchan_nnr))
    
    indx=0
  
    DO i=1,nchan_nnr
       DO j=1,nchan_fv3
          IF (ABS(aod_nnr_record(1)%channels(i) - aod_fv3_record%channels(j)) < &
               &small_wavelength_difference) THEN
             indx(i)=j
             EXIT
          ENDIF
       ENDDO
    ENDDO

!    PRINT *,aod_fv3_record%channels(indx(:))

    IF (ANY(indx == 0)) THEN
       PRINT *,'Obs and Model channels incompatible - Stopping'
       PRINT *,'Obs channels = ',aod_nnr_record(nobs)%channels
       PRINT *,'Model channels = ',aod_fv3_record%channels
       STOP
    ELSE
       PRINT *,'Obs and Model channels compatible - proceeding with interpolation'
    ENDIF

    nxy=SIZE(aod_fv3_record%lats)

    ALLOCATE(ll_src(nxy,2),ll_tgt(nobs,2))

    ll_src(:,1)=aod_fv3_record%lats(:)*d2r
    ll_src(:,2)=aod_fv3_record%lons(:)*d2r

    ll_tgt(:,1)=aod_nnr_record(:)%lat*d2r
    ll_tgt(:,2)=aod_nnr_record(:)%lon*d2r

    CALL slint_init(ll_src, nxy, ll_tgt, nobs)

    ALLOCATE(src_data(nxy),tgt_data(nobs))

    DO k=1,nchan_nnr

       src_data(:)=aod_fv3_record%values(indx(k),:)
       tgt_data(:)=aod_nnr_record(:)%values(k)

       CALL bilinear_interp(src_data, tgt_data)

       DO i=1,nobs       

          aod_m2o_record(i)%dt=aod_nnr_record(i)%obsdate-&
               &aod_fv3_record%fcstdate
          aod_m2o_record(i)%values(k)=tgt_data(i)

       ENDDO

    ENDDO

    DEALLOCATE(indx,ll_src,ll_tgt,src_data,tgt_data)

  END SUBROUTINE fv3_interp

END MODULE module_fv3_interp

