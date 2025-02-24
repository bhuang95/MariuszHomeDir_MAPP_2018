PROGRAM aodnnr_multichannel_correlation_4_hl
!  to calculate correlation matrix for nasa's nnr multichannel aods using H-L method
!  MP, Sept 2018

  USE module_misc, ONLY: max_name_length
  USE module_aod_nnr, ONLY: get_aod_nnr,aod_nnr
  USE module_fv3, ONLY: get_aod_fv3,aod_fv3
  USE module_obs_thinning, ONLY: obs_thinning
  USE module_fv3_interp, ONLY: aod_m2o, fv3_interp, alloc_m2o_record
  USE module_interp_output, ONLY: aod_output_ncdf

  IMPLICIT NONE

  TYPE(aod_nnr), ALLOCATABLE, DIMENSION(:) :: aod_nnr_record, aod_nnr_thin_record
  TYPE(aod_fv3) :: aod_fv3_record
  TYPE(aod_m2o), ALLOCATABLE, DIMENSION(:) :: aod_m2o_record

  INTEGER :: nobs,nobs_thin,nchan_nnr
  LOGICAL :: interp

  INTEGER :: i

  CONTINUE

  CALL get_aod_nnr(aod_nnr_record)
  nobs=SIZE(aod_nnr_record)
  nchan_nnr=SIZE(aod_nnr_record(1)%channels(:))

  CALL get_aod_fv3(aod_fv3_record)

  CALL obs_thinning(aod_fv3_record%fcstdate,aod_nnr_record,nobs,aod_nnr_thin_record,nobs_thin)

! need to allocate beforehand since allocation of m2o_record in fv3_interp
! causes errors
  CALL alloc_m2o_record(aod_m2o_record,nobs_thin,nchan_nnr)

  CALL fv3_interp(aod_fv3_record,aod_nnr_thin_record,aod_m2o_record,nobs_thin,interp)  

  CALL aod_output_ncdf(aod_fv3_record%analdate,aod_fv3_record%fcstdate,&
       &aod_nnr_thin_record,aod_m2o_record,nobs_thin)

END PROGRAM aodnnr_multichannel_correlation_4_hl




