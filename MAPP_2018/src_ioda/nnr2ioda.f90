PROGRAM  nnr2ioda
!  to format nnr for ioda
!  MZP, March 2020

  USE module_aod_nnr, ONLY: aod_nnr, get_aod_nnr
  USE module_obs_thinning_nnr, ONLY: obs_thinning_nnr
  USE module_write2ioda, ONLY: write_nnr2ioda

  IMPLICIT NONE

  TYPE(aod_nnr), ALLOCATABLE, DIMENSION(:) :: aod_nnr_record, aod_nnr_thin_record

  CHARACTER(len=10) :: center_date_time
  INTEGER :: nobs,nobs_thin,nchan_nnr

  INTEGER :: i

  CONTINUE

  CALL get_aod_nnr(aod_nnr_record)
  nobs=SIZE(aod_nnr_record)
  nchan_nnr=SIZE(aod_nnr_record(1)%channels(:))

  IF (ALLOCATED(aod_nnr_thin_record)) DEALLOCATE(aod_nnr_thin_record)
  CALL obs_thinning_nnr(aod_nnr_record,nobs,aod_nnr_thin_record,&
       &nobs_thin,center_date_time)

  CALL write_nnr2ioda(aod_nnr_thin_record,nobs_thin,center_date_time)

END PROGRAM nnr2ioda




