PROGRAM get_crtm_vawelengths
!get wavelenghts for modis and viirs.
!Mariusz Pagowski Oct, 2016
!Mariusz Pagowski Sept, 2018

  USE kinds,ONLY: r_kind,i_kind,r_single
  USE crtm_spccoeff, ONLY: sc
  USE crtm_module, ONLY: crtm_init,crtm_channelinfo_type
  USE module_wrf_gocart, ONLY: n_aerosols_crtm
  USE module_utils, ONLY: upper2lower

  IMPLICIT NONE

  INTEGER(i_kind), PARAMETER :: nchan_viirs=11, nchan_modis=20, nchan_abi=6

  TYPE(crtm_channelinfo_type),SAVE,DIMENSION(1)  :: channelinfo

  CHARACTER(20),DIMENSION(1) :: isis 
  CHARACTER(20) :: satellite
  INTEGER(i_kind) :: sensorindex  

  INTEGER(i_kind) :: nchanl
  INTEGER(i_kind) :: i,j,k,it,ii
  INTEGER(i_kind) :: ier,error_status


  INTEGER(i_kind) :: ncunit, binunit=112,status, ierr, ndim
  CHARACTER(len=250) :: flnm_in,flnm_out
  LOGICAL :: Load_AerosolCoeff=.TRUE.,Load_CloudCoeff=.FALSE.

  INTEGER :: iargc

  IF (iargc() < 1) THEN
     WRITE(6,*)'Need satellite name as input'
     STOP
  ENDIF

  CALL getarg(1,satellite)

  IF (INDEX(upper2lower(satellite),'viirs') > 0) THEN
     isis='v.viirs-m_npp'
     nchanl=nchan_viirs
  ELSEIF (INDEX(upper2lower(satellite),'modis') > 0) THEN
     isis='v.modis_aqua'
     nchanl=nchan_modis

  ELSEIF (INDEX(upper2lower(satellite),'abi') > 0) THEN
     isis='v.abi_gr'
     nchanl=nchan_abi
  ELSE
     PRINT *,'Unknown satellite - Stopping'
     STOP
  ENDIF

  error_status = crtm_init(isis,channelinfo,&
       &Load_CloudCoeff=Load_CloudCoeff,&
       &Load_AerosolCoeff=Load_AerosolCoeff)

  sensorindex = 0
  IF (channelinfo(1)%sensor_id == isis(1)) THEN
     sensorindex = 1
  ENDIF
  IF (sensorindex == 0 ) THEN
     PRINT *,'problem with sensorindex = ',isis
     STOP
  ENDIF

!wavelength, you may USE c/f (lightspeed/frequency) or 1/wavenumber.

  PRINT *,isis
  
  DO i=1,nchanl
     PRINT *,i,sc(sensorindex)%frequency(i),1.e7/sc(sensorindex)%wavenumber(i)
  END DO
  
END PROGRAM get_crtm_vawelengths
