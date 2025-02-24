PROGRAM test

!to test reading/writing fv3 netcdf

  USE netcdf
  USE module_kinds
  USE module_netcdf_io_generic

  CHARACTER(len=NF90_MAX_NAME)    :: varname,fnamein,fnameout
  REAL(SINGLE), ALLOCATABLE       :: aoddata(:,:,:,:,:),channels(:)


  fnamein='/scratch1/BMC/gsd-fv3-dev/pagowski/FV3_CHEM/INOUTDATA/control/wfm_jpss_control/&
       &gfs.20190525/00/RESTART/20190526.000000.fv_tracer.res.tile1.nc'

  varname='bc1'
  CALL netcdf_read(fnamein,varname,aoddata)

!  varname='bc2'
!  CALL netcdf_read(fnamein,varname,channels)

  ALLOCATE(channels(SIZE(aoddata,4)))
  channels=550.

  fnameout='/scratch1/BMC/gsd-fv3-dev/pagowski/FV3_CHEM/INOUTDATA/control/wfm_jpss_control/&
       &gfs.20190525/00/RESTART/20190526.000000.fv_tracer.res.tile1_copy.nc'

  CALL netcdf_write_fv3_aod(fnameout,aoddata(:,:,1,:,:),aoddata(:,:,:,:,:),channels)

END PROGRAM test
