MODULE module_netcdf_handles

  USE netcdf

CONTAINS

  SUBROUTINE handle_err(status)
    INTEGER status
    WRITE(6,*) 'Error number ',status
    WRITE(6,*) nf90_strerror(status)
    STOP
  END SUBROUTINE handle_err

END MODULE module_netcdf_handles
