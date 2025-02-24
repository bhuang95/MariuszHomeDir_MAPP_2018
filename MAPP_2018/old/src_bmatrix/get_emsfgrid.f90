PROGRAM get_emsfgrid

  USE kinds
  USE netcdf
  USE netcdfio_interface
  USE mpi_interface
  USE module_esmfgrid_init

  IMPLICIT NONE

  TYPE(esmfgrid) :: grid

  grid%filename='./INPUT/esmfgrid_fv3.nc'

  CALL esmfgrid_init_sub(grid)

  PRINT *,grid%n_s,grid%n_a,grid%n_b
  PRINT *,MINVAL(grid%inlons),MAXVAL(grid%inlons)
  PRINT *,MINVAL(grid%inlats),MAXVAL(grid%inlats)

END PROGRAM get_emsfgrid


