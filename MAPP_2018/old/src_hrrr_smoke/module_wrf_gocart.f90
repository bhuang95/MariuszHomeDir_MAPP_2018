MODULE module_wrf_gocart

  USE kinds, only : i_kind
  USE constants, ONLY : max_varname_length
  USE module_utils

  PRIVATE

  PUBLIC :: naero_gocart_wrf, n_aerosols_crtm, &
       &aeronames_gocart_wrf, aeronames_gocart_wrf_gsi
       
 
  INTEGER(i_kind), PARAMETER :: naero_gocart_wrf=15,n_aerosols_crtm=14
  
  CHARACTER(len=max_varname_length), DIMENSION(naero_gocart_wrf), PARAMETER :: &
       aeronames_gocart_wrf=(/&
       'sulf      ','BC1       ','BC2       ','OC1       ',&
       'OC2       ','DUST_1    ','DUST_2    ','DUST_3    ',&
       'DUST_4    ','DUST_5    ','SEAS_1    ','SEAS_2    ',&
       'SEAS_3    ','SEAS_4    ','P25       '/)

  CHARACTER(len=max_varname_length), DIMENSION(naero_gocart_wrf) :: &
       aeronames_gocart_wrf_gsi

END MODULE module_wrf_gocart
