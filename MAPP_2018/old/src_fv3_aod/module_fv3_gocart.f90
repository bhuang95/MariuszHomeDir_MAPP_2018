MODULE module_fv3_gocart

  USE kinds, only : i_kind
  USE constants, ONLY : max_varname_length
  USE module_utils

  PRIVATE

  PUBLIC :: naero_gocart_fv3, n_aerosols_crtm, &
       &aeronames_gocart_fv3, aeronames_gocart_fv3_gsi
       
 
  INTEGER(i_kind), PARAMETER :: n_aerosols_crtm=14,naero_gocart_fv3=n_aerosols_crtm
  
  CHARACTER(len=max_varname_length), DIMENSION(naero_gocart_fv3), PARAMETER :: &
       aeronames_gocart_fv3=(/&
       'sulf      ','bc1       ','bc2       ','oc1       ',&
       'oc2       ','dust1    ','dust2    ','dust3    ',&
       'dust4    ','dust5    ','seas1    ','seas2    ',&
       'seas3    ','seas4    '/)
  
  CHARACTER(len=max_varname_length), DIMENSION(naero_gocart_fv3) :: &
       aeronames_gocart_fv3_gsi

END MODULE module_fv3_gocart
