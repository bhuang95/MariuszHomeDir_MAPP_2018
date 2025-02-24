&fv3post 
  yyyymmddhhmmss = "20150601000000"
  cres = 384                         ! fv3 run resolution 
  nz = 1                             ! max vertical levels 
  inputdir = "."
  outputdir = "."
  input_filetype = "fcst_prod"       ! choices: fcst_prod, nggps2d, and nggps3d
  output_fmt = "grib"
  gribtable = "fv3_gribtable"
  grid_id = 4                        
  is = 1
  npls = 64 
  numvars = 4 
  var_list = 'h500', 'h850', 'u850', 'v850'   ! fcst_prod variables
!  var_list = 'temp', 'ucomp', 'vcomp'        ! nggps3d variables
!  var_list = 'PRATEsfc'                      ! nggps2d variables
  tbeg = 6                                     
  tend = 168
  delta_t = 6 
/
